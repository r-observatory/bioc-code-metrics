# tests/testthat/test-run-update.R: tests for scripts/update.R
#
# All tests are fully offline; they inject a fake io whose clone() builds a
# real minimal git repo in a temp directory with RELEASE_X_Y branches, allowing
# the real analyze_package to run on it.
#
# Source order expected (from the test validation command):
#   config.R -> git.R -> context.R -> metrics/*.R -> analyze.R -> export.R -> update.R

# ---------------------------------------------------------------------------
# Shared helpers (not test_that blocks)
# ---------------------------------------------------------------------------

# Create a minimal real git repo at dest for analyze_package to work on.
# Each element of `versions` becomes a RELEASE_X_Y local branch commit.
# The version string "1.0" produces branch "RELEASE_1_0", etc.
# DESCRIPTION Version: is the version string so list_versions() returns it.
.make_fake_clone <- function(pkg, dest, versions = "1.0") {
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  system2("git", c("init", dest), stdout = FALSE, stderr = FALSE)
  system2("git", c("-C", dest, "config", "user.email", "test@example.com"),
          stdout = FALSE, stderr = FALSE)
  system2("git", c("-C", dest, "config", "user.name", "Test Bot"),
          stdout = FALSE, stderr = FALSE)

  # Initial commit on master (needed so branches have a base)
  writeLines(paste("#", pkg), file.path(dest, "README"))
  system2("git", c("-C", dest, "add", "-A"),
          stdout = FALSE, stderr = FALSE)
  system2("git", c("-C", dest, "commit", "-m", "init"),
          stdout = FALSE, stderr = FALSE)

  for (ver in versions) {
    # Branch name: "1.0" -> "RELEASE_1_0"
    branch <- paste0("RELEASE_", gsub(".", "_", ver, fixed = TRUE))
    system2("git", c("-C", dest, "checkout", "-b", branch),
            stdout = FALSE, stderr = FALSE)

    writeLines(c(
      paste("Package:", pkg),
      paste("Version:", ver),
      "Title: Test Package",
      "Description: Minimal test package for run_update tests.",
      "Author: Test Bot",
      "Maintainer: Test Bot <test@example.com>",
      "License: MIT"
    ), file.path(dest, "DESCRIPTION"))

    writeLines("export(hello)", file.path(dest, "NAMESPACE"))

    r_dir <- file.path(dest, "R")
    dir.create(r_dir, recursive = TRUE, showWarnings = FALSE)
    writeLines(
      c(paste0("## Version ", ver, " of ", pkg),
        "hello <- function() 'hello'"),
      file.path(r_dir, "hello.R")
    )

    system2("git", c("-C", dest, "add", "-A"),    stdout = FALSE, stderr = FALSE)
    system2("git", c("-C", dest, "commit", "-m", paste0("release-", ver)),
            stdout = FALSE, stderr = FALSE)
    system2("git", c("-C", dest, "checkout", "-"),
            stdout = FALSE, stderr = FALSE)
  }
  TRUE
}

# Build a fake io from a data.frame(package, latest_version).
# fail_clones: character vector of package names whose clone() returns FALSE.
# version_map: named list pkg -> character vector of version strings; if absent,
#   clone() uses the single latest_version from pkg_df.
.fake_io <- function(pkg_df, fail_clones = character(0L),
                     version_map = NULL) {
  list(
    package_list = function() pkg_df,
    clone = function(pkg, dest) {
      if (pkg %in% fail_clones) return(FALSE)
      vers <- if (!is.null(version_map) && pkg %in% names(version_map)) {
        version_map[[pkg]]
      } else {
        v <- pkg_df$latest_version[pkg_df$package == pkg]
        if (length(v) == 0L || is.na(v)) "1.0" else as.character(v)
      }
      .make_fake_clone(pkg, dest, versions = vers)
    }
  )
}

# Override WORK_DIR for the duration of a test.
# Returns a list that .restore_work_dir() can consume.
.override_work_dir <- function() {
  orig <- WORK_DIR
  tmp  <- tempfile("bcm_work_")
  dir.create(tmp, recursive = TRUE)
  WORK_DIR <<- tmp
  list(orig = orig, tmp = tmp)
}

.restore_work_dir <- function(state) {
  WORK_DIR <<- state$orig
  unlink(state$tmp, recursive = TRUE, force = TRUE)
}

# ---------------------------------------------------------------------------
# Test 1: sharded bootstrap -- three runs exhaust a 5-package universe
# ---------------------------------------------------------------------------

#' Skip when the analyzer binary is absent.
#'
#' These two tests assert that a shard ADVANCES between runs. Advancing depends
#' on the per-package sentinels being written, and only the analyzer binary
#' writes them, so without the binary the same shard is selected forever and
#' the assertions fail with count mismatches that say nothing about the cause.
#' Only a linux-x86_64 build is published, so this skips on macOS rather than
#' reporting twelve mysterious failures to anyone developing there.
skip_without_analyzer <- function() {
  if (!nzchar(rpkg_analyzer_bin()))
    testthat::skip("needs rpkg-analyzer: without it the backfill pool never drains")
}

test_that("sharded bootstrap: three runs cover universe of 5, fourth is no-op", {
  skip_without_analyzer()
  out_dir <- tempfile()
  dir.create(out_dir)
  on.exit(unlink(out_dir, recursive = TRUE, force = TRUE), add = TRUE)

  wstate <- .override_work_dir()
  on.exit(.restore_work_dir(wstate), add = TRUE)

  pkgs   <- c("pkgA", "pkgB", "pkgC", "pkgD", "pkgE")
  pkg_df <- data.frame(package = pkgs, latest_version = rep("1.0", 5L),
                       stringsAsFactors = FALSE)
  io <- .fake_io(pkg_df)

  # ---- Run 1 ---------------------------------------------------------------
  m1 <- run_update(io, out_dir, shard_size = 2L)

  expect_equal(m1$n_shard,    2L)
  expect_equal(m1$n_universe, 5L)
  expect_false(m1$bootstrap_complete)
  expect_true(m1$changed)
  expect_equal(m1$shard_failures$count, 0L)

  con  <- DBI::dbConnect(RSQLite::SQLite(), file.path(out_dir, DB_FILENAME))
  pkgs1 <- sort(DBI::dbGetQuery(
    con, "SELECT DISTINCT package FROM bioc_code_summary")$package)
  DBI::dbDisconnect(con)

  expect_equal(length(pkgs1), 2L)
  expect_equal(pkgs1, c("pkgA", "pkgB"))  # deterministic alphabetical shard

  # code-manifest.json and data-manifest.json written to out_dir
  expect_true(file.exists(file.path(out_dir, "code-manifest.json")))
  expect_true(file.exists(file.path(out_dir, "data-manifest.json")))

  # ---- Run 2 ---------------------------------------------------------------
  m2 <- run_update(io, out_dir, shard_size = 2L)

  expect_equal(m2$n_shard, 2L)
  expect_false(m2$bootstrap_complete)
  expect_equal(m2$n_analyzed, 4L)

  con  <- DBI::dbConnect(RSQLite::SQLite(), file.path(out_dir, DB_FILENAME))
  pkgs2 <- sort(DBI::dbGetQuery(
    con, "SELECT DISTINCT package FROM bioc_code_summary")$package)
  DBI::dbDisconnect(con)

  expect_equal(length(pkgs2), 4L)
  expect_true(all(c("pkgA", "pkgB") %in% pkgs2))  # carry-forward preserved
  expect_true(all(c("pkgC", "pkgD") %in% pkgs2))  # new this shard

  # ---- Run 3 ---------------------------------------------------------------
  m3 <- run_update(io, out_dir, shard_size = 2L)

  expect_equal(m3$n_shard, 1L)          # only pkgE remains
  expect_true(m3$bootstrap_complete)
  expect_equal(m3$n_analyzed, 5L)

  con  <- DBI::dbConnect(RSQLite::SQLite(), file.path(out_dir, DB_FILENAME))
  pkgs3 <- sort(DBI::dbGetQuery(
    con, "SELECT DISTINCT package FROM bioc_code_summary")$package)
  DBI::dbDisconnect(con)

  expect_equal(pkgs3, sort(pkgs))

  # ---- Run 4: no-op --------------------------------------------------------
  m4 <- run_update(io, out_dir, shard_size = 2L)

  expect_false(m4$changed)
  expect_equal(m4$n_shard,    0L)
  expect_equal(m4$n_analyzed, 5L)
  expect_true(m4$bootstrap_complete)
})

# ---------------------------------------------------------------------------
# Test 2: version change triggers re-analysis; old rows are replaced
# ---------------------------------------------------------------------------

test_that("package with new version is re-analyzed and carry-forward rows replaced", {
  out_dir <- tempfile()
  dir.create(out_dir)
  on.exit(unlink(out_dir, recursive = TRUE, force = TRUE), add = TRUE)

  wstate <- .override_work_dir()
  on.exit(.restore_work_dir(wstate), add = TRUE)

  # Initial universe: pkgA at 1.0 (single RELEASE_1_0 branch).
  pkg_df_v1 <- data.frame(package = "pkgA", latest_version = "1.0",
                           stringsAsFactors = FALSE)
  io_v1 <- .fake_io(pkg_df_v1, version_map = list(pkgA = "1.0"))

  run_update(io_v1, out_dir, shard_size = 10L)

  con    <- DBI::dbConnect(RSQLite::SQLite(), file.path(out_dir, DB_FILENAME))
  rows_v1 <- DBI::dbGetQuery(
    con, "SELECT version FROM bioc_code_summary WHERE package='pkgA'")
  DBI::dbDisconnect(con)

  expect_equal(nrow(rows_v1), 1L)
  expect_equal(rows_v1$version, "1.0")

  # Universe updated: pkgA now at 1.1; repo has both RELEASE_1_0 and RELEASE_1_1.
  pkg_df_v2 <- data.frame(package = "pkgA", latest_version = "1.1",
                           stringsAsFactors = FALSE)
  io_v2 <- .fake_io(pkg_df_v2, version_map = list(pkgA = c("1.0", "1.1")))

  m2 <- run_update(io_v2, out_dir, shard_size = 10L)

  expect_true(m2$changed)
  expect_equal(m2$n_shard, 1L)

  con    <- DBI::dbConnect(RSQLite::SQLite(), file.path(out_dir, DB_FILENAME))
  rows_v2 <- DBI::dbGetQuery(
    con,
    "SELECT version FROM bioc_code_summary WHERE package='pkgA' ORDER BY version")
  DBI::dbDisconnect(con)

  # Fresh analysis replaces old row(s); now both versions are present.
  expect_equal(nrow(rows_v2), 2L)
  expect_true("1.0" %in% rows_v2$version)
  expect_true("1.1" %in% rows_v2$version)
})

# ---------------------------------------------------------------------------
# Test 3: clone failure is recorded and does not abort the shard
# ---------------------------------------------------------------------------

test_that("clone failure is recorded in shard_failures and does not abort", {
  out_dir <- tempfile()
  dir.create(out_dir)
  on.exit(unlink(out_dir, recursive = TRUE, force = TRUE), add = TRUE)

  wstate <- .override_work_dir()
  on.exit(.restore_work_dir(wstate), add = TRUE)

  pkg_df <- data.frame(
    package        = c("pkgFail", "pkgOk"),
    latest_version = c("1.0",     "1.0"),
    stringsAsFactors = FALSE
  )
  io <- .fake_io(pkg_df, fail_clones = "pkgFail")

  m <- run_update(io, out_dir, shard_size = 10L)

  # Failure is recorded in the manifest.
  expect_equal(m$shard_failures$count,    1L)
  expect_true("pkgFail" %in% m$shard_failures$packages)

  # Run did not abort: pkgOk must be in the DB.
  con     <- DBI::dbConnect(RSQLite::SQLite(), file.path(out_dir, DB_FILENAME))
  pkgs_db <- DBI::dbGetQuery(
    con, "SELECT DISTINCT package FROM bioc_code_summary")$package
  DBI::dbDisconnect(con)

  expect_true("pkgOk"   %in% pkgs_db)
  expect_false("pkgFail" %in% pkgs_db)
})

# ---------------------------------------------------------------------------
# Test 4: force_full re-analyzes packages already in the DB
# ---------------------------------------------------------------------------

test_that("force_full re-analyzes packages already in DB within shard_size limit", {
  out_dir <- tempfile()
  dir.create(out_dir)
  on.exit(unlink(out_dir, recursive = TRUE, force = TRUE), add = TRUE)

  wstate <- .override_work_dir()
  on.exit(.restore_work_dir(wstate), add = TRUE)

  pkg_df <- data.frame(
    package        = c("pkgA", "pkgB", "pkgC"),
    latest_version = c("1.0",  "1.0",  "1.0"),
    stringsAsFactors = FALSE
  )
  io <- .fake_io(pkg_df)

  # Full bootstrap first.
  run_update(io, out_dir, shard_size = 10L)

  # A no-op run would set changed=FALSE; force_full=TRUE overrides that.
  m <- run_update(io, out_dir, shard_size = 2L, force_full = TRUE)

  expect_true(m$changed)
  expect_equal(m$n_shard, 2L)   # shard_size still caps the run
  # One package (pkgC) remains after this shard.
  expect_false(m$bootstrap_complete)
  expect_equal(m$shard_failures$count, 0L)
})

# ---------------------------------------------------------------------------
# Test 5: package reaching MAX_CLONE_FAILURES is excluded and counted
# ---------------------------------------------------------------------------

test_that("package hitting MAX_CLONE_FAILURES is excluded from todo and counted in permanent_failures", {
  skip_without_analyzer()
  out_dir <- tempfile()
  dir.create(out_dir)
  on.exit(unlink(out_dir, recursive = TRUE, force = TRUE), add = TRUE)

  wstate <- .override_work_dir()
  on.exit(.restore_work_dir(wstate), add = TRUE)

  pkg_df <- data.frame(
    package        = c("pkgFail", "pkgOk"),
    latest_version = c("1.0",     "1.0"),
    stringsAsFactors = FALSE
  )
  io_mixed <- .fake_io(pkg_df, fail_clones = "pkgFail")

  # Run MAX_CLONE_FAILURES times; pkgFail accumulates consecutive_failures.
  for (i in seq_len(MAX_CLONE_FAILURES)) {
    run_update(io_mixed, out_dir, shard_size = 10L)
  }

  # At this point pkgFail has exactly MAX_CLONE_FAILURES consecutive failures.
  # Next run should exclude pkgFail from the to-do list entirely.
  m_final <- run_update(io_mixed, out_dir, shard_size = 10L)

  expect_equal(m_final$permanent_failures, 1L)
  # pkgFail excluded; pkgOk already analyzed, so nothing to do this run.
  expect_equal(m_final$n_shard, 0L)
  expect_equal(m_final$shard_failures$count, 0L)
})

# ---------------------------------------------------------------------------
# Test 6: transient failure followed by success resets the failure counter
# ---------------------------------------------------------------------------

test_that("transient failure that later succeeds resets the failure counter", {
  out_dir <- tempfile()
  dir.create(out_dir)
  on.exit(unlink(out_dir, recursive = TRUE, force = TRUE), add = TRUE)

  wstate <- .override_work_dir()
  on.exit(.restore_work_dir(wstate), add = TRUE)

  pkg_df <- data.frame(package = "pkgFlaky", latest_version = "1.0",
                       stringsAsFactors = FALSE)

  # Run 1: clone fails -> consecutive_failures = 1.
  io_fail <- .fake_io(pkg_df, fail_clones = "pkgFlaky")
  run_update(io_fail, out_dir, shard_size = 10L)

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out_dir, DB_FILENAME))
  cf1 <- DBI::dbGetQuery(con,
    "SELECT consecutive_failures FROM bioc_metrics_failures WHERE package = 'pkgFlaky'")
  DBI::dbDisconnect(con)
  expect_equal(cf1$consecutive_failures, 1L)

  # Run 2: clone succeeds -> failure record deleted, package appears in DB.
  io_ok <- .fake_io(pkg_df)
  m2 <- run_update(io_ok, out_dir, shard_size = 10L)

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out_dir, DB_FILENAME))
  cf2 <- DBI::dbGetQuery(con,
    "SELECT consecutive_failures FROM bioc_metrics_failures WHERE package = 'pkgFlaky'")
  pkgs_db <- DBI::dbGetQuery(
    con, "SELECT DISTINCT package FROM bioc_code_summary")$package
  DBI::dbDisconnect(con)

  expect_equal(nrow(cf2), 0L)               # failure row deleted on success
  expect_true("pkgFlaky" %in% pkgs_db)      # package now in DB
  expect_equal(m2$permanent_failures, 0L)   # no permanent failures
})

# ---------------------------------------------------------------------------
# .parse_bioc_release: the up-to-date check compares stored versions against
# the current Bioconductor RELEASE, parsed from config.yaml.
# ---------------------------------------------------------------------------

test_that(".parse_bioc_release extracts release_version from config.yaml lines", {
  lines <- c('release_version: "3.23"', 'devel_version: "3.24"', '  bioc_version: "3.23"')
  expect_equal(.parse_bioc_release(lines), "3.23")
  expect_equal(.parse_bioc_release(c("release_version: 3.20")), "3.20")   # unquoted
  expect_true(is.na(.parse_bioc_release(c("devel_version: 3.24"))))       # no release line
  expect_true(is.na(.parse_bioc_release(character(0))))                   # empty
})

# ---------------------------------------------------------------------------
# The dataset marker is only earned by a run that actually read datasets
# ---------------------------------------------------------------------------
# Dataset records come from the analyzer binary alone; analyze_package falls
# back to the pure-R analyze_version() whenever analyze_with_binary() returns
# NULL. Both tests stub that one call rather than depending on whether this
# machine has the binary, so the assertion means the same thing on a laptop and
# in CI.

.with_binary_returning <- function(value, expr) {
  env <- environment(analyze_package)
  old <- get("analyze_with_binary", envir = env)
  assign("analyze_with_binary",
         function(dir, kind = ANALYZER_INPUT_KIND, memo = NULL, protect = FALSE,
                  limit = NULL) value,
         envir = env)
  on.exit(assign("analyze_with_binary", old, envir = env), add = TRUE)
  force(expr)
}

test_that("a package analyzed without the dataset reader is not marked scanned", {
  repo <- tempfile("bcm_ds_")
  on.exit(unlink(repo, recursive = TRUE, force = TRUE), add = TRUE)
  .make_fake_clone("pkgNoReader", repo, versions = c("1.0", "1.1"))

  res <- .with_binary_returning(NULL, analyze_package(repo, "pkgNoReader"))

  expect_equal(nrow(res$datasets), 0L)
  expect_true(all(is.na(res$summary$datasets_scanned)))
})

test_that("a package the reader looked at is marked scanned even with nothing to report", {
  repo <- tempfile("bcm_ds_")
  on.exit(unlink(repo, recursive = TRUE, force = TRUE), add = TRUE)
  .make_fake_clone("pkgReader", repo, versions = c("1.0", "1.1"))

  # A package that ships no data still gets a scan: the reader ran and found
  # nothing, which is a different fact from never having looked.
  metrics <- structure(list(loc_r = 1L),
                       functions = .empty_functions_df()[, -(1:2), drop = FALSE],
                       edges     = .empty_edges_df()[, -(1:2), drop = FALSE],
                       datasets  = .datasets_frame(list()))
  res <- .with_binary_returning(metrics, analyze_package(repo, "pkgReader"))

  last <- nrow(res$summary)
  expect_equal(nrow(res$datasets), 0L)
  expect_true(isTRUE(res$summary$datasets_scanned[last]))
  expect_true(all(is.na(res$summary$datasets_scanned[-last])))
})

# ---------------------------------------------------------------------------
# A release that cannot be extracted, or whose analyzer output does not parse,
# fails the package and changes nothing
# ---------------------------------------------------------------------------

# A stub analyzer that reads every release and, on `bad_version`, also prints
# a line that is not JSON.
.stub_reads_bin <- function(dir, bad_version = "none", version = "0.4.0-test") {
  stub <- file.path(dir, "stub-reads.sh")
  writeLines(c(
    "#!/bin/sh",
    sprintf('if [ "$1" = "--version" ]; then echo "rpkg-analyzer %s"; exit 0; fi', version),
    'dir=$(echo "$1" | tr -d "\'")',
    'v=$(sed -n "s/^Version: *//p" "$dir/DESCRIPTION" | head -1)',
    'echo "{\\"rec\\":\\"summary\\",\\"loc_r\\":1,\\"n_fns_r\\":1}"',
    sprintf('if [ "$v" = "%s" ]; then echo "{\\"rec\\":\\"function\\",\\"name\\":"; fi', bad_version),
    "exit 0"), stub)
  Sys.chmod(stub, mode = "0755")
  stub
}

test_that("analyze_package fails as a whole when one release cannot be extracted", {
  repo <- tempfile("bcm_xf_")
  on.exit(unlink(repo, recursive = TRUE, force = TRUE), add = TRUE)
  .make_fake_clone("pkgX", repo, versions = c("1.0", "1.1"))
  real <- extract_version
  .local_global("extract_version", function(repo, ref, dest) {
    if (grepl("RELEASE_1_1$", ref)) stop(.extract_failure("archive", ref, 128L, "fatal: bad object"))
    real(repo, ref, dest)
  })
  err <- tryCatch(analyze_package(repo, "pkgX"), error = function(e) e)
  expect_s3_class(err, "extract_failure")
  expect_identical(err$ref, "RELEASE_1_1")
})

test_that("a cap during extraction extracts again into an emptied directory", {
  repo <- tempfile("bcm_xc_")
  on.exit(unlink(repo, recursive = TRUE, force = TRUE), add = TRUE)
  .make_fake_clone("pkgX", repo, versions = c("1.0", "1.1"))
  .local_global("analyze_with_binary",
                function(dir, kind = ANALYZER_INPUT_KIND, memo = NULL, protect = FALSE,
                         limit = NULL) NULL)
  want <- suppressWarnings(analyze_package(repo, "pkgX"))

  real  <- extract_version
  fired <- FALSE
  .local_global("extract_version", function(repo, ref, dest) {
    if (!fired && grepl("RELEASE_1_1$", ref)) {
      fired <<- TRUE
      real(repo, ref, dest)
      writeLines("leftover <- function() 1", file.path(dest, "R", "leftover.R"))
      stop(.cap_error())
    }
    real(repo, ref, dest)
  })
  got <- suppressWarnings(analyze_package(repo, "pkgX"))
  expect_true(fired)
  # When each walk began is the one value the two analyses differ in.
  got$state$read_at  <- NULL
  want$state$read_at <- NULL
  expect_identical(got, want)
})

test_that("a release that cannot be extracted leaves the package's stored rows as they were", {
  out_dir <- tempfile(); dir.create(out_dir)
  on.exit(unlink(out_dir, recursive = TRUE, force = TRUE), add = TRUE)
  wstate <- .override_work_dir()
  on.exit(.restore_work_dir(wstate), add = TRUE)
  orig_cores <- ANALYSIS_CORES
  ANALYSIS_CORES <<- 1L
  on.exit(ANALYSIS_CORES <<- orig_cores, add = TRUE)
  withr::local_envvar(RPKG_ANALYZER_BIN = .stub_reads_bin(withr::local_tempdir()))

  v1 <- data.frame(package = "pkgA", latest_version = "1.0", stringsAsFactors = FALSE)
  run_update(.fake_io(v1, version_map = list(pkgA = "1.0")), out_dir, shard_size = 10L)
  before <- .package_rows(out_dir, "pkgA")
  expect_gt(nrow(before[[paste(DB_FILENAME, SUMMARY_TABLE)]]), 0L)

  real <- extract_version
  .local_global("extract_version", function(repo, ref, dest) {
    if (grepl("RELEASE_1_1$", ref)) stop(.extract_failure("archive", ref, 128L, "fatal: bad object"))
    real(repo, ref, dest)
  })
  v2 <- data.frame(package = "pkgA", latest_version = "1.1", stringsAsFactors = FALSE)
  logged <- capture.output(m <- suppressWarnings(
    run_update(.fake_io(v2, version_map = list(pkgA = c("1.0", "1.1"))),
               out_dir, shard_size = 10L)))

  expect_identical(m$shard_failures$packages, "pkgA")
  expect_true(any(grepl("FAIL pkgA: extract after", logged, fixed = TRUE)))
  expect_identical(.package_rows(out_dir, "pkgA"), before)
})

test_that("analyze_package fails with analyzer_parse_incomplete on a line that does not parse", {
  skip_on_os("windows")
  repo <- tempfile("bcm_pi_")
  on.exit(unlink(repo, recursive = TRUE, force = TRUE), add = TRUE)
  .make_fake_clone("pkgP", repo, versions = c("1.0", "1.1"))
  withr::local_envvar(RPKG_ANALYZER_BIN = .stub_reads_bin(withr::local_tempdir(), "1.1"))
  err <- tryCatch(analyze_package(repo, "pkgP"), error = function(e) e)
  expect_s3_class(err, "analyzer_parse_incomplete")
  expect_identical(err$n_bad, 1L)
})

test_that("an analyzer line that does not parse leaves stored rows and read attempts as they were", {
  skip_on_os("windows")
  out_dir <- tempfile(); dir.create(out_dir)
  on.exit(unlink(out_dir, recursive = TRUE, force = TRUE), add = TRUE)
  wstate <- .override_work_dir()
  on.exit(.restore_work_dir(wstate), add = TRUE)
  withr::local_envvar(RPKG_ANALYZER_BIN = .stub_reads_bin(withr::local_tempdir(), "1.1"))

  v1 <- data.frame(package = "pkgA", latest_version = "1.0", stringsAsFactors = FALSE)
  run_update(.fake_io(v1, version_map = list(pkgA = "1.0")), out_dir, shard_size = 10L)
  before <- .package_rows(out_dir, "pkgA")
  expect_gt(nrow(before[[paste(DB_FILENAME, SUMMARY_TABLE)]]), 0L)

  v2 <- data.frame(package = "pkgA", latest_version = "1.1", stringsAsFactors = FALSE)
  m <- suppressWarnings(run_update(.fake_io(v2, version_map = list(pkgA = c("1.0", "1.1"))),
                                   out_dir, shard_size = 10L))

  expect_identical(m$shard_failures$packages, "pkgA")
  expect_identical(.package_rows(out_dir, "pkgA"), before)
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out_dir, DB_FILENAME))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_identical(nrow(DBI::dbGetQuery(con,
    "SELECT * FROM bioc_analyzer_read_attempts WHERE package = 'pkgA'")), 0L)
})

test_that("a cap in a per-release step of analyze_package changes nothing it returns", {
  repo <- tempfile("bcm_capsteps_")
  on.exit(unlink(repo, recursive = TRUE, force = TRUE), add = TRUE)
  .make_fake_clone("pkgCap", repo, versions = c("1.0", "1.1"))
  system2("git", c("-C", repo, "checkout", "-q", "RELEASE_1_1"), stdout = FALSE, stderr = FALSE)
  writeLines('old <- function() .Deprecated("hello")', file.path(repo, "R", "old.R"))
  system2("git", c("-C", repo, "add", "-A"), stdout = FALSE, stderr = FALSE)
  system2("git", c("-C", repo, "commit", "-m", "extras"), stdout = FALSE, stderr = FALSE)
  system2("git", c("-C", repo, "checkout", "-q", "-"), stdout = FALSE, stderr = FALSE)

  metrics <- structure(list(loc_r = 1L),
                       functions = .empty_functions_df()[, -(1:2), drop = FALSE],
                       edges     = .empty_edges_df()[, -(1:2), drop = FALSE],
                       datasets  = .datasets_frame(list()))
  .local_global("analyze_with_binary",
                function(dir, kind = ANALYZER_INPUT_KIND, memo = NULL, protect = FALSE,
                         limit = NULL) metrics)
  # read_at is the clock at each reading, so it differs between any two runs.
  analyse <- function() {
    res <- analyze_package(repo, "pkgCap")
    res$text$versions$read_at <- NULL
    res$state$read_at <- NULL
    res
  }
  want <- analyse()

  at_1.1 <- function(ctx) identical(ctx$version, "1.1")
  .local_global("deprecation_signals", .fires_cap_once(deprecation_signals, when = at_1.1))
  .local_global("parse_namespace", .fires_cap_once(parse_namespace))
  expect_identical(analyse(), want)
})

# ---------------------------------------------------------------------------
# A git killed at GIT_TIMEOUT is a timeout, not a fetch failure
# ---------------------------------------------------------------------------

# Put a git in front of PATH that exits 124 on `archive`, as system2 reports a
# kill at GIT_TIMEOUT, and runs the real git for everything else.
.local_git_archive_124 <- function(frame = parent.frame()) {
  dir  <- withr::local_tempdir(.local_envir = frame)
  real <- Sys.which("git")
  writeLines(c("#!/bin/sh",
               'for a in "$@"; do [ "$a" = archive ] && exit 124; done',
               sprintf('exec %s "$@"', shQuote(real))), file.path(dir, "git"))
  Sys.chmod(file.path(dir, "git"), mode = "0755")
  withr::local_envvar(PATH = paste(dir, Sys.getenv("PATH"), sep = .Platform$path.sep),
                      .local_envir = frame)
}

test_that("an archive killed at GIT_TIMEOUT parks as a timeout and is released by a new build", {
  skip_on_os("windows")
  out_dir <- withr::local_tempdir()
  wstate <- .override_work_dir()
  on.exit(.restore_work_dir(wstate), add = TRUE)
  stub_dir <- withr::local_tempdir()
  withr::local_envvar(RPKG_ANALYZER_BIN = .stub_reads_bin(stub_dir))
  .local_git_archive_124()
  io <- .fake_io(data.frame(package = "pkgT", latest_version = "1.0",
                            stringsAsFactors = FALSE))
  failures <- function() {
    con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out_dir, DB_FILENAME))
    on.exit(DBI::dbDisconnect(con))
    DBI::dbGetQuery(con, "SELECT stage, fetch_failures, timeout_failures
                            FROM bioc_metrics_failures WHERE package = 'pkgT'")
  }

  for (i in seq_len(MAX_TIMEOUT_FAILURES)) {
    suppressWarnings(run_update(io, out_dir, shard_size = 10L))
  }
  expect_identical(failures(), data.frame(stage = "git_timeout", fetch_failures = 0L,
                                          timeout_failures = MAX_TIMEOUT_FAILURES,
                                          stringsAsFactors = FALSE))
  expect_identical(suppressWarnings(run_update(io, out_dir, shard_size = 10L))$n_shard, 0L)

  .stub_reads_bin(stub_dir, version = "0.4.1-test")
  expect_identical(suppressWarnings(run_update(io, out_dir, shard_size = 10L))$n_shard, 1L)
  expect_identical(failures()$timeout_failures, 1L)
})

# ---------------------------------------------------------------------------
# The MAX_CLONE_FAILURES path as CI runs it
# ---------------------------------------------------------------------------

test_that("the MAX_CLONE_FAILURES path parks the same way when Actions sets GITHUB_RUN_ID", {
  # Actions sets GITHUB_RUN_ID in the unit-test step too; a run id read from it
  # would skip pkgFail on calls 2 to 5 in CI only.
  withr::local_envvar(c(GITHUB_RUN_ID = "ci"))
  withr::local_envvar(RPKG_ANALYZER_BIN = .stub_reads_bin(withr::local_tempdir()))
  out_dir <- withr::local_tempdir()
  wstate <- .override_work_dir()
  on.exit(.restore_work_dir(wstate), add = TRUE)
  pkg_df <- data.frame(package = c("pkgFail", "pkgOk"), latest_version = c("1.0", "1.0"),
                       stringsAsFactors = FALSE)
  io_mixed <- .fake_io(pkg_df, fail_clones = "pkgFail")
  for (i in seq_len(MAX_CLONE_FAILURES)) {
    suppressWarnings(run_update(io_mixed, out_dir, shard_size = 10L))
  }
  m_final <- suppressWarnings(run_update(io_mixed, out_dir, shard_size = 10L))
  expect_equal(m_final$permanent_failures, 1L)
  expect_equal(m_final$n_shard, 0L)
  expect_equal(m_final$shard_failures$count, 0L)
})

# ---------------------------------------------------------------------------
# The shard loop ends although a package failed this run
# ---------------------------------------------------------------------------

# The workflow's loop: run_update as the shard, then shard_loop_done read
# through bash, as update.yml runs them. Returns each shard's run status.
.run_shard_loop <- function(io, out_dir, max_shards = 10L) {
  script <- normalizePath(test_path("..", "..", "scripts", "publish.sh"))
  status_path <- file.path(out_dir, "run-status.json")
  statuses <- list()
  for (i in seq_len(max_shards)) {
    suppressWarnings(run_update(io, out_dir, shard_size = 2L))
    statuses[[i]] <- jsonlite::read_json(status_path)
    rc <- system2("bash", c("-c", shQuote(sprintf("source %s && shard_loop_done %s",
                                                  shQuote(script), shQuote(status_path)))),
                  stdout = FALSE, stderr = FALSE)
    if (identical(rc, 0L)) break
  }
  statuses
}

test_that("the shard loop stops at the shard that drains its queue and publishes no empty shard", {
  skip_on_os("windows")
  skip_if(!nzchar(Sys.which("jq")), "jq is not installed")
  withr::local_envvar(c(PIPELINE_RUN_ID = "r1"))
  withr::local_envvar(RPKG_ANALYZER_BIN = .stub_reads_bin(withr::local_tempdir()))
  out_dir <- withr::local_tempdir()
  wstate <- .override_work_dir()
  on.exit(.restore_work_dir(wstate), add = TRUE)
  pkgs <- c("pkgA", "pkgB", "pkgC", "pkgD", "pkgE", "pkgF")
  io <- .fake_io(data.frame(package = pkgs, latest_version = rep("1.0", 6L),
                            stringsAsFactors = FALSE), fail_clones = "pkgC")
  # A baseline whose fingerprint the first shard moves, so every later shard of
  # the run reads as changed against it.
  write_manifest(file.path(out_dir, "prev-code-manifest.json"),
                 list(schema_version = 1L, series = "code", fingerprint = strrep("0", 64L)))

  statuses <- .run_shard_loop(io, out_dir)

  expect_lte(length(statuses), 3L)
  published <- Filter(function(s) isTRUE(s$changed), statuses)
  expect_true(all(vapply(published, function(s) s$n_shard > 0L, logical(1L))))
  expect_false(statuses[[length(statuses)]]$bootstrap_complete)
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out_dir, DB_FILENAME))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_identical(DBI::dbGetQuery(con,
    "SELECT consecutive_failures FROM bioc_metrics_failures WHERE package = 'pkgC'")[[1L]], 1L)
})


# ---------------------------------------------------------------------------
# Each package's analyzer directory, and the worker's phase times
# ---------------------------------------------------------------------------

# A stub analyzer that reads every version and, as 0.5.1 does, appends one
# statistics line to $RPKG_ANALYZER_STATS. Run by a worker (the statistics
# variable set), it notes its cache directory, or "no-cache", in $STUB_SEEN.
.stub_stats_bin <- function(dir) {
  stub <- file.path(dir, "stub-stats.sh")
  stats <- paste0('{\\"build\\":\\"0.5.1-test\\",\\"ms\\":1500.5,\\"ms_compiled\\":1000,',
                  '\\"ms_r\\":300,\\"ms_tests\\":100,\\"ms_data\\":50,\\"ms_other\\":50.5,',
                  '\\"compiled\\":{\\"files\\":4,\\"hits\\":3},\\"r\\":{\\"files\\":2,\\"hits\\":0},',
                  '\\"tests\\":{\\"files\\":1,\\"hits\\":0},\\"data\\":{\\"files\\":0,\\"hits\\":0},',
                  '\\"cache_errors\\":0,\\"verify_mismatch\\":0}')
  writeLines(c(
    "#!/bin/sh",
    'if [ "$1" = "--version" ]; then echo "rpkg-analyzer 0.5.1-test"; exit 0; fi',
    sprintf('echo "{\\"rec\\":\\"summary\\",\\"input_kind\\":\\"%s\\",\\"loc_r\\":1,\\"n_fns_r\\":1}"',
            ANALYZER_INPUT_KIND),
    sprintf('if [ -n "$RPKG_ANALYZER_STATS" ]; then echo "%s" >> "$RPKG_ANALYZER_STATS"; fi', stats),
    'if [ -n "$STUB_SEEN" ] && [ -n "$RPKG_ANALYZER_STATS" ]; then echo "${RPKG_ANALYZER_CACHE_DIR:-no-cache}" >> "$STUB_SEEN"; fi',
    "exit 0"), stub)
  Sys.chmod(stub, mode = "0755")
  stub
}

test_that("each package's analyzer directory carries its cache, and goes when the package is done", {
  skip_on_os("windows")
  out_dir <- withr::local_tempdir()
  wstate <- .override_work_dir()
  on.exit(.restore_work_dir(wstate), add = TRUE)
  seen <- withr::local_tempfile()
  withr::local_envvar(RPKG_ANALYZER_BIN = .stub_stats_bin(withr::local_tempdir()),
                      STUB_SEEN = seen, RPA_CACHE = NA,
                      RPKG_ANALYZER_STATS = NA, RPKG_ANALYZER_CACHE_DIR = NA)
  pkg_df <- data.frame(package = c("pkgA", "pkgB"), latest_version = c("1.1", "1.0"),
                       stringsAsFactors = FALSE)
  capture.output(run_update(
    .fake_io(pkg_df, version_map = list(pkgA = c("1.0", "1.1"), pkgB = "1.0")),
    out_dir, shard_size = 10L))
  dirs <- readLines(seen)
  expect_length(dirs, 3L)
  expect_setequal(basename(dirname(dirs)), c("pkgA", "pkgB"))
  expect_true(all(basename(dirs) == "cache"))
  expect_true(all(basename(dirname(dirname(dirs))) == ".rpa"))
  expect_false(any(dir.exists(dirname(dirs))))
  expect_identical(Sys.getenv("RPKG_ANALYZER_STATS", unset = "unset"), "unset")
})

test_that("RPA_CACHE turns the cache off however it is spelled, and keeps the statistics file", {
  expect_true(all(vapply(c("off", "OFF", " Off ", "false", "no", "0"),
                         .analyzer_cache_off, logical(1L))))
  expect_false(any(vapply(c("", "on", "true", "1", "yes"), .analyzer_cache_off, logical(1L))))

  skip_on_os("windows")
  out_dir <- withr::local_tempdir()
  wstate <- .override_work_dir()
  on.exit(.restore_work_dir(wstate), add = TRUE)
  seen <- withr::local_tempfile()
  withr::local_envvar(RPKG_ANALYZER_BIN = .stub_stats_bin(withr::local_tempdir()),
                      STUB_SEEN = seen, RPA_CACHE = "off")
  pkg_df <- data.frame(package = "pkgA", latest_version = "1.0", stringsAsFactors = FALSE)
  capture.output(run_update(.fake_io(pkg_df), out_dir, shard_size = 10L))
  expect_identical(readLines(seen), "no-cache")
})

test_that("the analyzer directory of foo never touches the clone of a package named foo.rpa", {
  wstate <- .override_work_dir()
  on.exit(.restore_work_dir(wstate), add = TRUE)
  other <- file.path(WORK_DIR, "foo.rpa")
  dir.create(other)
  writeLines("Package: foo.rpa", file.path(other, "DESCRIPTION"))
  inside <- NULL
  .with_worker_telemetry(function(pkg) {
    inside <<- Sys.getenv("RPKG_ANALYZER_STATS")
    list(package = pkg, ok = TRUE)
  })("foo")
  expect_identical(basename(dirname(inside)), "foo")
  expect_identical(basename(dirname(dirname(inside))), ".rpa")
  expect_true(file.exists(file.path(other, "DESCRIPTION")))
})

test_that("a worker in this process puts the analyzer variables back after each package", {
  wstate <- .override_work_dir()
  on.exit(.restore_work_dir(wstate), add = TRUE)
  withr::local_envvar(RPKG_ANALYZER_CACHE_DIR = "/prior/cache", RPKG_ANALYZER_STATS = NA,
                      RPA_CACHE = NA)
  worker <- .with_worker_telemetry(function(pkg) {
    list(package = pkg, ok = TRUE, cache = Sys.getenv("RPKG_ANALYZER_CACHE_DIR"),
         stats = Sys.getenv("RPKG_ANALYZER_STATS"))
  })
  a <- worker("pkgA")
  b <- worker("pkgB")
  expect_identical(basename(dirname(a$cache)), "pkgA")
  expect_identical(basename(dirname(b$stats)), "pkgB")
  expect_identical(Sys.getenv("RPKG_ANALYZER_CACHE_DIR"), "/prior/cache")
  expect_identical(Sys.getenv("RPKG_ANALYZER_STATS", unset = "unset"), "unset")
  expect_true(is.numeric(a$tally$package_s))
  expect_identical(a$analyzer_stats, character(0L))
})

test_that("a worker's tally holds the seconds of each phase it ran, and the statistics lines", {
  skip_on_os("windows")
  wstate <- .override_work_dir()
  on.exit(.restore_work_dir(wstate), add = TRUE)
  withr::local_envvar(RPKG_ANALYZER_BIN = .stub_stats_bin(withr::local_tempdir()),
                      RPKG_ANALYZER_STATS = NA, STUB_SEEN = NA)
  res <- .with_worker_telemetry(function(pkg) {
    repo <- file.path(WORK_DIR, pkg)
    .make_fake_clone(pkg, repo, versions = c("1.0", "1.1"))
    out <- analyze_package(repo, pkg)
    list(package = pkg, ok = TRUE, n = nrow(out$summary))
  })("pkgT")
  expect_identical(res$n, 2L)
  expect_true(all(c("analyzer_s", "extract_s", "package_s", "parse_s", "versions_s") %in%
                    names(res$tally)))
  expect_true(all(unlist(res$tally) >= 0))
  expect_length(res$analyzer_stats, 2L)
})

# ---------------------------------------------------------------------------
# The shard's analyzer statistics and worker time
# ---------------------------------------------------------------------------

.run_status <- function(out_dir) {
  jsonlite::fromJSON(file.path(out_dir, "run-status.json"), simplifyVector = FALSE)
}

test_that("the shard sums the analyzer's statistics lines and prints them with the worker time", {
  skip_on_os("windows")
  out_dir <- withr::local_tempdir()
  wstate <- .override_work_dir()
  on.exit(.restore_work_dir(wstate), add = TRUE)
  withr::local_envvar(RPKG_ANALYZER_BIN = .stub_stats_bin(withr::local_tempdir()),
                      STUB_SEEN = NA, RPA_CACHE = NA)
  pkg_df <- data.frame(package = c("pkgA", "pkgB"), latest_version = c("1.1", "1.0"),
                       stringsAsFactors = FALSE)
  log <- capture.output(run_update(
    .fake_io(pkg_df, version_map = list(pkgA = c("1.0", "1.1"), pkgB = "1.0")),
    out_dir, shard_size = 10L))

  st <- .run_status(out_dir)
  expect_identical(st$analyzer_stats$runs, 3L)
  expect_identical(st$analyzer_stats$builds, "0.5.1-test")
  expect_equal(st$analyzer_stats$ms, 4501.5)
  expect_equal(st$analyzer_stats$compiled_files, 12)
  expect_equal(st$analyzer_stats$compiled_hits, 9)
  expect_identical(st$analyzer_stats$incomplete_parses, 0L)
  expect_true(paste("analyzer: 3 versions in 5 s; compiled 12 files (75.0% reused);",
                    "cache errors 0; verify mismatches 0; incomplete parses 0") %in% log)
  expect_true(any(startsWith(log, "worker time: clone ")))
  expect_identical(st$worker_phases$packages, 2L)
  expect_true(all(unlist(st$worker_phases) >= 0))
  expect_setequal(names(st$worker_phases),
                  c("packages", "clone_s", "extract_s", "analyzer_s", "parse_s", "metrics_s",
                    "other_s", "dataset_memo_hits", "dataset_memo_misses"))
  published <- jsonlite::fromJSON(file.path(out_dir, "code-manifest.json"), simplifyVector = FALSE)
  expect_false(any(c("analyzer_stats", "worker_phases") %in%
                     c(names(published), names(published$bootstrap))))
})

test_that("a build before 0.5.1 reports no statistics, and the shard says so", {
  skip_on_os("windows")
  out_dir <- withr::local_tempdir()
  wstate <- .override_work_dir()
  on.exit(.restore_work_dir(wstate), add = TRUE)
  withr::local_envvar(RPKG_ANALYZER_BIN = .stub_reads_bin(withr::local_tempdir()))
  pkg_df <- data.frame(package = "pkgA", latest_version = "1.0", stringsAsFactors = FALSE)
  log <- capture.output(run_update(.fake_io(pkg_df), out_dir, shard_size = 10L))
  expect_true("analyzer: no statistics from this build; incomplete parses 0" %in% log)
  expect_identical(.run_status(out_dir)$analyzer_stats$runs, 0L)
})

test_that("a package whose analyzer line does not parse is counted as an incomplete parse", {
  skip_on_os("windows")
  out_dir <- withr::local_tempdir()
  wstate <- .override_work_dir()
  on.exit(.restore_work_dir(wstate), add = TRUE)
  withr::local_envvar(RPKG_ANALYZER_BIN = .stub_reads_bin(withr::local_tempdir(), "1.0"))
  pkg_df <- data.frame(package = "pkgA", latest_version = "1.0", stringsAsFactors = FALSE)
  log <- capture.output(run_update(.fake_io(pkg_df), out_dir, shard_size = 10L))
  expect_identical(.run_status(out_dir)$analyzer_stats$incomplete_parses, 1L)
  expect_true("analyzer: no statistics from this build; incomplete parses 1" %in% log)
})

test_that("a statistics line cut short or unreadable is counted, not summed", {
  good <- '{"build":"0.5.1","ms":10,"compiled":{"files":2,"hits":1},"cache_errors":1}'
  s <- .sum_analyzer_stats(c(good, substr(good, 1L, 30L), "", "garbage", good))
  expect_identical(s$runs, 2L)
  expect_identical(s$unreadable, 3L)
  expect_equal(c(s$ms, s$compiled_files, s$compiled_hits, s$cache_errors), c(20, 4, 2, 2))
  expect_identical(s$builds, "0.5.1")
})

test_that("a fork that crashed adds nothing, and the phases add up to the package time", {
  tel <- .shard_telemetry(list(
    structure("boom", class = "try-error"), NULL,
    list(tally = list(package_s = 10, clone_s = 1, versions_s = 8, extract_s = 1,
                      analyzer_s = 4, parse_s = 1),
         analyzer_stats = character(0L))))
  expect_identical(tel$phases$packages, 1L)
  expect_equal(unlist(tel$phases[c("clone_s", "extract_s", "analyzer_s", "parse_s",
                                   "metrics_s", "other_s")]),
               c(clone_s = 1, extract_s = 1, analyzer_s = 4, parse_s = 1,
                 metrics_s = 2, other_s = 1))
  expect_identical(tel$analyzer$runs, 0L)
  expect_identical(.worker_phase_line(tel$phases), paste(
    "worker time: clone 1.0 s, extract 1.0 s, analyzer 4.0 s, record parse 1.0 s,",
    "metrics 2.0 s, other 1.0 s"))
})

# ---------------------------------------------------------------------------
# The shard's memory lines
# ---------------------------------------------------------------------------

# One statistics line of a build that writes the memory keys; each argument is
# JSON text, so "null" is a key the platform could not fill.
.mem_stats <- function(rss = "null", vm = "null", kept = "null", over = "null") {
  sprintf(paste0('{"build":"0.5.2-test","ms":1,"peak_rss_kb":%s,"peak_vm_kb":%s,',
                 '"data_kept_max":%s,"data_over_budget":%s}'), rss, vm, kept, over)
}

# A stub analyzer of build 0.5.2-test. `lines` names, by "package/version", the
# statistics line that release appends; any other release appends one with the
# memory keys null. A release named in `exits` ends with that status instead.
.stub_memory_bin <- function(dir, lines = character(0L), exits = integer(0L)) {
  stub <- file.path(dir, "stub-memory.sh")
  writeLines(c(
    "#!/bin/sh",
    'if [ "$1" = "--version" ]; then echo "rpkg-analyzer 0.5.2-test"; exit 0; fi',
    'p=$(sed -n "s/^Package: *//p" "$1/DESCRIPTION" | head -1)',
    'v=$(sed -n "s/^Version: *//p" "$1/DESCRIPTION" | head -1)',
    sprintf("stats='%s'", .mem_stats()),
    'case "$p/$v" in',
    sprintf("  %s) exit %d ;;", names(exits), as.integer(exits)),
    sprintf("  %s) stats='%s' ;;", names(lines), lines),
    "esac",
    sprintf("echo '{\"rec\":\"summary\",\"input_kind\":\"%s\",\"loc_r\":1,\"n_fns_r\":1}'",
            ANALYZER_INPUT_KIND),
    'if [ -n "$RPKG_ANALYZER_STATS" ]; then echo "$stats" >> "$RPKG_ANALYZER_STATS"; fi',
    "exit 0"), stub)
  Sys.chmod(stub, mode = "0755")
  stub
}

test_that("the shard keeps the largest peak of each kind with its package, and what the data kept", {
  s <- .sum_analyzer_stats(
    c(.mem_stats(1000, 9000, 10, 0), .mem_stats(3000, 3500, 30, 2),
      .mem_stats(2000, 4000, 20, 0), .mem_stats(), .mem_stats(500, 600, 5, 1)),
    packages = c("pkgA", "pkgB", "pkgB", "pkgC", "pkgD"))
  expect_identical(s$runs, 5L)
  expect_equal(s$peak_rss_kb, 3000)
  expect_identical(s$peak_rss_package, "pkgB")
  expect_equal(s$peak_vm_kb, 9000)
  expect_identical(s$peak_vm_package, "pkgA")
  expect_equal(s$data_kept_max, 30)
  expect_identical(as.character(s$data_over_budget), c("pkgB", "pkgD"))
  # One row a package, its largest of each over its releases, largest resident first.
  expect_equal(s$peaks, list(
    list(package = "pkgB", peak_rss_kb = 3000, peak_vm_kb = 4000),
    list(package = "pkgA", peak_rss_kb = 1000, peak_vm_kb = 9000),
    list(package = "pkgD", peak_rss_kb = 500, peak_vm_kb = 600)))
})

test_that("only the five largest peaks are kept", {
  rss  <- c(10, 70, 30, 50, 20, 60, 40)
  pkgs <- paste0("pkg", LETTERS[seq_along(rss)])
  s <- .sum_analyzer_stats(vapply(rss, function(x) .mem_stats(x, x + 1), character(1L)),
                           packages = pkgs)
  expect_identical(vapply(s$peaks, function(p) p$package, character(1L)),
                   c("pkgB", "pkgF", "pkgD", "pkgG", "pkgC"))
  expect_equal(vapply(s$peaks, function(p) p$peak_rss_kb, numeric(1L)), c(70, 60, 50, 40, 30))
})

test_that("absent or null memory keys read as no memory figures from this build", {
  old <- '{"build":"0.5.1","ms":10,"compiled":{"files":2,"hits":1},"cache_errors":0}'
  for (lines in list(c(old, old), c(.mem_stats(), .mem_stats()), character(0L))) {
    s <- .sum_analyzer_stats(lines, packages = rep("pkgA", length(lines)))
    for (k in c("peak_rss_kb", "peak_rss_package", "peak_vm_kb", "peak_vm_package",
                "data_kept_max")) {
      expect_true(is.na(s[[k]]), info = k)
    }
    expect_length(s$data_over_budget, 0L)
    expect_length(s$peaks, 0L)
    expect_identical(.analyzer_memory_line(s),
                     "analyzer memory: no memory figures from this build")
  }
  # The sums 0.5.1 has always given are what they were.
  expect_equal(.sum_analyzer_stats(c(old, old))$ms, 20)
})

test_that("the analyzer memory line names each figure it has, with its package", {
  s <- .sum_analyzer_stats(
    c(.mem_stats(1382400, 1458176, 1045000000, 0), .mem_stats(2048, 4096, 1048576, 3)),
    packages = c("HMP16SData", "pkgB"))
  expect_identical(.analyzer_memory_line(s), paste0(
    "analyzer memory: peak resident 1350.0 MiB (HMP16SData), ",
    "peak virtual 1424.0 MiB (HMP16SData), largest data kept 996.6 MiB, ",
    "over the data budget: pkgB"))
  none <- .sum_analyzer_stats(.mem_stats(2048, 4096, 1048576, 0), packages = "pkgA")
  expect_match(.analyzer_memory_line(none), "over the data budget: none$")
  # Off Linux the build writes the peaks as null and still counts the data.
  mac <- .sum_analyzer_stats(.mem_stats(kept = 1048576, over = 0), packages = "pkgA")
  expect_identical(.analyzer_memory_line(mac), paste0(
    "analyzer memory: no peak figures on this platform, largest data kept 1.0 MiB, ",
    "over the data budget: none"))
})

test_that("a figure is read from the process status file, and is NA where there is none", {
  status <- withr::local_tempfile()
  writeLines(c("Name:\tR", "VmPeak:\t 2101248 kB", "VmHWM:\t  239616 kB", "Threads:\t1"), status)
  expect_equal(.proc_status_kb("VmHWM", status), 239616)
  expect_equal(.proc_status_kb("VmPeak", status), 2101248)
  expect_true(is.na(.proc_status_kb("VmSwap", status)))
  expect_true(is.na(.proc_status_kb("VmHWM", file.path(withr::local_tempdir(), "none"))))
  here <- .proc_status_kb("VmHWM")
  if (file.exists("/proc/self/status")) expect_gt(here, 0) else expect_true(is.na(here))
})

test_that("a worker returns its peak and the size of its result, and fails nowhere", {
  .local_global("WORK_DIR", withr::local_tempdir())
  res <- .with_worker_telemetry(function(pkg) {
    list(package = pkg, ok = TRUE, summary = data.frame(x = seq_len(1000L)))
  })("pkgA")
  expect_gt(res$memory$result_bytes, 4000)
  if (file.exists("/proc/self/status")) {
    expect_gt(res$memory$peak_rss_kb, 0)
  } else {
    expect_true(is.na(res$memory$peak_rss_kb))
  }
  # A fork that raised returned no list, and gains nothing.
  expect_identical(.with_worker_telemetry(function(pkg) "boom")("pkgB"), "boom")
})

test_that("the shard keeps the largest worker peak and the largest result, each with its package", {
  tel <- .shard_telemetry(list(
    list(package = "pkgA", memory = list(peak_rss_kb = 239616, result_bytes = 1048576)),
    NULL, structure("boom", class = "try-error"),
    list(package = "pkgB", memory = list(peak_rss_kb = 102400, result_bytes = 52428800)),
    list(package = "pkgC")))
  expect_equal(tel$workers, list(peak_rss_kb = 239616, peak_rss_package = "pkgA",
                                 result_bytes = 52428800, result_package = "pkgB"))
  expect_identical(.worker_memory_line(tel$workers),
                   "worker memory: peak resident 234.0 MiB (pkgA), largest result 50.0 MiB (pkgB)")

  off_linux <- .shard_telemetry(list(
    list(package = "pkgA", memory = list(peak_rss_kb = NA_real_, result_bytes = 2097152))))
  expect_true(is.na(off_linux$workers$peak_rss_kb))
  expect_identical(.worker_memory_line(off_linux$workers), paste0(
    "worker memory: no peak figure on this platform, largest result 2.0 MiB (pkgA)"))
  expect_identical(.worker_memory_line(.shard_telemetry(list(NULL))$workers),
                   "worker memory: no memory figures")
})

test_that("the shard prints its memory lines and writes the peaks to run-status.json alone", {
  skip_on_os("windows")
  out_dir <- withr::local_tempdir()
  .local_global("WORK_DIR", withr::local_tempdir())
  stub <- .stub_memory_bin(withr::local_tempdir(), lines = c(
    "pkgA/1.0" = .mem_stats(1382400, 1458176, 1045000000, 0),
    "pkgA/1.1" = .mem_stats(102400, 2097152, 1048576, 0),
    "pkgB/1.0" = .mem_stats(204800, 409600, 2097152, 1)))
  withr::local_envvar(RPKG_ANALYZER_BIN = stub, RPA_CACHE = NA)
  pkg_df <- data.frame(package = c("pkgA", "pkgB", "pkgC"),
                       latest_version = c("1.1", "1.0", "1.0"), stringsAsFactors = FALSE)
  log <- capture.output(run_update(
    .fake_io(pkg_df, version_map = list(pkgA = c("1.0", "1.1"))), out_dir, shard_size = 10L))

  expect_true(paste0(
    "analyzer memory: peak resident 1350.0 MiB (pkgA), peak virtual 2048.0 MiB (pkgA), ",
    "largest data kept 996.6 MiB, over the data budget: pkgB") %in% log)
  expect_length(grep("^worker memory: ", log), 1L)
  expect_match(grep("^worker memory: ", log, value = TRUE),
               "largest result [0-9.]+ MiB \\(pkg[ABC]\\)$")

  st <- .run_status(out_dir)
  expect_identical(st$analyzer_stats$runs, 4L)
  expect_equal(st$analyzer_stats$peak_rss_kb, 1382400)
  expect_identical(st$analyzer_stats$peak_rss_package, "pkgA")
  expect_equal(st$analyzer_stats$peak_vm_kb, 2097152)
  expect_identical(st$analyzer_stats$peak_vm_package, "pkgA")
  expect_equal(st$analyzer_stats$data_kept_max, 1045000000)
  expect_identical(st$analyzer_stats$data_over_budget, list("pkgB"))
  expect_equal(st$analyzer_stats$peaks, list(
    list(package = "pkgA", peak_rss_kb = 1382400, peak_vm_kb = 2097152),
    list(package = "pkgB", peak_rss_kb = 204800, peak_vm_kb = 409600)))
  expect_setequal(names(st$worker_memory),
                  c("peak_rss_kb", "peak_rss_package", "result_bytes", "result_package"))
  expect_gt(st$worker_memory$result_bytes, 0)
  for (f in c("code-manifest.json", "data-manifest.json")) {
    published <- jsonlite::fromJSON(file.path(out_dir, f), simplifyVector = FALSE)
    expect_false(any(c("worker_memory", "peaks", "peak_rss_kb") %in%
                       c(names(published), names(published$bootstrap))), info = f)
  }
})

test_that("a build that writes no memory keys says so, and run-status.json holds nulls", {
  skip_on_os("windows")
  out_dir <- withr::local_tempdir()
  .local_global("WORK_DIR", withr::local_tempdir())
  withr::local_envvar(RPKG_ANALYZER_BIN = .stub_stats_bin(withr::local_tempdir()),
                      STUB_SEEN = NA, RPA_CACHE = NA)
  pkg_df <- data.frame(package = "pkgA", latest_version = "1.0", stringsAsFactors = FALSE)
  log <- capture.output(run_update(.fake_io(pkg_df), out_dir, shard_size = 10L))
  expect_true("analyzer memory: no memory figures from this build" %in% log)
  st <- .run_status(out_dir)$analyzer_stats
  expect_true(all(c("peak_rss_kb", "peak_rss_package", "peak_vm_kb", "peak_vm_package",
                    "data_kept_max", "data_over_budget", "peaks") %in% names(st)))
  expect_null(st$peak_rss_kb)
  expect_null(st$peak_vm_package)
  expect_identical(st$data_over_budget, list())
  expect_identical(st$peaks, list())
})

# ---------------------------------------------------------------------------
# The analyzer's exit status on the worker's line
# ---------------------------------------------------------------------------

test_that(".worker_line carries the analyzer's non-zero exits, ok or not", {
  expect_identical(
    .worker_line(2L, 9L, TRUE, "pkgA", "ok", 3L, 1.5, analyzer_exit = "101 x2"),
    "[2/9] ok pkgA: 3 versions in 1.5s [analyzer exit 101 x2]\n")
  expect_identical(
    .worker_line(1L, 9L, FALSE, "pkgB", "crash", 0L, 0.5, "it died", analyzer_exit = "134 x1"),
    "[1/9] FAIL pkgB: crash after 0.5s [analyzer exit 134 x1]: it died\n")
  expect_identical(.worker_line(2L, 9L, TRUE, "pkgA", "ok", 3L, 1.5, analyzer_exit = ""),
                   .worker_line(2L, 9L, TRUE, "pkgA", "ok", 3L, 1.5))
})

test_that("the exits a worker counted read as status and count, and as nothing when all were 0", {
  expect_identical(.analyzer_exit_text(list(analyzer_s = 2, package_s = 3)), "")
  expect_identical(.analyzer_exit_text(list(analyzer_exit_134 = 1, analyzer_exit_101 = 2,
                                            analyzer_s = 2)), "101 x2, 134 x1")
})

test_that("a package whose analyzer exited non-zero prints its line with the status", {
  skip_on_os("windows")
  out_dir <- withr::local_tempdir()
  .local_global("WORK_DIR", withr::local_tempdir())
  # One core, so the lines the workers print are captured here.
  .local_global("ANALYSIS_CORES", 1L)
  # pkgA takes the R fallback on both releases, pkgB is killed, pkgC and pkgD pass.
  stub <- .stub_memory_bin(withr::local_tempdir(), exits = c(
    "pkgA/1.0" = 101L, "pkgA/1.1" = 101L, "pkgB/1.0" = 134L))
  withr::local_envvar(RPKG_ANALYZER_BIN = stub, RPA_CACHE = NA)
  pkg_df <- data.frame(package = c("pkgA", "pkgB", "pkgC", "pkgD"),
                       latest_version = c("1.1", "1.0", "1.0", "1.0"), stringsAsFactors = FALSE)
  log <- capture.output(run_update(
    .fake_io(pkg_df, version_map = list(pkgA = c("1.0", "1.1"))), out_dir, shard_size = 10L))
  expect_length(grep("^\\[1/4\\] ok pkgA: 2 versions in [0-9.]+s \\[analyzer exit 101 x2\\]$",
                     log), 1L)
  expect_length(grep(paste0("^\\[2/4\\] FAIL pkgB: crash after [0-9.]+s ",
                            "\\[analyzer exit 134 x1\\]: rpkg-analyzer was killed"), log), 1L)
  # A package whose analyzer exited 0 is still thinned out of the log.
  expect_length(grep("pkgC", log), 0L)
})
