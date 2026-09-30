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
  assign("analyze_with_binary", function(dir) value, envir = env)
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
  .local_global("analyze_with_binary", function(dir, kind = ANALYZER_INPUT_KIND) NULL)
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
  .local_global("analyze_with_binary", function(dir, kind = ANALYZER_INPUT_KIND) metrics)
  # read_at is the clock at each reading, so it differs between any two runs.
  analyse <- function() {
    res <- analyze_package(repo, "pkgCap")
    res$text$versions$read_at <- NULL
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
