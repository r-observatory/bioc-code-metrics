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

test_that("sharded bootstrap: three runs cover universe of 5, fourth is no-op", {
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
