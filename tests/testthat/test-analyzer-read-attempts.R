# tests/testthat/test-analyzer-read-attempts.R
#
# The third state of a dataset scan. datasets_scanned answers one question with
# two answers: the reader ran (whatever it found), or it did not. A package the
# analyzer was asked about and could not read is neither, and while it had no
# record of its own it stayed in the backfill queue for good: every run
# re-analysed it, every run reported a change, and the workflow published a
# dated release for a database that had not moved.
#
# The record is the same shape the pipeline already uses for a package that
# cannot be cloned: a count, a cap, and no place in the queue past it.

.dsa_analyzer <- function(dir, version) {
  stub <- file.path(dir, "stub-analyzer.sh")
  writeLines(c(
    "#!/bin/sh",
    'if [ "$1" = "--version" ]; then',
    sprintf('  echo "rpkg-analyzer %s"', version),
    "  exit 0",
    "fi",
    # The one package it reads is the self-check a 0.5.0 build is asked first.
    'dir=$(echo "$1" | tr -d "\'")',
    'if grep -q "^Package: selfcheck" "$dir/DESCRIPTION" 2>/dev/null; then',
    '  echo "{\\"rec\\":\\"summary\\",\\"input_kind\\":\\"git\\"}"',
    "  exit 0",
    "fi",
    # A binary that answers for itself and fails on the package is what
    # "installed, and cannot read this one" looks like in production.
    "exit 1"), stub)
  Sys.chmod(stub, mode = "0755")
  stub
}

# A real git repo with one RELEASE_1_0 branch, so the real analyze_package runs
# over it and list_versions reads version "1.0" from the branch name.
.dsa_clone <- function(pkg, dest) {
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  system2("git", c("init", dest), stdout = FALSE, stderr = FALSE)
  system2("git", c("-C", dest, "config", "user.email", "t@example.com"),
          stdout = FALSE, stderr = FALSE)
  system2("git", c("-C", dest, "config", "user.name", "Test Bot"),
          stdout = FALSE, stderr = FALSE)
  writeLines("# readme", file.path(dest, "README"))
  system2("git", c("-C", dest, "add", "-A"), stdout = FALSE, stderr = FALSE)
  system2("git", c("-C", dest, "commit", "-m", "init"), stdout = FALSE, stderr = FALSE)
  system2("git", c("-C", dest, "checkout", "-b", "RELEASE_1_0"),
          stdout = FALSE, stderr = FALSE)
  writeLines(c("Package: pkgA", "Version: 1.0", "Title: Test Package",
               "Description: Minimal package for the dataset scan tests.",
               "Author: Test Bot", "Maintainer: Test Bot <t@example.com>",
               "License: MIT"), file.path(dest, "DESCRIPTION"))
  writeLines("export(hello)", file.path(dest, "NAMESPACE"))
  dir.create(file.path(dest, "R"), showWarnings = FALSE)
  writeLines("hello <- function() 'hello'", file.path(dest, "R", "hello.R"))
  system2("git", c("-C", dest, "add", "-A"), stdout = FALSE, stderr = FALSE)
  system2("git", c("-C", dest, "commit", "-m", "release-1.0"),
          stdout = FALSE, stderr = FALSE)
  system2("git", c("-C", dest, "checkout", "-"), stdout = FALSE, stderr = FALSE)
  TRUE
}

# latest_version is the Bioconductor release the universe is keyed on, which is
# what the RELEASE_1_0 branch above becomes.
.dsa_io <- function() list(
  package_list = function() data.frame(package = "pkgA", latest_version = "1.0",
                                       stringsAsFactors = FALSE),
  clone = .dsa_clone)

# Four consecutive runs over a universe that does not change. The sequence of
# `changed` is what the workflow publishes on, so it is the thing under test.
.dsa_four_runs <- function(out) {
  io <- .dsa_io()
  vapply(1:4, function(i) isTRUE(suppressWarnings(
    run_update(io, out, shard_size = 10L))$changed), logical(1L))
}

# ---------------------------------------------------------------------------
# The record itself
# ---------------------------------------------------------------------------

test_that("an unread dataset scan is counted until the package stops being asked", {
  con <- open_or_init_db(withr::local_tempfile(fileext = ".db"))
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  expect_equal(.analyzer_read_exhausted(con), character(0L))
  for (i in seq_len(MAX_ANALYZER_READ_ATTEMPTS - 1L)) {
    .record_analyzer_read_attempt(con, "pkgA", "0.4.0-test")
    expect_equal(.analyzer_read_exhausted(con), character(0L))
  }
  .record_analyzer_read_attempt(con, "pkgA", "0.4.0-test")
  expect_equal(.analyzer_read_exhausted(con), "pkgA")
  expect_identical(.n_datasets_unreadable(con), 1L)
})

test_that("a scan that reads the package forgets the attempts before it", {
  # Otherwise a package that failed once on a bad day carries that count for
  # the rest of the build's life and gives up sooner than it should.
  con <- open_or_init_db(withr::local_tempfile(fileext = ".db"))
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  .record_analyzer_read_attempt(con, "pkgA", "0.4.0-test")
  .clear_analyzer_read_attempts(con, "pkgA")
  expect_identical(.n_datasets_unreadable(con), 0L)
  expect_equal(
    DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM bioc_analyzer_read_attempts")$n, 0L)
})

test_that("a new analyzer build asks a package it gave up on again", {
  # A count that cannot come down is a package retired for good on the say-so
  # of one build. The reader that could not read it is part of the record, so
  # the next reader starts from nothing.
  con <- open_or_init_db(withr::local_tempfile(fileext = ".db"))
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  for (i in seq_len(MAX_ANALYZER_READ_ATTEMPTS)) {
    .record_analyzer_read_attempt(con, "pkgA", "0.4.0-test")
  }
  expect_equal(.analyzer_read_exhausted(con), "pkgA")

  expect_equal(.forget_other_builds_read_attempts(con, "0.5.0-test"), 1L)
  expect_equal(.analyzer_read_exhausted(con), character(0L))
})

test_that("a run that cannot name its analyzer forgets nothing", {
  # The no-binary run records attempts with no build against them. Clearing
  # those on a run that also cannot name a build would reset the count every
  # time and the queue would never drain, which is the whole failure.
  con <- open_or_init_db(withr::local_tempfile(fileext = ".db"))
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  for (i in seq_len(MAX_ANALYZER_READ_ATTEMPTS)) {
    .record_analyzer_read_attempt(con, "pkgA", NA_character_)
  }
  expect_equal(.forget_other_builds_read_attempts(con, NA_character_), 0L)
  expect_equal(.analyzer_read_exhausted(con), "pkgA")
})

# ---------------------------------------------------------------------------
# End to end: the three states of a dataset scan, over four runs each
# ---------------------------------------------------------------------------

test_that("a package the analyzer cannot read leaves the queue instead of never settling", {
  skip_on_os("windows")
  stub_dir <- withr::local_tempdir()
  withr::local_envvar(RPKG_ANALYZER_BIN = .dsa_analyzer(stub_dir, "0.4.0-test"))

  out <- withr::local_tempdir()
  changed <- .dsa_four_runs(out)

  # It may take the cap to get there; what it may not do is never get there.
  expect_true(changed[[1L]])
  expect_false(changed[[4L]])

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, DB_FILENAME))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  # Still honestly unread. The package aged out of the queue; it was never
  # scanned, and nothing in the database says it was.
  expect_true(all(is.na(
    DBI::dbGetQuery(con, "SELECT datasets_scanned FROM bioc_code_summary")[[1L]])))
  expect_identical(.n_datasets_unreadable(con), 1L)
})

test_that("the count of packages nobody could read reaches both manifests", {
  skip_on_os("windows")
  stub_dir <- withr::local_tempdir()
  withr::local_envvar(RPKG_ANALYZER_BIN = .dsa_analyzer(stub_dir, "0.4.0-test"))

  out <- withr::local_tempdir()
  .dsa_four_runs(out)

  cm <- jsonlite::fromJSON(file.path(out, "code-manifest.json"))
  dm <- jsonlite::fromJSON(file.path(out, "data-manifest.json"))
  # How many packages the pipeline has stopped asking about, which is the
  # number that does not come down on its own.
  expect_identical(cm$bootstrap$n_datasets_unreadable, 1L)
  expect_identical(dm$bootstrap$n_datasets_unreadable, 1L)
})

test_that("a run with no analyzer at all settles too", {
  # Deliberate: production installs the binary, so this is a local run or a
  # degraded download. Converging late is better than a marker that claims a
  # scan nothing performed.
  skip_on_os("windows")
  withr::local_envvar(RPKG_ANALYZER_BIN = "")
  skip_if(nzchar(rpkg_analyzer_bin()), "rpkg-analyzer is on PATH")

  out <- withr::local_tempdir()
  changed <- suppressWarnings(.dsa_four_runs(out))

  expect_true(changed[[1L]])
  expect_false(changed[[4L]])

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, DB_FILENAME))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_true(all(is.na(
    DBI::dbGetQuery(con, "SELECT datasets_scanned FROM bioc_code_summary")[[1L]])))
  expect_identical(.n_datasets_unreadable(con), 1L)
})

test_that("a package the reader did read keeps no attempt record and settles at once", {
  skip_on_os("windows")
  skip_if(!nzchar(rpkg_analyzer_bin()), "no rpkg-analyzer binary to read with")

  out <- withr::local_tempdir()
  changed <- .dsa_four_runs(out)
  expect_equal(changed, c(TRUE, FALSE, FALSE, FALSE))

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, DB_FILENAME))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  # The package ships no data. The reader ran and found none, which is a scan.
  expect_true(any(!is.na(
    DBI::dbGetQuery(con, "SELECT datasets_scanned FROM bioc_code_summary")[[1L]])))
  expect_identical(.n_datasets_unreadable(con), 0L)
})

test_that("a package given up on is asked again by the next analyzer build", {
  # The cap is a verdict about one reader. A count that outlived its reader
  # would retire a package for good on the say-so of a build nobody runs any
  # more, and the row it protects carries no scan marker, so the marker-based
  # invalidation cannot reach it either.
  skip_on_os("windows")
  stub_dir <- withr::local_tempdir()
  withr::local_envvar(RPKG_ANALYZER_BIN = .dsa_analyzer(stub_dir, "0.4.0-test"))

  out <- withr::local_tempdir()
  io  <- .dsa_io()
  for (i in seq_len(MAX_ANALYZER_READ_ATTEMPTS)) {
    suppressWarnings(run_update(io, out, shard_size = 10L))
  }
  settled <- suppressWarnings(run_update(io, out, shard_size = 10L))
  expect_false(settled$changed)

  # A new build arrives. Same package, same failure, but nothing here has been
  # asked of this reader yet.
  .dsa_analyzer(stub_dir, "0.5.0-test")
  retried <- suppressWarnings(run_update(io, out, shard_size = 10L))
  expect_equal(retried$n_fresh, 1L)
  expect_true(retried$changed)
})

# ---------------------------------------------------------------------------
# A build declared to reproduce the stored rows' build re-queues nothing
# ---------------------------------------------------------------------------

# A stub build that reads pkgA: one summary and one dataset record.
.dsa_reading_analyzer <- function(dir, version) {
  stub <- file.path(dir, "stub-analyzer.sh")
  writeLines(c(
    "#!/bin/sh",
    'if [ "$1" = "--version" ]; then',
    sprintf('  echo "rpkg-analyzer %s"', version),
    "  exit 0",
    "fi",
    'echo "{\\"rec\\":\\"summary\\",\\"input_kind\\":\\"git\\",\\"loc_r\\":1,\\"n_fns_r\\":1}"',
    paste0('echo "{\\"rec\\":\\"dataset\\",\\"name\\":\\"d\\",\\"file\\":\\"data/d.rda\\",',
           '\\"class\\":\\"data.frame\\",\\"kind\\":\\"table\\",\\"confidence\\":\\"exact\\",',
           '\\"content_fp\\":\\"cf\\",\\"schema_fp\\":\\"sf\\"}"'),
    "exit 0"), stub)
  Sys.chmod(stub, mode = "0755")
  stub
}

# A call's value and every message it raised, one trimmed line each.
.oc_messages <- function(expr) {
  msgs <- character(0L)
  value <- withCallingHandlers(suppressWarnings(expr), message = function(m) {
    msgs <<- c(msgs, trimws(conditionMessage(m)))
    invokeRestart("muffleMessage")
  })
  list(value = value, messages = msgs)
}

test_that("a build in the stored rows' output class re-queues nothing, and the run says so", {
  skip_on_os("windows")
  stub_dir <- withr::local_tempdir()
  withr::local_envvar(RPKG_ANALYZER_BIN = .dsa_reading_analyzer(stub_dir, "0.4.0-test"))
  old <- ANALYZER_SAME_OUTPUT
  ANALYZER_SAME_OUTPUT <<- c("0.4.0-test", "0.4.1-test")
  on.exit(ANALYZER_SAME_OUTPUT <<- old, add = TRUE)

  out <- withr::local_tempdir()
  io  <- .dsa_io()
  expect_identical(suppressWarnings(run_update(io, out, shard_size = 10L))$n_fresh, 1L)

  .dsa_reading_analyzer(stub_dir, "0.4.1-test")
  same <- .oc_messages(run_update(io, out, shard_size = 10L))
  expect_identical(same$value$n_fresh, 0L)
  expect_false(same$value$changed)
  expect_true("dataset scans invalidated by analyzer change: 0" %in% same$messages)
  expect_true("packages to re-read under this analyzer: 0" %in% same$messages)
  expect_true(paste("analyzer 0.4.1-test, output class 0.4.0-test 0.4.1-test;",
                    "latest rows on class: 1 of 1") %in% same$messages)
  boot <- jsonlite::fromJSON(file.path(out, "code-manifest.json"), simplifyVector = FALSE)$bootstrap
  expect_identical(boot$analyzer_version, "0.4.1-test")
  expect_identical(boot$output_class, list("0.4.0-test", "0.4.1-test"))
  expect_identical(boot$n_latest_on_build, 1L)

  .dsa_reading_analyzer(stub_dir, "0.4.2-test")
  other <- .oc_messages(run_update(io, out, shard_size = 10L))
  expect_true("dataset scans invalidated by analyzer change: 1" %in% other$messages)
  expect_identical(other$value$n_fresh, 1L)
  status <- jsonlite::fromJSON(file.path(out, "run-status.json"), simplifyVector = FALSE)
  expect_identical(status$output_class, list("0.4.2-test"))
  expect_identical(status$n_latest_on_build, 1L)
})

test_that("0.4.0 rows still go back in the queue, and a move from 0.5.0 to 0.5.1 re-queues nothing", {
  skip_on_os("windows")
  stub_dir <- withr::local_tempdir()
  withr::local_envvar(RPKG_ANALYZER_BIN = .dsa_reading_analyzer(stub_dir, "0.4.0"))
  out <- withr::local_tempdir()
  io  <- .dsa_io()
  expect_identical(suppressWarnings(run_update(io, out, shard_size = 10L))$n_fresh, 1L)

  .dsa_reading_analyzer(stub_dir, "0.5.0")
  on_050 <- .oc_messages(run_update(io, out, shard_size = 10L))
  expect_true("dataset scans invalidated by analyzer change: 1" %in% on_050$messages)
  expect_identical(on_050$value$n_fresh, 1L)

  .dsa_reading_analyzer(stub_dir, "0.5.1")
  on_051 <- .oc_messages(run_update(io, out, shard_size = 10L))
  expect_true("dataset scans invalidated by analyzer change: 0" %in% on_051$messages)
  expect_true("packages to re-read under this analyzer: 0" %in% on_051$messages)
  expect_true(paste("analyzer 0.5.1, output class 0.5.0 0.5.1;",
                    "latest rows on class: 1 of 1") %in% on_051$messages)
  expect_identical(on_051$value$n_fresh, 0L)
})
