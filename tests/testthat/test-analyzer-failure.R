# tests/testthat/test-analyzer-failure.R: an analyzer that is killed, or that
# fails on a version with analyzer rows, fails the package and leaves its rows.

# A stub analyzer of build `version`. It answers --version and, on the versions
# named in `bad` ("all" for every one), ends as `ending` says. After a summary:
# "abort" (exit 134), "kill" (exit 137), "panic" (exit 101), "silent" (exit 0,
# no statistics line), "cut" (the same, mid-record), "notfound" (exit 127, a
# command that does not exist). Finished, with a statistics line: "empty" (no
# output) and "nosummary" (a record, no summary). Any other run prints a
# summary, appends its statistics line and exits 0. The self-check package
# counts as a version only when spare_selfcheck is FALSE.
.af_stub <- function(dir, ending = "ok", bad = "all", version = "0.5.1",
                     spare_selfcheck = TRUE) {
  summary <- sprintf(
    'echo "{\\"rec\\":\\"summary\\",\\"input_kind\\":\\"%s\\",\\"loc_r\\":1,\\"n_fns_r\\":1}"',
    ANALYZER_INPUT_KIND)
  stats <- paste0('if [ -n "$RPKG_ANALYZER_STATS" ]; then ',
                  'echo "{\\"build\\":\\"stub\\",\\"ms\\":1}" >> "$RPKG_ANALYZER_STATS"; fi')
  body <- switch(ending,
    ok        = NULL,
    abort     = c(summary, "exit 134"),
    kill      = c(summary, "exit 137"),
    panic     = c(summary, "exit 101"),
    silent    = c(summary, "exit 0"),
    cut       = c(summary, 'printf "%s" "{\\"rec\\":\\"function\\",\\"name\\":"', "exit 0"),
    notfound  = c(summary, '/nonexistent/rpkg-analyzer "$1"', "exit $?"),
    empty     = c(stats, "exit 0"),
    nosummary = c('echo "{\\"rec\\":\\"function\\",\\"lang\\":\\"r\\",\\"name\\":\\"f\\"}"',
                  stats, "exit 0"))
  spared <- if (isTRUE(spare_selfcheck)) '[ "$p" != selfcheck ] && ' else ""
  stub <- file.path(dir, "stub-af.sh")
  writeLines(c(
    "#!/bin/sh",
    sprintf('if [ "$1" = "--version" ]; then echo "rpkg-analyzer %s"; exit 0; fi', version),
    'p=$(sed -n "s/^Package: *//p" "$1/DESCRIPTION" | head -1)',
    'v=$(sed -n "s/^Version: *//p" "$1/DESCRIPTION" | head -1)',
    if (!is.null(body)) c(
      sprintf('if %s{ [ "%s" = all ] || [ "$v" = "%s" ]; }; then', spared, bad, bad),
      paste0("  ", body),
      "fi"),
    summary, stats, "exit 0"), stub)
  Sys.chmod(stub, mode = "0755")
  stub
}

# f() with no analyzer anywhere, whatever RPKG_ANALYZER_BIN and PATH say.
.af_without_binary <- function(f) {
  .local_global("rpkg_analyzer_bin", function() "")
  f()
}

# An extracted package, version 1.0.
.af_pkg_dir <- function(frame = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = frame)
  writeLines(c("Package: demo", "Version: 1.0"), file.path(dir, "DESCRIPTION"))
  dir
}

# What analyze_with_binary returned, or the condition it raised.
.af_outcome <- function(...) tryCatch(analyze_with_binary(...), error = function(e) e)

# A git repository with one release branch per version. The identity rides on
# each command, so no git configuration is written.
.af_clone <- function(pkg, dest, versions) {
  git <- function(...) {
    system2("git", c("-C", shQuote(dest), "-c", "user.name=TestBot",
                     "-c", "user.email=test@example.com", ...),
            stdout = FALSE, stderr = FALSE)
  }
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  system2("git", c("init", "-q", shQuote(dest)), stdout = FALSE, stderr = FALSE)
  writeLines(paste("#", pkg), file.path(dest, "README"))
  git("add", "-A")
  git("commit", "-q", "-m", "init")
  for (ver in versions) {
    git("checkout", "-q", "-b", paste0("RELEASE_", gsub(".", "_", ver, fixed = TRUE)))
    writeLines(c(paste("Package:", pkg), paste("Version:", ver), "Title: Test Package",
                 "Description: A package for the analyzer failure tests.",
                 "License: MIT"), file.path(dest, "DESCRIPTION"))
    writeLines("export(hello)", file.path(dest, "NAMESPACE"))
    dir.create(file.path(dest, "R"), showWarnings = FALSE)
    writeLines(c(paste("## Version", ver), "hello <- function() 'hello'"),
               file.path(dest, "R", "hello.R"))
    git("add", "-A")
    git("commit", "-q", "-m", paste0("release-", ver))
    git("checkout", "-q", "-")
  }
  TRUE
}

# A universe of `pkgs`, each with the release branches `versions`.
.af_io <- function(versions, pkgs = "pkgA") list(
  package_list = function() data.frame(
    package = pkgs, latest_version = versions[[length(versions)]],
    stringsAsFactors = FALSE),
  clone = function(pkg, dest) .af_clone(pkg, dest, versions))

# One run_update in a work directory of its own; its manifest.
.af_run <- function(io, out, ...) {
  .local_global("WORK_DIR", withr::local_tempdir())
  capture.output(m <- suppressWarnings(run_update(io, out, shard_size = 10L, ...)))
  m
}

.af_query <- function(out, sql, pkg) {
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, DB_FILENAME))
  on.exit(DBI::dbDisconnect(con))
  DBI::dbGetQuery(con, sql, params = list(pkg))
}

# The failure verdict of a package.
.af_verdict <- function(out, pkg = "pkgA") .af_query(out, "
  SELECT stage, analyzer_version, timeout_failures, analyze_failures, reason
    FROM bioc_metrics_failures WHERE package = ?", pkg)

# The build named on each stored summary row of a package, by version.
.af_stamps <- function(out, pkg = "pkgA") {
  rows <- .af_query(out, sprintf(
    'SELECT * FROM "%s" WHERE package = ? ORDER BY version', SUMMARY_TABLE), pkg)
  stats::setNames(as.character(rows$analyzer_version %||% rep(NA, nrow(rows))),
                  rows$version)
}

# ---------------------------------------------------------------------------
# What analyze_with_binary makes of an exit status
# ---------------------------------------------------------------------------

test_that("a status of 128 or above is analyzer_killed, protected or not", {
  skip_on_os("windows")
  pkg <- .af_pkg_dir()
  withr::local_envvar(RPKG_ANALYZER_STATS = NA)
  status <- c(abort = 134L, kill = 137L)
  for (ending in names(status)) {
    withr::local_envvar(RPKG_ANALYZER_BIN = .af_stub(withr::local_tempdir(), ending))
    for (got in list(.af_outcome(pkg, protect = TRUE), .af_outcome(pkg))) {
      expect_s3_class(got, c("analyzer_killed", "error", "condition"), exact = TRUE)
      expect_identical(got$status, status[[ending]])
      expect_match(conditionMessage(got), as.character(status[[ending]]), fixed = TRUE)
    }
  }
})

test_that("any other non-zero status is analyzer_failed on a protected version", {
  skip_on_os("windows")
  pkg <- .af_pkg_dir()
  withr::local_envvar(RPKG_ANALYZER_STATS = NA,
                      RPKG_ANALYZER_BIN = .af_stub(withr::local_tempdir(), "panic"))
  got <- .af_outcome(pkg, protect = TRUE)
  expect_s3_class(got, c("analyzer_failed", "error", "condition"), exact = TRUE)
  expect_identical(got$status, 101L)
  expect_match(conditionMessage(got), "101", fixed = TRUE)
})

test_that("any other non-zero status on an unprotected version gives the R fallback", {
  skip_on_os("windows")
  pkg <- .af_pkg_dir()
  withr::local_envvar(RPKG_ANALYZER_STATS = NA,
                      RPKG_ANALYZER_BIN = .af_stub(withr::local_tempdir(), "panic"))
  expect_null(analyze_with_binary(pkg))
  expect_null(.af_outcome(pkg, protect = FALSE))
})

test_that("a zero status without the statistics line is analyzer_killed", {
  skip_on_os("windows")
  pkg   <- .af_pkg_dir()
  stats <- withr::local_tempfile()
  for (ending in c("silent", "cut")) {
    withr::local_envvar(RPKG_ANALYZER_STATS = stats,
                        RPKG_ANALYZER_BIN = .af_stub(withr::local_tempdir(), ending))
    for (got in list(.af_outcome(pkg, protect = TRUE), .af_outcome(pkg))) {
      expect_s3_class(got, c("analyzer_killed", "error", "condition"), exact = TRUE)
      expect_true(is.na(got$status))
    }
  }
  # A line an earlier release of the package left is not this run's.
  earlier <- '{"build":"stub","ms":1}'
  writeLines(earlier, stats)
  expect_s3_class(.af_outcome(pkg), "analyzer_killed")
  expect_identical(readLines(stats), earlier)
})

test_that("a zero status is accepted with its line, from a build that writes none, or with no file named", {
  skip_on_os("windows")
  pkg   <- .af_pkg_dir()
  stats <- withr::local_tempfile()
  withr::local_envvar(RPKG_ANALYZER_STATS = stats,
                      RPKG_ANALYZER_BIN = .af_stub(withr::local_tempdir()))
  expect_identical(analyze_with_binary(pkg, protect = TRUE)$loc_r, 1L)
  expect_length(readLines(stats), 1L)

  withr::local_envvar(RPKG_ANALYZER_BIN = .af_stub(withr::local_tempdir(), "silent",
                                                   version = "0.5.0"))
  expect_identical(analyze_with_binary(pkg, protect = TRUE)$loc_r, 1L)

  withr::local_envvar(RPKG_ANALYZER_STATS = NA,
                      RPKG_ANALYZER_BIN = .af_stub(withr::local_tempdir(), "silent"))
  expect_identical(analyze_with_binary(pkg, protect = TRUE)$loc_r, 1L)
})

test_that("the analyzer CI installs appends its statistics line, so a finished run is accepted", {
  skip_on_os("windows")
  skip_if(!nzchar(rpkg_analyzer_bin()), "needs rpkg-analyzer")
  skip_if(!analyzer_at_least(rpkg_analyzer_version(), "0.5.1"),
          "this build writes no statistics line")
  pkg   <- .af_pkg_dir()
  stats <- withr::local_tempfile()
  withr::local_envvar(RPKG_ANALYZER_STATS = stats)
  expect_false(is.null(analyze_with_binary(pkg, protect = TRUE)))
  expect_length(readLines(stats), 1L)
})

# ---------------------------------------------------------------------------
# No usable result without a signal: not run, no summary, no analyzer
# ---------------------------------------------------------------------------

test_that("an analyzer that cannot be run is analyzer_failed on a protected version", {
  skip_on_os("windows")
  pkg <- .af_pkg_dir()
  withr::local_envvar(RPKG_ANALYZER_STATS = NA,
                      RPKG_ANALYZER_BIN = .af_stub(withr::local_tempdir(), "notfound"))
  got <- .af_outcome(pkg, protect = TRUE)
  expect_s3_class(got, c("analyzer_failed", "error", "condition"), exact = TRUE)
  expect_true(is.na(got$status))
  expect_null(analyze_with_binary(pkg))
})

test_that("a finished run with no summary record is analyzer_failed on a protected version", {
  skip_on_os("windows")
  pkg   <- .af_pkg_dir()
  stats <- withr::local_tempfile()
  for (ending in c("empty", "nosummary")) {
    withr::local_envvar(RPKG_ANALYZER_STATS = stats,
                        RPKG_ANALYZER_BIN = .af_stub(withr::local_tempdir(), ending))
    got <- .af_outcome(pkg, protect = TRUE)
    expect_s3_class(got, c("analyzer_failed", "error", "condition"), exact = TRUE)
    expect_true(is.na(got$status))
    expect_null(analyze_with_binary(pkg))
    # The same from a build that writes no statistics line.
    withr::local_envvar(RPKG_ANALYZER_STATS = NA, RPKG_ANALYZER_BIN = .af_stub(
      withr::local_tempdir(), ending, version = "0.5.0"))
    expect_s3_class(.af_outcome(pkg, protect = TRUE), "analyzer_failed")
    expect_null(analyze_with_binary(pkg))
  }
  expect_length(readLines(stats), 4L)
})

test_that("a missing analyzer is analyzer_failed on a protected version", {
  pkg <- .af_pkg_dir()
  got <- .af_without_binary(function() .af_outcome(pkg, protect = TRUE))
  expect_s3_class(got, c("analyzer_failed", "error", "condition"), exact = TRUE)
  expect_true(is.na(got$status))
  expect_null(.af_without_binary(function() analyze_with_binary(pkg)))
})

# ---------------------------------------------------------------------------
# The self-check
# ---------------------------------------------------------------------------

test_that("the self-check reads a killed analyzer as a failed check", {
  skip_on_os("windows")
  withr::local_envvar(RPKG_ANALYZER_STATS = NA)
  for (ending in c("abort", "kill")) {
    withr::local_envvar(RPKG_ANALYZER_BIN = .af_stub(
      withr::local_tempdir(), ending, spare_selfcheck = FALSE))
    expect_false(rpkg_analyzer_selfcheck())
  }
  withr::local_envvar(
    RPKG_ANALYZER_STATS = withr::local_tempfile(),
    RPKG_ANALYZER_BIN = .af_stub(withr::local_tempdir(), "silent", spare_selfcheck = FALSE))
  expect_false(rpkg_analyzer_selfcheck())
})

test_that("an analyzer killed on the self-check package stops the run before any shard", {
  skip_on_os("windows")
  withr::local_envvar(RPKG_ANALYZER_STATS = NA, RPKG_ANALYZER_BIN = .af_stub(
    withr::local_tempdir(), "kill", spare_selfcheck = FALSE))
  out <- withr::local_tempdir()
  expect_error(run_update(.af_io("1.0"), out, shard_size = 10L), "--input-kind git")
  expect_false(file.exists(file.path(out, DB_FILENAME)))
})

# ---------------------------------------------------------------------------
# Which versions are protected
# ---------------------------------------------------------------------------

test_that("the stamped versions of a package are its stored rows that name a build", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  none <- list(pkgA = character(0L), pkgB = character(0L))
  expect_identical(.stamped_versions(con, c("pkgA", "pkgB")), none)

  DBI::dbExecute(con, sprintf('CREATE TABLE "%s" (package TEXT, version TEXT)', SUMMARY_TABLE))
  DBI::dbExecute(con, sprintf('INSERT INTO "%s" VALUES (\'pkgA\', \'1.0\')', SUMMARY_TABLE))
  expect_identical(.stamped_versions(con, c("pkgA", "pkgB")), none)

  DBI::dbExecute(con, sprintf('ALTER TABLE "%s" ADD COLUMN analyzer_version TEXT', SUMMARY_TABLE))
  DBI::dbExecute(con, sprintf("INSERT INTO \"%s\" VALUES
    ('pkgA', '1.1', '0.4.0'), ('pkgA', '1.2', ''), ('pkgA', '1.3', '0.5.1'),
    ('pkgC', '1.0', '0.5.1')", SUMMARY_TABLE))
  expect_identical(.stamped_versions(con, c("pkgA", "pkgB")),
                   list(pkgA = c("1.1", "1.3"), pkgB = character(0L)))
  expect_identical(.stamped_versions(con, character(0L)),
                   stats::setNames(list(), character(0L)))
})

test_that("analyze_package protects the versions it is told are stamped", {
  skip_on_os("windows")
  repo <- file.path(withr::local_tempdir(), "pkgA")
  .af_clone("pkgA", repo, c("1.0", "1.1"))
  stub_dir <- withr::local_tempdir()
  withr::local_envvar(RPKG_ANALYZER_STATS = NA,
                      RPKG_ANALYZER_BIN = .af_stub(stub_dir, "panic"))

  err <- tryCatch(analyze_package(repo, "pkgA", stamped = "1.1"), error = function(e) e)
  expect_s3_class(err, "analyzer_failed")
  fallback <- analyze_package(repo, "pkgA")
  expect_identical(fallback$summary$version, c("1.0", "1.1"))
  expect_identical(fallback$binary_versions, character(0L))

  # The analyzer fails on 1.1 alone, and only 1.0 has an analyzer row.
  .af_stub(stub_dir, "panic", bad = "1.1")
  mixed <- analyze_package(repo, "pkgA", stamped = "1.0")
  expect_identical(mixed$binary_versions, "1.0")
  .af_stub(stub_dir, "kill", bad = "1.1")
  err <- tryCatch(analyze_package(repo, "pkgA", stamped = "1.0"), error = function(e) e)
  expect_s3_class(err, "analyzer_killed")
})

# ---------------------------------------------------------------------------
# The verdict, and the stored rows
# ---------------------------------------------------------------------------

test_that("a killed or failed analyzer is a crash, whatever the elapsed time", {
  for (e in list(.analyzer_killed(137L), .analyzer_killed(), .analyzer_failed(101L))) {
    expect_identical(.classify_failure(e, 1, worker_timeout = 600L), "crash")
    expect_identical(.classify_failure(e, 900, worker_timeout = 600L), "crash")
  }
  expect_identical(.failure_class("crash"), "timeout")
})

test_that("an analyzer failure on a version with analyzer rows is a crash and leaves the rows", {
  skip_on_os("windows")
  for (ending in c("abort", "kill", "panic", "silent", "notfound", "empty", "nosummary")) {
    out      <- withr::local_tempdir()
    stub_dir <- withr::local_tempdir()
    withr::local_envvar(RPKG_ANALYZER_STATS = NA, RPKG_ANALYZER_BIN = .af_stub(stub_dir))
    expect_identical(.af_run(.af_io("1.0"), out)$n_fresh, 1L, info = ending)
    before <- .package_rows(out, "pkgA")
    expect_identical(.af_stamps(out), c(`1.0` = "0.5.1"), info = ending)

    .af_stub(stub_dir, ending)
    failed <- .af_run(.af_io(c("1.0", "1.1")), out)
    expect_identical(failed$shard_failures$packages, "pkgA", info = ending)
    expect_identical(failed$n_fresh, 0L, info = ending)
    expect_identical(.package_rows(out, "pkgA"), before, info = ending)
    expect_identical(.af_stamps(out), c(`1.0` = "0.5.1"), info = ending)
    verdict <- .af_verdict(out)
    expect_identical(verdict[c("stage", "timeout_failures", "analyze_failures")],
                     data.frame(stage = "crash", timeout_failures = 1L, analyze_failures = 0L,
                                stringsAsFactors = FALSE), info = ending)
    expect_true(nzchar(verdict$reason), info = ending)
  }
})

test_that("a killed analyzer fails a package with no analyzer rows, and writes none", {
  skip_on_os("windows")
  for (ending in c("abort", "kill", "silent")) {
    out <- withr::local_tempdir()
    withr::local_envvar(RPKG_ANALYZER_STATS = NA,
                        RPKG_ANALYZER_BIN = .af_stub(withr::local_tempdir(), ending))
    failed <- .af_run(.af_io("1.0"), out)
    expect_identical(failed$shard_failures$packages, "pkgA", info = ending)
    expect_identical(failed$n_fresh, 0L, info = ending)
    expect_identical(sum(vapply(.package_rows(out, "pkgA"), nrow, integer(1L))), 0L,
                     info = ending)
    expect_identical(.af_verdict(out)[c("stage", "timeout_failures")],
                     data.frame(stage = "crash", timeout_failures = 1L,
                                stringsAsFactors = FALSE), info = ending)
  }
})

test_that("a failure short of a kill on a version with no analyzer row still gives the R fallback", {
  skip_on_os("windows")
  for (ending in c("panic", "notfound", "empty", "nosummary")) {
    out <- withr::local_tempdir()
    withr::local_envvar(RPKG_ANALYZER_STATS = NA,
                        RPKG_ANALYZER_BIN = .af_stub(withr::local_tempdir(), ending))
    first <- .af_run(.af_io("1.0"), out)
    expect_identical(c(first$n_fresh, first$shard_failures$count), c(1L, 0L), info = ending)
    expect_identical(.af_stamps(out), c(`1.0` = NA_character_), info = ending)
    # A row the R fallback wrote names no build, so it is not protected either.
    again <- .af_run(.af_io("1.0"), out)
    expect_identical(c(again$n_shard, again$n_fresh, again$shard_failures$count),
                     c(1L, 1L, 0L), info = ending)
    expect_identical(nrow(.af_verdict(out)), 0L, info = ending)
  }
})

test_that("with no analyzer, a package with no analyzer rows still gets the R fallback", {
  out   <- withr::local_tempdir()
  first <- .af_without_binary(function() .af_run(.af_io("1.0"), out))
  expect_identical(c(first$n_fresh, first$shard_failures$count), c(1L, 0L))
  expect_identical(.af_stamps(out), c(`1.0` = NA_character_))
})

# The verdict of a run with no analyzer names no build, so it parks the package
# only for runs that have none.
test_that("with no analyzer, a package with analyzer rows fails under no build and is asked again once one is back", {
  skip_on_os("windows")
  out <- withr::local_tempdir()
  withr::local_envvar(RPKG_ANALYZER_STATS = NA,
                      RPKG_ANALYZER_BIN = .af_stub(withr::local_tempdir()))
  .af_run(.af_io("1.0"), out)
  before <- .package_rows(out, "pkgA")
  io <- .af_io(c("1.0", "1.1"))

  .af_without_binary(function() {
    for (i in seq_len(MAX_TIMEOUT_FAILURES)) {
      expect_identical(.af_run(io, out)$shard_failures$packages, "pkgA")
    }
    expect_identical(.af_verdict(out)[c("stage", "analyzer_version", "timeout_failures")],
                     data.frame(stage = "crash", analyzer_version = "",
                                timeout_failures = MAX_TIMEOUT_FAILURES,
                                stringsAsFactors = FALSE))
    parked <- .af_run(io, out)
    expect_identical(c(parked$n_shard, parked$permanent_failures), c(0L, 1L))
  })
  expect_identical(.package_rows(out, "pkgA"), before)
  expect_identical(.af_stamps(out), c(`1.0` = "0.5.1"))

  back <- .af_run(io, out)
  expect_identical(c(back$n_shard, back$n_fresh, back$permanent_failures), c(1L, 1L, 0L))
  expect_identical(.af_stamps(out), c(`1.0` = "0.5.1", `1.1` = "0.5.1"))
  expect_identical(nrow(.af_verdict(out)), 0L)
})

test_that("a killed analyzer leaves the rows the R fallback wrote as they were", {
  skip_on_os("windows")
  out      <- withr::local_tempdir()
  stub_dir <- withr::local_tempdir()
  withr::local_envvar(RPKG_ANALYZER_STATS = NA,
                      RPKG_ANALYZER_BIN = .af_stub(stub_dir, "panic"))
  .af_run(.af_io("1.0"), out)
  before <- .package_rows(out, "pkgA")
  expect_identical(.af_stamps(out), c(`1.0` = NA_character_))

  .af_stub(stub_dir, "kill")
  failed <- .af_run(.af_io(c("1.0", "1.1")), out)
  expect_identical(failed$shard_failures$packages, "pkgA")
  expect_identical(.package_rows(out, "pkgA"), before)
  expect_identical(.af_verdict(out)$stage, "crash")
})

test_that("the stamped versions reach a forked worker, one package protected and one not", {
  skip_on_os("windows")
  .local_global("ANALYSIS_CORES", 2L)
  out      <- withr::local_tempdir()
  stub_dir <- withr::local_tempdir()
  withr::local_envvar(RPKG_ANALYZER_STATS = NA, RPKG_ANALYZER_BIN = .af_stub(stub_dir))
  .af_run(.af_io("1.0"), out)
  before <- .package_rows(out, "pkgA")

  .af_stub(stub_dir, "panic")
  mixed <- .af_run(.af_io(c("1.0", "1.1"), pkgs = c("pkgA", "pkgB")), out)
  expect_identical(mixed$shard_failures$packages, "pkgA")
  # pkgB's fallback rows add columns to the summary table, so compare pkgA's
  # rows in the columns they had.
  after <- .package_rows(out, "pkgA")
  expect_identical(names(after), names(before))
  for (tbl in names(before)) {
    expect_identical(after[[tbl]][names(before[[tbl]])], before[[tbl]], info = tbl)
  }
  expect_identical(.af_stamps(out), c(`1.0` = "0.5.1"))
  expect_identical(.af_verdict(out)$stage, "crash")
  expect_identical(.af_stamps(out, "pkgB"), c(`1.0` = NA_character_, `1.1` = NA_character_))
})

test_that("a package whose analyzer keeps failing parks after MAX_TIMEOUT_FAILURES, and a new build asks again", {
  skip_on_os("windows")
  out      <- withr::local_tempdir()
  stub_dir <- withr::local_tempdir()
  withr::local_envvar(RPKG_ANALYZER_STATS = NA, RPKG_ANALYZER_BIN = .af_stub(stub_dir))
  .af_run(.af_io("1.0"), out)
  before <- .package_rows(out, "pkgA")

  .af_stub(stub_dir, "panic")
  io <- .af_io(c("1.0", "1.1"))
  for (i in seq_len(MAX_TIMEOUT_FAILURES)) {
    expect_identical(.af_run(io, out)$n_shard, 1L)
  }
  expect_identical(.af_verdict(out)[c("stage", "timeout_failures", "analyze_failures")],
                   data.frame(stage = "crash", timeout_failures = MAX_TIMEOUT_FAILURES,
                              analyze_failures = 0L, stringsAsFactors = FALSE))
  parked <- .af_run(io, out)
  expect_identical(c(parked$n_shard, parked$permanent_failures), c(0L, 1L))
  expect_identical(.package_rows(out, "pkgA"), before)

  .af_stub(stub_dir, version = "0.6.0-test")
  again <- .af_run(io, out)
  expect_identical(c(again$n_shard, again$n_fresh, again$permanent_failures), c(1L, 1L, 0L))
  expect_identical(.af_stamps(out), c(`1.0` = "0.6.0-test", `1.1` = "0.6.0-test"))
  expect_identical(nrow(.af_verdict(out)), 0L)
})

# ---------------------------------------------------------------------------
# The address-space limit on the analyzer process
# ---------------------------------------------------------------------------

# A stand-in for prlimit that appends its first argument to `seen` and runs the
# rest of its command line, so the call can be read where there is no prlimit.
.af_fake_prlimit <- function(dir, seen) {
  fake <- file.path(dir, "prlimit")
  writeLines(c("#!/bin/sh", sprintf('echo "$1" >> %s', shQuote(seen)), "shift", 'exec "$@"'),
             fake)
  Sys.chmod(fake, mode = "0755")
  fake
}

# Run with `mb` as the limit and `prlimit` as what .prlimit_bin finds, until the
# calling test ends.
.af_local_limit <- function(mb, prlimit = NULL, frame = parent.frame()) {
  .local_global("ANALYZER_MEMORY_LIMIT_MB", as.integer(mb), frame = frame)
  if (!is.null(prlimit)) .local_global(".prlimit_bin", function() prlimit, frame = frame)
  invisible(NULL)
}

.af_skip_without_prlimit <- function() {
  skip_on_os("windows")
  skip_if(!nzchar(unname(Sys.which("prlimit"))), "needs prlimit, which only Linux has")
}

# A stub analyzer that notes the address-space limit it runs under (ulimit -v,
# in kB, or "unlimited") in $STUB_LIMIT. With $STUB_ALLOC_MB set it first holds
# that much memory, on any package but the self-check's, and if it cannot it
# ends as the analyzer does: status 134 and no statistics line.
.af_limit_stub <- function(dir, version = "0.5.2-test") {
  stub <- file.path(dir, "stub-limit.sh")
  writeLines(c(
    "#!/bin/sh",
    sprintf('if [ "$1" = "--version" ]; then echo "rpkg-analyzer %s"; exit 0; fi', version),
    'p=$(sed -n "s/^Package: *//p" "$1/DESCRIPTION" | head -1)',
    'if [ -n "$STUB_LIMIT" ]; then ulimit -v >> "$STUB_LIMIT"; fi',
    'if [ -n "$STUB_ALLOC_MB" ] && [ "$p" != selfcheck ]; then',
    paste0("  /bin/sh -c 'x=$(head -c $((STUB_ALLOC_MB * 1048576)) /dev/zero | tr \"\\\\0\" a); ",
           "[ ${#x} -gt 0 ]' 2>/dev/null || exit 134"),
    "fi",
    sprintf("echo '{\"rec\":\"summary\",\"input_kind\":\"%s\",\"loc_r\":1,\"n_fns_r\":1}'",
            ANALYZER_INPUT_KIND),
    paste0('if [ -n "$RPKG_ANALYZER_STATS" ]; then ',
           'echo "{\\"build\\":\\"stub\\",\\"ms\\":1}" >> "$RPKG_ANALYZER_STATS"; fi'),
    "exit 0"), stub)
  Sys.chmod(stub, mode = "0755")
  stub
}

test_that("the analyzer runs under prlimit when the limit is above zero and prlimit is found", {
  .af_local_limit(1024L, "/usr/bin/prlimit")
  expect_identical(.analyzer_command("/opt/rpkg analyzer"),
                   list(command = "/usr/bin/prlimit",
                        args = c("--as=1073741824", shQuote("/opt/rpkg analyzer")),
                        limit_mb = 1024L))
  expect_identical(.analyzer_limit_mb(), 1024L)
  # Bytes past the largest integer are still written in full.
  .af_local_limit(8192L)
  expect_identical(.analyzer_command("/bin/a")$args[[1L]], "--as=8589934592")
})

test_that("with the limit at 0, or no prlimit, the analyzer is called as it was", {
  as_before <- list(command = "/bin/a", args = character(0L), limit_mb = 0L)
  .af_local_limit(0L, "/usr/bin/prlimit")
  expect_identical(.analyzer_command("/bin/a"), as_before)
  expect_identical(.analyzer_limit_mb(), 0L)
  .af_local_limit(1024L, "")
  expect_identical(.analyzer_command("/bin/a"), as_before)
  expect_identical(.analyzer_limit_mb(), 0L)
})

test_that("a package and the self-check both run through prlimit with the limit in bytes", {
  skip_on_os("windows")
  seen <- withr::local_tempfile()
  .af_local_limit(1024L, .af_fake_prlimit(withr::local_tempdir(), seen))
  stats <- withr::local_tempfile()
  withr::local_envvar(RPKG_ANALYZER_STATS = stats,
                      RPKG_ANALYZER_BIN = .af_stub(withr::local_tempdir(),
                                                   spare_selfcheck = FALSE))
  expect_identical(analyze_with_binary(.af_pkg_dir(), protect = TRUE)$loc_r, 1L)
  expect_identical(readLines(seen), "--as=1073741824")
  expect_true(rpkg_analyzer_selfcheck())
  expect_identical(readLines(seen), rep("--as=1073741824", 2L))
  expect_length(readLines(stats), 2L)

  # Off, the same calls never reach prlimit.
  .af_local_limit(0L)
  expect_identical(analyze_with_binary(.af_pkg_dir(), protect = TRUE)$loc_r, 1L)
  expect_true(rpkg_analyzer_selfcheck())
  expect_length(readLines(seen), 2L)
})

test_that("an abort names the limit it ran under, and nothing when there was none", {
  skip_on_os("windows")
  pkg <- .af_pkg_dir()
  withr::local_envvar(RPKG_ANALYZER_STATS = NA,
                      RPKG_ANALYZER_BIN = .af_stub(withr::local_tempdir(), "abort"))
  .af_local_limit(1024L, "")
  none <- .af_outcome(pkg, protect = TRUE)
  expect_s3_class(none, "analyzer_killed")
  expect_identical(conditionMessage(none), "rpkg-analyzer was killed, exit status 134")
  expect_identical(none$limit_mb, 0L)

  .af_local_limit(1024L, .af_fake_prlimit(withr::local_tempdir(), withr::local_tempfile()))
  for (got in list(.af_outcome(pkg, protect = TRUE), .af_outcome(pkg))) {
    expect_s3_class(got, c("analyzer_killed", "error", "condition"), exact = TRUE)
    expect_identical(got$status, 134L)
    expect_identical(got$limit_mb, 1024L)
    expect_identical(conditionMessage(got), paste0(
      "rpkg-analyzer was killed, exit status 134; its address-space limit was 1024 MiB"))
  }
  # Any other failure on a version with analyzer rows names it too.
  withr::local_envvar(RPKG_ANALYZER_BIN = .af_stub(withr::local_tempdir(), "panic"))
  failed <- .af_outcome(pkg, protect = TRUE)
  expect_s3_class(failed, "analyzer_failed")
  expect_match(conditionMessage(failed), "exited 101 .*; its address-space limit was 1024 MiB$")
})

test_that("an analyzer aborted under the limit is a crash that writes nothing and keeps the rows", {
  skip_on_os("windows")
  out      <- withr::local_tempdir()
  stub_dir <- withr::local_tempdir()
  .af_local_limit(1024L, .af_fake_prlimit(withr::local_tempdir(), withr::local_tempfile()))
  withr::local_envvar(RPKG_ANALYZER_STATS = NA, RPKG_ANALYZER_BIN = .af_stub(stub_dir))
  expect_identical(.af_run(.af_io("1.0"), out)$n_fresh, 1L)
  before <- .package_rows(out, "pkgA")

  # pkgA has analyzer rows and a new release; pkgB has no rows at all.
  .af_stub(stub_dir, "abort")
  failed <- .af_run(.af_io(c("1.0", "1.1"), pkgs = c("pkgA", "pkgB")), out)
  expect_identical(failed$shard_failures$packages, c("pkgA", "pkgB"))
  expect_identical(failed$n_fresh, 0L)
  expect_identical(.package_rows(out, "pkgA"), before)
  expect_identical(.af_stamps(out), c(`1.0` = "0.5.1"))
  expect_identical(sum(vapply(.package_rows(out, "pkgB"), nrow, integer(1L))), 0L)
  for (pkg in c("pkgA", "pkgB")) {
    verdict <- .af_verdict(out, pkg)
    expect_identical(verdict[c("stage", "timeout_failures", "analyze_failures")],
                     data.frame(stage = "crash", timeout_failures = 1L, analyze_failures = 0L,
                                stringsAsFactors = FALSE), info = pkg)
    expect_identical(verdict$reason, paste0(
      "rpkg-analyzer was killed, exit status 134; its address-space limit was 1024 MiB"),
      info = pkg)
  }
})

test_that("a limit the self-check cannot run under stops the run before any shard, and says so", {
  skip_on_os("windows")
  .af_local_limit(64L, .af_fake_prlimit(withr::local_tempdir(), withr::local_tempfile()))
  withr::local_envvar(RPKG_ANALYZER_STATS = NA, RPKG_ANALYZER_BIN = .af_stub(
    withr::local_tempdir(), "abort", spare_selfcheck = FALSE))
  out <- withr::local_tempdir()
  expect_error(run_update(.af_io("1.0"), out, shard_size = 10L),
               "--input-kind git with a summary naming it under its 64 MiB address-space limit")
  expect_false(file.exists(file.path(out, DB_FILENAME)))
})

test_that("the shard plan and run-status.json say which limit is in force", {
  skip_on_os("windows")
  withr::local_envvar(RPKG_ANALYZER_STATS = NA,
                      RPKG_ANALYZER_BIN = .af_stub(withr::local_tempdir()))
  status <- function(out) {
    jsonlite::fromJSON(file.path(out, "run-status.json"))$analyzer_memory_limit_mb
  }
  run <- function(out) {
    .local_global("WORK_DIR", withr::local_tempdir())
    capture.output(run_update(.af_io("1.0"), out, shard_size = 10L))
  }

  out <- withr::local_tempdir()
  .af_local_limit(1024L, .af_fake_prlimit(withr::local_tempdir(), withr::local_tempfile()))
  expect_true("analyzer memory limit: 1024 MiB of address space for each analyzer" %in% run(out))
  expect_identical(status(out), 1024L)

  out <- withr::local_tempdir()
  .af_local_limit(1024L, "")
  expect_true(paste("analyzer memory limit: none, prlimit was not found",
                    "(ANALYZER_MEMORY_LIMIT_MB is 1024)") %in% run(out))
  expect_identical(status(out), 0L)

  out <- withr::local_tempdir()
  .af_local_limit(0L, "/usr/bin/prlimit")
  expect_true("analyzer memory limit: none (ANALYZER_MEMORY_LIMIT_MB is 0)" %in% run(out))
  expect_identical(status(out), 0L)
})

# The tests below run the real prlimit, which only Linux has.

test_that("prlimit sets the address space of a package's analyzer and of the self-check", {
  .af_skip_without_prlimit()
  seen <- withr::local_tempfile()
  withr::local_envvar(RPKG_ANALYZER_STATS = NA, STUB_LIMIT = seen, STUB_ALLOC_MB = NA,
                      RPKG_ANALYZER_BIN = .af_limit_stub(withr::local_tempdir()))
  .af_local_limit(1024L)
  expect_identical(analyze_with_binary(.af_pkg_dir(), protect = TRUE)$loc_r, 1L)
  expect_true(rpkg_analyzer_selfcheck())
  expect_identical(readLines(seen), rep(as.character(1024L * 1024L), 2L))

  # Off, the analyzer keeps whatever limit the session has.
  .af_local_limit(0L)
  expect_true(rpkg_analyzer_selfcheck())
  expect_identical(readLines(seen)[[3L]],
                   system2("/bin/sh", c("-c", shQuote("ulimit -v")), stdout = TRUE))
})

test_that("an analyzer that allocates past the limit aborts, and the same one finishes without it", {
  .af_skip_without_prlimit()
  pkg <- .af_pkg_dir()
  withr::local_envvar(RPKG_ANALYZER_STATS = withr::local_tempfile(), STUB_LIMIT = NA,
                      STUB_ALLOC_MB = "192",
                      RPKG_ANALYZER_BIN = .af_limit_stub(withr::local_tempdir()))
  .af_local_limit(0L)
  expect_identical(analyze_with_binary(pkg, protect = TRUE)$loc_r, 1L)

  .af_local_limit(64L)
  for (got in list(.af_outcome(pkg, protect = TRUE), .af_outcome(pkg))) {
    expect_s3_class(got, c("analyzer_killed", "error", "condition"), exact = TRUE)
    expect_identical(got$status, 134L)
    expect_identical(conditionMessage(got), paste0(
      "rpkg-analyzer was killed, exit status 134; its address-space limit was 64 MiB"))
  }
})

test_that("a package whose analyzer the limit aborts is a crash, and its rows stay", {
  .af_skip_without_prlimit()
  out  <- withr::local_tempdir()
  withr::local_envvar(RPKG_ANALYZER_STATS = NA, STUB_LIMIT = NA, STUB_ALLOC_MB = NA,
                      RPKG_ANALYZER_BIN = .af_limit_stub(withr::local_tempdir()))
  .af_local_limit(64L)
  expect_identical(.af_run(.af_io("1.0"), out)$n_fresh, 1L)
  before <- .package_rows(out, "pkgA")
  expect_identical(.af_stamps(out), c(`1.0` = "0.5.2-test"))

  # The self-check package still fits under the limit; the releases do not.
  withr::local_envvar(STUB_ALLOC_MB = "192")
  failed <- .af_run(.af_io(c("1.0", "1.1"), pkgs = c("pkgA", "pkgB")), out)
  expect_identical(failed$shard_failures$packages, c("pkgA", "pkgB"))
  expect_identical(failed$n_fresh, 0L)
  expect_identical(.package_rows(out, "pkgA"), before)
  expect_identical(sum(vapply(.package_rows(out, "pkgB"), nrow, integer(1L))), 0L)
  expect_identical(.af_verdict(out, "pkgB")[c("stage", "reason")], data.frame(
    stage = "crash",
    reason = "rpkg-analyzer was killed, exit status 134; its address-space limit was 64 MiB",
    stringsAsFactors = FALSE))
})

test_that("the installed analyzer passes the self-check under the limit this pipeline sets", {
  .af_skip_without_prlimit()
  skip_if(!nzchar(rpkg_analyzer_bin()), "needs rpkg-analyzer")
  withr::local_envvar(RPKG_ANALYZER_STATS = NA)
  expect_gt(ANALYZER_MEMORY_LIMIT_MB, 0L)
  expect_identical(.analyzer_limit_mb(), ANALYZER_MEMORY_LIMIT_MB)
  expect_true(rpkg_analyzer_selfcheck())
})
