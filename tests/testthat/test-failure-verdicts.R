# tests/testthat/test-failure-verdicts.R: what a failed package is recorded as,
# when it is parked, and when it is tried again.

# A universe whose clone makes an empty directory, so each test decides what
# analyze_package does and a slow clone never trips a one-second cap.
# fail_clones names the packages whose clone fails, each with its exit status.
.fv_io <- function(pkgs, versions = rep("1.0", length(pkgs)), fail_clones = integer(0L)) {
  list(
    package_list = function() data.frame(package = pkgs, latest_version = versions,
                                         stringsAsFactors = FALSE),
    clone = function(pkg, dest) {
      if (pkg %in% names(fail_clones)) return(structure(FALSE, status = fail_clones[[pkg]]))
      dir.create(dest, recursive = TRUE, showWarnings = FALSE)
      TRUE
    })
}

# What analyze_package returns for a package that passes with one version.
.fv_result <- function(pkg, version = "1.0") list(
  summary = data.frame(package = pkg, version = version, loc_r = 10L, n_fns_r = 1L,
                       latest_release_date = "2026-01-01", datasets_scanned = 1L,
                       detail_scanned = 1L, stringsAsFactors = FALSE),
  churn = NULL, api = NULL, functions = NULL, edges = NULL, datasets = NULL)

# One run_update in a scratch work directory, on one core so the worker runs in
# this process and its lines can be captured.
.fv_run <- function(io, out, ..., shard_size = 10L) {
  .local_global("WORK_DIR", withr::local_tempdir())
  .local_global("ANALYSIS_CORES", 1L)
  # Run under no analyzer build whatever CI installs: a real one would re-queue
  # the fake rows as stale scans and stamp its version on each verdict.
  .local_global("rpkg_analyzer_version", function() NA_character_)
  suppressWarnings(run_update(io, out, shard_size = shard_size, ...))
}

# ---------------------------------------------------------------------------
# How a failure is classified
# ---------------------------------------------------------------------------

test_that("an error from analyze_package is classified by what raised it", {
  xf <- .extract_failure("archive", "1.1", 128L, "fatal: bad object")
  xt <- .extract_failure("archive", "1.1", 124L, "")
  pi <- .analyzer_parse_incomplete(1L, "{\"rec\":")
  expect_identical(.classify_failure(xf, 1, worker_timeout = 600L), "extract")
  expect_identical(.classify_failure(xt, 1, worker_timeout = 600L), "git_timeout")
  expect_identical(.classify_failure(pi, 900, worker_timeout = 600L), "analyze")
  expect_identical(.classify_failure(.cap_error(), 1, worker_timeout = 600L), "timeout")
  expect_identical(.classify_failure(simpleError("boom"), 600, worker_timeout = 600L), "timeout")
  expect_identical(.classify_failure(simpleError("boom"), 599.9, worker_timeout = 600L), "analyze")
  expect_identical(.classify_failure(NULL, 1, worker_timeout = 600L), "analyze")
})

test_that("a clone that did not succeed is a git timeout only when GIT_TIMEOUT killed it", {
  expect_identical(.clone_stage(structure(FALSE, status = 124L)), "git_timeout")
  expect_identical(.clone_stage(structure(FALSE, status = 128L)), "clone")
  expect_identical(.clone_stage(FALSE), "clone")
  expect_identical(.clone_reason(structure(FALSE, status = 128L)), "git clone exited 128")
  expect_identical(.clone_reason(structure(FALSE, reason = "no route")), "no route")
  expect_identical(.clone_reason(FALSE), "clone failed")
})

test_that("the parent reads a fork that returned nothing and an error outside the handlers", {
  crash <- .classify_result(NULL)
  expect_identical(crash[c("ok", "stage", "reason", "from_parent")],
                   list(ok = FALSE, stage = "crash", reason = "worker returned no result",
                        from_parent = TRUE))
  expect_true(is.na(crash$elapsed))
  expect_identical(.classify_result(try(stop(.cap_error()), silent = TRUE))$stage, "timeout")
  boom <- .classify_result(try(stop("fork lost its pipe"), silent = TRUE))
  expect_identical(boom[c("stage", "reason")], list(stage = "crash", reason = "fork lost its pipe"))

  ok <- .classify_result(list(package = "p", ok = TRUE, elapsed = 2.5))
  expect_true(ok$ok)
  expect_identical(ok$elapsed, 2.5)
  failed <- .classify_result(list(package = "p", ok = FALSE, stage = "extract",
                                  elapsed = 3, reason = "tar of 1.0 exited 2"))
  expect_identical(failed[c("ok", "stage", "elapsed", "reason", "from_parent")],
                   list(ok = FALSE, stage = "extract", elapsed = 3,
                        reason = "tar of 1.0 exited 2", from_parent = FALSE))
})

test_that("a failure line names its stage and time, and a pass past the cap says so", {
  expect_identical(
    .worker_line(1L, 9L, FALSE, "CONFESSdata", "timeout", 0L, 600.1,
                 "reached elapsed time limit", worker_timeout = 600L),
    "[1/9] FAIL CONFESSdata: timeout after 600.1s: reached elapsed time limit\n")
  expect_identical(.worker_line(2L, 9L, TRUE, "mzR", "ok", 3L, 612.4, worker_timeout = 600L),
                   "[2/9] ok mzR: 3 versions in 612.4s (past the 600s cap)\n")
  expect_identical(.worker_line(3L, 9L, TRUE, "fast", "ok", 3L, 12, worker_timeout = 600L),
                   "[3/9] ok fast: 3 versions in 12.0s\n")
  expect_identical(.worker_line(4L, 9L, FALSE, "gone", "crash", 0L, NA_real_,
                                "worker returned no result"),
                   "[4/9] FAIL gone: crash: worker returned no result\n")
})

test_that("each failure in a run is printed with its stage", {
  .local_global("WORKER_TIMEOUT", 1L)
  .local_global("analyze_package", function(dest, pkg) {
    switch(pkg,
      pkgBadLine = {
        tryCatch(.busy(1.5), error = function(e) NULL)
        stop(.analyzer_parse_incomplete(1L, "{\"rec\":"))
      },
      pkgNoTar = stop(.extract_failure("tar", "1.0", 2L, "tar: bad header")),
      pkgSlow  = { .busy(3); stop("never reached") })
  })
  io  <- .fv_io(c("pkgBadLine", "pkgGone", "pkgNoTar", "pkgSlow"), fail_clones = c(pkgGone = 128L))
  out <- withr::local_tempdir()
  logged <- capture.output(.fv_run(io, out))

  expect_true(any(grepl("FAIL pkgBadLine: analyze after", logged, fixed = TRUE)))
  expect_true(any(grepl("FAIL pkgGone: clone after [0-9.]+s: git clone exited 128", logged)))
  expect_true(any(grepl("FAIL pkgNoTar: extract after [0-9.]+s: tar of 1.0 exited 2", logged)))
  expect_true(any(grepl("FAIL pkgSlow: timeout after [0-9.]+s: reached elapsed time limit", logged)))
})

# SIGKILL, as the kernel's OOM killer sends it. quit() in a fork would also
# end it without a result, but it deletes the parent session's tempdir.
test_that("a fork that dies without a result is printed by the parent as a crash", {
  skip_on_os("windows")
  .local_global("WORK_DIR", withr::local_tempdir())
  .local_global("ANALYSIS_CORES", 2L)
  .local_global("analyze_package", function(dest, pkg) {
    if (identical(pkg, "pkgKilled")) tools::pskill(Sys.getpid(), tools::SIGKILL)
    .fv_result(pkg)
  })
  out <- withr::local_tempdir()
  logged <- capture.output(suppressWarnings(
    run_update(.fv_io(c("pkgKilled", "pkgOk")), out, shard_size = 10L)))
  expect_true(any(grepl("FAIL pkgKilled: crash: worker returned no result", logged, fixed = TRUE)))
})

# ---------------------------------------------------------------------------
# The line a worker prints
# ---------------------------------------------------------------------------

test_that(".worker_line carries the reason a package failed", {
  line <- .worker_line(3L, 400L, FALSE, "pkgX", "analyze", 0L, 12.34,
                       "cannot open file 'DESCRIPTION'")
  expect_true(grepl("[3/400] FAIL pkgX: analyze after 12.3s", line, fixed = TRUE))
  expect_true(grepl("cannot open file 'DESCRIPTION'", line, fixed = TRUE))
  # One write, one line: two would let another fork's output land between them.
  expect_identical(nchar(gsub("[^\n]", "", line)), 1L)
})

test_that(".worker_line says nothing extra when there is no reason", {
  ok <- .worker_line(25L, 400L, TRUE, "pkgY", "ok", 7L, 1.5)
  expect_identical(ok, "[25/400] ok pkgY: 7 versions in 1.5s\n")
})

test_that(".worker_line keeps one fork's line inside one pipe write", {
  reason <- paste(rep("a deparsed call that went on and on", 200L), collapse = "\n")
  line <- .worker_line(1L, 1L, FALSE, "pkgZ", "analyze", 0L, 0.5, reason)
  expect_lte(nchar(line, type = "bytes"), WORKER_LINE_MAX_BYTES)
  expect_identical(nchar(gsub("[^\n]", "", line)), 1L)
  expect_true(grepl("a deparsed call", line, fixed = TRUE))
})

test_that(".worker_line does not cut a multi-byte character in half", {
  line <- .worker_line(1L, 1L, FALSE, "pkgZ", "analyze", 0L, 0.5,
                       strrep("\u00e9\u4e2d\u6587", 500L))
  expect_lte(nchar(line, type = "bytes"), WORKER_LINE_MAX_BYTES)
  expect_true(validUTF8(line))
})

# ---------------------------------------------------------------------------
# What a failure records
# ---------------------------------------------------------------------------

.fv_con <- function(frame = parent.frame()) {
  con <- open_or_init_db(withr::local_tempfile(fileext = ".db", .local_envir = frame))
  withr::defer(DBI::dbDisconnect(con), envir = frame)
  con
}

.fv_row <- function(con, pkg) {
  DBI::dbGetQuery(con, "SELECT * FROM bioc_metrics_failures WHERE package = ?",
                  params = list(pkg))
}

.fv_fail <- function(con, pkg, stage, build = "0.5.0", wt = 600L, lv = "1.0",
                     run_id = NA_character_, elapsed = 1, reason = "boom") {
  .record_failure(con, pkg, stage, build, wt, run_id, elapsed, reason, lv)
}

test_that("a first failure is stored with its stage, build, cap, run and reason", {
  con <- .fv_con()
  .record_failure(con, "pkgA", "extract", "0.5.0", 600L, "r1", 12.5,
                  "git archive of 1.0 exited 128: fatal: bad object", "1.0")
  row <- .fv_row(con, "pkgA")
  expect_identical(row[c("consecutive_failures", "stage", "analyzer_version",
                         "worker_timeout", "fetch_failures", "fetch_version",
                         "analyze_failures", "timeout_failures", "last_run_id",
                         "elapsed_s", "reason")],
                   data.frame(consecutive_failures = 1L, stage = "extract",
                              analyzer_version = "0.5.0", worker_timeout = 600L,
                              fetch_failures = 1L, fetch_version = "1.0",
                              analyze_failures = 0L, timeout_failures = 0L,
                              last_run_id = "r1", elapsed_s = 12.5,
                              reason = "git archive of 1.0 exited 128: fatal: bad object",
                              stringsAsFactors = FALSE))
  expect_true(is.na(row$unparked_at))
})

test_that("each class keeps its own count, and consecutive_failures counts every failure", {
  con <- .fv_con()
  .fv_fail(con, "pkgA", "analyze")
  .fv_fail(con, "pkgA", "analyze")
  .fv_fail(con, "pkgA", "timeout")
  .fv_fail(con, "pkgA", "crash")
  row <- .fv_row(con, "pkgA")
  expect_identical(c(row$analyze_failures, row$timeout_failures, row$fetch_failures,
                     row$consecutive_failures), c(2L, 2L, 0L, 4L))
  expect_identical(row$stage, "crash")
})

test_that("a fetch failure carries across builds and restarts on a new release", {
  con <- .fv_con()
  .fv_fail(con, "pkgA", "clone", build = "0.4.0")
  .fv_fail(con, "pkgA", "extract", build = "0.5.0")
  expect_identical(.fv_row(con, "pkgA")$fetch_failures, 2L)
  .fv_fail(con, "pkgA", "clone", build = "0.5.0", lv = "1.1")
  row <- .fv_row(con, "pkgA")
  expect_identical(c(row$fetch_failures, row$consecutive_failures), c(1L, 3L))
  expect_identical(row$fetch_version, "1.1")
})

test_that("an archived package, with no latest_version, keeps counting its fetch failures", {
  con <- .fv_con()
  .fv_fail(con, "pkgA", "clone", lv = NA_character_)
  .fv_fail(con, "pkgA", "clone", lv = NA_character_)
  row <- .fv_row(con, "pkgA")
  expect_identical(row$fetch_failures, 2L)
  expect_true(is.na(row$fetch_version))
})

test_that("analyze and timeout counts restart on a new build, and timeouts on a new cap", {
  con <- .fv_con()
  .fv_fail(con, "pkgA", "analyze", build = "0.4.0")
  .fv_fail(con, "pkgA", "timeout", build = "0.4.0")
  .fv_fail(con, "pkgA", "timeout", build = "0.5.0")
  row <- .fv_row(con, "pkgA")
  expect_identical(c(row$analyze_failures, row$timeout_failures), c(0L, 1L))
  .fv_fail(con, "pkgA", "timeout", build = "0.5.0", wt = 900L)
  expect_identical(.fv_row(con, "pkgA")$timeout_failures, 1L)
  .fv_fail(con, "pkgA", "git_timeout", build = "0.5.0", wt = 900L)
  expect_identical(.fv_row(con, "pkgA")$timeout_failures, 2L)
})

test_that("a fetch verdict under a new build does not inherit the old build's counts", {
  con <- .fv_con()
  for (i in seq_len(MAX_CLONE_FAILURES)) .fv_fail(con, "pkgA", "analyze", build = "0.4.0")
  .fv_fail(con, "pkgA", "clone", build = "0.5.0")
  row <- .fv_row(con, "pkgA")
  expect_identical(c(row$analyze_failures, row$fetch_failures), c(0L, 1L))
  expect_identical(row$analyzer_version, "0.5.0")
})

test_that("an analyze verdict after a fetch failure sets fetch_failures to 0", {
  con <- .fv_con()
  .fv_fail(con, "pkgA", "clone")
  .fv_fail(con, "pkgA", "clone")
  .fv_fail(con, "pkgA", "analyze")
  row <- .fv_row(con, "pkgA")
  expect_identical(c(row$fetch_failures, row$analyze_failures), c(0L, 1L))
  expect_identical(row$fetch_version, "1.0")
})

test_that("a row from before stages were kept starts its counts afresh", {
  con <- .fv_con()
  DBI::dbExecute(con, "INSERT INTO bioc_metrics_failures
    (package, consecutive_failures, last_attempt, analyzer_version, analyze_failures)
    VALUES ('pkgOld', 7, '2026-09-01T00:00:00Z', '0.5.0', 4)")
  .fv_fail(con, "pkgOld", "analyze")
  row <- .fv_row(con, "pkgOld")
  expect_identical(c(row$analyze_failures, row$consecutive_failures), c(1L, 8L))
})

test_that("no build is stored as the empty string", {
  con <- .fv_con()
  .fv_fail(con, "pkgA", "analyze", build = NA_character_)
  .fv_fail(con, "pkgA", "analyze", build = NA_character_)
  row <- .fv_row(con, "pkgA")
  expect_identical(row$analyzer_version, "")
  expect_identical(row$analyze_failures, 2L)
})

test_that("a stored reason is redacted, on one line and within 512 bytes", {
  con <- .fv_con()
  .fv_fail(con, "pkgA", "clone", reason = paste0(
    "fatal: unable to access 'https://x-access-token:abc@github.com/cran/x.git/': 403\n",
    "token ghs_ABCdef123 and github_pat_11AB_cd"))
  expect_identical(.fv_row(con, "pkgA")$reason, paste0(
    "fatal: unable to access 'https://***github.com/cran/x.git/': 403 ",
    "token *** and ***"))
  .fv_fail(con, "pkgB", "analyze", reason = strrep("x", 2000L))
  expect_lte(nchar(.fv_row(con, "pkgB")$reason, type = "bytes"), 512L)
})

test_that("the run id is PIPELINE_RUN_ID, never GITHUB_RUN_ID", {
  withr::local_envvar(c(PIPELINE_RUN_ID = NA, GITHUB_RUN_ID = "g1"))
  expect_true(is.na(.current_run_id()))
  withr::local_envvar(c(PIPELINE_RUN_ID = ""))
  expect_true(is.na(.current_run_id()))
  withr::local_envvar(c(PIPELINE_RUN_ID = "36411585234"))
  expect_identical(.current_run_id(), "36411585234")
})

test_that("each failure in a run is stored with its stage", {
  .local_global("WORKER_TIMEOUT", 1L)
  withr::local_envvar(c(PIPELINE_RUN_ID = "r1"))
  .local_global("analyze_package", function(dest, pkg) {
    switch(pkg,
      pkgBadLine = {
        tryCatch(.busy(1.5), error = function(e) NULL)
        stop(.analyzer_parse_incomplete(1L, "{\"rec\":"))
      },
      pkgNoTar = stop(.extract_failure("tar", "1.0", 2L, "tar: bad header")),
      pkgSlow  = { .busy(3); stop("never reached") })
  })
  io  <- .fv_io(c("pkgBadLine", "pkgGone", "pkgNoTar", "pkgSlow", "pkgStuck"),
                fail_clones = c(pkgGone = 128L, pkgStuck = 124L))
  out <- withr::local_tempdir()
  .fv_run(io, out)

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, DB_FILENAME))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  rows <- DBI::dbGetQuery(con, "SELECT package, stage, fetch_failures,
    analyze_failures, timeout_failures, worker_timeout, last_run_id, elapsed_s, reason
    FROM bioc_metrics_failures ORDER BY package")
  expect_identical(rows$package, c("pkgBadLine", "pkgGone", "pkgNoTar", "pkgSlow", "pkgStuck"))
  expect_identical(rows$stage, c("analyze", "clone", "extract", "timeout", "git_timeout"))
  expect_identical(rows$fetch_failures, c(0L, 1L, 1L, 0L, 0L))
  expect_identical(rows$analyze_failures, c(1L, 0L, 0L, 0L, 0L))
  expect_identical(rows$timeout_failures, c(0L, 0L, 0L, 1L, 1L))
  expect_identical(unique(rows$worker_timeout), 1L)
  expect_identical(unique(rows$last_run_id), "r1")
  expect_gte(rows$elapsed_s[rows$package == "pkgSlow"], 1)
  expect_identical(rows$reason[rows$package == "pkgNoTar"], "tar of 1.0 exited 2: tar: bad header")
  expect_identical(rows$reason[rows$package == "pkgStuck"], "git clone exited 124")
})

test_that("a fork that dies without a result is stored as a crash with no elapsed time", {
  skip_on_os("windows")
  .local_global("WORK_DIR", withr::local_tempdir())
  .local_global("ANALYSIS_CORES", 2L)
  .local_global("analyze_package", function(dest, pkg) {
    if (identical(pkg, "pkgKilled")) tools::pskill(Sys.getpid(), tools::SIGKILL)
    .fv_result(pkg)
  })
  out <- withr::local_tempdir()
  suppressWarnings(capture.output(
    run_update(.fv_io(c("pkgKilled", "pkgOk")), out, shard_size = 10L)))
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, DB_FILENAME))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  row <- DBI::dbGetQuery(con, "SELECT * FROM bioc_metrics_failures")
  expect_identical(row$package, "pkgKilled")
  expect_identical(c(row$stage, row$reason), c("crash", "worker returned no result"))
  expect_identical(row$timeout_failures, 1L)
  expect_true(is.na(row$elapsed_s))
})

test_that("no test sees a run id it did not set", {
  expect_identical(Sys.getenv("PIPELINE_RUN_ID", "unset"), "unset")
})

# ---------------------------------------------------------------------------
# When a verdict parks its package
# ---------------------------------------------------------------------------

.fv_universe <- function(pkgs, versions = rep("1.0", length(pkgs))) {
  data.frame(package = pkgs, latest_version = versions, stringsAsFactors = FALSE)
}

# Move a verdict's last attempt `days` into the past.
.fv_age <- function(con, pkg, days) {
  DBI::dbExecute(con, "UPDATE bioc_metrics_failures SET last_attempt = ? WHERE package = ?",
                 params = list(format(Sys.time() - days * 86400, "%Y-%m-%dT%H:%M:%SZ",
                                      tz = "UTC"), pkg))
}

test_that("rows written before stages were kept park nothing", {
  con <- .fv_con()
  DBI::dbExecute(con, "INSERT INTO bioc_metrics_failures
    (package, consecutive_failures, last_attempt) VALUES
    ('pkgA', 5, '2026-08-01T00:00:00Z'), ('pkgB', 9, '2026-09-28T00:00:00Z')")
  st <- .verdict_state(con, "0.4.0", 600L, .fv_universe(c("pkgA", "pkgB")))
  expect_identical(st$package, c("pkgA", "pkgB"))
  expect_identical(st$parked, c(FALSE, FALSE))
  expect_true(all(is.na(st$stage)))
  expect_identical(.permanent_failures(con, "0.4.0", 600L, .fv_universe(c("pkgA", "pkgB"))),
                   character(0L))
})

test_that("one build parks a package at five analyze failures or three timeouts", {
  con <- .fv_con()
  u <- .fv_universe(c("pkgA", "pkgT"))
  for (i in seq_len(MAX_CLONE_FAILURES - 1L)) .fv_fail(con, "pkgA", "analyze")
  for (i in seq_len(MAX_TIMEOUT_FAILURES - 1L)) .fv_fail(con, "pkgT", "timeout")
  expect_identical(.permanent_failures(con, "0.5.0", 600L, u), character(0L))
  .fv_fail(con, "pkgA", "analyze")
  .fv_fail(con, "pkgT", "crash")
  expect_identical(.permanent_failures(con, "0.5.0", 600L, u), c("pkgA", "pkgT"))
  st <- .verdict_state(con, "0.5.0", 600L, u)
  expect_identical(st$class, c("analyze", "timeout"))
  # Another build asks again, and another cap asks again about the timeouts.
  expect_identical(.permanent_failures(con, "0.5.1", 600L, u), character(0L))
  expect_identical(.permanent_failures(con, "0.5.0", 900L, u), "pkgA")
})

test_that("five fetch failures park a package across builds until its release changes", {
  con <- .fv_con()
  for (i in seq_len(MAX_CLONE_FAILURES)) .fv_fail(con, "pkgF", "clone", build = "0.4.0")
  expect_identical(.permanent_failures(con, "0.5.0", 600L, .fv_universe("pkgF")), "pkgF")
  expect_identical(.verdict_state(con, "0.5.0", 600L, .fv_universe("pkgF"))$class, "fetch")
  expect_identical(.permanent_failures(con, "0.5.0", 600L, .fv_universe("pkgF", "1.1")),
                   character(0L))
})

test_that("an archived package stays fetch-parked with no latest_version", {
  con <- .fv_con()
  for (i in seq_len(MAX_CLONE_FAILURES)) .fv_fail(con, "pkgF", "clone", lv = NA_character_)
  expect_identical(.permanent_failures(con, "0.5.0", 600L,
                                       .fv_universe("pkgF", NA_character_)), "pkgF")
})

test_that("a fetch-parked package with no stored rows is due a recheck after a week", {
  con <- .fv_con()
  DBI::dbExecute(con, "CREATE TABLE bioc_code_summary (package TEXT, version TEXT)")
  DBI::dbExecute(con, "INSERT INTO bioc_code_summary VALUES ('pkgRows', '1.0')")
  u <- .fv_universe(c("pkgNone", "pkgRows"))
  for (p in c("pkgNone", "pkgRows")) {
    for (i in seq_len(MAX_CLONE_FAILURES)) .fv_fail(con, p, "clone")
  }
  .fv_age(con, "pkgNone", FETCH_RECHECK_DAYS - 1L)
  .fv_age(con, "pkgRows", FETCH_RECHECK_DAYS + 1L)
  expect_identical(.permanent_failures(con, "0.5.0", 600L, u), c("pkgNone", "pkgRows"))

  .fv_age(con, "pkgNone", FETCH_RECHECK_DAYS + 1L)
  st <- .verdict_state(con, "0.5.0", 600L, u)
  expect_identical(st$recheck_due, c(TRUE, FALSE))
  expect_identical(.permanent_failures(con, "0.5.0", 600L, u), "pkgRows")
})

test_that("a clone failure parks after five runs and is released by the next Bioconductor release", {
  out <- withr::local_tempdir()
  io  <- .fv_io("pkgF", "3.23", fail_clones = c(pkgF = 128L))
  for (i in seq_len(MAX_CLONE_FAILURES)) .fv_run(io, out)
  parked <- .fv_run(io, out)
  expect_identical(parked$n_shard, 0L)
  expect_identical(parked$permanent_failures, 1L)

  released <- .fv_run(.fv_io("pkgF", "3.24", fail_clones = c(pkgF = 128L)), out)
  expect_identical(released$n_shard, 1L)
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, DB_FILENAME))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  row <- .fv_row(con, "pkgF")
  expect_identical(row$fetch_failures, 1L)
  expect_identical(row$fetch_version, "3.24")
})

test_that("a run without the analyzer parks under no build, and a run with one asks again", {
  con <- .fv_con()
  u <- .fv_universe(c("pkgA", "pkgF"))
  for (i in seq_len(MAX_CLONE_FAILURES)) {
    .fv_fail(con, "pkgA", "analyze", build = NA_character_)
    .fv_fail(con, "pkgF", "clone", build = NA_character_)
  }
  expect_identical(.permanent_failures(con, NA_character_, 600L, u), c("pkgA", "pkgF"))
  expect_identical(.permanent_failures(con, "0.5.0", 600L, u), "pkgF")
})

test_that("the weekly recheck reads its age in UTC whatever the runner's time zone", {
  withr::local_timezone("Pacific/Auckland")
  con <- .fv_con()
  for (i in seq_len(MAX_CLONE_FAILURES)) .fv_fail(con, "pkgF", "clone")
  u <- .fv_universe("pkgF")
  .fv_age(con, "pkgF", FETCH_RECHECK_DAYS - 0.5)
  expect_identical(.verdict_state(con, "0.5.0", 600L, u)$recheck_due, FALSE)
  .fv_age(con, "pkgF", FETCH_RECHECK_DAYS + 0.5)
  expect_identical(.verdict_state(con, "0.5.0", 600L, u)$recheck_due, TRUE)
})

# ---------------------------------------------------------------------------
# One failed attempt per run
# ---------------------------------------------------------------------------

test_that("the packages tried this run are the ones whose verdict names it", {
  con <- .fv_con()
  .fv_fail(con, "pkgA", "clone", run_id = "r1")
  .fv_fail(con, "pkgB", "analyze", run_id = "r0")
  expect_identical(.tried_this_run(con, "r1"), "pkgA")
  expect_identical(.tried_this_run(con, NA_character_), character(0L))
})

test_that("a package that failed is attempted once per run", {
  out <- withr::local_tempdir()
  io  <- .fv_io(c("pkgF", "pkgOk"), fail_clones = c(pkgF = 128L))
  .local_global("analyze_package", function(dest, pkg) .fv_result(pkg))
  attempts <- function() {
    con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, DB_FILENAME))
    on.exit(DBI::dbDisconnect(con))
    .fv_row(con, "pkgF")$consecutive_failures
  }

  withr::with_envvar(c(PIPELINE_RUN_ID = "r1"), {
    .fv_run(io, out)
    second <- .fv_run(io, out)
  })
  expect_identical(attempts(), 1L)
  expect_identical(second$n_shard, 0L)

  withr::with_envvar(c(PIPELINE_RUN_ID = "r2"), .fv_run(io, out))
  expect_identical(attempts(), 2L)
})

test_that("GITHUB_RUN_ID alone does not make a run, so every call attempts again", {
  out <- withr::local_tempdir()
  io  <- .fv_io("pkgF", fail_clones = c(pkgF = 128L))
  withr::local_envvar(c(GITHUB_RUN_ID = "g1", PIPELINE_RUN_ID = NA))
  .fv_run(io, out)
  .fv_run(io, out)
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, DB_FILENAME))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_identical(.fv_row(con, "pkgF")$consecutive_failures, 2L)
})

test_that("a package tried this run is left out of every queue, a backfill included", {
  out <- withr::local_tempdir()
  .local_global("analyze_package", function(dest, pkg) {
    r <- .fv_result(pkg)
    r$summary$datasets_scanned <- NA_integer_
    r
  })
  .fv_run(.fv_io("pkgB"), out)
  withr::local_envvar(c(PIPELINE_RUN_ID = "r1"))
  .local_global("analyze_package", function(dest, pkg) stop("broke"))
  first  <- .fv_run(.fv_io("pkgB"), out)
  second <- .fv_run(.fv_io("pkgB"), out)
  expect_identical(c(first$n_shard, second$n_shard), c(1L, 0L))
})

# ---------------------------------------------------------------------------
# A verdict is news, and a shard that records one publishes
# ---------------------------------------------------------------------------

test_that("a shard whose only news is a failure reports changed", {
  out <- withr::local_tempdir()
  .local_global("analyze_package", function(dest, pkg) .fv_result(pkg))
  io <- .fv_io(c("pkgF", "pkgOk"), fail_clones = c(pkgF = 128L))
  .fv_run(io, out)
  second <- .fv_run(io, out)
  expect_identical(second$n_fresh, 0L)
  expect_identical(second$shard_failures$packages, "pkgF")
  expect_true(second$changed)
})

# last_changed says when the data moved, and a verdict or a release moves none.
test_that("a shard whose only news is a verdict or a release keeps last_changed", {
  out <- withr::local_tempdir()
  .local_global("analyze_package", function(dest, pkg) .fv_result(pkg))
  io <- .fv_io(c("pkgF", "pkgOk"), fail_clones = c(pkgF = 128L))
  .fv_run(io, out)
  # Stand in for the prior release's manifest the workflow downloads.
  prior <- jsonlite::read_json(file.path(out, "code-manifest.json"))
  prior$last_changed <- "2026-01-05T00:00:00Z"
  write_manifest(file.path(out, "prev-code-manifest.json"), prior)
  last_changed <- function() vapply(c("code-manifest.json", "data-manifest.json"),
    function(f) jsonlite::read_json(file.path(out, f))$last_changed, character(1L),
    USE.NAMES = FALSE)

  failed <- .fv_run(io, out)
  expect_identical(failed$n_fresh, 0L)
  expect_true(failed$changed)
  expect_true(jsonlite::read_json(file.path(out, "run-status.json"))$changed)
  expect_identical(last_changed(), rep("2026-01-05T00:00:00Z", 2L))

  released <- .fv_run(io, out, unpark = "pkgF")
  expect_identical(released$n_fresh, 0L)
  expect_true(released$changed)
  expect_identical(jsonlite::read_json(file.path(out, "run-status.json"))$n_released, 1L)
  expect_identical(last_changed(), rep("2026-01-05T00:00:00Z", 2L))
})

# The workflow downloads the prior release's manifest once, so every shard of a
# run reads the same pre-run last_changed. A later shard that moves no data has
# to keep the time an earlier shard of the same run moved it.
test_that("a verdict-only shard keeps the last_changed an earlier shard of its run wrote", {
  out <- withr::local_tempdir()
  .local_global("analyze_package", function(dest, pkg) .fv_result(pkg))
  .fv_run(.fv_io(c("pkgA", "pkgZ")), out)
  # Stand in for the workflow, which moves the prior release's manifest aside.
  prior <- jsonlite::read_json(file.path(out, "code-manifest.json"))
  prior$last_changed <- "2026-01-05T00:00:00Z"
  write_manifest(file.path(out, "prev-code-manifest.json"), prior)
  file.remove(file.path(out, "code-manifest.json"))
  last_changed <- function() vapply(c("code-manifest.json", "data-manifest.json"),
    function(f) jsonlite::read_json(file.path(out, f))$last_changed, character(1L),
    USE.NAMES = FALSE)
  io <- .fv_io(c("pkgA", "pkgF", "pkgZ"), fail_clones = c(pkgF = 128L))

  withr::with_envvar(c(PIPELINE_RUN_ID = "r1"), {
    # Shard 1 analyses pkgA again at the version it already has: the data
    # moves and the fingerprint does not.
    first <- .fv_run(io, out, requeue = "pkgA", shard_size = 1L)
    moved <- last_changed()
    # A shard that stamped its own time would differ from shard 1's by now.
    Sys.sleep(1.1)
    # Shard 2 only records pkgF's failure.
    second <- .fv_run(io, out, shard_size = 1L)
  })
  expect_identical(first$n_fresh, 1L)
  expect_identical(first$fingerprint, prior$fingerprint)
  expect_false("2026-01-05T00:00:00Z" %in% moved)
  expect_identical(second$n_fresh, 0L)
  expect_identical(second$shard_failures$packages, "pkgF")
  expect_true(second$changed)
  expect_identical(last_changed(), moved)

  # A later run reads its own prior release, not the last run's status file.
  prior$last_changed <- "2026-02-01T00:00:00Z"
  write_manifest(file.path(out, "prev-code-manifest.json"), prior)
  withr::with_envvar(c(PIPELINE_RUN_ID = "r2"), .fv_run(io, out, shard_size = 1L))
  expect_identical(last_changed(), rep("2026-02-01T00:00:00Z", 2L))
})

test_that("a weekly recheck that fails again at the same stage stays parked and publishes nothing", {
  out <- withr::local_tempdir()
  .local_global("analyze_package", function(dest, pkg) .fv_result(pkg))
  io <- .fv_io(c("pkgF", "pkgOk"), fail_clones = c(pkgF = 128L))
  for (i in seq_len(MAX_CLONE_FAILURES)) .fv_run(io, out)
  settled <- .fv_run(io, out)
  expect_identical(settled$n_shard, 0L)
  expect_false(settled$changed)

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, DB_FILENAME))
  .fv_age(con, "pkgF", FETCH_RECHECK_DAYS + 1L)
  DBI::dbDisconnect(con)
  recheck <- .fv_run(io, out)
  expect_identical(recheck$n_shard, 1L)
  expect_identical(recheck$shard_failures$packages, "pkgF")
  expect_false(recheck$changed)
  expect_identical(recheck$permanent_failures, 1L)
})

test_that("a weekly recheck that fails at a new stage publishes", {
  out <- withr::local_tempdir()
  .local_global("analyze_package", function(dest, pkg) .fv_result(pkg))
  for (i in seq_len(MAX_CLONE_FAILURES)) {
    .fv_run(.fv_io(c("pkgF", "pkgOk"), fail_clones = c(pkgF = 128L)), out)
  }
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, DB_FILENAME))
  .fv_age(con, "pkgF", FETCH_RECHECK_DAYS + 1L)
  DBI::dbDisconnect(con)
  .local_global("analyze_package", function(dest, pkg) {
    if (identical(pkg, "pkgF")) stop(.extract_failure("archive", "1.0", 128L, "bad"))
    .fv_result(pkg)
  })
  recheck <- .fv_run(.fv_io(c("pkgF", "pkgOk")), out)
  expect_identical(recheck$shard_failures$packages, "pkgF")
  expect_true(recheck$changed)
})

# ---------------------------------------------------------------------------
# The standing over-cap list
# ---------------------------------------------------------------------------

test_that("a pass past the cap goes on the standing list, and a pass under it takes it off", {
  out <- withr::local_tempdir()
  .local_global("WORKER_TIMEOUT", 1L)
  withr::local_envvar(c(PIPELINE_RUN_ID = "r1"))
  slow <- TRUE
  .local_global("analyze_package", function(dest, pkg) {
    if (slow) tryCatch(.busy(1.5), error = function(e) NULL)
    .fv_result(pkg)
  })
  logged <- capture.output(.fv_run(.fv_io("pkgBig"), out))
  expect_true(any(grepl("ok pkgBig: 1 versions in [0-9.]+s \\(past the 1s cap\\)", logged)))

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, DB_FILENAME))
  row <- DBI::dbGetQuery(con, "SELECT * FROM bioc_over_cap")
  expect_identical(row$package, "pkgBig")
  expect_gte(row$elapsed_s, 1)
  expect_identical(c(row$analyzer_version, row$last_run_id), c("", "r1"))
  expect_identical(.over_cap_packages(con), "pkgBig")
  DBI::dbDisconnect(con)

  slow <- FALSE
  .fv_run(.fv_io("pkgBig", "1.1"), out)
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, DB_FILENAME))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_identical(.over_cap_packages(con), character(0L))
})

test_that("the standing list is read longest first", {
  con <- .fv_con()
  DBI::dbExecute(con, "INSERT INTO bioc_over_cap (package, elapsed_s) VALUES
    ('pkgA', 700), ('pkgB', 1500), ('pkgC', 700)")
  expect_identical(.over_cap_packages(con), c("pkgB", "pkgA", "pkgC"))
})

test_that("a package on the standing list that then fails stays on it", {
  out <- withr::local_tempdir()
  .local_global("WORKER_TIMEOUT", 1L)
  .local_global("analyze_package", function(dest, pkg) {
    tryCatch(.busy(1.5), error = function(e) NULL)
    .fv_result(pkg)
  })
  .fv_run(.fv_io("pkgBig"), out)
  .local_global("analyze_package", function(dest, pkg) stop("broke"))
  .fv_run(.fv_io("pkgBig", "1.1"), out)
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, DB_FILENAME))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_identical(.over_cap_packages(con), "pkgBig")
  expect_identical(.fv_row(con, "pkgBig")$stage, "analyze")
})

# ---------------------------------------------------------------------------
# Operator releases: --unpark and --requeue
# ---------------------------------------------------------------------------

# A database with one parked package per class and a legacy row.
.fv_parked_con <- function(frame = parent.frame()) {
  con <- .fv_con(frame)
  for (i in seq_len(MAX_CLONE_FAILURES)) {
    .fv_fail(con, "pkgFetch", "clone", run_id = "r1")
    .fv_fail(con, "pkgAnalyze", "analyze", run_id = "r1")
  }
  for (i in seq_len(MAX_TIMEOUT_FAILURES)) .fv_fail(con, "pkgTimeout", "timeout", run_id = "r1")
  DBI::dbExecute(con, "INSERT INTO bioc_metrics_failures (package, consecutive_failures,
    last_attempt) VALUES ('pkgLegacy', 5, '2026-08-01T00:00:00Z')")
  con
}

.fv_counts <- function(con) {
  DBI::dbGetQuery(con, "SELECT package, fetch_failures + analyze_failures +
    timeout_failures AS n, last_run_id, unparked_at IS NOT NULL AS unparked
    FROM bioc_metrics_failures ORDER BY package")
}

test_that("--unpark by class zeroes that class's rows and deletes nothing", {
  con <- .fv_parked_con()
  u <- .fv_universe(c("pkgAnalyze", "pkgFetch", "pkgLegacy", "pkgTimeout"))
  expect_identical(.unpark(con, "timeout"), 1L)
  expect_identical(.permanent_failures(con, "0.5.0", 600L, u), c("pkgAnalyze", "pkgFetch"))
  counts <- .fv_counts(con)
  expect_identical(counts$package, c("pkgAnalyze", "pkgFetch", "pkgLegacy", "pkgTimeout"))
  expect_identical(counts$unparked, c(0L, 0L, 0L, 1L))
  expect_true(is.na(counts$last_run_id[counts$package == "pkgTimeout"]))
  expect_identical(.fv_row(con, "pkgTimeout")$consecutive_failures, MAX_TIMEOUT_FAILURES)

  expect_identical(.unpark(con, "fetch"), 1L)
  expect_identical(.unpark(con, "analyze"), 1L)
  expect_identical(.permanent_failures(con, "0.5.0", 600L, u), character(0L))
})

test_that("--unpark=all releases every row and --unpark names packages", {
  con <- .fv_parked_con()
  expect_identical(.unpark(con, "pkgFetch, pkgAnalyze"), 2L)
  expect_identical(.fv_counts(con)$n, c(0L, 0L, 0L, MAX_TIMEOUT_FAILURES))
  expect_identical(.unpark(con, "all"), 4L)
  expect_identical(.fv_counts(con)$n, c(0L, 0L, 0L, 0L))
})

test_that("a malformed or unknown package name is warned about and ignored", {
  con <- .fv_parked_con()
  expect_warning(n <- .unpark(con, "pkgFetch,x'; DROP TABLE bioc_metrics_failures,notHere"),
                 "x'; DROP TABLE bioc_metrics_failures, notHere")
  expect_identical(n, 1L)
  expect_identical(nrow(.fv_counts(con)), 4L)
  expect_identical(.unpark(con, ""), 0L)
})

test_that("--requeue releases the verdict, forgets read attempts and clears the latest scan marker", {
  con <- .fv_parked_con()
  DBI::dbExecute(con, "CREATE TABLE bioc_code_summary (package TEXT, version TEXT,
    latest_release_date TEXT, datasets_scanned INTEGER)")
  DBI::dbExecute(con, "INSERT INTO bioc_code_summary VALUES
    ('pkgAnalyze', '1.0', NULL, 1), ('pkgAnalyze', '1.1', '2026-09-01', 1),
    ('pkgOther', '2.0', '2026-09-01', 1)")
  .record_analyzer_read_attempt(con, "pkgAnalyze", "0.5.0")
  .record_analyzer_read_attempt(con, "pkgOther", "0.5.0")

  expect_identical(.requeue(con, "pkgAnalyze"), "pkgAnalyze")
  expect_identical(.fv_row(con, "pkgAnalyze")$analyze_failures, 0L)
  expect_false(is.na(.fv_row(con, "pkgAnalyze")$unparked_at))
  expect_identical(DBI::dbGetQuery(con,
    "SELECT package FROM bioc_analyzer_read_attempts")$package, "pkgOther")
  expect_identical(DBI::dbGetQuery(con,
    "SELECT datasets_scanned FROM bioc_code_summary ORDER BY package, version")$datasets_scanned,
    c(1L, NA, 1L))
})

test_that("--requeue=over_cap names the standing list, alongside named packages", {
  con <- .fv_parked_con()
  DBI::dbExecute(con, "INSERT INTO bioc_over_cap (package, elapsed_s) VALUES
    ('pkgBig', 900), ('pkgHuge', 1500)")
  expect_identical(.requeue(con, "pkgFetch,over_cap"), c("pkgHuge", "pkgBig", "pkgFetch"))
})

test_that("an unpark followed by a new failure adds no row", {
  out <- withr::local_tempdir()
  io  <- .fv_io("pkgF", fail_clones = c(pkgF = 128L))
  for (i in seq_len(MAX_CLONE_FAILURES)) .fv_run(io, out)
  expect_identical(.fv_run(io, out)$n_shard, 0L)
  released <- .fv_run(io, out, unpark = "pkgF")
  expect_identical(released$n_shard, 1L)
  expect_true(released$changed)
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, DB_FILENAME))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_identical(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM bioc_metrics_failures")$n, 1L)
  expect_identical(.fv_row(con, "pkgF")$fetch_failures, 1L)
})

test_that("a requeue is analysed again in the same run, and over_cap reaches the standing list", {
  out <- withr::local_tempdir()
  .local_global("WORKER_TIMEOUT", 1L)
  slow <- TRUE
  .local_global("analyze_package", function(dest, pkg) {
    if (slow) tryCatch(.busy(1.5), error = function(e) NULL)
    .fv_result(pkg)
  })
  .fv_run(.fv_io("pkgBig"), out)
  expect_identical(.fv_run(.fv_io("pkgBig"), out)$n_shard, 0L)
  slow <- FALSE
  again <- .fv_run(.fv_io("pkgBig"), out, requeue = "over_cap")
  expect_identical(again$n_shard, 1L)
  expect_true(again$changed)
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, DB_FILENAME))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_identical(.over_cap_packages(con), character(0L))
})

test_that("update.R reads --unpark and --requeue from its command line", {
  flags <- .parse_cli_flags(c("out/", "--unpark=timeout", "--requeue=a,b,over_cap",
                              "--shard=7", "--recollect"))
  expect_identical(flags[c("unpark", "requeue", "shard", "recollect", "force_full")],
                   list(unpark = "timeout", requeue = "a,b,over_cap", shard = 7L,
                        recollect = TRUE, force_full = FALSE))
  none <- .parse_cli_flags("out/")
  expect_null(none$unpark)
  expect_null(none$requeue)
  expect_identical(none$shard, SHARD_SIZE)
})

test_that("a re-run under the same run id skips what failed, unless the operator unparks it", {
  out <- withr::local_tempdir()
  io  <- .fv_io("pkgF", fail_clones = c(pkgF = 128L))
  withr::local_envvar(c(PIPELINE_RUN_ID = "r1"))
  .fv_run(io, out)
  expect_identical(.fv_run(io, out)$n_shard, 0L)
  expect_identical(.fv_run(io, out, unpark = "pkgF")$n_shard, 1L)
})

# ---------------------------------------------------------------------------
# What a run says about its verdicts
# ---------------------------------------------------------------------------

test_that("the progress lines count parked verdicts by class and failures by stage", {
  con <- .fv_parked_con()
  st <- .verdict_state(con, "0.5.0", 600L,
                       .fv_universe(c("pkgAnalyze", "pkgFetch", "pkgLegacy", "pkgTimeout")))
  expect_identical(.parked_counts(st), list(fetch = 1L, analyze = 1L, timeout = 1L, legacy = 1L))
  expect_identical(.stage_counts(c("timeout", "clone", "timeout")), list(clone = 1L, timeout = 2L))
  expect_identical(.stage_counts(character(0L)), stats::setNames(list(), character(0L)))
  expect_identical(.verdict_plan_line("0.5.0", 2L, st, 3L), paste0(
    "analyzer 0.5.0; verdicts released: 2; parked: fetch 1, analyze 1, timeout 1, ",
    "legacy 1; skipped as tried this run: 3; fetch rechecks due: 0\n"))
  expect_identical(.verdict_plan_line(NA_character_, 0L, st[0L, ], 0L), paste0(
    "analyzer none; verdicts released: 0; parked: fetch 0, analyze 0, timeout 0, ",
    "legacy 0; skipped as tried this run: 0; fetch rechecks due: 0\n"))
  expect_identical(.verdict_receipt_line(c("clone", "timeout", "timeout"), "mzR", 4L), paste0(
    "shard verdicts: 3 failed (clone 1, timeout 2); passed over the cap: 1 (mzR); ",
    "standing over-cap list: 4\n"))
  expect_identical(.verdict_receipt_line(character(0L), character(0L), 0L),
    "shard verdicts: 0 failed; passed over the cap: 0; standing over-cap list: 0\n")
})

test_that("the manifest and run status carry the verdict counts, and the data manifest does not", {
  out <- withr::local_tempdir()
  .local_global("WORKER_TIMEOUT", 1L)
  withr::local_envvar(c(PIPELINE_RUN_ID = "r1"))
  .local_global("analyze_package", function(dest, pkg) {
    if (identical(pkg, "pkgBad")) stop("broke")
    if (identical(pkg, "pkgBig")) tryCatch(.busy(1.5), error = function(e) NULL)
    .fv_result(pkg)
  })
  io <- .fv_io(c("pkgBad", "pkgBig", "pkgGone", "pkgOk"), fail_clones = c(pkgGone = 128L))
  logged <- capture.output(.fv_run(io, out))

  expect_true(any(logged == paste0(
    "analyzer none; verdicts released: 0; parked: fetch 0, analyze 0, timeout 0, ",
    "legacy 0; skipped as tried this run: 0; fetch rechecks due: 0")))
  expect_true(any(logged == paste0(
    "shard verdicts: 2 failed (clone 1, analyze 1); passed over the cap: 1 (pkgBig); ",
    "standing over-cap list: 1")))

  cm <- jsonlite::read_json(file.path(out, "code-manifest.json"))$bootstrap
  # jsonlite writes a key added twice as key.1, so each key is added once.
  expect_false(any(grepl("\\.[0-9]+$", names(cm))))
  expect_identical(cm$parked, list(fetch = 0L, analyze = 0L, timeout = 0L, legacy = 0L))
  expect_identical(cm$failed_this_run, 2L)
  expect_identical(cm$over_cap_ok, list(count = 1L, packages = list("pkgBig")))
  expect_identical(cm$over_cap_this_run, 1L)
  expect_null(jsonlite::read_json(file.path(out, "data-manifest.json"))$bootstrap$parked)

  rs <- jsonlite::read_json(file.path(out, "run-status.json"))
  expect_false(any(grepl("\\.[0-9]+$", names(rs))))
  expect_identical(rs$failed_by_stage, list(clone = 1L, analyze = 1L))
  expect_identical(rs[c("failed_this_run", "over_cap_this_run", "n_released",
                        "n_tried_skipped", "n_recheck_due")],
                   list(failed_this_run = 2L, over_cap_this_run = 1L, n_released = 0L,
                        n_tried_skipped = 0L, n_recheck_due = 0L))
  expect_identical(rs$parked, cm$parked)
  expect_identical(rs$over_cap_ok, cm$over_cap_ok)
  expect_identical(rs$latest_by_build, list(none = 2L))
})
