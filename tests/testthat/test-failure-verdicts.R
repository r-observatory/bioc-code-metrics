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
.fv_run <- function(io, out, ...) {
  .local_global("WORK_DIR", withr::local_tempdir())
  .local_global("ANALYSIS_CORES", 1L)
  # Run under no analyzer build whatever CI installs: a real one would re-queue
  # the fake rows as stale scans and stamp its version on each verdict.
  .local_global("rpkg_analyzer_version", function() NA_character_)
  suppressWarnings(run_update(io, out, shard_size = 10L, ...))
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
