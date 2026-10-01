# tests/testthat/test-shard-loop.R: when the workflow's shard loop stops, and
# what the run leaves on the Actions page. Both are shell functions in
# scripts/publish.sh, run here through bash as update.yml runs them.

.loop_script <- function() {
  normalizePath(test_path("..", "..", "scripts", "publish.sh"), mustWork = TRUE)
}

# Run `call` (a publish.sh call whose one %s is the file's path) through bash
# on a run-status.json holding `status`, or `raw` text, or no file at all.
.loop_bash <- function(call, status = NULL, raw = NULL, missing = FALSE) {
  skip_on_os("windows")
  skip_if(!nzchar(Sys.which("jq")), "jq is not installed")
  path <- withr::local_tempfile(fileext = ".json")
  if (!missing) {
    if (is.null(raw)) write_manifest(path, status) else writeLines(raw, path)
  }
  out <- suppressWarnings(system2("bash", c("-c", shQuote(sprintf(
    "source %s && %s", shQuote(.loop_script()), sprintf(call, shQuote(path))))),
    stdout = TRUE, stderr = TRUE))
  list(status = attr(out, "status") %||% 0L, output = as.character(out))
}

# ---------------------------------------------------------------------------
# When the shard loop stops
# ---------------------------------------------------------------------------

.loop_done <- function(...) .loop_bash("shard_loop_done %s", ...)

.loop_status <- function(complete = FALSE, changed = TRUE, remaining = 5L, shard = 2L) {
  list(changed = changed, bootstrap_complete = complete, n_analyzed = 10L,
       n_universe = 20L, n_remaining = remaining, n_fresh = 2L, n_shard = shard)
}

test_that("the shard loop goes on while the shard changed something and work is left", {
  res <- .loop_done(.loop_status())
  expect_identical(res$status, 1L)
  expect_identical(res$output, character(0L))
})

test_that("the shard loop stops on a complete bootstrap, no change, a drained queue or an empty shard", {
  cases <- list(
    list(.loop_status(complete = TRUE),
         "Nothing left to do (complete=true, changed=true, remaining=5, shard=2)."),
    list(.loop_status(changed = FALSE),
         "Nothing left to do (complete=false, changed=false, remaining=5, shard=2)."),
    list(.loop_status(remaining = 0L),
         "Nothing left to do (complete=false, changed=true, remaining=0, shard=2)."),
    list(.loop_status(shard = 0L),
         "Nothing left to do (complete=false, changed=true, remaining=5, shard=0)."))
  for (case in cases) {
    res <- .loop_done(case[[1L]])
    expect_identical(res$status, 0L)
    expect_identical(res$output, case[[2L]])
  }
})

test_that("a run status that cannot be read stops the shard loop with a warning", {
  for (res in list(.loop_done(missing = TRUE), .loop_done(raw = "{not json"),
                   .loop_done(raw = ""))) {
    expect_identical(res$status, 0L)
    expect_true(any(grepl("^::warning::could not read ", res$output)))
  }
})

# ---------------------------------------------------------------------------
# The run's summary on the Actions page
# ---------------------------------------------------------------------------

.step_summary <- function(status, secs, start) {
  .loop_bash(sprintf("write_step_summary %%s %s %s", secs, start), status)
}

test_that("the step summary tabulates the run status and an ETA at this run's rate", {
  status <- list(
    changed = TRUE, bootstrap_complete = FALSE, n_remaining = 300L, n_shard = 400L,
    failed_this_run = 3L, failed_by_stage = list(clone = 1L, timeout = 2L),
    parked = list(fetch = 2L, analyze = 0L, timeout = 1L, legacy = 0L),
    over_cap_ok = list(count = 2L, packages = I(c("mzR", "HMP16SData"))),
    latest_by_build = list(`0.4.0` = 33000L, none = 5L))
  res <- .step_summary(status, 3600, 1500)
  expect_identical(res$status, 0L)
  expect_identical(res$output, c(
    "### Shard loop", "", "| | |", "|---|---|",
    "| Packages still queued | 300 |",
    "| Failed this run | 3 |",
    "| Failed in the last shard, by stage | clone 1, timeout 2 |",
    "| Parked | fetch 2, analyze 0, timeout 1, legacy 0 |",
    "| Standing over-cap list | 2 (mzR, HMP16SData) |",
    "| Latest rows by build | 0.4.0 33000, none 5 |",
    "| ETA at this run's rate | about 0.3 h |"))
})

test_that("the step summary says done, or n/a, when there is no rate to go on", {
  done <- .step_summary(list(n_remaining = 0L, n_shard = 0L), 60, 0)$output
  expect_identical(done[[length(done)]], "| ETA at this run's rate | done |")
  expect_true("| Parked | none |" %in% done)
  stuck <- .step_summary(list(n_remaining = 50L, n_shard = 50L), 60, 50)$output
  expect_identical(stuck[[length(stuck)]], "| ETA at this run's rate | n/a |")
})

# ---------------------------------------------------------------------------
# The core count a dispatch asks for
# ---------------------------------------------------------------------------

# Call set_analysis_cores as the shard step does, then print the ANALYSIS_CORES
# a shard's Rscript would inherit, or "unset".
.cores_bash <- function(input) {
  skip_on_os("windows")
  out <- suppressWarnings(system2("bash", c("-c", shQuote(sprintf(paste(
    "set -euo pipefail; unset ANALYSIS_CORES; source %s;",
    "set_analysis_cores %s || exit 1;",
    "printenv ANALYSIS_CORES || echo unset"),
    shQuote(.loop_script()), shQuote(input)))), stdout = TRUE, stderr = TRUE))
  list(status = attr(out, "status") %||% 0L, output = as.character(out))
}

test_that("an empty core count leaves ANALYSIS_CORES unset, and a count exports it", {
  expect_identical(.cores_bash(""), list(status = 0L, output = "unset"))
  expect_identical(.cores_bash("2"), list(status = 0L, output = "2"))
  expect_identical(.cores_bash("999"), list(status = 0L, output = "999"))
})

test_that("a core count that is not a whole number from 1 to 999 stops the step", {
  for (bad in c("0", "1000", "abc", "1.5", "-1", " ", "2 ")) {
    expect_identical(.cores_bash(bad), list(status = 1L, output = sprintf(
      "::error::analysis_cores must be a whole number from 1 to 999, got '%s'.", bad)))
  }
})
