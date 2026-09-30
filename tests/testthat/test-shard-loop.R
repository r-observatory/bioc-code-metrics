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
