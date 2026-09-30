# tests/testthat/test-time-limit-handlers.R: every error handler in worker
# code retries a swallowed cap, or is listed here with the reason it need not.
#
# A tryCatch with an error handler nested inside .retry_after_time_limit is
# still flagged: it catches the cap before the retry can see it.

.HANDLER_ALLOWLIST <- data.frame(
  file = c("analyze.R", "update.R", "update.R", "update.R"),
  fn   = c("analyze_package", ".done", ".pkg_worker", ".pkg_worker"),
  call = c("try", "try", "tryCatch", "tryCatch"),
  expr = c(NA, NA, "io$clone(pkg, dest)", "analyze_package(dest, pkg)"),
  reason = c(
    "the per-release heartbeat only prints a progress line",
    "the worker's completion line only prints",
    "the clone is the worker's first step and ends by GIT_TIMEOUT, before the cap can fire",
    "this is where the package's verdict is made"),
  stringsAsFactors = FALSE)

test_that("every error handler in worker code retries a swallowed cap or is allowlisted", {
  inv <- .worker_handler_inventory()
  bad <- .unlisted_handlers(inv, .HANDLER_ALLOWLIST)
  expect_identical(nrow(bad), 0L,
                   info = paste(sprintf("%s:%d %s in %s", bad$file, bad$line, bad$call, bad$fn),
                                collapse = "\n"))
})

test_that("every allowlisted handler is still there, exactly once", {
  inv  <- .worker_handler_inventory()
  hits <- vapply(seq_len(nrow(.HANDLER_ALLOWLIST)), function(i) {
    sum(.allow_matches(inv, .HANDLER_ALLOWLIST[i, , drop = FALSE]))
  }, integer(1L))
  expect_identical(hits, rep(1L, nrow(.HANDLER_ALLOWLIST)))
})

test_that("the scan sees every wrapped handler, so it cannot pass by reading nothing", {
  inv <- .worker_handler_inventory()
  expect_identical(attr(inv, "n_retry"), 37L)
  expect_identical(sum(inv$flagged), nrow(.HANDLER_ALLOWLIST))
})

test_that("the scan rejects a handler nobody listed, including one nested in a retry", {
  f <- withr::local_tempfile(fileext = ".R")
  writeLines(c(
    "reader <- function(path) {",
    "  r1 <- .retry_after_time_limit(readLines(path), error = function(e) character(0))",
    "  r2 <- tryCatch(readLines(path), warning = function(w) NULL)",
    "  r3 <- tryCatch(readLines(path), error = function(e) character(0))",
    "  r4 <- .retry_after_time_limit(tryCatch(nchar(r1), condition = function(e) 0L),",
    "                                error = function(e) 0L)",
    "  r5 <- base::try(stop('x'), silent = TRUE)",
    "  list(r1, r2, r3, r4, r5)",
    "}"), f)
  inv <- .handler_inventory(f, "fixture.R")
  bad <- .unlisted_handlers(inv, .HANDLER_ALLOWLIST)
  expect_identical(bad$line, c(4L, 5L, 7L))
  expect_identical(bad$fn, rep("reader", 3L))
  expect_identical(bad$expr, c("readLines(path)", "nchar(r1)", "stop('x')"))
  expect_identical(attr(inv, "n_retry"), 2L)
})

test_that("an allowlisted name in another file or function is not accepted", {
  f <- withr::local_tempfile(fileext = ".R")
  writeLines(c(
    "analyze_package <- function(dest, pkg) {",
    "  try(cat('x'), silent = TRUE)",
    "}"), f)
  inv <- .handler_inventory(f, "other.R")
  expect_identical(nrow(.unlisted_handlers(inv, .HANDLER_ALLOWLIST)), 1L)
})
