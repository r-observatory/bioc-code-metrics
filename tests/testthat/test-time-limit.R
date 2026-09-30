# tests/testthat/test-time-limit.R: the retry after a swallowed elapsed cap.

test_that("a cap on the first evaluation evaluates once more and returns that value", {
  n <- 0L
  out <- .retry_after_time_limit({
    n <- n + 1L
    if (n == 1L) stop(.cap_error())
    "second"
  }, error = function(e) "handler")
  expect_identical(out, "second")
  expect_identical(n, 2L)
})

test_that("any other error goes to the handler after one evaluation", {
  n <- 0L
  seen <- NULL
  out <- .retry_after_time_limit({
    n <- n + 1L
    stop("boom")
  }, error = function(e) {
    seen <<- conditionMessage(e)
    "handled"
  })
  expect_identical(out, "handled")
  expect_identical(seen, "boom")
  expect_identical(n, 1L)
})

test_that("a cap on both evaluations reaches the handler after exactly two", {
  n <- 0L
  seen <- NULL
  out <- .retry_after_time_limit({
    n <- n + 1L
    stop(.cap_error())
  }, error = function(e) {
    seen <<- e
    "handled"
  })
  expect_identical(out, "handled")
  expect_identical(n, 2L)
  expect_true(.is_time_limit(seen))
})

test_that("an assignment inside a braced block lands in the caller's frame", {
  f <- function() {
    x <- "before"
    .retry_after_time_limit({
      x <- "inside"
      y <- 1L
    }, error = function(e) NULL)
    list(x = x, y = get0("y", inherits = FALSE))
  }
  expect_identical(f(), list(x = "inside", y = 1L))
})

test_that("the expression sees the calling function's arguments", {
  g <- function(p) {
    vapply(c("a", "b"), function(s) {
      .retry_after_time_limit(paste0(p, s), error = function(e) NA_character_)
    }, character(1L), USE.NAMES = FALSE)
  }
  expect_identical(g("x"), c("xa", "xb"))
})

test_that("a handler that raises passes its condition on", {
  err <- tryCatch(
    .retry_after_time_limit(stop(structure(
      class = c("my_failure", "error", "condition"),
      list(message = "mine", call = NULL))), error = function(e) stop(e)),
    error = function(e) e)
  expect_s3_class(err, "my_failure")
})

test_that("in a fork, a plain tryCatch swallows the real cap into its fallback", {
  skip_on_os("windows")
  job <- parallel::mcparallel({
    setTimeLimit(elapsed = 0.2, transient = TRUE)
    tryCatch({
      .busy(0.6)
      "finished"
    }, error = function(e) "fallback")
  })
  expect_identical(parallel::mccollect(job, wait = TRUE)[[1L]], "fallback")
})

test_that("in a fork, the real cap swallowed by the helper evaluates again", {
  skip_on_os("windows")
  job <- parallel::mcparallel({
    setTimeLimit(elapsed = 0.2, transient = TRUE)
    n <- 0L
    .retry_after_time_limit({
      n <- n + 1L
      .busy(0.6)
      n
    }, error = function(e) -1L)
  })
  expect_identical(parallel::mccollect(job, wait = TRUE)[[1L]], 2L)
})

test_that("the cap is recognised in a translated session", {
  skip_on_os("windows")
  withr::local_language("de")
  skip_if(identical(.time_limit_msg(), "reached elapsed time limit"),
          "R has no German catalog here")
  job <- parallel::mcparallel({
    setTimeLimit(elapsed = 0.2, transient = TRUE)
    e <- tryCatch({
      .busy(0.6)
      NULL
    }, error = function(e) e)
    list(msg = conditionMessage(e), is = .is_time_limit(e))
  })
  res <- parallel::mccollect(job, wait = TRUE)[[1L]]
  expect_true(res$is)
  expect_false(identical(res$msg, "reached elapsed time limit"))
})
