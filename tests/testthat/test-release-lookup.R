# .current_bioc_release: the up-to-date check compares every stored version
# against this value, so a failed lookup makes the whole catalog look current.
# That no-op is deliberate and stays, but it must never be silent: a lookup that
# fails on the day Bioconductor cuts a release would otherwise defer the whole
# update with nothing in the log to say so.

no_sleep <- function(s) invisible(NULL)

ok_lines <- c('release_version: "3.23"', 'devel_version: "3.24"')

test_that(".current_bioc_release returns the release parsed from config.yaml", {
  expect_equal(
    .current_bioc_release(read = function(u) ok_lines, sleep = no_sleep),
    "3.23")
})

test_that(".current_bioc_release retries a failing read and returns the recovered release", {
  calls <- 0L

  val <- .current_bioc_release(
    read = function(u) {
      calls <<- calls + 1L
      if (calls < 3L) stop("HTTP status was '504 Gateway Timeout'")
      ok_lines
    },
    sleep = no_sleep)

  expect_equal(val, "3.23")
  expect_equal(calls, 3L)
})

test_that(".current_bioc_release keeps the safe no-op when every attempt fails", {
  val <- suppressMessages(.current_bioc_release(
    read = function(u) stop("HTTP status was '504 Gateway Timeout'"),
    waits = c(0, 0), sleep = no_sleep))

  expect_true(is.na(val))
  expect_type(val, "character")
})

test_that(".current_bioc_release warns loudly when the lookup fails", {
  expect_message(
    .current_bioc_release(read = function(u) stop("504 Gateway Timeout"),
                          waits = 0, sleep = no_sleep),
    "::warning::")
})

test_that(".current_bioc_release names the consequence in its warning", {
  expect_message(
    .current_bioc_release(read = function(u) stop("504 Gateway Timeout"),
                          waits = 0, sleep = no_sleep),
    "up to date")
})

test_that(".current_bioc_release retries config.yaml with no release_version line", {
  calls <- 0L

  val <- .current_bioc_release(
    read = function(u) {
      calls <<- calls + 1L
      if (calls < 2L) c("devel_version: 3.24") else ok_lines
    },
    sleep = no_sleep)

  expect_equal(val, "3.23")
  expect_equal(calls, 2L)
})

test_that(".current_bioc_release stays quiet when the lookup succeeds", {
  expect_no_message(
    .current_bioc_release(read = function(u) ok_lines, sleep = no_sleep))
})

test_that("the release-lookup backoff covers more than fifteen minutes", {
  expect_gt(sum(RELEASE_RETRY_WAITS_S), 15 * 60)
})
