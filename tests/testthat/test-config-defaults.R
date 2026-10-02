# tests/testthat/test-config-defaults.R: the per-package time limit, the worker
# count and the analyzer memory limit a run gets when nothing overrides them.

test_that("a package gets 2,400 s unless WORKER_TIMEOUT names another limit", {
  expect_identical(.config_under()$WORKER_TIMEOUT, 2400L)
  expect_identical(.config_under(c(WORKER_TIMEOUT = "900"))$WORKER_TIMEOUT, 900L)
})

test_that("a run uses every logical core unless ANALYSIS_CORES names a count", {
  dc <- suppressWarnings(parallel::detectCores(logical = TRUE))
  expect_identical(.config_under()$ANALYSIS_CORES, if (is.na(dc)) 1L else as.integer(dc))
  expect_identical(.config_under(c(ANALYSIS_CORES = "2"))$ANALYSIS_CORES, 2L)
})

test_that("an analyzer gets the default limit unless ANALYZER_MEMORY_LIMIT_MB names another, and 0 is none", {
  default <- .config_under()$ANALYZER_MEMORY_LIMIT_MB
  expect_identical(default, 3072L)
  expect_identical(.config_under(c(ANALYZER_MEMORY_LIMIT_MB = "4096"))$ANALYZER_MEMORY_LIMIT_MB,
                   4096L)
  expect_identical(.config_under(c(ANALYZER_MEMORY_LIMIT_MB = "0"))$ANALYZER_MEMORY_LIMIT_MB, 0L)
  # An empty value is what an unset workflow variable arrives as.
  for (bad in c("", "lots", "-1")) {
    expect_identical(.config_under(c(ANALYZER_MEMORY_LIMIT_MB = bad))$ANALYZER_MEMORY_LIMIT_MB,
                     default, info = bad)
  }
})
