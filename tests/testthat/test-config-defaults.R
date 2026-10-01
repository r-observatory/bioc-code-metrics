# tests/testthat/test-config-defaults.R: the per-package time limit and the
# worker count a run gets when nothing overrides them.

test_that("a package gets 2,400 s unless WORKER_TIMEOUT names another limit", {
  expect_identical(.config_under()$WORKER_TIMEOUT, 2400L)
  expect_identical(.config_under(c(WORKER_TIMEOUT = "900"))$WORKER_TIMEOUT, 900L)
})

test_that("a run uses every logical core unless ANALYSIS_CORES names a count", {
  dc <- suppressWarnings(parallel::detectCores(logical = TRUE))
  expect_identical(.config_under()$ANALYSIS_CORES, if (is.na(dc)) 1L else as.integer(dc))
  expect_identical(.config_under(c(ANALYSIS_CORES = "2"))$ANALYSIS_CORES, 2L)
})
