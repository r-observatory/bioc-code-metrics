# tests/testthat/test-config-split.R
test_that("the code and data DB filenames are distinct and correct", {
  expect_identical(DB_FILENAME, "bioc-code-metrics.db")
  expect_identical(DATA_DB_FILENAME, "bioc-data-metrics.db")
})
