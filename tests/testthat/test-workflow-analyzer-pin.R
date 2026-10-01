# tests/testthat/test-workflow-analyzer-pin.R
# The tests and the daily update must run the same analyzer build.
.analyzer_pins <- function(file) {
  yml <- readLines(file.path("..", "..", ".github", "workflows", file))
  hits <- regmatches(yml, regexpr(
    "gh release download v[0-9]+\\.[0-9]+\\.[0-9]+ --repo r-observatory/rpkg-analyzer", yml))
  sub("^gh release download (v[0-9.]+) .*$", "\\1", hits)
}

test_that("update.yml and test.yml install rpkg-analyzer v0.5.1", {
  expect_identical(.analyzer_pins("update.yml"), "v0.5.1")
  expect_identical(.analyzer_pins("test.yml"), "v0.5.1")
})

test_that("the pinned build is in ANALYZER_SAME_OUTPUT, so a pin change says what it re-queues", {
  expect_true(sub("^v", "", .analyzer_pins("update.yml")) %in% ANALYZER_SAME_OUTPUT)
})

test_that("0.5.1 counts as 0.5.0 and 0.4.0 stands alone, so this pin re-queues nothing new", {
  expect_identical(ANALYZER_SAME_OUTPUT, c("0.5.0", "0.5.1"))
  expect_identical(.analyzer_output_class("0.5.1"), c("0.5.0", "0.5.1"))
  expect_identical(.analyzer_output_class("0.4.0"), "0.4.0")
})

test_that("the installed analyzer reports the pinned build, as the class names it", {
  skip_if(!nzchar(Sys.getenv("RPKG_ANALYZER_BIN")), "RPKG_ANALYZER_BIN is not set")
  v <- rpkg_analyzer_version()
  expect_identical(v, sub("^v", "", .analyzer_pins("update.yml")))
  expect_true(v %in% ANALYZER_SAME_OUTPUT)
})
