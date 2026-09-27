# tests/testthat/test-workflow-analyzer-pin.R
# The tests and the daily update must run the same analyzer build.
.analyzer_pins <- function(file) {
  yml <- readLines(file.path("..", "..", ".github", "workflows", file))
  hits <- regmatches(yml, regexpr(
    "gh release download v[0-9]+\\.[0-9]+\\.[0-9]+ --repo r-observatory/rpkg-analyzer", yml))
  sub("^gh release download (v[0-9.]+) .*$", "\\1", hits)
}

test_that("update.yml and test.yml install rpkg-analyzer v0.5.0", {
  expect_identical(.analyzer_pins("update.yml"), "v0.5.0")
  expect_identical(.analyzer_pins("test.yml"), "v0.5.0")
})
