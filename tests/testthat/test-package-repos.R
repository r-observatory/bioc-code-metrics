# tests/testthat/test-package-repos.R
test_that("bioc_package_repos covers software, workflow, and experiment", {
  repos <- bioc_package_repos()
  expect_true(any(grepl("/release/bioc$", repos)))
  expect_true(any(grepl("/release/workflows$", repos)))
  expect_true(any(grepl("/release/data/experiment$", repos)))
})

test_that("bioc_package_repos omits annotation (no git repos / RELEASE branches)", {
  expect_false(any(grepl("annotation", bioc_package_repos())))
})
