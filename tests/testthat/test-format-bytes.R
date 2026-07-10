# tests/testthat/test-format-bytes.R

test_that("format_bytes renders each unit at the spec's boundaries", {
  expect_identical(format_bytes(0L),         "0 bytes")
  expect_identical(format_bytes(512L),       "512 bytes")
  expect_identical(format_bytes(1023L),      "1023 bytes")
  expect_identical(format_bytes(240128L),    "235 KB")     # exactly 234.5 KB, rounds half up
  expect_identical(format_bytes(912340224L), "870 MB")
  expect_identical(format_bytes(1503238553), "1.4 GB")
})

test_that("format_bytes handles NULL/NA as n/a, never a fabricated 0", {
  expect_identical(format_bytes(NULL),        "n/a")
  expect_identical(format_bytes(NA),          "n/a")
  expect_identical(format_bytes(NA_real_),    "n/a")
  expect_identical(format_bytes(numeric(0L)), "n/a")
})
