# tests/testthat/test-redact-reason.R: failure reasons are safe to print and store.

test_that("a token in a clone URL is replaced", {
  expect_identical(
    .redact_reason("fatal: unable to access 'https://x-access-token:abc@github.com/cran/x.git/'"),
    "fatal: unable to access 'https://***github.com/cran/x.git/'")
})

test_that("GitHub token shapes are replaced wherever they appear", {
  expect_identical(.redact_reason("token ghs_AbC123xyz here"), "token *** here")
  expect_identical(.redact_reason("ghp_1 gho_2 ghu_3 ghr_4"), "*** *** *** ***")
  expect_identical(.redact_reason("github_pat_11AB_cd9 leaked"), "*** leaked")
})

test_that("several lines become one", {
  expect_identical(.redact_reason(c("first\nsecond", "third")), "first second third")
})

test_that("bytes that are not UTF-8 come back as <xx> and the result is valid UTF-8", {
  out <- .redact_reason("bad \xff\xfe path")
  expect_identical(out, "bad <ff><fe> path")
  expect_true(validUTF8(out))
})

test_that("a long reason is cut to the byte budget on a character boundary", {
  out <- .redact_reason(strrep("é", 400L), max_bytes = 512L)
  expect_lte(nchar(out, type = "bytes"), 512L)
  expect_true(validUTF8(out))
  expect_true(endsWith(out, "..."))
})

test_that("nothing to say is an empty string", {
  expect_identical(.redact_reason(character(0L)), "")
  expect_identical(.redact_reason(NA_character_), "")
  expect_identical(.redact_reason(NULL), "")
})
