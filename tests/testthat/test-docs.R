# tests/testthat/test-docs.R

# Helper: build a context from an in-memory named list (path -> content).
make_docs_ctx <- function(file_map) {
  read_fn <- function(p) file_map[[p]] %||% ""
  build_context("pkg", "1.0", "v1.0", "2024-01-01", names(file_map), read_fn)
}

# --- Shared Rd fixtures -------------------------------------------------------

foo_rd <- paste0(
  "\\name{foo}\n",
  "\\alias{foo}\n",
  "\\title{Foo}\n",
  "\\usage{\nfoo(x, y)\n}\n",
  "\\arguments{\n\\item{x}{The x.}\n\\item{y}{The y.}\n}\n",
  "\\value{A numeric.}\n",
  "\\references{Smith (2020).}\n",
  "\\examples{\nfoo(1, 2)\n}\n"
)

bar_rd <- paste0(
  "\\name{bar}\n",
  "\\alias{bar}\n",
  "\\title{Bar}\n",
  "\\usage{\nbar(z)\n}\n",
  "\\arguments{\n\\item{z}{The z.}\n}\n",
  "\\value{A string.}\n",
  "\\examples{\n\\dontrun{\nbar('hello')\n}\n}\n"
)

# =============================================================================
# 1. Comprehensive positive case
# =============================================================================
test_that("metrics_docs: all metrics on a well-documented package", {
  readme <- paste0(
    "# pkg\n\n",
    "[![CI](https://img.shields.io/badge/build-passing.svg)](https://ci.example.com)\n\n",
    "This package does useful things.\n\n",
    "```r\ninstall.packages('pkg')\n```\n\n",
    "See `?foo` for details.\n"
  )
  news <- paste0(
    "# pkg 1.1.0\n\n",
    "* Added bar function.\n\n",
    "# pkg 1.0.0\n\n",
    "* Initial release.\n"
  )
  file_map <- list(
    "DESCRIPTION"   = "Package: pkg\nVersion: 1.0\n",
    "NAMESPACE"     = "export(foo)\nexport(bar)\n",
    "man/foo.Rd"    = foo_rd,
    "man/bar.Rd"    = bar_rd,
    "README.md"     = readme,
    "_pkgdown.yml"  = "site:\n  title: pkg\n",
    "NEWS.md"       = news
  )
  ctx <- make_docs_ctx(file_map)
  m   <- metrics_docs(ctx)

  # foo.Rd: plain examples (not fully wrapped); bar.Rd: fully \dontrun wrapped
  expect_equal(m$dontrun_example_ratio, 0.5)

  # All params in both Rd files are documented
  expect_equal(m$undocumented_params_rate, 0.0)

  # Both exported Rd files have \value
  expect_equal(m$value_doc_rate, 1.0)

  # foo.Rd has \references, bar.Rd does not -> 1/2
  expect_equal(m$references_coverage, 0.5)

  # Both exports (foo, bar) have Rd pages
  expect_equal(m$roxygen_doc_coverage, 1.0)

  expect_true(m$has_readme)
  expect_true(m$readme_prose_length > 0L)
  expect_true(m$has_pkgdown)
  expect_true(m$news_present)

  # NEWS: has headings, bullets, descending 1.1.0 -> 1.0.0
  expect_equal(m$news_structure_quality, 1.0)
})

# =============================================================================
# 2. Empty package - absent / NA defaults
# =============================================================================
test_that("metrics_docs: empty package returns NA or FALSE for all metrics", {
  ctx <- build_context("empty", "0.1", "v0.1", "2024-01-01",
                       character(0L), function(p) "")
  m <- metrics_docs(ctx)

  expect_true(is.na(m$dontrun_example_ratio))
  expect_true(is.na(m$undocumented_params_rate))
  expect_true(is.na(m$value_doc_rate))
  expect_true(is.na(m$references_coverage))
  expect_true(is.na(m$roxygen_doc_coverage))
  expect_false(m$has_readme)
  expect_true(is.na(m$readme_prose_length))
  expect_false(m$has_pkgdown)
  expect_false(m$news_present)
  expect_true(is.na(m$news_structure_quality))
})

# =============================================================================
# 3. dontrun_example_ratio: \donttest is treated the same as \dontrun
# =============================================================================
test_that("metrics_docs: dontrun_example_ratio counts fully-wrapped \\donttest", {
  rd <- paste0(
    "\\name{baz}\n\\alias{baz}\n",
    "\\examples{\n\\donttest{\n  baz(1)\n}\n}\n"
  )
  file_map <- list(
    "NAMESPACE"  = "",
    "man/baz.Rd" = rd
  )
  ctx <- make_docs_ctx(file_map)
  m   <- metrics_docs(ctx)

  expect_equal(m$dontrun_example_ratio, 1.0)
})

# =============================================================================
# 4. undocumented_params_rate: one parameter missing from \arguments
# =============================================================================
test_that("metrics_docs: undocumented_params_rate detects missing param", {
  rd <- paste0(
    "\\name{qux}\n\\alias{qux}\n",
    "\\usage{\nqux(x, y, z)\n}\n",
    "\\arguments{\n\\item{x}{Desc.}\n\\item{y}{Desc.}\n}\n",
    "\\value{A value.}\n"
  )
  file_map <- list(
    "NAMESPACE"  = "export(qux)\n",
    "man/qux.Rd" = rd
  )
  ctx <- make_docs_ctx(file_map)
  m   <- metrics_docs(ctx)

  # z is undocumented; rate = 1/3
  expect_equal(m$undocumented_params_rate, 1 / 3, tolerance = 1e-9)
})

# =============================================================================
# 5. value_doc_rate: mixed presence of \value across exported-function Rd files
# =============================================================================
test_that("metrics_docs: value_doc_rate handles partial \\value presence", {
  rd_with <- paste0("\\name{f1}\n\\alias{f1}\n\\value{Something.}\n")
  rd_without <- paste0("\\name{f2}\n\\alias{f2}\n\\title{No value.}\n")
  file_map <- list(
    "NAMESPACE"   = "export(f1)\nexport(f2)\n",
    "man/f1.Rd"   = rd_with,
    "man/f2.Rd"   = rd_without
  )
  ctx <- make_docs_ctx(file_map)
  m   <- metrics_docs(ctx)

  expect_equal(m$value_doc_rate, 0.5)
})

# =============================================================================
# 6. references_coverage: zero when no Rd file has \references
# =============================================================================
test_that("metrics_docs: references_coverage is 0 when no Rd has \\references", {
  rd1 <- "\\name{a}\n\\alias{a}\n\\title{A.}\n"
  rd2 <- "\\name{b}\n\\alias{b}\n\\title{B.}\n"
  file_map <- list(
    "NAMESPACE" = "",
    "man/a.Rd"  = rd1,
    "man/b.Rd"  = rd2
  )
  ctx <- make_docs_ctx(file_map)
  m   <- metrics_docs(ctx)

  expect_equal(m$references_coverage, 0.0)
})

# =============================================================================
# 7. roxygen_doc_coverage: exported symbol without a matching Rd page
# =============================================================================
test_that("metrics_docs: roxygen_doc_coverage is < 1 when an export has no Rd", {
  rd <- "\\name{documented}\n\\alias{documented}\n\\title{Yes.}\n"
  file_map <- list(
    "NAMESPACE"         = "export(documented)\nexport(undocumented)\n",
    "man/documented.Rd" = rd
  )
  ctx <- make_docs_ctx(file_map)
  m   <- metrics_docs(ctx)

  # 1 of 2 exports has a page
  expect_equal(m$roxygen_doc_coverage, 0.5)
})

# =============================================================================
# 8. has_readme: README.Rmd counts as well as README.md
# =============================================================================
test_that("metrics_docs: has_readme is TRUE for README.Rmd", {
  file_map <- list(
    "README.Rmd" = "---\ntitle: pkg\n---\nSome prose.\n"
  )
  ctx <- make_docs_ctx(file_map)
  m   <- metrics_docs(ctx)

  expect_true(m$has_readme)
})

# =============================================================================
# 9. readme_prose_length: fenced code blocks are stripped; prose is counted
# =============================================================================
test_that("metrics_docs: readme_prose_length strips code blocks and counts words", {
  # README with known prose words and a code block that should not be counted
  readme_code_only <- "```r\nfoo <- function(x) x + 1\n```\n"
  readme_with_prose <- paste0(
    "# pkg\n\nInstall and use.\n\n",
    "```r\ninstall.packages('pkg')\n```\n\n",
    "Works great.\n"
  )

  ctx_code <- make_docs_ctx(list("README.md" = readme_code_only))
  ctx_prose <- make_docs_ctx(list("README.md" = readme_with_prose))

  m_code  <- metrics_docs(ctx_code)
  m_prose <- metrics_docs(ctx_prose)

  # Code-only README has zero prose words
  expect_equal(m_code$readme_prose_length, 0L)
  # README with prose words has a positive count
  expect_true(m_prose$readme_prose_length > 0L)
})

# =============================================================================
# 10. has_pkgdown: pkgdown/_pkgdown.yml in a subdirectory also counts
# =============================================================================
test_that("metrics_docs: has_pkgdown detects pkgdown/_pkgdown.yml", {
  file_map <- list(
    "pkgdown/_pkgdown.yml" = "url: https://example.com\n"
  )
  ctx <- make_docs_ctx(file_map)
  m   <- metrics_docs(ctx)

  expect_true(m$has_pkgdown)
})

# =============================================================================
# 11. news_present: a plain NEWS file (no extension) counts
# =============================================================================
test_that("metrics_docs: news_present is TRUE for a plain NEWS file", {
  file_map <- list(
    "NEWS" = "Changes in version 1.0.0\n- Initial release.\n"
  )
  ctx <- make_docs_ctx(file_map)
  m   <- metrics_docs(ctx)

  expect_true(m$news_present)
})

# =============================================================================
# 12. news_structure_quality: unstructured content scores 0
# =============================================================================
test_that("metrics_docs: news_structure_quality is 0 for unstructured NEWS", {
  file_map <- list(
    "NEWS.md" = "Fixed a bug. Added a feature. Cleaned up code.\n"
  )
  ctx <- make_docs_ctx(file_map)
  m   <- metrics_docs(ctx)

  expect_equal(m$news_structure_quality, 0.0)
})

# =============================================================================
# 13. news_structure_quality: partial structure scores between 0 and 1
# =============================================================================
test_that("metrics_docs: news_structure_quality is 1/3 with only version headings", {
  # Has version headings but no bullets; only one heading so ordering is skipped
  file_map <- list(
    "NEWS.md" = "# pkg 1.0.0\n\nInitial release with no bullet points.\n"
  )
  ctx <- make_docs_ctx(file_map)
  m   <- metrics_docs(ctx)

  expect_equal(m$news_structure_quality, 1 / 3, tolerance = 1e-9)
})
