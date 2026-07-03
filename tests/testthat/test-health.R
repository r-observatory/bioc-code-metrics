# tests/testthat/test-health.R
# Tests for metrics_health(). Fixture contexts are built via build_context()
# with an in-memory named list (path -> content), matching the pattern in
# test-structure.R.

make_ctx <- function(map) {
  build_context("pkg", "1.0", "v1.0", "2024-01-01",
                names(map), function(p) map[[p]] %||% "")
}

# Helper: count lines in a piece of fixture content (paste collapse -> no
# trailing newline, so strsplit gives exactly one element per original line).
n_lines <- function(code) length(strsplit(code, "\n", fixed = TRUE)[[1L]])

# ============================================================
# on_exit_coverage_rate
# ============================================================

test_that("on_exit_coverage_rate: function with mutator AND on.exit -> 1.0", {
  code <- paste(c(
    "safe_opts <- function() {",
    "  on.exit(options(warn = 0))",
    "  options(warn = -1)",
    "}"
  ), collapse = "\n")
  map <- list(
    "DESCRIPTION" = "Package: pkg\nVersion: 1.0\n",
    "R/safe.R"    = code
  )
  m <- metrics_health(make_ctx(map))
  expect_equal(m$on_exit_coverage_rate, 1.0)
})

test_that("on_exit_coverage_rate: mutator without on.exit -> 0.0", {
  code <- paste(c(
    "unsafe_wd <- function(dir) {",
    "  setwd(dir)",
    "}"
  ), collapse = "\n")
  map <- list(
    "DESCRIPTION" = "Package: pkg\nVersion: 1.0\n",
    "R/unsafe.R"  = code
  )
  m <- metrics_health(make_ctx(map))
  expect_equal(m$on_exit_coverage_rate, 0.0)
})

test_that("on_exit_coverage_rate: no mutating functions -> NA", {
  map <- list(
    "DESCRIPTION" = "Package: pkg\nVersion: 1.0\n",
    "R/pure.R"    = "add <- function(x, y) x + y"
  )
  m <- metrics_health(make_ctx(map))
  expect_true(is.na(m$on_exit_coverage_rate))
})

test_that("on_exit_coverage_rate: no R/ files -> NA", {
  map <- list("DESCRIPTION" = "Package: pkg\nVersion: 1.0\n")
  m <- metrics_health(make_ctx(map))
  expect_true(is.na(m$on_exit_coverage_rate))
})

test_that("on_exit_coverage_rate: partial coverage (1 of 2 mutating fns) -> 0.5", {
  code <- paste(c(
    "fn_safe <- function() {",
    "  on.exit(options(warn = 0))",
    "  options(warn = -1)",
    "}",
    "fn_unsafe <- function(dir) {",
    "  setwd(dir)",
    "}"
  ), collapse = "\n")
  map <- list(
    "DESCRIPTION" = "Package: pkg\nVersion: 1.0\n",
    "R/fns.R"     = code
  )
  m <- metrics_health(make_ctx(map))
  expect_equal(m$on_exit_coverage_rate, 0.5)
})

# ============================================================
# global_state_write_density
# ============================================================

test_that("global_state_write_density: counts <<-, options setter, Sys.setenv per KLOC", {
  code <- paste(c(
    "x <<- 1",
    "options(warn = -1)",
    "Sys.setenv(FOO = 'bar')"
  ), collapse = "\n")
  # 3 hits across n_lines() lines
  expected <- 3 / (n_lines(code) / 1000)
  map <- list(
    "DESCRIPTION" = "Package: pkg\nVersion: 1.0\n",
    "R/state.R"   = code
  )
  m <- metrics_health(make_ctx(map))
  expect_equal(m$global_state_write_density, expected)
})

test_that("global_state_write_density: clean code -> 0", {
  map <- list(
    "DESCRIPTION" = "Package: pkg\nVersion: 1.0\n",
    "R/clean.R"   = "add <- function(x, y) x + y"
  )
  m <- metrics_health(make_ctx(map))
  expect_equal(m$global_state_write_density, 0)
})

test_that("global_state_write_density: no R/ files -> NA", {
  map <- list("DESCRIPTION" = "Package: pkg\nVersion: 1.0\n")
  m <- metrics_health(make_ctx(map))
  expect_true(is.na(m$global_state_write_density))
})

test_that("global_state_write_density: commented-out <<- not counted", {
  code <- paste(c(
    "# x <<- 1  (removed, was global)",
    "x <- 1"
  ), collapse = "\n")
  map <- list(
    "DESCRIPTION"  = "Package: pkg\nVersion: 1.0\n",
    "R/comments.R" = code
  )
  m <- metrics_health(make_ctx(map))
  expect_equal(m$global_state_write_density, 0)
})

# ============================================================
# deprecated_idiom_density
# ============================================================

test_that("deprecated_idiom_density: detects bare T, 1:length, .Internal per KLOC", {
  code <- paste(c(
    "foo <- function(x) {",
    "  if (T) invisible(x)",
    "  for (i in 1:length(x)) x[[i]]",
    "  .Internal(x)",
    "}"
  ), collapse = "\n")
  # Hits: T(1), 1:length(1), .Internal(1) = 3 across n_lines() lines
  expected <- 3 / (n_lines(code) / 1000)
  map <- list(
    "DESCRIPTION" = "Package: pkg\nVersion: 1.0\n",
    "R/idioms.R"  = code
  )
  m <- metrics_health(make_ctx(map))
  expect_equal(m$deprecated_idiom_density, expected)
})

test_that("deprecated_idiom_density: TRUE/FALSE literals not counted as T/F", {
  code <- paste(c(
    "x <- TRUE",
    "y <- FALSE",
    "if (TRUE) x else y"
  ), collapse = "\n")
  map <- list(
    "DESCRIPTION" = "Package: pkg\nVersion: 1.0\n",
    "R/bools.R"   = code
  )
  m <- metrics_health(make_ctx(map))
  expect_equal(m$deprecated_idiom_density, 0)
})

test_that("deprecated_idiom_density: no R/ files -> NA", {
  map <- list("DESCRIPTION" = "Package: pkg\nVersion: 1.0\n")
  m <- metrics_health(make_ctx(map))
  expect_true(is.na(m$deprecated_idiom_density))
})

test_that("deprecated_idiom_density: indented require/library counted, top-level not", {
  code <- paste(c(
    "foo <- function() {",
    "  library(dplyr)",   # 2-space indent -> inside body -> counted
    "}",
    "library(utils)"      # no indent -> top-level -> not counted
  ), collapse = "\n")
  # Only 1 hit (the indented library)
  expected <- 1 / (n_lines(code) / 1000)
  map <- list(
    "DESCRIPTION" = "Package: pkg\nVersion: 1.0\n",
    "R/pkg.R"     = code
  )
  m <- metrics_health(make_ctx(map))
  expect_equal(m$deprecated_idiom_density, expected)
})

# ============================================================
# debug_artifact_density
# ============================================================

test_that("debug_artifact_density: browser, print, cat counted per KLOC", {
  code <- paste(c(
    "foo <- function(x) {",
    "  browser()",
    "  print(x)",
    "  cat('debug\\n')",
    "  x",
    "}"
  ), collapse = "\n")
  # 3 hits (browser, print, cat) across n_lines() lines
  expected <- 3 / (n_lines(code) / 1000)
  map <- list(
    "DESCRIPTION" = "Package: pkg\nVersion: 1.0\n",
    "R/debug.R"   = code
  )
  m <- metrics_health(make_ctx(map))
  expect_equal(m$debug_artifact_density, expected)
})

test_that("debug_artifact_density: clean R code with message() -> 0", {
  code <- paste(c(
    "foo <- function(x) {",
    "  message('computing result')",
    "  x + 1",
    "}"
  ), collapse = "\n")
  map <- list(
    "DESCRIPTION" = "Package: pkg\nVersion: 1.0\n",
    "R/clean.R"   = code
  )
  m <- metrics_health(make_ctx(map))
  expect_equal(m$debug_artifact_density, 0)
})

test_that("debug_artifact_density: no R/ files -> NA", {
  map <- list("DESCRIPTION" = "Package: pkg\nVersion: 1.0\n")
  m <- metrics_health(make_ctx(map))
  expect_true(is.na(m$debug_artifact_density))
})

test_that("debug_artifact_density: print inside another call (not stmt-pos) not counted", {
  code <- paste(c(
    "foo <- function(x) {",
    "  y <- invisible(print(x))",   # print not at statement position
    "  y",
    "}"
  ), collapse = "\n")
  map <- list(
    "DESCRIPTION" = "Package: pkg\nVersion: 1.0\n",
    "R/ok.R"      = code
  )
  m <- metrics_health(make_ctx(map))
  expect_equal(m$debug_artifact_density, 0)
})

# ============================================================
# has_code_of_conduct
# ============================================================

test_that("has_code_of_conduct: TRUE when CODE_OF_CONDUCT.md at package root", {
  map <- list(
    "DESCRIPTION"        = "Package: pkg\nVersion: 1.0\n",
    "CODE_OF_CONDUCT.md" = "# Code of Conduct\n"
  )
  m <- metrics_health(make_ctx(map))
  expect_true(m$has_code_of_conduct)
})

test_that("has_code_of_conduct: TRUE when under .github/", {
  map <- list(
    "DESCRIPTION"                = "Package: pkg\nVersion: 1.0\n",
    ".github/CODE_OF_CONDUCT.md" = "# CoC\n"
  )
  m <- metrics_health(make_ctx(map))
  expect_true(m$has_code_of_conduct)
})

test_that("has_code_of_conduct: FALSE when neither file present", {
  map <- list("DESCRIPTION" = "Package: pkg\nVersion: 1.0\n")
  m <- metrics_health(make_ctx(map))
  expect_false(m$has_code_of_conduct)
})

# ============================================================
# has_contributing_guide
# ============================================================

test_that("has_contributing_guide: TRUE when CONTRIBUTING.md at package root", {
  map <- list(
    "DESCRIPTION"     = "Package: pkg\nVersion: 1.0\n",
    "CONTRIBUTING.md" = "# Contributing\n"
  )
  m <- metrics_health(make_ctx(map))
  expect_true(m$has_contributing_guide)
})

test_that("has_contributing_guide: TRUE when under .github/", {
  map <- list(
    "DESCRIPTION"             = "Package: pkg\nVersion: 1.0\n",
    ".github/CONTRIBUTING.md" = "# How to contribute\n"
  )
  m <- metrics_health(make_ctx(map))
  expect_true(m$has_contributing_guide)
})

test_that("has_contributing_guide: FALSE when neither file present", {
  map <- list("DESCRIPTION" = "Package: pkg\nVersion: 1.0\n")
  m <- metrics_health(make_ctx(map))
  expect_false(m$has_contributing_guide)
})
