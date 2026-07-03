# tests/testthat/test-functions.R

# ── n_exports ─────────────────────────────────────────────────────────────────

test_that("n_exports counts explicit NAMESPACE export() entries", {
  map <- list(
    "DESCRIPTION" = "Package: mypkg\nVersion: 1.0\n",
    "NAMESPACE"   = "export(foo)\nexport(bar)\n",
    "R/foo.R"     = "foo <- function(x) x + 1\n",
    "R/bar.R"     = "bar <- function() NULL\nbaz <- function() NULL\n"
  )
  ctx <- build_context("mypkg", "1.0", "1.0", "2024-01-01",
                       names(map), function(p) map[[p]] %||% "")
  m   <- metrics_functions(ctx)

  expect_equal(m$n_exports, 2L)
  # baz is defined in R/ but not exported
  expect_equal(m$n_internal, 1L)
})

test_that("n_exports is NA when NAMESPACE file is absent", {
  map <- list(
    "DESCRIPTION" = "Package: mypkg\nVersion: 1.0\n",
    "R/foo.R"     = "foo <- function(x) x\n"
  )
  ctx <- build_context("mypkg", "1.0", "1.0", "2024-01-01",
                       names(map), function(p) map[[p]] %||% "")
  m   <- metrics_functions(ctx)

  expect_true(is.na(m$n_exports))
})

test_that("n_exports uses exportPattern to count matching R/ function defs", {
  # Package only uses exportPattern (no explicit export())
  map <- list(
    "DESCRIPTION" = "Package: mypkg\nVersion: 1.0\n",
    "NAMESPACE"   = "exportPattern(\"^[^.]\")\n",
    "R/api.R"     = paste0(
      "public_a <- function() NULL\n",
      "public_b <- function() NULL\n",
      ".internal <- function() NULL\n"  # dot-prefixed; should NOT match ^[^.]
    )
  )
  ctx <- build_context("mypkg", "1.0", "1.0", "2024-01-01",
                       names(map), function(p) map[[p]] %||% "")
  m   <- metrics_functions(ctx)

  expect_equal(m$n_exports,  2L)   # public_a and public_b
  expect_equal(m$n_internal, 1L)   # .internal
})

test_that("n_exports is 0 for NAMESPACE with no export directives", {
  map <- list(
    "DESCRIPTION" = "Package: mypkg\nVersion: 1.0\n",
    "NAMESPACE"   = "importFrom(stats, lm)\n",
    "R/foo.R"     = "foo <- function() NULL\n"
  )
  ctx <- build_context("mypkg", "1.0", "1.0", "2024-01-01",
                       names(map), function(p) map[[p]] %||% "")
  m   <- metrics_functions(ctx)

  expect_equal(m$n_exports, 0L)
})

# ── n_internal ────────────────────────────────────────────────────────────────

test_that("n_internal counts unexported top-level R/ function defs", {
  map <- list(
    "DESCRIPTION" = "Package: p\nVersion: 1.0\n",
    "NAMESPACE"   = "export(pub)\n",
    "R/a.R"       = paste0(
      "pub      <- function() NULL\n",
      "priv_one <- function() NULL\n",
      "priv_two <- function() NULL\n"
    )
  )
  ctx <- build_context("p", "1.0", "1.0", "2024-01-01",
                       names(map), function(p) map[[p]] %||% "")
  m   <- metrics_functions(ctx)

  expect_equal(m$n_internal, 2L)
})

test_that("n_internal is 0 when no R/ files are present", {
  map <- list(
    "DESCRIPTION" = "Package: p\nVersion: 1.0\n",
    "NAMESPACE"   = "useDynLib(p)\n"
  )
  ctx <- build_context("p", "1.0", "1.0", "2024-01-01",
                       names(map), function(p) map[[p]] %||% "")
  m   <- metrics_functions(ctx)

  expect_equal(m$n_internal, 0L)
})

test_that("n_internal is 0 when all R/ functions are exported", {
  map <- list(
    "DESCRIPTION" = "Package: p\nVersion: 1.0\n",
    "NAMESPACE"   = "export(a)\nexport(b)\n",
    "R/a.R"       = "a <- function() NULL\nb <- function() NULL\n"
  )
  ctx <- build_context("p", "1.0", "1.0", "2024-01-01",
                       names(map), function(p) map[[p]] %||% "")
  m   <- metrics_functions(ctx)

  expect_equal(m$n_internal, 0L)
})

# ── nse_surface ───────────────────────────────────────────────────────────────

test_that("nse_surface_n counts exported functions that use NSE keywords", {
  map <- list(
    "DESCRIPTION" = "Package: p\nVersion: 1.0\n",
    "NAMESPACE"   = "export(nse_fn)\nexport(clean_fn)\n",
    "R/fns.R"     = paste0(
      "nse_fn <- function(x) {\n",
      "  substitute(x)\n",
      "}\n",
      "clean_fn <- function(x) {\n",
      "  x + 1\n",
      "}\n"
    )
  )
  ctx <- build_context("p", "1.0", "1.0", "2024-01-01",
                       names(map), function(p) map[[p]] %||% "")
  m   <- metrics_functions(ctx)

  expect_equal(m$nse_surface_n,    1L)
  expect_equal(m$nse_surface_frac, 0.5, tolerance = 1e-9)
})

test_that("nse_surface_n and _frac are NA when NAMESPACE is absent", {
  map <- list(
    "DESCRIPTION" = "Package: p\nVersion: 1.0\n",
    "R/fns.R"     = "foo <- function(x) eval(x)\n"
  )
  ctx <- build_context("p", "1.0", "1.0", "2024-01-01",
                       names(map), function(p) map[[p]] %||% "")
  m   <- metrics_functions(ctx)

  expect_true(is.na(m$nse_surface_n))
  expect_true(is.na(m$nse_surface_frac))
})

test_that("nse_surface does not count NSE in unexported (internal) functions", {
  map <- list(
    "DESCRIPTION" = "Package: p\nVersion: 1.0\n",
    "NAMESPACE"   = "export(clean_fn)\n",
    "R/fns.R"     = paste0(
      "clean_fn <- function(x) {\n",
      "  x + 1\n",
      "}\n",
      "internal_nse <- function(x) {\n",
      "  substitute(x)\n",
      "}\n"
    )
  )
  ctx <- build_context("p", "1.0", "1.0", "2024-01-01",
                       names(map), function(p) map[[p]] %||% "")
  m   <- metrics_functions(ctx)

  expect_equal(m$nse_surface_n,    0L)
  expect_equal(m$nse_surface_frac, 0, tolerance = 1e-9)
})

test_that("nse_surface detects all NSE keywords: eval quote bquote match.call sys.call", {
  keywords <- c("eval(x)", "quote(x)", "bquote(x)", "match.call()", "sys.call()")
  for (kw in keywords) {
    fn_body <- paste0(
      "f <- function(x) {\n",
      "  ", kw, "\n",
      "}\n"
    )
    map <- list(
      "DESCRIPTION" = "Package: p\nVersion: 1.0\n",
      "NAMESPACE"   = "export(f)\n",
      "R/f.R"       = fn_body
    )
    ctx <- build_context("p", "1.0", "1.0", "2024-01-01",
                         names(map), function(p) map[[p]] %||% "")
    m   <- metrics_functions(ctx)
    expect_equal(m$nse_surface_n, 1L,
                 info = paste("keyword:", kw))
  }
})

test_that("nse_surface_frac is NA when n_exports is 0", {
  map <- list(
    "DESCRIPTION" = "Package: p\nVersion: 1.0\n",
    "NAMESPACE"   = "importFrom(stats, lm)\n",
    "R/f.R"       = "f <- function(x) eval(x)\n"
  )
  ctx <- build_context("p", "1.0", "1.0", "2024-01-01",
                       names(map), function(p) map[[p]] %||% "")
  m   <- metrics_functions(ctx)

  # n_exports == 0 so fraction is undefined
  expect_true(is.na(m$nse_surface_frac))
})

# ── triple_colon_count ────────────────────────────────────────────────────────

test_that("triple_colon_count detects pkg:::sym usage in R/ files", {
  map <- list(
    "DESCRIPTION" = "Package: mypkg\nVersion: 1.0\n",
    "NAMESPACE"   = "export(f)\n",
    "R/f.R"       = paste0(
      "f <- function() {\n",
      "  otherpkg:::hidden_fn()\n",
      "  anotherpkg:::secret()\n",
      "  otherpkg:::another()\n",
      "}\n"
    )
  )
  ctx <- build_context("mypkg", "1.0", "1.0", "2024-01-01",
                       names(map), function(p) map[[p]] %||% "")
  m   <- metrics_functions(ctx)

  # 3 occurrences, 2 distinct external packages
  expect_equal(m$triple_colon_count, 3L)
  expect_equal(m$triple_colon_pkgs,  2L)
})

test_that("triple_colon_count is 0 when no R/ files are present", {
  map <- list(
    "DESCRIPTION" = "Package: p\nVersion: 1.0\n",
    "NAMESPACE"   = "useDynLib(p)\n"
  )
  ctx <- build_context("p", "1.0", "1.0", "2024-01-01",
                       names(map), function(p) map[[p]] %||% "")
  m   <- metrics_functions(ctx)

  expect_equal(m$triple_colon_count, 0L)
  expect_equal(m$triple_colon_pkgs,  0L)
})

test_that("triple_colon_pkgs excludes self-references", {
  # Package "mypkg" calls mypkg:::hidden internally (unusual but legal)
  map <- list(
    "DESCRIPTION" = "Package: mypkg\nVersion: 1.0\n",
    "NAMESPACE"   = "export(f)\n",
    "R/f.R"       = paste0(
      "f <- function() {\n",
      "  mypkg:::hidden()\n",   # self-reference
      "  extpkg:::sym()\n",     # external
      "}\n"
    )
  )
  ctx <- build_context("mypkg", "1.0", "1.0", "2024-01-01",
                       names(map), function(p) map[[p]] %||% "")
  m   <- metrics_functions(ctx)

  expect_equal(m$triple_colon_count, 2L)   # both occurrences counted
  expect_equal(m$triple_colon_pkgs,  1L)   # only extpkg is external
})

test_that("triple_colon_count is 0 when R/ files exist but have no ::: calls", {
  map <- list(
    "DESCRIPTION" = "Package: p\nVersion: 1.0\n",
    "NAMESPACE"   = "export(f)\n",
    "R/f.R"       = "f <- function(x) x + 1\n"
  )
  ctx <- build_context("p", "1.0", "1.0", "2024-01-01",
                       names(map), function(p) map[[p]] %||% "")
  m   <- metrics_functions(ctx)

  expect_equal(m$triple_colon_count, 0L)
  expect_equal(m$triple_colon_pkgs,  0L)
})
