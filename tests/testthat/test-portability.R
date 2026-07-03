# tests/testthat/test-portability.R
# Offline tests for metrics_portability(). Each test builds a fixture ctx via
# build_context() with an in-memory file map, following the same pattern as
# test-structure.R.

# ---------------------------------------------------------------------------
# system_requirements_count
# ---------------------------------------------------------------------------

test_that("system_requirements_count: counts distinct libs tokenized on comma and 'and'", {
  map <- list(
    "DESCRIPTION" = paste0(
      "Package: mypkg\nVersion: 1.0\n",
      "SystemRequirements: libgit2, OpenSSL (>= 1.0.0) and libssh2\n"
    )
  )
  ctx <- build_context("mypkg", "1.0", "1.0", "2024-01-01",
                       names(map), function(p) map[[p]] %||% "")
  m <- metrics_portability(ctx)

  expect_equal(m$system_requirements_count, 3L)
})

test_that("system_requirements_count: returns NA when field is absent", {
  map <- list("DESCRIPTION" = "Package: p\nVersion: 1.0\n")
  ctx <- build_context("p", "1.0", "1.0", "2024-01-01",
                       names(map), function(p) map[[p]] %||% "")
  m <- metrics_portability(ctx)

  expect_true(is.na(m$system_requirements_count))
})

test_that("system_requirements_count: returns NA when field is empty string", {
  map <- list("DESCRIPTION" = "Package: p\nVersion: 1.0\nSystemRequirements: \n")
  ctx <- build_context("p", "1.0", "1.0", "2024-01-01",
                       names(map), function(p) map[[p]] %||% "")
  m <- metrics_portability(ctx)

  expect_true(is.na(m$system_requirements_count))
})

test_that("system_requirements_count: single lib gives count 1", {
  map <- list("DESCRIPTION" = "Package: p\nVersion: 1.0\nSystemRequirements: zlib\n")
  ctx <- build_context("p", "1.0", "1.0", "2024-01-01",
                       names(map), function(p) map[[p]] %||% "")
  m <- metrics_portability(ctx)

  expect_equal(m$system_requirements_count, 1L)
})

# ---------------------------------------------------------------------------
# cxx_standard_required
# ---------------------------------------------------------------------------

test_that("cxx_standard_required: detects CXX17 from src/Makevars", {
  map <- list(
    "DESCRIPTION"  = "Package: p\nVersion: 1.0\n",
    "src/Makevars" = "PKG_CXXFLAGS = $(SHLIB_OPENMP_CXXFLAGS)\nCXX_STD = CXX17\n"
  )
  ctx <- build_context("p", "1.0", "1.0", "2024-01-01",
                       names(map), function(p) map[[p]] %||% "")
  m <- metrics_portability(ctx)

  expect_equal(m$cxx_standard_required, "C++17")
})

test_that("cxx_standard_required: returns NA when no Makevars file present", {
  map <- list("DESCRIPTION" = "Package: p\nVersion: 1.0\n")
  ctx <- build_context("p", "1.0", "1.0", "2024-01-01",
                       names(map), function(p) map[[p]] %||% "")
  m <- metrics_portability(ctx)

  expect_true(is.na(m$cxx_standard_required))
})

test_that("cxx_standard_required: detects CXX11 from src/Makevars.win when no src/Makevars", {
  map <- list(
    "DESCRIPTION"      = "Package: p\nVersion: 1.0\n",
    "src/Makevars.win" = "CXX_STD = CXX11\n"
  )
  ctx <- build_context("p", "1.0", "1.0", "2024-01-01",
                       names(map), function(p) map[[p]] %||% "")
  m <- metrics_portability(ctx)

  expect_equal(m$cxx_standard_required, "C++11")
})

test_that("cxx_standard_required: commented-out CXX_STD is ignored", {
  map <- list(
    "DESCRIPTION"  = "Package: p\nVersion: 1.0\n",
    "src/Makevars" = "# CXX_STD = CXX17\nPKG_CXXFLAGS = -std=c++14\n"
  )
  ctx <- build_context("p", "1.0", "1.0", "2024-01-01",
                       names(map), function(p) map[[p]] %||% "")
  m <- metrics_portability(ctx)

  expect_true(is.na(m$cxx_standard_required))
})

# ---------------------------------------------------------------------------
# nonportable_compiler_flags
# ---------------------------------------------------------------------------

test_that("nonportable_compiler_flags: detects -march=native and -O3", {
  map <- list(
    "DESCRIPTION"  = "Package: p\nVersion: 1.0\n",
    "src/Makevars" = "PKG_CXXFLAGS = -march=native -O3 -Wall\n"
  )
  ctx <- build_context("p", "1.0", "1.0", "2024-01-01",
                       names(map), function(p) map[[p]] %||% "")
  m <- metrics_portability(ctx)

  expect_equal(m$nonportable_compiler_flags, 2L)
  flags <- jsonlite::fromJSON(m$nonportable_compiler_flags_json)
  expect_true("-march=native" %in% flags)
  expect_true("-O3" %in% flags)
})

test_that("nonportable_compiler_flags: zero when no Makevars files", {
  map <- list("DESCRIPTION" = "Package: p\nVersion: 1.0\n")
  ctx <- build_context("p", "1.0", "1.0", "2024-01-01",
                       names(map), function(p) map[[p]] %||% "")
  m <- metrics_portability(ctx)

  expect_equal(m$nonportable_compiler_flags, 0L)
  expect_equal(m$nonportable_compiler_flags_json, "[]")
})

test_that("nonportable_compiler_flags: comment lines are not scanned", {
  map <- list(
    "DESCRIPTION"  = "Package: p\nVersion: 1.0\n",
    "src/Makevars" = "# PKG_CXXFLAGS = -O3 -march=native\nPKG_CXXFLAGS = -Wall\n"
  )
  ctx <- build_context("p", "1.0", "1.0", "2024-01-01",
                       names(map), function(p) map[[p]] %||% "")
  m <- metrics_portability(ctx)

  expect_equal(m$nonportable_compiler_flags, 0L)
})

test_that("nonportable_compiler_flags: detects absolute -I and -L paths", {
  map <- list(
    "DESCRIPTION"  = "Package: p\nVersion: 1.0\n",
    "src/Makevars" = "PKG_CPPFLAGS = -I/usr/local/include\nPKG_LIBS = -L/usr/local/lib -lfoo\n"
  )
  ctx <- build_context("p", "1.0", "1.0", "2024-01-01",
                       names(map), function(p) map[[p]] %||% "")
  m <- metrics_portability(ctx)

  expect_true(m$nonportable_compiler_flags >= 2L)
  flags <- jsonlite::fromJSON(m$nonportable_compiler_flags_json)
  expect_true(any(grepl("^-I/", flags)))
  expect_true(any(grepl("^-L/", flags)))
})

test_that("nonportable_compiler_flags: same flag in two Makevars files counted once", {
  map <- list(
    "DESCRIPTION"      = "Package: p\nVersion: 1.0\n",
    "src/Makevars"     = "PKG_CXXFLAGS = -O3\n",
    "src/Makevars.win" = "PKG_CXXFLAGS = -O3\n"
  )
  ctx <- build_context("p", "1.0", "1.0", "2024-01-01",
                       names(map), function(p) map[[p]] %||% "")
  m <- metrics_portability(ctx)

  expect_equal(m$nonportable_compiler_flags, 1L)
})

test_that("nonportable_compiler_flags: variable-based include paths not flagged", {
  map <- list(
    "DESCRIPTION"  = "Package: p\nVersion: 1.0\n",
    "src/Makevars" = "PKG_CPPFLAGS = -I$(R_INCLUDE_DIR) -I$(MY_LIB)/include\n"
  )
  ctx <- build_context("p", "1.0", "1.0", "2024-01-01",
                       names(map), function(p) map[[p]] %||% "")
  m <- metrics_portability(ctx)

  expect_equal(m$nonportable_compiler_flags, 0L)
})

# ---------------------------------------------------------------------------
# min_r_version
# ---------------------------------------------------------------------------

test_that("min_r_version: parses R (>= 3.5.0) from Depends", {
  map <- list(
    "DESCRIPTION" = "Package: p\nVersion: 1.0\nDepends: R (>= 3.5.0), methods\n"
  )
  ctx <- build_context("p", "1.0", "1.0", "2024-01-01",
                       names(map), function(p) map[[p]] %||% "")
  m <- metrics_portability(ctx)

  expect_equal(m$min_r_version, "3.5.0")
})

test_that("min_r_version: returns NA when Depends field is absent", {
  map <- list("DESCRIPTION" = "Package: p\nVersion: 1.0\n")
  ctx <- build_context("p", "1.0", "1.0", "2024-01-01",
                       names(map), function(p) map[[p]] %||% "")
  m <- metrics_portability(ctx)

  expect_true(is.na(m$min_r_version))
})

test_that("min_r_version: returns NA when Depends has other packages but no R", {
  map <- list(
    "DESCRIPTION" = "Package: p\nVersion: 1.0\nDepends: methods, utils\n"
  )
  ctx <- build_context("p", "1.0", "1.0", "2024-01-01",
                       names(map), function(p) map[[p]] %||% "")
  m <- metrics_portability(ctx)

  expect_true(is.na(m$min_r_version))
})

test_that("min_r_version: parses two-component version R (>= 4.1)", {
  map <- list(
    "DESCRIPTION" = "Package: p\nVersion: 1.0\nDepends: R (>= 4.1)\n"
  )
  ctx <- build_context("p", "1.0", "1.0", "2024-01-01",
                       names(map), function(p) map[[p]] %||% "")
  m <- metrics_portability(ctx)

  expect_equal(m$min_r_version, "4.1")
})

# ---------------------------------------------------------------------------
# has_vignettes
# ---------------------------------------------------------------------------

test_that("has_vignettes: TRUE when vignettes/ contains an .Rmd file", {
  map <- list(
    "DESCRIPTION"         = "Package: p\nVersion: 1.0\n",
    "vignettes/intro.Rmd" = "# Introduction\n\n```{r setup}\nlibrary(p)\n```\n"
  )
  ctx <- build_context("p", "1.0", "1.0", "2024-01-01",
                       names(map), function(p) map[[p]] %||% "")
  m <- metrics_portability(ctx)

  expect_true(m$has_vignettes)
})

test_that("has_vignettes: FALSE when no vignette files", {
  map <- list("DESCRIPTION" = "Package: p\nVersion: 1.0\n", "R/foo.R" = "foo <- 1\n")
  ctx <- build_context("p", "1.0", "1.0", "2024-01-01",
                       names(map), function(p) map[[p]] %||% "")
  m <- metrics_portability(ctx)

  expect_false(m$has_vignettes)
})

test_that("has_vignettes: FALSE when vignettes/ dir has only non-Rmd/Rnw files", {
  map <- list(
    "DESCRIPTION"           = "Package: p\nVersion: 1.0\n",
    "vignettes/data.csv"    = "a,b\n1,2\n",
    "vignettes/figure.png"  = "(binary)"
  )
  ctx <- build_context("p", "1.0", "1.0", "2024-01-01",
                       names(map), function(p) map[[p]] %||% "")
  m <- metrics_portability(ctx)

  expect_false(m$has_vignettes)
})

test_that("has_vignettes: TRUE when vignettes/ contains an .Rnw file", {
  map <- list(
    "DESCRIPTION"         = "Package: p\nVersion: 1.0\n",
    "vignettes/guide.Rnw" = "\\documentclass{article}\n<<setup>>=\nlibrary(p)\n@\n"
  )
  ctx <- build_context("p", "1.0", "1.0", "2024-01-01",
                       names(map), function(p) map[[p]] %||% "")
  m <- metrics_portability(ctx)

  expect_true(m$has_vignettes)
})

# ---------------------------------------------------------------------------
# vignette_dynamic
# ---------------------------------------------------------------------------

test_that("vignette_dynamic: NA when no vignettes", {
  map <- list("DESCRIPTION" = "Package: p\nVersion: 1.0\n")
  ctx <- build_context("p", "1.0", "1.0", "2024-01-01",
                       names(map), function(p) map[[p]] %||% "")
  m <- metrics_portability(ctx)

  expect_true(is.na(m$vignette_dynamic))
})

test_that("vignette_dynamic: TRUE when Rmd chunk lacks eval=FALSE", {
  rmd <- paste0(
    "---\ntitle: My Vignette\n---\n\n",
    "```{r setup, include=FALSE}\nlibrary(p)\n```\n\n",
    "```{r example}\np_fn()\n```\n"
  )
  map <- list(
    "DESCRIPTION"         = "Package: p\nVersion: 1.0\n",
    "vignettes/intro.Rmd" = rmd
  )
  ctx <- build_context("p", "1.0", "1.0", "2024-01-01",
                       names(map), function(p) map[[p]] %||% "")
  m <- metrics_portability(ctx)

  expect_true(isTRUE(m$vignette_dynamic))
})

test_that("vignette_dynamic: FALSE when all Rmd chunks have eval=FALSE", {
  rmd <- paste0(
    "---\ntitle: Static\n---\n\n",
    "```{r ex1, eval=FALSE}\nsome_code()\n```\n\n",
    "```{r ex2, eval=FALSE}\nmore_code()\n```\n"
  )
  map <- list(
    "DESCRIPTION"          = "Package: p\nVersion: 1.0\n",
    "vignettes/static.Rmd" = rmd
  )
  ctx <- build_context("p", "1.0", "1.0", "2024-01-01",
                       names(map), function(p) map[[p]] %||% "")
  m <- metrics_portability(ctx)

  expect_false(isTRUE(m$vignette_dynamic))
  expect_false(is.na(m$vignette_dynamic))
})

test_that("vignette_dynamic: TRUE when Rnw chunk lacks eval=FALSE", {
  rnw <- paste0(
    "\\documentclass{article}\n",
    "<<setup, echo=FALSE>>=\nlibrary(p)\n@\n",
    "<<example>>=\np_fn()\n@\n"
  )
  map <- list(
    "DESCRIPTION"         = "Package: p\nVersion: 1.0\n",
    "vignettes/guide.Rnw" = rnw
  )
  ctx <- build_context("p", "1.0", "1.0", "2024-01-01",
                       names(map), function(p) map[[p]] %||% "")
  m <- metrics_portability(ctx)

  expect_true(isTRUE(m$vignette_dynamic))
})

test_that("vignette_dynamic: TRUE when Rmd file has no code chunks at all", {
  rmd <- "# My Vignette\n\nThis vignette contains only text.\n"
  map <- list(
    "DESCRIPTION"          = "Package: p\nVersion: 1.0\n",
    "vignettes/readme.Rmd" = rmd
  )
  ctx <- build_context("p", "1.0", "1.0", "2024-01-01",
                       names(map), function(p) map[[p]] %||% "")
  m <- metrics_portability(ctx)

  expect_true(isTRUE(m$vignette_dynamic))
})

# ---------------------------------------------------------------------------
# Full fixture: all metrics together
# ---------------------------------------------------------------------------

test_that("metrics_portability: full fixture with compiled package", {
  desc <- paste0(
    "Package: mypkg\nVersion: 2.1.0\n",
    "Depends: R (>= 4.0.0), methods\n",
    "SystemRequirements: libcurl, OpenSSL and zlib\n"
  )
  makevars <- paste0(
    "CXX_STD = CXX17\n",
    "PKG_CXXFLAGS = -march=native -O3\n",
    "PKG_CPPFLAGS = -I$(R_INCLUDE_DIR)\n"
  )
  rmd <- paste0(
    "---\ntitle: Intro\n---\n\n",
    "```{r setup}\nlibrary(mypkg)\n```\n"
  )
  map <- list(
    "DESCRIPTION"         = desc,
    "NAMESPACE"           = "export(foo)\n",
    "R/foo.R"             = "foo <- function() 42\n",
    "src/Makevars"        = makevars,
    "vignettes/intro.Rmd" = rmd
  )
  ctx <- build_context("mypkg", "2.1.0", "2.1.0", "2024-06-01",
                       names(map), function(p) map[[p]] %||% "")
  m <- metrics_portability(ctx)

  expect_equal(m$system_requirements_count, 3L)
  expect_equal(m$cxx_standard_required, "C++17")
  expect_equal(m$nonportable_compiler_flags, 2L)
  flags <- jsonlite::fromJSON(m$nonportable_compiler_flags_json)
  expect_true("-march=native" %in% flags)
  expect_true("-O3" %in% flags)
  expect_equal(m$min_r_version, "4.0.0")
  expect_true(m$has_vignettes)
  expect_true(isTRUE(m$vignette_dynamic))
})

test_that("metrics_portability: empty file list returns NA-safe defaults", {
  ctx <- build_context("p", "0.1", "0.1", "2024-01-01",
                       character(0L), function(p) "")
  m <- metrics_portability(ctx)

  expect_true(is.na(m$system_requirements_count))
  expect_true(is.na(m$cxx_standard_required))
  expect_equal(m$nonportable_compiler_flags, 0L)
  expect_equal(m$nonportable_compiler_flags_json, "[]")
  expect_true(is.na(m$min_r_version))
  expect_false(m$has_vignettes)
  expect_true(is.na(m$vignette_dynamic))
})
