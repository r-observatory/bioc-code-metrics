# tests/testthat/test-security.R
# Fixture helper: build an in-memory context from a named list of path -> content.
make_ctx <- function(map, package = "pkg", version = "1.0") {
  build_context(
    package, version, version, "2024-01-01",
    names(map),
    function(p) map[[p]] %||% ""
  )
}

# ==============================================================================
# 1. unsafe_pattern_score
# ==============================================================================

test_that("unsafe_pattern_score: eval(parse(text=)) scores 3 per occurrence", {
  map <- list(
    "DESCRIPTION" = "Package: pkg\nVersion: 1.0\n",
    "NAMESPACE"   = "export(foo)\n",
    "R/foo.R"     = "foo <- function(x) eval(parse(text = x))\n"
  )
  m <- metrics_security(make_ctx(map))
  expect_equal(m$unsafe_pattern_score, 3L)
})

test_that("unsafe_pattern_score: no R/ files returns 0", {
  map <- list(
    "DESCRIPTION" = "Package: pkg\nVersion: 1.0\n",
    "NAMESPACE"   = "export(foo)\n"
  )
  m <- metrics_security(make_ctx(map))
  expect_equal(m$unsafe_pattern_score, 0L)
})

test_that("unsafe_pattern_score: system(paste()) and system2+paste scored correctly", {
  map <- list(
    "DESCRIPTION" = "Package: pkg\nVersion: 1.0\n",
    "NAMESPACE"   = "export(run)\n",
    "R/utils.R"   = paste0(
      "run <- function(x) {\n",
      "  system(paste('ls', x))\n",        # +2
      "  system2('cmd', paste('--f', x))\n",  # +2
      "}\n"
    )
  )
  m <- metrics_security(make_ctx(map))
  # system(paste( = 2, system2 line with paste( = 2 => total 4
  expect_equal(m$unsafe_pattern_score, 4L)
})

# ==============================================================================
# 2. install_time_side_effect_surface
# ==============================================================================

test_that("install_time_side_effect_surface: configure + onLoad network => JSON flags set", {
  map <- list(
    "DESCRIPTION" = "Package: pkg\nVersion: 1.0\n",
    "NAMESPACE"   = "export(foo)\n",
    "configure"   = "#!/bin/sh\n./configure --enable-shared\n",
    "R/zzz.R"     = ".onLoad <- function(lib, pkg) {\n  download.file('http://x', 'y')\n}\n"
  )
  m  <- metrics_security(make_ctx(map))
  js <- jsonlite::fromJSON(m$install_time_side_effect_surface)
  expect_true("configure" %in% js$configure_files)
  expect_true(js$configure_loc > 0L)
  expect_true(js$onLoad_network)
  expect_false(js$onLoad_file_write)
})

test_that("install_time_side_effect_surface: no configure or hooks => safe JSON", {
  map <- list(
    "DESCRIPTION" = "Package: pkg\nVersion: 1.0\n",
    "NAMESPACE"   = "export(foo)\n",
    "R/foo.R"     = "foo <- function() 1\n"
  )
  m  <- metrics_security(make_ctx(map))
  js <- jsonlite::fromJSON(m$install_time_side_effect_surface)
  expect_equal(js$configure_loc, 0L)
  expect_false(js$onLoad_file_write)
  expect_false(js$onLoad_network)
})

test_that("install_time_side_effect_surface: onLoad with writeLines sets file_write flag", {
  map <- list(
    "DESCRIPTION" = "Package: pkg\nVersion: 1.0\n",
    "NAMESPACE"   = "export(foo)\n",
    "R/zzz.R"     = ".onLoad <- function(lib, pkg) {\n  writeLines('x', con = 'out.txt')\n}\n"
  )
  m  <- metrics_security(make_ctx(map))
  js <- jsonlite::fromJSON(m$install_time_side_effect_surface)
  expect_true(js$onLoad_file_write)
  expect_false(js$onLoad_network)
})

# ==============================================================================
# 3. dep_constraint_coverage
# ==============================================================================

test_that("dep_constraint_coverage: 2 of 3 constrained imports => 2/3", {
  map <- list(
    "DESCRIPTION" = paste0(
      "Package: pkg\nVersion: 1.0\n",
      "Imports: pkgA (>= 1.0), pkgB, pkgC (>= 2.0)\n"
    ),
    "NAMESPACE" = "export(foo)\n"
  )
  m <- metrics_security(make_ctx(map))
  expect_equal(m$dep_constraint_coverage, 2 / 3, tolerance = 1e-9)
})

test_that("dep_constraint_coverage: no Imports or Depends returns NA", {
  map <- list(
    "DESCRIPTION" = "Package: pkg\nVersion: 1.0\n",
    "NAMESPACE"   = "export(foo)\n"
  )
  m <- metrics_security(make_ctx(map))
  expect_true(is.na(m$dep_constraint_coverage))
})

test_that("dep_constraint_coverage: R excluded from Depends; unconstrained import => 0", {
  map <- list(
    "DESCRIPTION" = paste0(
      "Package: pkg\nVersion: 1.0\n",
      "Depends: R (>= 4.0)\n",
      "Imports: pkgA\n"
    ),
    "NAMESPACE" = "export(foo)\n"
  )
  m <- metrics_security(make_ctx(map))
  # pkgA has no >=, R is excluded => 0/1 = 0
  expect_equal(m$dep_constraint_coverage, 0, tolerance = 1e-9)
})

# ==============================================================================
# 4. non_registry_remotes
# ==============================================================================

test_that("non_registry_remotes: github:: and gitlab:: entries counted with schemes", {
  map <- list(
    "DESCRIPTION" = paste0(
      "Package: pkg\nVersion: 1.0\n",
      "Remotes: github::user/repo, gitlab::user/pkg2\n"
    ),
    "NAMESPACE" = "export(foo)\n"
  )
  m  <- metrics_security(make_ctx(map))
  js <- jsonlite::fromJSON(m$non_registry_remotes)
  expect_equal(js$count, 2L)
  expect_true("github" %in% js$schemes)
  expect_true("gitlab" %in% js$schemes)
})

test_that("non_registry_remotes: no Remotes field returns count 0", {
  map <- list(
    "DESCRIPTION" = "Package: pkg\nVersion: 1.0\n",
    "NAMESPACE"   = "export(foo)\n"
  )
  m  <- metrics_security(make_ctx(map))
  js <- jsonlite::fromJSON(m$non_registry_remotes)
  expect_equal(js$count, 0L)
})

test_that("non_registry_remotes: bare user/repo without :: inferred as github", {
  map <- list(
    "DESCRIPTION" = paste0(
      "Package: pkg\nVersion: 1.0\n",
      "Remotes: user/somepkg\n"
    ),
    "NAMESPACE" = "export(foo)\n"
  )
  m  <- metrics_security(make_ctx(map))
  js <- jsonlite::fromJSON(m$non_registry_remotes)
  expect_equal(js$count, 1L)
  expect_true("github" %in% js$schemes)
})

# ==============================================================================
# 5. secret_pattern_count
# ==============================================================================

test_that("secret_pattern_count: AWS AKIA key in R file detected", {
  map <- list(
    "DESCRIPTION" = "Package: pkg\nVersion: 1.0\n",
    "NAMESPACE"   = "export(foo)\n",
    "R/config.R"  = "KEY <- \"AKIAIOSFODNN7EXAMPLE\"\n"
  )
  m <- metrics_security(make_ctx(map))
  expect_true(m$secret_pattern_count >= 1L)
})

test_that("secret_pattern_count: clean package returns 0", {
  map <- list(
    "DESCRIPTION" = "Package: pkg\nVersion: 1.0\n",
    "NAMESPACE"   = "export(foo)\n",
    "R/foo.R"     = "foo <- function() 42\n"
  )
  m <- metrics_security(make_ctx(map))
  expect_equal(m$secret_pattern_count, 0L)
})

test_that("secret_pattern_count: GitHub personal access token detected", {
  map <- list(
    "DESCRIPTION" = "Package: pkg\nVersion: 1.0\n",
    "NAMESPACE"   = "export(foo)\n",
    "R/auth.R"    = "token <- \"ghp_ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890\"\n"
  )
  m <- metrics_security(make_ctx(map))
  expect_true(m$secret_pattern_count >= 1L)
})

# ==============================================================================
# 6. compiled_external_lib_exposure
# ==============================================================================

test_that("compiled_external_lib_exposure: -l flags in Makevars extracted", {
  map <- list(
    "DESCRIPTION"  = "Package: pkg\nVersion: 1.0\n",
    "NAMESPACE"    = "export(foo)\n",
    "src/Makevars" = "PKG_LIBS = -lssl -lcrypto\n"
  )
  m    <- metrics_security(make_ctx(map))
  libs <- jsonlite::fromJSON(m$compiled_external_lib_exposure)
  expect_true("ssl" %in% libs)
  expect_true("crypto" %in% libs)
})

test_that("compiled_external_lib_exposure: no src config files returns empty array", {
  map <- list(
    "DESCRIPTION" = "Package: pkg\nVersion: 1.0\n",
    "NAMESPACE"   = "export(foo)\n"
  )
  m    <- metrics_security(make_ctx(map))
  libs <- jsonlite::fromJSON(m$compiled_external_lib_exposure)
  expect_equal(length(libs), 0L)
})

test_that("compiled_external_lib_exposure: AC_CHECK_LIB in configure.ac extracted", {
  map <- list(
    "DESCRIPTION"  = "Package: pkg\nVersion: 1.0\n",
    "NAMESPACE"    = "export(foo)\n",
    "configure.ac" = "AC_CHECK_LIB(curl, curl_easy_init)\nAC_CHECK_LIB(z, deflate)\n"
  )
  m    <- metrics_security(make_ctx(map))
  libs <- jsonlite::fromJSON(m$compiled_external_lib_exposure)
  expect_true("curl" %in% libs)
  expect_true("z" %in% libs)
})

# ==============================================================================
# 7. bundled_third_party_code
# ==============================================================================

test_that("bundled_third_party_code: sqlite3.c under src/ detected", {
  map <- list(
    "DESCRIPTION"   = "Package: pkg\nVersion: 1.0\n",
    "NAMESPACE"     = "export(foo)\n",
    "src/sqlite3.c" = "/* SQLite amalgamation */\nvoid sqlite3_open(void) {}\n"
  )
  m  <- metrics_security(make_ctx(map))
  js <- jsonlite::fromJSON(m$bundled_third_party_code)
  expect_true(js$detected)
  expect_true("src/sqlite3.c" %in% js$files)
})

test_that("bundled_third_party_code: no vendored files returns detected = false", {
  map <- list(
    "DESCRIPTION" = "Package: pkg\nVersion: 1.0\n",
    "NAMESPACE"   = "export(foo)\n",
    "src/pkg.c"   = "void hello(void) {}\n"
  )
  m  <- metrics_security(make_ctx(map))
  js <- jsonlite::fromJSON(m$bundled_third_party_code)
  expect_false(js$detected)
  expect_equal(length(js$files), 0L)
})

test_that("bundled_third_party_code: LICENSE in src/ subdirectory detected", {
  map <- list(
    "DESCRIPTION"        = "Package: pkg\nVersion: 1.0\n",
    "NAMESPACE"          = "export(foo)\n",
    "src/vendor/mylib.c" = "// vendored code\n",
    "src/vendor/LICENSE" = "MIT License\nCopyright (c) 2020 Author\n"
  )
  m  <- metrics_security(make_ctx(map))
  js <- jsonlite::fromJSON(m$bundled_third_party_code)
  expect_true(js$detected)
  expect_true("src/vendor/LICENSE" %in% js$files)
})
