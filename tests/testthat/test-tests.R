# tests/testthat/test-tests.R

# Helper: build a context from an in-memory file map.
make_ctx <- function(file_map, package = "mypkg", version = "1.0",
                     prev_exports = NULL) {
  build_context(
    package      = package,
    version      = version,
    ref          = version,
    date         = "2024-01-01",
    files        = names(file_map),
    read_fn      = function(p) file_map[[p]] %||% "",
    prev_exports = prev_exports
  )
}

# ===========================================================================
# has_tests
# ===========================================================================

test_that("has_tests is TRUE when tests/ files are present", {
  ctx <- make_ctx(list(
    "DESCRIPTION"               = "Package: p\nVersion: 1.0\n",
    "R/foo.R"                   = "foo <- function() 1\n",
    "tests/testthat/test-foo.R" = "test_that('ok', expect_true(TRUE))\n"
  ))
  m <- metrics_tests(ctx)
  expect_true(m$has_tests)
})

test_that("has_tests is FALSE when no tests/ files are present", {
  ctx <- make_ctx(list(
    "DESCRIPTION" = "Package: p\nVersion: 1.0\n",
    "R/foo.R"     = "foo <- function() 1\n"
  ))
  m <- metrics_tests(ctx)
  expect_false(m$has_tests)
})

test_that("has_tests is FALSE on empty context", {
  ctx <- build_context("p", "0.1", "0.1", "2024-01-01",
                       character(0L), function(p) "")
  m <- metrics_tests(ctx)
  expect_false(m$has_tests)
})

# ===========================================================================
# test_to_code_ratio
# ===========================================================================

test_that("test_to_code_ratio is LOC(tests) / LOC(R)", {
  # R/foo.R has 3 non-trailing lines; test file has 6.
  ctx <- make_ctx(list(
    "R/foo.R"                   = "a\nb\nc\n",
    "tests/testthat/test-foo.R" = "x\ny\nz\nw\nv\nu\n"
  ))
  m <- metrics_tests(ctx)
  expect_equal(m$test_to_code_ratio, 6 / 3)
})

test_that("test_to_code_ratio is NA when no R/ files exist", {
  ctx <- make_ctx(list(
    "tests/testthat/test-foo.R" = "test_that('ok', TRUE)\n"
  ))
  m <- metrics_tests(ctx)
  expect_true(is.na(m$test_to_code_ratio))
})

test_that("test_to_code_ratio is NA when R/ has 0 LOC (all files empty)", {
  ctx <- make_ctx(list(
    "R/empty.R"                 = "",
    "tests/testthat/test-foo.R" = "test_that('ok', TRUE)\n"
  ))
  m <- metrics_tests(ctx)
  expect_true(is.na(m$test_to_code_ratio))
})

# ===========================================================================
# testthat_edition
# ===========================================================================

test_that("testthat_edition is parsed as integer from DESCRIPTION", {
  ctx <- make_ctx(list(
    "DESCRIPTION" = "Package: p\nVersion: 1.0\nConfig/testthat/edition: 3\n"
  ))
  m <- metrics_tests(ctx)
  expect_equal(m$testthat_edition, 3L)
  expect_type(m$testthat_edition, "integer")
})

test_that("testthat_edition is NA_integer_ when field is absent", {
  ctx <- make_ctx(list(
    "DESCRIPTION" = "Package: p\nVersion: 1.0\n"
  ))
  m <- metrics_tests(ctx)
  expect_true(is.na(m$testthat_edition))
  expect_type(m$testthat_edition, "integer")
})

test_that("testthat_edition is NA_integer_ when value is non-numeric", {
  ctx <- make_ctx(list(
    "DESCRIPTION" = "Package: p\nVersion: 1.0\nConfig/testthat/edition: abc\n"
  ))
  m <- metrics_tests(ctx)
  expect_true(is.na(m$testthat_edition))
})

# ===========================================================================
# snapshot_test_count
# ===========================================================================

test_that("snapshot_test_count counts expect_snapshot calls and snap files", {
  ctx <- make_ctx(list(
    "tests/testthat/test-snap.R" = paste0(
      "test_that('snaps', {\n",
      "  expect_snapshot(foo(1))\n",
      "  expect_snapshot(foo(2))\n",
      "  expect_snapshot_file(bar(), 'bar.txt')\n",
      "})\n"
    ),
    "tests/testthat/_snaps/foo.md" = "# foo\nsome snap output\n"
  ))
  m <- metrics_tests(ctx)
  # 2 expect_snapshot + 1 expect_snapshot_file + 1 _snaps/ file = 4
  expect_equal(m$snapshot_test_count, 4L)
})

test_that("snapshot_test_count is 0 when no snapshots present", {
  ctx <- make_ctx(list(
    "tests/testthat/test-foo.R" = "test_that('ok', expect_true(TRUE))\n"
  ))
  m <- metrics_tests(ctx)
  expect_equal(m$snapshot_test_count, 0L)
})

test_that("expect_snapshot_file is not double-counted as expect_snapshot", {
  # Only one expect_snapshot_file call; expect_snapshot\s*\( does NOT match it
  # because the char after 'expect_snapshot' is '_', not whitespace or '('.
  ctx <- make_ctx(list(
    "tests/testthat/test-snap.R" = "expect_snapshot_file(f(), 'x.txt')\n"
  ))
  m <- metrics_tests(ctx)
  expect_equal(m$snapshot_test_count, 1L)
})

test_that("snapshot_test_count counts _snaps/ files even with no calls", {
  ctx <- make_ctx(list(
    "tests/testthat/test-foo.R"          = "test_that('ok', TRUE)\n",
    "tests/testthat/_snaps/foo.md"       = "# foo\n",
    "tests/testthat/_snaps/bar.md"       = "# bar\n"
  ))
  m <- metrics_tests(ctx)
  expect_equal(m$snapshot_test_count, 2L)
})

# ===========================================================================
# test_isolation_libs
# ===========================================================================

test_that("test_isolation_libs detects withr:: usage and local_mocked_bindings", {
  ctx <- make_ctx(list(
    "tests/testthat/test-iso.R" = paste0(
      "withr::with_seed(42, {\n",
      "  local_mocked_bindings(fetch = function(...) list())\n",
      "})\n"
    )
  ))
  m <- metrics_tests(ctx)
  libs <- jsonlite::fromJSON(m$test_isolation_libs)
  expect_true("withr" %in% libs)
  expect_true("local_mocked_bindings" %in% libs)
})

test_that("test_isolation_libs returns '[]' when no isolation libs used", {
  ctx <- make_ctx(list(
    "tests/testthat/test-plain.R" = "test_that('ok', expect_true(TRUE))\n"
  ))
  m <- metrics_tests(ctx)
  expect_equal(m$test_isolation_libs, "[]")
})

test_that("test_isolation_libs detects mockr via double-colon", {
  ctx <- make_ctx(list(
    "tests/testthat/test-mockr.R" =
      "mockr::with_mock(foo = function() 1, { bar() })\n"
  ))
  m <- metrics_tests(ctx)
  libs <- jsonlite::fromJSON(m$test_isolation_libs)
  expect_true("mockr" %in% libs)
  expect_false("withr" %in% libs)
})

test_that("test_isolation_libs detects withr via library() call", {
  ctx <- make_ctx(list(
    "tests/testthat/helper.R" = "library(withr)\n"
  ))
  m <- metrics_tests(ctx)
  libs <- jsonlite::fromJSON(m$test_isolation_libs)
  expect_true("withr" %in% libs)
})

# ===========================================================================
# exported_fn_test_linkage
# ===========================================================================

test_that("exported_fn_test_linkage is fraction of exports appearing in tests", {
  ctx <- make_ctx(list(
    "NAMESPACE"                 = "export(foo)\nexport(bar)\n",
    "tests/testthat/test-foo.R" =
      "test_that('foo works', { expect_equal(foo(1), 2) })\n"
  ))
  m <- metrics_tests(ctx)
  # foo appears, bar does not -> 1/2 = 0.5
  expect_equal(m$exported_fn_test_linkage, 0.5)
})

test_that("exported_fn_test_linkage is NA when NAMESPACE has no concrete exports", {
  ctx <- make_ctx(list(
    "NAMESPACE"                 = "importFrom(base, c)\n",
    "tests/testthat/test-foo.R" = "test_that('ok', TRUE)\n"
  ))
  m <- metrics_tests(ctx)
  expect_true(is.na(m$exported_fn_test_linkage))
})

test_that("exported_fn_test_linkage is 1.0 when all exports appear in tests", {
  ctx <- make_ctx(list(
    "NAMESPACE"                  = "export(foo)\nexport(bar)\n",
    "tests/testthat/test-all.R"  =
      "test_that('both', { foo(); bar() })\n"
  ))
  m <- metrics_tests(ctx)
  expect_equal(m$exported_fn_test_linkage, 1.0)
})

test_that("exported_fn_test_linkage excludes exportPattern entries from denominator", {
  ctx <- make_ctx(list(
    "NAMESPACE"                 = "exportPattern('^[A-Z]')\n",
    "tests/testthat/test-foo.R" = "test_that('ok', TRUE)\n"
  ))
  m <- metrics_tests(ctx)
  # exportPattern -> no concrete exports -> NA
  expect_true(is.na(m$exported_fn_test_linkage))
})

test_that("exported_fn_test_linkage is 0 when exports exist but no test files", {
  ctx <- make_ctx(list(
    "NAMESPACE" = "export(foo)\nexport(bar)\n"
  ))
  m <- metrics_tests(ctx)
  expect_equal(m$exported_fn_test_linkage, 0)
})

# ===========================================================================
# stochastic_seed_discipline
# ===========================================================================

test_that("stochastic_seed_discipline is 1.0 when all RNG test files use set.seed", {
  ctx <- make_ctx(list(
    "tests/testthat/test-rng.R" = paste0(
      "set.seed(42)\n",
      "test_that('stochastic', { x <- rnorm(10); expect_length(x, 10) })\n"
    )
  ))
  m <- metrics_tests(ctx)
  expect_equal(m$stochastic_seed_discipline, 1.0)
})

test_that("stochastic_seed_discipline is 0.0 when RNG test files lack set.seed", {
  ctx <- make_ctx(list(
    "tests/testthat/test-rng.R" =
      "test_that('rand', { x <- runif(10); expect_length(x, 10) })\n"
  ))
  m <- metrics_tests(ctx)
  expect_equal(m$stochastic_seed_discipline, 0.0)
})

test_that("stochastic_seed_discipline is NA when no test files use RNG functions", {
  ctx <- make_ctx(list(
    "tests/testthat/test-foo.R" = "test_that('ok', expect_true(TRUE))\n"
  ))
  m <- metrics_tests(ctx)
  expect_true(is.na(m$stochastic_seed_discipline))
})

test_that("stochastic_seed_discipline is fractional when only some RNG files seeded", {
  ctx <- make_ctx(list(
    "tests/testthat/test-a.R" = "set.seed(1)\ntest_that('a', { x <- sample(10) })\n",
    "tests/testthat/test-b.R" = "test_that('b', { x <- rbinom(5, 1, 0.5) })\n"
  ))
  m <- metrics_tests(ctx)
  # test-a.R: has RNG + set.seed; test-b.R: has RNG, no set.seed -> 1/2
  expect_equal(m$stochastic_seed_discipline, 0.5)
})

# ===========================================================================
# ci_present
# ===========================================================================

test_that("ci_present is TRUE when a GitHub Actions .yml file is present", {
  ctx <- make_ctx(list(
    ".github/workflows/R-CMD-check.yml" =
      "on: push\njobs:\n  check:\n    runs-on: ubuntu-latest\n"
  ))
  m <- metrics_tests(ctx)
  expect_true(m$ci_present)
})

test_that("ci_present is TRUE for .travis.yml", {
  ctx <- make_ctx(list(".travis.yml" = "language: r\n"))
  m <- metrics_tests(ctx)
  expect_true(m$ci_present)
})

test_that("ci_present is TRUE for appveyor.yml", {
  ctx <- make_ctx(list("appveyor.yml" = "environment:\n  R_ARCH: /i386\n"))
  m <- metrics_tests(ctx)
  expect_true(m$ci_present)
})

test_that("ci_present is FALSE when no CI config is present", {
  ctx <- make_ctx(list(
    "DESCRIPTION" = "Package: p\nVersion: 1.0\n",
    "R/foo.R"     = "foo <- 1\n"
  ))
  m <- metrics_tests(ctx)
  expect_false(m$ci_present)
})

# ===========================================================================
# ci_type
# ===========================================================================

test_that("ci_type lists all detected CI systems as a JSON array", {
  ctx <- make_ctx(list(
    ".github/workflows/check.yml" = "on: push\n",
    ".travis.yml"                 = "language: r\n",
    "appveyor.yml"                = "environment:\n  R_ARCH: /i386\n"
  ))
  m <- metrics_tests(ctx)
  types <- jsonlite::fromJSON(m$ci_type)
  expect_true("github-actions" %in% types)
  expect_true("travis" %in% types)
  expect_true("appveyor" %in% types)
})

test_that("ci_type is '[]' when no CI is present", {
  ctx <- make_ctx(list("DESCRIPTION" = "Package: p\nVersion: 1.0\n"))
  m <- metrics_tests(ctx)
  expect_equal(m$ci_type, "[]")
})

test_that("ci_type detects only github-actions when only GHA present", {
  ctx <- make_ctx(list(
    ".github/workflows/ci.yml" = "on: push\n"
  ))
  m <- metrics_tests(ctx)
  types <- jsonlite::fromJSON(m$ci_type)
  expect_equal(types, "github-actions")
})

# ===========================================================================
# ci_matrix_breadth
# ===========================================================================

test_that("ci_matrix_breadth counts cross-product of inline os and r-version arrays", {
  ctx <- make_ctx(list(
    ".github/workflows/check.yml" = paste0(
      "jobs:\n",
      "  test:\n",
      "    strategy:\n",
      "      matrix:\n",
      "        os: [ubuntu-latest, windows-latest, macos-latest]\n",
      "        r-version: ['release', 'devel']\n"
    )
  ))
  m <- metrics_tests(ctx)
  # 3 os * 2 r-version = 6
  expect_equal(m$ci_matrix_breadth, 6L)
})

test_that("ci_matrix_breadth counts include-style object pairs", {
  ctx <- make_ctx(list(
    ".github/workflows/check.yml" = paste0(
      "jobs:\n",
      "  test:\n",
      "    strategy:\n",
      "      matrix:\n",
      "        include:\n",
      "          - {os: ubuntu-latest, r: 'release'}\n",
      "          - {os: macos-latest, r: 'release'}\n",
      "          - {os: windows-latest, r: 'release'}\n",
      "          - {os: ubuntu-latest, r: 'devel'}\n"
    )
  ))
  m <- metrics_tests(ctx)
  expect_equal(m$ci_matrix_breadth, 4L)
})

test_that("ci_matrix_breadth is 0 when no GHA workflows exist", {
  ctx <- make_ctx(list("DESCRIPTION" = "Package: p\nVersion: 1.0\n"))
  m <- metrics_tests(ctx)
  expect_equal(m$ci_matrix_breadth, 0L)
})

test_that("ci_matrix_breadth is 0 when GHA workflow has no matrix", {
  ctx <- make_ctx(list(
    ".github/workflows/check.yml" =
      "on: push\njobs:\n  check:\n    runs-on: ubuntu-latest\n"
  ))
  m <- metrics_tests(ctx)
  expect_equal(m$ci_matrix_breadth, 0L)
})

# ===========================================================================
# ci_pr_gated
# ===========================================================================

test_that("ci_pr_gated is TRUE when pull_request trigger is present in a workflow", {
  ctx <- make_ctx(list(
    ".github/workflows/check.yml" = paste0(
      "on:\n",
      "  push:\n",
      "    branches: [main]\n",
      "  pull_request:\n",
      "    branches: [main]\n",
      "jobs:\n",
      "  check:\n",
      "    runs-on: ubuntu-latest\n"
    )
  ))
  m <- metrics_tests(ctx)
  expect_true(m$ci_pr_gated)
})

test_that("ci_pr_gated is FALSE when workflow has no pull_request trigger", {
  ctx <- make_ctx(list(
    ".github/workflows/check.yml" =
      "on: push\njobs:\n  check:\n    runs-on: ubuntu-latest\n"
  ))
  m <- metrics_tests(ctx)
  expect_false(m$ci_pr_gated)
})

test_that("ci_pr_gated is FALSE when no CI is present", {
  ctx <- make_ctx(list("DESCRIPTION" = "Package: p\nVersion: 1.0\n"))
  m <- metrics_tests(ctx)
  expect_false(m$ci_pr_gated)
})

# ===========================================================================
# NA-safety: completely empty context
# ===========================================================================

test_that("metrics_tests returns correct defaults on an empty context", {
  ctx <- build_context("p", "0.1", "0.1", "2024-01-01",
                       character(0L), function(p) "")
  m <- metrics_tests(ctx)

  expect_false(m$has_tests)
  expect_true(is.na(m$test_to_code_ratio))
  expect_true(is.na(m$testthat_edition))
  expect_equal(m$snapshot_test_count, 0L)
  expect_equal(m$test_isolation_libs, "[]")
  expect_true(is.na(m$exported_fn_test_linkage))
  expect_true(is.na(m$stochastic_seed_discipline))
  expect_false(m$ci_present)
  expect_equal(m$ci_type, "[]")
  expect_equal(m$ci_matrix_breadth, 0L)
  expect_false(m$ci_pr_gated)
})
