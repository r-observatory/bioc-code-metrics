# tests/testthat/test-crossversion.R
# Cross-version (temporal) metric tests for add_cross_version_metrics() and
# deprecation_signals().
#
# Run via:
#   Rscript -e 'library(testthat); source("scripts/config.R");
#     source("scripts/git.R"); source("scripts/context.R");
#     for (f in list.files("scripts/metrics", pattern="[.]R$", full.names=TRUE)) source(f);
#     source("scripts/analyze.R");
#     testthat::test_file("tests/testthat/test-crossversion.R")'

# ---------------------------------------------------------------------------
# Synthetic fixtures used across tests
# ---------------------------------------------------------------------------

.make_summary <- function() {
  data.frame(
    package  = "testpkg",
    version  = c("1.0.0", "1.0.1", "1.1.0", "2.0.0"),
    released = c("2020-01-01", "2020-02-01", "2020-04-01", "2021-01-01"),
    dep_list = c(
      '["rlang"]',
      '["rlang","cli"]',
      '["rlang","cli"]',
      '["rlang"]'
    ),
    authors = c(
      '[{"given":"Alice","family":"Smith","roles":["aut"]}]',
      '[{"given":"Alice","family":"Smith","roles":["aut"]}]',
      '[{"given":"Alice","family":"Smith","roles":["aut"]},{"given":"Bob","family":"Jones","roles":["ctb"]}]',
      '[{"given":"Alice","family":"Smith","roles":["aut"]},{"given":"Bob","family":"Jones","roles":["ctb"]}]'
    ),
    stringsAsFactors = FALSE
  )
}

.make_api <- function() {
  data.frame(
    package         = "testpkg",
    version         = c("1.0.0", "1.0.1", "1.1.0", "2.0.0"),
    exports_added   = c('["foo"]', '[]', '["bar"]', '[]'),
    exports_removed = c('[]',      '["baz"]', '[]', '["bar"]'),
    n_exports       = c(1L, 0L, 1L, 0L),
    stringsAsFactors = FALSE
  )
}

# Deprecation series:
#  v1 (1.0.0) : no deprecation signals
#  v2 (1.0.1) : removed "baz" - no prior deprecation -> cold
#  v3 (1.1.0) : deprecates "bar" via base .Deprecated
#  v4 (2.0.0) : removed "bar" - was deprecated in v3 -> warm
.make_dep_series <- function() {
  list(
    list(symbols = character(0L), uses_lifecycle = FALSE),
    list(symbols = character(0L), uses_lifecycle = FALSE),
    list(symbols = "bar",         uses_lifecycle = FALSE),
    list(symbols = character(0L), uses_lifecycle = FALSE)
  )
}

# ---------------------------------------------------------------------------
# 1. bump_type
# ---------------------------------------------------------------------------

test_that("bump_type is initial for first version, then correct for subsequent", {
  res <- add_cross_version_metrics(.make_summary(), .make_api(), .make_dep_series())
  expect_equal(res$bump_type[1L], "initial")
  expect_equal(res$bump_type[2L], "patch")   # 1.0.0 -> 1.0.1
  expect_equal(res$bump_type[3L], "minor")   # 1.0.1 -> 1.1.0
  expect_equal(res$bump_type[4L], "major")   # 1.1.0 -> 2.0.0
})

test_that("bump_type classifies dash- and underscore-separated versions", {
  s <- data.frame(
    package  = "p",
    version  = c("1.0-0", "1.0-1", "1_1_0", "2_0_0"),
    released = c("2020-01-01", "2020-02-01", "2020-03-01", "2021-01-01"),
    stringsAsFactors = FALSE
  )
  a <- data.frame(
    package = "p", version = s$version,
    exports_added = rep('[]', 4L), exports_removed = rep('[]', 4L),
    n_exports = 0L, stringsAsFactors = FALSE
  )
  res <- add_cross_version_metrics(s, a, vector("list", 4L))
  expect_equal(res$bump_type[1L], "initial")
  expect_equal(res$bump_type[2L], "patch")
  expect_equal(res$bump_type[3L], "minor")
  expect_equal(res$bump_type[4L], "major")
})

test_that("malformed version string yields bump_type 'other'", {
  s <- data.frame(
    package = "p", version = c("1.0.0", "BROKEN"),
    released = c("2020-01-01", "2020-06-01"),
    stringsAsFactors = FALSE
  )
  a <- data.frame(
    package = "p", version = s$version,
    exports_added = rep('[]', 2L), exports_removed = rep('[]', 2L),
    n_exports = 0L, stringsAsFactors = FALSE
  )
  res <- add_cross_version_metrics(s, a, vector("list", 2L))
  expect_equal(res$bump_type[2L], "other")
})

# ---------------------------------------------------------------------------
# 2. exports counts, is_breaking, bump_fidelity_ok
# ---------------------------------------------------------------------------

test_that("exports_added_n and exports_removed_n match api_df JSON lengths", {
  res <- add_cross_version_metrics(.make_summary(), .make_api(), .make_dep_series())
  expect_equal(res$exports_added_n[1L],   1L)  # ["foo"]
  expect_equal(res$exports_removed_n[2L], 1L)  # ["baz"]
  expect_equal(res$exports_added_n[3L],   1L)  # ["bar"]
  expect_equal(res$exports_removed_n[4L], 1L)  # ["bar"]
  expect_equal(res$exports_added_n[2L],   0L)  # []
})

test_that("is_breaking is TRUE only when exports are removed", {
  res <- add_cross_version_metrics(.make_summary(), .make_api(), .make_dep_series())
  expect_false(res$is_breaking[1L])   # initial: no removals
  expect_true( res$is_breaking[2L])   # baz removed
  expect_false(res$is_breaking[3L])   # addition only
  expect_true( res$is_breaking[4L])   # bar removed
})

test_that("bump_fidelity_ok is NA for first version", {
  res <- add_cross_version_metrics(.make_summary(), .make_api(), .make_dep_series())
  expect_true(is.na(res$bump_fidelity_ok[1L]))
})

test_that("bump_fidelity_ok is FALSE when removal occurs under a patch bump", {
  # v2 (1.0.1): patch bump but removes "baz" -> fidelity violation
  res <- add_cross_version_metrics(.make_summary(), .make_api(), .make_dep_series())
  expect_false(res$bump_fidelity_ok[2L])
})

test_that("bump_fidelity_ok is TRUE when addition occurs under a minor bump", {
  # v3 (1.1.0): minor bump with addition -> OK
  res <- add_cross_version_metrics(.make_summary(), .make_api(), .make_dep_series())
  expect_true(res$bump_fidelity_ok[3L])
})

test_that("bump_fidelity_ok is TRUE when removal occurs under a major bump", {
  # v4 (2.0.0): major bump with removal -> OK
  res <- add_cross_version_metrics(.make_summary(), .make_api(), .make_dep_series())
  expect_true(res$bump_fidelity_ok[4L])
})

# ---------------------------------------------------------------------------
# 3. release_cadence_days
# ---------------------------------------------------------------------------

test_that("release_cadence_days is median of inter-release gaps in days", {
  # dates: 2020-01-01, 2020-02-01, 2020-04-01, 2021-01-01
  # diffs: 31, 60, 275  -> median = 60
  res <- add_cross_version_metrics(.make_summary(), .make_api(), .make_dep_series())
  expect_equal(res$release_cadence_days[4L], 60)
  # NA on earlier rows
  expect_true(is.na(res$release_cadence_days[1L]))
  expect_true(is.na(res$release_cadence_days[2L]))
})

test_that("release_cadence_days is NA when fewer than 2 versions", {
  s <- data.frame(
    package = "p", version = "1.0.0", released = "2020-01-01",
    stringsAsFactors = FALSE
  )
  a <- data.frame(
    package = "p", version = "1.0.0",
    exports_added = '["x"]', exports_removed = '[]',
    n_exports = 1L, stringsAsFactors = FALSE
  )
  res <- add_cross_version_metrics(s, a, list(list(symbols = character(0L), uses_lifecycle = FALSE)))
  expect_true(is.na(res$release_cadence_days[1L]))
})

# ---------------------------------------------------------------------------
# 4. dependency_drift
# ---------------------------------------------------------------------------

test_that("dependency_drift counts dep additions and removals across history", {
  # 1.0.0 -> 1.0.1: added cli (+1)
  # 1.0.1 -> 1.1.0: no change  (0)
  # 1.1.0 -> 2.0.0: removed cli (+1)
  # Total = 2
  res <- add_cross_version_metrics(.make_summary(), .make_api(), .make_dep_series())
  expect_equal(res$dependency_drift[4L], 2L)
  expect_true(is.na(res$dependency_drift[1L]))
})

test_that("dependency_drift is 0 when deps never change", {
  s <- .make_summary()
  s$dep_list <- rep('["rlang"]', 4L)
  res <- add_cross_version_metrics(s, .make_api(), .make_dep_series())
  expect_equal(res$dependency_drift[4L], 0L)
})

# ---------------------------------------------------------------------------
# 5. authors_added_later
# ---------------------------------------------------------------------------

test_that("authors_added_later counts identities new after the first version", {
  # Bob Jones appears first in v3 but not in v1
  res <- add_cross_version_metrics(.make_summary(), .make_api(), .make_dep_series())
  expect_equal(res$authors_added_later[4L], 1L)
  expect_true(is.na(res$authors_added_later[1L]))
})

test_that("authors_added_later is 0 when authorship is stable", {
  s <- .make_summary()
  s$authors <- rep(
    '[{"given":"Alice","family":"Smith","roles":["aut"]}]', 4L
  )
  res <- add_cross_version_metrics(s, .make_api(), .make_dep_series())
  expect_equal(res$authors_added_later[4L], 0L)
})

# ---------------------------------------------------------------------------
# 6. cold_removal_rate
# ---------------------------------------------------------------------------

test_that("cold_removal_rate is 1 when all removals have no prior deprecation signal", {
  # Build a 2-version scenario: v1 has foo; v2 removes foo with no prior depr
  s <- data.frame(
    package = "p", version = c("1.0.0", "2.0.0"),
    released = c("2020-01-01", "2021-01-01"),
    stringsAsFactors = FALSE
  )
  a <- data.frame(
    package = "p", version = c("1.0.0", "2.0.0"),
    exports_added   = c('["foo"]', '[]'),
    exports_removed = c('[]',      '["foo"]'),
    n_exports = c(1L, 0L), stringsAsFactors = FALSE
  )
  dep <- list(
    list(symbols = character(0L), uses_lifecycle = FALSE),
    list(symbols = character(0L), uses_lifecycle = FALSE)
  )
  res <- add_cross_version_metrics(s, a, dep)
  expect_equal(res$cold_removal_rate[2L], 1.0)
})

test_that("cold_removal_rate is 0 when all removed exports were previously deprecated", {
  # v2 removes foo and foo was in v1's deprecation symbols
  s <- data.frame(
    package = "p", version = c("1.0.0", "2.0.0"),
    released = c("2020-01-01", "2021-01-01"),
    stringsAsFactors = FALSE
  )
  a <- data.frame(
    package = "p", version = c("1.0.0", "2.0.0"),
    exports_added   = c('["foo"]', '[]'),
    exports_removed = c('[]',      '["foo"]'),
    n_exports = c(1L, 0L), stringsAsFactors = FALSE
  )
  dep <- list(
    list(symbols = "foo", uses_lifecycle = FALSE),
    list(symbols = character(0L), uses_lifecycle = FALSE)
  )
  res <- add_cross_version_metrics(s, a, dep)
  expect_equal(res$cold_removal_rate[2L], 0.0)
})

test_that("cold_removal_rate is NA when there are no removals across all versions", {
  s <- data.frame(
    package = "p", version = c("1.0.0", "2.0.0"),
    released = c("2020-01-01", "2021-01-01"),
    stringsAsFactors = FALSE
  )
  a <- data.frame(
    package = "p", version = c("1.0.0", "2.0.0"),
    exports_added   = c('["foo"]', '["bar"]'),
    exports_removed = c('[]',      '[]'),
    n_exports = c(1L, 2L), stringsAsFactors = FALSE
  )
  res <- add_cross_version_metrics(s, a, vector("list", 2L))
  expect_true(is.na(res$cold_removal_rate[2L]))
})

test_that("cold_removal_rate is 0.5 when half the removals had prior deprecation", {
  # v2 removes foo (cold) and bar (warm, deprecated in v1)
  s <- data.frame(
    package = "p", version = c("1.0.0", "2.0.0"),
    released = c("2020-01-01", "2021-01-01"),
    stringsAsFactors = FALSE
  )
  a <- data.frame(
    package = "p", version = c("1.0.0", "2.0.0"),
    exports_added   = c('["foo","bar"]', '[]'),
    exports_removed = c('[]',            '["foo","bar"]'),
    n_exports = c(2L, 0L), stringsAsFactors = FALSE
  )
  dep <- list(
    list(symbols = "bar", uses_lifecycle = FALSE),  # only bar deprecated
    list(symbols = character(0L), uses_lifecycle = FALSE)
  )
  res <- add_cross_version_metrics(s, a, dep)
  expect_equal(res$cold_removal_rate[2L], 0.5)
})

# ---------------------------------------------------------------------------
# 7. deprecation_infrastructure_maturity
# ---------------------------------------------------------------------------

test_that("deprecation_infrastructure_maturity is 0 with no deprecation signals ever", {
  res <- add_cross_version_metrics(
    .make_summary(), .make_api(),
    replicate(4L, list(symbols = character(0L), uses_lifecycle = FALSE), simplify = FALSE)
  )
  expect_equal(res$deprecation_infrastructure_maturity[4L], 0L)
})

test_that("deprecation_infrastructure_maturity is 1 when only base .Deprecated used", {
  res <- add_cross_version_metrics(.make_summary(), .make_api(), .make_dep_series())
  # v3 has symbols = "bar", uses_lifecycle = FALSE
  expect_equal(res$deprecation_infrastructure_maturity[4L], 1L)
})

test_that("deprecation_infrastructure_maturity is 2 when lifecycle is used", {
  dep <- list(
    list(symbols = character(0L), uses_lifecycle = FALSE),
    list(symbols = character(0L), uses_lifecycle = FALSE),
    list(symbols = "bar",         uses_lifecycle = TRUE),   # lifecycle used in v3
    list(symbols = character(0L), uses_lifecycle = FALSE)
  )
  res <- add_cross_version_metrics(.make_summary(), .make_api(), dep)
  expect_equal(res$deprecation_infrastructure_maturity[4L], 2L)
})

# ---------------------------------------------------------------------------
# 8. Package-level columns on latest row, NA on earlier rows
# ---------------------------------------------------------------------------

test_that("n_versions and other package-level columns are on the latest row only", {
  res <- add_cross_version_metrics(.make_summary(), .make_api(), .make_dep_series())
  n   <- nrow(res)

  # Latest row has non-NA values
  expect_equal(res$n_versions[n], 4L)
  expect_false(is.na(res$first_release_date[n]))
  expect_false(is.na(res$latest_release_date[n]))

  # Earlier rows have NA
  for (i in seq_len(n - 1L)) {
    expect_true(is.na(res$n_versions[i]))
    expect_true(is.na(res$dependency_drift[i]))
    expect_true(is.na(res$authors_added_later[i]))
    expect_true(is.na(res$release_cadence_days[i]))
  }
})

test_that("first_release_date and latest_release_date are correct", {
  res <- add_cross_version_metrics(.make_summary(), .make_api(), .make_dep_series())
  n   <- nrow(res)
  expect_equal(res$first_release_date[n],  "2020-01-01")
  expect_equal(res$latest_release_date[n], "2021-01-01")
})

# ---------------------------------------------------------------------------
# 9. assessed_at and assessed_with
# ---------------------------------------------------------------------------

test_that("assessed_at is present on every row as a non-NA character string", {
  res <- add_cross_version_metrics(.make_summary(), .make_api(), .make_dep_series())
  expect_true(all(!is.na(res$assessed_at)))
  expect_true(all(nzchar(res$assessed_at)))
  # Check it looks like a date (YYYY-MM-DD)
  expect_true(all(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", res$assessed_at)))
})

test_that("assessed_with carries a version stamp on every row", {
  res <- add_cross_version_metrics(.make_summary(), .make_api(), .make_dep_series())
  expect_true(all(nzchar(res$assessed_with)))
  expect_true(all(grepl("bioc-code-metrics", res$assessed_with)))
})

# ---------------------------------------------------------------------------
# 10. Empty summary_df: all expected columns present, zero rows
# ---------------------------------------------------------------------------

test_that("empty summary_df returns data.frame with all cross-version columns", {
  empty_s <- data.frame(stringsAsFactors = FALSE)
  empty_a <- data.frame(
    package = character(0L), version = character(0L),
    exports_added = character(0L), exports_removed = character(0L),
    n_exports = integer(0L), stringsAsFactors = FALSE
  )
  res <- add_cross_version_metrics(empty_s, empty_a, list())

  expected_cols <- c(
    "bump_type", "exports_added_n", "exports_removed_n",
    "is_breaking", "bump_fidelity_ok", "assessed_at", "assessed_with",
    "n_versions", "first_release_date", "latest_release_date",
    "release_cadence_days", "dependency_drift", "authors_added_later",
    "cold_removal_rate", "deprecation_infrastructure_maturity"
  )
  for (col in expected_cols) {
    expect_true(col %in% names(res), info = paste("missing column:", col))
  }
  expect_equal(nrow(res), 0L)
})

# ---------------------------------------------------------------------------
# 11. deprecation_signals() unit tests
# ---------------------------------------------------------------------------

test_that("deprecation_signals detects .Deprecated and .Defunct symbols", {
  file_map <- list(
    "R/old.R" = 'old_fn <- function() { .Deprecated("old_fn"); 1 }\n',
    "R/gone.R" = '.Defunct("gone_fn")\n'
  )
  ctx <- build_context("p", "1.0", "1.0", "2020-01-01",
                       names(file_map), function(p) file_map[[p]] %||% "")
  sig <- deprecation_signals(ctx)
  expect_true("old_fn" %in% sig$symbols)
  expect_true("gone_fn" %in% sig$symbols)
  expect_false(sig$uses_lifecycle)
})

test_that("deprecation_signals detects lifecycle calls and extracts what symbol", {
  file_map <- list(
    "R/dep.R" = 'f <- function() { lifecycle::deprecate_warn("1.0.0", "f()"); }\n'
  )
  ctx <- build_context("p", "1.0", "1.0", "2020-01-01",
                       names(file_map), function(p) file_map[[p]] %||% "")
  sig <- deprecation_signals(ctx)
  expect_true(sig$uses_lifecycle)
  expect_true("f" %in% sig$symbols)
})

test_that("deprecation_signals handles lifecycle with pkg:: prefix in what", {
  file_map <- list(
    "R/dep.R" = 'lifecycle::deprecate_soft("2.0.0", "mypkg::bar()")\n'
  )
  ctx <- build_context("p", "2.0", "2.0", "2021-01-01",
                       names(file_map), function(p) file_map[[p]] %||% "")
  sig <- deprecation_signals(ctx)
  expect_true(sig$uses_lifecycle)
  expect_true("bar" %in% sig$symbols)
})

test_that("deprecation_signals returns empty symbols and FALSE when no signals present", {
  file_map <- list("R/clean.R" = "foo <- function() 42\n")
  ctx <- build_context("p", "1.0", "1.0", "2020-01-01",
                       names(file_map), function(p) file_map[[p]] %||% "")
  sig <- deprecation_signals(ctx)
  expect_length(sig$symbols, 0L)
  expect_false(sig$uses_lifecycle)
})

# ---------------------------------------------------------------------------
# 12. Integration: analyze_package adds cross-version columns (requires network)
# ---------------------------------------------------------------------------

test_that("analyze_package adds cross-version columns (integration, requires network)", {
  git_ok <- tryCatch(
    system2("git", "--version", stdout = FALSE, stderr = FALSE) == 0L,
    error   = function(e) FALSE,
    warning = function(w) FALSE
  )
  skip_if(!git_ok, "git not available")

  net_ok <- tryCatch({
    con <- url("https://github.com", open = "r")
    close(con)
    TRUE
  }, warning = function(w) FALSE, error = function(e) FALSE)
  skip_if(!net_ok, "No network access")

  repo_dir <- tempfile("ccm_int_")
  on.exit(unlink(repo_dir, recursive = TRUE, force = TRUE), add = TRUE)

  rc <- suppressWarnings(
    system2("git",
            c("clone", "--quiet",
              "https://github.com/bioc/BiocGenerics", repo_dir),
            stdout = FALSE, stderr = FALSE)
  )
  skip_if(rc != 0L, "Could not clone BiocGenerics (network issue)")

  result <- analyze_package(repo_dir, "BiocGenerics")
  sum_df <- result$summary
  last   <- nrow(sum_df)

  # Per-version columns exist on every row
  per_ver_cols <- c("bump_type", "exports_added_n", "exports_removed_n",
                    "is_breaking", "bump_fidelity_ok", "assessed_at", "assessed_with")
  for (col in per_ver_cols) {
    expect_true(col %in% names(sum_df), info = paste("missing column:", col))
  }

  # Package-level columns exist
  pkg_cols <- c("n_versions", "first_release_date", "latest_release_date",
                "release_cadence_days", "dependency_drift", "authors_added_later",
                "cold_removal_rate", "deprecation_infrastructure_maturity")
  for (col in pkg_cols) {
    expect_true(col %in% names(sum_df), info = paste("missing column:", col))
  }

  # First version is always "initial"
  expect_equal(sum_df$bump_type[1L], "initial")

  # Package-level columns are non-NA on the latest row
  expect_false(is.na(sum_df$n_versions[last]))
  expect_equal(sum_df$n_versions[last], last)
  expect_false(is.na(sum_df$first_release_date[last]))
  expect_false(is.na(sum_df$latest_release_date[last]))

  # release_cadence_days is non-NA when there are multiple versions
  if (last >= 2L) {
    expect_false(is.na(sum_df$release_cadence_days[last]))
    expect_gt(sum_df$release_cadence_days[last], 0)
  }

  # Package-level columns are NA on all but the last row
  if (last >= 2L) {
    expect_true(all(is.na(sum_df$n_versions[-last])))
  }
})
