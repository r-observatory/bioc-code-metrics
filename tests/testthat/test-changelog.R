# tests/testthat/test-changelog.R
test_that("record/read changed packages unions and dedupes", {
  p <- withr::local_tempfile()
  record_changed_packages(p, c("b", "a"))
  record_changed_packages(p, c("a", "c"))
  expect_identical(read_changed_packages(p), c("a", "b", "c"))
})

test_that("read_changed_packages returns empty when absent", {
  expect_identical(read_changed_packages(tempfile()), character(0L))
})

test_that("build_changelog renders the first-release form when prev is NULL", {
  today <- list(n_packages = 3L, n_versions = 5L, db_bytes = 1000L)
  md <- build_changelog(today, prev = NULL, changed_pkgs = character(0L))
  expect_true(grepl("Changes since \\(first dated release\\)", md))
  expect_true(grepl("Packages: 3", md))
})

test_that("build_changelog renders deltas and a capped package list", {
  prev  <- list(n_packages = 2L, n_versions = 4L, db_bytes = 900L,
                tables = list(bioc_code_summary = 4L),
                stats = list(loc_r_mean = 10))
  today <- list(n_packages = 3L, n_versions = 5L, db_bytes = 1000L,
                tables = list(bioc_code_summary = 5L),
                stats = list(loc_r_mean = 12))
  md <- build_changelog(today, prev, changed_pkgs = paste0("p", 1:30), cap = 25L)
  expect_true(grepl("Packages: 3 \\(\\+1\\)", md))
  expect_true(grepl("Versions: 5 \\(\\+1\\)", md))
  expect_true(grepl("bioc_code_summary: 5 \\(\\+1\\)", md))
  expect_true(grepl("loc_r mean: 12 \\(\\+2\\)", md))
  expect_true(grepl("25 shown of 30", md))
})

test_that("build_changelog cap keeps only the first `cap` packages in sorted order", {
  today <- list(n_packages = 1L, n_versions = 1L, db_bytes = 1L, series = "code")
  prev  <- list(n_packages = 1L, n_versions = 1L, db_bytes = 1L)
  md <- build_changelog(today, prev, changed_pkgs = c("zeta", "alpha", "mid", "beta"), cap = 2L)
  expect_true(grepl("Newly or re-analyzed packages \\(2 shown of 4\\):", md))
  expect_true(grepl("- alpha, beta", md, fixed = TRUE))
  expect_false(grepl("zeta", md))
  expect_false(grepl("mid", md))
})

test_that("build_changelog renders (n/a) rather than a fabricated 0 when a stat or table exists on only one side", {
  today <- list(n_packages = 3L, n_versions = 5L, db_bytes = 1000L, series = "code",
                tables = list(bioc_code_summary = 5L),
                stats = list(loc_r_mean = 12, loc_py_mean = 8))
  prev  <- list(n_packages = 2L, n_versions = 4L, db_bytes = 900L,
                tables = list(bioc_code_summary = 4L, bioc_api_history = 9L),
                stats = list(loc_r_mean = 10))
  md <- build_changelog(today, prev, changed_pkgs = character(0L))

  # loc_py_mean exists only in today: show its known value, but the delta
  # must be "(n/a)", never a fabricated "+8".
  expect_true(grepl("loc_py mean: 8 \\(n/a\\)", md))
  expect_false(grepl("loc_py mean: 8 \\(\\+8\\)", md))

  # bioc_api_history exists only in prev: today has no known value, so the
  # value column must read "n/a" (never a fabricated 0), with an "(n/a)" delta.
  expect_true(grepl("bioc_api_history: n/a \\(n/a\\)", md))

  # No changed packages this run: say so explicitly, not "(0 shown of 0)".
  expect_true(grepl("Newly or re-analyzed packages \\(0\\): none", md))
})

test_that("build_changelog uses prev$dated_tag verbatim in the heading when supplied", {
  today <- list(n_packages = 1L, n_versions = 1L, db_bytes = 1L, series = "code")
  prev  <- list(n_packages = 1L, n_versions = 1L, db_bytes = 1L,
                dated_tag = "code-2026-07-01")
  md <- build_changelog(today, prev, changed_pkgs = character(0L))
  first_line <- strsplit(md, "\n", fixed = TRUE)[[1]][1]
  expect_identical(first_line, "## Changes since code-2026-07-01")
})

test_that("build_changelog falls back to a <series>-<prev> heading when prev$dated_tag is absent", {
  today <- list(n_packages = 1L, n_versions = 1L, db_bytes = 1L, series = "data")
  prev  <- list(n_packages = 1L, n_versions = 1L, db_bytes = 1L)
  md <- build_changelog(today, prev, changed_pkgs = character(0L))
  first_line <- strsplit(md, "\n", fixed = TRUE)[[1]][1]
  expect_identical(first_line, "## Changes since data-<prev>")
})

test_that("render_release_notes assembles the title, manifest facts and changelog", {
  manifest <- list(
    db_filename = "bioc-code-metrics.db",
    db_bytes = 123456L,
    generated_at = "2026-07-10T00:00:00Z",
    fingerprint = "deadbeef",
    n_packages = 10L,
    n_versions = 12L,
    bootstrap = list(n_analyzed = 10L, n_universe = 20L, n_remaining = 10L,
                     bootstrap_complete = FALSE)
  )
  changelog_md <- "## Changes since (first dated release)\n\nInitial dated release; no prior snapshot to diff."
  lines <- render_release_notes(manifest, changelog_md, title = "bioc-code-metrics 2026-07-10")

  expect_true(is.character(lines))
  expect_identical(lines[1], "# bioc-code-metrics 2026-07-10")
  expect_true(any(grepl("bioc-code-metrics.db` \\(123,456 bytes\\)", lines)))
  expect_true(any(grepl("Packages: 10 {1,}Versions: 12", lines)))
  expect_true(any(grepl("Bootstrap: 10/20 analyzed \\(10 remaining, complete=false\\)", lines)))
  expect_true(any(grepl("Changes since \\(first dated release\\)", lines)))
})

test_that("render_release_notes shows \"?\" (never a fabricated count) for unmeasurable bootstrap fields", {
  manifest <- list(
    db_filename = "x.db", db_bytes = 10L, generated_at = "t", fingerprint = "fp",
    n_packages = 1L, n_versions = 1L,
    bootstrap = list(n_analyzed = 1L, n_universe = NULL, n_remaining = NULL,
                     bootstrap_complete = TRUE)
  )
  lines <- render_release_notes(manifest, "changelog body", title = "t")
  expect_true(any(grepl("Bootstrap: 1/\\? analyzed \\(\\? remaining, complete=true\\)", lines)))
})
