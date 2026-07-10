# tests/testthat/test-render-notes.R
test_that("render_notes writes both notes files", {
  out <- withr::local_tempdir()
  write_manifest(file.path(out, "code-manifest.json"), list(
    schema_version = 1L, series = "code", repo = "r", db_filename = "bioc-code-metrics.db",
    generated_at = "2026-07-10T00:00:00Z", db_bytes = 100L, fingerprint = strrep("a", 64),
    n_packages = 2L, n_versions = 3L, tables = list(bioc_code_summary = 3L),
    stats = list(loc_r_mean = 10), bootstrap = list(n_analyzed = 2L, n_universe = 5L,
      n_remaining = 3L, bootstrap_complete = FALSE)))
  write_manifest(file.path(out, "data-manifest.json"), list(
    schema_version = 1L, series = "data", repo = "r", db_filename = "bioc-data-metrics.db",
    generated_at = "2026-07-10T00:00:00Z", db_bytes = 50L, fingerprint = strrep("b", 64),
    n_packages = 1L, n_versions = 1L, tables = list(bioc_datasets = 1L),
    stats = list(), bootstrap = list(n_analyzed = 2L, n_universe = 5L,
      n_remaining = 3L, bootstrap_complete = FALSE)))
  writeLines(c("pkgA", "pkgB"), file.path(out, "changed-packages.txt"))

  render_notes(out, prev_code_tag = NULL, prev_data_tag = NULL,
               title_prefix = "Bioconductor")
  code_md <- readLines(file.path(out, "release-notes-code.md"))
  expect_true(any(grepl("first dated release", code_md)))
  expect_true(file.exists(file.path(out, "release-notes-data.md")))
})

test_that("render_notes annotates a prev manifest with its dated_tag in the changelog heading", {
  out <- withr::local_tempdir()
  write_manifest(file.path(out, "code-manifest.json"), list(
    schema_version = 1L, series = "code", repo = "r", db_filename = "bioc-code-metrics.db",
    generated_at = "2026-07-10T00:00:00Z", db_bytes = 120L, fingerprint = strrep("a", 64),
    n_packages = 3L, n_versions = 4L, tables = list(bioc_code_summary = 4L),
    stats = list(loc_r_mean = 11), bootstrap = list(n_analyzed = 3L, n_universe = 5L,
      n_remaining = 2L, bootstrap_complete = FALSE)))
  write_manifest(file.path(out, "data-manifest.json"), list(
    schema_version = 1L, series = "data", repo = "r", db_filename = "bioc-data-metrics.db",
    generated_at = "2026-07-10T00:00:00Z", db_bytes = 60L, fingerprint = strrep("b", 64),
    n_packages = 2L, n_versions = 2L, tables = list(bioc_datasets = 2L),
    stats = list(), bootstrap = list(n_analyzed = 2L, n_universe = 5L,
      n_remaining = 3L, bootstrap_complete = FALSE)))
  write_manifest(file.path(out, "prev-code-manifest.json"), list(
    schema_version = 1L, series = "code", repo = "r", db_filename = "bioc-code-metrics.db",
    generated_at = "2026-07-09T00:00:00Z", db_bytes = 100L, fingerprint = strrep("c", 64),
    n_packages = 2L, n_versions = 3L, tables = list(bioc_code_summary = 3L),
    stats = list(loc_r_mean = 10), bootstrap = list(n_analyzed = 2L, n_universe = 5L,
      n_remaining = 3L, bootstrap_complete = FALSE)))
  write_manifest(file.path(out, "prev-data-manifest.json"), list(
    schema_version = 1L, series = "data", repo = "r", db_filename = "bioc-data-metrics.db",
    generated_at = "2026-07-09T00:00:00Z", db_bytes = 50L, fingerprint = strrep("d", 64),
    n_packages = 1L, n_versions = 1L, tables = list(bioc_datasets = 1L),
    stats = list(), bootstrap = list(n_analyzed = 2L, n_universe = 5L,
      n_remaining = 3L, bootstrap_complete = FALSE)))
  writeLines(c("pkgA", "pkgB"), file.path(out, "changed-packages.txt"))

  render_notes(out, prev_code_tag = "code-2026-07-09", prev_data_tag = "data-2026-07-09",
               title_prefix = "Bioconductor")

  code_md <- readLines(file.path(out, "release-notes-code.md"))
  expect_true(any(grepl("^## Changes since code-2026-07-09$", code_md)))
  expect_false(any(grepl("first dated release", code_md)))

  data_md <- readLines(file.path(out, "release-notes-data.md"))
  expect_true(any(grepl("^## Changes since data-2026-07-09$", data_md)))
})

test_that("render_notes treats an empty-string prev tag as absent", {
  out <- withr::local_tempdir()
  write_manifest(file.path(out, "code-manifest.json"), list(
    schema_version = 1L, series = "code", repo = "r", db_filename = "bioc-code-metrics.db",
    generated_at = "2026-07-10T00:00:00Z", db_bytes = 100L, fingerprint = strrep("a", 64),
    n_packages = 2L, n_versions = 3L, tables = list(bioc_code_summary = 3L),
    stats = list(loc_r_mean = 10), bootstrap = list(n_analyzed = 2L, n_universe = 5L,
      n_remaining = 3L, bootstrap_complete = FALSE)))
  write_manifest(file.path(out, "data-manifest.json"), list(
    schema_version = 1L, series = "data", repo = "r", db_filename = "bioc-data-metrics.db",
    generated_at = "2026-07-10T00:00:00Z", db_bytes = 50L, fingerprint = strrep("b", 64),
    n_packages = 1L, n_versions = 1L, tables = list(bioc_datasets = 1L),
    stats = list(), bootstrap = list(n_analyzed = 2L, n_universe = 5L,
      n_remaining = 3L, bootstrap_complete = FALSE)))
  writeLines(character(0L), file.path(out, "changed-packages.txt"))

  render_notes(out, prev_code_tag = "", prev_data_tag = "", title_prefix = "Bioconductor")

  code_md <- readLines(file.path(out, "release-notes-code.md"))
  expect_true(any(grepl("first dated release", code_md)))
})
