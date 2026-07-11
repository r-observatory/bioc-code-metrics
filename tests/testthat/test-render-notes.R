# tests/testthat/test-render-notes.R
#
# Fixture for the rich, per-package release notes format: a code DB with
# two packages (limma already in the seed set = updated; DESeq2 not in the
# seed set = new) and a dataset DB where only limma has rows.

.empty_churn_rn <- function() {
  data.frame(package = character(0L), version = character(0L),
             file = character(0L), added = integer(0L), deleted = integer(0L),
             stringsAsFactors = FALSE)
}

.empty_api_rn <- function() {
  data.frame(package = character(0L), version = character(0L),
             exports_added = character(0L), exports_removed = character(0L),
             n_exports = integer(0L), stringsAsFactors = FALSE)
}

# Writes code-manifest.json, data-manifest.json, changed-packages.txt,
# seed-packages.txt, and the two databases into `out` (a tempdir the caller
# owns/cleans up -- withr::local_tempdir() must be called in the test_that
# block itself, not in here, or the dir is removed as soon as this helper
# returns). `changed` lets the zero-changes case override the default
# two-package changed set.
.setup_notes_fixture <- function(out, changed = c("limma", "DESeq2")) {
  summary_df <- data.frame(
    package             = c("limma", "DESeq2"),
    version              = c("3.62.0", "1.46.0"),
    loc_r                = c(18240L, 42110L),
    n_exports            = c(128L, 512L),
    n_internal           = c(214L, 378L),
    n_deps_direct        = c(2L, 11L),
    latest_release_date  = c("2026-06-01", "2026-06-15"),
    stringsAsFactors = FALSE
  )
  export_metrics(file.path(out, DB_FILENAME), summary_df, .empty_churn_rn(), .empty_api_rn())

  data_con <- open_or_init_data_db(file.path(out, DATA_DB_FILENAME))
  DBI::dbExecute(data_con,
    "INSERT INTO bioc_datasets (package, name) VALUES (?, ?)",
    params = list(rep("limma", 3L), c("ds1", "ds2", "ds3")))
  DBI::dbDisconnect(data_con)

  write_manifest(file.path(out, "code-manifest.json"), list(
    schema_version = 1L, series = "code", repo = "r", db_filename = DB_FILENAME,
    generated_at = "2026-07-10T00:00:00Z", db_bytes = 100000L,
    fingerprint = paste0("abcdef12", strrep("0", 56)),
    n_packages = 1500L, n_versions = 5000L,
    tables = list(bioc_code_summary = 5000L, bioc_functions = 45000L),
    stats = list(loc_r_mean = 900, loc_r_median = 850),
    bootstrap = list(n_analyzed = 1000L, n_universe = 2000L,
      n_remaining = 1000L, bootstrap_complete = FALSE)))

  write_manifest(file.path(out, "data-manifest.json"), list(
    schema_version = 1L, series = "data", repo = "r", db_filename = DATA_DB_FILENAME,
    generated_at = "2026-07-10T00:00:00Z", db_bytes = 20000L,
    fingerprint = strrep("b", 64),
    n_packages = 3000L, n_versions = 12000L,
    tables = list(bioc_datasets = 9000L),
    stats = list(),
    bootstrap = list(n_analyzed = 1000L, n_universe = 2000L,
      n_remaining = 1000L, bootstrap_complete = FALSE)))

  writeLines(changed, file.path(out, "changed-packages.txt"))
  writeLines("limma", file.path(out, "seed-packages.txt"))

  invisible(NULL)
}

test_that("render_notes renders the headline, per-package table, catalog and footer", {
  out <- withr::local_tempdir()
  .setup_notes_fixture(out)
  render_notes(out, prev_code_tag = NULL, prev_data_tag = NULL, title_prefix = "Bioconductor")

  code_md <- readLines(file.path(out, "release-notes-code.md"))
  data_md <- readLines(file.path(out, "release-notes-data.md"))

  # Both notes files carry the same rich body.
  expect_identical(code_md, data_md)

  # No redundant top-level H1 (the GitHub release title already has it).
  expect_false(any(grepl("^# ", code_md)))

  # Headline: 1 new (DESeq2, not in seed-packages.txt), 1 updated (limma).
  expect_true(any(grepl(
    "^1 package new to the catalog, 1 updated\\. Now tracking 1,500 packages across 5,000 versions\\.",
    code_md)))
  expect_true(any(grepl("Bootstrap 50% processed \\(1,000 remaining\\)\\.", code_md)))

  # Table: limma is tagged updated (no "(new)"); DESeq2 is tagged new.
  expect_true(any(grepl("^\\| DESeq2 \\| 1\\.46\\.0 \\(new\\) \\| 42,110 \\| 890 \\| 512 \\| 11 \\| 0 \\|$", code_md)))
  expect_true(any(grepl("^\\| limma \\| 3\\.62\\.0 \\| 18,240 \\| 342 \\| 128 \\| 2 \\| 3 \\|$", code_md)))

  # Catalog section pulls straight from the manifests already read.
  expect_true(any(grepl("^## Catalog at a glance$", code_md)))
  expect_true(any(grepl("1,500 packages, 5,000 versions, 45,000 functions", code_md)))
  expect_true(any(grepl("median 850 LOC per package", code_md)))
  # Distinct datasets (bioc_datasets = 9,000), NOT dataset versions (n_versions = 12,000).
  expect_true(any(grepl("9,000 datasets across 3,000 packages", code_md)))
  expect_false(any(grepl("12,000 datasets", code_md)))
  # Both DB sizes are named and flagged as separate releases (98 KB / 20 KB here).
  expect_true(any(grepl(
    "Databases: code metrics 98 KB and dataset metrics 20 KB \\(published as separate code and data releases\\)",
    code_md)))

  # Footer: short fingerprint only, no db bytes/full fingerprint/timestamp.
  expect_true(any(grepl("^<sub>fingerprint abcdef12 - full manifest in the release assets</sub>$", code_md)))
  expect_false(any(grepl("100000", code_md)))
  expect_false(any(grepl(strrep("0", 56), code_md, fixed = TRUE)))
  expect_false(any(grepl("2026-07-10T00:00:00Z", code_md)))
})

test_that("render_notes writes 'No package changes in this release.' when nothing changed", {
  out <- withr::local_tempdir()
  .setup_notes_fixture(out, changed = character(0L))
  render_notes(out, prev_code_tag = NULL, prev_data_tag = NULL, title_prefix = "Bioconductor")

  code_md <- readLines(file.path(out, "release-notes-code.md"))
  expect_true(any(grepl("^0 packages new to the catalog, 0 updated\\.", code_md)))
  expect_true(any(grepl("^No package changes in this release\\.$", code_md)))
  expect_false(any(grepl("^\\|", code_md)))  # no table rows at all
  expect_false(any(grepl("^# ", code_md)))
})

test_that("render_notes treats an absent seed-packages.txt as an empty seed set", {
  out <- withr::local_tempdir()
  .setup_notes_fixture(out)
  unlink(file.path(out, "seed-packages.txt"))
  render_notes(out, prev_code_tag = NULL, prev_data_tag = NULL, title_prefix = "Bioconductor")

  code_md <- readLines(file.path(out, "release-notes-code.md"))
  # Both packages now count as new (no seed set at all).
  expect_true(any(grepl("^2 packages new to the catalog, 0 updated\\.", code_md)))
  expect_true(any(grepl("limma \\| 3\\.62\\.0 \\(new\\)", code_md)))
})
