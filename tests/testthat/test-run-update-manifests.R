# tests/testthat/test-run-update-manifests.R
.fake_io2 <- function() list(
  package_list = function() data.frame(package = "pkgA", latest_version = "1.0",
                                       stringsAsFactors = FALSE),
  clone = function(pkg, dest) { dir.create(dest, showWarnings = FALSE); TRUE })

test_that("run_update writes both manifests and the changed-packages file", {
  old <- analyze_package
  assign("analyze_package", function(dest, pkg) list(
    summary = data.frame(package = pkg, version = "1.0", loc_r = 10L, n_fns_r = 1L,
      latest_release_date = "2026-01-01", datasets_scanned = 1L, detail_scanned = 1L,
      analyzer_version = "0.4.0-test", stringsAsFactors = FALSE),
    churn = NULL, api = NULL, functions = NULL, edges = NULL,
    datasets = data.frame(package = pkg, name = "d1", version = "1.0",
      file = "data/d1.rda", internal = 0L, format = "rda", compression = "gzip",
      confidence = "high", class = "data.frame", kind = "table", nrow = 5L,
      ncol = 1L, n_missing_total = 0L, content_fp = "cf", schema_fp = "sf",
      fp_algo_version = FP_ALGO_VERSION, columns = '["a"]', row_sketch = NA_character_,
      is_current = 1L, stringsAsFactors = FALSE),
    binary_versions = "1.0"),
    envir = environment(run_update))
  on.exit(assign("analyze_package", old, envir = environment(run_update)), add = TRUE)

  out <- withr::local_tempdir()
  run_update(.fake_io2(), out, shard_size = 10L)

  expect_true(file.exists(file.path(out, "code-manifest.json")))
  expect_true(file.exists(file.path(out, "data-manifest.json")))
  expect_true(file.exists(file.path(out, "run-status.json")))
  cm <- jsonlite::fromJSON(file.path(out, "code-manifest.json"))
  expect_identical(cm$series, "code")
  expect_identical(cm$n_packages, 1L)
  dm <- jsonlite::fromJSON(file.path(out, "data-manifest.json"))
  expect_identical(dm$series, "data")
  # The data manifest must be built against the DATA connection: a dataset row
  # was written, so n_packages is 1. Building it against the code connection
  # (a data_con/con swap) would read 0 here, catching that mistake.
  expect_identical(dm$n_packages, 1L)
  expect_true("pkgA" %in% read_changed_packages(file.path(out, "changed-packages.txt")))
  # A row that says it was dataset-scanned names the build that scanned it.
  # analyze_package cannot return one without the other, so a stub that does is
  # standing in for a shape the pipeline never sees.
  ccon <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, DB_FILENAME))
  on.exit(DBI::dbDisconnect(ccon), add = TRUE)
  expect_equal(
    DBI::dbGetQuery(ccon, "SELECT analyzer_version FROM bioc_code_summary")[[1L]],
    "0.4.0-test")
})

test_that("both manifests report the packages no dataset scan reached", {
  # bootstrap_complete is true here and the package has never been dataset
  # scanned. Only the new count says so, and it has to reach the file: the log
  # line that would have said it scrolls away with the run.
  old <- analyze_package
  assign("analyze_package", function(dest, pkg) list(
    summary = data.frame(package = pkg, version = "1.0", loc_r = 10L, n_fns_r = 1L,
      latest_release_date = "2026-01-01", datasets_scanned = NA, detail_scanned = 1L,
      stringsAsFactors = FALSE),
    churn = NULL, api = NULL, functions = NULL, edges = NULL, datasets = NULL,
    binary_versions = character(0L)),
    envir = environment(run_update))
  on.exit(assign("analyze_package", old, envir = environment(run_update)), add = TRUE)

  out <- withr::local_tempdir()
  run_update(.fake_io2(), out, shard_size = 10L)

  cm <- jsonlite::fromJSON(file.path(out, "code-manifest.json"))
  expect_true(cm$bootstrap$bootstrap_complete)
  expect_identical(cm$bootstrap$n_datasets_unscanned, 1L)
  # Both series carry it: whoever is reading the dataset manifest is the one
  # asking how much of the catalog was ever looked at.
  dm <- jsonlite::fromJSON(file.path(out, "data-manifest.json"))
  expect_identical(dm$bootstrap$n_datasets_unscanned, 1L)
})

test_that("both manifests report the datasets the reader could not measure", {
  # A dataset the analyzer described and could not fingerprint reaches the
  # catalog with no profile behind it. Nothing outside the shard's own log said
  # how many, so a build that started losing objects looked like a quiet run.
  old <- analyze_package
  assign("analyze_package", function(dest, pkg) list(
    summary = data.frame(package = pkg, version = "1.0", loc_r = 10L, n_fns_r = 1L,
      latest_release_date = "2026-01-01", datasets_scanned = 1L, detail_scanned = 1L,
      analyzer_version = "0.4.0-test", stringsAsFactors = FALSE),
    churn = NULL, api = NULL, functions = NULL, edges = NULL,
    datasets = data.frame(
      package = pkg, version = "1.0", is_current = 1L,
      fp_algo_version = FP_ALGO_VERSION,
      name = c("measured", "packed"),
      file = c("data/measured.rda", "data/packed.rda"),
      internal = 0L, format = "rda", compression = "gzip",
      class = c("data.frame", "PackedSpatRaster"),
      kind = c("data.frame", "object"),
      nrow = c(3L, NA_integer_), ncol = c(2L, NA_integer_),
      schema_fp = c("S1", NA_character_),
      shape_fp = c("SH", NA_character_),
      content_fp = c("C1", NA_character_),
      confidence = c("exact", "degraded"),
      row_sketch = NA_character_, stringsAsFactors = FALSE),
    binary_versions = c(rpkg_analyzer = "0.4.0-test")),
    envir = environment(run_update))
  on.exit(assign("analyze_package", old, envir = environment(run_update)), add = TRUE)

  out <- withr::local_tempdir()
  run_update(.fake_io2(), out, shard_size = 10L)

  dm <- jsonlite::fromJSON(file.path(out, "data-manifest.json"))
  expect_identical(dm$bootstrap$n_datasets_unmeasured, 1L)
  expect_identical(dm$tables$bioc_dataset_versions, 2L)
  cm <- jsonlite::fromJSON(file.path(out, "code-manifest.json"))
  expect_identical(cm$bootstrap$n_datasets_unmeasured, 1L)
})
