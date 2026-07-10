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
      stringsAsFactors = FALSE),
    churn = NULL, api = NULL, functions = NULL, edges = NULL,
    datasets = data.frame(package = pkg, name = "d1", version = "1.0",
      file = "data/d1.rda", internal = 0L, format = "rda", compression = "gzip",
      confidence = "high", class = "data.frame", kind = "table", nrow = 5L,
      ncol = 1L, n_missing_total = 0L, content_fp = "cf", schema_fp = "sf",
      fp_algo_version = 1L, columns = '["a"]', row_sketch = NA_character_,
      is_current = 1L, stringsAsFactors = FALSE)),
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
})
