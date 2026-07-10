# tests/testthat/test-run-update-split.R
.fake_io <- function() {
  list(
    package_list = function() data.frame(
      package = "pkgA", latest_version = "1.0", stringsAsFactors = FALSE),
    clone = function(pkg, dest) { dir.create(dest, showWarnings = FALSE); TRUE }
  )
}

test_that("run_update produces separate code and data DB files", {
  # Stub analyze_package to emit a summary row plus one dataset row.
  local_mocked_bindings <- NULL
  old <- analyze_package
  assign("analyze_package", function(dest, pkg) list(
    summary = data.frame(package = pkg, version = "1.0", loc_r = 10L,
                         n_fns_r = 1L, latest_release_date = "2026-01-01",
                         datasets_scanned = 1L, detail_scanned = 1L,
                         stringsAsFactors = FALSE),
    churn = NULL, api = NULL, functions = NULL, edges = NULL,
    datasets = data.frame(package = pkg, name = "d1", version = "1.0",
      file = "data/d1.rda", internal = 0L, format = "rda", compression = "gzip",
      confidence = "high", class = "data.frame", kind = "table", nrow = 5L,
      ncol = 1L, n_missing_total = 0L, content_fp = "cf", schema_fp = "sf",
      fp_algo_version = 1L, columns = '["a"]', row_sketch = NA_character_,
      is_current = 1L, stringsAsFactors = FALSE)
  ), envir = environment(run_update))
  on.exit(assign("analyze_package", old, envir = environment(run_update)), add = TRUE)

  out <- withr::local_tempdir()
  run_update(.fake_io(), out, shard_size = 10L)

  expect_true(file.exists(file.path(out, "bioc-code-metrics.db")))
  expect_true(file.exists(file.path(out, "bioc-data-metrics.db")))
  ccon <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, "bioc-code-metrics.db"))
  on.exit(DBI::dbDisconnect(ccon), add = TRUE)
  expect_false("bioc_datasets" %in% DBI::dbListTables(ccon))
  expect_equal(DBI::dbGetQuery(ccon, "SELECT COUNT(*) n FROM bioc_code_summary")$n, 1L)
  dcon <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, "bioc-data-metrics.db"))
  on.exit(DBI::dbDisconnect(dcon), add = TRUE)
  expect_equal(DBI::dbGetQuery(dcon, "SELECT COUNT(*) n FROM bioc_datasets")$n, 1L)
})
