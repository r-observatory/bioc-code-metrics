# tests/testthat/test-split-upsert.R
.mk_summary <- function() data.frame(
  package = "pkgA", version = "1.0", loc_r = 100L, n_fns_r = 3L,
  datasets_scanned = 1L, stringsAsFactors = FALSE)
.mk_datasets <- function() data.frame(
  package = "pkgA", name = "d1", version = "1.0", file = "data/d1.rda",
  internal = 0L, format = "rda", compression = "gzip", confidence = "high",
  class = "data.frame", kind = "table", nrow = 10L, ncol = 2L, n_missing_total = 0L,
  content_fp = "cf1", schema_fp = "sf1", fp_algo_version = 1L,
  columns = '["a","b"]', row_sketch = '{"a":[1]}', is_current = 1L,
  stringsAsFactors = FALSE)

test_that("upsert_shard writes code tables to the code DB and no dataset tables", {
  code_db <- withr::local_tempfile(fileext = ".db")
  con <- open_or_init_db(code_db); on.exit(DBI::dbDisconnect(con))
  upsert_shard(con, .mk_summary(),
               churn_df = data.frame(package=character(), version=character(),
                                     file=character(), added=integer(), deleted=integer()),
               api_df   = data.frame(package=character(), version=character(),
                                     exports_added=character(), exports_removed=character(),
                                     n_exports=integer()))
  tbls <- DBI::dbListTables(con)
  expect_true("bioc_code_summary" %in% tbls)
  expect_false("bioc_datasets" %in% tbls)
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM bioc_code_summary")$n, 1L)
})

test_that("upsert_datasets writes the normalized dataset tables to the data DB", {
  data_db <- withr::local_tempfile(fileext = ".db")
  dcon <- open_or_init_data_db(data_db); on.exit(DBI::dbDisconnect(dcon))
  upsert_datasets(dcon, .mk_datasets(), pkgs = "pkgA")
  expect_equal(DBI::dbGetQuery(dcon, "SELECT COUNT(*) n FROM bioc_datasets")$n, 1L)
  expect_equal(DBI::dbGetQuery(dcon, "SELECT COUNT(*) n FROM bioc_dataset_versions")$n, 1L)
  expect_equal(DBI::dbGetQuery(dcon, "SELECT COUNT(*) n FROM bioc_dataset_contents")$n, 1L)
  # sketch lands in the data DB (excluded from the merge allowlist, kept here).
  expect_equal(DBI::dbGetQuery(dcon, "SELECT COUNT(*) n FROM bioc_dataset_sketches")$n, 1L)
})
