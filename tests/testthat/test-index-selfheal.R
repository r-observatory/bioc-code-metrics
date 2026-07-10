# tests/testthat/test-index-selfheal.R
# Indexes must be (re)created on an existing-but-unindexed database, not only
# at first table creation. Simulates a .dump restore that dropped indexes.

.idx_on <- function(con, table) sort(DBI::dbGetQuery(con,
  "SELECT name FROM sqlite_master WHERE type='index' AND tbl_name = ? AND name LIKE 'idx_%'",
  params = list(table))$name)

test_that("open_or_init_db backfills churn/api indexes on an unindexed DB", {
  path <- withr::local_tempfile(fileext = ".db")
  raw <- DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbExecute(raw, "CREATE TABLE bioc_code_churn (package TEXT, version TEXT, file TEXT, added INTEGER, deleted INTEGER)")
  DBI::dbExecute(raw, "CREATE TABLE bioc_api_history (package TEXT, version TEXT, exports_added TEXT, exports_removed TEXT, n_exports INTEGER)")
  DBI::dbDisconnect(raw)                 # tables exist, zero indexes

  con <- open_or_init_db(path); on.exit(DBI::dbDisconnect(con))
  expect_true("idx_churn_pkg_ver" %in% .idx_on(con, "bioc_code_churn"))
  expect_true("idx_churn_pkg"     %in% .idx_on(con, "bioc_code_churn"))
  expect_true("idx_api_pkg_ver"   %in% .idx_on(con, "bioc_api_history"))
})

test_that(".append_detail_table backfills indexes on an existing unindexed detail table", {
  path <- withr::local_tempfile(fileext = ".db")
  con <- open_or_init_db(path); on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "CREATE TABLE bioc_functions (package TEXT, version TEXT, name TEXT)")  # no index
  .append_detail_table(con, "bioc_functions",
                       data.frame(package = "pkgA", version = "1.0", name = "f", stringsAsFactors = FALSE))
  expect_equal(.idx_on(con, "bioc_functions"),
               c("idx_bioc_functions_pkg", "idx_bioc_functions_pkg_ver"))
})

test_that("upsert_shard keeps the summary UNIQUE index present on a pre-existing summary table", {
  path <- withr::local_tempfile(fileext = ".db")
  con <- open_or_init_db(path); on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "CREATE TABLE bioc_code_summary (package TEXT, version TEXT, loc_r INTEGER)")  # no index
  upsert_shard(con,
    data.frame(package = "pkgA", version = "1.0", loc_r = 10L, stringsAsFactors = FALSE),
    churn_df = data.frame(package=character(), version=character(), file=character(), added=integer(), deleted=integer()),
    api_df   = data.frame(package=character(), version=character(), exports_added=character(), exports_removed=character(), n_exports=integer()))
  expect_true("idx_summary_pkg_ver" %in% .idx_on(con, "bioc_code_summary"))
})

test_that("open_or_init_data_db backfills dataset indexes on an unindexed data DB", {
  path <- withr::local_tempfile(fileext = ".db")
  raw <- DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbExecute(raw, "CREATE TABLE bioc_dataset_versions (package TEXT, version TEXT, content_id TEXT)")
  DBI::dbExecute(raw, "CREATE TABLE bioc_dataset_contents (content_id TEXT, schema_fp TEXT)")
  DBI::dbDisconnect(raw)

  dcon <- open_or_init_data_db(path); on.exit(DBI::dbDisconnect(dcon))
  expect_true("idx_bioc_dsv_content" %in% .idx_on(dcon, "bioc_dataset_versions"))
  expect_true("idx_bioc_dsc_schema"  %in% .idx_on(dcon, "bioc_dataset_contents"))
})
