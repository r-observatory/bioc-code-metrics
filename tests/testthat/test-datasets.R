# Tests for dataset-record parsing (binary.R) and the bioc_datasets detail
# table (export.R). Dataset records are emitted by rpkg-analyzer for every file
# under data/ and R/sysdata.rda; the pipeline stamps them with package+version
# and stores one row per dataset per version.

test_that("parse_analyzer_records collects dataset records into a frame", {
  lines <- c(
    '{"rec":"summary","package":"p","version":"1.0"}',
    '{"rec":"dataset","name":"mtcars","file":"data/mtcars.rda","internal":false,"format":"rda","format_version":2,"compression":"gzip","class":"data.frame","kind":"data.frame","nrow":32,"ncol":11,"has_rownames":true,"n_missing_total":0,"schema_fp":"aaa","shape_fp":"bbb","content_fp":"ccc","columns":[{"name":"mpg","type":"numeric","is_factor":false,"n_missing":0,"n_unique":25}],"row_sketch":["0001","0002"],"confidence":"exact"}',
    '{"rec":"dataset","name":"internal_df","file":"R/sysdata.rda","internal":true,"format":"rda","format_version":3,"compression":"xz","class":"S4:RangedSummarizedExperiment","s4_package":"SummarizedExperiment","kind":"RangedSummarizedExperiment","nrow":100,"ncol":8,"confidence":"degraded","notes":"s4-assay-dims"}'
  )
  ds <- parse_analyzer_records(lines)$datasets

  expect_equal(nrow(ds), 2L)
  expect_true(all(c("name", "file", "internal", "format", "format_version",
                    "compression", "class", "kind", "nrow", "ncol", "length",
                    "n_cols", "n_missing_total", "schema_fp", "shape_fp",
                    "content_fp", "s4_package", "confidence", "notes",
                    "columns", "row_sketch") %in% names(ds)))

  mt <- ds[ds$name == "mtcars", ]
  expect_equal(mt$nrow, 32L)
  expect_equal(mt$ncol, 11L)
  expect_equal(mt$content_fp, "ccc")
  expect_equal(mt$n_cols, 1L)          # derived from the columns array length
  expect_false(mt$internal)
  expect_true(grepl("mpg", mt$columns))       # nested columns kept as JSON
  expect_true(grepl("0001", mt$row_sketch))   # nested row_sketch kept as JSON

  sd <- ds[ds$name == "internal_df", ]
  expect_true(sd$internal)
  expect_equal(sd$s4_package, "SummarizedExperiment")
  expect_equal(sd$nrow, 100L)
  expect_equal(sd$confidence, "degraded")
})

test_that("a stream with no dataset records yields a zero-row frame", {
  ds <- parse_analyzer_records('{"rec":"summary","package":"p","version":"1.0"}')$datasets
  expect_equal(nrow(ds), 0L)
  expect_true("content_fp" %in% names(ds))
})

test_that(".empty_datasets_df matches the stamped dataset row shape", {
  empty <- .empty_datasets_df()
  expect_equal(nrow(empty), 0L)
  expect_true(all(c("package", "version") == names(empty)[1:2]))
})

test_that(".append_detail_table creates bioc_datasets with the expected indexes", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))

  ds <- parse_analyzer_records(c(
    '{"rec":"summary"}',
    '{"rec":"dataset","name":"d","file":"data/d.rda","internal":false,"format":"rda","class":"data.frame","kind":"data.frame","nrow":3,"ncol":2,"content_fp":"zzz","confidence":"exact"}'
  ))$datasets
  rows <- cbind(package = "p", version = "1.0", ds, stringsAsFactors = FALSE)

  .append_detail_table(con, "bioc_datasets", rows)

  expect_true("bioc_datasets" %in% DBI::dbListTables(con))
  idx <- DBI::dbGetQuery(con,
    "SELECT name FROM sqlite_master WHERE type='index' AND tbl_name='bioc_datasets'")$name
  expect_true("idx_bioc_datasets_pkg_ver" %in% idx)
  expect_true("idx_bioc_datasets_pkg" %in% idx)

  got <- DBI::dbGetQuery(con,
    "SELECT package, version, name, nrow, ncol, content_fp FROM bioc_datasets")
  expect_equal(nrow(got), 1L)
  expect_equal(got$package, "p")
  expect_equal(got$nrow, 3L)
  expect_equal(got$content_fp, "zzz")
})
