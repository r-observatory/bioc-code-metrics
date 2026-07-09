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

# One per-version dataset row, as analyze.R produces it (binary frame + stamps).
.mk_ds_row <- function(package, version, is_current, content_fp,
                       name = "d", schema_fp = "S1", internal = 0L) {
  data.frame(
    package = package, version = version,
    is_current = as.integer(is_current), fp_algo_version = 1L,
    name = name,
    file = if (internal) "R/sysdata.rda" else paste0("data/", name, ".rda"),
    internal = as.integer(internal),
    format = "rda", format_version = 2L, compression = "gzip",
    class = "data.frame", kind = "data.frame", nrow = 3L, ncol = 2L,
    length = NA_integer_, n_cols = 2L, n_missing_total = 0L,
    schema_fp = schema_fp, shape_fp = "SH", content_fp = content_fp,
    s4_package = NA_character_, confidence = "exact", notes = NA_character_,
    columns = '[{"name":"a","type":"integer"}]', row_sketch = '["0001","0002"]',
    stringsAsFactors = FALSE
  )
}

test_that(".write_datasets_normalized splits into four tables and dedups content across versions", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))

  # Two versions of the same dataset (same content_fp).
  df <- rbind(.mk_ds_row("p", "1.0", FALSE, "C1"),
              .mk_ds_row("p", "1.1", TRUE,  "C1"))
  DBI::dbWithTransaction(con, .write_datasets_normalized(con, df, "p"))

  expect_setequal(
    DBI::dbListTables(con),
    c("bioc_datasets", "bioc_dataset_versions", "bioc_dataset_contents", "bioc_dataset_sketches"))
  count <- function(t) DBI::dbGetQuery(con, sprintf("SELECT count(*) n FROM %s", t))$n
  expect_equal(count("bioc_dataset_versions"), 2L)   # one link per version
  expect_equal(count("bioc_dataset_contents"), 1L)   # content deduped across the two versions
  expect_equal(count("bioc_datasets"),         1L)   # one identity row
  expect_equal(count("bioc_dataset_sketches"), 1L)   # one sketch per distinct content
  expect_equal(DBI::dbGetQuery(con, "SELECT current_version FROM bioc_datasets")$current_version, "1.1")
  # both version rows reconstruct to the same content
  cids <- DBI::dbGetQuery(con, "SELECT DISTINCT content_id FROM bioc_dataset_versions")$content_id
  expect_length(cids, 1L)
})

test_that(".write_datasets_normalized collapses a dataset name colliding within one version", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  count <- function(t) DBI::dbGetQuery(con, sprintf("SELECT count(*) n FROM %s", t))$n

  # A single package version can surface one dataset name twice: an exported
  # data/ object and an internal sysdata object of the same name. (package, name,
  # version) is unique in bioc_dataset_versions, so the writer must collapse to
  # one row rather than fail the PK, keeping the exported copy.
  df <- rbind(
    .mk_ds_row("p", "1.0", TRUE, "CE", name = "d", internal = 0L),
    .mk_ds_row("p", "1.0", TRUE, "CI", name = "d", internal = 1L))
  DBI::dbWithTransaction(con, .write_datasets_normalized(con, df, "p"))

  expect_equal(count("bioc_dataset_versions"), 1L)   # collapsed, no PK violation
  expect_equal(count("bioc_datasets"),         1L)
  cid <- DBI::dbGetQuery(con, "SELECT content_id FROM bioc_dataset_versions")$content_id
  fp  <- DBI::dbGetQuery(con,
    sprintf("SELECT content_fp FROM bioc_dataset_contents WHERE content_id = %d", cid))$content_fp
  expect_equal(fp, "CE")                              # exported copy wins
  expect_equal(DBI::dbGetQuery(con, "SELECT internal FROM bioc_datasets")$internal, 0L)
})

test_that("re-analysis is idempotent and content dedups across packages", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  count <- function(t) DBI::dbGetQuery(con, sprintf("SELECT count(*) n FROM %s", t))$n

  # Packages p and q ship the identical dataset (content C1).
  DBI::dbWithTransaction(con, .write_datasets_normalized(
    con, rbind(.mk_ds_row("p", "1.0", TRUE, "C1"), .mk_ds_row("q", "1.0", TRUE, "C1")), c("p", "q")))
  expect_equal(count("bioc_dataset_contents"), 1L)   # shared across packages
  expect_equal(count("bioc_dataset_versions"), 2L)

  # Re-analyze p with the same data: no duplicate version or content rows.
  DBI::dbWithTransaction(con, .write_datasets_normalized(con, .mk_ds_row("p", "1.0", TRUE, "C1"), "p"))
  expect_equal(count("bioc_dataset_versions"), 2L)
  expect_equal(count("bioc_dataset_contents"), 1L)
})

test_that(".gc_dataset_contents reclaims content orphaned by a data change", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  count <- function(t) DBI::dbGetQuery(con, sprintf("SELECT count(*) n FROM %s", t))$n

  DBI::dbWithTransaction(con, .write_datasets_normalized(con, .mk_ds_row("p", "1.0", TRUE, "C1"), "p"))
  # Data changed on re-analysis: new content C2 written, C1 no longer referenced.
  DBI::dbWithTransaction(con, .write_datasets_normalized(con, .mk_ds_row("p", "1.1", TRUE, "C2"), "p"))
  expect_equal(count("bioc_dataset_contents"), 2L)   # C1 orphan + C2

  .gc_dataset_contents(con)
  expect_equal(count("bioc_dataset_contents"), 1L)   # C1 reclaimed
  expect_equal(count("bioc_dataset_sketches"), 1L)   # its sketch reclaimed too
})
