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

test_that(".write_datasets_normalized migrates away from the legacy flat table", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))

  # Legacy flat schema (pre-normalization): one row per dataset per version, no
  # current_version column. A database that still holds it must be migrated, not
  # appended to, or the identity write fails with "no column named current_version".
  DBI::dbExecute(con, "CREATE TABLE bioc_datasets
    (package TEXT, version TEXT, name TEXT, file TEXT, internal INTEGER,
     columns TEXT, row_sketch TEXT)")
  DBI::dbExecute(con, "INSERT INTO bioc_datasets (package, name, version)
                       VALUES ('old', 'd', '0.9')")
  # Summary carries the datasets_scanned marker set under the old design. In a
  # real shard upsert_shard has already written the current package's summary
  # (marker set) before the dataset write, so pre-set p as scanned here too.
  DBI::dbExecute(con, "CREATE TABLE bioc_code_summary
    (package TEXT, version TEXT, datasets_scanned INTEGER)")
  DBI::dbExecute(con, "INSERT INTO bioc_code_summary VALUES ('old','0.9',1), ('p','1.0',1)")

  DBI::dbWithTransaction(con, .write_datasets_normalized(con, .mk_ds_row("p", "1.0", TRUE, "C1"), "p"))

  # Flat table replaced by the normalized identity + link tables.
  expect_true("current_version" %in% DBI::dbListFields(con, "bioc_datasets"))
  expect_true(all(c("bioc_dataset_versions", "bioc_dataset_contents") %in% DBI::dbListTables(con)))
  expect_equal(
    DBI::dbGetQuery(con, "SELECT current_version FROM bioc_datasets WHERE package='p'")$current_version,
    "1.0")
  # A package scanned only under the old design is un-marked so it re-scans.
  expect_true(is.na(
    DBI::dbGetQuery(con, "SELECT datasets_scanned FROM bioc_code_summary WHERE package='old'")$datasets_scanned))
  # The current shard's freshly written marker is preserved.
  expect_equal(
    DBI::dbGetQuery(con, "SELECT datasets_scanned FROM bioc_code_summary WHERE package='p'")$datasets_scanned,
    1L)
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

# --- carrying what a newer analyzer describes --------------------------------
# A scan of the whole archive is expensive, and every one of these is a way for
# it to cost that and change nothing in the database.

.mk_wide_row <- function(package = "p", version = "1.0", content_fp = "C1",
                         origin_dir = "data", name = "d") {
  row <- .mk_ds_row(package, version, TRUE, content_fp, name = name)
  row$fp_algo_version <- 3L
  # Fields the analyzer describes that the tables have never seen.
  row$matrix_shape  <- "symmetric"
  row$matrix_uplo   <- "L"
  row$density       <- 0.125
  row$n_stored      <- 3L
  row$object_system <- "S4"
  row$is_spatial    <- TRUE
  row$origin_dir    <- origin_dir
  row
}

test_that("fields a newer analyzer describes reach the contents table", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbWithTransaction(con, .write_datasets_normalized(con, .mk_wide_row(), "p"))

  got <- DBI::dbGetQuery(con, "SELECT * FROM bioc_dataset_contents")
  expect_equal(got$matrix_shape, "symmetric")
  expect_equal(got$matrix_uplo, "L")
  expect_equal(got$density, 0.125)
  expect_equal(got$n_stored, 3L)
  expect_equal(got$object_system, "S4")
  expect_equal(got$is_spatial, 1L)         # logicals store as integers
})

test_that("how many elements a vector holds survives the write", {
  # A vector has no rows and no columns, so length is the only size it has. The
  # CREATE dropped the column, which left every vector row in the catalog with
  # nothing at all recorded about how big it was.
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  row <- .mk_wide_row()
  row$class  <- "numeric"
  row$kind   <- "vector"
  row$nrow   <- NA_integer_
  row$ncol   <- NA_integer_
  row$length <- 4085L
  DBI::dbWithTransaction(con, .write_datasets_normalized(con, row, "p"))

  expect_equal(DBI::dbGetQuery(con, "SELECT length FROM bioc_dataset_contents")$length, 4085L)
})

test_that("where a dataset was found is identity, not content", {
  # origin_dir differs between two files holding the same bytes, so putting it
  # on the content row would give them two rows and break the dedup.
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbWithTransaction(con, .write_datasets_normalized(con, .mk_wide_row(), "p"))
  DBI::dbWithTransaction(con, .write_datasets_normalized(
    con, .mk_wide_row(package = "q", origin_dir = "extdata"), "q"))

  expect_equal(DBI::dbGetQuery(con, "SELECT count(*) n FROM bioc_dataset_contents")$n, 1L)
  expect_false("origin_dir" %in% DBI::dbListFields(con, "bioc_dataset_contents"))
  ids <- DBI::dbGetQuery(con, "SELECT package, origin_dir FROM bioc_datasets ORDER BY package")
  expect_equal(ids$origin_dir, c("data", "extdata"))
})

test_that("a table created before these fields existed is widened, not skipped", {
  # The incremental path runs against a database downloaded from the last
  # release, so a widened CREATE never applies to it. Without an ALTER the new
  # columns are dropped in silence and the scan that produced them is wasted.
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "CREATE TABLE bioc_dataset_contents (
    content_id INTEGER PRIMARY KEY,
    content_fp TEXT NOT NULL, schema_fp TEXT NOT NULL, fp_algo_version INTEGER NOT NULL,
    class TEXT, kind TEXT, nrow INTEGER, ncol INTEGER, n_missing_total INTEGER, columns TEXT,
    UNIQUE (content_fp, schema_fp, fp_algo_version))")
  expect_false("matrix_shape" %in% DBI::dbListFields(con, "bioc_dataset_contents"))

  DBI::dbWithTransaction(con, .write_datasets_normalized(con, .mk_wide_row(), "p"))

  expect_true("matrix_shape" %in% DBI::dbListFields(con, "bioc_dataset_contents"))
  expect_equal(DBI::dbGetQuery(con, "SELECT matrix_shape FROM bioc_dataset_contents")$matrix_shape,
               "symmetric")
})

test_that("how a file stores its data is recorded, not just what it holds", {
  # R's serialization format has versions, and a version 3 file cannot be read
  # by R before 3.5.0, so this is the difference between a dataset a reader can
  # open and one they cannot. It was being parsed and then dropped, along with
  # the on-disk size and the note saying how the file was read.
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  row <- .mk_wide_row()
  row$format_version   <- 3L
  row$compressed_bytes <- 4096L
  row$notes            <- "s4-dim-slot"
  row$shape_fp         <- "SHP1"
  DBI::dbWithTransaction(con, .write_datasets_normalized(con, row, "p"))

  v <- DBI::dbGetQuery(con, "SELECT format_version, compressed_bytes, notes FROM bioc_dataset_versions")
  expect_equal(v$format_version, 3L)
  expect_equal(v$compressed_bytes, 4096L)
  expect_equal(v$notes, "s4-dim-slot")
  # The shape fingerprint describes the data, so it sits with the data.
  expect_equal(DBI::dbGetQuery(con, "SELECT shape_fp FROM bioc_dataset_contents")$shape_fp, "SHP1")
  expect_false("format_version" %in% DBI::dbListFields(con, "bioc_dataset_contents"))
})

test_that("two versions of one dataset can differ in how they were stored", {
  # The same data saved twice under different serialization versions is one
  # content row and two version rows, so this has to live on the version.
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  a <- .mk_ds_row("p", "1.0", FALSE, "C1"); a$format_version <- 2L
  b <- .mk_ds_row("p", "1.1", TRUE,  "C1"); b$format_version <- 3L
  DBI::dbWithTransaction(con, .write_datasets_normalized(con, rbind(a, b), "p"))

  expect_equal(DBI::dbGetQuery(con, "SELECT count(*) n FROM bioc_dataset_contents")$n, 1L)
  got <- DBI::dbGetQuery(con,
    "SELECT version, format_version FROM bioc_dataset_versions ORDER BY version")
  expect_equal(got$format_version, c(2L, 3L))
})

test_that("what the analyzer describes reaches the tables that hold it", {
  # Every one of these was read, carried through the frame, and then dropped at
  # the write because the column list had not heard of it.
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  row <- .mk_wide_row()
  row$mean <- 2.5; row$sd <- 1.25; row$q1 <- 1.5; row$q3 <- 3.5
  row$sort_order <- "ascending"; row$n_zero <- 0L; row$p_zero <- 0
  row$levels <- '["a","b"]'; row$is_ordered <- TRUE
  row$frame_class <- "tibble"; row$dt_key <- '["id"]'
  row$inner_nrow_total <- 2000L; row$element_names <- '["train","test"]'
  row$dimnames <- '[{"margin":1,"labels":["A","B"]}]'
  row$index_delta <- 1; row$index_regular <- TRUE; row$ts_span <- 3.5
  row$resolution <- "[0.5,0.5]"; row$nodata_value <- -9999; row$in_memory <- TRUE
  row$n_nonzero <- 6L; row$skewness <- 1.5; row$n_outliers <- 2L
  row$title <- "Readings from an instrument"
  DBI::dbWithTransaction(con, .write_datasets_normalized(con, row, "p"))

  got <- DBI::dbGetQuery(con, "SELECT * FROM bioc_dataset_contents")
  expect_equal(got$mean, 2.5)
  expect_equal(got$sd, 1.25)
  expect_equal(got$sort_order, "ascending")
  expect_equal(got$inner_nrow_total, 2000L)
  expect_equal(got$n_nonzero, 6L)
  expect_equal(got$nodata_value, -9999)
  expect_equal(got$skewness, 1.5)
  expect_true(all(c("levels", "dimnames", "dt_key", "element_names", "resolution",
                    "index_delta", "ts_span", "n_outliers", "frame_class")
                  %in% names(got)))

  # A title belongs to the package's documentation, not to the bytes: two
  # packages carrying identical data may describe it differently.
  ident <- DBI::dbGetQuery(con, "SELECT title FROM bioc_datasets")
  expect_equal(ident$title, "Readings from an instrument")
  expect_false("title" %in% DBI::dbListFields(con, "bioc_dataset_contents"))
})

test_that("versions describing different things still bind into one frame", {
  # .datasets_frame carries the fields its records actually had, so two versions
  # of one package differ in width as soon as they differ in what they hold.
  # Plain rbind stops on that, and the caller reads the error as the whole
  # package failing: it loses its summary, functions and edges too, and five
  # consecutive failures exclude it from the pipeline for good.
  v1 <- .datasets_frame(list(list(rec = "dataset", name = "s", class = "S4:X", nrow = 1L)))
  v2 <- .datasets_frame(list(list(rec = "dataset", name = "d", class = "data.frame",
                                  nrow = 3L, has_rownames = TRUE)))
  expect_false(ncol(v1) == ncol(v2))
  bound <- .rbind_datasets(list(v1, v2))
  expect_equal(nrow(bound), 2L)
  expect_true("has_rownames" %in% names(bound))
  expect_true(is.na(bound$has_rownames[bound$name == "s"]))
  expect_null(.rbind_datasets(list()))
})

test_that("a record's fields are carried through the frame rather than a fixed list", {
  # The fixed list silently dropped every field added since it was written, so a
  # richer scan cost its own runtime and changed nothing in the database.
  ds <- parse_analyzer_records(c(
    '{"rec":"summary","package":"p","version":"1.0"}',
    paste0('{"rec":"dataset","name":"m","content_fp":"C1","class":"dgCMatrix",',
           '"kind":"sparse_matrix","density":0.125,"n_stored":3,"matrix_uplo":"L",',
           '"object_system":"S4","is_spatial":false,"origin_dir":"data",',
           '"title":"A sparse thing"}')
  ))$datasets

  expect_equal(ds$density, 0.125)
  expect_equal(ds$n_stored, 3L)
  expect_equal(ds$matrix_uplo, "L")
  expect_equal(ds$object_system, "S4")
  expect_false(ds$is_spatial)
  expect_equal(ds$origin_dir, "data")
  expect_equal(ds$title, "A sparse thing")
  # The base shape downstream code addresses by name is still there in full.
  expect_true(all(names(.DATASET_BASE_COLS) %in% names(ds)))
})

test_that("a padded frame writes without carrying its padding into the wrong type", {
  # Padding fills an absent column with a logical NA, and most of the columns it
  # can land in are declared TEXT or REAL. Writing that must leave a NULL rather
  # than fixing the column's type or failing the whole shard.
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  wide   <- .mk_wide_row(version = "1.1")
  narrow <- .mk_ds_row("p", "1.0", FALSE, "C0")
  narrow$fp_algo_version <- 3L
  df <- .rbind_datasets(list(narrow, wide))
  expect_equal(nrow(df), 2L)
  DBI::dbWithTransaction(con, .write_datasets_normalized(con, df, "p"))

  got <- DBI::dbGetQuery(con,
    "SELECT content_fp, matrix_shape, density FROM bioc_dataset_contents ORDER BY content_fp")
  expect_equal(got$content_fp, c("C0", "C1"))
  expect_true(is.na(got$matrix_shape[got$content_fp == "C0"]))
  expect_equal(got$matrix_shape[got$content_fp == "C1"], "symmetric")
  expect_equal(got$density[got$content_fp == "C1"], 0.125)
  # The identity row still comes from the current version, padding or not.
  expect_equal(DBI::dbGetQuery(con, "SELECT origin_dir FROM bioc_datasets")$origin_dir, "data")
})

# --- noticing that a scan is out of date -------------------------------------

.mk_summary_tbl <- function(con, rows) {
  DBI::dbWriteTable(con, "bioc_code_summary", rows)
}

test_that("an analyzer upgrade puts the packages it already scanned back in the queue", {
  # The marker records that a package was scanned, not what scanned it, so
  # without this every package looks done after an upgrade and nothing re-runs.
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  .mk_summary_tbl(con, data.frame(
    package = c("current", "older", "unknown"),
    datasets_scanned = c(TRUE, TRUE, TRUE),
    analyzer_version = c("0.4.0", "0.2.0", NA_character_),
    stringsAsFactors = FALSE))

  n <- .invalidate_stale_dataset_scans(con, "0.4.0")
  expect_equal(n, 2L)
  got <- DBI::dbGetQuery(con,
    "SELECT package, datasets_scanned FROM bioc_code_summary ORDER BY package")
  # Only the row produced by the running build keeps its marker.
  expect_equal(got$package[!is.na(got$datasets_scanned)], "current")
})

test_that("nothing is invalidated when the running version cannot be determined", {
  # Clearing on a guess would re-scan the archive every run and never settle.
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  .mk_summary_tbl(con, data.frame(
    package = "p", datasets_scanned = TRUE, analyzer_version = "0.2.0",
    stringsAsFactors = FALSE))

  expect_equal(.invalidate_stale_dataset_scans(con, NA_character_), 0L)
  expect_equal(.invalidate_stale_dataset_scans(con, ""), 0L)
  expect_true(DBI::dbGetQuery(con, "SELECT datasets_scanned FROM bioc_code_summary")[[1]][[1]] == 1L)
})

test_that("rows from before the version was recorded are all invalidated once", {
  # Nothing on them says which build produced them, so none can be shown to
  # match. The column appears on this run's write, so the branch is taken once.
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  .mk_summary_tbl(con, data.frame(
    package = c("a", "b"), datasets_scanned = c(TRUE, NA),
    stringsAsFactors = FALSE))

  expect_equal(.invalidate_stale_dataset_scans(con, "0.4.0"), 1L)  # only the marked one
  left <- DBI::dbGetQuery(con, "SELECT datasets_scanned FROM bioc_code_summary")[[1]]
  expect_true(all(is.na(left)))
})

test_that("the re-scan queue settles instead of clearing every marker forever", {
  # If the version column never appears, the column-absent branch fires on every
  # run: the whole archive is queued, the shard truncates to its alphabetical
  # prefix, and packages later in the alphabet are never reached again.
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  .mk_summary_tbl(con, data.frame(
    package = c("a", "b"), datasets_scanned = c(TRUE, TRUE),
    stringsAsFactors = FALSE))

  # First run: nothing records which build produced these, so both are queued.
  expect_equal(.invalidate_stale_dataset_scans(con, "0.4.0"), 2L)
  # That run re-analyses them, and the write leaves the version behind.
  DBI::dbExecute(con, "ALTER TABLE bioc_code_summary ADD COLUMN analyzer_version TEXT")
  DBI::dbExecute(con, "UPDATE bioc_code_summary SET datasets_scanned = 1, analyzer_version = '0.4.0'")
  # Every run after that clears nothing.
  expect_equal(.invalidate_stale_dataset_scans(con, "0.4.0"), 0L)
  expect_equal(.invalidate_stale_dataset_scans(con, "0.4.0"), 0L)
  expect_equal(.invalidate_stale_dataset_scans(con, "0.4.0"), 0L)
})

test_that("a re-scan under a new generation is stored beside the profile it supersedes", {
  # bioc_dataset_contents is keyed on (content_fp, schema_fp, fp_algo_version)
  # and written with INSERT OR IGNORE. Data whose bytes have not changed keeps
  # its content_fp, so a richer profile of it only reaches the table when the
  # generation moves; on the old generation the write is silently dropped and
  # the new fields are computed and thrown away.
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))

  old <- .mk_ds_row("p", "1.0", TRUE, "C1")
  old$fp_algo_version <- 1L
  DBI::dbWithTransaction(con, .write_datasets_normalized(con, old, "p"))

  new <- .mk_wide_row(package = "q", content_fp = "C1")
  new$fp_algo_version <- FP_ALGO_VERSION
  DBI::dbWithTransaction(con, .write_datasets_normalized(con, new, "q"))

  got <- DBI::dbGetQuery(con, "SELECT fp_algo_version, matrix_shape
                             FROM bioc_dataset_contents ORDER BY fp_algo_version")
  expect_equal(nrow(got), 2L)
  expect_equal(got$fp_algo_version, c(1L, FP_ALGO_VERSION))
  # The generation-1 row never held these; the new one does.
  expect_true(is.na(got$matrix_shape[[1L]]))
  expect_equal(got$matrix_shape[[2L]], "symmetric")
})

# Build a real one-release git repo so analyze_package walks a version of it.
# Bioconductor versions are RELEASE_X_Y branches (list_versions ignores tags),
# so RELEASE_1_0 is version "1.0".
.make_one_version_repo <- function(repo) {
  dir.create(repo)
  system2("git", c("init", repo), stdout = FALSE, stderr = FALSE)
  system2("git", c("-C", repo, "config", "user.email", "t@t.test"),
          stdout = FALSE, stderr = FALSE)
  system2("git", c("-C", repo, "config", "user.name", "T"),
          stdout = FALSE, stderr = FALSE)
  writeLines("# readme", file.path(repo, "README"))
  system2("git", c("-C", repo, "add", "."), stdout = FALSE, stderr = FALSE)
  system2("git", c("-C", repo, "commit", "-m", "init"), stdout = FALSE, stderr = FALSE)

  system2("git", c("-C", repo, "checkout", "-b", "RELEASE_1_0"),
          stdout = FALSE, stderr = FALSE)
  dir.create(file.path(repo, "R"), showWarnings = FALSE)
  writeLines("foo <- function() 1", file.path(repo, "R", "foo.R"))
  writeLines("Package: mypkg\nVersion: 1.0\n", file.path(repo, "DESCRIPTION"))
  writeLines("export(foo)\n", file.path(repo, "NAMESPACE"))
  system2("git", c("-C", repo, "add", "."), stdout = FALSE, stderr = FALSE)
  system2("git", c("-C", repo, "commit", "-m", "release-1.0"),
          stdout = FALSE, stderr = FALSE)
  system2("git", c("-C", repo, "checkout", "-"), stdout = FALSE, stderr = FALSE)
}

# A stub analyzer that reports a version of its own and otherwise prints the
# fixture, so a run can be told apart from a run by a different build.
.write_versioned_stub <- function(dir, ndjson_lines, version) {
  fixture <- file.path(dir, "fixture.ndjson")
  writeLines(ndjson_lines, fixture)
  stub <- file.path(dir, "stub-analyzer.sh")
  writeLines(c(
    "#!/bin/sh",
    'if [ "$1" = "--version" ]; then',
    sprintf('  echo "rpkg-analyzer %s"', version),
    "  exit 0",
    "fi",
    sprintf("cat %s", shQuote(fixture))), stub)
  Sys.chmod(stub, mode = "0755")
  stub
}

test_that("a scanned row records which build scanned it and which generation it used", {
  # Without both stamps the row cannot be told from one an older build produced,
  # and a re-scan of unchanged bytes is dropped as a duplicate.
  skip_on_os("windows")
  stub_dir <- tempfile("bcm_stub_")
  dir.create(stub_dir)
  on.exit(unlink(stub_dir, recursive = TRUE), add = TRUE)
  stub <- .write_versioned_stub(stub_dir, c(
    '{"rec":"summary","package":"mypkg","n_fns_r":1}',
    paste0('{"rec":"dataset","name":"d","file":"data/d.rda","internal":false,',
           '"format":"rda","class":"data.frame","kind":"data.frame","nrow":3,',
           '"ncol":2,"schema_fp":"S1","content_fp":"C1"}')
  ), "0.4.0-test")
  withr::local_envvar(RPKG_ANALYZER_BIN = stub)

  repo <- tempfile("bcm_ds_repo_")
  on.exit(unlink(repo, recursive = TRUE), add = TRUE)
  .make_one_version_repo(repo)

  result <- analyze_package(repo, "mypkg")

  expect_equal(unique(result$summary$analyzer_version), "0.4.0-test")
  expect_equal(unique(result$datasets$fp_algo_version), FP_ALGO_VERSION)
})

# --- the run has to record which build scanned its rows -----------------------
# The stale-scan check reads bioc_code_summary.analyzer_version, and clears
# every marker for as long as that column is absent. Nothing guarantees the
# column ever arrives: it reaches the table only when the per-package summary
# happens to carry it, which is a property of what analyze_package returned
# rather than of the run. A run that writes rows without recording the build it
# scanned under leaves the check with nothing to compare, so the clear repeats
# on the next run, and the one after, and the whole universe is queued again
# every time.

.ds_run_io <- function() list(
  package_list = function() data.frame(package = "pkgA", latest_version = "1.0",
                                       stringsAsFactors = FALSE),
  clone = function(pkg, dest) { dir.create(dest, showWarnings = FALSE); TRUE })

# analyze_package without the analyzer version its own binary would have put
# there, which is what any older stored row looks like.
.ds_stub_analyze <- function() {
  env <- environment(run_update)
  old <- get("analyze_package", envir = env)
  assign("analyze_package", function(dest, pkg) list(
    summary = data.frame(package = pkg, version = "1.0", loc_r = 10L, n_fns_r = 1L,
      latest_release_date = "2026-01-01", datasets_scanned = 1L, detail_scanned = 1L,
      stringsAsFactors = FALSE),
    churn = NULL, api = NULL, functions = NULL, edges = NULL, datasets = NULL),
    envir = env)
  old
}

test_that("a run records the analyzer build on rows that arrived without one", {
  skip_on_os("windows")
  stub_dir <- tempfile("bcm_stub_")
  dir.create(stub_dir)
  on.exit(unlink(stub_dir, recursive = TRUE), add = TRUE)
  withr::local_envvar(
    RPKG_ANALYZER_BIN = .write_versioned_stub(stub_dir, character(0L), "0.4.0-test"))

  old <- .ds_stub_analyze()
  on.exit(assign("analyze_package", old, envir = environment(run_update)), add = TRUE)

  out <- withr::local_tempdir()
  run_update(.ds_run_io(), out, shard_size = 10L)

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, DB_FILENAME))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_true("analyzer_version" %in% DBI::dbListFields(con, "bioc_code_summary"))
  expect_equal(
    DBI::dbGetQuery(con, "SELECT analyzer_version FROM bioc_code_summary")[[1L]],
    "0.4.0-test")
})

test_that("the scan marker survives a second run over the same universe", {
  skip_on_os("windows")
  stub_dir <- tempfile("bcm_stub_")
  dir.create(stub_dir)
  on.exit(unlink(stub_dir, recursive = TRUE), add = TRUE)
  withr::local_envvar(
    RPKG_ANALYZER_BIN = .write_versioned_stub(stub_dir, character(0L), "0.4.0-test"))

  old <- .ds_stub_analyze()
  on.exit(assign("analyze_package", old, envir = environment(run_update)), add = TRUE)

  out <- withr::local_tempdir()
  io  <- .ds_run_io()
  run_update(io, out, shard_size = 10L)
  m2 <- run_update(io, out, shard_size = 10L)

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, DB_FILENAME))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_equal(
    DBI::dbGetQuery(con, "SELECT datasets_scanned FROM bioc_code_summary")[[1L]], 1L)
  expect_equal(m2$n_fresh, 0L)
  expect_false(m2$changed)
})
