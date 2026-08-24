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

test_that("fields a newer analyzer describes reach the table that holds them", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbWithTransaction(con, .write_datasets_normalized(con, .mk_wide_row(), "p"))

  # Counted off the values, so two datasets sharing this row agree on them.
  got <- DBI::dbGetQuery(con, "SELECT * FROM bioc_dataset_contents")
  expect_equal(got$density, 0.125)
  expect_equal(got$n_stored, 3L)
  expect_equal(got$is_spatial, 1L)         # logicals store as integers

  # Read off the class rather than the cells. The fingerprints cannot tell a
  # symmetric matrix from the general one holding the same numbers, so these
  # sit on the profile only because the key separates two profiles that
  # disagree on them.
  expect_equal(got$matrix_shape, "symmetric")
  expect_equal(got$matrix_uplo, "L")
  expect_equal(got$object_system, "S4")
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
  expect_false("summary_over" %in% DBI::dbListFields(con, "bioc_dataset_contents"))

  DBI::dbWithTransaction(con, .write_datasets_normalized(con, .mk_wide_row(), "p"))

  expect_true("summary_over" %in% DBI::dbListFields(con, "bioc_dataset_contents"))
  expect_equal(DBI::dbGetQuery(con, "SELECT n_stored FROM bioc_dataset_contents")$n_stored, 3L)
})

test_that("a profile already in the table keeps the row the links name", {
  # The published table is narrow and keyed on the fingerprints. Widening it
  # and re-keying it must leave the profiles that are in it addressable: the
  # version links carry content_id and nothing else, so a row that changes id
  # is a link pointing at another dataset's data.
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "CREATE TABLE bioc_dataset_contents (
    content_id INTEGER PRIMARY KEY,
    content_fp TEXT NOT NULL, schema_fp TEXT NOT NULL, fp_algo_version INTEGER NOT NULL,
    class TEXT, kind TEXT, nrow INTEGER, ncol INTEGER, n_missing_total INTEGER, columns TEXT,
    UNIQUE (content_fp, schema_fp, fp_algo_version))")
  DBI::dbExecute(con, "INSERT INTO bioc_dataset_contents
    (content_id, content_fp, schema_fp, fp_algo_version, class, kind, nrow)
    VALUES (7, 'C9', 'S9', 1, 'matrix', 'matrix', 3)")

  DBI::dbWithTransaction(con, .write_datasets_normalized(con, .mk_wide_row(), "p"))

  got <- DBI::dbGetQuery(con,
    "SELECT content_id, class, nrow FROM bioc_dataset_contents WHERE content_fp = 'C9'")
  expect_equal(got$content_id, 7L)
  expect_equal(got$class, "matrix")
  expect_equal(got$nrow, 3L)
  # The new row went in beside it rather than over it.
  expect_equal(DBI::dbGetQuery(con,
    "SELECT COUNT(*) n FROM bioc_dataset_contents")$n, 2L)
  expect_equal(DBI::dbGetQuery(con,
    "SELECT c.class FROM bioc_dataset_versions v
       JOIN bioc_dataset_contents c ON c.content_id = v.content_id
      WHERE v.package = 'p'")$class, "data.frame")
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
  expect_equal(got$skewness, 1.5)
  expect_true("n_outliers" %in% names(got))

  # The rest were read off an attribute or off the class. Neither fingerprint
  # covers them, and they are on the profile all the same, because the key is
  # the digest of the profile and not the fingerprints.
  expect_equal(got$nodata_value, -9999)
  expect_true(all(c("levels", "dimnames", "dt_key", "element_names", "resolution",
                    "index_delta", "ts_span", "frame_class")
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
    "SELECT content_fp, summary_over, density FROM bioc_dataset_contents ORDER BY content_fp")
  expect_equal(got$content_fp, c("C0", "C1"))
  expect_true(is.na(got$summary_over[got$content_fp == "C0"]))
  expect_equal(got$density[got$content_fp == "C1"], 0.125)
  shp <- DBI::dbGetQuery(con,
    "SELECT v.version, c.matrix_shape FROM bioc_dataset_versions v
       JOIN bioc_dataset_contents c ON c.content_id = v.content_id
      ORDER BY v.version")
  expect_true(is.na(shp$matrix_shape[shp$version == "1.0"]))
  expect_equal(shp$matrix_shape[shp$version == "1.1"], "symmetric")
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

  got <- DBI::dbGetQuery(con, "SELECT fp_algo_version, n_stored
                             FROM bioc_dataset_contents ORDER BY fp_algo_version")
  expect_equal(nrow(got), 2L)
  expect_equal(got$fp_algo_version, c(1L, FP_ALGO_VERSION))
  # The generation-1 row never held these; the new one does.
  expect_true(is.na(got$n_stored[[1L]]))
  expect_equal(got$n_stored[[2L]], 3L)
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

# A one-version repo carrying one dataset at each of the analyzer's four column
# depths, and both of the two ways a record can reach structural depth.
#
# `skipped` is two columns of nine million values written as a compact
# sequence, which the reader cannot materialize and cannot hash either, so the
# record carries no fingerprint at all. `tall` is one column of eight million
# and one real values past the cell cap: its cells are never read, but its
# bytes were hashed on the way past, so it carries all three fingerprints taken
# over those digests. The two are the whole difference between a record that
# gets a content row and one that cannot have one, and a fixture holding only
# the first would have said structural means no fingerprint.
#
# `tall` costs half a second to write and 124 KB on disk: it is two values
# repeated, so it compresses to nothing.
.make_depth_repo <- function(repo) {
  .make_one_version_repo(repo)
  system2("git", c("-C", repo, "checkout", "RELEASE_1_0"),
          stdout = FALSE, stderr = FALSE)
  d <- file.path(repo, "data")
  dir.create(d, showWarnings = FALSE)
  set.seed(11L)
  narrow <- data.frame(a = 1:5, b = letters[1:5], stringsAsFactors = FALSE)
  save(narrow, file = file.path(d, "narrow.rda"))
  uniform <- as.data.frame(matrix(stats::runif(5L * 600L), nrow = 5L))
  save(uniform, file = file.path(d, "uniform.rda"))
  mixed <- as.data.frame(matrix(stats::runif(5L * 600L), nrow = 5L))
  mixed$V1 <- 1:5
  save(mixed, file = file.path(d, "mixed.rda"))
  skipped <- data.frame(a = 1:9000000L, b = 1:9000000L)
  save(skipped, file = file.path(d, "skipped.rda"))
  tall <- data.frame(v = rep_len(c(1.5, 2.5), 8000001L))
  save(tall, file = file.path(d, "tall.rda"))
  system2("git", c("-C", repo, "add", "."), stdout = FALSE, stderr = FALSE)
  system2("git", c("-C", repo, "commit", "-m", "data"), stdout = FALSE, stderr = FALSE)
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

# --- the build a row was collected under, on the rows the analyzer produced ---
# The stale-scan check reads bioc_code_summary.analyzer_version, and reads a
# scanned row that names no build as one it cannot show to be current, so a
# scanned row has to name the build that scanned it or the clear repeats on the
# next run, and the one after, and the whole universe is queued again every
# time.
#
# It has to name it only when the analyzer really produced it. Writing the
# running build onto a row the pure-R fallback produced claims a producer that
# produced nothing, which is the same false claim datasets_scanned is withheld
# to avoid, and the two would then disagree about the same row.

.ds_run_io <- function() list(
  package_list = function() data.frame(package = "pkgA", latest_version = "1.0",
                                       stringsAsFactors = FALSE),
  clone = function(pkg, dest) { dir.create(dest, showWarnings = FALSE); TRUE })

# What build the stored rows name, if the column is there to name one at all.
# A database whose every row came from the fallback never grows the column,
# which is the same answer as a column full of NULLs and has to read as one.
.ds_named_builds <- function(con) {
  if (!"analyzer_version" %in% DBI::dbListFields(con, "bioc_code_summary")) {
    return(NA_character_)
  }
  as.character(
    DBI::dbGetQuery(con, "SELECT analyzer_version FROM bioc_code_summary")[[1L]])
}

# A package the analyzer read: the dataset marker and the build that earned it
# arrive together, because the reader that sets one is the producer that names
# the other. This is the shape analyze_package can actually return.
.ds_stub_analyze <- function(version = "0.4.0-test") {
  env <- environment(run_update)
  old <- get("analyze_package", envir = env)
  assign("analyze_package", function(dest, pkg) list(
    summary = data.frame(package = pkg, version = "1.0", loc_r = 10L, n_fns_r = 1L,
      latest_release_date = "2026-01-01", datasets_scanned = TRUE, detail_scanned = TRUE,
      analyzer_version = version, stringsAsFactors = FALSE),
    churn = NULL, api = NULL, functions = NULL, edges = NULL, datasets = NULL,
    binary_versions = "1.0"),
    envir = env)
  old
}

# The other shape: the pure-R fallback ran, so there is no scan and no build.
.ds_stub_fallback <- function() {
  env <- environment(run_update)
  old <- get("analyze_package", envir = env)
  assign("analyze_package", function(dest, pkg) list(
    summary = data.frame(package = pkg, version = "1.0", loc_r = 10L,
      latest_release_date = "2026-01-01", datasets_scanned = NA, detail_scanned = TRUE,
      stringsAsFactors = FALSE),
    churn = NULL, api = NULL, functions = NULL, edges = NULL, datasets = NULL,
    binary_versions = character(0L)),
    envir = env)
  old
}

test_that("the run fills the build in on a row the analyzer produced without one", {
  df <- data.frame(package = c("a", "b"), version = c("1.0", "1.0"),
                   analyzer_version = c(NA_character_, NA_character_),
                   stringsAsFactors = FALSE)
  got <- .stamp_analyzer_version(df, "0.4.0-test", .analyzer_row_keys("a", "1.0"))
  expect_equal(got$analyzer_version, c("0.4.0-test", NA_character_))
})

test_that("a row the analyzer did not produce is left naming nobody", {
  # The pure-R fallback wrote this row. Stamping the running build on it would
  # say the analyzer collected data the analyzer never saw, and the re-scan
  # queue would then read a fallback row as one it has no reason to re-scan.
  df <- data.frame(package = "a", version = "1.0",
                   analyzer_version = NA_character_, stringsAsFactors = FALSE)
  got <- .stamp_analyzer_version(df, "0.4.0-test")
  expect_true(is.na(got$analyzer_version))
  expect_true(is.na(.stamp_analyzer_version(
    df, "0.4.0-test", .analyzer_row_keys("a", character(0L)))$analyzer_version))
})

test_that("a run records the analyzer build on the rows the analyzer produced", {
  skip_on_os("windows")
  stub_dir <- tempfile("bcm_stub_")
  dir.create(stub_dir)
  on.exit(unlink(stub_dir, recursive = TRUE), add = TRUE)
  withr::local_envvar(
    RPKG_ANALYZER_BIN = .write_versioned_stub(stub_dir, character(0L), "0.4.0-test"))

  old <- .ds_stub_analyze(version = NA_character_)
  on.exit(assign("analyze_package", old, envir = environment(run_update)), add = TRUE)

  out <- withr::local_tempdir()
  run_update(.ds_run_io(), out, shard_size = 10L)

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, DB_FILENAME))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
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

test_that("a row the analyzer stamped keeps the build it names", {
  # The run fills a gap; it does not restate what the analyzer already said.
  # Overwriting would erase the one signal that tells a row collected by an
  # older build from one collected by this one.
  df <- data.frame(package = c("a", "b"), version = c("1.0", "1.0"),
                   analyzer_version = c("0.2.0", NA_character_),
                   stringsAsFactors = FALSE)
  got <- .stamp_analyzer_version(df, "0.4.0-test",
                                 .analyzer_row_keys("a", "1.0"))
  expect_equal(got$analyzer_version, c("0.2.0", NA_character_))
})

test_that("a run that cannot name its analyzer stamps nothing", {
  # Writing a guess would make every row look current and stop the re-scan
  # queue from ever noticing an upgrade.
  df <- data.frame(package = "a", version = "1.0",
                   analyzer_version = NA_character_, stringsAsFactors = FALSE)
  expect_true(is.na(.stamp_analyzer_version(
    df, NA_character_, .analyzer_row_keys("a", "1.0"))$analyzer_version))
  expect_false("analyzer_version" %in%
                 names(.stamp_analyzer_version(
                   data.frame(package = "a", stringsAsFactors = FALSE),
                   NA_character_)))
})

test_that("a fallback row reaches the database naming no build at all", {
  skip_on_os("windows")
  stub_dir <- tempfile("bcm_stub_")
  dir.create(stub_dir)
  on.exit(unlink(stub_dir, recursive = TRUE), add = TRUE)
  withr::local_envvar(
    RPKG_ANALYZER_BIN = .write_versioned_stub(stub_dir, character(0L), "0.4.0-test"))

  old <- .ds_stub_fallback()
  on.exit(assign("analyze_package", old, envir = environment(run_update)), add = TRUE)

  out <- withr::local_tempdir()
  run_update(.ds_run_io(), out, shard_size = 10L)

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, DB_FILENAME))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  # Neither half of the claim. A row with no scan and a build against it says
  # the analyzer was here and read nothing, which is the state this pipeline
  # cannot tell from a package that ships no data.
  expect_true(all(is.na(.ds_named_builds(con))))
  expect_true(all(is.na(
    DBI::dbGetQuery(con, "SELECT datasets_scanned FROM bioc_code_summary")[[1L]])))
})

test_that("a package the installed analyzer cannot read names no build either", {
  # The same thing without a stub in the way: a real analyze_package, a real
  # binary that answers --version and fails on the package.
  skip_on_os("windows")
  stub_dir <- tempfile("bcm_stub_")
  dir.create(stub_dir)
  on.exit(unlink(stub_dir, recursive = TRUE), add = TRUE)
  stub <- file.path(stub_dir, "failing-analyzer.sh")
  writeLines(c("#!/bin/sh",
               'if [ "$1" = "--version" ]; then',
               '  echo "rpkg-analyzer 0.4.0-test"',
               "  exit 0",
               "fi",
               "exit 1"), stub)
  Sys.chmod(stub, mode = "0755")
  withr::local_envvar(RPKG_ANALYZER_BIN = stub)

  out  <- withr::local_tempdir()
  repo <- file.path(withr::local_tempdir(), "pkgA_src")
  .make_one_version_repo(repo)
  io <- list(
    package_list = function() data.frame(package = "mypkg", latest_version = "1.0",
                                         stringsAsFactors = FALSE),
    clone = function(pkg, dest) {
      file.copy(repo, dirname(dest), recursive = TRUE)
      file.rename(file.path(dirname(dest), basename(repo)), dest)
      TRUE
    })
  run_update(io, out, shard_size = 10L)

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, DB_FILENAME))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_true(all(is.na(.ds_named_builds(con))))
  expect_true(all(is.na(
    DBI::dbGetQuery(con, "SELECT datasets_scanned FROM bioc_code_summary")[[1L]])))
})

test_that("a columns profile too large to serve is refused, and the row says so", {
  # A single value over MySQL's max_allowed_packet cannot be loaded at all, and
  # that ceiling is not raisable: a misparsed file has already produced a
  # 306 MB profile in the sibling pipeline, whose dataset tables merge into the
  # same database as these. The profile is what gets dropped, never the row.
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  orig <- MAX_DATASET_COLUMNS_BYTES
  MAX_DATASET_COLUMNS_BYTES <<- 64L
  on.exit(MAX_DATASET_COLUMNS_BYTES <<- orig, add = TRUE)

  big <- .mk_ds_row("p", "1.0", TRUE, "C1")
  big$columns <- paste0('[{"name":"', strrep("x", 500L), '"}]')
  DBI::dbWithTransaction(con, .write_datasets_normalized(con, big, "p"))

  got <- DBI::dbGetQuery(con,
    "SELECT nrow, ncol, columns, columns_refused_bytes FROM bioc_dataset_contents")
  expect_equal(nrow(got), 1L)
  expect_true(is.na(got$columns))
  expect_equal(got$columns_refused_bytes, nchar(big$columns, type = "bytes"))
  # The rest of the profile is still true and still stored.
  expect_equal(got$nrow, 3L)
  expect_equal(got$ncol, 2L)
})

test_that("a columns profile within the bound is stored untouched", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  row <- .mk_ds_row("p", "1.0", TRUE, "C1")
  DBI::dbWithTransaction(con, .write_datasets_normalized(con, row, "p"))

  got <- DBI::dbGetQuery(con,
    "SELECT columns, columns_refused_bytes FROM bioc_dataset_contents")
  expect_equal(got$columns, row$columns)
  # Zero rather than NULL: nothing was refused is a measurement, and a column
  # that is NULL for every healthy row reads to a coverage check as dead.
  expect_equal(got$columns_refused_bytes, 0L)
})

test_that("the fields the analyzer reports about empty slots and time zones are stored", {
  # Both are top-level fields of a dataset record and neither had a declared
  # column, so .write_datasets_normalized computed them and dropped them on the
  # way into SQLite.
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  row <- .mk_ds_row("p", "1.0", TRUE, "C1")
  row$n_empty_slots <- 4L
  row$tz <- "Europe/Berlin"
  DBI::dbWithTransaction(con, .write_datasets_normalized(con, row, "p"))

  # Both describe the object, so both sit with the profile. The time zone is an
  # attribute the cells know nothing about, which is why the key has to be the
  # digest of the profile and not the fingerprints over the cells.
  got <- DBI::dbGetQuery(con,
    "SELECT n_empty_slots, tz FROM bioc_dataset_contents")
  expect_equal(got$n_empty_slots, 4L)
  expect_equal(got$tz, "Europe/Berlin")
})

# --- how deep the column profile goes -------------------------------------
# The analyzer stopped truncating a wide object's column list and started
# saying instead how much of one a record carries: full, reduced, none or
# structural. Without a declared column the pipeline computes that and drops it
# on the way into SQLite, and a reader of bioc_dataset_contents cannot tell a
# record with no columns array from one whose columns are all there.

test_that("how deep a record's column profile goes is stored beside the profile", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  df <- rbind(
    .mk_ds_row("p", "1.0", TRUE, "C1", name = "a"),
    .mk_ds_row("p", "1.0", TRUE, "C2", name = "b"),
    .mk_ds_row("p", "1.0", TRUE, "C3", name = "c"))
  df$column_detail <- c("full", "reduced", "none")
  # A record at none depth carries no columns array at all; the whole-object
  # summary stands in its place.
  df$columns[3L] <- NA_character_
  DBI::dbWithTransaction(con, .write_datasets_normalized(con, df, "p"))

  got <- DBI::dbGetQuery(con,
    "SELECT d.name, c.column_detail, c.columns
       FROM bioc_datasets d
       JOIN bioc_dataset_contents c ON c.content_id = d.current_content_id
      ORDER BY d.name")
  expect_equal(got$name, c("a", "b", "c"))
  expect_equal(got$column_detail, c("full", "reduced", "none"))
  expect_true(is.na(got$columns[3L]))
})

test_that("a record the reader could not fingerprint is in the catalog, naming no profile", {
  # An S4 object the reader holds no representation for, a raster packed into
  # bytes, and an .R script under data/ all come back with no fingerprint. They
  # were being dropped whole, so the package did not appear to ship them at
  # all: no identity row, no version link, nothing saying the dataset is there.
  #
  # They get no content row, because that table is addressed by fingerprint and
  # a key invented for a record with none would tell two objects that were
  # never compared that they hold the same data. They get the rest.
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  df <- rbind(
    .mk_ds_row("p", "1.0", TRUE, "C1", name = "kept"),
    .mk_ds_row("p", "1.0", TRUE, NA_character_, name = "packed"),
    .mk_ds_row("p", "1.0", TRUE, NA_character_, name = "script"))
  df$column_detail <- c("full", NA_character_, NA_character_)

  said <- capture.output(
    DBI::dbWithTransaction(con, .write_datasets_normalized(con, df, "p")))

  expect_setequal(DBI::dbGetQuery(con, "SELECT name FROM bioc_datasets")$name,
                  c("kept", "packed", "script"))
  got <- DBI::dbGetQuery(con,
    "SELECT name, content_id, confidence FROM bioc_dataset_versions ORDER BY name")
  expect_equal(got$name, c("kept", "packed", "script"))
  expect_true(is.na(got$content_id[got$name == "packed"]))
  expect_true(is.na(got$content_id[got$name == "script"]))
  expect_false(is.na(got$content_id[got$name == "kept"]))
  # One content row, for the one record that has a fingerprint.
  expect_equal(DBI::dbGetQuery(con, "SELECT count(*) n FROM bioc_dataset_contents")$n, 1L)
  # Said out loud, because a catalog entry with nothing behind it is a coverage
  # figure and a shard where the number climbs is the reader losing objects.
  expect_true(any(grepl("2 datasets", said, fixed = TRUE)))
  expect_true(any(grepl("p packed", said, fixed = TRUE)))
})

test_that("the identity row of an unmeasured dataset names no profile either", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  row <- .mk_ds_row("p", "1.0", TRUE, NA_character_, name = "packed")
  row$class <- "PackedSpatRaster"
  row$kind <- "object"
  DBI::dbWithTransaction(con, .write_datasets_normalized(con, row, "p"))

  idn <- DBI::dbGetQuery(con, "SELECT * FROM bioc_datasets")
  expect_equal(idn$name, "packed")
  expect_true(is.na(idn$current_content_id))
  expect_equal(idn$current_version, "1.0")
  # The link says the package ships it, at which version, and how far the
  # reader got. What kind of thing it is is on the profile, and this record
  # does not have one: everything the reader describes about an object now
  # sits on the profile row, and a record with no fingerprint has no profile
  # row to sit on. The catalog entry is the whole of what is kept.
  got <- DBI::dbGetQuery(con,
    "SELECT package, name, version, content_id, confidence FROM bioc_dataset_versions")
  expect_equal(got$name, "packed")
  expect_equal(got$version, "1.0")
  expect_true(is.na(got$content_id))
  expect_equal(got$confidence, "exact")
  expect_false("class" %in% names(got))
})

test_that("one dataset with no profile does not retire the whole reclaim", {
  # NOT IN over a set holding a NULL is NULL for every row it is asked about,
  # so a single unmeasured dataset anywhere in the table would quietly stop the
  # reclaim from ever deleting anything, with nothing in the log to say so.
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbWithTransaction(con, .write_datasets_normalized(
    con, .mk_ds_row("p", "1.0", TRUE, "C1", name = "d"), "p"))
  DBI::dbWithTransaction(con, .write_datasets_normalized(
    con, .mk_ds_row("q", "1.0", TRUE, NA_character_, name = "e"), "q"))
  # p's data changes, orphaning C1.
  DBI::dbWithTransaction(con, .write_datasets_normalized(
    con, .mk_ds_row("p", "1.0", TRUE, "C2", name = "d"), "p"))

  .gc_dataset_contents(con)
  expect_equal(DBI::dbGetQuery(con,
    "SELECT content_fp FROM bioc_dataset_contents")$content_fp, "C2")
})

test_that("a version table that forbids a missing profile is rebuilt to allow one", {
  # The published table declared content_id NOT NULL, which is what made "the
  # reader took no fingerprint" mean "the dataset leaves the catalog". The
  # constraint cannot be dropped in place.
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "CREATE TABLE bioc_dataset_versions (
    package TEXT NOT NULL, name TEXT NOT NULL, version TEXT NOT NULL,
    content_id INTEGER NOT NULL, format TEXT, compression TEXT, confidence TEXT,
    is_current INTEGER NOT NULL DEFAULT 0,
    notes TEXT,
    PRIMARY KEY (package, name, version))")
  DBI::dbExecute(con, "INSERT INTO bioc_dataset_versions
    (package, name, version, content_id, format, confidence, is_current, notes)
    VALUES ('old', 'd', '1.0', 7, 'rda', 'exact', 1, 'kept')")

  DBI::dbWithTransaction(con, .write_datasets_normalized(
    con, .mk_ds_row("p", "1.0", TRUE, NA_character_, name = "packed"), "p"))

  # Every column the table had picked up, and every row, came across.
  kept <- DBI::dbGetQuery(con,
    "SELECT package, content_id, notes FROM bioc_dataset_versions WHERE package = 'old'")
  expect_equal(kept$content_id, 7L)
  expect_equal(kept$notes, "kept")
  expect_true(is.na(DBI::dbGetQuery(con,
    "SELECT content_id FROM bioc_dataset_versions WHERE package = 'p'")$content_id))
})

test_that("the analyzer's four column depths reach the table, or say why they do not", {
  skip_if(!nzchar(rpkg_analyzer_bin()),
          "no rpkg-analyzer binary: set RPKG_ANALYZER_BIN or put one on PATH")
  skip_on_os("windows")
  repo <- tempfile("bcm_depth_repo_")
  on.exit(unlink(repo, recursive = TRUE), add = TRUE)
  .make_depth_repo(repo)

  result <- analyze_package(repo, "mypkg")
  ds <- result$datasets
  expect_true(!is.null(ds) && nrow(ds) > 0L)
  skip_if(!"column_detail" %in% names(ds),
          sprintf("rpkg-analyzer %s does not declare a column depth",
                  rpkg_analyzer_version()))
  depth <- stats::setNames(ds$column_detail, ds$name)
  expect_equal(unname(depth["narrow"]),  "full")
  expect_equal(unname(depth["uniform"]), "none")
  expect_equal(unname(depth["mixed"]),   "reduced")
  expect_equal(unname(depth["skipped"]), "structural")
  expect_equal(unname(depth["tall"]),    "structural")

  # Structural does not mean unfingerprinted. `tall` was never read for its
  # values and was hashed for its bytes, so it has all three; `skipped` holds a
  # column the reader could neither read nor hash, and one of those takes the
  # whole record's identity with it.
  expect_true(is.na(ds$content_fp[ds$name == "skipped"]))
  expect_true(all(!is.na(ds$content_fp[ds$name != "skipped"])))
  expect_false(is.na(ds$schema_fp[ds$name == "tall"]))
  expect_false(is.na(ds$shape_fp[ds$name == "tall"]))
  expect_true(is.na(ds$row_sketch[ds$name == "tall"]))

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWithTransaction(con, .write_datasets_normalized(con, ds, "mypkg"))
  got <- DBI::dbGetQuery(con,
    "SELECT d.name, c.column_detail, c.ncol
       FROM bioc_datasets d
       JOIN bioc_dataset_contents c ON c.content_id = d.current_content_id
      ORDER BY d.name")
  expect_setequal(got$name, c("mixed", "narrow", "tall", "uniform"))
  expect_equal(got$column_detail[got$name == "uniform"], "none")
  expect_equal(got$column_detail[got$name == "tall"], "structural")
  # ncol is the true width at every depth, whatever the columns array holds.
  expect_equal(got$ncol[got$name == "uniform"], 600L)
  expect_equal(got$ncol[got$name == "tall"], 1L)

  # And the one record with no fingerprint is in the catalog too, naming no
  # profile rather than being absent from it.
  all_ds <- DBI::dbGetQuery(con,
    "SELECT name, content_id FROM bioc_dataset_versions ORDER BY name")
  expect_setequal(all_ds$name, c("mixed", "narrow", "skipped", "tall", "uniform"))
  expect_true(is.na(all_ds$content_id[all_ds$name == "skipped"]))
})

# --- what the fingerprint covers, and what it cannot -----------------------
# The content-addressed row is shared by every record with the same
# (content_fp, schema_fp, fp_algo_version), and the shard writes it with
# `df[!duplicated(ck), ]` after a sort on (package, name, version, internal).
# So whichever record sorts first supplies every column on that row, and for a
# column the fingerprint does not cover that is one dataset publishing another
# dataset's answer. content_fp is blake3 over, per column in order, the type
# string and the cell bytes; schema_fp is over name:type. Nothing read off an
# attribute or off the class vector goes into either.

test_that("two datasets holding the same instants keep their own time zones", {
  # The cells of a POSIXct are seconds since the epoch, so the same moments
  # written in two zones share content_fp and schema_fp. One profile row each,
  # both naming the same fingerprint, and each with its own zone.
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  a <- .mk_ds_row("aaa", "1.0", TRUE, "C1")
  b <- .mk_ds_row("zzz", "1.0", TRUE, "C1")
  a$tz <- "UTC"
  b$tz <- "America/Chicago"
  DBI::dbWithTransaction(
    con, .write_datasets_normalized(con, rbind(a, b), c("aaa", "zzz")))

  got <- DBI::dbGetQuery(con,
    "SELECT v.package, c.tz, c.content_fp FROM bioc_dataset_versions v
       JOIN bioc_dataset_contents c ON c.content_id = v.content_id
      ORDER BY v.package")
  expect_equal(got$package, c("aaa", "zzz"))
  expect_equal(got$tz, c("UTC", "America/Chicago"))
  expect_equal(got$content_fp, c("C1", "C1"))
})

test_that("no dataset field is declared on two tables at once", {
  expect_equal(intersect(names(.DATASET_CONTENT_COLS),
                         names(.DATASET_VERSION_COLS)), character(0L))
  expect_equal(intersect(names(.DATASET_CONTENT_COLS),
                         names(.DATASET_IDENTITY_COLS)), character(0L))
  expect_equal(intersect(names(.DATASET_VERSION_COLS),
                         names(.DATASET_IDENTITY_COLS)), character(0L))
})

test_that("two profiles that differ inside the columns array get a row each", {
  # The residual no relocation could reach. `columns` is the profile payload
  # and the reason this table exists, so it cannot move to the version link,
  # and the fingerprints do not cover the per-column time zone, label, comment,
  # units or declared levels that ride inside it. Two frames of the same
  # instants written in two zones share content_fp and schema_fp and differ
  # only here, and the row published one of them under both.
  utc <- paste0('[{"name":"t","type":"POSIXct","tz":"UTC",',
                '"attrs_other":[{"name":"tzone","values":["UTC"]}]}]')
  chi <- paste0('[{"name":"t","type":"POSIXct","tz":"America/Chicago",',
                '"attrs_other":[{"name":"tzone","values":["America/Chicago"]}]}]')
  for (rev in c(FALSE, TRUE)) {
    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    a <- .mk_ds_row("aaa", "1.0", TRUE, "C1")
    b <- .mk_ds_row("zzz", "1.0", TRUE, "C1")
    a$columns <- utc
    b$columns <- chi
    rows <- if (rev) rbind(b, a) else rbind(a, b)
    DBI::dbWithTransaction(
      con, .write_datasets_normalized(con, rows, c("aaa", "zzz")))

    got <- DBI::dbGetQuery(con,
      "SELECT v.package, c.columns
         FROM bioc_dataset_versions v
         JOIN bioc_dataset_contents c ON c.content_id = v.content_id
        ORDER BY v.package")
    expect_equal(got$package, c("aaa", "zzz"), info = sprintf("reversed = %s", rev))
    expect_equal(got$columns, c(utc, chi), info = sprintf("reversed = %s", rev))
    DBI::dbDisconnect(con)
  }
})

test_that("the same data in two packages is still one fingerprint", {
  # content_fp is the user-facing signal, and splitting the row must not split
  # it: the two rows below hold the same bytes profiled two ways, and anything
  # grouping on content_fp still sees one dataset shipped twice.
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  a <- .mk_ds_row("aaa", "1.0", TRUE, "C1")
  b <- .mk_ds_row("zzz", "1.0", TRUE, "C1")
  a$columns <- '[{"name":"t","tz":"UTC"}]'
  b$columns <- '[{"name":"t","tz":"America/Chicago"}]'
  DBI::dbWithTransaction(
    con, .write_datasets_normalized(con, rbind(a, b), c("aaa", "zzz")))

  got <- DBI::dbGetQuery(con,
    "SELECT COUNT(*) rows, COUNT(DISTINCT content_fp) fps
       FROM bioc_dataset_contents")
  expect_equal(got$rows, 2L)
  expect_equal(got$fps, 1L)
})

test_that("one profile written twice is still one row", {
  # The dedup is the point of the table and the digest must not weaken it: two
  # packages shipping byte-identical data profiled identically share the row.
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  a <- .mk_ds_row("aaa", "1.0", TRUE, "C1")
  b <- .mk_ds_row("zzz", "1.0", TRUE, "C1")
  DBI::dbWithTransaction(
    con, .write_datasets_normalized(con, rbind(a, b), c("aaa", "zzz")))

  expect_equal(DBI::dbGetQuery(con,
    "SELECT COUNT(*) n FROM bioc_dataset_contents")$n, 1L)
  expect_equal(DBI::dbGetQuery(con,
    "SELECT COUNT(DISTINCT content_id) n FROM bioc_dataset_versions")$n, 1L)
})

test_that("a profile digest stands over every field the row records", {
  # The digest is what makes the row unique, so it has to be on the row, it has
  # to be declared NOT NULL, and it has to be the uniqueness key. A key still
  # naming only the fingerprints is the defect back again.
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  a <- .mk_ds_row("aaa", "1.0", TRUE, "C1")
  DBI::dbWithTransaction(con, .write_datasets_normalized(con, a, "aaa"))

  expect_true("profile_fp" %in% DBI::dbListFields(con, "bioc_dataset_contents"))
  fp <- DBI::dbGetQuery(con, "SELECT profile_fp FROM bioc_dataset_contents")$profile_fp
  expect_equal(nchar(fp), 64L)

  sql <- DBI::dbGetQuery(con, "SELECT sql FROM sqlite_master
     WHERE type = 'table' AND name = 'bioc_dataset_contents'")$sql
  expect_true(grepl("profile_fp TEXT NOT NULL", sql, fixed = TRUE))
  expect_true(grepl("UNIQUE (profile_fp, fp_algo_version)", sql, fixed = TRUE))
  expect_false(grepl("UNIQUE (content_fp, schema_fp, fp_algo_version)", sql,
                     fixed = TRUE))
})

test_that("the digest separates values that would otherwise run together", {
  # Field values are folded in with their name and their length, so a pair of
  # rows whose text merely shifts across a field boundary cannot be told they
  # hold the same profile.
  a <- .mk_ds_row("aaa", "1.0", TRUE, "C1")
  b <- .mk_ds_row("zzz", "1.0", TRUE, "C1")
  a$sort_order   <- "ab"
  a$summary_over <- "c"
  b$sort_order   <- "a"
  b$summary_over <- "bc"
  expect_false(identical(.dataset_profile_fp(a), .dataset_profile_fp(b)))
})

test_that("a field the record does not carry does not move the digest", {
  # A column the analyzer has never emitted is absent rather than NA-valued,
  # and declaring one must not re-mint every row in the table.
  a <- .mk_ds_row("aaa", "1.0", TRUE, "C1")
  b <- a
  b$n_vertices <- NA_integer_
  expect_equal(.dataset_profile_fp(a), .dataset_profile_fp(b))
})

test_that("a database keyed by fingerprint alone is re-keyed in place", {
  # The deployed database is keyed (content_fp, schema_fp, fp_algo_version).
  # The incremental path opens that file, so the new key has to arrive by
  # migration or it only ever applies to a database built from nothing.
  path <- tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbExecute(con, "CREATE TABLE bioc_dataset_contents (
    content_id INTEGER PRIMARY KEY,
    content_fp TEXT NOT NULL, schema_fp TEXT NOT NULL, fp_algo_version INTEGER NOT NULL,
    class TEXT, kind TEXT, nrow INTEGER, ncol INTEGER, n_missing_total INTEGER, columns TEXT,
    UNIQUE (content_fp, schema_fp, fp_algo_version))")
  DBI::dbExecute(con, "CREATE INDEX idx_bioc_dsc_schema ON bioc_dataset_contents(schema_fp)")
  DBI::dbExecute(con, "CREATE TABLE bioc_dataset_versions (
    package TEXT NOT NULL, name TEXT NOT NULL, version TEXT NOT NULL,
    content_id INTEGER NOT NULL, format TEXT, compression TEXT, confidence TEXT,
    is_current INTEGER NOT NULL DEFAULT 0,
    PRIMARY KEY (package, name, version))")
  for (i in 1:50) {
    DBI::dbExecute(con,
      "INSERT INTO bioc_dataset_contents
         (content_id, content_fp, schema_fp, fp_algo_version, nrow, columns)
       VALUES (?, ?, 'S1', 1, ?, ?)",
      params = list(i, sprintf("C%03d", i), i, sprintf('[{"name":"c%d"}]', i)))
    DBI::dbExecute(con,
      "INSERT INTO bioc_dataset_versions
         (package, name, version, content_id, is_current)
       VALUES (?, 'd', '1.0', ?, 1)",
      params = list(sprintf("p%03d", i), i))
  }
  DBI::dbDisconnect(con)

  con <- open_or_init_data_db(path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  sql <- DBI::dbGetQuery(con, "SELECT sql FROM sqlite_master
     WHERE type = 'table' AND name = 'bioc_dataset_contents'")$sql
  expect_true(grepl("UNIQUE (profile_fp, fp_algo_version)", sql, fixed = TRUE))
  # Every row and every link comes across, and the ids the links name still
  # point at the same profile.
  expect_equal(DBI::dbGetQuery(con,
    "SELECT COUNT(*) n FROM bioc_dataset_contents")$n, 50L)
  expect_equal(DBI::dbGetQuery(con,
    "SELECT COUNT(*) n FROM bioc_dataset_versions")$n, 50L)
  got <- DBI::dbGetQuery(con,
    "SELECT content_id, content_fp, nrow FROM bioc_dataset_contents ORDER BY content_id")
  expect_equal(got$content_id, 1:50)
  expect_equal(got$content_fp, sprintf("C%03d", 1:50))
  expect_equal(got$nrow, 1:50)
  # Backfilled rather than left NULL: the column is the key and a table of
  # NULLs is a table with no key at all.
  expect_equal(DBI::dbGetQuery(con,
    "SELECT COUNT(*) n FROM bioc_dataset_contents WHERE profile_fp IS NULL")$n, 0L)
  expect_equal(DBI::dbGetQuery(con,
    "SELECT COUNT(DISTINCT profile_fp) n FROM bioc_dataset_contents")$n, 50L)
  # The index the rebuild drops with the table is back.
  expect_true("idx_bioc_dsc_schema" %in% DBI::dbGetQuery(con,
    "SELECT name FROM sqlite_master WHERE type = 'index'")$name)
})

test_that("re-keying a database twice changes nothing the second time", {
  path <- tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbExecute(con, "CREATE TABLE bioc_dataset_contents (
    content_id INTEGER PRIMARY KEY,
    content_fp TEXT NOT NULL, schema_fp TEXT NOT NULL, fp_algo_version INTEGER NOT NULL,
    nrow INTEGER, columns TEXT,
    UNIQUE (content_fp, schema_fp, fp_algo_version))")
  DBI::dbExecute(con, "INSERT INTO bioc_dataset_contents
    (content_id, content_fp, schema_fp, fp_algo_version, nrow)
    VALUES (1, 'C1', 'S1', 1, 3)")
  DBI::dbDisconnect(con)

  con <- open_or_init_data_db(path)
  first <- DBI::dbGetQuery(con, "SELECT * FROM bioc_dataset_contents")
  DBI::dbDisconnect(con)
  con <- open_or_init_data_db(path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_equal(DBI::dbGetQuery(con, "SELECT * FROM bioc_dataset_contents"), first)
})

test_that("the fingerprint the catalog groups on keeps an index", {
  # content_fp led the old uniqueness key and was indexed by it for free. It no
  # longer leads any key, and "the same data ships in N packages" groups on it,
  # which without an index is a scan of every profile in the catalog.
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  .ensure_dataset_tables(con)
  idx <- DBI::dbGetQuery(con,
    "SELECT name FROM sqlite_master
      WHERE type = 'index' AND tbl_name = 'bioc_dataset_contents'")$name
  expect_true("idx_bioc_dsc_content" %in% idx)
  plan <- DBI::dbGetQuery(con,
    "EXPLAIN QUERY PLAN SELECT content_fp, COUNT(*) FROM bioc_dataset_contents
      GROUP BY content_fp")$detail
  expect_true(any(grepl("idx_bioc_dsc_content", plan, fixed = TRUE)))
})

test_that("what the reader says about the data sits with the data", {
  # These were taken off the content row while the row was keyed on the
  # fingerprints, because the fingerprints do not cover them and the row would
  # have published one dataset's answer for another. The key now covers them,
  # so they belong back where they describe what they describe, and where the
  # catalog reads them.
  described <- c(
    "class", "kind", "frame_class", "object_system", "s4_package",
    "has_rownames", "has_dimnames", "dimnames",
    "label", "comment", "units", "attrs_other", "tz",
    "levels", "n_levels", "is_ordered",
    "is_grouped", "group_vars", "n_groups", "is_rowwise",
    "dt_key", "dt_indices",
    "ts_start", "ts_end", "ts_frequency", "frequency", "ts_span",
    "index_start", "index_end", "index_n", "index_class", "index_span",
    "index_tz", "index_delta", "index_regular", "index_n_gaps",
    "index_max_gap", "index_sorted", "index_has_duplicates",
    "crs_input", "crs_epsg", "crs_wkt",
    "matrix_shape", "matrix_storage", "matrix_uplo", "matrix_diag",
    "matrix_value_type",
    "n_layers", "layer_names", "layer_min", "layer_max", "resolution",
    "nodata_value", "in_memory",
    "element_names", "inner_names", "inner_schema_varies")
  expect_equal(setdiff(described, names(.DATASET_CONTENT_COLS)), character(0L))
  expect_equal(intersect(described, names(.DATASET_VERSION_COLS)), character(0L))
})

test_that("the version link keeps only what is true of the file", {
  # A dataset's own row says how one file happened to store it. Everything else
  # describes the data and is on the profile, which is now keyed finely enough
  # to hold it.
  expect_equal(sort(names(.DATASET_VERSION_COLS)),
               sort(c("format_version", "compressed_bytes", "notes",
                      "delimiter_looks_like", "delimiter_would_give_ncol")))
})

test_that("each record keeps its own answer on the row that describes it", {
  # Every field here was measured against the analyzer at c045665 on a pair of
  # objects sharing both fingerprints. The pair is written in both orders,
  # because the defect is invisible in whichever order happens to be right.
  differing <- list(
    class             = c("data.frame", "tbl_df/tbl/data.frame"),
    kind              = c("data.frame", "matrix"),
    frame_class       = c("data.frame", "tbl_df"),
    has_rownames      = c(0L, 1L),
    dimnames          = c(NA_character_, '["r1","r2"]'),
    label             = c(NA_character_, "A label"),
    comment           = c(NA_character_, "a note"),
    levels            = c('["a","b"]', '["a","b","zz"]'),
    n_levels          = c(2L, 3L),
    inner_schema_varies = c(0L, 1L),
    is_ordered        = c(0L, 1L),
    ts_start          = c(2000, 1990),
    index_start       = c("2020-01-01", "1999-01-01"),
    dt_key            = c(NA_character_, "x"),
    group_vars        = c(NA_character_, '["g"]'),
    crs_epsg          = c(4326L, 3857L),
    matrix_value_type = c("logical", "pattern"),
    in_memory         = c(1L, 0L),
    inner_names       = c('["aa"]', '["bb"]'),
    tz                = c("UTC", "America/Chicago"))

  for (rev in c(FALSE, TRUE)) {
    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    a <- .mk_ds_row("aaa", "1.0", TRUE, "C1")
    b <- .mk_ds_row("zzz", "1.0", TRUE, "C1")
    for (k in names(differing)) {
      a[[k]] <- differing[[k]][[1L]]
      b[[k]] <- differing[[k]][[2L]]
    }
    rows <- if (rev) rbind(b, a) else rbind(a, b)
    DBI::dbWithTransaction(
      con, .write_datasets_normalized(con, rows, c("aaa", "zzz")))
    # SELECT *, because SQLite reads a double-quoted name it does not know as a
    # string literal rather than refusing it, and a test that cannot tell a
    # missing column from a present one is not a test.
    got <- DBI::dbGetQuery(con,
      "SELECT v.package, c.* FROM bioc_dataset_versions v
         JOIN bioc_dataset_contents c ON c.content_id = v.content_id
        ORDER BY v.package")
    expect_equal(got$package, c("aaa", "zzz"), info = sprintf("reversed = %s", rev))
    for (k in names(differing)) {
      expect_equal(got[[k]], differing[[k]],
                   info = sprintf("%s, reversed = %s", k, rev))
    }
    DBI::dbDisconnect(con)
  }
})

test_that("a column taken off the profile table is put back, and off the link", {
  # A database written while those fields sat on the version link exists, and
  # opening it must both restore the column the catalog reads and retire the
  # copy on the link. Left there, the link's copy would hold whatever the run
  # that wrote it recorded and would never be updated again, which is the
  # half-filled column nobody can explain.
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "CREATE TABLE bioc_dataset_contents (
    content_id INTEGER PRIMARY KEY, profile_fp TEXT NOT NULL,
    content_fp TEXT NOT NULL, schema_fp TEXT NOT NULL, fp_algo_version INTEGER NOT NULL,
    nrow INTEGER, ncol INTEGER, n_missing_total INTEGER, columns TEXT,
    UNIQUE (profile_fp, fp_algo_version))")
  DBI::dbExecute(con, "CREATE TABLE bioc_dataset_versions (
    package TEXT NOT NULL, name TEXT NOT NULL, version TEXT NOT NULL,
    content_id INTEGER, format TEXT, compression TEXT, confidence TEXT,
    is_current INTEGER NOT NULL DEFAULT 0,
    class TEXT, kind TEXT, tz TEXT, matrix_uplo TEXT, notes TEXT,
    PRIMARY KEY (package, name, version))")
  DBI::dbExecute(con, "INSERT INTO bioc_dataset_versions
    (package, name, version, content_id, is_current, class, tz, notes)
    VALUES ('old', 'd', '0.9', NULL, 1, 'stale', 'stale', 'kept')")

  first <- capture.output(
    DBI::dbWithTransaction(con, .write_datasets_normalized(con, .mk_wide_row(), "p")))
  expect_true(any(grepl("bioc_dataset_versions", first, fixed = TRUE)))

  fields <- DBI::dbListFields(con, "bioc_dataset_versions")
  expect_false("class" %in% fields)
  expect_false("tz" %in% fields)
  expect_false("matrix_uplo" %in% fields)
  # What the link is genuinely the home of stays, and so does its row.
  expect_true("notes" %in% fields)
  expect_equal(DBI::dbGetQuery(con,
    "SELECT notes FROM bioc_dataset_versions WHERE package = 'old'")$notes, "kept")
  expect_true(all(c("class", "kind", "tz", "matrix_uplo")
                  %in% DBI::dbListFields(con, "bioc_dataset_contents")))
  expect_equal(DBI::dbGetQuery(con,
    "SELECT matrix_uplo FROM bioc_dataset_contents")$matrix_uplo, "L")

  # A one-time move, not a daily one.
  again <- capture.output(
    DBI::dbWithTransaction(con, .write_datasets_normalized(
      con, .mk_wide_row(package = "p2"), "p2")))
  expect_false(any(grepl("bioc_dataset_versions", again, fixed = TRUE)))
})

test_that("a profile table the re-key cannot read stops the run rather than losing its key", {
  # The new key column is declared beside content_fp. A table carrying the old
  # key and spelling content_fp some other way would otherwise be rebuilt with
  # no key column at all, which fails later on a name nobody can trace.
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, 'CREATE TABLE bioc_dataset_contents (
    content_id INTEGER PRIMARY KEY,
    "content_fp" TEXT NOT NULL, schema_fp TEXT NOT NULL, fp_algo_version INTEGER NOT NULL,
    UNIQUE (content_fp, schema_fp, fp_algo_version))')
  expect_error(.rekey_dataset_contents(con), "cannot re-key bioc_dataset_contents")
})

test_that("the digest describes the row as it is stored, not as it arrived", {
  # SQLite has no NaN: RSQLite writes one as NULL, so a record holding NaN and
  # a record holding nothing store the same row. Digesting them apart would
  # give one stored profile two keys and two identical rows.
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  a <- .mk_ds_row("aaa", "1.0", TRUE, "C1")
  b <- .mk_ds_row("zzz", "1.0", TRUE, "C1")
  a$mean <- NaN
  b$mean <- NA_real_
  expect_equal(.dataset_profile_fp(a), .dataset_profile_fp(b))

  DBI::dbWithTransaction(
    con, .write_datasets_normalized(con, rbind(a, b), c("aaa", "zzz")))
  expect_equal(DBI::dbGetQuery(con,
    "SELECT COUNT(*) n FROM bioc_dataset_contents")$n, 1L)

  # An infinity is a value SQLite does store, so it stays its own profile.
  c1 <- .mk_ds_row("qqq", "1.0", TRUE, "C1"); c1$mean <- Inf
  expect_false(identical(.dataset_profile_fp(c1), .dataset_profile_fp(b)))
})

test_that("the same text in two encodings is one profile", {
  # A character value reaches the digest as bytes. The same characters marked
  # latin1 and marked UTF-8 are different bytes and the same text, and they
  # store as the same text, so they have to digest alike.
  a <- .mk_ds_row("aaa", "1.0", TRUE, "C1")
  b <- .mk_ds_row("zzz", "1.0", TRUE, "C1")
  a$label <- "café"
  b$label <- iconv("café", "UTF-8", "latin1")
  expect_false(identical(charToRaw(a$label), charToRaw(b$label)))
  expect_equal(.dataset_profile_fp(a), .dataset_profile_fp(b))
})

test_that("the migration that retires a moved column cannot reach a table's own", {
  # It finds what to drop by intersecting the version table's columns with the
  # content spec. The version table's own columns are named one by one by the
  # writer and are not in either spec, so declaring one of them on the content
  # row would make the migration delete it and empty half the catalog.
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  .ensure_dataset_tables(con)
  own <- c("package", "name", "version", "content_id", "format", "compression",
           "confidence", "is_current")
  expect_true(all(own %in% DBI::dbListFields(con, "bioc_dataset_versions")))
  expect_equal(intersect(own, names(.DATASET_CONTENT_COLS)), character(0L))
  # And the identity table's, for the same reason.
  expect_equal(intersect(c("package", "name", "file", "internal",
                           "current_version", "current_content_id"),
                         names(.DATASET_CONTENT_COLS)), character(0L))
})
