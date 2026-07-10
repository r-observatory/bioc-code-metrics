# tests/testthat/test-manifest.R
test_that("build_manifest reports measured counts, stats and fingerprint", {
  db <- withr::local_tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), db); on.exit(DBI::dbDisconnect(con))
  DBI::dbWriteTable(con, "bioc_code_summary", data.frame(
    package = c("a","a","b"), version = c("1.0","1.1","2.0"),
    loc_r = c(10L, 20L, 30L), n_fns_r = c(1L, 2L, 3L), stringsAsFactors = FALSE))
  DBI::dbWriteTable(con, "bioc_api_history", data.frame(
    package = "a", version = "1.0", stringsAsFactors = FALSE))

  m <- build_manifest(
    con, series = "code", repo = "r-observatory/bioc-code-metrics",
    db_filename = "bioc-code-metrics.db", db_bytes = 4096L,
    tables = c("bioc_code_summary", "bioc_api_history", "bioc_functions"),
    fp_table = "bioc_code_summary", fp_cols = c("package", "version"),
    pkg_table = "bioc_code_summary", ver_table = "bioc_code_summary",
    stat_table = "bioc_code_summary", stat_cols = c("loc_r", "n_fns_r"),
    bootstrap = list(n_analyzed = 2L, n_universe = 5L, n_remaining = 3L,
                     bootstrap_complete = FALSE))

  expect_identical(m$schema_version, 1L)
  expect_identical(m$series, "code")
  expect_identical(m$n_packages, 2L)
  expect_identical(m$n_versions, 3L)
  expect_identical(m$tables[["bioc_code_summary"]], 3L)
  expect_identical(m$tables[["bioc_functions"]], 0L)      # absent table -> 0
  expect_equal(m$stats[["loc_r_mean"]], 20)
  expect_equal(m$stats[["loc_r_median"]], 20)
  expect_true(grepl("^[0-9a-f]{64}$", m$fingerprint))

  # (d) bootstrap n_universe/n_remaining pass through when supplied.
  expect_identical(m$bootstrap$n_universe, 5L)
  expect_identical(m$bootstrap$n_remaining, 3L)
})

test_that("build_manifest emits null stats for absent columns", {
  db <- withr::local_tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), db); on.exit(DBI::dbDisconnect(con))
  DBI::dbWriteTable(con, "bioc_code_summary", data.frame(
    package = "a", version = "1.0", stringsAsFactors = FALSE))
  m <- build_manifest(
    con, series = "code", repo = "r", db_filename = "x.db", db_bytes = 1L,
    tables = "bioc_code_summary", fp_table = "bioc_code_summary",
    fp_cols = c("package","version"), pkg_table = "bioc_code_summary",
    ver_table = "bioc_code_summary", stat_table = "bioc_code_summary",
    stat_cols = c("loc_r"), bootstrap = list(n_analyzed = 1L, n_universe = NULL,
      n_remaining = NULL, bootstrap_complete = FALSE))
  expect_null(m$stats[["loc_r_mean"]])
  expect_null(m$stats[["loc_r_median"]])

  # (d) n_universe/n_remaining stay NULL (not coerced to 0) when passed NULL.
  expect_null(m$bootstrap$n_universe)
  expect_null(m$bootstrap$n_remaining)
})

test_that("code-series fingerprint matches db_fingerprint() for identical data", {
  db <- withr::local_tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), db); on.exit(DBI::dbDisconnect(con))
  # Include mixed-case package names: R's default locale-aware sort() and
  # SQLite's byte-order ORDER BY disagree on these, which is what exposed
  # the original separator/ordering bug.
  DBI::dbWriteTable(con, "bioc_code_summary", data.frame(
    package = c("a", "a", "b", "zeta", "Apple"),
    version = c("1.0", "1.1", "2.0", "9.9", "0.1"),
    loc_r = c(10L, 20L, 30L, 5L, 7L), stringsAsFactors = FALSE))

  m <- build_manifest(
    con, series = "code", repo = "r", db_filename = "x.db", db_bytes = 1L,
    tables = "bioc_code_summary", fp_table = "bioc_code_summary",
    fp_cols = c("package", "version"), pkg_table = "bioc_code_summary",
    ver_table = "bioc_code_summary", stat_table = "bioc_code_summary",
    stat_cols = character(0),
    bootstrap = list(n_analyzed = 5L, n_universe = NULL, n_remaining = NULL,
                     bootstrap_complete = FALSE))

  expect_true(grepl("^[0-9a-f]{64}$", m$fingerprint))
  expect_identical(nchar(m$fingerprint), 64L)
  expect_identical(m$fingerprint, db_fingerprint(con))
})

test_that("code-series fingerprint matches db_fingerprint() when a package name is a prefix of another", {
  db <- withr::local_tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), db); on.exit(DBI::dbDisconnect(con))
  # "Rcpp" is a prefix of "Rcpp11", followed by a digit (0x31), which sorts
  # below ':' (0x3a). Sorting the already-concatenated "package:version"
  # strings therefore puts "Rcpp11:2.0" before "Rcpp:1.0", while the correct
  # tuple order (matching db_fingerprint()'s SQL ORDER BY) puts "Rcpp" first.
  # Rows are inserted out of sorted order on purpose to catch any reliance on
  # insertion order.
  DBI::dbWriteTable(con, "bioc_code_summary", data.frame(
    package = c("zzz", "Rcpp11", "Rcpp"),
    version = c("1.0", "2.0", "1.0"),
    stringsAsFactors = FALSE))

  m <- build_manifest(
    con, series = "code", repo = "r", db_filename = "x.db", db_bytes = 1L,
    tables = "bioc_code_summary", fp_table = "bioc_code_summary",
    fp_cols = c("package", "version"), pkg_table = "bioc_code_summary",
    ver_table = "bioc_code_summary", stat_table = "bioc_code_summary",
    stat_cols = character(0),
    bootstrap = list(n_analyzed = 3L, n_universe = NULL, n_remaining = NULL,
                     bootstrap_complete = FALSE))

  expect_true(grepl("^[0-9a-f]{64}$", m$fingerprint))
  expect_identical(nchar(m$fingerprint), 64L)
  expect_identical(m$fingerprint, db_fingerprint(con))
})

test_that("data-series fingerprint matches when a package name is a prefix of another", {
  db <- withr::local_tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), db); on.exit(DBI::dbDisconnect(con))
  .ensure_dataset_tables(con)
  # bioc_datasets rows for two packages where one name is a prefix of the
  # other ("bioc11pkg" contains "bioc1pkg" as a prefix, followed by '1', which
  # sorts below ':'/other separators). Inserted out of sorted order so the
  # tuple-ORDER-BY fingerprint cannot rely on insertion order.
  DBI::dbExecute(con,
    "INSERT INTO bioc_dataset_contents
       (content_id, content_fp, schema_fp, fp_algo_version, class, kind, nrow, ncol)
     VALUES (1, 'cf1', 'sf1', 1, 'data.frame', 'table', 10, 2),
            (2, 'cf2', 'sf2', 1, 'data.frame', 'table', 20, 3)")
  DBI::dbExecute(con,
    "INSERT INTO bioc_datasets (package, name, file, internal, current_version, current_content_id)
     VALUES ('bioc11pkg', 'd', 'data/d.rda', 0, '1.0', 2),
            ('bioc1pkg',  'd', 'data/d.rda', 0, '1.0', 1)")

  m <- build_manifest(
    con, series = "data", repo = "r", db_filename = "x.db", db_bytes = 1L,
    tables = "bioc_datasets", fp_table = "bioc_datasets",
    fp_cols = c("package", "name", "current_content_id"),
    pkg_table = "bioc_datasets", ver_table = "bioc_datasets",
    stat_table = "bioc_dataset_contents", stat_cols = c("nrow", "ncol"),
    bootstrap = list(n_analyzed = 2L, n_universe = NULL, n_remaining = NULL,
                     bootstrap_complete = FALSE))

  expect_true(grepl("^[0-9a-f]{64}$", m$fingerprint))
  expect_identical(nchar(m$fingerprint), 64L)

  # Recompute the expected fingerprint the same way db_fingerprint() would for
  # this key set, using SQL BINARY tuple ORDER BY (not R's locale sort()).
  df <- DBI::dbGetQuery(con,
    "SELECT package, name, current_content_id FROM bioc_datasets ORDER BY package, name, current_content_id")
  keys <- apply(df, 1L, function(r) paste(r, collapse = "\x1f"))
  expected_fp <- digest::digest(paste(keys, collapse = ","), algo = "sha256", serialize = FALSE)
  expect_identical(m$fingerprint, expected_fp)
})

test_that("db_bytes survives values >= 2^31 without integer overflow", {
  db <- withr::local_tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), db); on.exit(DBI::dbDisconnect(con))
  DBI::dbWriteTable(con, "bioc_code_summary", data.frame(
    package = "a", version = "1.0", stringsAsFactors = FALSE))

  m <- build_manifest(
    con, series = "code", repo = "r", db_filename = "x.db",
    db_bytes = 3000000000, tables = "bioc_code_summary",
    fp_table = "bioc_code_summary", fp_cols = c("package", "version"),
    pkg_table = "bioc_code_summary", ver_table = "bioc_code_summary",
    stat_table = "bioc_code_summary", stat_cols = character(0),
    bootstrap = list(n_analyzed = 1L, n_universe = NULL, n_remaining = NULL,
                     bootstrap_complete = FALSE))

  expect_false(is.na(m$db_bytes))
  expect_true(m$db_bytes == 3000000000)

  out <- withr::local_tempfile(fileext = ".json")
  write_manifest(out, m)
  json_text <- paste(readLines(out), collapse = "\n")
  expect_true(grepl("3000000000", json_text, fixed = TRUE))
  expect_false(grepl("3e+09", json_text, fixed = TRUE))
  expect_false(grepl('"3000000000"', json_text, fixed = TRUE))
})

test_that("a non-numeric column in stat_cols yields null mean and median, never a fabricated number", {
  db <- withr::local_tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), db); on.exit(DBI::dbDisconnect(con))
  DBI::dbWriteTable(con, "bioc_code_summary", data.frame(
    package = c("a", "b"), version = c("1.0", "2.0"),
    label = c("10", "20"), stringsAsFactors = FALSE))

  m <- build_manifest(
    con, series = "code", repo = "r", db_filename = "x.db", db_bytes = 1L,
    tables = "bioc_code_summary", fp_table = "bioc_code_summary",
    fp_cols = c("package", "version"), pkg_table = "bioc_code_summary",
    ver_table = "bioc_code_summary", stat_table = "bioc_code_summary",
    stat_cols = c("label"),
    bootstrap = list(n_analyzed = 2L, n_universe = NULL, n_remaining = NULL,
                     bootstrap_complete = FALSE))

  expect_null(m$stats[["label_mean"]])
  expect_null(m$stats[["label_median"]])
})

test_that("an empty but present fp_table yields a stable 64-hex fingerprint without error", {
  db <- withr::local_tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), db); on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con,
    "CREATE TABLE bioc_code_summary (package TEXT, version TEXT)")

  m <- build_manifest(
    con, series = "code", repo = "r", db_filename = "x.db", db_bytes = 1L,
    tables = "bioc_code_summary", fp_table = "bioc_code_summary",
    fp_cols = c("package", "version"), pkg_table = "bioc_code_summary",
    ver_table = "bioc_code_summary", stat_table = "bioc_code_summary",
    stat_cols = character(0),
    bootstrap = list(n_analyzed = 0L, n_universe = NULL, n_remaining = NULL,
                     bootstrap_complete = FALSE))

  expect_true(grepl("^[0-9a-f]{64}$", m$fingerprint))
  expect_identical(m$fingerprint, digest::digest("", algo = "sha256", serialize = FALSE))
  expect_identical(m$fingerprint, db_fingerprint(con))
})
