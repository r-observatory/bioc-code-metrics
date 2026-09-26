# tests/testthat/test-summary-schema.R: declared column types and retired
# columns on the summary table. Identical in both code-metrics pipelines.

# A shard whose every 0.5.0 column is NA, the shape that used to type text as INTEGER.
.ss_all_na_shard <- function(pkg = "pkgA", version = "1.0") {
  df <- data.frame(package = pkg, version = version, loc_r = 10L,
                   stringsAsFactors = FALSE)
  for (col in names(.SUMMARY_050_COLS)) df[[col]] <- NA
  df
}

.ss_types <- function(con) {
  info <- DBI::dbGetQuery(con, sprintf('PRAGMA table_info("%s")', SUMMARY_TABLE))
  stats::setNames(info$type, info$name)
}

.ss_upsert <- function(con, df, analyzer_version) {
  upsert_shard(con, df, churn_df = .empty_churn(), api_df = .empty_api(),
               analyzer_version = analyzer_version)
}

test_that("a fresh table takes the declared type for every 0.5.0 column", {
  path <- withr::local_tempfile(fileext = ".db")
  con <- open_or_init_db(path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .ss_upsert(con, .ss_all_na_shard(), "0.5.0")
  types <- .ss_types(con)
  expect_identical(types[names(.SUMMARY_050_COLS)], .SUMMARY_050_COLS)
})

test_that("an existing table gains the declared types through the ALTER path", {
  path <- withr::local_tempfile(fileext = ".db")
  con <- open_or_init_db(path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .ss_upsert(con, data.frame(package = "old", version = "0.9", loc_r = 1L,
                             stringsAsFactors = FALSE), "0.4.0")
  .ss_upsert(con, .ss_all_na_shard(), "0.5.0-test")
  types <- .ss_types(con)
  expect_identical(types[names(.SUMMARY_050_COLS)], .SUMMARY_050_COLS)
  expect_equal(DBI::dbGetQuery(con, sprintf(
    'SELECT COUNT(*) n FROM "%s"', SUMMARY_TABLE))$n, 2L)
})

test_that("the whole-database writer declares the same types", {
  path <- withr::local_tempfile(fileext = ".db")
  export_metrics(path, .ss_all_na_shard(), .empty_churn(), .empty_api(),
                 analyzer_version = "0.5.0")
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_identical(.ss_types(con)[names(.SUMMARY_050_COLS)], .SUMMARY_050_COLS)
})

test_that("below 0.5.0 the frame decides the types, as it always has", {
  path <- withr::local_tempfile(fileext = ".db")
  con <- open_or_init_db(path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  df <- data.frame(package = "pkgA", version = "1.0", input_kind = NA,
                   stringsAsFactors = FALSE)
  .ss_upsert(con, df, "0.4.0")
  types <- .ss_types(con)
  expect_identical(types[["input_kind"]], "INTEGER")
  expect_false("news_file" %in% names(types))
})

# An analyzer that answers the self-check for this pipeline's input kind.
.ss_stub_bin <- function(dir, version) {
  stub <- file.path(dir, "stub-analyzer.sh")
  writeLines(c(
    "#!/bin/sh",
    'if [ "$1" = "--version" ]; then',
    sprintf('  echo "rpkg-analyzer %s"', version),
    "  exit 0",
    "fi",
    sprintf('echo "{\\"rec\\":\\"summary\\",\\"input_kind\\":\\"%s\\"}"', ANALYZER_INPUT_KIND)),
    stub)
  Sys.chmod(stub, mode = "0755")
  stub
}

test_that("a run under a 0.5.0 analyzer writes the declared types", {
  skip_on_os("windows")
  withr::local_envvar(RPKG_ANALYZER_BIN = .ss_stub_bin(withr::local_tempdir(), "0.5.0-test"),
                      PREV_CODE_TAG = "", PREV_DATA_TAG = "", PREV_TEXT_TAG = "")
  env <- environment(run_update)
  old <- get("analyze_package", envir = env)
  on.exit(assign("analyze_package", old, envir = env), add = TRUE)
  assign("analyze_package", function(dest, pkg) {
    s <- .ss_all_na_shard(pkg)
    s$analyzer_version <- "0.5.0-test"
    s$latest_release_date <- "2026-01-01"
    s$datasets_scanned <- TRUE
    s$detail_scanned <- TRUE
    list(summary = s,
         api = data.frame(package = pkg, version = "1.0", exports_added = "[]",
                          exports_removed = "[]", n_exports = 1L, stringsAsFactors = FALSE),
         churn = NULL, functions = NULL, edges = NULL, datasets = NULL,
         binary_versions = "1.0")
  }, envir = env)
  io <- list(
    package_list = function() data.frame(package = "pkgA", latest_version = "1.0",
                                         stringsAsFactors = FALSE),
    clone = function(pkg, dest) { dir.create(dest, showWarnings = FALSE); TRUE })

  out <- withr::local_tempdir()
  suppressWarnings(run_update(io, out, shard_size = 10L))
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, DB_FILENAME))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_identical(.ss_types(con)[names(.SUMMARY_050_COLS)], .SUMMARY_050_COLS)
})

# A shard where every package fell back to R carries none of the 0.5.0 keys.
test_that("a 0.5.0 shard that carries none of the new columns still gets them all", {
  bare <- function(pkg) data.frame(package = pkg, version = "1.0", loc_r = 10L,
                                   stringsAsFactors = FALSE)

  created_path <- withr::local_tempfile(fileext = ".db")
  created <- open_or_init_db(created_path)
  on.exit(DBI::dbDisconnect(created), add = TRUE)
  .ss_upsert(created, bare("pkgA"), "0.5.0")
  expect_identical(.ss_types(created)[names(.SUMMARY_050_COLS)], .SUMMARY_050_COLS)

  altered_path <- withr::local_tempfile(fileext = ".db")
  altered <- open_or_init_db(altered_path)
  on.exit(DBI::dbDisconnect(altered), add = TRUE)
  .ss_upsert(altered, bare("old"), "0.4.0")
  .ss_upsert(altered, bare("pkgA"), "0.5.0")
  expect_identical(.ss_types(altered)[names(.SUMMARY_050_COLS)], .SUMMARY_050_COLS)

  exported_path <- withr::local_tempfile(fileext = ".db")
  export_metrics(exported_path, bare("pkgA"), .empty_churn(), .empty_api(),
                 analyzer_version = "0.5.0")
  exported <- DBI::dbConnect(RSQLite::SQLite(), exported_path)
  on.exit(DBI::dbDisconnect(exported), add = TRUE)
  expect_identical(.ss_types(exported)[names(.SUMMARY_050_COLS)], .SUMMARY_050_COLS)
})

.ss_retired_shard <- function(with_retired, pkg = "prova") {
  df <- data.frame(package = pkg, version = "2.3.0",
                   url = "https://example.org/prova", stringsAsFactors = FALSE)
  if (with_retired) {
    df$has_website <- 1L
    df$copyright_holder_declared <- 1L
  }
  df
}

.ss_cols <- function(con) DBI::dbListFields(con, SUMMARY_TABLE)

test_that("a retired column stays under an older analyzer", {
  path <- withr::local_tempfile(fileext = ".db")
  con <- open_or_init_db(path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .ss_upsert(con, .ss_retired_shard(TRUE), "0.4.0")
  .ss_upsert(con, .ss_retired_shard(TRUE), "0.4.0")
  expect_true(all(c("has_website", "copyright_holder_declared") %in% .ss_cols(con)))
  expect_equal(DBI::dbGetQuery(con, sprintf(
    'SELECT has_website FROM "%s"', SUMMARY_TABLE))$has_website, 1L)
})

test_that("a shard with no analyzer at all never drops a filled column", {
  path <- withr::local_tempfile(fileext = ".db")
  con <- open_or_init_db(path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .ss_upsert(con, .ss_retired_shard(TRUE), "0.4.0")
  # The R fallback's frame lacks the column; the run could not name a build.
  .ss_upsert(con, .ss_retired_shard(FALSE, pkg = "other"), NA_character_)
  expect_true("has_website" %in% .ss_cols(con))
  vals <- DBI::dbGetQuery(con, sprintf(
    'SELECT package, has_website FROM "%s" ORDER BY package', SUMMARY_TABLE))
  expect_equal(vals$has_website, c(NA, 1L))
})

test_that("the first 0.5.0 shard drops both retired columns and keeps the rows", {
  path <- withr::local_tempfile(fileext = ".db")
  con <- open_or_init_db(path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .ss_upsert(con, .ss_retired_shard(TRUE, pkg = "kept"), "0.4.0")
  .ss_upsert(con, .ss_retired_shard(FALSE), "0.5.0")
  cols <- .ss_cols(con)
  expect_false(any(c("has_website", "copyright_holder_declared") %in% cols))
  rows <- DBI::dbGetQuery(con, sprintf(
    'SELECT package, url FROM "%s" ORDER BY package', SUMMARY_TABLE))
  expect_equal(rows$package, c("kept", "prova"))
  expect_equal(rows$url, rep("https://example.org/prova", 2L))
})

test_that("a later row carrying a retired column does not add it back", {
  path <- withr::local_tempfile(fileext = ".db")
  con <- open_or_init_db(path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .ss_upsert(con, .ss_retired_shard(TRUE), "0.4.0")
  .ss_upsert(con, .ss_retired_shard(FALSE), "0.5.0")
  .ss_upsert(con, .ss_retired_shard(TRUE, pkg = "late"), "0.5.0")
  expect_false(any(c("has_website", "copyright_holder_declared") %in% .ss_cols(con)))
  expect_no_error(.ss_upsert(con, .ss_retired_shard(TRUE, pkg = "later"), "0.5.0"))
})

test_that("columns mapped to 0.5.0 later are stripped and dropped the same way", {
  retired <- c(.RETIRED_SUMMARY_COLS, dontrun_example_ratio = "0.5.0",
               examples_coverage = "0.5.0", testing_frameworks = "0.5.0",
               n_test_cases = "0.5.0")
  df <- data.frame(package = "p", version = "1.0", dontrun_example_ratio = 0.5,
                   examples_coverage = 1, testing_frameworks = "[]",
                   n_test_cases = 3L, stringsAsFactors = FALSE)
  expect_identical(names(.strip_retired_columns(df, "0.4.0", retired)), names(df))
  expect_identical(names(.strip_retired_columns(df, "0.5.0", retired)), c("package", "version"))

  path <- withr::local_tempfile(fileext = ".db")
  con <- open_or_init_db(path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .ss_upsert(con, df, "0.4.0")
  expect_identical(.drop_retired_columns(con, "0.4.0", retired), character(0L))
  expect_setequal(.drop_retired_columns(con, "0.5.0", retired),
                  c("dontrun_example_ratio", "examples_coverage",
                    "testing_frameworks", "n_test_cases"))
  expect_setequal(.ss_cols(con), c("package", "version"))
})

test_that("a database with no summary table yet is left alone", {
  path <- withr::local_tempfile(fileext = ".db")
  con <- open_or_init_db(path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_identical(.drop_retired_columns(con, "0.5.0"), character(0L))
  expect_identical(.ensure_summary_columns(con, "0.5.0"), character(0L))
})
