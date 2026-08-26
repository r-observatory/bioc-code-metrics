# tests/testthat/test-dataset-coverage.R: the coverage canary over the dataset
# tables.
#
# The dataset column specs are held to what the analyzer emits by the contract
# test, and that test needs a fixture package rich enough to reach every shape.
# A field the fixture never provokes is a field the contract cannot check, and
# the column that field feeds then ships as public data holding nothing for
# anybody. That is exactly how the two truncation markers were dropped for as
# long as they were, and it is not a failure the contract test can catch on its
# own: it measures the reader, and this measures the corpus.
#
# So the same question metric_coverage asks of bioc_code_summary, asked of the
# three dataset tables against the release itself, every run.

# One per-version dataset row, the way analyze.R hands it to the writer. Kept
# here rather than shared with test-datasets.R because each test file is
# sourced into an environment of its own.
.mk_cov_row <- function(package = "p", version = "1.0", content_fp = "C1") {
  data.frame(
    package = package, version = version,
    is_current = 1L, fp_algo_version = 3L,
    name = "d", file = "data/d.rda", internal = 0L,
    format = "rda", format_version = 2L, compression = "gzip",
    class = "data.frame", kind = "data.frame", nrow = 3L, ncol = 2L,
    length = NA_integer_, n_cols = 2L, n_missing_total = 0L,
    schema_fp = "S1", shape_fp = "SH", content_fp = content_fp,
    s4_package = NA_character_, confidence = "exact", notes = NA_character_,
    columns = '[{"name":"a","type":"integer"}]', row_sketch = '["0001","0002"]',
    stringsAsFactors = FALSE
  )
}

.cov_con <- function(rows) {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  if (!is.null(rows)) {
    DBI::dbWithTransaction(con, .write_datasets_normalized(con, rows, unique(rows$package)))
  } else {
    .ensure_dataset_tables(con)
  }
  con
}

test_that("dataset_column_coverage counts, per declared column, the rows that carry a value", {
  row <- .mk_cov_row()
  row$mean <- 2.5
  con <- .cov_con(row)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  cov <- dataset_column_coverage(con)
  expect_s3_class(cov, "data.frame")
  expect_identical(names(cov), c("table", "column", "n_rows", "measured"))

  pick <- function(tbl, col) cov[cov$table == tbl & cov$column == col, ]
  # class is written for this row, mean is written for this row, and the ~140
  # other content columns are not.
  expect_equal(pick("bioc_dataset_contents", "class")$measured, 1L)
  expect_equal(pick("bioc_dataset_contents", "mean")$measured, 1L)
  expect_equal(pick("bioc_dataset_contents", "density")$measured, 0L)
  expect_equal(pick("bioc_dataset_contents", "class")$n_rows, 1L)

  # All three dataset tables are covered, not just the wide one.
  expect_setequal(unique(cov$table),
                  c("bioc_dataset_contents", "bioc_dataset_versions", "bioc_datasets"))
})

test_that("dataset_column_coverage reports nothing for tables that are not there", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  cov <- dataset_column_coverage(con)
  expect_equal(nrow(cov), 0L)
  expect_identical(names(cov), c("table", "column", "n_rows", "measured"))
})

test_that("dataset_coverage_alerts names a column that is empty for the whole corpus", {
  row <- .mk_cov_row()
  con <- .cov_con(row)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  alerts <- dataset_coverage_alerts(dataset_column_coverage(con))
  expect_true(any(grepl("bioc_dataset_contents.density", alerts, fixed = TRUE)))
  # A column this row does fill is not an alert.
  expect_false(any(grepl("bioc_dataset_contents.class", alerts, fixed = TRUE)))
  # And the message says how much of the corpus it looked at, because a column
  # empty across one row means nothing and across a million means a great deal.
  expect_true(any(grepl("1 row", alerts)))
})

test_that("dataset_coverage_alerts stays quiet on an empty table", {
  con <- .cov_con(NULL)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  # Nothing has been written, so every column is empty and none of that is a
  # finding. A first shard must not open with 150 alerts.
  expect_identical(dataset_coverage_alerts(dataset_column_coverage(con)), character(0L))
})

test_that("the marker on a list the reader cut short is one of the columns watched", {
  # The defect this canary exists for, in the form it actually took. The
  # analyzer emits levels_truncated and the writer used to drop it, so the
  # column would be declared and empty in every release; the canary is what
  # says so out loud rather than letting it read as an honest NA.
  row <- .mk_cov_row()
  row$levels_truncated <- TRUE
  con <- .cov_con(row)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  cov <- dataset_column_coverage(con)
  pick <- function(col) cov[cov$table == "bioc_dataset_contents" & cov$column == col, ]
  expect_equal(pick("levels_truncated")$measured, 1L)
  # Its sibling is watched too, and this row does not carry it, so it is named.
  expect_true(any(grepl("bioc_dataset_contents.level_counts_truncated",
                        dataset_coverage_alerts(cov), fixed = TRUE)))
})

test_that("build_manifest carries the count of columns nobody fills", {
  row <- .mk_cov_row()
  con <- .cov_con(row)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  cov <- dataset_column_coverage(con)
  m <- build_manifest(
    con, series = "data", repo = "r/x", db_filename = "d.db", db_bytes = 1,
    tables = c("bioc_datasets"), fp_table = "bioc_datasets",
    fp_cols = c("package", "name"), pkg_table = "bioc_datasets",
    ver_table = "bioc_dataset_versions", stat_table = "bioc_dataset_contents",
    stat_cols = c("nrow"),
    bootstrap = list(n_analyzed = 1L, n_universe = 1L, n_remaining = 0L,
                     bootstrap_complete = TRUE),
    coverage = cov)

  expect_true(m$coverage$n_all_null > 0L)
  expect_equal(m$coverage$n_columns, nrow(cov))
  expect_true("bioc_dataset_contents.density" %in% m$coverage$all_null)
  # The list of names is capped: the count is the number that matters and the
  # names are there to start the search, not to be the search.
  expect_lte(length(m$coverage$all_null), 20L)
})

test_that("build_manifest leaves the coverage block out when nobody measured it", {
  con <- .cov_con(.mk_cov_row())
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  m <- build_manifest(
    con, series = "code", repo = "r/x", db_filename = "d.db", db_bytes = 1,
    tables = c("bioc_datasets"), fp_table = "bioc_datasets",
    fp_cols = c("package", "name"), pkg_table = "bioc_datasets",
    ver_table = "bioc_dataset_versions", stat_table = "bioc_dataset_contents",
    stat_cols = c("nrow"),
    bootstrap = list(n_analyzed = 1L, n_universe = 1L, n_remaining = 0L,
                     bootstrap_complete = TRUE))
  expect_null(m$coverage)
})

test_that("a run publishes what its dataset columns hold", {
  # End to end rather than against build_manifest directly: the block is worth
  # nothing if the run never computes it and never hands it over.
  skip_on_os("windows")
  out <- withr::local_tempdir()
  io <- list(
    package_list = function() data.frame(package = "pkgA", latest_version = "1.0",
                                         stringsAsFactors = FALSE),
    clone = function(pkg, dest) {
      dir.create(file.path(dest, "R"), recursive = TRUE, showWarnings = FALSE)
      writeLines(c("Package: pkgA", "Version: 1.0", "Title: T",
                   "Description: D.", "Author: A", "Maintainer: A <a@e.com>",
                   "License: MIT"), file.path(dest, "DESCRIPTION"))
      writeLines("hello <- function() 1", file.path(dest, "R", "hello.R"))
      TRUE
    })
  suppressWarnings(run_update(io, out, shard_size = 10L))

  dm <- jsonlite::fromJSON(file.path(out, "data-manifest.json"))
  cm <- jsonlite::fromJSON(file.path(out, "code-manifest.json"))
  # How many columns were looked at is reported whether or not any of them is
  # empty, so a release where the block is absent means the run did not look.
  expect_true(dm$coverage$n_columns > 100L)
  expect_false(is.null(dm$coverage$n_all_null))
  # The code series has no dataset columns to measure, so it says nothing
  # rather than saying zero.
  expect_null(cm$coverage)
})
