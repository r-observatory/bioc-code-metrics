# tests/testthat/test-release-versions.R: the package version of each analysed
# Bioconductor release, kept as a narrow table the merger copies.

.rv_db <- function(frame = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = frame)
  con <- open_or_init_db(file.path(dir, "code.db"))
  withr::defer(DBI::dbDisconnect(con), envir = frame)
  open_or_init_release_text_db(file.path(dir, "code.db"), con = con)
  con
}

.rv_history <- function(con, rows) {
  DBI::dbAppendTable(con, DESCRIPTION_HISTORY_TABLE, rows)
}

.rv_read <- function(con) {
  DBI::dbGetQuery(con, sprintf(
    'SELECT package, version, package_version FROM "%s" ORDER BY package, version',
    RELEASE_VERSIONS_TABLE))
}

test_that("each release gets the Version its DESCRIPTION carried", {
  con <- .rv_db()
  .rv_history(con, data.frame(
    package = c("limma", "limma", "limma", "limma"),
    version = c("3.22", "3.22", "3.23", "3.23"),
    field   = c("Version", "Title", "Version", "Title"),
    value   = c("3.62.2", "Linear Models", "3.64.1", "Linear Models"),
    stringsAsFactors = FALSE))
  expect_equal(refresh_release_versions(con), 2L)
  expect_equal(.rv_read(con), data.frame(
    package = c("limma", "limma"), version = c("3.22", "3.23"),
    package_version = c("3.62.2", "3.64.1"), stringsAsFactors = FALSE))
})

test_that("a release with no Version, or a blank one, gets no row", {
  con <- .rv_db()
  .rv_history(con, data.frame(
    package = c("a", "b", "c"), version = c("3.23", "3.23", "3.23"),
    field = c("Title", "Version", "Version"), value = c("A", "  ", " 1.2.0 "),
    stringsAsFactors = FALSE))
  refresh_release_versions(con)
  expect_equal(.rv_read(con)$package, "c")
  expect_equal(.rv_read(con)$package_version, "1.2.0")
})

test_that("a rebuild matches the history and leaves nothing stale", {
  con <- .rv_db()
  .rv_history(con, data.frame(package = "a", version = "3.22", field = "Version",
                              value = "1.0.0", stringsAsFactors = FALSE))
  refresh_release_versions(con)
  DBI::dbExecute(con, sprintf('DELETE FROM "%s"', DESCRIPTION_HISTORY_TABLE))
  .rv_history(con, data.frame(package = "b", version = "3.23", field = "Version",
                              value = "2.0.0", stringsAsFactors = FALSE))
  refresh_release_versions(con)
  expect_equal(.rv_read(con)$package, "b")
})

test_that("a database with no history yet gets an empty table and no error", {
  dir <- withr::local_tempdir()
  con <- open_or_init_db(file.path(dir, "code.db"))
  withr::defer(DBI::dbDisconnect(con))
  expect_equal(refresh_release_versions(con), 0L)
  expect_true(RELEASE_VERSIONS_TABLE %in% DBI::dbListTables(con))
})

test_that("the update run rebuilds the table after the text write and before the manifest", {
  src <- readLines(file.path("..", "..", "scripts", "update.R"))   # tests run from tests/testthat
  text_at     <- grep("upsert_release_text(text_con", src, fixed = TRUE)[[1L]]
  refresh_at  <- grep("refresh_release_versions(con)", src, fixed = TRUE)
  manifest_at <- grep("# ---- 8. Manifest", src, fixed = TRUE)[[1L]]
  expect_length(refresh_at, 1L)
  expect_true(text_at < refresh_at && refresh_at < manifest_at)
})
