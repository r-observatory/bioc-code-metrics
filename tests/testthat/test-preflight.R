# tests/testthat/test-preflight.R: the check that stands between a lost
# download and a republished empty database.
#
# The pipeline is incremental: the released database IS the accumulated state.
# A run that starts without it rebuilds from nothing and publishes that as
# latest, green. These tests cover the R half; the workflow half is asserted
# against the yaml at the bottom of the file.

# A code database with `n` packages in it, one version row each.
.pf_code_db <- function(path, n) {
  con <- open_or_init_db(path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  if (n > 0L) {
    df <- data.frame(package = sprintf("pkg%02d", seq_len(n)),
                     version = "1.0", stringsAsFactors = FALSE)
    DBI::dbWriteTable(con, "bioc_code_summary", df, append = TRUE)
  }
  path
}

.pf_manifest <- function(series = "code", n_packages = 4, n_versions = 4) {
  list(schema_version = 1L, series = series, n_packages = n_packages,
       n_versions = n_versions)
}

test_that("a first run has nothing to check", {
  out <- withr::local_tempdir()
  res <- preflight_prior_dbs(out, character(0L))
  expect_identical(res$violations, character(0L))
  expect_identical(res$notes, character(0L))
})

test_that("an advertised database with nothing in it yet does not stop the run", {
  # A database that holds no rows is what the first run of a cold bootstrap
  # publishes for whichever series its first shard had nothing for, and the
  # release it publishes stays latest. Refusing on the count alone was a state
  # with no way out: run 2 refuses, so does run 3, and force_full is the wipe
  # this check exists to prevent.
  out <- withr::local_tempdir()
  con <- open_or_init_data_db(file.path(out, DATA_DB_FILENAME))
  DBI::dbDisconnect(con)
  res <- preflight_prior_dbs(out, "data")
  expect_identical(res$violations, character(0L))
  expect_identical(res$checked, "data")
})

test_that("an advertised database that never landed stops the run", {
  out <- withr::local_tempdir()
  v <- preflight_prior_dbs(out, "code")$violations
  expect_true(length(v) > 0L)
})

test_that("a database holding what its manifest recorded passes", {
  out <- withr::local_tempdir()
  .pf_code_db(file.path(out, DB_FILENAME), 4L)
  write_manifest(file.path(out, "prev-code-manifest.json"), .pf_manifest())
  res <- preflight_prior_dbs(out, "code")
  expect_identical(res$violations, character(0L))
  expect_identical(res$notes, character(0L))
})

test_that("a database holding less than its manifest recorded stops the run", {
  out <- withr::local_tempdir()
  .pf_code_db(file.path(out, DB_FILENAME), 2L)
  write_manifest(file.path(out, "prev-code-manifest.json"), .pf_manifest())
  v <- preflight_prior_dbs(out, "code")$violations
  expect_true(length(v) > 0L)
  expect_true(any(grepl("bioc_code_summary", v, fixed = TRUE)))
})

test_that("a database ahead of its manifest is a note, not a refusal", {
  # publish_metrics uploads four assets in one `gh release upload --clobber`,
  # which cannot be atomic, so an interrupted publish leaves one shard's
  # database beside an earlier shard's manifest. Refusing on that would make a
  # transient upload failure permanent: the same release stays latest tomorrow.
  out <- withr::local_tempdir()
  .pf_code_db(file.path(out, DB_FILENAME), 9L)
  write_manifest(file.path(out, "prev-code-manifest.json"), .pf_manifest())
  res <- preflight_prior_dbs(out, "code")
  expect_identical(res$violations, character(0L))
  expect_true(length(res$notes) > 0L)
})

test_that("a manifest describing the other series is refused", {
  out <- withr::local_tempdir()
  .pf_code_db(file.path(out, DB_FILENAME), 4L)
  write_manifest(file.path(out, "prev-code-manifest.json"),
                 .pf_manifest(series = "data"))
  v <- preflight_prior_dbs(out, "code")$violations
  expect_true(length(v) > 0L)
  expect_true(any(grepl("series", v, fixed = TRUE)))
})

test_that("a manifest predating the series field is noted, not refused", {
  out <- withr::local_tempdir()
  .pf_code_db(file.path(out, DB_FILENAME), 4L)
  write_manifest(file.path(out, "prev-code-manifest.json"),
                 list(schema_version = 1L, n_packages = 99, n_versions = 99))
  res <- preflight_prior_dbs(out, "code")
  expect_identical(res$violations, character(0L))
  expect_true(length(res$notes) > 0L)
})

test_that("a manifest that came back without its database stops the run", {
  # publish_metrics uploads the database and the manifest in one
  # `gh release upload --clobber`, which deletes each existing asset before
  # replacing it and cannot do so atomically. An interrupted publish can
  # therefore leave a release advertising code-manifest.json and no
  # bioc-code-metrics.db, in which case the download step has nothing to fetch
  # and hands preflight an empty `expected`. The manifest that DID come back is
  # the evidence that this is not a cold start.
  out <- withr::local_tempdir()
  write_manifest(file.path(out, "prev-code-manifest.json"), .pf_manifest())
  res <- preflight_prior_dbs(out, character(0L))
  expect_true(length(res$violations) > 0L)
  expect_true(any(grepl("bioc-code-metrics.db", res$violations, fixed = TRUE)))
  expect_true(any(grepl("code-manifest.json", res$violations, fixed = TRUE)))
})

test_that("a data manifest that came back without its database stops the run", {
  out <- withr::local_tempdir()
  write_manifest(file.path(out, "prev-data-manifest.json"),
                 .pf_manifest(series = "data"))
  v <- preflight_prior_dbs(out, character(0L))$violations
  expect_true(length(v) > 0L)
  expect_true(any(grepl("bioc-data-metrics.db", v, fixed = TRUE)))
})

test_that("a manifest predating the series field still demands its database", {
  # The series field is what lets the row counts be compared; the FILENAME is
  # what says which database should have come with it. A baseline too old to
  # compare against is still proof that there was a prior release.
  out <- withr::local_tempdir()
  write_manifest(file.path(out, "prev-code-manifest.json"),
                 list(schema_version = 1L, n_packages = 99, n_versions = 99))
  v <- preflight_prior_dbs(out, character(0L))$violations
  expect_true(length(v) > 0L)
  expect_true(any(grepl("bioc-code-metrics.db", v, fixed = TRUE)))
})

test_that("preflight reports which series it actually looked at", {
  # The download step cannot tell the caller whether anything was checked, and
  # the run log has to say "nothing to build on" only when that is true.
  out <- withr::local_tempdir()
  expect_identical(preflight_prior_dbs(out, character(0L))$checked, character(0L))

  .pf_code_db(file.path(out, DB_FILENAME), 4L)
  write_manifest(file.path(out, "prev-code-manifest.json"), .pf_manifest())
  expect_identical(preflight_prior_dbs(out, "code")$checked, "code")
  expect_identical(preflight_prior_dbs(out, character(0L))$checked, "code")
})

test_that("a file that is not a database at all stops the run", {
  # A download that stops partway leaves bytes on disk that SQLite will not
  # open. That is the one thing a file can say for itself about a lost
  # download, and it has to read as a refusal rather than as an R error nobody
  # can act on.
  out <- withr::local_tempdir()
  writeLines("not a database", file.path(out, DB_FILENAME))
  v <- preflight_prior_dbs(out, "code")$violations
  expect_true(length(v) > 0L)
  expect_true(any(grepl("not a readable database", v, fixed = TRUE)))
})

test_that("the data series is checked on its own tables", {
  out <- withr::local_tempdir()
  con <- open_or_init_data_db(file.path(out, DATA_DB_FILENAME))
  DBI::dbExecute(con, "INSERT INTO bioc_dataset_versions
    (package, name, version, content_id) VALUES ('p', 'd', '1.0', 1)")
  DBI::dbDisconnect(con)
  write_manifest(file.path(out, "prev-data-manifest.json"),
                 .pf_manifest(series = "data", n_packages = 0, n_versions = 9))
  v <- preflight_prior_dbs(out, "data")$violations
  expect_true(length(v) > 0L)
  expect_true(any(grepl("bioc_dataset_versions", v, fixed = TRUE)))
})

# ---------------------------------------------------------------------------
# The baseline measured from a database whose manifest never landed
# ---------------------------------------------------------------------------

test_that("a release carrying a database and no manifest gets a baseline measured from it", {
  # publish_metrics uploads four assets in one non-atomic --clobber, so a run
  # that died in that window leaves a release with its database and no
  # manifest. The database is right there and it is the thing worth protecting,
  # so measure it rather than have nothing to check against.
  out <- withr::local_tempdir()
  .pf_code_db(file.path(out, DB_FILENAME), 4L)

  notes <- ensure_prior_baseline(out)
  expect_true(length(notes) > 0L)
  expect_true(any(grepl("code-manifest.json", notes, fixed = TRUE)))

  mpath <- file.path(out, "prev-code-manifest.json")
  expect_true(file.exists(mpath))
  m <- jsonlite::fromJSON(mpath)
  expect_identical(m$series, "code")
  expect_equal(m$n_packages, 4)
  expect_equal(m$n_versions, 4)
  expect_identical(m$measured_from, DB_FILENAME)

  # And the run may then build on it.
  expect_identical(preflight_prior_dbs(out, "code")$violations, character(0L))
})

test_that("a measured baseline is a real floor for the run after it", {
  # The point of measuring is not to make the refusal go away. A database that
  # then comes back holding less than what was measured is still refused.
  out <- withr::local_tempdir()
  .pf_code_db(file.path(out, DB_FILENAME), 9L)
  ensure_prior_baseline(out)

  unlink(file.path(out, DB_FILENAME))
  .pf_code_db(file.path(out, DB_FILENAME), 2L)
  v <- preflight_prior_dbs(out, "code")$violations
  expect_true(length(v) > 0L)
  expect_true(any(grepl("bioc_code_summary", v, fixed = TRUE)))
})

test_that("a published manifest is never replaced by a measured one", {
  # Absence of a record is not evidence that nothing was lost, but a record
  # saying the database used to be bigger is, and the check has to keep seeing
  # it.
  out <- withr::local_tempdir()
  .pf_code_db(file.path(out, DB_FILENAME), 2L)
  write_manifest(file.path(out, "prev-code-manifest.json"), .pf_manifest())

  expect_identical(ensure_prior_baseline(out), character(0L))
  m <- jsonlite::fromJSON(file.path(out, "prev-code-manifest.json"))
  expect_equal(m$n_packages, 4)
  expect_true(length(preflight_prior_dbs(out, "code")$violations) > 0L)
})

test_that("a database with nothing to measure yields no baseline", {
  # An empty database and one that never arrived have to stay
  # indistinguishable here: writing a baseline of zero would publish a floor of
  # zero as though it were a record of what the release held.
  out <- withr::local_tempdir()
  expect_null(derive_baseline_manifest("code", file.path(out, DB_FILENAME)))
  .pf_code_db(file.path(out, DB_FILENAME), 0L)
  expect_null(derive_baseline_manifest("code", file.path(out, DB_FILENAME)))
  expect_identical(ensure_prior_baseline(out), character(0L))
  expect_false(file.exists(file.path(out, "prev-code-manifest.json")))
})

# ---------------------------------------------------------------------------
# The workflow half: the download that must not swallow its failure
# ---------------------------------------------------------------------------

test_that("update.yml fails the run when a prior asset does not arrive", {
  workflow_path <- file.path("..", "..", ".github", "workflows", "update.yml")
  yml <- readLines(workflow_path)
  dl  <- grep("gh release download", yml, value = TRUE, fixed = TRUE)
  expect_true(length(dl) > 0L)
  # Not one of the prior-state fetches may end in `|| true`: that is the line
  # that let a sibling pipeline republish an empty database as latest.
  expect_false(any(grepl("|| true", dl, fixed = TRUE)))
  expect_false(any(grepl("2>/dev/null", dl, fixed = TRUE)))

  y <- paste(yml, collapse = "\n")
  expect_true(grepl("sleep", y, fixed = TRUE))             # retry with backoff
  expect_true(grepl("preflight.R", y, fixed = TRUE))       # row-count gate
  expect_true(grepl("-s \"out/$name\"", y, fixed = TRUE))  # zero length is a failure
})
