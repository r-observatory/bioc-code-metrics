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
  # A same-day publish replaces four assets one at a time, each uploaded under
  # swap-next-<name> and then renamed into place, so an interrupted publish leaves
  # one shard's database beside an earlier shard's manifest. Refusing on that
  # would make a transient upload failure permanent: the same release stays
  # latest tomorrow.
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
  # A same-day publish replaces the database and the manifest one at a time,
  # each uploaded under swap-next-<name> and then renamed into place. A run
  # stopped between those renames leaves the bytes under swap-prev-<name> and nothing
  # under the name, so the release can advertise code-manifest.json and no
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
  # A same-day publish replaces four assets one at a time, each uploaded under
  # swap-next-<name> and then renamed into place, so a run that died between two of
  # them leaves a release with its database and no manifest. The database is
  # right there and it is the thing worth protecting, so measure it rather than
  # have nothing to check against.
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
# A release that resolved and carried nothing
# ---------------------------------------------------------------------------

test_that("a resolved release carrying neither database nor manifest stops the run", {
  # Keyed on the files alone, a release with no assets at all looks exactly
  # like no release: nothing advertised, nothing downloaded. That is the shape
  # a failed `gh release create` leaves when its uploads and its cleanup both
  # fail, and read as a cold start it would publish one shard as latest. The
  # resolved tag is what says a release was there.
  out <- withr::local_tempdir()
  res <- preflight_prior_dbs(out, character(0L),
                             resolved = c(code = "metrics-2026-09-13",
                                          data = "metrics-2026-09-13"))
  v <- res$violations
  expect_length(v, 2L)
  expect_true(all(grepl("metrics-2026-09-13", v, fixed = TRUE)))
  expect_true(any(grepl("bioc-code-metrics.db", v, fixed = TRUE) &
                  grepl("code-manifest.json", v, fixed = TRUE)))
  expect_true(any(grepl("bioc-data-metrics.db", v, fixed = TRUE) &
                  grepl("data-manifest.json", v, fixed = TRUE)))
  expect_setequal(res$checked, c("code", "data"))
  # The download step resolves published releases only, so the release named
  # here is a published one. Calling it a draft sends the operator to the
  # repair for a release that has no git tag.
  expect_false(any(grepl("draft", v, ignore.case = TRUE)))
})

test_that("a run that resolved no release is still a cold start", {
  out <- withr::local_tempdir()
  res <- preflight_prior_dbs(out, character(0L), resolved = c(code = "", data = ""))
  expect_identical(res$violations, character(0L))
  expect_identical(res$checked, character(0L))
})

test_that("a resolved release holds only the series it resolved for", {
  # The legacy split tags resolve code and data separately, so a code release
  # with no data release beside it is a cold start for the data series only.
  out <- withr::local_tempdir()
  .pf_code_db(file.path(out, DB_FILENAME), 4L)
  write_manifest(file.path(out, "prev-code-manifest.json"), .pf_manifest())
  res <- preflight_prior_dbs(out, "code", resolved = c(code = "code-2026-07-01", data = ""))
  expect_identical(res$violations, character(0L))
  expect_identical(res$checked, "code")
})

test_that("preflight reads the resolved tags off its command line", {
  a <- .pf_parse_args(c("out/", "--code-src=metrics-2026-09-13", "--data-src=", "code"))
  expect_identical(a$out_dir, "out/")
  expect_identical(a$expected, "code")
  expect_identical(a$resolved, c(code = "metrics-2026-09-13", data = ""))

  a <- .pf_parse_args(c("out/", "code", "data"))
  expect_identical(a$expected, c("code", "data"))
  expect_identical(a$resolved, c(code = "", data = ""))
})

test_that("the preflight script refuses a resolved release that carried nothing", {
  skip_on_os("windows")
  out <- withr::local_tempdir()
  script <- normalizePath(file.path("..", "..", "scripts", "preflight.R"))
  run <- function(...) {
    res <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
                                    c(shQuote(script), shQuote(out), ...),
                                    stdout = TRUE, stderr = TRUE))
    list(status = attr(res, "status") %||% 0L, output = paste(res, collapse = "\n"))
  }

  bad <- run("--code-src=metrics-2026-09-13", "--data-src=metrics-2026-09-13")
  expect_false(identical(bad$status, 0L))
  expect_true(grepl("metrics-2026-09-13", bad$output, fixed = TRUE))

  cold <- run("--code-src=", "--data-src=")
  expect_identical(cold$status, 0L, info = cold$output)
  expect_true(grepl("starts from nothing", cold$output, fixed = TRUE))
})

test_that("the repair advice deletes a release that carries nothing along with its tag", {
  # What reaches the refusal is a published release, and it has a git tag.
  # Deleted without --cleanup-tag, the tag stays behind where the prune, which
  # lists releases, never finds it, and a later publish under that date
  # attaches to the old commit.
  advice <- preflight_repair_advice()
  expect_true(grepl("gh release delete TAG --yes --cleanup-tag", advice, fixed = TRUE))
  expect_false(grepl("without --cleanup-tag", advice, fixed = TRUE))
  # When a draft shares the tag, a delete by tag can take the published
  # release instead, so that case goes by id.
  expect_true(grepl("gh api -X DELETE repos/{owner}/{repo}/releases/<id>", advice, fixed = TRUE))
  expect_true(grepl("force_full", advice, fixed = TRUE))
})

test_that("the repair advice describes how a publish leaves a release now", {
  # It used to say the publish deletes each asset before uploading its
  # replacement, which is what sent an operator looking for a release that had
  # lost one outright. A replacement uploads beside the live asset and renames,
  # so what is actually left is a database from one shard beside a manifest
  # from another, or the bytes sitting under swap-prev-<name> with nothing under the
  # name; and the next run repairs the second of those by itself.
  advice <- preflight_repair_advice()
  expect_false(grepl("--clobber", advice, fixed = TRUE))
  expect_true(grepl("swap-prev-<name>", advice, fixed = TRUE))
  expect_true(grepl("renamed into place", advice, fixed = TRUE))
  # And it says how to see an upload that was cut off, which gh release view
  # does not list.
  expect_true(grepl("releases/<id>/assets", advice, fixed = TRUE))
})

# ---------------------------------------------------------------------------
# The workflow half: the download that must not swallow its failure
# ---------------------------------------------------------------------------

test_that("update.yml keeps the prior manifest under the name preflight reads", {
  # The download step fetches the manifest off the release and renames it on
  # disk, and preflight_prior_dbs looks for that name and nothing else. The
  # swap-prev-<name> a replacement leaves on the release is a different thing
  # that never reaches the runner, so the two must not drift into each other.
  yml <- paste(readLines(file.path("..", "..", ".github", "workflows", "update.yml")),
               collapse = "\n")
  for (spec in .preflight_specs()) {
    expect_true(grepl(sprintf("mv out/%s out/%s", spec$manifest_asset, spec$manifest),
                      yml, fixed = TRUE), info = spec$series)
  }
})

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
