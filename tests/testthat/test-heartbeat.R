# The manifest carries two different questions, and the merger's freshness gate
# asks the first one: "did this pipeline run" (last_checked) rather than "did the
# data change" (last_changed). This pipeline only publishes when the data
# changes, and its universe is keyed on the Bioconductor RELEASE, which moves
# twice a year. Without a separate last_checked, a perfectly healthy pipeline
# reads as months stale between releases.

# A run that never reached Bioconductor produces an EMPTY universe, and from
# there it is shaped exactly like a healthy months-of-silence run: nothing to
# analyze, nothing changed, nothing published, exit 0. available.packages()
# warns rather than errors when the repos are unreachable, and package_list()
# swallows what follows, so no error escapes to stop the run. Publishing a
# heartbeat for that would refresh last_checked on behalf of a pipeline that
# cannot see upstream, and the freshness gate going stale is the only thing that
# would otherwise notice.
test_that("update.yml refuses to publish a heartbeat for an empty universe", {
  yml <- paste(readLines(file.path("..", "..", ".github", "workflows", "update.yml")),
               collapse = "\n")

  expect_true(grepl("N_UNIVERSE", yml, fixed = TRUE))

  guard_at <- regexpr("N_UNIVERSE", yml, fixed = TRUE)[[1]]
  calls    <- gregexpr("refresh_heartbeat", yml, fixed = TRUE)[[1]]
  expect_true(guard_at > 0)
  expect_lt(guard_at, calls[length(calls)])
})

.heartbeat_io <- function() list(
  package_list = function() data.frame(package = "pkgA", latest_version = "1.0",
                                       stringsAsFactors = FALSE),
  clone = function(pkg, dest) { dir.create(dest, showWarnings = FALSE); TRUE })

.stub_analyze <- function() {
  old <- analyze_package
  assign("analyze_package", function(dest, pkg) {
    build <- rpkg_analyzer_version()
    # The analyzer prints a dcf record, and from 0.5.0 a release_notes record too.
    notes <- if (analyzer_at_least(build, "0.5.0")) {
      list(rec = "release_notes", package_version = "1.0", news_file = "NEWS.md",
           release_notes_source = "news_md", release_notes = "- first release",
           release_notes_truncated = FALSE)
    }
    list(
      # A package the analyzer read. The scan marker, the build that earned it
      # and the version named as one the binary produced arrive together,
      # because that is the only combination analyze_package can return: the
      # reader that sets the marker is the producer that names the build. The
      # build is whatever this machine's analyzer answers, so the row is one the
      # re-scan queue reads as current rather than as collected by somebody else.
      summary = data.frame(package = pkg, version = "1.0", loc_r = 10L, n_fns_r = 1L,
        latest_release_date = "2026-01-01", datasets_scanned = TRUE, detail_scanned = TRUE,
        analyzer_version = build, stringsAsFactors = FALSE),
      churn = NULL, api = NULL, functions = NULL, edges = NULL, datasets = NULL,
      # The text reading analyze_package keeps for each version the binary produced.
      text = .release_text_collect(list(.release_text_rows(
        pkg, "1.0", c(Package = pkg, Version = "1.0", Title = "A Package"),
        notes, build)), "1.0"),
      binary_versions = "1.0")
  }, envir = environment(run_update))
  old
}

test_that("build_manifest records when the run checked, not only when it was generated", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  m <- build_manifest(con, "code", "r-observatory/bioc-code-metrics",
                      DB_FILENAME, 1024, character(0L), "t", "c", "t", "t",
                      "t", character(0L),
                      list(n_analyzed = 0L, n_universe = 0L, n_remaining = 0L,
                           bootstrap_complete = TRUE))

  expect_type(m$last_checked, "character")
  expect_equal(m$last_checked, m$generated_at)
})

test_that("build_manifest carries a supplied last_changed through untouched", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  m <- build_manifest(con, "code", "r-observatory/bioc-code-metrics",
                      DB_FILENAME, 1024, character(0L), "t", "c", "t", "t",
                      "t", character(0L),
                      list(n_analyzed = 0L, n_universe = 0L, n_remaining = 0L,
                           bootstrap_complete = TRUE),
                      last_changed = "2026-07-22T06:38:00Z")

  expect_equal(m$last_changed, "2026-07-22T06:38:00Z")
  expect_false(identical(m$last_checked, m$last_changed))
})

test_that("build_manifest defaults last_changed to this run when none is supplied", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  m <- build_manifest(con, "code", "r-observatory/bioc-code-metrics",
                      DB_FILENAME, 1024, character(0L), "t", "c", "t", "t",
                      "t", character(0L),
                      list(n_analyzed = 0L, n_universe = 0L, n_remaining = 0L,
                           bootstrap_complete = TRUE))

  expect_equal(m$last_changed, m$generated_at)
})

test_that("build_manifest stamps the time it is given", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  m <- build_manifest(con, "code", "r-observatory/bioc-code-metrics",
                      DB_FILENAME, 1024, character(0L), "t", "c", "t", "t",
                      "t", character(0L),
                      list(n_analyzed = 0L, n_universe = 0L, n_remaining = 0L,
                           bootstrap_complete = TRUE),
                      now = as.POSIXct("2026-07-22 06:38:00", tz = "UTC"))

  expect_identical(c(m$generated_at, m$last_checked, m$last_changed),
                   rep("2026-07-22T06:38:00Z", 3L))
})

# A shard writes two manifests, and a second can turn over between two clock
# reads. The clock here moves a second at every read, so a shard that read it
# once per manifest would date the data manifest a second after the code one.
test_that("a shard stamps both manifests and its status from one clock read", {
  old <- .stub_analyze()
  on.exit(assign("analyze_package", old, envir = environment(run_update)), add = TRUE)

  out <- withr::local_tempdir()
  .local_stepping_clock()
  returned <- run_update(.heartbeat_io(), out, shard_size = 10L)
  code   <- jsonlite::fromJSON(file.path(out, "code-manifest.json"))
  data   <- jsonlite::fromJSON(file.path(out, "data-manifest.json"))
  status <- jsonlite::fromJSON(file.path(out, "run-status.json"))

  expect_true(returned$changed)
  for (field in c("generated_at", "last_checked", "last_changed")) {
    expect_identical(data[[field]], code[[field]], info = field)
  }
  expect_identical(returned$generated_at, code$generated_at)
  expect_identical(c(status$last_changed, status$data_last_changed),
                   rep(code$last_changed, 2L))
})

# The behaviour that actually fixes the daily red merge: a run that finds nothing
# to do must still refresh last_checked, while leaving last_changed at the moment
# the data really moved.
# The manifest already published to metrics-2026-07-22 has neither field (its
# keys are bootstrap, db_bytes, db_filename, fingerprint, generated_at,
# n_packages, n_versions, repo, schema_version, series, stats, tables), so the
# first run after this ships resolves last_changed through the generated_at
# fallback. That is a different path from the carry-forward below.
test_that("a prior manifest with no last_changed falls back to its generated_at", {
  old <- .stub_analyze()
  on.exit(assign("analyze_package", old, envir = environment(run_update)), add = TRUE)

  out <- withr::local_tempdir()
  io  <- .heartbeat_io()

  run_update(io, out, shard_size = 10L)
  run1 <- jsonlite::fromJSON(file.path(out, "code-manifest.json"))

  legacy <- run1[setdiff(names(run1), c("last_checked", "last_changed"))]
  legacy$generated_at <- "2026-07-22T06:38:00Z"
  write_manifest(file.path(out, "prev-code-manifest.json"), legacy)

  m2     <- run_update(io, out, shard_size = 10L)
  second <- jsonlite::fromJSON(file.path(out, "code-manifest.json"))

  expect_false(m2$changed)
  expect_equal(second$last_changed, "2026-07-22T06:38:00Z")
  expect_equal(second$last_checked, second$generated_at)
})

# The previous manifest here carries a last_changed that differs from its own
# generated_at, and both differ from now. Without that spread the assertion is
# vacuous: run 1's manifest has last_changed == generated_at, and two runs of
# this fixture finish inside the same wall-clock second, so "carried forward",
# "read the wrong field" and "no carry-forward at all" all look identical.
test_that("a no-op run carries last_changed forward, not the prior generated_at", {
  old <- .stub_analyze()
  on.exit(assign("analyze_package", old, envir = environment(run_update)), add = TRUE)

  out <- withr::local_tempdir()
  io  <- .heartbeat_io()

  run_update(io, out, shard_size = 10L)
  run1 <- jsonlite::fromJSON(file.path(out, "code-manifest.json"))

  # Stand in for the release download the workflow performs between runs. The
  # fingerprint is run 1's, so this second run is genuinely a no-op.
  prior <- run1
  prior$last_changed <- "2026-01-05T00:00:00Z"   # the run that moved data
  prior$last_checked <- "2026-07-25T00:00:00Z"   # a later no-op run
  prior$generated_at <- "2026-07-25T00:00:00Z"
  write_manifest(file.path(out, "prev-code-manifest.json"), prior)

  m2     <- run_update(io, out, shard_size = 10L)
  second <- jsonlite::fromJSON(file.path(out, "code-manifest.json"))

  expect_false(m2$changed)
  # Reading generated_at instead of last_changed would yield 2026-07-25 here,
  # which is what makes this distinguishable from the legacy-fallback test.
  expect_equal(second$last_changed, "2026-01-05T00:00:00Z")
  expect_equal(second$last_checked, second$generated_at)
  expect_true(second$last_checked > second$last_changed)
})
