# tests/testthat/test-output-class.R: builds declared to reproduce the pinned
# build's output (ANALYZER_SAME_OUTPUT) count as the running build, so moving
# between them re-queues nothing; any other build stands alone.

# A summary with one latest, scanned row per build named, and one read attempt
# per build at the cap, so both kinds of re-queue can be seen.
.oc_db <- function(builds) {
  con <- open_or_init_db(withr::local_tempfile(fileext = ".db", .local_envir = parent.frame()))
  DBI::dbWriteTable(con, SUMMARY_TABLE, data.frame(
    package = paste0("p", seq_along(builds)), version = "1.0",
    latest_release_date = "2026-01-01", datasets_scanned = TRUE,
    analyzer_version = builds, stringsAsFactors = FALSE))
  for (i in seq_along(builds)) {
    for (k in seq_len(MAX_ANALYZER_READ_ATTEMPTS)) {
      .record_analyzer_read_attempt(con, paste0("p", i), builds[[i]])
    }
  }
  con
}

.oc_scanned <- function(con) {
  DBI::dbGetQuery(con, sprintf(
    'SELECT package FROM "%s" WHERE datasets_scanned IS NOT NULL ORDER BY package',
    SUMMARY_TABLE))$package
}

test_that("a listed build brings its whole class, any other stands alone, and NA none", {
  same <- c("0.5.0", "0.5.1")
  expect_identical(.analyzer_output_class("0.5.1", same), same)
  expect_identical(.analyzer_output_class("0.5.0", same), same)
  expect_identical(.analyzer_output_class("0.6.0", same), "0.6.0")
  expect_identical(.analyzer_output_class(NA_character_, same), character(0L))
  expect_identical(.analyzer_output_class("", same), character(0L))
  expect_identical(.analyzer_output_class(NULL, same), character(0L))
  expect_identical(.analyzer_output_class(ANALYZER_SAME_OUTPUT[[1L]]), ANALYZER_SAME_OUTPUT)
})

test_that("a build string that only looks like a class entry stands alone", {
  for (b in c("0.5.0-test", "v0.5.0", "0.5", " 0.5.0", "0.5.0.1")) {
    expect_identical(.analyzer_output_class(b, "0.5.0"), b, info = b)
  }
})

test_that("rows and read attempts of a build in the class survive a move within it", {
  con <- .oc_db("0.5.0")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  same <- c("0.5.0", "0.5.1")
  expect_identical(.invalidate_stale_dataset_scans(con, "0.5.1", same), 0L)
  expect_identical(.forget_other_builds_read_attempts(con, "0.5.1", same), 0L)
  expect_identical(.oc_scanned(con), "p1")
  expect_identical(.analyzer_read_exhausted(con), "p1")
})

test_that("the 0.5.2 pin re-queues nothing where every latest row is 0.5.0 or 0.5.1", {
  con <- .oc_db(c("0.5.0", "0.5.1", "0.5.0", "0.5.1"))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  pkgs <- paste0("p", 1:4)
  # The class the scripts hold, not one the test names.
  expect_identical(.invalidate_stale_dataset_scans(con, "0.5.2"), 0L)
  expect_identical(.forget_other_builds_read_attempts(con, "0.5.2"), 0L)
  expect_identical(.oc_scanned(con), pkgs)
  expect_identical(sort(.analyzer_read_exhausted(con)), pkgs)
  expect_identical(.n_latest_on_class(con, "0.5.2"), c(on_class = 4L, latest = 4L))
})

test_that("with the class naming only the old build, the same move re-queues them", {
  con <- .oc_db("0.5.0")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_identical(.invalidate_stale_dataset_scans(con, "0.5.1", "0.5.0"), 1L)
  expect_identical(.forget_other_builds_read_attempts(con, "0.5.1", "0.5.0"), 1L)
  expect_identical(.oc_scanned(con), character(0L))
  expect_identical(.analyzer_read_exhausted(con), character(0L))
})

test_that("a build outside the class re-queues rows of every build in it", {
  con <- .oc_db(c("0.5.0", "0.5.1"))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  same <- c("0.5.0", "0.5.1")
  expect_identical(.invalidate_stale_dataset_scans(con, "0.6.0", same), 2L)
  expect_identical(.forget_other_builds_read_attempts(con, "0.6.0", same), 2L)
  expect_identical(.oc_scanned(con), character(0L))
})

test_that("a run that cannot name its build re-queues nothing", {
  con <- .oc_db(c("0.5.0", "0.4.0"))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_identical(.invalidate_stale_dataset_scans(con, NA_character_, "0.5.0"), 0L)
  expect_identical(.forget_other_builds_read_attempts(con, NA_character_, "0.5.0"), 0L)
  expect_identical(.oc_scanned(con), c("p1", "p2"))
})

test_that("rows no build is named on are re-queued whatever the class", {
  con <- .oc_db(NA_character_)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  same <- c("0.5.0", "0.5.1")
  expect_identical(.invalidate_stale_dataset_scans(con, "0.5.1", same), 1L)
  expect_identical(.forget_other_builds_read_attempts(con, "0.5.1", same), 1L)
})

test_that("latest rows are counted on the class, not on the exact build", {
  con <- .oc_db(c("0.5.0", "0.5.1", "0.4.0", NA_character_))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  same <- c("0.5.0", "0.5.1")
  expect_identical(.n_latest_on_class(con, "0.5.1", same), c(on_class = 2L, latest = 4L))
  expect_identical(.n_latest_on_class(con, "0.6.0", same), c(on_class = 0L, latest = 4L))
  expect_identical(.n_latest_on_class(con, NA_character_, same), c(on_class = 0L, latest = 4L))
})
