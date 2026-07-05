# Tests for .recollect_todo: which packages a backfill run should reprocess.

.mk_summary <- function(rows) {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbExecute(con,
    "CREATE TABLE bioc_code_summary (package TEXT, version TEXT, n_fns_r INTEGER)")
  for (r in rows) {
    DBI::dbExecute(con,
      "INSERT INTO bioc_code_summary (package, version, n_fns_r) VALUES (?, ?, ?)",
      params = list(r[[1]], r[[2]], r[[3]]))
  }
  con
}

test_that(".recollect_todo returns only un-migrated packages in the universe", {
  con <- .mk_summary(list(
    list("a", "1", 5L),   # migrated
    list("b", "1", NA),   # needs backfill
    list("c", "1", NA),   # needs backfill
    list("gone", "1", NA) # needs backfill but not in universe
  ))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  todo <- .recollect_todo(con, c("a", "b", "c"), perm_fail_pkgs = character(0L))
  expect_equal(todo, c("b", "c"))
})

test_that(".recollect_todo excludes permanent failures", {
  con <- .mk_summary(list(list("b", "1", NA), list("c", "1", NA)))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_equal(.recollect_todo(con, c("b", "c"), perm_fail_pkgs = "b"), "c")
})

test_that(".recollect_todo treats a package with any NULL row as needing backfill", {
  con <- .mk_summary(list(list("d", "1", 3L), list("d", "2", NA)))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_equal(.recollect_todo(con, "d", perm_fail_pkgs = character(0L)), "d")
})

test_that(".recollect_todo returns all stored packages when the column is absent", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE bioc_code_summary (package TEXT, version TEXT)")
  DBI::dbExecute(con, "INSERT INTO bioc_code_summary VALUES ('a', '1'), ('b', '1')")
  expect_equal(.recollect_todo(con, c("a", "b"), character(0L)), c("a", "b"))
})

test_that(".recollect_todo is empty when the table does not exist", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_equal(.recollect_todo(con, "a", character(0L)), character(0L))
})

# ---------------------------------------------------------------------------
# Detail backfill: latest-row-scoped convergence marker (detail_scanned).
# The marker lives only on a package's latest-version row (the one carrying a
# non-NULL latest_release_date), so the NULL check must be confined to that row
# or a multi-version package would be re-flagged forever.
# ---------------------------------------------------------------------------

# Rows: list(package, version, n_fns_r, latest_release_date, detail_scanned)
.mk_detail_summary <- function(rows) {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbExecute(con,
    "CREATE TABLE bioc_code_summary
       (package TEXT, version TEXT, n_fns_r INTEGER,
        latest_release_date TEXT, detail_scanned INTEGER)")
  for (r in rows) {
    DBI::dbExecute(con,
      "INSERT INTO bioc_code_summary
         (package, version, n_fns_r, latest_release_date, detail_scanned)
       VALUES (?, ?, ?, ?, ?)",
      params = list(r[[1]], r[[2]], r[[3]], r[[4]], r[[5]]))
  }
  con
}

.detail_todo <- function(con, universe, perm = character(0L)) {
  .recollect_todo(con, universe, perm,
                  sentinel = "detail_scanned", latest_only = TRUE)
}

test_that("detail backfill returns packages whose latest row is not detail-scanned", {
  con <- .mk_detail_summary(list(
    list("a", "1.0", 5L, "2020-01-01", NA),  # latest row unscanned  -> todo
    list("b", "1.0", 5L, "2020-01-01", 1L)   # latest row scanned    -> converged
  ))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_equal(.detail_todo(con, c("a", "b")), "a")
})

test_that("a detail-scanned package never re-appears, even data-only (zero functions)", {
  con <- .mk_detail_summary(list(
    list("withfns",  "1.0", 12L, "2020-01-01", 1L),  # scanned, has functions
    list("dataonly", "1.0",  0L, "2020-01-01", 1L)   # scanned, zero functions
  ))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_equal(.detail_todo(con, c("withfns", "dataonly")), character(0L))
})

test_that("detail backfill scopes the NULL check to the latest row (multi-version converges)", {
  # Older row unmarked (NULL), latest row scanned (1). The older NULL must NOT
  # keep the package on the todo list once its latest row has been scanned.
  con <- .mk_detail_summary(list(
    list("m", "1.0", 5L, NA,           NA),  # non-latest row: detail_scanned NULL
    list("m", "1.1", 5L, "2021-01-01", 1L)   # latest row:     detail_scanned set
  ))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_equal(.detail_todo(con, "m"), character(0L))
})

test_that("detail backfill returns a multi-version package whose latest row is unscanned", {
  con <- .mk_detail_summary(list(
    list("m", "1.0", 5L, NA,           1L),  # non-latest row happens to be marked
    list("m", "1.1", 5L, "2021-01-01", NA)   # latest row unscanned -> todo
  ))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_equal(.detail_todo(con, "m"), "m")
})

test_that("detail backfill excludes permanent failures", {
  con <- .mk_detail_summary(list(
    list("a", "1.0", 5L, "2020-01-01", NA),
    list("b", "1.0", 5L, "2020-01-01", NA)
  ))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_equal(.detail_todo(con, c("a", "b"), perm = "a"), "b")
})

test_that("detail backfill flags every latest-row package when the column is absent", {
  con <- .mk_summary(list(list("a", "1", 5L), list("b", "1", NA)))
  DBI::dbExecute(con, "ALTER TABLE bioc_code_summary ADD COLUMN latest_release_date TEXT")
  DBI::dbExecute(con, "UPDATE bioc_code_summary SET latest_release_date = '2020-01-01'")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_equal(.detail_todo(con, c("a", "b")), c("a", "b"))
})

test_that("n_fns_r backfill semantics are unchanged by the detail marker", {
  # The n_fns_r source keeps its any-NULL-row semantics; the detail source is
  # independent and latest-row-scoped.
  con <- .mk_detail_summary(list(
    list("p", "1.0", 3L, NA,           NA),  # older, n_fns_r set
    list("p", "1.1", NA, "2021-01-01", 1L)   # latest, n_fns_r NULL, detail scanned
  ))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  # n_fns_r path (default): any NULL row keeps the package in.
  expect_equal(.recollect_todo(con, "p", character(0L)), "p")
  # detail path: converged, since the latest row is scanned.
  expect_equal(.detail_todo(con, "p"), character(0L))
})

test_that("todo union of changed + n_fns_r backfill + detail backfill has no duplicates", {
  # Mirrors run_update's union expression across the three independent sources.
  con <- .mk_detail_summary(list(
    list("a", "1.0", NA, "2020-01-01", NA),  # n_fns_r NULL and detail NULL
    list("b", "1.0", 5L, "2020-01-01", NA),  # detail NULL only
    list("c", "1.0", NA, "2020-01-01", 1L)   # n_fns_r NULL only
  ))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  universe <- c("a", "b", "c")
  changed  <- c("a", "c")  # Bioc reports new versions for a and c
  backfill <- .recollect_todo(con, universe, character(0L))   # a, c
  detail   <- .detail_todo(con, universe)                     # a, b
  todo <- sort(unique(c(changed, backfill, detail)))
  expect_equal(todo, c("a", "b", "c"))
  expect_false(anyDuplicated(todo) > 0L)
})
