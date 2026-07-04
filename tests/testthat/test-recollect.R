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
