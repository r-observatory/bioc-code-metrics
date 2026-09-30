# tests/testthat/helper-stored-rows.R: every stored row of one package.

# Rows of `pkg` in every table with a package column, across the databases
# run_update writes, each ordered by all its columns so two snapshots compare
# with identical(). The failures and over-cap tables are left out: they hold
# verdicts, not the package's data.
.package_rows <- function(out_dir, pkg) {
  paths <- unique(file.path(out_dir, c(DB_FILENAME, DATA_DB_FILENAME,
                                       RELEASE_TEXT_DB_FILENAME)))
  out <- list()
  for (path in paths[file.exists(paths)]) {
    con <- DBI::dbConnect(RSQLite::SQLite(), path)
    for (tbl in sort(DBI::dbListTables(con))) {
      if (grepl("_metrics_failures$|_over_cap$", tbl)) next
      cols <- DBI::dbListFields(con, tbl)
      if (!"package" %in% cols) next
      sql <- sprintf('SELECT * FROM "%s" WHERE package = ? ORDER BY %s', tbl,
                     paste(sprintf('"%s"', cols), collapse = ", "))
      out[[paste(basename(path), tbl)]] <- DBI::dbGetQuery(con, sql, params = list(pkg))
    }
    DBI::dbDisconnect(con)
  }
  out
}
