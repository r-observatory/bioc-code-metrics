# tests/testthat/helper-version-skip.R: runs with the version skip on or off.

# The suite runs in record mode whatever the shell exports; a skip test turns it on.
.local_global("VERSION_SKIP", FALSE, frame = testthat::teardown_env())

# One .vst_run() over `latest` (package -> latest release) with the version skip
# on, or off when skip is FALSE.
.vs_run <- function(out_dir, root, latest, skip = TRUE, ...) {
  .local_global("VERSION_SKIP", skip)
  .vst_run(root, out_dir, names(latest), unname(latest), ...)
}

# The profile_fp each dataset content id of out_dir links to, named by id.
.vs_profiles <- function(out_dir) {
  path <- file.path(out_dir, DATA_DB_FILENAME)
  if (!file.exists(path)) return(character(0L))
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con))
  if (!"bioc_dataset_contents" %in% DBI::dbListTables(con)) return(character(0L))
  ids <- DBI::dbGetQuery(con, "SELECT content_id, profile_fp FROM bioc_dataset_contents")
  stats::setNames(ids$profile_fp, ids$content_id)
}

# .package_rows() without the read attempts and the columns a run stamps with
# its own time, each content id read as the profile it links to (the ids are
# row numbers), and each frame ordered by the columns it keeps.
.vs_rows <- function(out_dir, pkg) {
  rows <- .package_rows(out_dir, pkg)
  rows <- rows[!grepl(" bioc_analyzer_read_attempts$", names(rows))]
  fp <- .vs_profiles(out_dir)
  lapply(rows, function(df) {
    for (cn in intersect(c("content_id", "current_content_id"), names(df))) {
      id <- as.character(df[[cn]])
      df[[cn]] <- ifelse(is.na(id), NA_character_,
                         ifelse(id %in% names(fp), unname(fp[id]), paste("no content", id)))
    }
    df <- df[, setdiff(names(df), c("read_at", "assessed_at")), drop = FALSE]
    df <- df[do.call(order, unname(as.list(df))), , drop = FALSE]
    rownames(df) <- NULL
    df
  })
}
