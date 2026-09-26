# scripts/release_text.R: the DESCRIPTION and release notes the analyzer prints per
# version. Identical in both code-metrics pipelines; config.R names the tables.

# DESCRIPTION fields the merged latest-only table keeps, besides every Config/* field.
.LATEST_DESCRIPTION_FIELDS <- c("RdMacros", "RoxygenNote", "SystemRequirements",
                                "Language", "LazyData", "Date")

.RELEASE_NOTES_COLS <- c("package", "version", "package_version", "news_file",
                         "release_notes_source", "release_notes",
                         "release_notes_truncated")

.empty_description_rows <- function() {
  data.frame(package = character(0L), version = character(0L),
             field = character(0L), value = character(0L),
             stringsAsFactors = FALSE)
}

.empty_description_fields_rows <- function() {
  data.frame(package = character(0L), version = character(0L),
             field = character(0L), value = character(0L),
             value_truncated = integer(0L), stringsAsFactors = FALSE)
}

.empty_release_notes_rows <- function() {
  data.frame(package = character(0L), version = character(0L),
             package_version = character(0L), news_file = character(0L),
             release_notes_source = character(0L), release_notes = character(0L),
             release_notes_truncated = integer(0L), stringsAsFactors = FALSE)
}

.empty_text_versions_rows <- function() {
  data.frame(package = character(0L), version = character(0L),
             analyzer_version = character(0L), n_fields = integer(0L),
             has_release_notes = integer(0L), read_at = character(0L),
             stringsAsFactors = FALSE)
}

.empty_release_text <- function() {
  list(description = .empty_description_rows(),
       release_notes = .empty_release_notes_rows(),
       versions = .empty_text_versions_rows(),
       description_latest = .empty_description_fields_rows(),
       release_notes_latest = .empty_release_notes_rows())
}

#' Cut each string to at most `max_bytes` of UTF-8 without splitting a character.
.cap_utf8_bytes <- function(x, max_bytes) {
  vapply(x, function(s) {
    if (is.na(s)) return(NA_character_)
    b <- charToRaw(enc2utf8(s))
    if (length(b) <= max_bytes) return(s)
    k <- max_bytes
    # A byte of the form 10xxxxxx continues a character, so back off to its start.
    while (k > 0L && bitwAnd(as.integer(b[k + 1L]), 0xC0L) == 0x80L) k <- k - 1L
    out <- rawToChar(b[seq_len(k)])
    Encoding(out) <- "UTF-8"
    out
  }, character(1L), USE.NAMES = FALSE)
}

.description_field_kept <- function(field) {
  field %in% .LATEST_DESCRIPTION_FIELDS | startsWith(field, "Config/")
}

#' The text rows one analysed version contributes. The versions row is written even
#' when the dcf record is empty or absent, so reconciliation never re-reads it forever.
.release_text_rows <- function(package, version, dcf, notes, analyzer_version,
                               read_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ",
                                                tz = "UTC")) {
  description <- if (length(dcf)) {
    data.frame(package = package, version = version, field = names(dcf),
               value = unname(dcf), stringsAsFactors = FALSE)
  } else {
    .empty_description_rows()
  }
  release_notes <- if (!is.null(notes)) {
    data.frame(package = package, version = version,
               package_version = .rec_chr(notes, "package_version"),
               news_file = .rec_chr(notes, "news_file"),
               release_notes_source = .rec_chr(notes, "release_notes_source"),
               release_notes = .rec_chr(notes, "release_notes"),
               release_notes_truncated = as.integer(.rec_lgl(notes, "release_notes_truncated")),
               stringsAsFactors = FALSE)
  } else {
    .empty_release_notes_rows()
  }
  versions <- data.frame(
    package = package, version = version,
    analyzer_version = as.character(analyzer_version %||% NA_character_),
    n_fields = if (is.null(dcf)) NA_integer_ else length(dcf),
    has_release_notes = as.integer(!is.null(notes)),
    read_at = read_at, stringsAsFactors = FALSE)
  list(description = description, release_notes = release_notes, versions = versions)
}

#' Bind one package's per-version text rows and cap its latest version's rows.
#' NULL entries in `rows` are versions the R fallback wrote.
.release_text_collect <- function(rows, latest_version,
                                  max_bytes = RELEASE_TEXT_FIELD_MAX_BYTES) {
  rows <- Filter(Negate(is.null), rows)
  # A version read twice keeps its last reading, the row upsert_shard keeps.
  read <- vapply(rows, function(r) r$versions$version[[1L]], character(1L))
  rows <- rows[!duplicated(read, fromLast = TRUE)]
  out <- .empty_release_text()
  if (!length(rows)) return(out)
  out$description   <- do.call(rbind, c(list(out$description),
                                        lapply(rows, `[[`, "description")))
  out$release_notes <- do.call(rbind, c(list(out$release_notes),
                                        lapply(rows, `[[`, "release_notes")))
  out$versions      <- do.call(rbind, c(list(out$versions),
                                        lapply(rows, `[[`, "versions")))

  d <- out$description[out$description$version == latest_version &
                         .description_field_kept(out$description$field), , drop = FALSE]
  if (nrow(d)) {
    capped <- .cap_utf8_bytes(d$value, max_bytes)
    out$description_latest <- data.frame(
      package = d$package, version = d$version, field = d$field, value = capped,
      value_truncated = as.integer(!is.na(d$value) &
                                     nchar(d$value, type = "bytes") > max_bytes),
      stringsAsFactors = FALSE)
  }
  n <- out$release_notes[out$release_notes$version == latest_version, , drop = FALSE]
  if (nrow(n)) {
    cut <- !is.na(n$release_notes) & nchar(n$release_notes, type = "bytes") > max_bytes
    n$release_notes <- .cap_utf8_bytes(n$release_notes, max_bytes)
    n$release_notes_truncated[cut] <- 1L
    out$release_notes_latest <- n
  }
  out
}

#' Bind the text of every package in a shard.
.bind_release_text <- function(texts) {
  texts <- Filter(Negate(is.null), texts)
  out <- .empty_release_text()
  for (k in names(out)) {
    out[[k]] <- do.call(rbind, c(list(out[[k]]), lapply(texts, `[[`, k)))
  }
  out
}

.ensure_release_text_tables <- function(con) {
  DBI::dbExecute(con, sprintf('CREATE TABLE IF NOT EXISTS "%s" (
    package TEXT NOT NULL, version TEXT NOT NULL, field TEXT NOT NULL, value TEXT,
    PRIMARY KEY (package, version, field)) WITHOUT ROWID', DESCRIPTION_HISTORY_TABLE))
  DBI::dbExecute(con, sprintf('CREATE TABLE IF NOT EXISTS "%s" (
    package TEXT NOT NULL, version TEXT NOT NULL, package_version TEXT,
    news_file TEXT, release_notes_source TEXT, release_notes TEXT,
    release_notes_truncated INTEGER,
    PRIMARY KEY (package, version)) WITHOUT ROWID', RELEASE_NOTES_HISTORY_TABLE))
  DBI::dbExecute(con, sprintf('CREATE TABLE IF NOT EXISTS "%s" (
    package TEXT NOT NULL, version TEXT NOT NULL, analyzer_version TEXT,
    n_fields INTEGER, has_release_notes INTEGER, read_at TEXT,
    PRIMARY KEY (package, version))', RELEASE_TEXT_VERSIONS_TABLE))
  invisible(NULL)
}

#' Open the database holding the text history and create its tables. Pass `con`
#' when the history shares the code database, as it does on Bioconductor.
open_or_init_release_text_db <- function(path, con = NULL) {
  text_con <- if (is.null(con)) DBI::dbConnect(RSQLite::SQLite(), path) else con
  .ensure_release_text_tables(text_con)
  text_con
}

.ensure_latest_text_tables <- function(con) {
  DBI::dbExecute(con, sprintf('CREATE TABLE IF NOT EXISTS "%s" (
    package TEXT NOT NULL, version TEXT NOT NULL, field TEXT NOT NULL,
    value TEXT, value_truncated INTEGER NOT NULL DEFAULT 0,
    PRIMARY KEY (package, field))', DESCRIPTION_FIELDS_TABLE))
  DBI::dbExecute(con, sprintf('CREATE TABLE IF NOT EXISTS "%s" (
    package TEXT NOT NULL PRIMARY KEY, version TEXT NOT NULL,
    package_version TEXT, news_file TEXT, release_notes_source TEXT,
    release_notes TEXT, release_notes_truncated INTEGER)', RELEASE_NOTES_TABLE))
  invisible(NULL)
}

#' Replace the history rows of the versions just read, and only those: a version
#' the R fallback wrote this time keeps the history it already has.
upsert_release_text <- function(text_con, description_df, notes_df, versions_df) {
  if (is.null(versions_df) || nrow(versions_df) == 0L) return(invisible(NULL))
  keys <- unique(versions_df[, c("package", "version")])
  DBI::dbWithTransaction(text_con, {
    for (tbl in c(DESCRIPTION_HISTORY_TABLE, RELEASE_NOTES_HISTORY_TABLE,
                  RELEASE_TEXT_VERSIONS_TABLE)) {
      DBI::dbExecute(text_con,
        sprintf('DELETE FROM "%s" WHERE package = ? AND version = ?', tbl),
        params = list(keys$package, keys$version))
    }
    if (!is.null(description_df) && nrow(description_df) > 0L) {
      DBI::dbAppendTable(text_con, DESCRIPTION_HISTORY_TABLE,
                         description_df[, c("package", "version", "field", "value")])
    }
    if (!is.null(notes_df) && nrow(notes_df) > 0L) {
      DBI::dbAppendTable(text_con, RELEASE_NOTES_HISTORY_TABLE,
                         notes_df[, .RELEASE_NOTES_COLS])
    }
    DBI::dbAppendTable(text_con, RELEASE_TEXT_VERSIONS_TABLE, versions_df)
  })
  invisible(NULL)
}

#' Replace the merged latest-only rows of a shard's packages. Runs inside
#' upsert_shard's transaction, the way the vignette rows are replaced.
.write_latest_text <- function(con, pkgs, description_latest, notes_latest) {
  .ensure_latest_text_tables(con)
  .delete_by_package(con, DESCRIPTION_FIELDS_TABLE, pkgs)
  .delete_by_package(con, RELEASE_NOTES_TABLE, pkgs)
  if (!is.null(description_latest) && nrow(description_latest) > 0L) {
    DBI::dbAppendTable(con, DESCRIPTION_FIELDS_TABLE, description_latest)
  }
  if (!is.null(notes_latest) && nrow(notes_latest) > 0L) {
    DBI::dbAppendTable(con, RELEASE_NOTES_TABLE, notes_latest[, .RELEASE_NOTES_COLS])
  }
  invisible(NULL)
}

#' Re-queue every package whose 0.5.0 rows the text history lacks or holds only from an
#' older build. Past `max_missing` the text database is the wrong copy, and the run stops.
.reconcile_release_text <- function(con, text_con, max_missing = RELEASE_TEXT_REQUEUE_MAX) {
  if (!SUMMARY_TABLE %in% DBI::dbListTables(con)) return(invisible(0L))
  fields <- DBI::dbListFields(con, SUMMARY_TABLE)
  if (!"analyzer_version" %in% fields) return(invisible(0L))
  builds <- DBI::dbGetQuery(con, sprintf(
    'SELECT DISTINCT analyzer_version FROM "%s" WHERE analyzer_version IS NOT NULL',
    SUMMARY_TABLE))$analyzer_version
  checked <- builds[vapply(builds, analyzer_at_least, logical(1L), min = "0.5.0")]
  if (!length(checked)) return(invisible(0L))
  rows <- DBI::dbGetQuery(con, sprintf(
    'SELECT package, version FROM "%s" WHERE analyzer_version IN (%s)',
    SUMMARY_TABLE, paste(rep("?", length(checked)), collapse = ", ")),
    params = as.list(checked))
  # A row an older build wrote carries no release notes, so only a reading by a
  # 0.5.0 build closes the gap; an older text copy can hold one from before the pin.
  read_by <- DBI::dbGetQuery(text_con, sprintf(
    'SELECT DISTINCT analyzer_version FROM "%s" WHERE analyzer_version IS NOT NULL',
    RELEASE_TEXT_VERSIONS_TABLE))$analyzer_version
  current <- read_by[vapply(read_by, analyzer_at_least, logical(1L), min = "0.5.0")]
  have <- if (length(current)) {
    DBI::dbGetQuery(text_con, sprintf(
      'SELECT package, version FROM "%s" WHERE analyzer_version IN (%s)',
      RELEASE_TEXT_VERSIONS_TABLE, paste(rep("?", length(current)), collapse = ", ")),
      params = as.list(current))
  } else {
    data.frame(package = character(0L), version = character(0L))
  }
  missing <- rows[!paste(rows$package, rows$version, sep = "\r") %in%
                    paste(have$package, have$version, sep = "\r"), , drop = FALSE]
  n <- nrow(missing)
  if (n == 0L) return(invisible(0L))
  if (n > max_missing) {
    stop(sprintf(paste0(
      "%d analysed versions have no 0.5.0 reading in %s, more than the %d a run re-reads: ",
      "%s is not the copy that belongs with this code database. Restore it from ",
      "the release that published it and re-run."),
      n, RELEASE_TEXT_VERSIONS_TABLE, max_missing, RELEASE_TEXT_DB_FILENAME),
      call. = FALSE)
  }
  pkgs <- unique(missing$package)
  if (all(c("datasets_scanned", "latest_release_date") %in% fields)) {
    for (i in seq(1L, length(pkgs), by = 900L)) {
      chunk <- pkgs[i:min(i + 899L, length(pkgs))]
      DBI::dbExecute(con, sprintf(
        'UPDATE "%s" SET datasets_scanned = NULL
          WHERE latest_release_date IS NOT NULL AND package IN (%s)',
        SUMMARY_TABLE, paste(rep("?", length(chunk)), collapse = ", ")),
        params = as.list(chunk))
    }
  }
  shown <- head(paste(missing$package, missing$version), 10L)
  message(sprintf("release text history lacks %d analysed version%s; re-reading %d package%s: %s%s",
                  n, if (n == 1L) "" else "s", length(pkgs),
                  if (length(pkgs) == 1L) "" else "s",
                  paste(shown, collapse = ", "), if (n > 10L) ", ..." else ""))
  invisible(n)
}
