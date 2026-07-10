# scripts/export.R: SQLite export, manifest, and fingerprint helpers.
#
# Load order: config.R -> export.R
# Does NOT auto-source dependencies; caller controls load order.

#' Coerce logical columns in a data.frame to 0/1 INTEGER.
#'
#' SQLite has no native boolean type. This helper converts every logical
#' column to integer (TRUE -> 1L, FALSE -> 0L, NA -> NA_integer_) so that
#' downstream reads are stable regardless of driver type inference.
#'
#' @param df A data.frame. Non-logical columns are unchanged.
#' @return A copy of df with all logical columns replaced by integer.
.coerce_logicals <- function(df) {
  for (col in names(df)) {
    if (is.logical(df[[col]])) {
      df[[col]] <- as.integer(df[[col]])
    }
  }
  df
}

#' Export code-metrics tables to a fresh SQLite database.
#'
#' Creates (or replaces) the file at `path` with three tables:
#'   bioc_code_summary  -- one row per package-version, all metric columns.
#'   bioc_code_churn    -- one row per file per version (added/deleted lines).
#'   bioc_api_history   -- one row per version (export diffs as JSON arrays).
#'
#' The schema for bioc_code_summary is derived entirely from `summary_df`
#' (schema-flexible). Logical columns in any input frame are coerced to 0/1
#' INTEGER before writing. NA values are preserved.
#'
#' @param path       File path for the output .db file.
#' @param summary_df data.frame with columns package, version, date, and any
#'   number of metric columns (integer/numeric/logical/character).
#'   If empty (0 rows), the table is still created with at least package and
#'   version TEXT columns.
#' @param churn_df   data.frame with columns package, version, file, added,
#'   deleted. added/deleted may be NA for binary files.
#' @param api_df     data.frame with columns package, version, exports_added,
#'   exports_removed (JSON array strings), n_exports (integer), and optionally
#'   cold_removals.
export_metrics <- function(path, summary_df, churn_df, api_df) {
  if (file.exists(path)) unlink(path)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # ---- bioc_code_summary -----------------------------------------------------
  write_summary <- .coerce_logicals(summary_df)
  # Guarantee at least package and version columns for schema stability.
  if (!"package" %in% names(write_summary)) {
    write_summary[["package"]] <- rep(NA_character_, nrow(write_summary))
  }
  if (!"version" %in% names(write_summary)) {
    write_summary[["version"]] <- rep(NA_character_, nrow(write_summary))
  }
  DBI::dbWriteTable(con, "bioc_code_summary", write_summary, row.names = FALSE)
  DBI::dbExecute(con,
    "CREATE UNIQUE INDEX idx_summary_pkg_ver ON bioc_code_summary(package, version)")

  # ---- bioc_code_churn -------------------------------------------------------
  DBI::dbWriteTable(con, "bioc_code_churn", .coerce_logicals(churn_df), row.names = FALSE)
  DBI::dbExecute(con,
    "CREATE INDEX idx_churn_pkg_ver ON bioc_code_churn(package, version)")
  DBI::dbExecute(con,
    "CREATE INDEX idx_churn_pkg ON bioc_code_churn(package)")

  # ---- bioc_api_history ------------------------------------------------------
  DBI::dbWriteTable(con, "bioc_api_history", .coerce_logicals(api_df), row.names = FALSE)
  DBI::dbExecute(con,
    "CREATE INDEX idx_api_pkg_ver ON bioc_api_history(package, version)")

  DBI::dbExecute(con, "VACUUM")
  invisible(NULL)
}

#' Write an R list as pretty-printed JSON.
#'
#' @param path File path for the output .json file.
#' @param obj  R list to serialise.
write_manifest <- function(path, obj) {
  jsonlite::write_json(obj, path, auto_unbox = TRUE, pretty = TRUE)
  invisible(NULL)
}

#' Compute a stable SHA-256 fingerprint over the set of package-version pairs.
#'
#' Derives a 64-character hex string from the sorted vector of
#' "package:version" keys in summary_df.  Adding a new version for any
#' package changes the key set and therefore changes the fingerprint.
#' Identical inputs in the same R session always produce the same hash.
#'
#' @param summary_df data.frame with at least columns package and version.
#' @return 64-character lower-case hex string (SHA-256).
metrics_fingerprint <- function(summary_df) {
  if (nrow(summary_df) == 0L) {
    keys <- character(0L)
  } else {
    keys <- sort(paste(summary_df$package, summary_df$version, sep = ":"))
  }
  digest::digest(paste(keys, collapse = ","), algo = "sha256", serialize = FALSE)
}

# ---------------------------------------------------------------------------
# In-place DB helpers (used by run_update for O(shard) memory writes)
# ---------------------------------------------------------------------------

# Delete rows for a set of packages from one table, chunking IN lists to <= 900.
# Silently no-ops if the table does not exist or pkgs is empty.
.delete_by_package <- function(con, table, pkgs) {
  tables <- DBI::dbListTables(con)
  if (!table %in% tables || length(pkgs) == 0L) return(invisible(NULL))
  chunk_size <- 900L
  for (i in seq(1L, length(pkgs), by = chunk_size)) {
    chunk <- pkgs[i:min(i + chunk_size - 1L, length(pkgs))]
    ph    <- paste(rep("?", length(chunk)), collapse = ", ")
    DBI::dbExecute(
      con,
      sprintf("DELETE FROM %s WHERE package IN (%s)", table, ph),
      params = as.list(chunk)
    )
  }
  invisible(NULL)
}

# Append rows to a detail table, creating it (schema derived from the frame) on
# first write and tolerating new columns via ALTER TABLE ... ADD COLUMN. Mirrors
# the schema-flexible bioc_code_summary path but without a UNIQUE index (detail
# tables carry many rows per package/version). A zero-row frame still creates the
# table with the correct column types. Logical columns are coerced to 0/1.
.append_detail_table <- function(con, table, df) {
  if (is.null(df)) return(invisible(NULL))
  df     <- .coerce_logicals(df)
  tables <- DBI::dbListTables(con)

  if (!table %in% tables) {
    DBI::dbWriteTable(con, table, df, row.names = FALSE,
                      overwrite = FALSE, append = FALSE)
  } else {
    existing_cols <- DBI::dbListFields(con, table)
    for (col in setdiff(names(df), existing_cols)) {
      col_type <- if (is.integer(df[[col]])) "INTEGER"
                  else if (is.numeric(df[[col]])) "REAL"
                  else "TEXT"
      DBI::dbExecute(con,
        sprintf("ALTER TABLE %s ADD COLUMN \"%s\" %s", table, col, col_type))
    }
    if (nrow(df) > 0L) DBI::dbAppendTable(con, table, df)
  }
  DBI::dbExecute(con, sprintf(
    "CREATE INDEX IF NOT EXISTS idx_%s_pkg_ver ON %s(package, version)",
    table, table))
  DBI::dbExecute(con, sprintf(
    "CREATE INDEX IF NOT EXISTS idx_%s_pkg ON %s(package)",
    table, table))
  invisible(NULL)
}

# ---- dataset tables (normalized, content-addressed) -------------------------
# Datasets are split three ways so identical content is stored once, not per
# version: an identity row per (package, name), a per-version link that carries
# only a small integer content_id, and a content-addressed profile keyed by
# (content_fp, schema_fp, fp_algo_version) shared across versions AND packages.
# The heavy row_sketch lives in its own table (kept out of the merge allowlist).

.ensure_dataset_tables <- function(con) {
  tables <- DBI::dbListTables(con)
  if (!"bioc_datasets" %in% tables) {
    DBI::dbExecute(con, "CREATE TABLE bioc_datasets (
      package TEXT NOT NULL, name TEXT NOT NULL, file TEXT, internal INTEGER,
      current_version TEXT, current_content_id INTEGER,
      PRIMARY KEY (package, name))")
  }
  if (!"bioc_dataset_versions" %in% tables) {
    DBI::dbExecute(con, "CREATE TABLE bioc_dataset_versions (
      package TEXT NOT NULL, name TEXT NOT NULL, version TEXT NOT NULL,
      content_id INTEGER NOT NULL, format TEXT, compression TEXT, confidence TEXT,
      is_current INTEGER NOT NULL DEFAULT 0,
      PRIMARY KEY (package, name, version))")
  }
  if (!"bioc_dataset_contents" %in% tables) {
    DBI::dbExecute(con, "CREATE TABLE bioc_dataset_contents (
      content_id INTEGER PRIMARY KEY,
      content_fp TEXT NOT NULL, schema_fp TEXT NOT NULL, fp_algo_version INTEGER NOT NULL,
      class TEXT, kind TEXT, nrow INTEGER, ncol INTEGER, n_missing_total INTEGER, columns TEXT,
      UNIQUE (content_fp, schema_fp, fp_algo_version))")
  }
  if (!"bioc_dataset_sketches" %in% tables) {
    DBI::dbExecute(con, "CREATE TABLE bioc_dataset_sketches (
      content_id INTEGER PRIMARY KEY, row_sketch TEXT)")
  }
  DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_bioc_dsv_content ON bioc_dataset_versions(content_id)")
  DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_bioc_dsc_schema ON bioc_dataset_contents(schema_fp)")
  invisible(NULL)
}

#' Migrate away from the pre-normalization flat bioc_datasets table (one row per
#' dataset per version, carrying columns/row_sketch inline). It collides by name
#' with the normalized identity table, so an incremental run against a database
#' that still holds it would fail the identity append. Drop it, and clear the
#' datasets_scanned sentinel on every package we are NOT writing this shard, so
#' the flat rows are rebuilt into the normalized tables instead of being skipped
#' as already scanned. The current shard's packages keep the marker just written
#' for them. Identified by the absence of the identity-only current_version
#' column, so it is a one-time no-op once the normalized schema is in place.
.migrate_legacy_dataset_table <- function(con, keep_pkgs = character(0L)) {
  tables <- DBI::dbListTables(con)
  if (!"bioc_datasets" %in% tables) return(invisible(NULL))
  if ("current_version" %in% DBI::dbListFields(con, "bioc_datasets")) {
    return(invisible(NULL))
  }
  DBI::dbExecute(con, "DROP TABLE bioc_datasets")
  if ("bioc_code_summary" %in% tables &&
      "datasets_scanned" %in% DBI::dbListFields(con, "bioc_code_summary")) {
    keep_pkgs <- unique(as.character(keep_pkgs))
    if (length(keep_pkgs) > 0L) {
      ph <- paste(rep("?", length(keep_pkgs)), collapse = ",")
      DBI::dbExecute(con, sprintf(
        "UPDATE bioc_code_summary SET datasets_scanned = NULL WHERE package NOT IN (%s)", ph),
        params = as.list(keep_pkgs))
    } else {
      DBI::dbExecute(con, "UPDATE bioc_code_summary SET datasets_scanned = NULL")
    }
  }
  invisible(NULL)
}

#' Write per-version dataset records into the four normalized tables. `df` is one
#' row per (package, version, dataset). Runs inside the caller's transaction.
.write_datasets_normalized <- function(con, df, pkgs) {
  .migrate_legacy_dataset_table(con, pkgs)
  .ensure_dataset_tables(con)
  # Per-package wipe: children (version links) then parents (identity). Contents
  # and sketches are shared/immutable and are reclaimed by GC, not deleted here.
  .delete_by_package(con, "bioc_dataset_versions", pkgs)
  .delete_by_package(con, "bioc_datasets",         pkgs)
  if (is.null(df) || nrow(df) == 0L) return(invisible(NULL))

  df$fp_algo_version <- as.integer(df$fp_algo_version)
  df$internal        <- as.integer(df$internal)
  df$is_current      <- as.integer(df$is_current)
  # Records without a content fingerprint (.R scripts, unreadable, S4 class-only)
  # have no profile to store; keep them out of the normalized tables.
  df <- df[!is.na(df$content_fp) & nzchar(df$content_fp), , drop = FALSE]
  if (nrow(df) == 0L) return(invisible(NULL))
  # Atomic vectors / matrices / S4 have values but no column schema, so schema_fp
  # is NA. Use an empty string so they still dedup by content and satisfy the
  # NOT NULL + UNIQUE(content_fp, schema_fp, fp_algo_version) constraint (a NULL
  # would make every such row distinct and get dropped by INSERT OR IGNORE).
  df$schema_fp[is.na(df$schema_fp)] <- ""

  # A single package version can surface one dataset name twice: an exported
  # data/ object and an internal sysdata object of the same name, or the same
  # object reached through two files. (package, name) is unique in bioc_datasets
  # and (package, name, version) in bioc_dataset_versions, so collapse to one
  # record per (package, name, version) up front, preferring the exported copy
  # (internal = 0 sorts first). Without this the version append fails the PK.
  df <- df[order(df$package, df$name, df$version, df$internal), , drop = FALSE]
  df <- df[!duplicated(paste(df$package, df$name, df$version, sep = "\x1f")), , drop = FALSE]

  # 1. Content-addressed profiles: one INSERT OR IGNORE per distinct fingerprint.
  ck  <- paste(df$content_fp, df$schema_fp, df$fp_algo_version, sep = "\x1f")
  cts <- df[!duplicated(ck), , drop = FALSE]
  DBI::dbExecute(con,
    "INSERT OR IGNORE INTO bioc_dataset_contents
       (content_fp, schema_fp, fp_algo_version, class, kind, nrow, ncol, n_missing_total, columns)
     VALUES (?,?,?,?,?,?,?,?,?)",
    params = list(cts$content_fp, cts$schema_fp, cts$fp_algo_version, cts$class,
                  cts$kind, cts$nrow, cts$ncol, cts$n_missing_total, cts$columns))

  ids <- DBI::dbGetQuery(con,
    "SELECT content_id, content_fp, schema_fp, fp_algo_version FROM bioc_dataset_contents")
  key_map <- stats::setNames(
    ids$content_id,
    paste(ids$content_fp, ids$schema_fp, ids$fp_algo_version, sep = "\x1f"))
  df$content_id <- unname(key_map[ck])

  # 2. Sketches: one INSERT OR IGNORE per content_id.
  sk <- df[!duplicated(df$content_id) & !is.na(df$row_sketch), c("content_id", "row_sketch"), drop = FALSE]
  if (nrow(sk) > 0L) {
    DBI::dbExecute(con,
      "INSERT OR IGNORE INTO bioc_dataset_sketches (content_id, row_sketch) VALUES (?, ?)",
      params = list(sk$content_id, sk$row_sketch))
  }

  # 3. Version links (package was wiped above, so a plain append is idempotent).
  ver <- df[, c("package", "name", "version", "content_id", "format", "compression", "confidence", "is_current"), drop = FALSE]
  DBI::dbAppendTable(con, "bioc_dataset_versions", ver)

  # 4. Identity, one per (package, name), stamped with the current version's content.
  cur <- df[df$is_current == 1L, , drop = FALSE]
  cur <- cur[!duplicated(paste(cur$package, cur$name, sep = "\x1f")), , drop = FALSE]
  if (nrow(cur) > 0L) {
    idn <- data.frame(package = cur$package, name = cur$name, file = cur$file,
                      internal = cur$internal, current_version = cur$version,
                      current_content_id = cur$content_id, stringsAsFactors = FALSE)
    DBI::dbAppendTable(con, "bioc_datasets", idn)
  }
  invisible(NULL)
}

#' Reclaim content/sketch rows no longer referenced by any version link.
.gc_dataset_contents <- function(con) {
  tables <- DBI::dbListTables(con)
  if (!"bioc_dataset_contents" %in% tables) return(invisible(NULL))
  DBI::dbExecute(con,
    "DELETE FROM bioc_dataset_sketches
      WHERE content_id NOT IN (SELECT content_id FROM bioc_dataset_versions)")
  DBI::dbExecute(con,
    "DELETE FROM bioc_dataset_contents
      WHERE content_id NOT IN (SELECT content_id FROM bioc_dataset_versions)")
  invisible(NULL)
}

#' Open (or create) the dataset SQLite database, ensuring the four normalized
#' dataset tables exist. Mirrors open_or_init_db() but for the data series.
#'
#' @param path File path for the dataset SQLite database.
#' @return An open DBI connection. Caller must dbDisconnect().
open_or_init_data_db <- function(path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  .ensure_dataset_tables(con)
  con
}

#' Open (or create) the pipeline SQLite database.
#'
#' If the file does not yet exist it is created. The three non-summary tables
#' (bioc_code_churn, bioc_api_history, bioc_metrics_failures) are created with
#' fixed schemas and indexes on first open. bioc_code_summary is created lazily
#' by upsert_shard the first time data is written (its schema is dynamic).
#'
#' @param path File path for the SQLite database.
#' @return An open DBI connection. The caller is responsible for calling
#'   DBI::dbDisconnect() when done.
open_or_init_db <- function(path) {
  con    <- DBI::dbConnect(RSQLite::SQLite(), path)
  tables <- DBI::dbListTables(con)

  if (!"bioc_code_churn" %in% tables) {
    DBI::dbExecute(con, "
      CREATE TABLE bioc_code_churn (
        package TEXT,
        version TEXT,
        file    TEXT,
        added   INTEGER,
        deleted INTEGER
      )")
  }

  if (!"bioc_api_history" %in% tables) {
    DBI::dbExecute(con, "
      CREATE TABLE bioc_api_history (
        package         TEXT,
        version         TEXT,
        exports_added   TEXT,
        exports_removed TEXT,
        n_exports       INTEGER
      )")
  }

  if (!"bioc_metrics_failures" %in% tables) {
    DBI::dbExecute(con, "
      CREATE TABLE bioc_metrics_failures (
        package              TEXT PRIMARY KEY,
        consecutive_failures INTEGER NOT NULL DEFAULT 0,
        last_attempt         TEXT
      )")
  }

  DBI::dbExecute(con,
    "CREATE INDEX IF NOT EXISTS idx_churn_pkg_ver ON bioc_code_churn(package, version)")
  DBI::dbExecute(con,
    "CREATE INDEX IF NOT EXISTS idx_churn_pkg ON bioc_code_churn(package)")
  DBI::dbExecute(con,
    "CREATE INDEX IF NOT EXISTS idx_api_pkg_ver ON bioc_api_history(package, version)")

  con
}

#' Query the latest analyzed version per package from the DB.
#'
#' Uses latest_release_date (set by add_cross_version_metrics on the newest
#' version row) as the primary signal. Falls back to a window-function query
#' over released/rowid for packages that lack that marker.
#'
#' Memory cost: O(n_packages), not O(n_rows).
#'
#' @param con Open DBI connection to the pipeline SQLite database.
#' @return data.frame with columns package (chr) and version (chr); one row
#'   per package. Empty data.frame when bioc_code_summary does not exist yet.
db_analyzed_state <- function(con) {
  tables <- DBI::dbListTables(con)
  if (!"bioc_code_summary" %in% tables) {
    return(data.frame(package = character(0L), version = character(0L),
                      stringsAsFactors = FALSE))
  }

  cols <- DBI::dbListFields(con, "bioc_code_summary")

  if ("latest_release_date" %in% cols) {
    # Primary: add_cross_version_metrics marks the newest-version row per package.
    primary <- DBI::dbGetQuery(con,
      "SELECT package, version
       FROM bioc_code_summary
       WHERE latest_release_date IS NOT NULL")
  } else {
    primary <- data.frame(package = character(0L), version = character(0L),
                          stringsAsFactors = FALSE)
  }

  # Fallback: packages with no non-NULL latest_release_date row (e.g. legacy data
  # or missing column). Use an explicit ORDER so the result does not depend on
  # SQLite internal row order.
  order_expr <- if ("released" %in% cols) {
    "ORDER BY released DESC, rowid DESC"
  } else {
    "ORDER BY rowid DESC"
  }
  fallback <- DBI::dbGetQuery(con, sprintf("
    SELECT package, version FROM (
      SELECT package, version,
             ROW_NUMBER() OVER (
               PARTITION BY package
               %s
             ) AS rn
      FROM bioc_code_summary
      WHERE package NOT IN (
        SELECT DISTINCT package FROM bioc_code_summary
        WHERE latest_release_date IS NOT NULL
      )
    ) WHERE rn = 1", order_expr))

  rbind(primary, fallback)
}

#' Upsert one shard's rows into the pipeline database in-place.
#'
#' For each package present in summary_df, deletes all prior rows from the
#' three metric tables (bioc_code_summary, bioc_code_churn, bioc_api_history),
#' then appends the fresh rows. Everything runs inside one transaction so the
#' DB is never left in a partially-written state.
#'
#' Schema growth for bioc_code_summary: if summary_df contains columns not yet
#' present in the table, ALTER TABLE ... ADD COLUMN is issued for each before
#' the append. If the table does not exist yet, it is created from summary_df
#' (schema-flexible) and indexed.
#'
#' Logical columns in all three data.frames are coerced to 0/1 INTEGER.
#'
#' @param con        Open DBI connection from open_or_init_db().
#' @param summary_df data.frame; columns package + version required.
#' @param churn_df   data.frame; columns package, version, file, added, deleted.
#' @param api_df     data.frame; columns package, version, exports_added,
#'   exports_removed, n_exports.
#' @param functions_df Optional data.frame of per-function detail (package,
#'   version, lang, name, exported, file, line, loc, n_params, cyclocomp).
#'   NULL (the default) leaves bioc_functions untouched.
#' @param edges_df   Optional data.frame of per-call-edge detail (package,
#'   version, graph, from, to). NULL (the default) leaves bioc_call_edges
#'   untouched. Detail is expected to cover each package's latest version only;
#'   the delete-by-package step still clears any prior-version detail rows so no
#'   stale rows survive a re-analysis.
#' @return invisible(NULL)
upsert_shard <- function(con, summary_df, churn_df, api_df,
                         functions_df = NULL, edges_df = NULL) {
  pkgs <- unique(as.character(summary_df$package))
  if (length(pkgs) == 0L) return(invisible(NULL))

  # Defensive dedup: bioc_code_summary has a UNIQUE(package, version) index, so a
  # single package that somehow yields two rows for one version would otherwise
  # abort the whole shard. Keep the last occurrence per (package, version).
  dup_key <- paste(summary_df$package, summary_df$version, sep = "\x1f")
  if (anyDuplicated(dup_key)) {
    keep_row  <- !duplicated(dup_key, fromLast = TRUE)
    summary_df <- summary_df[keep_row, , drop = FALSE]
  }

  DBI::dbWithTransaction(con, {
    # -- Delete prior rows for these packages from every table ---------------
    # Detail tables are wiped per-package (not per-version) so a package moving
    # to a new latest version does not leave its previous version's detail rows.
    .delete_by_package(con, "bioc_code_summary", pkgs)
    .delete_by_package(con, "bioc_code_churn",   pkgs)
    .delete_by_package(con, "bioc_api_history",  pkgs)
    if (!is.null(functions_df)) .delete_by_package(con, "bioc_functions",  pkgs)
    if (!is.null(edges_df))     .delete_by_package(con, "bioc_call_edges", pkgs)

    # -- Insert fresh summary rows (with schema-growth handling) -------------
    summary_write <- .coerce_logicals(summary_df)
    tables        <- DBI::dbListTables(con)

    if (!"bioc_code_summary" %in% tables) {
      # First-ever write: create the table from the data.frame schema.
      DBI::dbWriteTable(con, "bioc_code_summary", summary_write,
                        row.names = FALSE, overwrite = FALSE, append = FALSE)
    } else {
      # Possibly new columns have appeared since the table was first created.
      existing_cols <- DBI::dbListFields(con, "bioc_code_summary")
      for (col in setdiff(names(summary_write), existing_cols)) {
        col_type <- if (is.integer(summary_write[[col]])) "INTEGER"
                    else if (is.numeric(summary_write[[col]])) "REAL"
                    else "TEXT"
        DBI::dbExecute(con,
          sprintf("ALTER TABLE bioc_code_summary ADD COLUMN \"%s\" %s",
                  col, col_type))
      }
      DBI::dbAppendTable(con, "bioc_code_summary", summary_write)
    }
    DBI::dbExecute(con,
      "CREATE UNIQUE INDEX IF NOT EXISTS idx_summary_pkg_ver
       ON bioc_code_summary(package, version)")

    # -- Insert fresh churn rows ---------------------------------------------
    churn_write <- .coerce_logicals(churn_df)
    if (!is.null(churn_write) && nrow(churn_write) > 0L) {
      DBI::dbAppendTable(con, "bioc_code_churn", churn_write)
    }

    # -- Insert fresh api rows -----------------------------------------------
    api_write <- .coerce_logicals(api_df)
    if (!is.null(api_write) && nrow(api_write) > 0L) {
      DBI::dbAppendTable(con, "bioc_api_history", api_write)
    }

    # -- Insert fresh per-function / per-call-edge detail --------------------
    .append_detail_table(con, "bioc_functions",  functions_df)
    .append_detail_table(con, "bioc_call_edges", edges_df)
  })

  invisible(NULL)
}

#' Upsert one shard's dataset rows into the dataset database in-place.
#'
#' Runs the normalized-dataset write and the content GC inside one transaction
#' on the dataset connection. Separated from upsert_shard so the code and
#' dataset tables live in different files.
#'
#' @param data_con    Open DBI connection from open_or_init_data_db().
#' @param datasets_df  Per-(package, version, dataset) rows, or NULL.
#' @param pkgs         Character vector of packages written this shard.
#' @return invisible(NULL)
upsert_datasets <- function(data_con, datasets_df, pkgs) {
  pkgs <- unique(as.character(pkgs))
  DBI::dbWithTransaction(data_con, {
    .write_datasets_normalized(data_con, datasets_df, pkgs)
    .gc_dataset_contents(data_con)
  })
  invisible(NULL)
}

#' Compute a SHA-256 fingerprint over the current package:version set in the DB.
#'
#' Queries only the two key columns (result bounded to O(n_packages)) and
#' hashes the sorted "package:version" strings. Semantically equivalent to
#' metrics_fingerprint() but reads from the live DB rather than a data.frame.
#'
#' @param con Open DBI connection to the pipeline SQLite database.
#' @return 64-character lower-case hex string (SHA-256).
db_fingerprint <- function(con) {
  tables <- DBI::dbListTables(con)
  if (!"bioc_code_summary" %in% tables) {
    return(digest::digest("", algo = "sha256", serialize = FALSE))
  }
  df   <- DBI::dbGetQuery(con,
    "SELECT package, version FROM bioc_code_summary ORDER BY package, version")
  keys <- if (nrow(df) == 0L) character(0L) else paste(df$package, df$version, sep = ":")
  digest::digest(paste(keys, collapse = ","), algo = "sha256", serialize = FALSE)
}
