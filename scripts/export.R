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

#' Build a per-DB insight manifest matching the pipeline MANIFEST SCHEMA.
#'
#' All values are measured from `con`; a missing table counts 0 and a missing
#' numeric column yields NULL mean/median (rendered as JSON null). bootstrap's
#' n_universe/n_remaining may be NULL when unmeasurable.
#'
#' @param con         Open DBI connection to the pipeline SQLite database.
#' @param series      "code" or "data".
#' @param repo        "owner/name" of the publishing repo.
#' @param db_filename The asset filename this manifest describes.
#' @param db_bytes    On-disk size of the DB file, in bytes.
#' @param tables      Character vector of table names to report row counts for.
#' @param fp_table    Table to fingerprint.
#' @param fp_cols     Columns within fp_table forming the fingerprint key.
#' @param pkg_table   Table to count DISTINCT package from for n_packages.
#' @param ver_table   Table to count rows from for n_versions.
#' @param stat_table  Table to probe for stat_cols.
#' @param stat_cols   Character vector of numeric columns to summarise.
#' @param bootstrap   list(n_analyzed, n_universe, n_remaining, bootstrap_complete).
#'   n_universe/n_remaining may be NULL.
#' @return A named list matching the MANIFEST SCHEMA.
build_manifest <- function(con, series, repo, db_filename, db_bytes,
                           tables, fp_table, fp_cols, pkg_table, ver_table,
                           stat_table, stat_cols, bootstrap) {
  present <- DBI::dbListTables(con)
  count_tbl <- function(t) {
    if (!t %in% present) return(0L)
    as.integer(DBI::dbGetQuery(con, sprintf('SELECT COUNT(*) n FROM "%s"', t))$n)
  }
  table_counts <- stats::setNames(lapply(tables, count_tbl), tables)

  n_packages <- if (pkg_table %in% present) {
    as.integer(DBI::dbGetQuery(con,
      sprintf('SELECT COUNT(DISTINCT package) n FROM "%s"', pkg_table))$n)
  } else 0L
  n_versions <- count_tbl(ver_table)

  # Fingerprint over the concatenation of fp_cols keys, ordered by the SQL
  # tuple (not by sorting the already-concatenated strings). Code-series
  # keys join fields with ":" (matching db_fingerprint()); data-series keys
  # join fields with "\x1f" per the manifest schema.
  fp_sep <- if (identical(series, "code")) ":" else "\x1f"
  fingerprint <- {
    if (!fp_table %in% present) {
      digest::digest("", algo = "sha256", serialize = FALSE)
    } else {
      cols <- paste(sprintf('"%s"', fp_cols), collapse = ", ")
      df <- DBI::dbGetQuery(con,
        sprintf('SELECT %s FROM "%s" ORDER BY %s', cols, fp_table, cols))
      keys <- if (nrow(df) == 0L) character(0L) else
        apply(df, 1L, function(r) paste(r, collapse = fp_sep))
      # Rows are ordered by SQLite's ORDER BY (BINARY collation, i.e. byte
      # order) *before* concatenation, exactly matching db_fingerprint()'s
      # "ORDER BY package, version". Sorting the already-concatenated
      # "package:version" strings in R instead is NOT equivalent: whenever
      # one key is a prefix of another followed by a character below ':'
      # (0x3a) -- e.g. package "Rcpp" vs "Rcpp11" -- tuple order and
      # concatenated-string order disagree, so the two fingerprints would
      # diverge for real CRAN data.
      digest::digest(paste(keys, collapse = ","),
                     algo = "sha256", serialize = FALSE)
    }
  }

  # Stats: mean/median per column that exists AND is numeric, else NULL.
  # A non-numeric column (e.g. character) must never be coerced into a
  # fabricated statistic.
  stat_fields <- list()
  stat_cols_present <- if (stat_table %in% present) DBI::dbListFields(con, stat_table) else character(0L)
  for (col in stat_cols) {
    if (col %in% stat_cols_present) {
      v <- DBI::dbGetQuery(con, sprintf('SELECT "%s" AS v FROM "%s"', col, stat_table))$v
      if (is.numeric(v)) {
        v <- v[!is.na(v)]
        stat_fields[[paste0(col, "_mean")]]   <- if (length(v)) mean(v) else NULL
        stat_fields[[paste0(col, "_median")]] <- if (length(v)) stats::median(v) else NULL
      } else {
        stat_fields[[paste0(col, "_mean")]]   <- NULL
        stat_fields[[paste0(col, "_median")]] <- NULL
      }
    } else {
      stat_fields[[paste0(col, "_mean")]]   <- NULL
      stat_fields[[paste0(col, "_median")]] <- NULL
    }
  }

  list(
    schema_version = 1L,
    series         = series,
    repo           = repo,
    db_filename    = db_filename,
    generated_at   = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    db_bytes       = round(as.numeric(db_bytes)),
    fingerprint    = fingerprint,
    n_packages     = n_packages,
    n_versions     = n_versions,
    tables         = table_counts,
    stats          = stat_fields,
    bootstrap      = list(
      n_analyzed         = bootstrap$n_analyzed,
      n_universe         = bootstrap$n_universe,
      n_remaining        = bootstrap$n_remaining,
      bootstrap_complete = isTRUE(bootstrap$bootstrap_complete)
    )
  )
}

#' Union `pkgs` into a sorted, deduped newline file at `path` (accumulates the
#' run's changed set across shards for the changelog).
#'
#' @param path Newline-delimited text file. Created if absent.
#' @param pkgs Character vector of package names touched this run.
#' @return Invisibly NULL.
record_changed_packages <- function(path, pkgs) {
  existing <- if (file.exists(path)) readLines(path, warn = FALSE) else character(0L)
  all <- sort(unique(c(existing, as.character(pkgs))))
  all <- all[nzchar(all)]
  writeLines(all, path)
  invisible(NULL)
}

#' Read the accumulated changed-package set (empty vector if absent).
#'
#' @param path Newline-delimited text file as written by record_changed_packages().
#' @return Character vector, sorted as stored; character(0L) when path is absent.
read_changed_packages <- function(path) {
  if (!file.exists(path)) return(character(0L))
  x <- readLines(path, warn = FALSE)
  x[nzchar(x)]
}

#' Format a numeric delta as "(+N)"/"(-N)"; "(n/a)" when either side is
#' missing so an unmeasurable quantity is never rendered as a fabricated 0.
#'
#' @param cur  Current value, or NULL/length-0 if unmeasured.
#' @param prev Previous value, or NULL/length-0 if unmeasured.
#' @return A one-line string.
.fmt_delta <- function(cur, prev) {
  if (is.null(cur) || is.null(prev) || length(cur) == 0L || length(prev) == 0L) return("(n/a)")
  d <- cur - prev
  sprintf("(%s%s)", if (d >= 0) "+" else "-", format(abs(d), trim = TRUE))
}

#' Build the "Changes since <prev>" markdown section.
#'
#' Every number rendered comes from `today`/`prev` (already-parsed manifests)
#' or `changed_pkgs`; nothing is invented. A stat/table present on only one
#' side prints its known value (or "n/a") with an "(n/a)" delta rather than a
#' misleading "+N"/"-N".
#'
#' @param today  Manifest list (already-parsed) for this release.
#' @param prev   Manifest list for the previous dated release, or NULL for
#'   the first dated release of a series.
#' @param changed_pkgs Character vector of packages touched this run.
#' @param cap    Max packages listed (default 25).
#' @return A single markdown string.
build_changelog <- function(today, prev, changed_pkgs, cap = 25L) {
  L <- character(0L)
  if (is.null(prev)) {
    L <- c(L, "## Changes since (first dated release)", "",
           "Initial dated release; no prior snapshot to diff.", "",
           sprintf("- Packages: %s", today$n_packages),
           sprintf("- Versions: %s", today$n_versions),
           sprintf("- DB size: %s bytes", format(today$db_bytes, big.mark = ",")))
    return(paste(L, collapse = "\n"))
  }
  # today$series is always populated by build_manifest(); the "release"
  # fallback only guards a directly-constructed today list in tests. Either
  # way sprintf() must never receive a NULL/length-0 argument here, or the
  # whole heading line silently vanishes from L.
  prev_tag <- prev$dated_tag %||% sprintf("%s-<prev>", today$series %||% "release")
  L <- c(L, sprintf("## Changes since %s", prev_tag), "",
    sprintf("- Packages: %s %s", today$n_packages, .fmt_delta(today$n_packages, prev$n_packages)),
    sprintf("- Versions: %s %s", today$n_versions, .fmt_delta(today$n_versions, prev$n_versions)),
    sprintf("- DB size: %s bytes %s", format(today$db_bytes, big.mark = ","),
            .fmt_delta(today$db_bytes, prev$db_bytes)))
  # Stats deltas (union of keys, sorted).
  for (k in sort(unique(c(names(today$stats), names(prev$stats))))) {
    label <- sub("_mean$", " mean", sub("_median$", " median", k))
    L <- c(L, sprintf("- %s: %s %s", label, today$stats[[k]] %||% "n/a",
                      .fmt_delta(today$stats[[k]], prev$stats[[k]])))
  }
  # Per-table row-count deltas.
  L <- c(L, "- Table row counts:")
  for (t in sort(unique(c(names(today$tables), names(prev$tables))))) {
    L <- c(L, sprintf("  - %s: %s %s", t, today$tables[[t]] %||% "n/a",
                      .fmt_delta(today$tables[[t]], prev$tables[[t]])))
  }
  # Changed-package list, capped.
  n <- length(changed_pkgs)
  if (n == 0L) {
    L <- c(L, "- Newly or re-analyzed packages (0): none")
  } else {
    shown <- head(sort(changed_pkgs), cap)
    L <- c(L, sprintf("- Newly or re-analyzed packages (%d shown of %d):", length(shown), n),
           sprintf("  - %s", paste(shown, collapse = ", ")))
  }
  paste(L, collapse = "\n")
}

#' Render the full release notes for a DB.
#'
#' @param manifest A manifest list as produced by build_manifest().
#' @param changelog_md The markdown string returned by build_changelog().
#' @param title Release title, rendered as the top-level heading.
#' @return Character vector of markdown lines.
render_release_notes <- function(manifest, changelog_md, title) {
  c(
    sprintf("# %s", title), "",
    sprintf("- DB: `%s` (%s bytes)", manifest$db_filename, format(manifest$db_bytes, big.mark = ",")),
    sprintf("- Generated: %s", manifest$generated_at),
    sprintf("- Fingerprint: `%s`", manifest$fingerprint),
    sprintf("- Packages: %s   Versions: %s", manifest$n_packages, manifest$n_versions),
    sprintf("- Bootstrap: %s/%s analyzed (%s remaining, complete=%s)",
            manifest$bootstrap$n_analyzed,
            manifest$bootstrap$n_universe %||% "?",
            manifest$bootstrap$n_remaining %||% "?",
            tolower(as.character(manifest$bootstrap$bootstrap_complete))),
    "",
    changelog_md, ""
  )
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
