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
# only a small integer content_id, and a profile keyed by the digest of
# everything that profile records, shared across versions AND packages. The
# heavy row_sketch lives in its own table (kept out of the merge allowlist).

# What a dataset record says about the data itself, and so belongs on the
# profile row shared by every copy of that data.
#
# What decides this list is what the field describes, not what a fingerprint
# covers, and that is a change. It used to be decided by coverage, because the
# row was keyed on the two fingerprints and they are narrower than "describes
# the data": content_fp is a hash over each column's type name and the bytes of
# its cells, schema_fp is over `name:type` per column, and between them they
# reach the cells, the column count, order, names, types and lengths and
# nothing else. A time zone, a class, a series start, a projection, a factor's
# declared levels and a table's row names are invisible to both. The row is
# written with INSERT OR IGNORE from whichever record reaches it first, so any
# field the key did not separate was published for every dataset sharing the
# key with the answer belonging to one of them.
#
# The key is now a digest of the whole recorded profile, so two records that
# differ in any of this get two rows and neither is handed the other's
# measurement. That is what lets these fields sit here, where they describe
# what they describe and where the catalog reads them.
#
# It also reaches what no relocation could. The per-column time zone, label,
# comment, units, declared levels and projection ride inside the `columns` and
# `elements` arrays, which are the profile payload and by far the largest
# values written here: a per-version copy of them is exactly the duplication
# this table exists to remove, so they could never move to the version link.
# Keying on the digest separates them where moving them could not.
#
# Types are declared rather than inferred from whatever a shard happens to
# carry. A shard whose every density is missing would otherwise fix that column
# as text for good, and the column would then read back as text forever.
.DATASET_CONTENT_COLS <- c(
  # What the object calls itself, and what shape of thing it is.
  class = "TEXT", kind = "TEXT", frame_class = "TEXT",
  object_system = "TEXT", s4_package = "TEXT",

  nrow = "INTEGER", ncol = "INTEGER",
  length = "INTEGER", n_cols = "INTEGER", n_unique = "INTEGER",
  n_missing_total = "INTEGER", columns = "TEXT",
  shape_fp = "TEXT",
  dim = "TEXT", n_dim = "INTEGER",
  n_stored = "INTEGER", n_cells = "INTEGER", density = "REAL",
  geom_type = "TEXT", is_geometry = "INTEGER", n_geometries = "INTEGER",
  is_spatial = "INTEGER", bbox = "TEXT",

  # What a column or a grid holds, on the same terms summary() reports it.
  type = "TEXT", mean = "REAL", median = "REAL", q1 = "REAL", q3 = "REAL",
  sd = "REAL", col_min = "REAL", col_max = "REAL",
  skewness = "REAL", kurtosis = "REAL",
  n_outliers = "INTEGER", n_outliers_low = "INTEGER", n_outliers_high = "INTEGER",
  mode_value = "REAL", mode_share = "REAL",
  n_true = "INTEGER", n_false = "INTEGER",
  min_nchar = "INTEGER", max_nchar = "INTEGER", n_blank = "INTEGER",
  n_zero = "INTEGER", p_zero = "REAL",
  n_infinite = "INTEGER", max_infinite = "INTEGER", min_infinite = "INTEGER",
  is_integer_valued = "INTEGER", sort_order = "TEXT",
  n_missing_leading = "INTEGER", n_missing_trailing = "INTEGER",
  max_missing_run = "INTEGER",
  # Which of two things the summary beside it was taken over, cells or stored
  # values. Decided by how the object holds its numbers, and a dense grid and a
  # sparse one holding the same numbers hash differently, so two records on this
  # row were read the same way and the word is the same for both.
  summary_over = "TEXT",

  # What a factor uses and what it declares. A spare level nobody used changes
  # `levels` and `n_levels` and changes no cell, which is why this pair could
  # not be separated by the fingerprints and had to be separated by the key.
  levels = "TEXT", n_levels = "INTEGER", level_counts = "TEXT",
  is_factor = "INTEGER", is_ordered = "INTEGER",

  # How a frame is grouped and keyed, all of it attributes: two frames of the
  # same numbers, one grouped and one not, hash identically.
  is_grouped = "INTEGER", group_vars = "TEXT", n_groups = "INTEGER",
  is_rowwise = "INTEGER", dt_key = "TEXT", dt_indices = "TEXT",

  # Labels rather than values. Row names, dimension names and the margin labels
  # of a grid are written beside the cells and none of them is hashed.
  has_rownames = "INTEGER", has_dimnames = "INTEGER", dimnames = "TEXT",

  # Whatever else was written beside the values.
  label = "TEXT", comment = "TEXT", units = "TEXT", attrs_other = "TEXT",

  # The time zone an instant is stored in. The cells of a POSIXct are seconds
  # since the epoch, so the same moments written in two zones hash the same and
  # read as two different local times. index_tz below is the zone of a series'
  # index, a different field on a different kind of object.
  tz = "TEXT",

  # What a list holds. The inner row count is the one that matters: a nested
  # table reports its group count as its rows.
  element_class = "TEXT", element_classes = "TEXT",
  element_len_min = "INTEGER", element_len_max = "INTEGER",
  element_len_total = "INTEGER", max_depth = "INTEGER",
  inner_nrow_total = "INTEGER", inner_ncol = "INTEGER",
  # The names a list files its slots under, and the columns of an inner table.
  element_names = "TEXT", inner_names = "TEXT",
  inner_schema_varies = "INTEGER",

  # Where a series starts and how often it is sampled. The `tsp` attribute is
  # not hashed, so twelve monthly readings from 2000 and the same twelve read as
  # quarterly readings from 1990 hold the same numbers.
  ts_start = "REAL", ts_end = "REAL", ts_frequency = "REAL", frequency = "REAL",
  ts_span = "REAL",

  # The index of an indexed series, which is an attribute beside the values
  # rather than a column of them, and every figure taken off it.
  index_start = "TEXT", index_end = "TEXT", index_n = "INTEGER",
  index_class = "TEXT", index_span = "REAL", index_tz = "TEXT",
  index_delta = "REAL", index_regular = "INTEGER",
  index_n_gaps = "INTEGER", index_max_gap = "REAL",
  index_sorted = "INTEGER", index_has_duplicates = "INTEGER",

  # Spatial detail read off the coordinates, and the projection written beside
  # them. The coordinates are hashed and the projection is not, so a bounding
  # box of the same numbers can mean two different places.
  geom_dimension = "TEXT", n_empty = "INTEGER",
  crs_input = "TEXT", crs_epsg = "INTEGER", crs_wkt = "TEXT",

  # How a matrix is held, all of it read off the class rather than the cells.
  matrix_shape = "TEXT", matrix_storage = "TEXT",
  matrix_uplo = "TEXT", matrix_diag = "TEXT", matrix_value_type = "TEXT",

  # Raster metadata, read off the object's slots.
  n_layers = "INTEGER", layer_names = "TEXT", layer_min = "TEXT",
  layer_max = "TEXT", resolution = "TEXT", nodata_value = "REAL",
  in_memory = "INTEGER",

  # Which kind of missing, and which end an infinity runs to. Both are
  # column-level too and ride in the columns JSON; these are for the objects
  # that are one vector rather than a table.
  n_nan = "INTEGER", n_infinite_pos = "INTEGER", n_infinite_neg = "INTEGER",

  # A broken-down time: how many fields it is stored in, and the years it
  # covers, which is what is recoverable without rebuilding the instants.
  n_fields = "INTEGER", year_min = "INTEGER", year_max = "INTEGER",

  # Sparse and graph. All four are counted off the values themselves.
  n_nonzero = "INTEGER", n_vertices = "INTEGER", n_edges = "INTEGER",
  directed = "INTEGER",

  # A list's elements profiled the way a frame's columns are, in the same
  # shape, so one renderer serves both.
  elements = "TEXT",

  # Per element rather than reduced across the list. The aggregates cannot say
  # how big any one slot was, which is the question a list of folds raises.
  element_lens = "TEXT", element_inner_nrow = "TEXT",

  # Where a grid's variation runs. A summary over every cell reads a matrix and
  # its transpose identically; the means along each margin do not. Fourteen
  # columns whatever the size of the matrix, because the margins are summarised
  # rather than stored.
  row_mean_min = "REAL", row_mean_q1 = "REAL", row_mean_median = "REAL",
  row_mean_mean = "REAL", row_mean_q3 = "REAL", row_mean_max = "REAL",
  row_mean_sd = "REAL",
  col_mean_min = "REAL", col_mean_q1 = "REAL", col_mean_median = "REAL",
  col_mean_mean = "REAL", col_mean_q3 = "REAL", col_mean_max = "REAL",
  col_mean_sd = "REAL",

  # How much of a column profile the record beside it carries, which used to be
  # unsaid. A wide object's column list was truncated at 512 entries with no
  # marker, so a 19,763 column frame stored the word "numeric" 512 times and
  # nothing said the other 19,251 were gone. Now the analyzer declares a depth
  # and drops no column: `full` is every column and every statistic, `reduced`
  # is every column with four counts and no per-column fingerprint, `none`
  # replaces the array entirely with the whole-object summary and the
  # row_mean_*/col_mean_* margins, so a NULL columns value beside `none` is the
  # profile rather than a gap in it, and `structural` is every column named and
  # typed with nothing counted, because no value was read. ncol is the true
  # width at every depth.
  column_detail = "TEXT",

  # Slots of a list that hold nothing at all. They count towards its length and
  # they draw as nothing, so a list of ten with four of them empty is not the
  # list its length says it is.
  n_empty_slots = "INTEGER",

  # How many bytes of column profile this row does NOT carry. Zero on a row
  # that carries all of it, which is every honest row; a count says the profile
  # was over MAX_DATASET_COLUMNS_BYTES and was refused rather than stored. It
  # is written by the pipeline rather than read from the analyzer, so that a
  # reader can tell an object with no columns from one whose columns would not
  # fit through the load.
  columns_refused_bytes = "INTEGER"
)

# How one file happened to store the data, which is not a property of the data
# at all: the same table saved twice can differ in every one of these. R's
# serialization format has versions, and a version 3 file cannot be read by R
# before 3.5.0, so this is the difference between a dataset a reader can open
# and one they cannot.
#
# Nothing describing the data belongs here. That was tried, for the fields the
# fingerprints do not cover, and it could not work: it could not reach the two
# profile arrays, which carry the same properties per column and cannot be
# copied per version; it left `levels`, `n_levels` and `inner_schema_varies`
# colliding anyway; and it took `class` and `kind` away from the table the
# catalog reads them off. Keying the profile row on its own digest fixes all of
# that at once, and this list goes back to being about the file.
.DATASET_VERSION_COLS <- c(
  format_version = "INTEGER", compressed_bytes = "INTEGER", notes = "TEXT",
  # A file that is not what its name says: which separator would work, and how
  # many columns it would give. A property of this file, not of the data.
  delimiter_looks_like = "TEXT", delimiter_would_give_ncol = "INTEGER"
)

# Where a dataset was found. Not a property of its contents: the same data can
# sit under data/ in one package and inst/extdata in another, and only the first
# is loadable by name.
.DATASET_IDENTITY_COLS <- c(
  origin_dir = "TEXT",
  # The title of the help page documenting this dataset. Not a property of the
  # data: two packages carrying identical bytes may document them differently,
  # or one may not document them at all.
  title = "TEXT"
)

# How a profile digest is built. The pair separator joins a field's name, the
# byte length of its value and the value; the field separator joins the pairs.
# Both are control characters, which JSON escapes rather than carries, so
# neither can appear inside a value the analyzer emits. The length is folded in
# so that text shifting across a field boundary cannot produce the same input
# from two different profiles.
.DATASET_DIGEST_PAIR_SEP  <- "\x1e"
.DATASET_DIGEST_FIELD_SEP <- "\x1f"

# How much profile text one digest pass concatenates at a time. The values
# being hashed include the column profile, which is bounded at
# MAX_DATASET_COLUMNS_BYTES per row and nothing smaller, so hashing a whole
# table in one vectorized pass would hold a second copy of it in memory.
.DATASET_DIGEST_CHUNK_BYTES <- 16 * 1024^2

# How much profile text the re-key migration reads at once, and how many rows
# it will take to reach it. The migration digests rows that are already stored,
# so it reads them back, and the same bound applies: one row's column profile
# runs to MAX_DATASET_COLUMNS_BYTES and nothing smaller. A batch counted in
# rows is therefore a batch whose size nobody stated, and on a catalog of
# profiles near the cap it is the published table read whole with the digest's
# working copies on top of it. Counted in bytes it is what the batch actually
# holds. The row ceiling is the other end of the same bound: a table of tiny
# profiles would otherwise bind a quarter of a million values into one
# statement on its way to the byte budget.
.DATASET_REKEY_BATCH_BYTES <- 32 * 1024^2
.DATASET_REKEY_BATCH_ROWS  <- 2000L

#' Digest the profile a dataset record carries, field by field.
#'
#' This is the uniqueness key of the content-addressed row, and it exists
#' because the fingerprints beside it are not one. content_fp is a hash over
#' each column's type name and cell bytes and schema_fp is over `name:type`,
#' so between them they cover the cells, the column count, order, names, types
#' and lengths, and nothing else. Everything the reader took off an attribute
#' or off the class vector is invisible to both: a time zone, a class, a series
#' start, a projection, a factor's declared levels, a table's row names.
#'
#' The row is written with INSERT OR IGNORE from the first record that reaches
#' it, so a key that does not separate those fields hands every later dataset
#' sharing the key whichever answer the first one had. Digesting the whole
#' recorded profile means two profiles that differ in any recorded way get two
#' rows, and no dataset is ever handed another's measurement.
#'
#' It is not a replacement for content_fp and does not change what it means.
#' content_fp stays exactly as the reader computes it and stays on the row,
#' because it is the signal behind "the same data ships in N packages" and a
#' surrogate keyed finer would undercount that.
#'
#' Encoding, chosen so that a value can only ever hash to itself and so that
#' the digest describes the row as it is stored:
#'   - a present field folds in its name, the byte length of its value and the
#'     value, so no value can impersonate a separator or another field;
#'   - a field that is missing, and a field the shard's frame does not carry at
#'     all, both contribute nothing. A shard is one analyzer invocation per
#'     package, so a raster field is a column in a shard that read a raster and
#'     absent in one that did not, and the same record has to digest alike in
#'     both. It also means declaring a column the analyzer does not emit yet
#'     leaves every digest already in the table alone;
#'   - doubles are rendered at full precision one element at a time. format()
#'     would choose a width from whatever else is in the shard, so the same
#'     value would digest differently in two runs.
#'
#' @param df Data frame of dataset records, or of content rows read back.
#' @return Character vector of 64-character lower-case hex digests, one per row.
.dataset_profile_fp <- function(df) {
  n <- nrow(df)
  if (n == 0L) return(character(0L))
  cols <- sort(intersect(c("content_fp", "schema_fp", names(.DATASET_CONTENT_COLS)),
                         names(df)))
  if (!length(cols)) return(rep(NA_character_, n))

  parts <- lapply(cols, function(k) {
    v <- df[[k]]
    # Logicals become integers first, because that is what the writer stores
    # and a field arrives from the parser as either, depending on whether one
    # package's records left it empty.
    if (is.logical(v)) v <- as.integer(v)
    # is.na() and not is.nan(): SQLite has no NaN and RSQLite writes one as
    # NULL, so a record holding NaN and a record holding nothing store the same
    # row and have to digest alike. An infinity does store, and keeps its own.
    absent <- is.na(v)
    s <- if (is.double(v)) {
      sprintf("%.17g", v)
    } else if (is.character(v)) {
      # As bytes, so the same characters marked latin1 and marked UTF-8 are one
      # value rather than two: they store as the same text.
      enc2utf8(v)
    } else {
      as.character(v)
    }
    s[absent] <- ""
    # Each field carries its own leading separator rather than the join
    # supplying one, so an absent field contributes literally nothing and not
    # an empty slot: a column declared before the analyzer emits it must not
    # move the digest of every row already in the table.
    out <- paste0(.DATASET_DIGEST_FIELD_SEP, k, .DATASET_DIGEST_PAIR_SEP,
                  nchar(s, type = "bytes"), .DATASET_DIGEST_PAIR_SEP, s)
    out[absent] <- ""
    out
  })

  weight <- rep(0, n)
  for (p in parts) weight <- weight + nchar(p, type = "bytes")

  out <- character(n)
  lo <- 1L
  while (lo <= n) {
    hi  <- lo
    acc <- weight[[lo]]
    while (hi < n && acc + weight[[hi + 1L]] <= .DATASET_DIGEST_CHUNK_BYTES) {
      hi  <- hi + 1L
      acc <- acc + weight[[hi]]
    }
    idx  <- seq.int(lo, hi)
    keys <- do.call(paste0, lapply(parts, `[`, idx))
    out[idx] <- vapply(keys, function(k)
      digest::digest(k, algo = "sha256", serialize = FALSE),
      character(1L), USE.NAMES = FALSE)
    lo <- hi + 1L
  }
  out
}

#' Take off the version link the columns that describe the data, not the file.
#'
#' They were moved there while the profile row was keyed on the fingerprints
#' and so could not hold them, and a database written in that state exists.
#' The profile row is keyed on its own digest now and holds them again, and the
#' copy on the link would otherwise sit there carrying whatever the run that
#' wrote it recorded, never written again and never removed: a column half
#' filled with values from a schema nobody can look up.
#'
#' Not copied onto the profile row first. A profile row can outlive the record
#' that minted it and nothing distinguishes that case from the one where the
#' link's value is the row's own, so copying would carry a wrong answer
#' forward. The profile fills in from the analyzer as each package is scanned
#' again, which the analyzer-version invalidation already forces on an upgrade,
#' so the gap is the convergence window rather than a hole left open.
#'
#' Identified as columns on the version table that are declared on the content
#' row: nothing else can produce that overlap. A one-time no-op afterwards.
.drop_relocated_version_columns <- function(con) {
  if (!"bioc_dataset_versions" %in% DBI::dbListTables(con)) return(invisible(NULL))
  moved <- intersect(DBI::dbListFields(con, "bioc_dataset_versions"),
                     names(.DATASET_CONTENT_COLS))
  if (!length(moved)) return(invisible(NULL))
  # SQLite rewrites every row of the table once per dropped column, so on a
  # database downloaded from a release this is seconds each rather than
  # nothing, once. Said out loud because a run that stops here otherwise looks
  # like a run that hung.
  cat(sprintf("moving %d column%s off bioc_dataset_versions: %s\n",
              length(moved), if (length(moved) == 1L) "" else "s",
              paste(moved, collapse = ", ")), file = stdout())
  flush(stdout())
  for (col in moved) {
    tryCatch(
      DBI::dbExecute(con, sprintf('ALTER TABLE bioc_dataset_versions DROP COLUMN "%s"', col)),
      error = function(e) {
        # SQLite refuses to drop a column an index or a constraint names. None
        # of these is one, and a refusal is worth saying out loud rather than
        # leaving a column nobody can explain.
        cat(sprintf("could not drop %s from bioc_dataset_versions: %s\n",
                    col, conditionMessage(e)), file = stdout())
      })
  }
  invisible(NULL)
}

#' Add any dataset column the analyzer now emits that the table has not seen.
#' Mirrors what bioc_code_summary already does for its own new columns; without
#' it the widened CREATE only ever applies to a database built from nothing.
.ensure_dataset_columns <- function(con) {
  add <- function(table, spec) {
    if (!table %in% DBI::dbListTables(con)) return(invisible(NULL))
    existing <- DBI::dbListFields(con, table)
    for (col in setdiff(names(spec), existing)) {
      DBI::dbExecute(con, sprintf('ALTER TABLE %s ADD COLUMN "%s" %s',
                                  table, col, spec[[col]]))
    }
    invisible(NULL)
  }
  .drop_relocated_version_columns(con)
  add("bioc_dataset_contents", .DATASET_CONTENT_COLS)
  add("bioc_dataset_versions", .DATASET_VERSION_COLS)
  add("bioc_datasets", .DATASET_IDENTITY_COLS)
  # After the widening, so the digest of an existing row is taken over the
  # shape the row will keep rather than over a narrower one it is about to
  # leave. Columns added just above are NULL on every existing row and an
  # absent field contributes nothing, so the two give the same answer, but the
  # order says which one is meant.
  .rekey_dataset_contents(con)
  invisible(NULL)
}

#' Group rows into batches bounded by the bytes they carry.
#'
#' Greedy and in order, because the rows have to reach the rebuilt table with
#' their content_id and reading them by a contiguous range of it is a walk down
#' the primary key rather than a scan per batch.
#'
#' A row heavier on its own than the whole budget still has to travel. It takes
#' a batch to itself: the budget is a ceiling on what a batch adds to a working
#' set, not a promise that any single row fits under it.
#'
#' @param weight   Byte weight of each row, in the order they will be read. A
#'   weight that could not be taken counts as nothing rather than dropping the
#'   row out of the plan.
#' @param max_bytes Byte budget for one batch.
#' @param max_rows  Row ceiling for one batch.
#' @return Integer vector, one per row, naming the batch it belongs to. Batch
#'   numbers start at 1 and rise by one, so the runs are contiguous.
.dataset_rekey_batches <- function(weight,
                                   max_bytes = .DATASET_REKEY_BATCH_BYTES,
                                   max_rows  = .DATASET_REKEY_BATCH_ROWS) {
  n <- length(weight)
  if (n == 0L) return(integer(0L))
  w <- as.numeric(weight)
  w[is.na(w)] <- 0
  out  <- integer(n)
  b    <- 1L
  acc  <- 0
  rows <- 0L
  for (i in seq_len(n)) {
    if (rows > 0L && (acc + w[[i]] > max_bytes || rows >= max_rows)) {
      b    <- b + 1L
      acc  <- 0
      rows <- 0L
    }
    out[[i]] <- b
    acc  <- acc + w[[i]]
    rows <- rows + 1L
  }
  out
}

#' Key the content-addressed table on the profile digest rather than on the
#' fingerprints.
#'
#' The deployed database is keyed (content_fp, schema_fp, fp_algo_version), and
#' the incremental path opens that file rather than building one, so the new
#' key arrives here or it only ever applies to a database built from nothing.
#'
#' A UNIQUE is part of the CREATE and cannot be altered in place, so the table
#' is rebuilt from its own CREATE with the key phrase replaced and profile_fp
#' declared beside content_fp. Every column it has picked up since comes across
#' by name, and every row comes across with its content_id, which the version
#' links name. The indexes the DROP takes with it are recreated by the caller.
#'
#' Existing rows are digested rather than left NULL: the column is the key, and
#' a key column full of NULLs is not a key. The digests cannot collide, because
#' the old UNIQUE guarantees the rows differ in content_fp or schema_fp and
#' both are folded into the digest.
#'
#' Rows are copied in batches bounded by the bytes they hold rather than by a
#' count of them. One dataset's column profile runs to MAX_DATASET_COLUMNS_BYTES
#' and nothing smaller, so a fixed number of rows is a working set nobody stated:
#' on a catalog of profiles near the cap it is the published table read whole,
#' with the digest's working copies on top. What each row weighs is asked of
#' SQLite before any of it is in memory, and the batch that carries it is
#' whatever fits under the budget.
#'
#' The whole rebuild runs inside a savepoint. CREATE, INSERT, DROP and RENAME
#' are four statements and a run killed between them leaves the rebuild table
#' behind, which the next run met as its own leftover and stopped on, and so did
#' every run after it. Inside a savepoint an interrupted run leaves the file
#' exactly as it found it.
#'
#' A one-time no-op once the key is the digest.
.rekey_dataset_contents <- function(con) {
  if (!"bioc_dataset_contents" %in% DBI::dbListTables(con)) return(invisible(NULL))
  sql <- DBI::dbGetQuery(con,
    "SELECT sql FROM sqlite_master
      WHERE type = 'table' AND name = 'bioc_dataset_contents'")$sql
  old_key <- "UNIQUE (content_fp, schema_fp, fp_algo_version)"
  if (length(sql) != 1L || is.na(sql) || !grepl(old_key, sql, fixed = TRUE)) {
    return(invisible(NULL))
  }
  n <- as.integer(DBI::dbGetQuery(con,
    "SELECT COUNT(*) n FROM bioc_dataset_contents")$n)
  cat(sprintf("re-keying bioc_dataset_contents on the profile digest: %d row%s\n",
              n, if (n == 1L) "" else "s"), file = stdout())
  flush(stdout())

  # SAVEPOINT and not BEGIN: this also runs from inside the writer's own
  # transaction, where a second BEGIN is an error.
  DBI::dbExecute(con, "SAVEPOINT rekey_dataset_contents")
  done <- FALSE
  on.exit({
    if (!done) {
      try(DBI::dbExecute(con, "ROLLBACK TO rekey_dataset_contents"), silent = TRUE)
    }
    try(DBI::dbExecute(con, "RELEASE rekey_dataset_contents"), silent = TRUE)
  }, add = TRUE)

  # A rebuild table from a run that died before this was atomic. The guard
  # above has already established that the real table is here and still carries
  # the old key, so this is an abandoned attempt and not the only copy of
  # anything. Only ever dropped on that footing: if the original were the one
  # missing, this function returns above and leaves the leftover alone.
  DBI::dbExecute(con, "DROP TABLE IF EXISTS bioc_dataset_contents_new")

  create <- sub(old_key, "UNIQUE (profile_fp, fp_algo_version)", sql, fixed = TRUE)
  cols   <- DBI::dbListFields(con, "bioc_dataset_contents")
  if (!"profile_fp" %in% cols) {
    # Declared beside content_fp, which every table carrying the old key has.
    # If it is spelled some other way the new CREATE would come out without a
    # key column at all, so say which CREATE could not be read rather than
    # failing later on a column nobody declared.
    if (!grepl("content_fp TEXT NOT NULL", create, fixed = TRUE)) {
      stop("cannot re-key bioc_dataset_contents: its CREATE does not declare ",
           "content_fp the way the key migration expects: ", create)
    }
    create <- sub("content_fp TEXT NOT NULL",
                  "profile_fp TEXT NOT NULL, content_fp TEXT NOT NULL",
                  create, fixed = TRUE)
  }
  create <- sub("bioc_dataset_contents", "bioc_dataset_contents_new", create,
                fixed = TRUE)
  DBI::dbExecute(con, create)

  read_names <- setdiff(cols, "profile_fp")
  copy_cols  <- c("profile_fp", read_names)
  ins <- sprintf("INSERT INTO bioc_dataset_contents_new (%s) VALUES (%s)",
                 paste(sprintf('"%s"', copy_cols), collapse = ", "),
                 paste(rep("?", length(copy_cols)), collapse = ", "))
  read_cols <- paste(sprintf('"%s"', read_names), collapse = ", ")

  # What every row weighs, asked of SQLite rather than of memory. LENGTH over a
  # CAST to BLOB is the stored byte count and not a character count, so a
  # profile carrying multi-byte text is not planned for as smaller than it is,
  # and SQLite answers it off the record header without reading the value: on a
  # 923 MB table this pass is a tenth of a second and allocates nothing.
  #
  # Added up in groups of 64 and finished in R, because SQLite stops at an
  # expression a thousand deep and one chain of column lengths is exactly that
  # deep. The profile is 148 columns and gains a few each time the reader
  # describes more, so a single chain is a migration that works until the spec
  # crosses a line nobody is watching, and then fails whole.
  groups <- split(read_names, (seq_along(read_names) - 1L) %/% 64L)
  sums   <- vapply(seq_along(groups), function(g) sprintf(
    "%s AS w%d",
    paste(sprintf('COALESCE(LENGTH(CAST("%s" AS BLOB)), 0)', groups[[g]]),
          collapse = " + "), g), character(1L))
  plan <- DBI::dbGetQuery(con, sprintf(
    "SELECT content_id, %s FROM bioc_dataset_contents ORDER BY content_id",
    paste(sums, collapse = ", ")))
  weight <- rowSums(as.matrix(plan[, sprintf("w%d", seq_along(groups)), drop = FALSE]))
  runs <- rle(.dataset_rekey_batches(weight))
  ends <- cumsum(runs$lengths)
  for (b in seq_along(ends)) {
    lo <- plan$content_id[[ends[[b]] - runs$lengths[[b]] + 1L]]
    hi <- plan$content_id[[ends[[b]]]]
    # By content_id and not LIMIT/OFFSET: content_id is the rowid, so a range
    # of it is a walk down the primary key, where OFFSET re-walked every row
    # already copied and made the migration quadratic in the size of the table.
    chunk <- DBI::dbGetQuery(con, sprintf(
      "SELECT %s FROM bioc_dataset_contents
        WHERE content_id BETWEEN ? AND ? ORDER BY content_id", read_cols),
      params = list(lo, hi))
    chunk$profile_fp <- .dataset_profile_fp(chunk)
    DBI::dbExecute(con, ins, params = lapply(copy_cols, function(k) chunk[[k]]))
    rm(chunk)
  }

  DBI::dbExecute(con, "DROP TABLE bioc_dataset_contents")
  DBI::dbExecute(con,
    "ALTER TABLE bioc_dataset_contents_new RENAME TO bioc_dataset_contents")
  done <- TRUE
  invisible(NULL)
}

#' Let a version link stand without a profile behind it.
#'
#' bioc_dataset_versions.content_id was NOT NULL, which is what made "the reader
#' took no fingerprint" mean "the dataset leaves the catalog". The constraint
#' cannot be dropped in place, so the table is rebuilt from its own CREATE with
#' that one phrase removed: every column it has picked up since, and every row,
#' come across untouched. The index it carries is recreated by the caller.
#'
#' A one-time no-op once the constraint is gone.
.relax_dataset_version_content_id <- function(con) {
  if (!"bioc_dataset_versions" %in% DBI::dbListTables(con)) return(invisible(NULL))
  sql <- DBI::dbGetQuery(con,
    "SELECT sql FROM sqlite_master
      WHERE type = 'table' AND name = 'bioc_dataset_versions'")$sql
  notnull <- "content_id INTEGER NOT NULL"
  if (length(sql) != 1L || is.na(sql) || !grepl(notnull, sql, fixed = TRUE)) {
    return(invisible(NULL))
  }
  cols   <- paste(sprintf('"%s"', DBI::dbListFields(con, "bioc_dataset_versions")),
                  collapse = ", ")
  create <- sub(notnull, "content_id INTEGER", sql, fixed = TRUE)
  create <- sub("bioc_dataset_versions", "bioc_dataset_versions_new", create,
                fixed = TRUE)
  DBI::dbExecute(con, create)
  DBI::dbExecute(con, sprintf(
    "INSERT INTO bioc_dataset_versions_new (%s) SELECT %s FROM bioc_dataset_versions",
    cols, cols))
  DBI::dbExecute(con, "DROP TABLE bioc_dataset_versions")
  DBI::dbExecute(con,
    "ALTER TABLE bioc_dataset_versions_new RENAME TO bioc_dataset_versions")
  invisible(NULL)
}

.ensure_dataset_tables <- function(con) {
  .relax_dataset_version_content_id(con)
  tables <- DBI::dbListTables(con)
  if (!"bioc_datasets" %in% tables) {
    DBI::dbExecute(con, "CREATE TABLE bioc_datasets (
      package TEXT NOT NULL, name TEXT NOT NULL, file TEXT, internal INTEGER,
      current_version TEXT, current_content_id INTEGER,
      PRIMARY KEY (package, name))")
  }
  # content_id is nullable: a dataset whose values the reader could not take
  # comes back with no fingerprints, so there is no content row for it to point
  # at, and none can be invented without telling two objects that were never
  # compared that they hold the same data. The link still says the package ships
  # this dataset at this version, and format, confidence, notes, class and the
  # rest beside it say what was and was not read.
  if (!"bioc_dataset_versions" %in% tables) {
    DBI::dbExecute(con, "CREATE TABLE bioc_dataset_versions (
      package TEXT NOT NULL, name TEXT NOT NULL, version TEXT NOT NULL,
      content_id INTEGER, format TEXT, compression TEXT, confidence TEXT,
      is_current INTEGER NOT NULL DEFAULT 0,
      PRIMARY KEY (package, name, version))")
  }
  # Keyed on the digest of the whole recorded profile rather than on the
  # fingerprints, because the fingerprints do not cover everything the row
  # records and a key that does not separate two profiles publishes one of them
  # under both. content_fp is still here and still means what it always meant.
  if (!"bioc_dataset_contents" %in% tables) {
    DBI::dbExecute(con, "CREATE TABLE bioc_dataset_contents (
      content_id INTEGER PRIMARY KEY, profile_fp TEXT NOT NULL,
      content_fp TEXT NOT NULL, schema_fp TEXT NOT NULL, fp_algo_version INTEGER NOT NULL,
      nrow INTEGER, ncol INTEGER, n_missing_total INTEGER, columns TEXT,
      UNIQUE (profile_fp, fp_algo_version))")
  }
  if (!"bioc_dataset_sketches" %in% tables) {
    DBI::dbExecute(con, "CREATE TABLE bioc_dataset_sketches (
      content_id INTEGER PRIMARY KEY, row_sketch TEXT)")
  }
  # The analyzer describes more of a dataset over time, and those fields arrive
  # as columns that do not exist yet. This is also what widens the narrow CREATEs
  # above to the full declared shape, so a database being built from nothing and
  # one downloaded from the last release end up with the same columns.
  .ensure_dataset_columns(con)
  # Each index is created only where the column it names is there. A table
  # written before that column existed is the case this whole function is for,
  # and CREATE INDEX on a name SQLite does not know fails the open rather than
  # the index.
  idx <- function(name, table, col) {
    if (!table %in% DBI::dbListTables(con)) return(invisible(NULL))
    if (!col %in% DBI::dbListFields(con, table)) return(invisible(NULL))
    DBI::dbExecute(con, sprintf(
      'CREATE INDEX IF NOT EXISTS %s ON %s("%s")', name, table, col))
    invisible(NULL)
  }
  idx("idx_bioc_dsv_content", "bioc_dataset_versions", "content_id")
  idx("idx_bioc_dsc_schema",  "bioc_dataset_contents", "schema_fp")
  # content_fp used to lead the uniqueness key and so had an index for free.
  # It no longer does, and grouping on it is how the catalog answers "the same
  # data ships in N packages", which is a scan of the whole table without one.
  idx("idx_bioc_dsc_content", "bioc_dataset_contents", "content_fp")
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
  # Atomic vectors / matrices / S4 have values but no column schema, so schema_fp
  # is NA. Use an empty string so they still satisfy the column's NOT NULL and
  # so two such records read the same way still digest alike: an absent field
  # and an empty one are different inputs to the profile digest, and a schema_fp
  # left NA would make every vector its own profile.
  df$schema_fp[is.na(df$schema_fp)] <- ""

  # Nothing upstream bounds one column profile, and one pathological value is
  # enough to make the published database unloadable: MySQL refuses any single
  # value over its 32 MiB packet ceiling and fails the whole table's load, not
  # the row's. A file read as something it is not has already produced profiles
  # of 321 MB in the sibling pipeline that loads into the same place.
  #
  # What is refused is the profile, never the row. The class, the shape, the
  # counts and the fingerprints are all still true and still worth storing, and
  # the refused size stays beside them so a reader can tell a row whose columns
  # would not fit from an object that has no columns at all.
  df$columns_refused_bytes <- 0L
  if ("columns" %in% names(df)) {
    sizes <- nchar(as.character(df$columns), type = "bytes")
    over  <- !is.na(sizes) & sizes > MAX_DATASET_COLUMNS_BYTES
    if (any(over)) {
      df$columns_refused_bytes[over] <- sizes[over]
      df$columns[over] <- NA_character_
      named <- sprintf("%s %s (%s)", df$package[over], df$name[over],
                       vapply(sizes[over], format_bytes, character(1L)))
      cat(sprintf("refused %d column profile%s over %s: %s%s\n",
                  sum(over), if (sum(over) == 1L) "" else "s",
                  format_bytes(MAX_DATASET_COLUMNS_BYTES),
                  paste(head(named, 5L), collapse = ", "),
                  if (length(named) > 5L)
                    sprintf(" and %d more", length(named) - 5L) else ""),
          file = stdout())
      flush(stdout())
    }
  }

  # A single package version can surface one dataset name twice: an exported
  # data/ object and an internal sysdata object of the same name, or the same
  # object reached through two files. (package, name) is unique in bioc_datasets
  # and (package, name, version) in bioc_dataset_versions, so collapse to one
  # record per (package, name, version) up front, preferring the exported copy
  # (internal = 0 sorts first). Without this the version append fails the PK.
  df <- df[order(df$package, df$name, df$version, df$internal), , drop = FALSE]
  df <- df[!duplicated(paste(df$package, df$name, df$version, sep = "\x1f")), , drop = FALSE]

  # Which records the reader fingerprinted. The ones it did not are objects it
  # described and could not measure: an S4 object it holds no representation
  # for, a raster packed into bytes, and an .R script under data/, which only R
  # can evaluate. Every such record used to be dropped here, whole, so the
  # dataset left the catalog rather than appearing in it with what is known:
  # no identity row, no version link, nothing saying the package ships it.
  #
  # They still get no content row: that table is addressed by fingerprint, and
  # a key invented for a record with none would tell two objects that were
  # never compared that they hold the same data. They get the identity row and
  # the version link, with no content_id, and the format, the confidence, the
  # notes and the class beside it say what was read and what was not.
  fingerprinted <- !is.na(df$content_fp) & nzchar(df$content_fp)
  if (any(!fingerprinted)) {
    # Said out loud for the same reason the refusal above is: a dataset in the
    # catalog with nothing behind it is a coverage figure, and a shard where
    # that number climbs is the reader losing objects it used to measure.
    named <- sprintf("%s %s", df$package[!fingerprinted], df$name[!fingerprinted])
    cat(sprintf("kept %d dataset%s with no profile, unmeasured by the reader: %s%s\n",
                length(named), if (length(named) == 1L) "" else "s",
                paste(head(named, 5L), collapse = ", "),
                if (length(named) > 5L)
                  sprintf(" and %d more", length(named) - 5L) else ""),
        file = stdout())
    flush(stdout())
  }

  # 1. Content-addressed profiles: one INSERT OR IGNORE per distinct profile.
  #
  # Keyed on the digest of everything the row records rather than on the
  # fingerprints, which cover the cells and the column schema and nothing else.
  # Under the fingerprints alone, two datasets whose profiles differ in a time
  # zone, a declared level, a class or a projection resolved to one row, and
  # whichever record sorted first supplied the answer for both.
  df$profile_fp <- NA_character_
  df$profile_fp[fingerprinted] <-
    .dataset_profile_fp(df[fingerprinted, , drop = FALSE])
  ck <- rep(NA_character_, nrow(df))
  ck[fingerprinted] <- paste(df$profile_fp[fingerprinted],
                             df$fp_algo_version[fingerprinted], sep = "\x1f")
  cts <- df[fingerprinted & !duplicated(ck), , drop = FALSE]
  df$content_id <- NA_integer_
  if (nrow(cts) > 0L) {
    content_cols <- intersect(names(.DATASET_CONTENT_COLS), names(cts))
    ins_cols <- c("profile_fp", "content_fp", "schema_fp", "fp_algo_version",
                  content_cols)
    DBI::dbExecute(con,
      sprintf("INSERT OR IGNORE INTO bioc_dataset_contents (%s) VALUES (%s)",
              paste(sprintf('"%s"', ins_cols), collapse = ", "),
              paste(rep("?", length(ins_cols)), collapse = ", ")),
      params = lapply(ins_cols, function(k) {
        v <- cts[[k]]
        if (is.logical(v)) as.integer(v) else v
      }))

    # Resolve content_id for the profiles in this shard and attach it to every
    # row that has one. The rest keep NA, which is the whole of what the
    # profile table can say about them.
    ids <- DBI::dbGetQuery(con,
      "SELECT content_id, profile_fp, fp_algo_version FROM bioc_dataset_contents")
    key_map <- stats::setNames(
      ids$content_id,
      paste(ids$profile_fp, ids$fp_algo_version, sep = "\x1f"))
    df$content_id[fingerprinted] <- unname(key_map[ck[fingerprinted]])
  }

  # 2. Sketches: one INSERT OR IGNORE per content_id. The sketch is taken off
  # the values, so two profiles of the same data hold the same sketch and now
  # store it twice, once per profile row. Keyed on content_id all the same,
  # because that is what the version link and the reclaim both name; the cost
  # is a copy for each of the few profiles that split.
  sk <- df[!is.na(df$content_id) & !duplicated(df$content_id) & !is.na(df$row_sketch),
           c("content_id", "row_sketch"), drop = FALSE]
  if (nrow(sk) > 0L) {
    DBI::dbExecute(con,
      "INSERT OR IGNORE INTO bioc_dataset_sketches (content_id, row_sketch) VALUES (?, ?)",
      params = list(sk$content_id, sk$row_sketch))
  }

  # 3. Version links (package was wiped above, so a plain append is idempotent).
  ver_cols <- c("package", "name", "version", "content_id", "format",
                "compression", "confidence", "is_current",
                intersect(names(.DATASET_VERSION_COLS), names(df)))
  ver <- df[, ver_cols, drop = FALSE]
  DBI::dbAppendTable(con, "bioc_dataset_versions", ver)

  # 4. Identity, one per (package, name), stamped with the current version's content.
  cur <- df[df$is_current == 1L, , drop = FALSE]
  cur <- cur[!duplicated(paste(cur$package, cur$name, sep = "\x1f")), , drop = FALSE]
  if (nrow(cur) > 0L) {
    idn <- data.frame(package = cur$package, name = cur$name, file = cur$file,
                      internal = cur$internal, current_version = cur$version,
                      current_content_id = cur$content_id, stringsAsFactors = FALSE)
    for (k in intersect(names(.DATASET_IDENTITY_COLS), names(cur))) {
      idn[[k]] <- cur[[k]]
    }
    DBI::dbAppendTable(con, "bioc_datasets", idn)
  }
  invisible(NULL)
}

#' Reclaim content/sketch rows no longer referenced by any version link.
#'
#' The subquery leaves out the links that name no profile. NOT IN over a set
#' holding one NULL is NULL for every row it is asked about, so a single
#' unmeasured dataset anywhere in the table would quietly retire the whole
#' reclaim and leave nothing in the log to say so.
.gc_dataset_contents <- function(con) {
  tables <- DBI::dbListTables(con)
  if (!"bioc_dataset_contents" %in% tables) return(invisible(NULL))
  referenced <-
    "SELECT content_id FROM bioc_dataset_versions WHERE content_id IS NOT NULL"
  DBI::dbExecute(con, sprintf(
    "DELETE FROM bioc_dataset_sketches WHERE content_id NOT IN (%s)", referenced))
  DBI::dbExecute(con, sprintf(
    "DELETE FROM bioc_dataset_contents WHERE content_id NOT IN (%s)", referenced))
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
#' If the file does not yet exist it is created. The four non-summary tables
#' (bioc_code_churn, bioc_api_history, bioc_metrics_failures,
#' bioc_analyzer_read_attempts) are created with fixed schemas and indexes on
#' first open, so a database downloaded from an older release gains the ones it
#' does not have yet. bioc_code_summary is created lazily by upsert_shard the
#' first time data is written (its schema is dynamic).
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

  # Packages handed to the analyzer that it did not read. The fields the
  # backfill queues wait on (n_fns_r, the dataset rows) come from the binary
  # alone, so a package the pure-R fallback analysed carries none of them and
  # both queues hand it back on every run for good. analyzer_version is the
  # build that could not read it, so a later build can ask again.
  if (!"bioc_analyzer_read_attempts" %in% tables) {
    DBI::dbExecute(con, "
      CREATE TABLE bioc_analyzer_read_attempts (
        package          TEXT PRIMARY KEY,
        attempts         INTEGER NOT NULL DEFAULT 0,
        analyzer_version TEXT,
        last_attempt     TEXT
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
#' @param bootstrap   list(n_analyzed, n_universe, n_remaining,
#'   bootstrap_complete, n_datasets_unscanned, n_datasets_unreadable).
#'   n_universe/n_remaining and the two dataset counts may be NULL, in which
#'   case they are left out.
#' @return A named list matching the MANIFEST SCHEMA.
#' @param last_changed ISO-8601 timestamp of the last run that actually moved the
#'   data, or NULL when this run did. Kept separate from the generation time
#'   because a run that finds nothing to do still needs to report that it ran.
build_manifest <- function(con, series, repo, db_filename, db_bytes,
                           tables, fp_table, fp_cols, pkg_table, ver_table,
                           stat_table, stat_cols, bootstrap,
                           last_changed = NULL) {
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

  now_iso <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

  list(
    schema_version = 1L,
    series         = series,
    repo           = repo,
    db_filename    = db_filename,
    generated_at   = now_iso,
    # last_checked answers "did this pipeline run", last_changed answers "did the
    # data move". They diverge for months at a time here: the universe is keyed
    # on the Bioconductor release, so between releases every daily run correctly
    # finds nothing to do. Consumers that key freshness on last_changed would
    # read that healthy silence as a dead pipeline.
    last_checked   = now_iso,
    last_changed   = last_changed %||% now_iso,
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
      bootstrap_complete = isTRUE(bootstrap$bootstrap_complete),
      # A different question from bootstrap_complete, and one it hides:
      # completion is measured against the code analysis, so it reads true
      # while packages sit with no dataset scan at all and no queue that will
      # ever pick them up.
      n_datasets_unscanned = bootstrap$n_datasets_unscanned,
      # How many packages the pipeline has stopped asking for datasets: asked
      # to the cap under this analyzer build and never read. Kept beside
      # bootstrap_complete because completion is measured against the code
      # analysis, so it reads true while these packages sit unread. The number
      # does not come down on its own, which is what makes it worth publishing:
      # it says what the corpus is missing for good, until a build that can
      # read them arrives.
      n_datasets_unreadable = bootstrap$n_datasets_unreadable,
      # How many datasets are in the catalog with nothing behind them: the
      # reader described them and could not fingerprint them, so they have an
      # identity row and a version link and no profile. Unlike the two counts
      # above it is per dataset rather than per package, and the table count
      # beside it in this same file is its denominator. It is here rather than
      # only in a line the shard prints because that line scrolls away with the
      # run, and a shard where this number jumps is the one worth seeing.
      n_datasets_unmeasured = bootstrap$n_datasets_unmeasured
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

# ---------------------------------------------------------------------------
# Rich release notes (headline + per-package metrics table + catalog summary)
# ---------------------------------------------------------------------------

#' Render a byte count as a compact human-readable string.
#'
#' Bytes below 1024 render as "N bytes"; above that, KB/MB/GB in powers of
#' 1024, whole numbers for KB and MB, one decimal place for GB. NULL/NA (an
#' unmeasured size) renders as "n/a", never a fabricated 0.
#'
#' @param n A single byte count (numeric), or NULL/NA.
#' @return A one-line string, e.g. "870 MB", "1.4 GB", "235 KB", "512 bytes".
format_bytes <- function(n) {
  if (is.null(n) || length(n) == 0L || is.na(n)) return("n/a")
  n <- as.numeric(n)
  # Round half up for the whole-number units so an exact x.5 boundary
  # (e.g. 240128 / 1024 = 234.5) matches everyday expectation rather than
  # R's round-half-to-even default (which would report 234 KB).
  half_up <- function(x) floor(x + 0.5)
  if (n < 1024)  return(sprintf("%d bytes", as.integer(half_up(n))))
  kb <- n / 1024
  if (kb < 1024) return(sprintf("%d KB", as.integer(half_up(kb))))
  mb <- kb / 1024
  if (mb < 1024) return(sprintf("%d MB", as.integer(half_up(mb))))
  gb <- mb / 1024
  sprintf("%.1f GB", gb)
}

#' Fetch all rows for a set of packages from one table, chunking IN-lists to
#' <= 900 params so a large changed-package set never exceeds SQLite's
#' bound-parameter limit. Mirrors the chunking pattern in .delete_by_package.
#'
#' @param con    Open DBI connection.
#' @param table  Table name (trusted; not user input).
#' @param pkgs   Character vector of package names to fetch (deduped).
#' @param select SELECT-list fragment, inserted verbatim (default "*").
#' @return data.frame of matching rows (any number per package); a 0x0
#'   data.frame when the table is absent or pkgs is empty.
.fetch_by_package <- function(con, table, pkgs, select = "*") {
  pkgs <- unique(as.character(pkgs))
  if (!table %in% DBI::dbListTables(con) || length(pkgs) == 0L) {
    return(data.frame())
  }
  chunk_size <- 900L
  out <- list()
  for (i in seq(1L, length(pkgs), by = chunk_size)) {
    chunk <- pkgs[i:min(i + chunk_size - 1L, length(pkgs))]
    ph    <- paste(rep("?", length(chunk)), collapse = ", ")
    out[[length(out) + 1L]] <- DBI::dbGetQuery(
      con,
      sprintf("SELECT %s FROM %s WHERE package IN (%s)", select, table, ph),
      params = as.list(chunk))
  }
  do.call(rbind, out)
}

#' Pick each package's "latest tracked version" row out of a multi-row slice
#' of bioc_code_summary.
#'
#' Winner per package: the row with a non-NA latest_release_date (there is
#' at most one, per add_cross_version_metrics, which stamps it only on the
#' newest-version row); ties (or a schema that lacks the column) fall back
#' to version (lexicographic, descending), then to insertion order via the
#' rowid_ column (expected to be selected as `rowid AS rowid_, *`) when
#' present.
#'
#' @param rows data.frame from .fetch_by_package() for bioc_code_summary;
#'   may have zero rows.
#' @return data.frame, one row per distinct package present in `rows`.
.pick_latest_rows <- function(rows) {
  if (nrow(rows) == 0L) return(rows)
  has_lrd <- "latest_release_date" %in% names(rows)
  has_rid <- "rowid_" %in% names(rows)
  chosen <- lapply(split(rows, rows$package), function(pr) {
    if (has_lrd) {
      marked <- pr[!is.na(pr$latest_release_date), , drop = FALSE]
      if (nrow(marked) > 0L) {
        marked <- marked[order(marked$version, decreasing = TRUE), , drop = FALSE]
        return(marked[1L, , drop = FALSE])
      }
    }
    if (has_rid) {
      pr <- pr[order(pr$rowid_, decreasing = TRUE), , drop = FALSE]
    }
    pr[1L, , drop = FALSE]
  })
  do.call(rbind, chosen)
}

#' Derive the notes table's four numeric metrics from one latest-version row
#' of bioc_code_summary, binding to the real schema with the documented
#' fallbacks. A column absent from the row's schema, or NA for this specific
#' package, yields NA (rendered "n/a" downstream) -- never a fabricated 0.
#'
#' @param row One-row data.frame (as returned by .pick_latest_rows()).
#' @return list(loc_r, functions, exports, deps); each a scalar or NA.
.row_metrics <- function(row) {
  g   <- function(col) row[[col]] %||% NA
  has <- function(col) col %in% names(row)

  loc_r <- g("loc_r")

  # Functions: n_exports + n_internal when the schema carries the split
  # columns; the fused rpkg-analyzer field n_fns_r only when it does not.
  functions <- if (has("n_exports") || has("n_internal")) {
    ne <- g("n_exports"); ni <- g("n_internal")
    if (is.na(ne) && is.na(ni)) {
      NA_integer_
    } else {
      (if (is.na(ne)) 0L else as.integer(ne)) + (if (is.na(ni)) 0L else as.integer(ni))
    }
  } else if (has("n_fns_r")) {
    g("n_fns_r")
  } else {
    NA_integer_
  }

  exports <- g("n_exports")

  # Deps: n_deps_direct when available; else a best-effort count parsed out
  # of the raw Depends/Imports DESCRIPTION text (excluding R itself).
  deps <- {
    d <- g("n_deps_direct")
    if (!is.na(d)) {
      as.integer(d)
    } else {
      parts_present <- c(g("depends"), g("imports"))
      parts_present <- parts_present[!is.na(parts_present)]
      dep_txt <- paste(parts_present, collapse = ",")
      if (!nzchar(trimws(dep_txt))) {
        NA_integer_
      } else {
        pkg_names <- strsplit(dep_txt, ",", fixed = TRUE)[[1L]]
        pkg_names <- trimws(sub("\\s*\\(.*", "", pkg_names, perl = TRUE))
        pkg_names <- pkg_names[nzchar(pkg_names) & !grepl("^R$", pkg_names, perl = TRUE)]
        length(pkg_names)
      }
    }
  }

  list(loc_r = loc_r, functions = functions, exports = exports, deps = deps)
}

#' Count each package's rows in the dataset database's identity table.
#'
#' @param data_con Open DBI connection to the dataset database, or NULL.
#' @param pkgs     Character vector of packages to count for.
#' @return Named integer vector (names = pkgs). NA when the dataset DB/table
#'   is unavailable (unmeasurable); a real 0 when the table exists but a
#'   package simply has no dataset rows.
.count_datasets <- function(data_con, pkgs) {
  pkgs <- unique(as.character(pkgs))
  if (length(pkgs) == 0L) return(stats::setNames(integer(0L), character(0L)))
  if (is.null(data_con) || !("bioc_datasets" %in% DBI::dbListTables(data_con))) {
    return(stats::setNames(rep(NA_integer_, length(pkgs)), pkgs))
  }
  rows <- .fetch_by_package(data_con, "bioc_datasets", pkgs, select = "package")
  tab  <- table(factor(rows$package, levels = pkgs))
  stats::setNames(as.integer(tab), pkgs)
}

#' Format a scalar for display: "n/a" for NULL/NA, else comma-grouped.
#'
#' @param x A length-0/1 numeric-ish value.
#' @return A one-line string.
.fmt_n <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x)) return("n/a")
  format(round(as.numeric(x)), big.mark = ",", trim = TRUE, scientific = FALSE)
}

#' Build the one-paragraph headline: new/updated counts, catalog size, and
#' the bootstrap clause.
#'
#' @param code_manifest Parsed code-manifest.json (list).
#' @param changed_pkgs  Character vector, this run's changed packages.
#' @param seed_pkgs     Character vector, the prior release's package set
#'   ("new to the catalog" = not present here).
#' @return A single-line string.
.build_headline <- function(code_manifest, changed_pkgs, seed_pkgs) {
  n_changed <- length(changed_pkgs)
  n_new     <- sum(!changed_pkgs %in% seed_pkgs)
  n_updated <- n_changed - n_new

  bs <- code_manifest$bootstrap
  bootstrap_clause <- if (is.null(bs) || is.null(bs$n_universe)) {
    ""
  } else if (isTRUE(bs$bootstrap_complete)) {
    " Bootstrap complete."
  } else {
    n_universe  <- as.numeric(bs$n_universe)
    n_remaining <- as.numeric(bs$n_remaining %||% 0)
    if (length(n_remaining) == 0L || is.na(n_remaining) || n_remaining < 0) {
      n_remaining <- 0
    }
    if (is.na(n_universe) || n_universe <= 0) {
      # Degenerate/empty universe: no meaningful progress to report.
      ""
    } else {
      # Progress and the remaining count share one denominator (n_universe), so
      # the percentage reaches 100 only when nothing remains; floor() never
      # rounds up to 100 while work is queued. "processed" (not "complete")
      # keeps the completion wording in the bootstrap_complete branch only.
      n_remaining <- min(n_remaining, n_universe)
      pct <- floor(100 * (n_universe - n_remaining) / n_universe)
      sprintf(" Bootstrap %s%% processed (%s remaining).",
              format(pct, trim = TRUE), .fmt_n(n_remaining))
    }
  }

  new_word <- if (isTRUE(n_new == 1L)) "package" else "packages"
  pkg_word <- if (isTRUE(as.numeric(code_manifest$n_packages) == 1)) "package" else "packages"
  ver_word <- if (isTRUE(as.numeric(code_manifest$n_versions) == 1)) "version" else "versions"
  sprintf(
    "%s %s new to the catalog, %s updated. Now tracking %s %s across %s %s.%s",
    .fmt_n(n_new), new_word, .fmt_n(n_updated),
    .fmt_n(code_manifest$n_packages), pkg_word,
    .fmt_n(code_manifest$n_versions), ver_word,
    bootstrap_clause)
}

#' Build the "Updated this release" table's rows: one row per changed
#' package that has a row in the code DB, sorted alphabetically, with its
#' latest-version metrics and dataset count.
#'
#' @param code_con     Open DBI connection to the code database, or NULL.
#' @param data_con     Open DBI connection to the dataset database, or NULL.
#' @param changed_pkgs Character vector, this run's changed packages.
#' @param seed_pkgs    Character vector, the prior release's package set.
#' @return data.frame: package, version (tagged " (new)" as appropriate),
#'   loc_r, functions, exports, deps, datasets. Zero rows when there is
#'   nothing to show.
.build_package_rows <- function(code_con, data_con, changed_pkgs, seed_pkgs) {
  empty <- data.frame(package = character(0L), version = character(0L),
                      loc_r = integer(0L), functions = integer(0L),
                      exports = integer(0L), deps = integer(0L),
                      datasets = integer(0L), stringsAsFactors = FALSE)
  if (is.null(code_con) || length(changed_pkgs) == 0L) return(empty)

  raw <- .fetch_by_package(code_con, "bioc_code_summary", changed_pkgs,
                           select = "rowid AS rowid_, *")
  if (nrow(raw) == 0L) return(empty)

  latest <- .pick_latest_rows(raw)
  latest <- latest[order(latest$package), , drop = FALSE]
  ds_counts <- .count_datasets(data_con, latest$package)

  out <- lapply(seq_len(nrow(latest)), function(i) {
    r   <- latest[i, , drop = FALSE]
    m   <- .row_metrics(r)
    ver <- as.character(r$version)
    if (!(r$package %in% seed_pkgs)) ver <- paste0(ver, " (new)")
    data.frame(package = r$package, version = ver,
               loc_r = m$loc_r, functions = m$functions,
               exports = m$exports, deps = m$deps,
               datasets = unname(ds_counts[[r$package]]),
               stringsAsFactors = FALSE)
  })
  do.call(rbind, out)
}

#' Render the "## Updated this release" section: a markdown table capped at
#' `cap` rows (with a summary row for the remainder), or an honest "no
#' changes" / empty-shell fallback.
#'
#' @param rows      data.frame from .build_package_rows().
#' @param n_changed Total changed-package count for this run (from
#'   changed-packages.txt, independent of DB presence).
#' @param cap       Max rows to show before collapsing into a summary row.
#' @return Character vector of markdown lines (no trailing blank line).
.build_table_section <- function(rows, n_changed, cap = 40L) {
  if (n_changed == 0L) {
    return(c("## Updated this release", "", "No package changes in this release."))
  }
  header <- c("| Package | Version | R LOC | Functions | Exports | Deps | Datasets |",
              "|---|---|--:|--:|--:|--:|--:|")
  if (nrow(rows) == 0L) {
    # Every changed package was absent from the code DB (edge case): changes
    # did happen this run, so do not claim otherwise -- show the empty shell.
    return(c("## Updated this release", "", header))
  }
  shown <- utils::head(rows, cap)
  body  <- vapply(seq_len(nrow(shown)), function(i) {
    r <- shown[i, , drop = FALSE]
    sprintf("| %s | %s | %s | %s | %s | %s | %s |",
            r$package, r$version, .fmt_n(r$loc_r), .fmt_n(r$functions),
            .fmt_n(r$exports), .fmt_n(r$deps), .fmt_n(r$datasets))
  }, character(1L))
  extra <- nrow(rows) - nrow(shown)
  if (extra > 0L) {
    body <- c(body, sprintf("| ...and %s more updated packages | | | | | | |",
                            format(extra, big.mark = ",", trim = TRUE)))
  }
  c("## Updated this release", "", header, body)
}

#' Render the "## Catalog at a glance" section straight from the two
#' already-read manifests; nothing here is recomputed from the databases.
#' The code and data DB sizes are shown human-readable via format_bytes()
#' (never as raw byte counts).
#'
#' @param code_manifest Parsed code-manifest.json (list).
#' @param data_manifest Parsed data-manifest.json (list).
#' @return Character vector of markdown lines (no trailing blank line).
.build_catalog_section <- function(code_manifest, data_manifest) {
  f          <- code_manifest$tables[["bioc_functions"]]
  median_loc <- code_manifest$stats[["loc_r_median"]]
  # Count distinct datasets (the bioc_datasets table), not dataset *versions*
  # (n_versions counts bioc_dataset_versions). Fall back to n_versions only if
  # the table count is somehow absent.
  d          <- data_manifest$tables[["bioc_datasets"]] %||% data_manifest$n_versions

  c("## Catalog at a glance", "",
    sprintf("- %s packages, %s versions, %s functions",
            .fmt_n(code_manifest$n_packages), .fmt_n(code_manifest$n_versions), .fmt_n(f)),
    sprintf("- R code: median %s LOC per package", .fmt_n(median_loc)),
    sprintf("- %s datasets across %s packages", .fmt_n(d), .fmt_n(data_manifest$n_packages)),
    # The code and dataset databases ship as two separate releases, so state
    # both sizes and say so -- the same notes body is attached to each release.
    sprintf("- Databases: code metrics %s and dataset metrics %s (published as separate code and data releases)",
            format_bytes(code_manifest$db_bytes), format_bytes(data_manifest$db_bytes)))
}

#' Build the full release notes body: headline paragraph, per-package
#' metrics table, catalog summary, and a plumbing footer. No top-level "# "
#' heading is emitted -- the GitHub release title already carries that.
#'
#' @param code_manifest Parsed code-manifest.json (list).
#' @param data_manifest Parsed data-manifest.json (list).
#' @param changed_pkgs  Character vector, this run's changed packages.
#' @param seed_pkgs     Character vector, the prior release's package set
#'   (empty when seed-packages.txt is absent/empty: every changed package
#'   counts as new).
#' @param code_con      Open DBI connection to the code database, or NULL.
#' @param data_con      Open DBI connection to the dataset database, or NULL.
#' @param cap           Max table rows before collapsing into a summary row.
#' @return Character vector of markdown lines.
build_release_notes <- function(code_manifest, data_manifest, changed_pkgs,
                                seed_pkgs, code_con, data_con, cap = 40L) {
  headline        <- .build_headline(code_manifest, changed_pkgs, seed_pkgs)
  rows            <- .build_package_rows(code_con, data_con, changed_pkgs, seed_pkgs)
  table_section   <- .build_table_section(rows, length(changed_pkgs), cap = cap)
  catalog_section <- .build_catalog_section(code_manifest, data_manifest)

  short_fp <- substr(code_manifest$fingerprint %||% "", 1L, 8L)
  footer   <- sprintf("<sub>fingerprint %s - full manifest in the release assets</sub>",
                      short_fp)

  c(headline, "", table_section, "", catalog_section, "", footer)
}
