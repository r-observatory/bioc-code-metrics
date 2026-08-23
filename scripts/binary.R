# Bridge to the rpkg-analyzer binary (r-observatory/rpkg-analyzer).
#
# The binary is a pure function of one extracted package source directory: it
# emits newline-delimited JSON. The first record is the per-version summary;
# subsequent records describe dependencies, exports, per-function detail
# (rec=="function") and per-call-edge detail (rec=="call_edge"). It reproduces
# the R metric groups' output and adds many static metrics, so it replaces the
# build_context + analyze_version computation for a single version.
#
# This is binary-first with an R fallback: when the binary is unavailable or
# fails, analyze_with_binary() returns NULL and the caller uses analyze_version.

#' Locate the rpkg-analyzer binary.
#'
#' Honours the RPKG_ANALYZER_BIN environment variable (an explicit path),
#' otherwise looks for `rpkg-analyzer` on PATH. Returns "" when not found.
rpkg_analyzer_bin <- function() {
  bin <- Sys.getenv("RPKG_ANALYZER_BIN", unset = "")
  if (nzchar(bin) && file.exists(bin)) return(bin)
  unname(Sys.which("rpkg-analyzer"))
}

# Extract one scalar field from a parsed NDJSON record, defaulting to NA.
# With simplifyVector = FALSE, scalar JSON values decode to length-1 atomics.
.rec_chr <- function(rec, key) {
  v <- rec[[key]]
  if (is.null(v)) NA_character_ else as.character(v)[[1L]]
}
.rec_int <- function(rec, key) {
  v <- rec[[key]]
  if (is.null(v)) NA_integer_ else as.integer(v)[[1L]]
}
.rec_lgl <- function(rec, key) {
  v <- rec[[key]]
  if (is.null(v)) NA else as.logical(v)[[1L]]
}

# Flatten a summary record (a parsed named list) to length-1 scalars, encoding
# any array/object value as a JSON string. This is the historical behaviour used
# by analyze_with_binary and must not change (the summary column set is stable).
.flatten_summary <- function(summ) {
  summ[["rec"]] <- NULL
  lapply(summ, function(v) {
    if (is.null(v)) {
      NA
    } else if (is.list(v) || length(v) != 1L) {
      as.character(jsonlite::toJSON(v, auto_unbox = TRUE, null = "null"))
    } else {
      v[[1L]]
    }
  })
}

# Build the per-dataset detail frame from parsed "dataset" records. Scalar
# fields become columns; the nested `columns` and `row_sketch` are kept as JSON
# strings (as .flatten_summary does for nested values). Column order matches
# .empty_datasets_df in analyze.R (minus the package/version stamp).
# The columns a dataset frame always has, whether or not this shard's records
# happen to mention them, with the type each takes when empty. Downstream code
# addresses these by name, so a shard where nothing carried a row_sketch must
# still have the column. Mirrors .empty_datasets_df in analyze.R, minus the
# package/version stamp that is applied later.
.DATASET_BASE_COLS <- list(
  name = character(0L), file = character(0L), internal = logical(0L),
  format = character(0L), format_version = integer(0L),
  compression = character(0L), class = character(0L), kind = character(0L),
  nrow = integer(0L), ncol = integer(0L), length = integer(0L),
  n_cols = integer(0L), n_missing_total = integer(0L),
  schema_fp = character(0L), shape_fp = character(0L),
  content_fp = character(0L), s4_package = character(0L),
  confidence = character(0L), notes = character(0L),
  columns = character(0L), row_sketch = character(0L)
)

# Row-bind dataset frames whose columns need not match.
#
# .datasets_frame carries whatever fields the records in front of it happened to
# have, so two versions of one package produce frames of different widths as
# soon as they differ in what they hold: a version shipping a data.frame emits
# has_rownames and one shipping only an S4 object does not. Plain rbind stops on
# that, and the caller treats the error as the whole package failing.
.rbind_datasets <- function(dfs) {
  dfs <- Filter(function(d) !is.null(d) && nrow(d) > 0L, dfs)
  if (!length(dfs)) return(NULL)
  all_cols <- unique(unlist(lapply(dfs, names), use.names = FALSE))
  do.call(rbind, lapply(dfs, function(d) {
    for (col in setdiff(all_cols, names(d))) d[[col]] <- NA
    d[, all_cols, drop = FALSE]
  }))
}

.datasets_frame <- function(recs) {
  # Carry whatever the analyzer emits rather than a fixed list of names. The
  # list version silently dropped every field added since it was written, so a
  # richer scan cost its own runtime and changed nothing in the database. What
  # belongs in which table is decided on the way in, not here.
  n <- length(recs)
  out <- list()
  if (n) {
    keys <- setdiff(unique(unlist(lapply(recs, names), use.names = FALSE)), "rec")
    for (k in keys) {
      vals <- lapply(recs, function(r) r[[k]])
      # A value that is a list, or that is not a single element, cannot be a
      # column; keep it as JSON the way the summary record's nested values are.
      nested <- vapply(vals, function(v) !is.null(v) && (is.list(v) || length(v) != 1L),
                       logical(1L))
      if (any(nested)) {
        out[[k]] <- vapply(vals, function(v) {
          if (is.null(v)) NA_character_
          else as.character(jsonlite::toJSON(v, auto_unbox = TRUE, null = "null"))
        }, character(1L))
        next
      }
      present <- vals[!vapply(vals, is.null, logical(1L))]
      out[[k]] <- if (!length(present)) {
        rep(NA, n)
      } else if (all(vapply(present, is.logical, logical(1L)))) {
        vapply(vals, function(v) if (is.null(v)) NA else as.logical(v)[[1L]], logical(1L))
      } else if (all(vapply(present, function(v) is.numeric(v) && !is.na(v) &&
                                                 v == trunc(v) && abs(v) < .Machine$integer.max,
                            logical(1L)))) {
        vapply(vals, function(v) if (is.null(v)) NA_integer_ else as.integer(v)[[1L]], integer(1L))
      } else if (all(vapply(present, is.numeric, logical(1L)))) {
        vapply(vals, function(v) if (is.null(v)) NA_real_ else as.numeric(v)[[1L]], numeric(1L))
      } else {
        vapply(vals, function(v) if (is.null(v)) NA_character_ else as.character(v)[[1L]], character(1L))
      }
    }
    # n_cols is the width of the profiled schema, which is the length of a
    # nested value rather than a field of its own.
    out[["n_cols"]] <- vapply(recs, function(r) {
      c <- r[["columns"]]
      if (is.null(c)) NA_integer_ else length(c)
    }, integer(1L))
  }
  # Fill in any base column this shard never mentioned, so the shape downstream
  # code addresses by name is the same every run.
  for (k in names(.DATASET_BASE_COLS)) {
    if (is.null(out[[k]])) {
      out[[k]] <- rep(.DATASET_BASE_COLS[[k]][NA_integer_], n)
    }
  }
  # Base columns first, in their canonical order, then whatever is new.
  ord <- c(names(.DATASET_BASE_COLS), setdiff(names(out), names(.DATASET_BASE_COLS)))
  out <- out[ord]
  df <- as.data.frame(out, stringsAsFactors = FALSE, optional = TRUE)
  names(df) <- ord
  df
}

#' Parse a full NDJSON analyzer stream into summary + detail frames.
#'
#' Reads every line (unlike the historical parser, which stopped at the summary)
#' and dispatches on each record's "rec" field:
#'   - "summary"   -> the first such record, flattened to length-1 scalars.
#'   - "function"  -> one row in the functions frame. Compiled languages
#'                    (c/cpp/rust/fortran) carry NA for exported/n_params/
#'                    cyclocomp, which R functions populate.
#'   - "call_edge" -> one row in the edges frame.
#' All other record types are ignored.
#'
#' @param lines Character vector of NDJSON lines (analyzer stdout).
#' @return A list with three elements:
#'   $summary   flattened named list, or NULL if no summary record was present.
#'   $functions data.frame(lang, name, exported, file, line, loc, n_params,
#'              cyclocomp); zero rows when the stream has no function records.
#'   $edges     data.frame(graph, from, to); zero rows when none present.
parse_analyzer_records <- function(lines) {
  summ <- NULL
  fn <- list(lang = character(0L), name = character(0L), exported = logical(0L),
             file = character(0L), line = integer(0L), loc = integer(0L),
             n_params = integer(0L), cyclocomp = integer(0L))
  eg <- list(graph = character(0L), from = character(0L), to = character(0L))
  ds_recs <- list()

  for (line in lines) {
    parsed <- tryCatch(
      jsonlite::fromJSON(line, simplifyVector = FALSE),
      error = function(e) NULL
    )
    if (is.null(parsed)) next
    rec <- parsed[["rec"]]

    if (identical(rec, "summary")) {
      # Keep the first summary record (matches the historical first-wins parser).
      if (is.null(summ)) summ <- parsed
    } else if (identical(rec, "function")) {
      fn$lang     <- c(fn$lang,     .rec_chr(parsed, "lang"))
      fn$name     <- c(fn$name,     .rec_chr(parsed, "name"))
      fn$exported <- c(fn$exported, .rec_lgl(parsed, "exported"))
      fn$file     <- c(fn$file,     .rec_chr(parsed, "file"))
      fn$line     <- c(fn$line,     .rec_int(parsed, "line"))
      fn$loc      <- c(fn$loc,      .rec_int(parsed, "loc"))
      fn$n_params <- c(fn$n_params, .rec_int(parsed, "n_params"))
      fn$cyclocomp <- c(fn$cyclocomp, .rec_int(parsed, "cyclocomp"))
    } else if (identical(rec, "call_edge")) {
      eg$graph <- c(eg$graph, .rec_chr(parsed, "graph"))
      eg$from  <- c(eg$from,  .rec_chr(parsed, "from"))
      eg$to    <- c(eg$to,    .rec_chr(parsed, "to"))
    } else if (identical(rec, "dataset")) {
      ds_recs[[length(ds_recs) + 1L]] <- parsed
    }
  }

  functions <- data.frame(
    lang = fn$lang, name = fn$name, exported = fn$exported,
    file = fn$file, line = fn$line, loc = fn$loc,
    n_params = fn$n_params, cyclocomp = fn$cyclocomp,
    stringsAsFactors = FALSE
  )
  edges <- data.frame(
    graph = eg$graph, from = eg$from, to = eg$to,
    stringsAsFactors = FALSE
  )

  list(
    summary   = if (is.null(summ)) NULL else .flatten_summary(summ),
    functions = functions,
    edges     = edges,
    datasets  = .datasets_frame(ds_recs)
  )
}

#' Run the analyzer over an extracted package directory.
#'
#' @param dir Path to the extracted package source (a DESCRIPTION at its root).
#' @return A flat named list of metrics for the version, with nested values
#'   (maps and arrays) serialised to JSON strings to match how the R metric
#'   groups store fields such as lang_breakdown. The per-function and
#'   per-call-edge detail frames are attached as the "functions" and "edges"
#'   attributes (data.frames without package/version stamps). NULL if the binary
#'   is unavailable or does not produce a summary record.
analyze_with_binary <- function(dir) {
  bin <- rpkg_analyzer_bin()
  if (!nzchar(bin)) return(NULL)

  out <- tryCatch(
    system2(bin, shQuote(dir), stdout = TRUE, stderr = FALSE),
    error   = function(e) NULL,
    warning = function(w) NULL
  )
  if (is.null(out) || length(out) == 0L) return(NULL)

  parsed <- parse_analyzer_records(out)
  if (is.null(parsed$summary)) return(NULL)

  metrics <- parsed$summary
  attr(metrics, "functions") <- parsed$functions
  attr(metrics, "edges")     <- parsed$edges
  attr(metrics, "datasets")  <- parsed$datasets
  metrics
}
