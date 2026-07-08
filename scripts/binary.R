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
.datasets_frame <- function(recs) {
  chr <- function(k) vapply(recs, function(r) .rec_chr(r, k), character(1L))
  int <- function(k) vapply(recs, function(r) .rec_int(r, k), integer(1L))
  lgl <- function(k) vapply(recs, function(r) .rec_lgl(r, k), logical(1L))
  jsn <- function(k) vapply(recs, function(r) {
    v <- r[[k]]
    if (is.null(v)) NA_character_
    else as.character(jsonlite::toJSON(v, auto_unbox = TRUE, null = "null"))
  }, character(1L))
  n_cols <- vapply(recs, function(r) {
    c <- r[["columns"]]
    if (is.null(c)) NA_integer_ else length(c)
  }, integer(1L))
  data.frame(
    name = chr("name"), file = chr("file"), internal = lgl("internal"),
    format = chr("format"), format_version = int("format_version"),
    compression = chr("compression"), class = chr("class"), kind = chr("kind"),
    nrow = int("nrow"), ncol = int("ncol"), length = int("length"),
    n_cols = n_cols, n_missing_total = int("n_missing_total"),
    schema_fp = chr("schema_fp"), shape_fp = chr("shape_fp"),
    content_fp = chr("content_fp"), s4_package = chr("s4_package"),
    confidence = chr("confidence"), notes = chr("notes"),
    columns = jsn("columns"), row_sketch = jsn("row_sketch"),
    stringsAsFactors = FALSE
  )
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
