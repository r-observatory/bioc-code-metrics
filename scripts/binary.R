# Bridge to the rpkg-analyzer binary (r-observatory/rpkg-analyzer).
#
# The binary is a pure function of one extracted package source directory: it
# emits newline-delimited JSON. The first record is the per-version summary;
# subsequent records describe dependencies, exports, per-function detail
# (rec=="function") and per-call-edge detail (rec=="call_edge"). It reproduces
# the R metric groups' output and adds many static metrics, so it replaces the
# build_context + analyze_version computation for a single version.
#
# This is binary-first with an R fallback: when the binary gives no usable
# result on a version with no stored analyzer row, analyze_with_binary()
# returns NULL and the caller uses analyze_version. A killed analyzer, or no
# usable result on a version with an analyzer row, is raised.

#' Locate the rpkg-analyzer binary.
#'
#' Honours the RPKG_ANALYZER_BIN environment variable (an explicit path),
#' otherwise looks for `rpkg-analyzer` on PATH. Returns "" when not found.
rpkg_analyzer_bin <- function() {
  bin <- Sys.getenv("RPKG_ANALYZER_BIN", unset = "")
  if (nzchar(bin) && file.exists(bin)) return(bin)
  unname(Sys.which("rpkg-analyzer"))
}

#' The version of the analyzer binary that will run, or NA when it cannot be
#' determined. Data collected by an older build describes less than the same
#' scan would now, and this is what lets that be noticed.
rpkg_analyzer_version <- function() {
  bin <- rpkg_analyzer_bin()
  if (!nzchar(bin)) return(NA_character_)
  out <- .retry_after_time_limit(
    suppressWarnings(system2(bin, "--version", stdout = TRUE, stderr = FALSE)),
    error = function(e) character(0L))
  if (!length(out)) return(NA_character_)
  # "rpkg-analyzer 0.4.0"
  v <- sub("^\\s*rpkg-analyzer\\s+", "", out[[1L]])
  v <- trimws(v)
  if (!nzchar(v) || identical(v, out[[1L]])) NA_character_ else v
}

#' Whether a reported analyzer version is at least `min`. "0.5.0-test" reads as
#' 0.5.0; NA, NULL, "" and "dev" read as older than every release.
analyzer_at_least <- function(v, min) {
  if (is.null(v) || length(v) != 1L || is.na(v)) return(FALSE)
  lead <- regmatches(v, regexpr("^[0-9]+(\\.[0-9]+)*", v))
  if (!length(lead)) return(FALSE)
  utils::compareVersion(lead, min) >= 0L
}

# Seconds and counts one worker adds up for its package, by name; each fork has
# its own copy, which .with_worker_telemetry in update.R resets and reads.
.WORKER_TALLY <- new.env(parent = emptyenv())

.tally_reset <- function() {
  rm(list = ls(.WORKER_TALLY, all.names = TRUE), envir = .WORKER_TALLY)
  invisible(NULL)
}

.tally_add <- function(name, x) {
  assign(name, (get0(name, envir = .WORKER_TALLY, inherits = FALSE) %||% 0) + x,
         envir = .WORKER_TALLY)
  invisible(NULL)
}

.tally_snapshot <- function() {
  mget(sort(ls(.WORKER_TALLY, all.names = TRUE)), envir = .WORKER_TALLY)
}

# The analyzer's non-zero exits in a worker's tally, as its line carries them:
# "101 x2, 134 x1" for two runs that exited 101 and one that exited 134; ""
# when every run exited 0.
.analyzer_exit_text <- function(tally = .tally_snapshot()) {
  nm <- sort(grep("^analyzer_exit_", names(tally), value = TRUE))
  if (length(nm) == 0L) return("")
  paste(sprintf("%s x%d", sub("^analyzer_exit_", "", nm), as.integer(unlist(tally[nm]))),
        collapse = ", ")
}

# Seconds since t0, a proc.time()[["elapsed"]] reading.
.secs_since <- function(t0) proc.time()[["elapsed"]] - t0

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

# Build the per-dataset detail frame from parsed "dataset" records. Scalar
# fields become columns; nested values such as `columns` and `row_sketch` are
# kept as JSON strings (as .flatten_summary does for the summary record). The
# base columns lead in the order .empty_datasets_df declares, minus the
# package/version stamp that analyze.R applies later.
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

#' The condition parse_analyzer_records raises when lines still fail to parse
#' after the retry.
#'
#' @param n_bad Number of lines that did not parse.
#' @param line  The first of them.
#' @return A condition of class c("analyzer_parse_incomplete", "error",
#'   "condition") carrying n_bad and first_bad, the first 80 bytes of `line`.
.analyzer_parse_incomplete <- function(n_bad, line) {
  first_bad <- .head_bytes(line, 80L)
  structure(
    class = c("analyzer_parse_incomplete", "error", "condition"),
    list(message = sprintf("%d analyzer line(s) did not parse, the first begins: %s",
                           as.integer(n_bad), first_bad),
         call = NULL, n_bad = as.integer(n_bad), first_bad = first_bad))
}

# The first n bytes of x as valid UTF-8; a byte of a character the cut split,
# or of a line that was not UTF-8, is written <xx>.
.head_bytes <- function(x, n) {
  b <- charToRaw(x)
  if (length(b) > n) b <- b[seq_len(n)]
  iconv(rawToChar(b), from = "UTF-8", to = "UTF-8", sub = "byte")
}

# The parser that reads one line at a time: the fallback that counts and names
# a line that does not parse, and the result parse_analyzer_records must equal.
.parse_records_per_line <- function(lines) {
  summ <- NULL
  fn <- list(lang = character(0L), name = character(0L), exported = logical(0L),
             file = character(0L), line = integer(0L), loc = integer(0L),
             n_params = integer(0L), cyclocomp = integer(0L))
  eg <- list(graph = character(0L), from = character(0L), to = character(0L))
  ds_recs <- list()
  dcf <- NULL
  notes <- NULL
  n_bad <- 0L
  first_bad <- NULL

  for (line in lines) {
    # A blank line was never a record.
    if (!grepl("[^[:space:]]", line, useBytes = TRUE)) next
    failed <- FALSE
    parsed <- .retry_after_time_limit(
      jsonlite::fromJSON(line, simplifyVector = FALSE),
      error = function(e) {
        failed <<- TRUE
        NULL
      }
    )
    if (failed) {
      n_bad <- n_bad + 1L
      if (is.null(first_bad)) first_bad <- line
      next
    }
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
    } else if (identical(rec, "dcf")) {
      # An empty record still says the DESCRIPTION was read, so it stays character(0).
      if (is.null(dcf)) {
        fields <- parsed[setdiff(names(parsed), "rec")]
        dcf <- vapply(fields, function(v) if (is.null(v)) NA_character_ else as.character(v)[[1L]],
                      character(1L))
      }
    } else if (identical(rec, "release_notes")) {
      if (is.null(notes)) notes <- parsed
    }
  }

  # A record that did not parse is not dropped: the package fails, and its
  # stored rows stay as they were.
  if (n_bad > 0L) {
    .tally_add("incomplete_parses", 1)
    stop(.analyzer_parse_incomplete(n_bad, first_bad))
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
    datasets  = .datasets_frame(ds_recs),
    dcf       = dcf,
    release_notes = notes
  )
}

# Lines this long or longer are parsed one at a time, and dataset records among
# them memoised; shorter ones are parsed together.
.RECORD_LONG_BYTES <- 4096L

# Flattened dataset records of one package, keyed by the sha256 of their line,
# for this version and the previous one only: about two versions of dataset text.
.record_memo <- function() {
  memo <- new.env(parent = emptyenv())
  memo$cur  <- new.env(hash = TRUE, parent = emptyenv())
  memo$prev <- new.env(hash = TRUE, parent = emptyenv())
  memo
}

# A new version: this version's entries become the previous version's.
.record_memo_rotate <- function(memo) {
  memo$prev <- memo$cur
  memo$cur  <- new.env(hash = TRUE, parent = emptyenv())
  invisible(memo)
}

# A parsed record's "rec" string, or NA. A value that is not a list errors here
# exactly where the per-line parser's parsed[["rec"]] does.
.record_kind <- function(p) {
  if (is.null(p)) return(NA_character_)
  if (inherits(p, "dataset_flat")) return("dataset")
  rec <- p[["rec"]]
  if (is.character(rec) && length(rec) == 1L && !is.na(rec) && is.null(attributes(rec))) {
    rec
  } else {
    NA_character_
  }
}

# A dataset record as .datasets_frame_flat reads it, a pure function of the
# record: its field names, each scalar as parsed, the JSON .datasets_frame
# writes for each nested value, and n_cols.
.dataset_flat <- function(rec) {
  keys   <- setdiff(names(rec), "rec")
  vals   <- lapply(keys, function(k) rec[[k]])
  nested <- vapply(vals, function(v) !is.null(v) && (is.list(v) || length(v) != 1L),
                   logical(1L))
  json   <- rep(NA_character_, length(keys))
  json[nested] <- vapply(vals[nested], function(v) {
    as.character(jsonlite::toJSON(v, auto_unbox = TRUE, null = "null"))
  }, character(1L))
  vals[nested] <- list(NULL)
  names(vals)   <- keys
  names(nested) <- keys
  names(json)   <- keys
  cols <- rec[["columns"]]
  structure(list(names = names(rec), vals = vals, nested = nested, json = json,
                 n_cols = if (is.null(cols)) NA_integer_ else length(cols)),
            class = "dataset_flat")
}

# .datasets_frame over flattened records, with the same result. The choices
# that span records (which keys are nested anywhere, the JSON of a scalar in a
# nested column, each column's type) are made here, per version.
.datasets_frame_flat <- function(flats) {
  n <- length(flats)
  out <- list()
  if (n) {
    keys <- setdiff(unique(unlist(lapply(flats, function(f) f$names), use.names = FALSE)), "rec")
    for (k in keys) {
      vals   <- lapply(flats, function(f) f$vals[[k]])
      nested <- vapply(flats, function(f) isTRUE(f$nested[k]), logical(1L))
      if (any(nested)) {
        out[[k]] <- vapply(seq_len(n), function(i) {
          if (nested[[i]]) return(flats[[i]]$json[[k]])
          v <- vals[[i]]
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
    out[["n_cols"]] <- vapply(flats, function(f) f$n_cols, integer(1L))
  }
  for (k in names(.DATASET_BASE_COLS)) {
    if (is.null(out[[k]])) {
      out[[k]] <- rep(.DATASET_BASE_COLS[[k]][NA_integer_], n)
    }
  }
  ord <- c(names(.DATASET_BASE_COLS), setdiff(names(out), names(.DATASET_BASE_COLS)))
  out <- out[ord]
  df <- as.data.frame(out, stringsAsFactors = FALSE, optional = TRUE)
  names(df) <- ord
  df
}

# One long line. A dataset record the memo holds comes back without a parse (a
# hit from the previous version is kept for the next); otherwise the line is
# parsed, and a dataset record flattened and kept. Returns list(ok, value).
.parse_long_record <- function(line, memo) {
  key <- if (!is.null(memo)) digest::digest(line, algo = "sha256", serialize = FALSE)
  if (!is.null(key)) {
    hit <- get0(key, envir = memo$cur, inherits = FALSE)
    if (is.null(hit)) hit <- get0(key, envir = memo$prev, inherits = FALSE)
    if (!is.null(hit)) {
      assign(key, hit, envir = memo$cur)
      .tally_add("memo_hits", 1)
      return(list(ok = TRUE, value = hit))
    }
  }
  failed <- FALSE
  p <- .retry_after_time_limit(
    jsonlite::fromJSON(line, simplifyVector = FALSE),
    error = function(e) {
      failed <<- TRUE
      NULL
    }
  )
  if (failed) return(list(ok = FALSE, value = NULL))
  if (identical(.record_kind(p), "dataset")) {
    p <- .dataset_flat(p)
    if (!is.null(key)) {
      assign(key, p, envir = memo$cur)
      .tally_add("memo_misses", 1)
    }
  }
  list(ok = TRUE, value = p)
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
#'   - "dataset"   -> one row in the datasets frame.
#'   - "dcf"       -> the first such record, as a named character vector.
#'   - "release_notes" -> the first such record, as a list.
#' Record types not named here are skipped. Blank lines are skipped; any other
#' line that does not parse raises analyzer_parse_incomplete once all lines
#' have been read.
#'
#' Lines under .RECORD_LONG_BYTES are parsed in one call and longer ones one at
#' a time; the frames are built from the records in line order. A long dataset
#' record is kept in `memo` by the sha256 of its line, and the next version
#' reuses it. The result is identical to .parse_records_per_line(lines), with
#' or without a memo, and that parser takes over whenever the stream cannot be
#' read this way.
#'
#' @param lines Character vector of NDJSON lines (analyzer stdout).
#' @param memo  A .record_memo() shared by one package's versions, or NULL.
#' @return A list:
#'   $summary   flattened named list, or NULL if no summary record was present.
#'   $functions data.frame(lang, name, exported, file, line, loc, n_params,
#'              cyclocomp); zero rows when the stream has no function records.
#'   $edges     data.frame(graph, from, to); zero rows when none present.
#'   $datasets  data.frame of dataset records.
#'   $dcf       DESCRIPTION fields; character(0) for an empty record, NULL for none.
#'   $release_notes the release_notes record, or NULL.
parse_analyzer_records <- function(lines, memo = NULL) {
  lines  <- lines[grepl("[^[:space:]]", lines, useBytes = TRUE)]
  if (!is.null(memo)) .record_memo_rotate(memo)
  long   <- nchar(lines, type = "bytes") >= .RECORD_LONG_BYTES
  parsed <- vector("list", length(lines))
  short  <- which(!long)
  if (length(short)) {
    batch <- .retry_after_time_limit(
      jsonlite::fromJSON(paste0("[", paste(lines[short], collapse = ","), "]"),
                         simplifyVector = FALSE),
      error = function(e) NULL
    )
    # A line that does not parse, or one holding two values, sends the stream
    # through the per-line parser, which counts and names the bad line.
    if (!is.list(batch) || length(batch) != length(short)) {
      return(.parse_records_per_line(lines))
    }
    parsed[short] <- batch
  }
  for (i in which(long)) {
    one <- .parse_long_record(lines[[i]], memo)
    if (!one$ok) return(.parse_records_per_line(lines))
    parsed[i] <- list(one$value)
  }

  kinds <- vapply(parsed, .record_kind, character(1L))
  first <- function(kind) {
    i <- match(kind, kinds)
    if (is.na(i)) NULL else parsed[[i]]
  }
  fns <- parsed[kinds %in% "function"]
  egs <- parsed[kinds %in% "call_edge"]
  ds  <- lapply(parsed[kinds %in% "dataset"], function(p) {
    if (inherits(p, "dataset_flat")) p else .dataset_flat(p)
  })
  summ <- first("summary")
  dcf  <- first("dcf")
  if (!is.null(dcf)) {
    # An empty record still says the DESCRIPTION was read, so it stays character(0).
    fields <- dcf[setdiff(names(dcf), "rec")]
    dcf <- vapply(fields, function(v) if (is.null(v)) NA_character_ else as.character(v)[[1L]],
                  character(1L))
  }

  functions <- data.frame(
    lang      = vapply(fns, .rec_chr, character(1L), key = "lang"),
    name      = vapply(fns, .rec_chr, character(1L), key = "name"),
    exported  = vapply(fns, .rec_lgl, logical(1L),   key = "exported"),
    file      = vapply(fns, .rec_chr, character(1L), key = "file"),
    line      = vapply(fns, .rec_int, integer(1L),   key = "line"),
    loc       = vapply(fns, .rec_int, integer(1L),   key = "loc"),
    n_params  = vapply(fns, .rec_int, integer(1L),   key = "n_params"),
    cyclocomp = vapply(fns, .rec_int, integer(1L),   key = "cyclocomp"),
    stringsAsFactors = FALSE
  )
  edges <- data.frame(
    graph = vapply(egs, .rec_chr, character(1L), key = "graph"),
    from  = vapply(egs, .rec_chr, character(1L), key = "from"),
    to    = vapply(egs, .rec_chr, character(1L), key = "to"),
    stringsAsFactors = FALSE
  )

  list(
    summary   = if (is.null(summ)) NULL else .flatten_summary(summ),
    functions = functions,
    edges     = edges,
    datasets  = .datasets_frame_flat(ds),
    dcf       = dcf,
    release_notes = first("release_notes")
  )
}

# prlimit's path, "" where there is none, which is anywhere but Linux.
.prlimit_bin <- function() unname(Sys.which("prlimit"))

# The address-space limit an analyzer of build `version` runs under, in MiB,
# and the prlimit that holds it there: ANALYZER_MEMORY_LIMIT_MB when that is
# above zero, the build is 0.5.2 or later and prlimit is found, and 0 with no
# prlimit otherwise. A build before 0.5.2 can write a wrong record and exit 0
# when an allocation fails, so it never runs under a limit. run_update works
# this out once a run, from the build it has read.
.analyzer_limit <- function(version = rpkg_analyzer_version(),
                            limit_mb = ANALYZER_MEMORY_LIMIT_MB) {
  prlimit <- if (isTRUE(limit_mb > 0L) && analyzer_at_least(version, "0.5.2")) {
    .prlimit_bin()
  } else {
    ""
  }
  list(limit_mb = if (nzchar(prlimit)) as.integer(limit_mb) else 0L, prlimit = prlimit)
}

# How to run `bin` under `limit`, an .analyzer_limit(): through its prlimit,
# with its address space and no core file, when it is above zero, and as it is
# otherwise. An analyzer aborted at the limit then leaves no core file of up to
# the limit's size on the runner. limit_mb is the limit the run gets, 0 for none.
.analyzer_command <- function(bin, limit = .analyzer_limit()) {
  if (!isTRUE(limit$limit_mb > 0L)) {
    return(list(command = bin, args = character(0L), limit_mb = 0L))
  }
  list(command = limit$prlimit,
       args = c(sprintf("--as=%.0f", as.numeric(limit$limit_mb) * 1024^2), "--core=0",
                shQuote(bin)),
       limit_mb = as.integer(limit$limit_mb))
}

# What a failure says of the limit it ran under: an analyzer past its limit
# aborts, so exit status 134 beside a limit is how that reads.
.limit_note <- function(limit_mb) {
  if (isTRUE(limit_mb > 0L)) {
    sprintf("; its address-space limit was %d MiB", as.integer(limit_mb))
  } else {
    ""
  }
}

#' The condition analyze_with_binary raises for an analyzer that was killed: an
#' exit status of 128 or above, or a zero status with no statistics line.
#'
#' @param status   The exit status; NA when the statistics line is what is missing.
#' @param limit_mb The address-space limit the run had, in MiB; 0 for none.
#' @return A condition of class c("analyzer_killed", "error", "condition")
#'   carrying status and limit_mb.
.analyzer_killed <- function(status = NA_integer_, limit_mb = 0L) {
  status <- as.integer(status)
  structure(
    class = c("analyzer_killed", "error", "condition"),
    list(message = if (is.na(status)) {
           "rpkg-analyzer exited 0 without its statistics line"
         } else {
           sprintf("rpkg-analyzer was killed, exit status %d%s", status, .limit_note(limit_mb))
         },
         call = NULL, status = status, limit_mb = as.integer(limit_mb)))
}

#' The condition analyze_with_binary raises when a version that has a stored
#' analyzer row gets no usable result and the analyzer was not killed.
#'
#' @param status   The exit status; NA when the run left none.
#' @param what     What happened instead, when there is no status.
#' @param limit_mb The address-space limit the run had, in MiB; 0 for none.
#' @return A condition of class c("analyzer_failed", "error", "condition")
#'   carrying status.
.analyzer_failed <- function(status = NA_integer_, what = "gave no result", limit_mb = 0L) {
  status <- as.integer(status)
  note   <- ""
  if (!is.na(status)) {
    what <- sprintf("exited %d", status)
    note <- .limit_note(limit_mb)
  }
  structure(
    class = c("analyzer_failed", "error", "condition"),
    list(message = sprintf("rpkg-analyzer %s on a version with analyzer rows%s", what, note),
         call = NULL, status = status))
}

# The size of the statistics file, 0 when none is named or it does not exist.
.stats_bytes <- function(path) {
  n <- if (nzchar(path)) file.size(path) else NA_real_
  if (is.na(n)) 0 else n
}

# Whether a run left no statistics line although its build writes one: 0.5.1
# and later, with RPKG_ANALYZER_STATS naming the file.
.stats_line_missing <- function(path, bytes_before) {
  nzchar(path) && .stats_bytes(path) <= bytes_before &&
    analyzer_at_least(rpkg_analyzer_version(), "0.5.1")
}

#' Run the analyzer over an extracted package directory.
#'
#' @param dir Path to the extracted package source (a DESCRIPTION at its root).
#' @param kind The input kind passed as --input-kind.
#' @param memo The package's .record_memo(), or NULL.
#' @param protect Whether the version has a stored analyzer row, which a
#'   fallback row must not replace.
#' @param limit The address-space limit the analyzer runs under, an
#'   .analyzer_limit().
#' @return A flat named list of metrics for the version, with nested values
#'   (maps and arrays) serialised to JSON strings to match how the R metric
#'   groups store fields such as lang_breakdown. The per-function and
#'   per-call-edge detail frames are attached as the "functions" and "edges"
#'   attributes (data.frames without package/version stamps). Raises
#'   analyzer_killed for a status of 128 or above or a zero status with no
#'   statistics line. Any other run with no usable result (no binary, a binary
#'   that could not be run, another non-zero status, no summary record) is
#'   NULL on an unprotected version and raises analyzer_failed on a protected
#'   one.
analyze_with_binary <- function(dir, kind = ANALYZER_INPUT_KIND, memo = NULL,
                                protect = FALSE, limit = .analyzer_limit()) {
  # No usable result: the R fallback, unless its row would replace an analyzer row.
  unusable <- function(...) {
    if (isTRUE(protect)) stop(.analyzer_failed(...))
    NULL
  }
  bin <- rpkg_analyzer_bin()
  if (!nzchar(bin)) return(unusable(what = "was not found"))

  # A non-zero exit signals a warning and leaves a status, which is read below.
  # R raises an error of its own for status 127 and for a pipe it cannot open.
  run   <- .analyzer_command(bin, limit)
  stats <- Sys.getenv("RPKG_ANALYZER_STATS", unset = "")
  t0  <- proc.time()[["elapsed"]]
  out <- .retry_after_time_limit({
    stats_before <- .stats_bytes(stats)
    withCallingHandlers(system2(run$command,
                                c(run$args, shQuote(dir), "--input-kind", kind),
                                stdout = TRUE, stderr = FALSE),
                        warning = function(w) invokeRestart("muffleWarning"))
  }, error = function(e) e)
  .tally_add("analyzer_s", .secs_since(t0))
  if (inherits(out, "condition")) {
    return(unusable(what = sprintf("could not be run (%s)", conditionMessage(out))))
  }

  status <- as.integer(attr(out, "status") %||% 0L)
  if (status != 0L) .tally_add(sprintf("analyzer_exit_%d", status), 1)
  if (status >= 128L) stop(.analyzer_killed(status, run$limit_mb))
  if (status != 0L) return(unusable(status, limit_mb = run$limit_mb))
  # A signalled analyzer can come back with partial lines and no status.
  if (.stats_line_missing(stats, stats_before)) stop(.analyzer_killed())
  if (length(out) == 0L) return(unusable(what = "printed nothing"))

  t0 <- proc.time()[["elapsed"]]
  parsed <- parse_analyzer_records(out, memo)
  .tally_add("parse_s", .secs_since(t0))
  if (is.null(parsed$summary)) return(unusable(what = "printed no summary record"))

  metrics <- parsed$summary
  attr(metrics, "functions") <- parsed$functions
  attr(metrics, "edges")     <- parsed$edges
  attr(metrics, "datasets")  <- parsed$datasets
  attr(metrics, "dcf")       <- parsed$dcf
  attr(metrics, "release_notes") <- parsed$release_notes
  metrics
}

#' Whether the analyzer honours the input kind this pipeline passes: a
#' DESCRIPTION-only package must come back with a summary naming `kind`. An
#' analyzer that is killed, or fails, on that package does not pass. It runs
#' under `limit`, the address-space limit every package's analyzer gets.
rpkg_analyzer_selfcheck <- function(kind = ANALYZER_INPUT_KIND, limit = .analyzer_limit()) {
  dir <- tempfile("selfcheck_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE, force = TRUE), add = TRUE)
  writeLines(c("Package: selfcheck", "Version: 0.0.1"), file.path(dir, "DESCRIPTION"))
  metrics <- tryCatch(analyze_with_binary(dir, kind = kind, limit = limit),
                      analyzer_killed = function(e) NULL,
                      analyzer_failed = function(e) NULL)
  if (is.null(metrics)) return(FALSE)
  identical(as.character(metrics[["input_kind"]] %||% NA_character_), kind)
}
