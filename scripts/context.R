# scripts/context.R: per-version context construction + DESCRIPTION/NAMESPACE parsers.
# Dependency: config.R must be sourced first (%||%).

#' Parse a DESCRIPTION (DCF-format) string into a named list.
#'
#' Folds continuation lines (lines starting with whitespace) into the preceding
#' field value. When a field appears more than once, the LAST occurrence wins.
#' Returns an empty list for blank or unparseable input.
#'
#' @param text  Character string containing DCF content.
#' @return Named list; values are character strings (trimmed).
parse_dcf <- function(text) {
  if (!nzchar(trimws(text %||% ""))) return(list())
  lines <- strsplit(text, "\n", fixed = TRUE)[[1L]]
  out   <- list()
  cur_key <- NULL

  for (line in lines) {
    # Blank line: record separator (ignored; DCF allows multiple records)
    if (!nzchar(trimws(line))) {
      cur_key <- NULL
      next
    }
    # Continuation line: starts with whitespace
    if (grepl("^[ \t]", line) && !is.null(cur_key)) {
      out[[cur_key]] <- paste0(out[[cur_key]], "\n", trimws(line))
      next
    }
    # Field line: Key: value
    colon <- regexpr(":", line, fixed = TRUE)
    if (colon == -1L) next
    cur_key       <- trimws(substring(line, 1L, colon - 1L))
    val           <- trimws(substring(line, colon + 1L))
    out[[cur_key]] <- val
  }
  out
}

#' Parse a NAMESPACE file into export/import component lists.
#'
#' Handles the directives used by CRAN packages:
#'   export(sym, ...)           -> $exports (character vector)
#'   exportPattern("regex")     -> $exports (the pattern string prefixed "pattern:")
#'   S3method(generic, class)   -> $S3method ("generic.class" strings)
#'   importFrom(pkg, fn, ...)   -> $importFrom ("pkg::fn" strings)
#'   useDynLib(lib, sym, ...)   -> $useDynLib (lib name and "lib::sym" for named syms)
#'
#' Comments (# ...) are stripped.  Directives that span multiple physical lines
#' are NOT supported (NAMESPACE files virtually never do this in practice).
#'
#' @param text  Character string containing NAMESPACE file content.
#' @return Named list with character vector components:
#'   $exports, $importFrom, $S3method, $useDynLib.
parse_namespace <- function(text) {
  result <- list(
    exports    = character(0L),
    importFrom = character(0L),
    S3method   = character(0L),
    useDynLib  = character(0L)
  )
  if (!nzchar(trimws(text %||% ""))) return(result)

  lines <- strsplit(text, "\n", fixed = TRUE)[[1L]]
  # Strip comments and blank lines
  lines <- sub("#.*$", "", lines)
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]
  if (length(lines) == 0L) return(result)

  # Collapse lines that appear to be continuations of a multi-line call
  # (ends without closing paren).  Simple heuristic: count parens.
  collapsed <- character(0L)
  buf <- ""
  depth <- 0L
  for (ln in lines) {
    opens  <- nchar(gsub("[^(]", "", ln))
    closes <- nchar(gsub("[^)]", "", ln))
    if (!nzchar(buf) && opens == 0L && closes == 0L) {
      # No parens at all -- just a bare identifier line, pass through
      collapsed <- c(collapsed, ln)
      next
    }
    buf   <- if (nzchar(buf)) paste0(buf, " ", ln) else ln
    depth <- depth + opens - closes
    if (depth <= 0L) {
      collapsed <- c(collapsed, buf)
      buf   <- ""
      depth <- 0L
    }
  }
  if (nzchar(buf)) collapsed <- c(collapsed, buf)

  # Helper: extract args from "directive(arg1, arg2, ...)" -> character vector
  # Handles quoted strings and unquoted identifiers; skips named args (key=val).
  parse_args <- function(content) {
    # content is everything inside the outermost parens
    content <- trimws(content)
    if (!nzchar(content)) return(character(0L))
    # Simple split by comma, then clean each token
    # This is not fully general (nested parens in strings), but sufficient for NAMESPACE
    tokens <- strsplit(content, ",")[[1L]]
    tokens <- trimws(tokens)
    # Remove named arguments (key = value) -- skip any token containing "="
    # (except quoted strings that happen to contain "=")
    tokens <- tokens[!grepl("^[A-Za-z._][A-Za-z0-9._]*\\s*=", tokens)]
    # Strip surrounding quotes
    tokens <- gsub('^["\']|["\']$', "", tokens)
    tokens <- trimws(tokens)
    tokens[nzchar(tokens)]
  }

  for (line in collapsed) {
    # Match: directive(...)
    m <- regexpr("^([A-Za-z][A-Za-z0-9_.]*)\\s*\\((.*)\\)\\s*$", line, perl = TRUE)
    if (m == -1L) next
    starts <- attr(m, "capture.start")
    lens   <- attr(m, "capture.length")
    directive <- substring(line, starts[1L], starts[1L] + lens[1L] - 1L)
    inner     <- substring(line, starts[2L], starts[2L] + lens[2L] - 1L)

    if (directive == "export") {
      args <- parse_args(inner)
      result$exports <- c(result$exports, args)

    } else if (directive == "exportPattern") {
      pat <- trimws(inner)
      pat <- gsub('^["\']|["\']$', "", pat)
      if (nzchar(pat)) result$exports <- c(result$exports, paste0("pattern:", pat))

    } else if (directive == "S3method") {
      args <- parse_args(inner)
      if (length(args) >= 2L) {
        result$S3method <- c(result$S3method,
                             paste0(args[1L], ".", args[2L]))
      }

    } else if (directive == "importFrom") {
      args <- parse_args(inner)
      if (length(args) >= 2L) {
        pkg   <- args[1L]
        fns   <- args[-1L]
        result$importFrom <- c(result$importFrom,
                               paste0(pkg, "::", fns))
      }

    } else if (directive == "useDynLib") {
      args <- parse_args(inner)
      if (length(args) >= 1L) {
        lib <- args[1L]
        result$useDynLib <- c(result$useDynLib, lib)
        if (length(args) >= 2L) {
          syms <- args[-1L]
          result$useDynLib <- c(result$useDynLib, paste0(lib, "::", syms))
        }
      }
    }
  }

  result
}

#' Build a per-version context object used by all metric group functions.
#'
#' The returned environment (`ctx`) exposes:
#'   $package      - package name (character)
#'   $version      - version string (character)
#'   $ref          - git ref / tag (character)
#'   $date         - author date YYYY-MM-DD (character)
#'   $files        - character vector of file paths in this version tree
#'   $read(path)   - content of a file as a single string; "" when absent
#'                   (memoised: each path is read at most once per ctx)
#'   $lines(path)  - content as character vector of lines (split on "\n")
#'   $exists(path) - TRUE when path is in $files
#'   $find(regex)  - files whose path matches the regex
#'   $desc         - named list: parse_dcf of DESCRIPTION (lazily evaluated)
#'   $namespace    - named list: parse_namespace of NAMESPACE (lazily evaluated)
#'   $churn        - data.frame(file, added, deleted) for THIS version's commit
#'   $prev_exports - character vector of exports from the previous version, or NULL
#'
#' @param package      Package name.
#' @param version      Version string.
#' @param ref          Git ref (tag or commit SHA).
#' @param date         Author date (YYYY-MM-DD).
#' @param files        Character vector of file paths in the version tree.
#' @param read_fn      function(path) -> character string or NULL/"" when absent.
#' @param churn_df     data.frame(file, added, deleted) for this version; NULL ok.
#' @param prev_exports Character vector of exports from the prior version; NULL for v1.
#' @return An environment with the fields and methods described above.
build_context <- function(package, version, ref, date, files, read_fn,
                          churn_df = NULL, prev_exports = NULL) {
  # Memoisation cache: path -> content string
  .cache <- new.env(parent = emptyenv())

  read_memo <- function(path) {
    cached <- .cache[[path]]
    if (!is.null(cached)) return(cached)
    content <- tryCatch(read_fn(path), error = function(e) "")
    if (is.null(content) || length(content) == 0L) content <- ""
    .cache[[path]] <- content
    content
  }

  lines_fn <- function(path) {
    content <- read_memo(path)
    if (!nzchar(content)) return(character(0L))
    strsplit(content, "\n", fixed = TRUE)[[1L]]
  }

  exists_fn <- function(path) path %in% files

  find_fn <- function(regex) grep(regex, files, value = TRUE)

  # Normalize churn_df to a stable schema
  if (is.null(churn_df) || nrow(churn_df) == 0L) {
    churn_df <- data.frame(
      file    = character(0L),
      added   = integer(0L),
      deleted = integer(0L),
      stringsAsFactors = FALSE
    )
  }

  # Lazy desc / namespace via closures and active bindings
  .desc      <- NULL
  .namespace <- NULL

  desc_fn <- function() {
    if (is.null(.desc)) .desc <<- parse_dcf(read_memo("DESCRIPTION"))
    .desc
  }

  ns_fn <- function() {
    if (is.null(.namespace)) .namespace <<- parse_namespace(read_memo("NAMESPACE"))
    .namespace
  }

  ctx <- new.env(parent = emptyenv())
  ctx$package      <- package
  ctx$version      <- version
  ctx$ref          <- ref
  ctx$date         <- date
  ctx$files        <- files
  ctx$read         <- read_memo
  ctx$lines        <- lines_fn
  ctx$exists       <- exists_fn
  ctx$find         <- find_fn
  ctx$churn        <- churn_df
  ctx$prev_exports <- prev_exports

  makeActiveBinding("desc",      desc_fn, ctx)
  makeActiveBinding("namespace", ns_fn,   ctx)

  ctx
}
