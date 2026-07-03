# scripts/metrics/health.R: health/hygiene code metrics.
# Dependency: config.R, context.R must be sourced first.

#' Compute health/hygiene metrics for a package version.
#'
#' Scans R/ source files for hygiene signals using text-based heuristics.
#' All metrics are NA-safe: absent or empty input returns NA rather than
#' erroring or warning.  Regex patterns favour precision over recall
#' (conservative: signals, not verdicts).
#'
#' @param ctx  A context environment as returned by build_context().
#' @return Named list of scalars:
#'   on_exit_coverage_rate     numeric  fraction of state-mutating R/ functions
#'                                      that also call on.exit(); NA if none mutate
#'   global_state_write_density numeric count of <<-, assign()-to-global,
#'                                      options() setters, Sys.setenv() per KLOC of R/
#'   deprecated_idiom_density  numeric  count of bare T/F, 1:length/nrow/ncol,
#'                                      require()/library() in bodies, .Internal()
#'                                      per KLOC of R/; NA when no R/ exists
#'   debug_artifact_density    numeric  browser() and stray print()/cat() per KLOC
#'                                      of R/ (test files live in tests/, not R/)
#'   has_code_of_conduct       logical  CODE_OF_CONDUCT.md or .github/... present
#'   has_contributing_guide    logical  CONTRIBUTING.md or .github/... present
metrics_health <- function(ctx) {

  # ---- internal helpers ---------------------------------------------------

  # Strip single-line comments (rough: ignores # inside strings).
  strip_comments <- function(lines) sub("#.*$", "", lines)

  # Count '{' or '}' characters in a single string.
  n_open  <- function(s) nchar(gsub("[^{]", "", s))
  n_close <- function(s) nchar(gsub("[^}]", "", s))

  # Extract top-level function bodies from comment-stripped lines.
  #
  # Brace tracking handles multi-line argument lists by scanning up to 15
  # lines ahead for the opening '{'.  Nested function bodies are absorbed
  # into the enclosing body and are NOT separately extracted.  This is a
  # conservative heuristic: some false positives (brace inside a string)
  # and false negatives (multi-line sigs > 15 lines) are possible but rare.
  extract_function_bodies <- function(lines) {
    stripped <- strip_comments(lines)
    n        <- length(stripped)
    bodies   <- list()
    i        <- 1L

    while (i <= n) {
      if (!grepl("\\bfunction\\s*\\(", stripped[[i]], perl = TRUE)) {
        i <- i + 1L
        next
      }

      # Locate the opening '{' (handles multi-line signatures).
      brace_at <- NA_integer_
      for (k in seq(i, min(i + 15L, n))) {
        if (n_open(stripped[[k]]) > 0L) { brace_at <- k; break }
      }

      if (is.na(brace_at)) {
        # No '{' found within look-ahead: single-expression body.
        bodies <- c(bodies, list(stripped[[i]]))
        i      <- i + 1L
        next
      }

      # Collect signature lines plus body until brace depth returns to 0.
      body_lines <- stripped[i:brace_at]
      depth <- sum(vapply(body_lines,
                          function(l) n_open(l) - n_close(l),
                          integer(1L)))
      j <- brace_at + 1L

      while (j <= n && depth > 0L) {
        l          <- stripped[[j]]
        body_lines <- c(body_lines, l)
        depth      <- depth + n_open(l) - n_close(l)
        j          <- j + 1L
      }

      bodies <- c(bodies, list(body_lines))
      i <- j   # continue from the first line after the closing '}'
    }
    bodies
  }

  # Patterns that indicate a function body mutates shared/global state.
  # options()/par() with a named argument (=) are setters; bare calls are getters.
  # Connection-opening functions are included because unclosed connections
  # affect global file-descriptor state.
  MUTATOR_PATS <- c(
    "<<-",
    "\\boptions\\s*\\([^)]*=",
    "\\bpar\\s*\\([^)]*=",
    "\\bsetwd\\s*\\(",
    "\\bSys\\.setenv\\s*\\(",
    "\\bsink\\s*\\(",
    "\\bfile\\s*\\(",
    "\\burl\\s*\\(",
    "\\bpipe\\s*\\(",
    "\\bgzfile\\s*\\(",
    "\\bbzfile\\s*\\(",
    "\\bxzfile\\s*\\(",
    "\\btextConnection\\s*\\("
  )

  body_has_mutator <- function(body) {
    txt <- paste(body, collapse = "\n")
    any(vapply(MUTATOR_PATS,
               function(p) grepl(p, txt, perl = TRUE),
               logical(1L)))
  }

  body_has_on_exit <- function(body) {
    grepl("\\bon\\.exit\\s*\\(", paste(body, collapse = "\n"), perl = TRUE)
  }

  # ---- R/ file inventory --------------------------------------------------

  r_files <- ctx$find("^R/.*\\.R$")
  r_loc   <- 0L
  for (f in r_files) r_loc <- r_loc + length(ctx$lines(f))
  kloc_r  <- if (r_loc > 0L) r_loc / 1000 else 0

  # ---- on_exit_coverage_rate ----------------------------------------------
  # Fraction of R/ function bodies that both mutate global/shared state AND
  # call on.exit().  NA when no function mutates state (denominator = 0).

  n_mutating <- 0L
  n_on_exit  <- 0L

  for (f in r_files) {
    lns <- ctx$lines(f)
    if (length(lns) == 0L) next
    for (body in extract_function_bodies(lns)) {
      if (body_has_mutator(body)) {
        n_mutating <- n_mutating + 1L
        if (body_has_on_exit(body)) n_on_exit <- n_on_exit + 1L
      }
    }
  }

  on_exit_coverage_rate <- if (n_mutating == 0L) NA_real_ else n_on_exit / n_mutating

  # ---- global_state_write_density -----------------------------------------
  # Per KLOC of R/: <<-, assign()-to-global, options() setters, Sys.setenv().
  # NA when there are no R/ files (kloc_r == 0).

  global_state_write_density <- if (kloc_r == 0) NA_real_ else {
    cnt <- 0L
    for (f in r_files) {
      lns <- strip_comments(ctx$lines(f))
      # <<- super-assignment operator
      cnt <- cnt + sum(grepl("<<-", lns, fixed = TRUE))
      # assign() explicitly targeting global or base environment
      cnt <- cnt + sum(grepl(
        "\\bassign\\s*\\([^)]*(?:\\.GlobalEnv|globalenv\\s*\\(|baseenv\\s*\\()",
        lns, perl = TRUE))
      # options() setter: has a named argument (contains '=')
      cnt <- cnt + sum(grepl("\\boptions\\s*\\([^)]*=",  lns, perl = TRUE))
      # Sys.setenv() always sets environment variables (global side-effect)
      cnt <- cnt + sum(grepl("\\bSys\\.setenv\\s*\\(",   lns, perl = TRUE))
    }
    cnt / kloc_r
  }

  # ---- deprecated_idiom_density -------------------------------------------
  # Per KLOC of R/: bare T/F boolean literals, 1:length/nrow/ncol sequences,
  # require()/library() inside function bodies (indentation heuristic), .Internal().
  # NA when there are no R/ files.

  deprecated_idiom_density <- if (kloc_r == 0) NA_real_ else {
    cnt <- 0L
    for (f in r_files) {
      lns <- strip_comments(ctx$lines(f))

      # Bare T or F as boolean literal: not preceded or followed by an
      # identifier character, and not followed by '=' (assignment or comparison).
      cnt <- cnt + sum(grepl(
        "(?<![A-Za-z0-9_.])[TF](?![A-Za-z0-9_.=])",
        lns, perl = TRUE))

      # 1:length(x), 1:nrow(x), 1:ncol(x) — prefer seq_len/seq_along.
      cnt <- cnt + sum(grepl(
        "\\b1:(?:length|nrow|ncol)\\s*\\(",
        lns, perl = TRUE))

      # require()/library() inside function bodies: lines with 2+ leading
      # spaces/tabs are assumed to be inside a block (indentation heuristic).
      cnt <- cnt + sum(
        grepl("^[ \t]{2,}", lns) &
        grepl("\\b(?:require|library)\\s*\\(", lns, perl = TRUE))

      # .Internal() calls (package code should not use internal primitives).
      cnt <- cnt + sum(grepl("\\.Internal\\s*\\(", lns, perl = TRUE))
    }
    cnt / kloc_r
  }

  # ---- debug_artifact_density ---------------------------------------------
  # Per KLOC of R/ (test files live in tests/, not R/).
  # Detects browser() and stray print()/cat() at statement position (start
  # of line after optional whitespace).  Lines that also contain message(),
  # warning(), or stop() are excluded (intentional output contexts).

  debug_artifact_density <- if (kloc_r == 0) NA_real_ else {
    cnt <- 0L
    for (f in r_files) {
      lns <- strip_comments(ctx$lines(f))
      # browser() at statement position
      cnt <- cnt + sum(grepl("^\\s*browser\\s*\\(\\s*\\)", lns, perl = TRUE))
      # stray print()/cat() at statement position, outside message/warning context
      is_warn_ctx <- grepl(
        "\\b(?:message|warning|stop)\\s*\\(", lns, perl = TRUE)
      cnt <- cnt + sum(
        grepl("^\\s*(?:print|cat)\\s*\\(", lns, perl = TRUE) & !is_warn_ctx)
    }
    cnt / kloc_r
  }

  # ---- community health files ---------------------------------------------

  has_code_of_conduct <- ctx$exists("CODE_OF_CONDUCT.md") ||
    ctx$exists(".github/CODE_OF_CONDUCT.md")

  has_contributing_guide <- ctx$exists("CONTRIBUTING.md") ||
    ctx$exists(".github/CONTRIBUTING.md")

  # ---- result -------------------------------------------------------------

  list(
    on_exit_coverage_rate      = on_exit_coverage_rate,
    global_state_write_density = global_state_write_density,
    deprecated_idiom_density   = deprecated_idiom_density,
    debug_artifact_density     = debug_artifact_density,
    has_code_of_conduct        = has_code_of_conduct,
    has_contributing_guide     = has_contributing_guide
  )
}
