# scripts/metrics/functions.R: function-surface metrics.
# Dependency: config.R, context.R must be sourced first.

#' Compute function-surface metrics for a package version.
#'
#' Metrics:
#'   n_exports          integer  count of NAMESPACE exports (explicit export()
#'                               directives); when only exportPattern is present,
#'                               count top-level R/ function defs matching the pattern
#'   n_internal         integer  top-level function defs in R/ NOT exported
#'   nse_surface_n      integer  count of exported fns whose R/ body uses
#'                               eval/substitute/quote/bquote/match.call/sys.call
#'   nse_surface_frac   numeric  nse_surface_n / n_exports (NA when denominator 0)
#'   triple_colon_count integer  total pkg:::sym occurrences in R/ files
#'   triple_colon_pkgs  integer  distinct external packages accessed via ::: in R/
#'                               (self-references excluded)
#'
#' All metrics are NA-safe: absent/empty/malformed input returns NA rather than
#' erroring.  Regex scanners are signals, not verdicts.
#'
#' @param ctx  A context environment as returned by build_context().
#' @return Named list of scalars.
metrics_functions <- function(ctx) {

  # ── helpers ─────────────────────────────────────────────────────────────────

  r_files <- ctx$find("^R/.*\\.[Rr]$")

  # Conservative regex for a top-level function definition:
  # identifier (no leading whitespace) followed by <- / <<- / = then function(
  # Note: dot must NOT lead a character class (PCRE rejects [. as collating element).
  fn_def_re <- "^([A-Za-z.][A-Za-z0-9_.]*)\\s*(?:<<?-|=)\\s*function\\s*\\("

  # Collect all top-level function definitions found in R/ files.
  # Returns a named list: fn_name -> list(file, line) for first occurrence.
  build_fn_lookup <- function() {
    lookup <- list()
    for (f in r_files) {
      lns <- ctx$lines(f)
      if (length(lns) == 0L) next
      for (i in seq_along(lns)) {
        m <- regexpr(fn_def_re, lns[[i]], perl = TRUE)
        if (m == -1L) next
        # Extract capture group 1 (the function name) via capture attributes.
        # Using sub(re, "\\1", line) would append the unmatched tail of the line.
        cap_s <- attr(m, "capture.start")[1L]
        cap_l <- attr(m, "capture.length")[1L]
        nm    <- substr(lns[[i]], cap_s, cap_s + cap_l - 1L)
        if (!nm %in% names(lookup)) {
          lookup[[nm]] <- list(file = f, line = i)
        }
      }
    }
    lookup
  }

  # Extract approximate function body text starting at line start_idx in lines.
  # Uses brace-depth tracking; falls back to the definition line for single-
  # expression bodies.  Capped at 200 lines to avoid runaway scans.
  extract_body <- function(lines, start_idx) {
    n <- length(lines)
    if (start_idx > n) return("")
    depth     <- 0L
    seen_open <- FALSE
    limit     <- min(start_idx + 199L, n)
    for (i in seq(start_idx, limit)) {
      ln     <- lines[[i]]
      opens  <- nchar(gsub("[^{]", "", ln))
      closes <- nchar(gsub("[^}]", "", ln))
      depth  <- depth + opens - closes
      if (opens > 0L) seen_open <- TRUE
      if (seen_open && depth <= 0L) {
        return(paste(lines[seq(start_idx, i)], collapse = "\n"))
      }
    }
    # No balanced braces found within limit: single-expression body or very long.
    # Return up to 3 lines from the definition to catch inline expressions.
    paste(lines[seq(start_idx, min(start_idx + 2L, n))], collapse = "\n")
  }

  # ── parse NAMESPACE ─────────────────────────────────────────────────────────

  has_ns   <- ctx$exists("NAMESPACE")
  ns       <- tryCatch(ctx$namespace, error = function(e) NULL)
  ns_exps  <- if (is.null(ns)) character(0L) else ns$exports %||% character(0L)

  is_pat   <- startsWith(ns_exps, "pattern:")
  explicit <- ns_exps[!is_pat]
  patterns <- sub("^pattern:", "", ns_exps[is_pat])

  # Check whether a name is exported (explicit or via any exportPattern).
  fn_is_exported <- function(nm) {
    if (nm %in% explicit) return(TRUE)
    if (length(patterns) == 0L) return(FALSE)
    any(vapply(patterns, function(p)
      tryCatch(isTRUE(grepl(p, nm, perl = TRUE)), error = function(e) FALSE),
      logical(1L)))
  }

  # ── collect R/ function definitions ─────────────────────────────────────────

  fn_lookup  <- tryCatch(build_fn_lookup(), error = function(e) list())
  r_fn_names <- names(fn_lookup)

  # ── n_exports ────────────────────────────────────────────────────────────────

  n_exports <- tryCatch({
    if (!has_ns) {
      NA_integer_
    } else if (length(ns_exps) == 0L) {
      0L
    } else if (length(explicit) > 0L) {
      # Explicit exports present; if patterns also exist, count R/ names that
      # match a pattern and are not already in explicit.
      extra <- if (length(patterns) > 0L) {
        pat_matched <- r_fn_names[vapply(r_fn_names, fn_is_exported, logical(1L))]
        sum(!pat_matched %in% explicit)
      } else {
        0L
      }
      length(explicit) + extra
    } else {
      # Only exportPattern(s): count R/ top-level fns matching any pattern.
      sum(vapply(r_fn_names, fn_is_exported, logical(1L)))
    }
  }, error = function(e) NA_integer_)

  # ── n_internal ───────────────────────────────────────────────────────────────

  n_internal <- tryCatch({
    if (length(r_fn_names) == 0L) {
      0L
    } else {
      n_exp <- sum(vapply(r_fn_names, fn_is_exported, logical(1L)))
      length(r_fn_names) - n_exp
    }
  }, error = function(e) NA_integer_)

  # ── nse_surface ──────────────────────────────────────────────────────────────

  nse_pat <- "\\b(eval|substitute|quote|bquote|match\\.call|sys\\.call)\\s*\\("

  nse_result <- tryCatch({
    if (!has_ns) {
      list(n = NA_integer_, frac = NA_real_)
    } else {
      # Collect all names that are exported and have an R/ definition.
      r_exported <- r_fn_names[vapply(r_fn_names, fn_is_exported, logical(1L))]
      # Also honour explicit exports even if not in R/ (no body -> no NSE count).
      all_exported_names <- unique(c(explicit, r_exported))

      nse_count <- 0L
      for (fn in all_exported_names) {
        loc <- fn_lookup[[fn]]
        if (is.null(loc)) next          # no R/ definition; cannot check body
        lns  <- ctx$lines(loc$file)
        body <- extract_body(lns, loc$line)
        if (grepl(nse_pat, body, perl = TRUE)) {
          nse_count <- nse_count + 1L
        }
      }

      n_exp <- if (is.na(n_exports)) NA_integer_ else n_exports
      frac  <- if (!is.na(n_exp) && n_exp > 0L) nse_count / n_exp else NA_real_
      list(n = nse_count, frac = frac)
    }
  }, error = function(e) list(n = NA_integer_, frac = NA_real_))

  # ── triple_colon ─────────────────────────────────────────────────────────────

  triple_result <- tryCatch({
    if (length(r_files) == 0L) {
      list(count = 0L, pkgs = 0L)
    } else {
      all_content <- paste(vapply(r_files, ctx$read, character(1L)), collapse = "\n")
      tc_pat <- "([A-Za-z.][A-Za-z0-9.]*):::([A-Za-z.][A-Za-z0-9._]*)"
      m <- gregexpr(tc_pat, all_content, perl = TRUE)
      hits <- regmatches(all_content, m)[[1L]]
      if (length(hits) == 0L) {
        list(count = 0L, pkgs = 0L)
      } else {
        pkg_re    <- "^([A-Za-z.][A-Za-z0-9.]*):::.*$"
        pkg_names <- sub(pkg_re, "\\1", hits, perl = TRUE)
        ext_pkgs  <- pkg_names[pkg_names != ctx$package]
        list(count = length(hits), pkgs = length(unique(ext_pkgs)))
      }
    }
  }, error = function(e) list(count = NA_integer_, pkgs = NA_integer_))

  # ── result ───────────────────────────────────────────────────────────────────

  list(
    n_exports          = n_exports,
    n_internal         = n_internal,
    nse_surface_n      = nse_result$n,
    nse_surface_frac   = nse_result$frac,
    triple_colon_count = triple_result$count,
    triple_colon_pkgs  = triple_result$pkgs
  )
}
