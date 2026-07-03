# scripts/metrics/portability.R: portability metrics.
# Dependency: config.R, context.R must be sourced first.

#' Compute portability metrics for a package version.
#'
#' @param ctx  A context environment as returned by build_context().
#' @return Named list of scalars:
#'   system_requirements_count       integer   distinct OS-level libs in SystemRequirements;
#'                                             NA when field absent or empty
#'   cxx_standard_required           character C++ standard from CXX_STD in src/Makevars*
#'                                             (e.g. "C++11", "C++17"); NA if none
#'   nonportable_compiler_flags      integer   count of unique non-portable flags found in
#'                                             src/Makevars* (-march=native, -O3,
#'                                             -funroll-loops, -ffast-math, hardcoded -I/-L)
#'   nonportable_compiler_flags_json character JSON array of the flag strings found
#'   min_r_version                   character minimum R version from Depends (x.y.z or x.y);
#'                                             NA when not specified
#'   has_vignettes                   logical   TRUE when vignettes/ contains a .Rmd or .Rnw
#'   vignette_dynamic                logical   TRUE when vignette sources are not trivially all
#'                                             eval=FALSE; NA when no vignettes
metrics_portability <- function(ctx) {

  # -------------------------------------------------------------------------
  # system_requirements_count
  # -------------------------------------------------------------------------
  system_requirements_count <- tryCatch({
    sr <- ctx$desc[["SystemRequirements"]]
    if (is.null(sr) || !nzchar(trimws(sr %||% ""))) {
      NA_integer_
    } else {
      parts <- strsplit(sr, "[,]|\\band\\b", perl = TRUE)[[1L]]
      parts <- trimws(parts)
      parts <- parts[nzchar(parts)]
      if (length(parts) == 0L) NA_integer_ else length(unique(tolower(parts)))
    }
  }, error = function(e) NA_integer_)

  # -------------------------------------------------------------------------
  # cxx_standard_required + nonportable_compiler_flags (share one loop)
  # -------------------------------------------------------------------------
  bad_flag_pats <- c("-march=native", "-O3", "-funroll-loops", "-ffast-math")
  makevars_files <- ctx$find("^src/Makevars(\\.win)?$")

  cxx_standard_required <- NA_character_
  found_flags           <- character(0L)

  tryCatch({
    for (mf in makevars_files) {
      lns <- ctx$lines(mf)
      for (ln in lns) {
        # Skip comment lines
        if (grepl("^\\s*#", ln)) next

        # CXX_STD (take first occurrence across all Makevars files)
        if (is.na(cxx_standard_required)) {
          m <- regexpr("^\\s*CXX_STD\\s*=\\s*CXX(\\d+)", ln, perl = TRUE)
          if (m != -1L) {
            cs <- attr(m, "capture.start")[[1L]]
            cl <- attr(m, "capture.length")[[1L]]
            cxx_standard_required <- paste0("C++", substring(ln, cs, cs + cl - 1L))
          }
        }

        # Non-portable named flags
        for (pat in bad_flag_pats) {
          if (grepl(pat, ln, fixed = TRUE)) found_flags <- c(found_flags, pat)
        }

        # Hardcoded absolute -I/-L paths (e.g. -I/usr/local but not -I$(VAR))
        abs_paths <- regmatches(
          ln,
          gregexpr("-[IL]/[^[:space:]]+", ln, perl = TRUE)
        )[[1L]]
        found_flags <- c(found_flags, abs_paths)
      }
    }
    found_flags <- unique(found_flags)
  }, error = function(e) {
    found_flags <<- unique(found_flags)
  })

  nonportable_compiler_flags      <- length(found_flags)
  nonportable_compiler_flags_json <- as.character(
    jsonlite::toJSON(found_flags, auto_unbox = FALSE)
  )

  # -------------------------------------------------------------------------
  # min_r_version
  # -------------------------------------------------------------------------
  min_r_version <- tryCatch({
    dep <- ctx$desc[["Depends"]]
    if (is.null(dep) || !nzchar(trimws(dep %||% ""))) {
      NA_character_
    } else {
      m <- regexpr(
        "\\bR\\s*\\(\\s*>=\\s*([0-9]+\\.[0-9]+(?:\\.[0-9]+)?)\\s*\\)",
        dep, perl = TRUE
      )
      if (m == -1L) {
        NA_character_
      } else {
        cs <- attr(m, "capture.start")[[1L]]
        cl <- attr(m, "capture.length")[[1L]]
        substring(dep, cs, cs + cl - 1L)
      }
    }
  }, error = function(e) NA_character_)

  # -------------------------------------------------------------------------
  # has_vignettes
  # -------------------------------------------------------------------------
  vig_files     <- ctx$find("^vignettes/.*\\.[Rr](md|nw)$")
  has_vignettes <- length(vig_files) > 0L

  # -------------------------------------------------------------------------
  # vignette_dynamic
  # -------------------------------------------------------------------------
  vignette_dynamic <- tryCatch({
    if (!has_vignettes) {
      NA
    } else {
      found_any_chunk    <- FALSE
      found_active_chunk <- FALSE

      for (vf in vig_files) {
        content <- ctx$read(vf)
        if (!nzchar(content)) next

        if (grepl("\\.[Rr]md$", vf)) {
          # knitr Rmd: chunk openers are ```{r ...}
          headers <- regmatches(
            content,
            gregexpr("```\\{r[^}]*\\}", content, perl = TRUE)
          )[[1L]]
        } else {
          # Sweave / knitr Rnw: chunk openers are <<...>>=
          headers <- regmatches(
            content,
            gregexpr("<<[^>]*>>=", content, perl = TRUE)
          )[[1L]]
        }

        if (length(headers) == 0L) next
        found_any_chunk <- TRUE

        for (h in headers) {
          if (!grepl("eval\\s*=\\s*(FALSE|F)(?=[,}[:space:]]|$)", h, perl = TRUE)) {
            found_active_chunk <- TRUE
            break
          }
        }
        if (found_active_chunk) break
      }

      # When no parseable chunks found, do not declare the vignette static
      if (!found_any_chunk) TRUE else found_active_chunk
    }
  }, error = function(e) NA)

  list(
    system_requirements_count       = system_requirements_count,
    cxx_standard_required           = cxx_standard_required,
    nonportable_compiler_flags      = nonportable_compiler_flags,
    nonportable_compiler_flags_json = nonportable_compiler_flags_json,
    min_r_version                   = min_r_version,
    has_vignettes                   = has_vignettes,
    vignette_dynamic                = vignette_dynamic
  )
}
