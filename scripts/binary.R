# Bridge to the rpkg-analyzer binary (r-observatory/rpkg-analyzer).
#
# The binary is a pure function of one extracted package source directory: it
# emits newline-delimited JSON, the first line being the per-version summary.
# It reproduces the R metric groups' output and adds many static metrics, so it
# replaces the build_context + analyze_version computation for a single version.
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

#' Run the analyzer over an extracted package directory.
#'
#' @param dir Path to the extracted package source (a DESCRIPTION at its root).
#' @return A flat named list of metrics for the version, with nested values
#'   (maps and arrays) serialised to JSON strings to match how the R metric
#'   groups store fields such as lang_breakdown. NULL if the binary is
#'   unavailable or does not produce a summary record.
analyze_with_binary <- function(dir) {
  bin <- rpkg_analyzer_bin()
  if (!nzchar(bin)) return(NULL)

  out <- tryCatch(
    system2(bin, shQuote(dir), stdout = TRUE, stderr = FALSE),
    error   = function(e) NULL,
    warning = function(w) NULL
  )
  if (is.null(out) || length(out) == 0L) return(NULL)

  # The summary record is the first line; guard in case of stray output.
  summ <- NULL
  for (line in out) {
    parsed <- tryCatch(
      jsonlite::fromJSON(line, simplifyVector = FALSE),
      error = function(e) NULL
    )
    if (!is.null(parsed) && identical(parsed[["rec"]], "summary")) {
      summ <- parsed
      break
    }
  }
  if (is.null(summ)) return(NULL)
  summ[["rec"]] <- NULL

  # Scalars pass through; arrays/objects become JSON strings.
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
