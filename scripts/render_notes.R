# scripts/render_notes.R: render release notes for the code and data DBs.
#
# Load order: config.R -> export.R -> render_notes.R
# This file does NOT auto-source its dependencies; the caller controls source order.

#' Render out/release-notes-code.md and out/release-notes-data.md.
#'
#' Reads code-manifest.json and data-manifest.json (and, when a prior dated
#' tag is supplied and the file is present, prev-code-manifest.json /
#' prev-data-manifest.json) from out_dir, along with the accumulated
#' changed-packages.txt, and writes the two rendered notes files back into
#' out_dir.
#'
#' @param out_dir Directory holding the manifests and changed-packages.txt;
#'   also where the two release-notes-*.md files are written.
#' @param prev_code_tag Dated tag of the prior code release (e.g.
#'   "code-2026-07-09"), or NULL/"" for the first dated release of the series.
#' @param prev_data_tag Same, for the data series.
#' @param title_prefix Prefix used to build each release's title heading.
#' @return Invisibly NULL.
render_notes <- function(out_dir, prev_code_tag = NULL, prev_data_tag = NULL,
                         title_prefix = "Bioconductor") {
  if (identical(prev_code_tag, "")) prev_code_tag <- NULL
  if (identical(prev_data_tag, "")) prev_data_tag <- NULL

  changed <- read_changed_packages(file.path(out_dir, "changed-packages.txt"))
  one <- function(series, prev_tag, title) {
    m <- jsonlite::fromJSON(file.path(out_dir, sprintf("%s-manifest.json", series)))
    prev_path <- file.path(out_dir, sprintf("prev-%s-manifest.json", series))
    prev <- if (!is.null(prev_tag) && file.exists(prev_path)) {
      p <- jsonlite::fromJSON(prev_path); p$dated_tag <- prev_tag; p
    } else NULL
    cl <- build_changelog(m, prev, changed, cap = 25L)
    notes <- render_release_notes(m, cl, title)
    writeLines(notes, file.path(out_dir, sprintf("release-notes-%s.md", series)))
  }
  one("code", prev_code_tag, sprintf("%s Code Metrics", title_prefix))
  one("data", prev_data_tag, sprintf("%s Data Metrics", title_prefix))
  invisible(NULL)
}

# ---------------------------------------------------------------------------
# CLI entry point
# ---------------------------------------------------------------------------
if (identical(sys.nframe(), 0L)) {
  # Standalone invocation (Rscript scripts/render_notes.R): locate this
  # script's directory so it works from any cwd, then source only the
  # dependencies render_notes() needs.
  .d <- {
    fa <- grep("^--file=", commandArgs(FALSE), value = TRUE)
    if (length(fa) >= 1L) dirname(sub("^--file=", "", fa[1L])) else "scripts"
  }
  source(file.path(.d, "config.R"))
  source(file.path(.d, "export.R"))
  args <- commandArgs(trailingOnly = TRUE)
  out_dir <- if (length(args) >= 1L) args[[1L]] else "out"
  render_notes(out_dir,
               prev_code_tag = Sys.getenv("PREV_CODE_TAG", ""),
               prev_data_tag = Sys.getenv("PREV_DATA_TAG", ""),
               title_prefix  = Sys.getenv("NOTES_TITLE_PREFIX", "Bioconductor"))
  message("Notes rendered.")
}
