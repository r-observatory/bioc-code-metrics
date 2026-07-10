# scripts/render_notes.R: render release notes for the code and data DBs.
#
# Load order: config.R -> export.R -> render_notes.R
# This file does NOT auto-source its dependencies; the caller controls source order.

#' Render out/release-notes-code.md and out/release-notes-data.md.
#'
#' Reads code-manifest.json and data-manifest.json from out_dir, along with
#' the accumulated changed-packages.txt and the seed-packages.txt snapshot
#' (the prior release's package set, written by the workflow before the
#' shard loop), opens the code and dataset databases read-only, and builds
#' one rich notes body via build_release_notes(): a plain-language headline,
#' a table of this release's changed packages with their real metrics, and
#' a compact catalog summary. The same body is written to both notes files,
#' since both the code and dataset releases describe the same run.
#'
#' prev_code_tag/prev_data_tag/title_prefix are accepted for call-site
#' compatibility (the workflow still passes them) but no longer affect the
#' rendered content: the new format has no delta-from-previous section and
#' no top-level title heading (the GitHub release title already carries it).
#'
#' @param out_dir Directory holding the manifests, changed-packages.txt,
#'   seed-packages.txt, and the two databases; also where the two
#'   release-notes-*.md files are written.
#' @param prev_code_tag Unused; retained for signature compatibility.
#' @param prev_data_tag Unused; retained for signature compatibility.
#' @param title_prefix Unused; retained for signature compatibility.
#' @return Invisibly NULL.
render_notes <- function(out_dir, prev_code_tag = NULL, prev_data_tag = NULL,
                         title_prefix = "Bioconductor") {
  if (identical(prev_code_tag, "")) prev_code_tag <- NULL
  if (identical(prev_data_tag, "")) prev_data_tag <- NULL

  changed <- read_changed_packages(file.path(out_dir, "changed-packages.txt"))
  seed    <- read_changed_packages(file.path(out_dir, "seed-packages.txt"))

  code_manifest <- jsonlite::fromJSON(file.path(out_dir, "code-manifest.json"))
  data_manifest <- jsonlite::fromJSON(file.path(out_dir, "data-manifest.json"))

  code_path <- file.path(out_dir, DB_FILENAME)
  data_path <- file.path(out_dir, DATA_DB_FILENAME)

  code_con <- NULL
  data_con <- NULL
  if (file.exists(code_path)) {
    code_con <- DBI::dbConnect(RSQLite::SQLite(), code_path, flags = RSQLite::SQLITE_RO)
  }
  if (file.exists(data_path)) {
    data_con <- DBI::dbConnect(RSQLite::SQLite(), data_path, flags = RSQLite::SQLITE_RO)
  }
  on.exit({
    if (!is.null(code_con)) DBI::dbDisconnect(code_con)
    if (!is.null(data_con)) DBI::dbDisconnect(data_con)
  }, add = TRUE)

  notes <- build_release_notes(code_manifest, data_manifest, changed, seed,
                               code_con, data_con)

  writeLines(notes, file.path(out_dir, "release-notes-code.md"))
  writeLines(notes, file.path(out_dir, "release-notes-data.md"))
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
