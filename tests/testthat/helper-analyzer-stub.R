# tests/testthat/helper-analyzer-stub.R: a stand-in for the rpkg-analyzer
# binary, shared by every test that needs one installed.
#
# Three files grew their own copy of this, two of them byte for byte, and each
# carried an argument nothing read. One copy here instead, with the argument
# doing the job its name promised.

#' Write a stub analyzer binary and return its path.
#'
#' The stub answers `--version` for `version`, which is what
#' rpkg_analyzer_version() reads and what the re-scan queue compares rows
#' against. On a package it reads only the versions named in `reads`, emitting
#' one summary record and one dataset record for each; on any other version it
#' exits 1.
#'
#' With `reads` empty it fails on every package, which is what "installed, and
#' cannot read this one" looks like in production: the pure-R fallback writes
#' the row, so it carries neither n_fns_r nor a dataset, and both backfill
#' queues hand the package straight back.
#'
#' @param dir     Directory to write the stub into.
#' @param version The build the stub answers `--version` with.
#' @param reads   Package versions the stub will read, matched against the
#'   Version field of the DESCRIPTION in the directory it is pointed at.
#' @param input_kind When set, each summary names it, as a 0.5.0 build does.
#' @param stats When TRUE, each read appends a statistics line to the file
#'   RPKG_ANALYZER_STATS names, as a 0.5.1 build does.
#' @param detail When TRUE, each read also emits one function record, one call
#'   edge and a DESCRIPTION record carrying RoxygenNote.
#' @param content_by_version When TRUE, the dataset's content_fp names the
#'   version read, so each version links to a content of its own.
.stub_analyzer_bin <- function(dir, version, reads = character(0L),
                              input_kind = NULL, stats = FALSE, detail = FALSE,
                              content_by_version = FALSE) {
  stub <- file.path(dir, "stub-analyzer.sh")
  content <- if (isTRUE(content_by_version)) "cf-$v" else "cf"
  kind <- if (is.null(input_kind)) "" else
    sprintf(',\\"input_kind\\":\\"%s\\"', input_kind)
  stats_line <- if (isTRUE(stats)) {
    sprintf(paste0("  if [ -n \"${RPKG_ANALYZER_STATS:-}\" ]; then ",
                   "echo '{\"build\":\"%s\",\"ms\":1}' >> \"$RPKG_ANALYZER_STATS\"; fi"),
            version)
  }
  detail_lines <- if (isTRUE(detail)) {
    c(paste0('  echo "{\\"rec\\":\\"function\\",\\"lang\\":\\"R\\",\\"name\\":\\"f\\",',
             '\\"exported\\":true,\\"file\\":\\"R/code.R\\",\\"line\\":1,\\"loc\\":1,',
             '\\"n_params\\":0,\\"cyclocomp\\":1}"'),
      '  echo "{\\"rec\\":\\"call_edge\\",\\"graph\\":\\"R\\",\\"from\\":\\"f\\",\\"to\\":\\"g\\"}"',
      '  echo "{\\"rec\\":\\"dcf\\",\\"RoxygenNote\\":\\"7.3.2\\"}"')
  }
  read_branch <- if (length(reads) > 0L) {
    c(
      'dir=$(echo "$1" | tr -d "\'")',
      'v=$(sed -n "s/^Version: *//p" "$dir/DESCRIPTION" | head -1)',
      sprintf('case "$v" in %s)', paste(reads, collapse = "|")),
      sprintf('  echo "{\\"rec\\":\\"summary\\",\\"loc_r\\":1,\\"n_fns_r\\":1%s}"', kind),
      paste0('  echo "{\\"rec\\":\\"dataset\\",\\"name\\":\\"d\\",',
             '\\"file\\":\\"data/d.rda\\",\\"class\\":\\"data.frame\\",',
             '\\"kind\\":\\"table\\",\\"confidence\\":\\"exact\\",',
             '\\"content_fp\\":\\"', content, '\\",\\"schema_fp\\":\\"sf\\"}"'),
      detail_lines,
      stats_line,
      "  exit 0",
      "  ;;",
      "esac")
  } else {
    character(0L)
  }
  writeLines(c(
    "#!/bin/sh",
    'if [ "$1" = "--version" ]; then',
    sprintf('  echo "rpkg-analyzer %s"', version),
    "  exit 0",
    "fi",
    read_branch,
    "exit 1"), stub)
  Sys.chmod(stub, mode = "0755")
  stub
}
