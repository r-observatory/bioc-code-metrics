# scripts/metrics/structure.R: structural code metrics.
# Dependency: config.R, context.R must be sourced first.

#' Compute structural metrics for a package version.
#'
#' File classification (by path prefix/pattern):
#'   R/            -> R source
#'   src/*.{c,cc,cpp,h,hpp,f,f90,f95}  -> compiled source
#'   tests/        -> test files
#'   man/          -> documentation (Rd files, etc.)
#'   vignettes/    -> vignettes
#'
#' Non-code files (images, binaries, MD5, data files) are included in n_files
#' but excluded from all LOC counts.
#'
#' @param ctx  A context environment as returned by build_context().
#' @return Named list of scalars:
#'   n_files        integer  total file count
#'   loc_total      integer  sum of LOC across all classified code categories
#'   loc_r          integer  LOC in R/
#'   loc_src        integer  LOC in src/ compiled files
#'   loc_tests      integer  LOC in tests/
#'   loc_docs       integer  LOC in man/
#'   loc_vignettes  integer  LOC in vignettes/
#'   compiled_share numeric  loc_src / loc_total (0 when loc_total == 0)
#'   has_src        logical  any compiled source files present
#'   lang_breakdown character JSON object mapping file extension -> line count
metrics_structure <- function(ctx) {
  files <- ctx$files

  # --- classification masks ---
  is_r   <- grepl("^R/",   files)
  is_src <- grepl(
    "^src/[^/].*\\.(c|cc|cpp|cxx|h|hpp|hxx|f|f90|f95)$",
    files, ignore.case = TRUE, perl = TRUE
  )
  is_tests     <- grepl("^tests/",     files)
  is_docs      <- grepl("^man/",       files)
  is_vignettes <- grepl("^vignettes/", files)

  # Non-code / binary extensions excluded from LOC
  noncode_pat <- paste0(
    "\\.(rda|rdata|pdf|png|jpg|jpeg|gif|bmp|svg|ico|",
    "woff|woff2|eot|ttf|otf|",
    "gz|zip|tar|bz2|xz|7z|",
    "dll|so|dylib|o|a|lib|pyd|class|jar|pyc|",
    "xlsx|xls|docx|doc|pptx|ppt|",
    "mp3|mp4|ogg|wav|avi|mov)$"
  )
  is_noncode <- grepl(noncode_pat, files, ignore.case = TRUE, perl = TRUE) |
    grepl("(^|/)MD5$", files)

  # Count LOC for a subset of files (skipping non-code)
  count_loc <- function(mask) {
    fs <- files[mask & !is_noncode]
    if (length(fs) == 0L) return(0L)
    total <- 0L
    for (f in fs) {
      ln <- ctx$lines(f)
      total <- total + length(ln)
    }
    total
  }

  loc_r         <- count_loc(is_r)
  loc_src       <- count_loc(is_src)
  loc_tests     <- count_loc(is_tests)
  loc_docs      <- count_loc(is_docs)
  loc_vignettes <- count_loc(is_vignettes)
  loc_total     <- loc_r + loc_src + loc_tests + loc_docs + loc_vignettes

  compiled_share <- if (loc_total > 0L) loc_src / loc_total else 0
  has_src        <- any(is_src)

  # lang_breakdown: extension -> LOC across all files (code files only)
  is_code <- (is_r | is_src | is_tests | is_docs | is_vignettes) & !is_noncode
  code_files <- files[is_code]

  lang_breakdown <- if (length(code_files) > 0L) {
    exts <- tools::file_ext(code_files)
    exts[!nzchar(exts)] <- "(none)"
    loc_per_file <- vapply(code_files,
                           function(f) length(ctx$lines(f)),
                           integer(1L))
    by_ext <- tapply(loc_per_file, exts, sum)
    as.character(jsonlite::toJSON(as.list(by_ext), auto_unbox = TRUE))
  } else {
    "{}"
  }

  list(
    n_files        = length(files),
    loc_total      = loc_total,
    loc_r          = loc_r,
    loc_src        = loc_src,
    loc_tests      = loc_tests,
    loc_docs       = loc_docs,
    loc_vignettes  = loc_vignettes,
    compiled_share = compiled_share,
    has_src        = has_src,
    lang_breakdown = lang_breakdown
  )
}
