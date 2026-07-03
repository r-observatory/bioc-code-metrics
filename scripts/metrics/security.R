# scripts/metrics/security.R: security metrics.
# Dependency: config.R, context.R must be sourced first. jsonlite must be available.

#' Compute security metrics for a package version.
#'
#' @param ctx  A context environment as returned by build_context().
#' @return Named list of scalars:
#'   unsafe_pattern_score             integer  weighted count of unsafe R code patterns
#'   install_time_side_effect_surface character  JSON summary of install-time risk surface
#'   dep_constraint_coverage          numeric  fraction of Imports+Depends with explicit >= bound
#'   non_registry_remotes             character  JSON {count, schemes} of Remotes entries
#'   secret_pattern_count             integer  count of potential secret leaks in text files
#'   compiled_external_lib_exposure   character  JSON array of external library names
#'   bundled_third_party_code         character  JSON {detected, files} of vendored code
metrics_security <- function(ctx) {

  # Helper: count regex matches in a single string; returns 0L on no match or error.
  n_matches <- function(pattern, text, perl = TRUE) {
    if (!nzchar(text %||% "")) return(0L)
    m <- tryCatch(
      gregexpr(pattern, text, perl = perl)[[1L]],
      error = function(e) -1L
    )
    if (length(m) == 0L || is.na(m[1L]) || m[1L] == -1L) 0L else length(m)
  }

  # ---------------------------------------------------------------------------
  # 1. unsafe_pattern_score
  #    Weighted count of risky patterns in R/ files.
  #    Weights: eval(parse(text=))=3, system(paste())/system2+paste=2,
  #             Sys.setenv with non-literal=2, network in .onLoad/.onAttach=2.
  # ---------------------------------------------------------------------------
  unsafe_pattern_score <- tryCatch({
    r_files <- ctx$find("^R/.*\\.R$")
    score <- 0L
    for (f in r_files) {
      content <- ctx$read(f)
      if (!nzchar(content)) next

      # eval(parse(text=...)) - weight 3
      score <- score + 3L * n_matches(
        "eval\\s*\\(\\s*parse\\s*\\(\\s*text\\s*=", content
      )

      # system(paste(...)) - weight 2
      score <- score + 2L * n_matches(
        "system\\s*\\(\\s*paste\\s*\\(", content
      )

      # system2(..., paste(...)) on the same line - weight 2 per line
      score <- score + 2L * n_matches(
        "system2\\s*\\([^\n]*paste\\s*\\(", content
      )

      # Sys.setenv with a non-literal RHS - weight 2 per matching line.
      # Strip quoted string contents, then look for = followed by an identifier.
      for (ln in strsplit(content, "\n", fixed = TRUE)[[1L]]) {
        if (!grepl("Sys\\.setenv\\s*\\(", ln, perl = TRUE)) next
        stripped <- gsub('"[^"]*"', '""', gsub("'[^']*'", "''", ln))
        if (grepl("=\\s*[A-Za-z_.][A-Za-z0-9_.]*", stripped, perl = TRUE)) {
          score <- score + 2L
        }
      }

      # download.file / url() / curl in a file that defines .onLoad or .onAttach - weight 2
      if (grepl("\\.on(?:Load|Attach)\\s*<-\\s*function", content, perl = TRUE)) {
        score <- score + 2L * n_matches(
          "download\\.file\\s*\\(|\\burl\\s*\\(|\\bcurl\\s*\\(", content
        )
      }
    }
    score
  }, error = function(e) NA_integer_)

  # ---------------------------------------------------------------------------
  # 2. install_time_side_effect_surface
  #    JSON summary: which configure/cleanup scripts exist + their total LOC,
  #    and whether .onLoad/.onAttach perform file writes or network I/O.
  # ---------------------------------------------------------------------------
  install_time_side_effect_surface <- tryCatch({
    cfg_names   <- c("configure", "configure.win", "cleanup", "cleanup.win")
    cfg_present <- cfg_names[vapply(cfg_names, ctx$exists, logical(1L))]
    cfg_loc     <- sum(vapply(
      cfg_present,
      function(f) length(ctx$lines(f)),
      integer(1L)
    ))

    r_files           <- ctx$find("^R/.*\\.R$")
    onload_file_write <- FALSE
    onload_network    <- FALSE

    file_write_pat <- paste0(
      "writeLines?\\s*\\(|writeBin\\s*\\(|\\bcat\\s*\\(\\s*[^)]*,\\s*(?:file|con)\\s*=|",
      "(?<![A-Za-z0-9_.])file\\s*\\(|\\bsink\\s*\\(|",
      "write\\.csv\\s*\\(|write\\.table\\s*\\("
    )
    network_pat <- paste0(
      "download\\.file\\s*\\(|\\burl\\s*\\(|\\bcurl\\s*\\(|",
      "httr::|RCurl::|curl::"
    )

    for (f in r_files) {
      content <- ctx$read(f)
      if (!nzchar(content)) next
      if (!grepl("\\.on(?:Load|Attach)\\s*<-\\s*function", content, perl = TRUE)) next
      if (grepl(file_write_pat, content, perl = TRUE)) onload_file_write <- TRUE
      if (grepl(network_pat,    content, perl = TRUE)) onload_network    <- TRUE
    }

    as.character(jsonlite::toJSON(list(
      configure_files   = as.list(cfg_present),
      configure_loc     = cfg_loc,
      onLoad_file_write = onload_file_write,
      onLoad_network    = onload_network
    ), auto_unbox = TRUE))
  }, error = function(e) NA_character_)

  # ---------------------------------------------------------------------------
  # 3. dep_constraint_coverage
  #    Fraction of Imports+Depends entries (excluding R itself) that carry
  #    an explicit >= version bound.  NA when there are no qualifying entries.
  # ---------------------------------------------------------------------------
  dep_constraint_coverage <- tryCatch({
    parse_dep_entries <- function(raw) {
      if (is.null(raw) || !nzchar(trimws(raw))) return(character(0L))
      entries <- strsplit(raw, ",", fixed = TRUE)[[1L]]
      trimws(entries)
    }

    all_entries <- c(
      parse_dep_entries(ctx$desc$Imports),
      parse_dep_entries(ctx$desc$Depends)
    )
    all_entries <- all_entries[nzchar(all_entries)]

    # Extract package names and exclude "R"
    pkg_names   <- trimws(sub("\\s*\\(.*", "", all_entries))
    keep        <- nzchar(pkg_names) & pkg_names != "R"
    all_entries <- all_entries[keep]

    if (length(all_entries) == 0L) {
      NA_real_
    } else {
      sum(grepl(">=", all_entries, fixed = TRUE)) / length(all_entries)
    }
  }, error = function(e) NA_real_)

  # ---------------------------------------------------------------------------
  # 4. non_registry_remotes
  #    JSON {count, schemes}: count of Remotes entries and the scheme of each
  #    (github/gitlab/bitbucket/git/url/local/...; bare "user/repo" = "github").
  # ---------------------------------------------------------------------------
  non_registry_remotes <- tryCatch({
    raw <- ctx$desc$Remotes %||% ""
    if (!nzchar(trimws(raw))) {
      as.character(jsonlite::toJSON(
        list(count = 0L, schemes = list()),
        auto_unbox = TRUE
      ))
    } else {
      entries <- strsplit(raw, ",", fixed = TRUE)[[1L]]
      entries <- trimws(entries)
      entries <- entries[nzchar(entries)]

      schemes <- vapply(entries, function(e) {
        if (grepl("::", e, fixed = TRUE)) {
          sub("::.*", "", trimws(e))
        } else {
          "github"
        }
      }, character(1L))

      as.character(jsonlite::toJSON(list(
        count   = length(entries),
        schemes = as.list(unname(schemes))
      ), auto_unbox = TRUE))
    }
  }, error = function(e) NA_character_)

  # ---------------------------------------------------------------------------
  # 5. secret_pattern_count
  #    Count of potential secret-leaking patterns across all text files.
  #    Patterns: AWS AKIA keys, GitHub tokens (ghp_/gho_/ghu_/ghs_/github_pat_),
  #    generic api_key/api_secret/secret_key/access_token assignments to long literals,
  #    and long base64-like strings assigned to names containing key/token/secret/password.
  #    Binary and non-text files are excluded.  Regex scanners are conservative
  #    (favour precision): these are signals, not verdicts.
  # ---------------------------------------------------------------------------
  secret_pattern_count <- tryCatch({
    nonbinary_pat <- paste0(
      "\\.(rda|rdata|rds|pdf|png|jpg|jpeg|gif|bmp|svg|ico|",
      "woff|woff2|eot|ttf|otf|",
      "gz|zip|tar|bz2|xz|7z|",
      "dll|so|dylib|o|a|lib|pyd|class|jar|pyc|",
      "xlsx|xls|docx|doc|pptx|ppt|",
      "mp3|mp4|ogg|wav|avi|mov|",
      # Genomic / bioinformatics data formats: large sequence or alignment files
      # that cannot contain credential secrets and are prohibitively slow to scan.
      "sam|bam|bai|cram|fasta|fa|fastq|fq|vcf|bcf|",
      "bed|wig|bedgraph|bigwig|bw|bigbed|bb)$"
    )
    text_files <- ctx$files[
      !grepl(nonbinary_pat, ctx$files, ignore.case = TRUE, perl = TRUE) &
      !grepl("(^|/)MD5$", ctx$files)
    ]

    # AWS IAM access key id: AKIA followed by exactly 16 uppercase alphanumerics
    akia_pat <- "AKIA[0-9A-Z]{16}"

    # GitHub tokens: ghp_/gho_/ghu_/ghs_ + 36+ alphanumeric chars; github_pat_ prefix
    gh_pat <- "gh[pous]_[A-Za-z0-9_]{36,}|github_pat_[A-Za-z0-9_]{36,}"

    # Generic: api_key / api_secret / secret_key / access_token assigned to a long literal
    api_key_pat <- paste0(
      "(?i)(api[_-]?key|api[_-]?secret|secret[_-]?key|access[_-]?token)",
      "\\s*[=:]\\s*[\"'][A-Za-z0-9+/=_\\-]{16,}[\"']"
    )

    # High-entropy base64-like string assigned to a suspicious variable name
    b64_pat <- paste0(
      "(?i)(password|passwd|api_?key|auth_?token|secret)",
      "\\s*=\\s*[\"'][A-Za-z0-9+/]{40,}={0,2}[\"']"
    )

    count <- 0L
    for (f in text_files) {
      content <- ctx$read(f)
      if (!nzchar(content)) next
      # Skip files larger than 1 MB: scanning multi-megabyte blobs (e.g. TCGA
      # expression tables, EPS graphics) with PCRE is non-interruptible at the
      # C level and cannot contain credential patterns in practice.  Known
      # genomic formats are already excluded above via nonbinary_pat; this guard
      # catches any remaining large files with unrecognised extensions.
      if (nchar(content, type = "bytes") > 1e6L) next
      count <- count + n_matches(akia_pat,    content, perl = TRUE)
      count <- count + n_matches(gh_pat,      content, perl = TRUE)
      count <- count + n_matches(api_key_pat, content, perl = TRUE)
      count <- count + n_matches(b64_pat,     content, perl = TRUE)
    }
    count
  }, error = function(e) NA_integer_)

  # ---------------------------------------------------------------------------
  # 6. compiled_external_lib_exposure
  #    JSON array of unique external library names referenced via -l flags
  #    in src/Makevars*, configure, configure.ac, or via AC_CHECK_LIB().
  # ---------------------------------------------------------------------------
  compiled_external_lib_exposure <- tryCatch({
    src_cfg_files <- c(
      "src/Makevars", "src/Makevars.win", "src/Makevars.in", "src/Makevars.ucrt",
      "configure", "configure.ac", "configure.in"
    )

    all_libs <- character(0L)

    for (f in src_cfg_files) {
      if (!ctx$exists(f)) next
      content <- ctx$read(f)
      if (!nzchar(content)) next

      # -l<libname> flags (lowercase -l only; -L is a search path, not a library)
      flag_matches <- regmatches(
        content,
        gregexpr("-l[A-Za-z][A-Za-z0-9_-]*", content, perl = TRUE)
      )[[1L]]
      if (length(flag_matches) > 0L) {
        all_libs <- c(all_libs, sub("^-l", "", flag_matches))
      }

      # AC_CHECK_LIB(libname, ...) in autoconf scripts
      ac_full <- regmatches(
        content,
        gregexpr("AC_CHECK_LIB\\s*\\(\\s*[A-Za-z][A-Za-z0-9_-]*", content, perl = TRUE)
      )[[1L]]
      if (length(ac_full) > 0L) {
        lib_names <- trimws(sub("AC_CHECK_LIB\\s*\\(\\s*", "", ac_full, perl = TRUE))
        all_libs  <- c(all_libs, lib_names)
      }
    }

    all_libs <- unique(all_libs)
    as.character(jsonlite::toJSON(all_libs, auto_unbox = TRUE))
  }, error = function(e) NA_character_)

  # ---------------------------------------------------------------------------
  # 7. bundled_third_party_code
  #    JSON {detected, files}: detects known vendored filenames under src/ or inst/,
  #    or LICENSE/COPYING files inside src/ subdirectories (indicating vendored code).
  # ---------------------------------------------------------------------------
  bundled_third_party_code <- tryCatch({
    known_vendored <- c(
      "sqlite3.c", "sqlite3.h",
      "json.hpp",
      "miniz.c", "miniz.h",
      "stb_image.h", "stb_image_write.h",
      "nanosvg.h", "nanosvgrast.h",
      "xxhash.h", "xxhash.c",
      "tinyxml2.cpp", "tinyxml2.h",
      "pugixml.cpp", "pugixml.hpp"
    )

    found <- character(0L)

    for (f in ctx$files) {
      if (!grepl("^(src|inst)/", f, perl = TRUE)) next
      if (basename(f) %in% known_vendored) found <- c(found, f)
    }

    # LICENSE or COPYING inside a subdirectory of src/ (not src/LICENSE itself)
    for (f in ctx$files) {
      if (grepl("^src/.+/(LICENSE|COPYING)(\\.[A-Za-z]+)?$", f, perl = TRUE)) {
        found <- c(found, f)
      }
    }

    found    <- unique(found)
    detected <- length(found) > 0L

    as.character(jsonlite::toJSON(list(
      detected = detected,
      files    = as.list(found)
    ), auto_unbox = TRUE))
  }, error = function(e) NA_character_)

  # ---------------------------------------------------------------------------
  list(
    unsafe_pattern_score             = unsafe_pattern_score,
    install_time_side_effect_surface = install_time_side_effect_surface,
    dep_constraint_coverage          = dep_constraint_coverage,
    non_registry_remotes             = non_registry_remotes,
    secret_pattern_count             = secret_pattern_count,
    compiled_external_lib_exposure   = compiled_external_lib_exposure,
    bundled_third_party_code         = bundled_third_party_code
  )
}
