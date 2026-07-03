# scripts/metrics/tests.R: test quality and CI metrics.
# Dependency: config.R, context.R must be sourced first.

# ---- internal helpers --------------------------------------------------------

# Escape a string for literal use inside a regex pattern.
.regex_escape <- function(x) {
  gsub("([.?*+^$\\[\\]{}()|\\\\])", "\\\\\\1", x, perl = TRUE)
}

# Extract unique values from YAML inline-array syntax for a given key.
# Matches lines of the form:  <whitespace>key: [val1, val2, 'val3']
# `key` may contain regex metacharacters (e.g. "r(?:-version)?").
# Returns a character vector of trimmed, unquoted values.
.yaml_inline_array <- function(content, key) {
  if (!nzchar(content)) return(character(0L))
  pat <- paste0("(?m)^\\s+", key, ":\\s*\\[([^\\]]+)\\]")
  matches <- regmatches(content, gregexpr(pat, content, perl = TRUE))[[1L]]
  if (length(matches) == 0L) return(character(0L))
  vals <- character(0L)
  inner_pat <- paste0(".*", key, ":\\s*\\[([^\\]]+)\\].*")
  for (m in matches) {
    inner <- sub(inner_pat, "\\1", m, perl = TRUE)
    parts <- trimws(strsplit(inner, ",", fixed = TRUE)[[1L]])
    parts <- gsub("[\"'`]", "", parts)
    vals  <- c(vals, parts[nzchar(parts)])
  }
  unique(vals)
}

# Count distinct (os, r-version) combinations across GitHub Actions workflow
# files.  Returns an integer >= 0.
# Strategy:
#   1. Cross-product of inline os: [...] and r: [...] / r-version: [...] arrays.
#   2. Inline object pairs: {os: ..., r: ...} (include-style matrices).
#   The per-file breadth is max(cross_product, include_count); the overall
#   result is the maximum across all workflow files.
.gha_matrix_breadth <- function(ctx, ci_yml_files) {
  if (length(ci_yml_files) == 0L) return(0L)

  max_breadth <- 0L

  for (f in ci_yml_files) {
    content <- ctx$read(f)
    if (!nzchar(content)) next

    # -- Method 1: cross-product of inline arrays -----------------------------
    os_vals <- .yaml_inline_array(content, "os")
    r_vals  <- .yaml_inline_array(content, "r(?:-version)?")

    cross_n <- if (length(os_vals) > 0L && length(r_vals) > 0L) {
      as.integer(length(os_vals) * length(r_vals))
    } else {
      as.integer(max(length(os_vals), length(r_vals)))
    }

    # -- Method 2: include-style object pairs ---------------------------------
    # Match inline YAML objects containing an os: key, e.g.:
    #   - {os: ubuntu-latest, r: 'release'}
    pair_matches <- regmatches(
      content,
      gregexpr("\\{[^}\\n]*\\bos:\\s*[^}\\n]+\\}", content, perl = TRUE)
    )[[1L]]

    include_combos <- character(0L)
    for (pair in pair_matches) {
      # Extract os value
      os_m <- regmatches(
        pair,
        regexpr("\\bos:\\s*['\"]?([^,'\"{}\\s]+)", pair, perl = TRUE)
      )
      if (length(os_m) == 0L || !nzchar(os_m)) next
      os_val <- gsub("^.*\\bos:\\s*['\"]?", "", os_m, perl = TRUE)

      # Extract r / r-version value (optional; fall back to "any")
      r_m <- regmatches(
        pair,
        regexpr("\\br(?:-version)?:\\s*['\"]?([^,'\"{}\\s]+)", pair, perl = TRUE)
      )
      if (length(r_m) == 0L || !nzchar(r_m)) {
        include_combos <- c(include_combos, paste0(os_val, ":any"))
      } else {
        r_val <- gsub("^.*\\br(?:-version)?:\\s*['\"]?", "", r_m, perl = TRUE)
        include_combos <- c(include_combos, paste0(os_val, ":", r_val))
      }
    }

    include_n    <- as.integer(length(unique(include_combos)))
    file_breadth <- max(cross_n, include_n)
    max_breadth  <- max(max_breadth, file_breadth)
  }

  as.integer(max_breadth)
}

# ---- main metric function ----------------------------------------------------

#' Compute test-quality and CI metrics for a package version.
#'
#' @param ctx  A context environment as returned by build_context().
#' @return Named list of scalars (numeric/integer/logical/character/NA).
#'   has_tests                logical   any file under tests/ is present
#'   test_to_code_ratio       numeric   LOC(tests/) / LOC(R/); NA if no R/ lines
#'   testthat_edition         integer   Config/testthat/edition from DESCRIPTION; NA if absent
#'   snapshot_test_count      integer   expect_snapshot/expect_snapshot_file calls in
#'                                      tests/ plus file count under tests/testthat/_snaps/
#'   test_isolation_libs      character JSON array of isolation packages detected in tests/
#'                                      (withr, mockr, httptest2, webfakes, local_mocked_bindings)
#'   exported_fn_test_linkage numeric   fraction of NAMESPACE exports that appear as a
#'                                      literal token in any tests/ file; NA if no exports
#'   stochastic_seed_discipline numeric fraction of test files using sample/runif/rnorm/rbinom
#'                                      that also call set.seed(); NA if none use RNG functions
#'   ci_present               logical   any recognised CI config file is present
#'   ci_type                  character JSON array of detected CI system names
#'   ci_matrix_breadth        integer   distinct (os, r-version) combos in GHA matrix; 0 if none
#'   ci_pr_gated              logical   any GHA workflow has a pull_request trigger
metrics_tests <- function(ctx) {

  # ---- helpers ---------------------------------------------------------------

  loc_for <- function(paths) {
    if (length(paths) == 0L) return(0L)
    total <- 0L
    for (p in paths) total <- total + length(ctx$lines(p))
    total
  }

  # ---- has_tests -------------------------------------------------------------

  test_files <- ctx$find("^tests/")
  has_tests  <- length(test_files) > 0L

  # ---- test_to_code_ratio ----------------------------------------------------

  r_files   <- ctx$find("^R/")
  loc_r     <- loc_for(r_files)
  loc_tests <- loc_for(test_files)

  test_to_code_ratio <- if (loc_r == 0L) NA_real_ else loc_tests / loc_r

  # ---- testthat_edition ------------------------------------------------------

  testthat_edition <- tryCatch({
    ed_raw <- ctx$desc[["Config/testthat/edition"]]
    if (is.null(ed_raw) || !nzchar(trimws(ed_raw %||% ""))) {
      NA_integer_
    } else {
      val <- suppressWarnings(as.integer(trimws(ed_raw)))
      if (is.na(val)) NA_integer_ else val
    }
  }, error = function(e) NA_integer_)

  # ---- snapshot_test_count ---------------------------------------------------

  # Scan R/r test files only; non-R files under tests/ are skipped.
  test_r_files <- ctx$find("^tests/.*\\.[Rr]$")

  snap_calls <- 0L
  for (f in test_r_files) {
    content <- ctx$read(f)
    if (!nzchar(content)) next
    # expect_snapshot_file\s*\( --- must be counted BEFORE expect_snapshot to
    # verify they are distinct (they are: \s*\( cannot follow expect_snapshot when
    # the next char is "_").  Counted separately to be explicit.
    m_snap <- gregexpr("expect_snapshot\\s*\\(", content, perl = TRUE)[[1L]]
    m_file <- gregexpr("expect_snapshot_file\\s*\\(", content, perl = TRUE)[[1L]]
    n_snap <- if (m_snap[[1L]] != -1L) length(m_snap) else 0L
    n_file <- if (m_file[[1L]] != -1L) length(m_file) else 0L
    snap_calls <- snap_calls + n_snap + n_file
  }

  snap_files          <- ctx$find("^tests/testthat/_snaps/")
  snapshot_test_count <- snap_calls + length(snap_files)

  # ---- test_isolation_libs ---------------------------------------------------

  # Build a single search string from all test files (memoised reads).
  test_content <- if (length(test_files) > 0L) {
    paste(vapply(test_files, ctx$read, character(1L)), collapse = "\n")
  } else {
    ""
  }

  # Packages detected by pkg:: usage or library()/require() calls.
  isolation_pkgs <- c("withr", "mockr", "httptest2", "webfakes")
  detected_libs  <- character(0L)
  for (lib in isolation_pkgs) {
    pat <- paste0(
      "\\b", lib, "::|",
      "library\\(\\s*['\"]?", lib, "['\"]?\\s*\\)|",
      "require\\(\\s*['\"]?", lib, "['\"]?\\s*\\)"
    )
    if (isTRUE(grepl(pat, test_content, perl = TRUE))) {
      detected_libs <- c(detected_libs, lib)
    }
  }
  # local_mocked_bindings is a testthat function, detected by function call site.
  if (isTRUE(grepl("\\blocal_mocked_bindings\\s*\\(", test_content, perl = TRUE))) {
    detected_libs <- c(detected_libs, "local_mocked_bindings")
  }

  test_isolation_libs <- as.character(
    jsonlite::toJSON(detected_libs, auto_unbox = FALSE)
  )

  # ---- exported_fn_test_linkage ----------------------------------------------

  exports      <- ctx$namespace$exports
  real_exports <- exports[!grepl("^pattern:", exports)]

  exported_fn_test_linkage <- if (length(real_exports) == 0L) {
    NA_real_
  } else if (!nzchar(test_content)) {
    0
  } else {
    seen <- vapply(real_exports, function(fn) {
      pat <- paste0("\\b", .regex_escape(fn), "\\b")
      isTRUE(grepl(pat, test_content, perl = TRUE))
    }, logical(1L))
    sum(seen) / length(real_exports)
  }

  # ---- stochastic_seed_discipline --------------------------------------------

  stochastic_pat <- "\\b(?:sample|runif|rnorm|rbinom)\\s*\\("
  seed_pat       <- "\\bset\\.seed\\s*\\("

  stochastic_files <- test_r_files[vapply(test_r_files, function(f) {
    isTRUE(grepl(stochastic_pat, ctx$read(f), perl = TRUE))
  }, logical(1L))]

  stochastic_seed_discipline <- if (length(stochastic_files) == 0L) {
    NA_real_
  } else {
    seeded <- vapply(stochastic_files, function(f) {
      isTRUE(grepl(seed_pat, ctx$read(f), perl = TRUE))
    }, logical(1L))
    sum(seeded) / length(stochastic_files)
  }

  # ---- CI detection ----------------------------------------------------------

  ci_yml_files <- ctx$find("^\\.github/workflows/.*\\.ya?ml$")
  has_travis   <- ctx$exists(".travis.yml")
  has_appveyor <- ctx$exists("appveyor.yml")
  has_circleci <- length(ctx$find("^\\.circleci/")) > 0L

  ci_present <- length(ci_yml_files) > 0L || has_travis || has_appveyor || has_circleci

  # ---- ci_type ---------------------------------------------------------------

  ci_systems <- character(0L)
  if (length(ci_yml_files) > 0L) ci_systems <- c(ci_systems, "github-actions")
  if (has_travis)                 ci_systems <- c(ci_systems, "travis")
  if (has_appveyor)               ci_systems <- c(ci_systems, "appveyor")
  if (has_circleci)               ci_systems <- c(ci_systems, "circleci")

  ci_type <- as.character(jsonlite::toJSON(ci_systems, auto_unbox = FALSE))

  # ---- ci_matrix_breadth -----------------------------------------------------

  ci_matrix_breadth <- .gha_matrix_breadth(ctx, ci_yml_files)

  # ---- ci_pr_gated -----------------------------------------------------------

  ci_pr_gated <- FALSE
  for (f in ci_yml_files) {
    if (isTRUE(grepl("pull_request", ctx$read(f), fixed = TRUE))) {
      ci_pr_gated <- TRUE
      break
    }
  }

  # ---- return ----------------------------------------------------------------

  list(
    has_tests                  = has_tests,
    test_to_code_ratio         = test_to_code_ratio,
    testthat_edition           = testthat_edition,
    snapshot_test_count        = snapshot_test_count,
    test_isolation_libs        = test_isolation_libs,
    exported_fn_test_linkage   = exported_fn_test_linkage,
    stochastic_seed_discipline = stochastic_seed_discipline,
    ci_present                 = ci_present,
    ci_type                    = ci_type,
    ci_matrix_breadth          = ci_matrix_breadth,
    ci_pr_gated                = ci_pr_gated
  )
}
