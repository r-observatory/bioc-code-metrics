# scripts/analyze.R: metric dispatcher + per-package analysis orchestration.
#
# Load order: config.R -> git.R -> context.R -> metrics/structure.R -> analyze.R
# This file does NOT auto-source its dependencies so the caller controls order.

#' Registry of metric group functions.
#' Each value is a function(ctx) -> named list of scalar metric values.
#' Add more groups here when new metric modules are implemented.
METRIC_GROUPS <- list(
  structure   = metrics_structure,
  functions   = metrics_functions,
  docs        = metrics_docs,
  tests       = metrics_tests,
  security    = metrics_security,
  health      = metrics_health,
  portability = metrics_portability,
  legal       = metrics_legal,
  meta        = metrics_meta
)

#' Compute all registered metrics for one package version.
#'
#' Iterates over METRIC_GROUPS.  A group that throws an error:
#'   - emits a warning naming the group, the package, and the error message
#'   - contributes a sentinel NA entry (.error.<group>) to the result
#'   - does NOT abort the call
#'
#' @param ctx  A context environment from build_context().
#' @return Named list of scalars (metric name -> value).  Failed groups add
#'   `.error.<group_name> = NA` entries so callers can detect them.
analyze_version <- function(ctx) {
  result <- list()
  for (nm in names(METRIC_GROUPS)) {
    fn <- METRIC_GROUPS[[nm]]
    group_out <- tryCatch(
      fn(ctx),
      error = function(e) {
        warning(sprintf(
          "metric group '%s' failed for %s %s: %s",
          nm,
          ctx$package %||% "?",
          ctx$version %||% "?",
          conditionMessage(e)
        ))
        NULL
      }
    )
    if (is.null(group_out)) {
      # Record failure with a sentinel; column names are not known without success
      result[[paste0(".error.", nm)]] <- NA
    } else {
      result <- c(result, group_out)
    }
  }
  result
}

# ---- Cross-version helpers -----------------------------------------------

#' Detect deprecation signals in R source files of a package version.
#'
#' Scans all R/*.R files for:
#'   .Deprecated("sym") / .Defunct("sym")  -- extracts the first quoted argument
#'   lifecycle::deprecate_warn/soft/stop   -- marks uses_lifecycle TRUE and
#'                                            extracts the "what" symbol
#'
#' This is NOT a per-version metric; it is called inside the analyze_package
#' loop and its results kept in a parallel series for add_cross_version_metrics.
#'
#' @param ctx A context environment from build_context().
#' @return Named list:
#'   $symbols      character vector of symbol names in deprecation calls
#'   $uses_lifecycle logical; TRUE when any lifecycle:: deprecation is found
deprecation_signals <- function(ctx) {
  r_files        <- ctx$find("^R/.*\\.[Rr]$")
  symbols        <- character(0L)
  uses_lifecycle <- FALSE

  for (f in r_files) {
    content <- ctx$read(f)
    if (!nzchar(content)) next

    # .Deprecated("sym") or .Defunct("sym"): capture first quoted argument
    dep_pattern <- '\\.(?:Deprecated|Defunct)\\s*\\(\\s*["\']([^"\']+)["\']'
    dep_matches <- regmatches(content,
                              gregexpr(dep_pattern, content, perl = TRUE))[[1L]]
    for (m in dep_matches) {
      qm <- regexpr('["\']([^"\']+)["\']', m, perl = TRUE)
      if (qm == -1L) next
      cs <- attr(qm, "capture.start")[[1L]]
      cl <- attr(qm, "capture.length")[[1L]]
      symbols <- c(symbols, substring(m, cs, cs + cl - 1L))
    }

    # lifecycle::deprecate_warn / _soft / _stop
    if (grepl("lifecycle::deprecate_(?:warn|soft|stop)", content, perl = TRUE)) {
      uses_lifecycle <- TRUE
      # Match calls with at least two arguments; capture up to and including
      # the second quoted argument (the "what" parameter)
      lc_pattern <- 'lifecycle::deprecate_(?:warn|soft|stop)\\s*\\([^,\n]+,\\s*["\'][^"\']+["\']'
      lc_matches <- regmatches(content,
                               gregexpr(lc_pattern, content, perl = TRUE))[[1L]]
      for (m in lc_matches) {
        all_q <- regmatches(m, gregexpr('["\'][^"\']+["\']', m, perl = TRUE))[[1L]]
        if (length(all_q) < 2L) next
        what <- substr(all_q[2L], 2L, nchar(all_q[2L]) - 1L)
        what <- sub("^[^:]+::", "", what, perl = TRUE)  # strip pkg:: prefix
        what <- sub("\\(\\)$",   "", what, perl = TRUE)  # strip trailing ()
        if (nzchar(what)) symbols <- c(symbols, what)
      }
    }
  }

  list(symbols = unique(symbols), uses_lifecycle = uses_lifecycle)
}


# Parse a version string into an integer triple (major, minor, patch).
# Separators accepted: '.', '-', '_'.  Fewer-than-three parts are padded with 0.
# Returns rep(NA_integer_, 3L) for NULL/NA/empty input.
.xv_parse_ver <- function(v) {
  if (is.null(v) || length(v) == 0L || is.na(v) || !nzchar(trimws(v))) {
    return(rep(NA_integer_, 3L))
  }
  parts <- strsplit(v, "[._-]", perl = TRUE)[[1L]]
  parts <- suppressWarnings(as.integer(parts))
  length(parts) <- 3L                              # pad shorter versions with NA
  ifelse(is.na(parts), 0L, parts)                  # treat missing component as 0
}


# Classify the bump type between two consecutive version strings.
# Returns one of "major", "minor", "patch", "other".
.xv_classify_bump <- function(prev, curr) {
  pv <- tryCatch(.xv_parse_ver(prev), error = function(e) rep(NA_integer_, 3L))
  cv <- tryCatch(.xv_parse_ver(curr), error = function(e) rep(NA_integer_, 3L))
  if (any(is.na(pv)) || any(is.na(cv))) return("other")
  if (cv[1L] > pv[1L]) return("major")
  if (cv[2L] > pv[2L]) return("minor")
  if (cv[3L] > pv[3L]) return("patch")
  "other"
}


# Parse a JSON array string into a character vector; returns character(0) on error/NA.
.xv_json_to_chr <- function(json_str) {
  if (is.null(json_str) || length(json_str) == 0L) return(character(0L))
  if (is.na(json_str)   || !nzchar(json_str))       return(character(0L))
  tryCatch(
    as.character(jsonlite::fromJSON(json_str, simplifyVector = TRUE)),
    error = function(e) character(0L)
  )
}


# Parse a JSON author array into lowercase "given family" identity strings.
.xv_author_identities <- function(json_str) {
  if (is.null(json_str) || length(json_str) == 0L) return(character(0L))
  if (is.na(json_str)   || !nzchar(json_str))       return(character(0L))
  parsed <- tryCatch(
    jsonlite::fromJSON(json_str, simplifyDataFrame = TRUE, simplifyVector = TRUE),
    error = function(e) NULL
  )
  if (is.null(parsed) || length(parsed) == 0L) return(character(0L))
  if (is.data.frame(parsed)) {
    g <- ifelse(is.na(parsed$given),  "", tolower(trimws(parsed$given)))
    f <- ifelse(is.na(parsed$family), "", tolower(trimws(parsed$family)))
    paste(g, f)
  } else if (is.list(parsed)) {
    vapply(parsed, function(p) {
      paste(tolower(trimws(p[["given"]]  %||% "")),
            tolower(trimws(p[["family"]] %||% "")))
    }, character(1L))
  } else {
    character(0L)
  }
}


#' Add cross-version (temporal) derived metrics to a package summary data.frame.
#'
#' Called once per package after all per-version metrics have been assembled
#' into summary_df and api_df by analyze_package().
#'
#' Per-version columns added to every row:
#'   bump_type           character "initial"/"major"/"minor"/"patch"/"other"
#'   exports_added_n     integer count of exports added vs. prior version
#'   exports_removed_n   integer count of exports removed vs. prior version
#'   is_breaking         logical exports_removed_n > 0
#'   bump_fidelity_ok    logical bump is at least as large as the change implies;
#'                       NA for the first version
#'   assessed_at         character Sys.Date() ISO string
#'   assessed_with       character version string: "bioc-code-metrics; R <version>"
#'
#' Package-level columns placed only on the LATEST version row (NA elsewhere):
#'   n_versions                          integer
#'   first_release_date                  character min(released)
#'   latest_release_date                 character max(released)
#'   release_cadence_days                numeric median inter-release days;
#'                                       NA when fewer than 2 versions
#'   dependency_drift                    integer total dep additions + removals
#'                                       across all consecutive-version transitions
#'   authors_added_later                 integer distinct author identities
#'                                       present in a later version but not the first
#'   cold_removal_rate                   numeric fraction of removed exports with
#'                                       no prior deprecation signal; NA if no removals
#'   deprecation_infrastructure_maturity integer 0/1/2 (0=none, 1=base, 2=lifecycle)
#'
#' @param summary_df       data.frame; one row per version, oldest first.
#'   Expected columns: version, released (or date), dep_list, authors.
#' @param api_df           data.frame; columns version, exports_added,
#'   exports_removed (JSON strings), n_exports.
#' @param deprecation_series  list of length nrow(summary_df); each element
#'   is list(symbols = character, uses_lifecycle = logical) from
#'   deprecation_signals(ctx).
#' @return Augmented summary_df with cross-version columns appended.
add_cross_version_metrics <- function(summary_df, api_df, deprecation_series) {
  n <- nrow(summary_df)

  # Normalise a possibly-missing deprecation_series entry to safe defaults.
  dep_sig_at <- function(i) {
    sig <- if (i >= 1L && i <= length(deprecation_series)) deprecation_series[[i]] else NULL
    list(
      symbols        = sig$symbols       %||% character(0L),
      uses_lifecycle = isTRUE(sig$uses_lifecycle)
    )
  }

  # Zero-row fast path: add all expected columns as correctly-typed empty vectors.
  if (n == 0L) {
    summary_df$bump_type            <- character(0L)
    summary_df$exports_added_n      <- integer(0L)
    summary_df$exports_removed_n    <- integer(0L)
    summary_df$is_breaking          <- logical(0L)
    summary_df$bump_fidelity_ok     <- logical(0L)
    summary_df$assessed_at          <- character(0L)
    summary_df$assessed_with        <- character(0L)
    summary_df$n_versions           <- integer(0L)
    summary_df$first_release_date   <- character(0L)
    summary_df$latest_release_date  <- character(0L)
    summary_df$release_cadence_days <- numeric(0L)
    summary_df$dependency_drift     <- integer(0L)
    summary_df$authors_added_later  <- integer(0L)
    summary_df$cold_removal_rate    <- numeric(0L)
    summary_df$deprecation_infrastructure_maturity <- integer(0L)
    return(summary_df)
  }

  versions  <- summary_df$version %||% rep(NA_character_, n)
  rel_dates <- if ("released" %in% names(summary_df)) {
    summary_df$released
  } else if ("date" %in% names(summary_df)) {
    summary_df$date
  } else {
    rep(NA_character_, n)
  }

  # ---------- per-version: bump_type ----------
  bump_type     <- character(n)
  bump_type[1L] <- "initial"
  if (n >= 2L) {
    for (i in 2L:n) {
      bump_type[i] <- tryCatch(
        .xv_classify_bump(versions[i - 1L], versions[i]),
        error = function(e) "other"
      )
    }
  }

  # ---------- per-version: export counts from api_df ----------
  api_count <- function(ver, col) {
    if (is.null(api_df) || nrow(api_df) == 0L) return(NA_integer_)
    idx <- match(ver, api_df$version)
    if (is.na(idx)) return(NA_integer_)
    v <- api_df[[col]][idx]
    as.integer(length(.xv_json_to_chr(v %||% "[]")))
  }
  exports_added_n   <- vapply(versions, api_count, integer(1L), col = "exports_added")
  exports_removed_n <- vapply(versions, api_count, integer(1L), col = "exports_removed")
  is_breaking       <- !is.na(exports_removed_n) & exports_removed_n > 0L

  # ---------- per-version: bump_fidelity_ok ----------
  # A removal requires a major bump; an addition requires at least minor.
  bump_fidelity_ok <- vector("logical", n)
  if (n >= 2L) {
    for (i in 2L:n) {
      bt  <- bump_type[i]
      add <- if (is.na(exports_added_n[i]))   0L else exports_added_n[i]
      rem <- if (is.na(exports_removed_n[i])) 0L else exports_removed_n[i]
      bump_fidelity_ok[i] <- if (rem > 0L) {
        identical(bt, "major")
      } else if (add > 0L) {
        bt %in% c("major", "minor")
      } else {
        TRUE
      }
    }
  }
  bump_fidelity_ok[1L] <- NA   # first version has no prior to compare against

  # ---------- per-version: stamp columns ----------
  assessed_at   <- rep(as.character(Sys.Date()), n)
  assessed_with <- rep(paste0("bioc-code-metrics; R ", as.character(getRversion())), n)

  # ---------- package-level: release cadence ----------
  parsed_dates <- suppressWarnings(as.Date(rel_dates, format = "%Y-%m-%d"))
  valid_dates  <- sort(parsed_dates[!is.na(parsed_dates)])
  first_date   <- if (length(valid_dates) >= 1L) as.character(min(valid_dates)) else NA_character_
  latest_date  <- if (length(valid_dates) >= 1L) as.character(max(valid_dates)) else NA_character_
  cadence_days <- if (length(valid_dates) >= 2L) {
    median(as.numeric(diff(valid_dates)))
  } else {
    NA_real_
  }

  # ---------- package-level: dependency_drift ----------
  dep_list_col <- if ("dep_list" %in% names(summary_df)) {
    summary_df$dep_list
  } else {
    rep(NA_character_, n)
  }
  dep_drift <- 0L
  if (n >= 2L) {
    for (i in 2L:n) {
      prev_deps <- .xv_json_to_chr(dep_list_col[i - 1L])
      curr_deps <- .xv_json_to_chr(dep_list_col[i])
      dep_drift <- dep_drift +
        length(setdiff(curr_deps, prev_deps)) +
        length(setdiff(prev_deps, curr_deps))
    }
  }

  # ---------- package-level: authors_added_later ----------
  authors_col <- if ("authors" %in% names(summary_df)) {
    summary_df$authors
  } else {
    rep(NA_character_, n)
  }
  first_ids <- .xv_author_identities(authors_col[1L])
  later_ids <- character(0L)
  if (n >= 2L) {
    later_ids <- unique(unlist(lapply(authors_col[2L:n], .xv_author_identities)))
  }
  authors_added_later_val <- length(setdiff(later_ids, first_ids))

  # ---------- package-level: cold_removal_rate ----------
  total_removals <- 0L
  cold_removals  <- 0L
  if (!is.null(api_df) && nrow(api_df) > 0L && n >= 2L) {
    for (i in 2L:n) {
      idx          <- match(versions[i], api_df$version)
      removed_json <- if (!is.na(idx)) api_df$exports_removed[idx] else "[]"
      removed      <- .xv_json_to_chr(removed_json)
      if (length(removed) > 0L) {
        prior_syms     <- dep_sig_at(i - 1L)$symbols
        total_removals <- total_removals + length(removed)
        cold_removals  <- cold_removals  + sum(!removed %in% prior_syms)
      }
    }
  }
  cold_removal_rate_val <- if (total_removals > 0L) {
    cold_removals / total_removals
  } else {
    NA_real_
  }

  # ---------- package-level: deprecation_infrastructure_maturity ----------
  any_lifecycle <- any(vapply(seq_len(n),
                              function(i) dep_sig_at(i)$uses_lifecycle,
                              logical(1L)))
  any_dep_syms  <- any(vapply(seq_len(n),
                              function(i) length(dep_sig_at(i)$symbols) > 0L,
                              logical(1L)))
  dep_maturity  <- if (any_lifecycle) 2L else if (any_dep_syms) 1L else 0L

  # ---------- assemble per-version columns ----------
  summary_df$bump_type         <- bump_type
  summary_df$exports_added_n   <- exports_added_n
  summary_df$exports_removed_n <- exports_removed_n
  summary_df$is_breaking       <- is_breaking
  summary_df$bump_fidelity_ok  <- bump_fidelity_ok
  summary_df$assessed_at       <- assessed_at
  summary_df$assessed_with     <- assessed_with

  # Package-level: NA on all rows, then overwrite the latest row only.
  summary_df$n_versions           <- NA_integer_
  summary_df$first_release_date   <- NA_character_
  summary_df$latest_release_date  <- NA_character_
  summary_df$release_cadence_days <- NA_real_
  summary_df$dependency_drift     <- NA_integer_
  summary_df$authors_added_later  <- NA_integer_
  summary_df$cold_removal_rate    <- NA_real_
  summary_df$deprecation_infrastructure_maturity <- NA_integer_

  summary_df$n_versions[n]           <- n
  summary_df$first_release_date[n]   <- first_date
  summary_df$latest_release_date[n]  <- latest_date
  summary_df$release_cadence_days[n] <- cadence_days
  summary_df$dependency_drift[n]     <- dep_drift
  summary_df$authors_added_later[n]  <- authors_added_later_val
  summary_df$cold_removal_rate[n]    <- cold_removal_rate_val
  summary_df$deprecation_infrastructure_maturity[n] <- dep_maturity

  summary_df
}


#' Analyze all versions of a cloned package repository.
#'
#' For each version (oldest first):
#'   1. Extracts the version tree via git archive.
#'   2. Builds a read_fn over the extracted files.
#'   3. Subsets the full-history churn to this version's commit.
#'   4. Builds a context with prev_exports from the preceding version.
#'   5. Calls analyze_version() to compute metrics.
#'   6. Records the per-version API diff (exports added/removed).
#'   7. Cleans up the extraction directory.
#'
#' Returns a list with three data.frames:
#'   $summary  one row per version, all metric columns + package/version/released
#'   $churn    package-stamped churn (package, version, file, added, deleted)
#'   $api      per-version export diff (package, version, exports_added,
#'             exports_removed, n_exports as JSON strings)
#'
#' Hook for Task 10: cross-version metrics should be computed from $summary after
#' this function returns, before writing to the database.
#'
#' @param repo_dir  Path to the cloned git repository.
#' @param package   Package name string.
#' @return Named list: $summary, $churn, $api.
analyze_package <- function(repo_dir, package) {
  versions_df <- list_versions(repo_dir)
  churn_all   <- package_churn(repo_dir)

  summary_rows       <- vector("list", nrow(versions_df))
  api_rows           <- vector("list", nrow(versions_df))
  prev_exports       <- NULL
  deprecation_series <- vector("list", nrow(versions_df))

  for (i in seq_len(nrow(versions_df))) {
    v      <- versions_df$version[i]
    ref    <- versions_df$ref[i]
    date   <- versions_df$date[i]
    commit <- versions_df$commit[i]

    # Per-version work is wrapped in local() so on.exit fires per iteration
    # rather than accumulating in the outer function's exit handlers.
    # This ensures temp-dir cleanup even when the loop body throws.
    iter <- local({
      tmp <- tempfile(pattern = paste0("ccm_", package, "_"))
      dir.create(tmp, recursive = TRUE)
      on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

      files <- tryCatch(
        extract_version(repo_dir, ref, tmp),
        error = function(e) character(0L)
      )

      # Build a read_fn closed over this iteration's extraction directory
      read_fn <- local({
        .d <- tmp
        function(path) {
          full <- file.path(.d, path)
          if (!file.exists(full)) return("")
          paste(readLines(full, warn = FALSE), collapse = "\n")
        }
      })

      # Subset churn_all to this version's commit
      churn_v <- if (nrow(churn_all) > 0L && !is.na(commit)) {
        # Commit SHAs from list_versions may be full or abbreviated;
        # match on startsWith to handle both
        mask <- startsWith(churn_all$commit, commit) |
                startsWith(commit, churn_all$commit)
        ch <- churn_all[mask, c("file", "added", "deleted"), drop = FALSE]
        rownames(ch) <- NULL
        ch
      } else {
        data.frame(file = character(0L), added = integer(0L),
                   deleted = integer(0L), stringsAsFactors = FALSE)
      }

      ctx <- build_context(
        package      = package,
        version      = v,
        ref          = ref,
        date         = date,
        files        = files,
        read_fn      = read_fn,
        churn_df     = churn_v,
        prev_exports = prev_exports
      )

      # Prefer the rpkg-analyzer binary (a superset of analyze_version, computed
      # from the same extracted source); fall back to the R groups when absent.
      metrics <- analyze_with_binary(tmp)
      if (is.null(metrics)) {
        metrics <- analyze_version(ctx)
      }

      dep_sig <- tryCatch(
        deprecation_signals(ctx),
        error = function(e) list(symbols = character(0L), uses_lifecycle = FALSE)
      )

      # Stamp identity columns
      metrics[["package"]]  <- package
      metrics[["version"]]  <- v
      metrics[["released"]] <- date %||% NA_character_

      # Coerce each metric to a length-1 scalar (guard against bad group output)
      safe_metrics <- lapply(metrics, function(x) {
        if (is.null(x) || length(x) != 1L) NA else x
      })

      # API diff
      curr_exports <- tryCatch(ctx$namespace$exports, error = function(e) character(0L))
      curr_exports <- curr_exports %||% character(0L)
      added_exp    <- setdiff(curr_exports, prev_exports %||% character(0L))
      removed_exp  <- setdiff(prev_exports %||% character(0L), curr_exports)

      api_row <- data.frame(
        package         = package,
        version         = v,
        exports_added   = as.character(
          jsonlite::toJSON(added_exp,   auto_unbox = FALSE)),
        exports_removed = as.character(
          jsonlite::toJSON(removed_exp, auto_unbox = FALSE)),
        n_exports       = length(curr_exports),
        stringsAsFactors = FALSE
      )

      list(safe_metrics = safe_metrics, api_row = api_row, prev_exports = curr_exports,
           dep_sig = dep_sig)
    })

    summary_rows[[i]]       <- iter$safe_metrics
    api_rows[[i]]           <- iter$api_row
    prev_exports            <- iter$prev_exports
    deprecation_series[[i]] <- iter$dep_sig
  }

  # Assemble summary data.frame from list of named lists
  summary_df <- if (length(summary_rows) > 0L) {
    # Collect all column names across rows (groups may differ if some fail)
    all_cols <- unique(unlist(lapply(summary_rows, names)))
    rows_df  <- lapply(summary_rows, function(r) {
      row <- lapply(all_cols, function(cn) {
        v <- r[[cn]]
        if (is.null(v)) NA else v
      })
      names(row) <- all_cols
      as.data.frame(row, stringsAsFactors = FALSE)
    })
    do.call(rbind, rows_df)
  } else {
    data.frame()
  }

  # Stamp churn with package name
  churn_df <- if (nrow(churn_all) > 0L) {
    data.frame(
      package = package,
      version = churn_all$version,
      file    = churn_all$file,
      added   = churn_all$added,
      deleted = churn_all$deleted,
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      package = character(0L), version = character(0L),
      file    = character(0L), added   = integer(0L),
      deleted = integer(0L),  stringsAsFactors = FALSE
    )
  }

  api_df <- if (length(api_rows) > 0L) {
    do.call(rbind, api_rows)
  } else {
    data.frame(
      package         = character(0L), version         = character(0L),
      exports_added   = character(0L), exports_removed = character(0L),
      n_exports       = integer(0L),
      stringsAsFactors = FALSE
    )
  }

  summary_df <- add_cross_version_metrics(summary_df, api_df, deprecation_series)

  list(
    summary = summary_df,
    churn   = churn_df,
    api     = api_df
  )
}
