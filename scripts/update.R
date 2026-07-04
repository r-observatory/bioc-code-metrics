# scripts/update.R: sharded, resumable orchestration layer.
#
# Load order: config.R -> git.R -> context.R -> metrics/*.R -> analyze.R -> export.R -> update.R
# This file does NOT auto-source its dependencies; the caller controls source order.

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

# Row-bind a list of data.frames, filling missing columns with NA.
# Any NULL or zero-row element is silently dropped.
# Returns NULL when no non-empty frames are present.
.rbind_union_all <- function(dfs) {
  dfs <- Filter(function(df) !is.null(df) && nrow(df) > 0L, dfs)
  if (length(dfs) == 0L) return(NULL)
  all_cols <- unique(unlist(lapply(dfs, names)))
  padded <- lapply(dfs, function(df) {
    missing_cols <- setdiff(all_cols, names(df))
    for (col in missing_cols) df[[col]] <- NA
    df[, all_cols, drop = FALSE]
  })
  do.call(rbind, padded)
}

.empty_summary <- function() {
  data.frame(package = character(0L), version = character(0L),
             stringsAsFactors = FALSE)
}

.empty_churn <- function() {
  data.frame(
    package = character(0L), version = character(0L),
    file    = character(0L), added   = integer(0L),
    deleted = integer(0L),
    stringsAsFactors = FALSE
  )
}

.empty_api <- function() {
  data.frame(
    package         = character(0L), version         = character(0L),
    exports_added   = character(0L), exports_removed = character(0L),
    n_exports       = integer(0L),
    stringsAsFactors = FALSE
  )
}

# Increment consecutive_failures for a package in bioc_metrics_failures.
.record_failure <- function(con, pkg) {
  now_str  <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  existing <- DBI::dbGetQuery(con,
    "SELECT consecutive_failures FROM bioc_metrics_failures WHERE package = ?",
    params = list(pkg))
  if (nrow(existing) == 0L) {
    DBI::dbExecute(con,
      "INSERT INTO bioc_metrics_failures (package, consecutive_failures, last_attempt)
       VALUES (?, 1, ?)",
      params = list(pkg, now_str))
  } else {
    DBI::dbExecute(con,
      "UPDATE bioc_metrics_failures
       SET consecutive_failures = consecutive_failures + 1, last_attempt = ?
       WHERE package = ?",
      params = list(now_str, pkg))
  }
  invisible(NULL)
}

# Delete a package's failure record (reset after a successful analysis).
.reset_failure <- function(con, pkg) {
  DBI::dbExecute(con,
    "DELETE FROM bioc_metrics_failures WHERE package = ?",
    params = list(pkg))
  invisible(NULL)
}

# Return packages with consecutive_failures >= MAX_CLONE_FAILURES.
.permanent_failures <- function(con) {
  DBI::dbGetQuery(con,
    "SELECT package FROM bioc_metrics_failures WHERE consecutive_failures >= ?",
    params = list(MAX_CLONE_FAILURES))$package
}

#' Packages needing a metrics backfill: those with a stored row where the binary
#' sentinel column is NULL, or every stored package when that column has not been
#' added yet. Restricted to the current universe and excluding permanent
#' failures.
.recollect_todo <- function(con, universe_pkgs, perm_fail_pkgs,
                            sentinel = "n_fns_r", table = "bioc_code_summary") {
  if (!table %in% DBI::dbListTables(con)) return(character(0L))
  pkgs <- if (!sentinel %in% DBI::dbListFields(con, table)) {
    DBI::dbGetQuery(con, sprintf("SELECT DISTINCT package FROM %s", table))[["package"]]
  } else {
    DBI::dbGetQuery(con, sprintf(
      'SELECT DISTINCT package FROM %s WHERE "%s" IS NULL', table, sentinel
    ))[["package"]]
  }
  pkgs <- pkgs[!pkgs %in% perm_fail_pkgs]
  pkgs <- pkgs[pkgs %in% as.character(universe_pkgs)]
  sort(as.character(pkgs))
}

# ---------------------------------------------------------------------------
# default_io
# ---------------------------------------------------------------------------

#' Build the default production IO interface.
#'
#' @return A list with:
#'   \item{package_list}{function() -> data.frame(package, latest_version)}
#'   \item{clone}{function(pkg, dest) -> logical}
default_io <- function() {
  list(
    package_list = function() {
      # Bioconductor SOFTWARE and WORKFLOW packages (current release).
      # Only these two categories have github.com/bioc repos with RELEASE_X_Y
      # branches. Annotation and experiment data packages are excluded because
      # their repos either do not exist on github.com/bioc or carry no code.
      tryCatch({
        repos <- c(
          "https://bioconductor.org/packages/release/bioc",
          "https://bioconductor.org/packages/release/workflows"
        )
        m <- available.packages(repos = repos)
        df <- data.frame(
          package        = as.character(m[, "Package"]),
          latest_version = as.character(m[, "Version"]),
          stringsAsFactors = FALSE,
          row.names        = NULL
        )
        df[order(df$package), ]
      }, error = function(e) {
        warning(sprintf("Could not fetch Bioconductor package list: %s",
                        conditionMessage(e)))
        data.frame(package = character(0L), latest_version = character(0L),
                   stringsAsFactors = FALSE)
      })
    },

    clone = function(pkg, dest) {
      clone_package(pkg, dest, base = BIOC_GIT_BASE,
                    token = Sys.getenv("GITHUB_TOKEN", ""))
    }
  )
}

# ---------------------------------------------------------------------------
# run_update
# ---------------------------------------------------------------------------

#' Run one sharded update of the bioc-code-metrics pipeline.
#'
#' Opens (or creates) the SQLite DB at out_dir/DB_FILENAME, determines which
#' packages need analysis by querying the DB (not by reading whole tables),
#' processes the next shard, upserts only the shard's rows in-place (bounded
#' to O(shard) memory), and emits a manifest.json.
#'
#' Clone and analyze failures are tracked per-package. Packages that have
#' failed >= MAX_CLONE_FAILURES consecutive times are permanently excluded from
#' the to-do list and counted in the manifest permanent_failures field.
#'
#' @param io         IO interface: list with $package_list() and $clone().
#'   Use default_io() for production; inject a fake for tests.
#' @param out_dir    Directory to read prior DB from and write outputs to.
#' @param shard_size Maximum packages to analyze in this run.
#'   Defaults to SHARD_SIZE from config.R.
#' @param force_full When TRUE, wipes all existing metric rows and re-analyzes
#'   all packages (excluding permanent failures) from scratch.
#' @param recollect When TRUE, re-analyzes only packages whose stored rows
#'   predate the binary metrics (a sentinel column is NULL). Nothing is wiped:
#'   rows are upserted in place, so the served DB stays complete throughout.
#' @return Manifest list (invisibly).
run_update <- function(io, out_dir, shard_size = SHARD_SIZE, force_full = FALSE,
                       recollect = FALSE) {
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  db_path <- file.path(out_dir, DB_FILENAME)

  # ---- 1. Open DB (creates tables if absent) --------------------------------
  con <- open_or_init_db(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # ---- 2. Analyzed state (O(n_packages) query, not full table read) ---------
  if (isTRUE(force_full)) {
    # Wipe all metric rows so everything is treated as unseen.
    tables <- DBI::dbListTables(con)
    for (tbl in c("bioc_code_summary", "bioc_code_churn", "bioc_api_history")) {
      if (tbl %in% tables) DBI::dbExecute(con, sprintf("DELETE FROM %s", tbl))
    }
    analyzed <- character(0L)
  } else {
    analyzed_df <- db_analyzed_state(con)
    analyzed <- if (nrow(analyzed_df) > 0L) {
      setNames(as.character(analyzed_df$version),
               as.character(analyzed_df$package))
    } else {
      character(0L)
    }
  }

  # ---- 3. Universe ----------------------------------------------------------
  universe <- io$package_list()
  if (!is.data.frame(universe) || nrow(universe) == 0L) {
    universe <- data.frame(package = character(0L), latest_version = character(0L),
                           stringsAsFactors = FALSE)
  }
  n_universe <- nrow(universe)

  # ---- 4. Permanent failures: exclude from to-do ----------------------------
  perm_fail_pkgs <- .permanent_failures(con)

  # ---- 5. To-do: packages that need analysis --------------------------------
  if (isTRUE(force_full)) {
    todo_pkgs <- sort(as.character(
      universe$package[!universe$package %in% perm_fail_pkgs]
    ))
  } else if (isTRUE(recollect)) {
    # Backfill: only packages whose rows predate the binary metrics. No wipe;
    # upsert_shard replaces each package's rows in place.
    todo_pkgs <- .recollect_todo(con, universe$package, perm_fail_pkgs)
  } else {
    is_todo <- vapply(seq_len(n_universe), function(i) {
      pkg <- as.character(universe$package[i])
      if (pkg %in% perm_fail_pkgs) return(FALSE)  # permanently excluded
      lv  <- universe$latest_version[i]
      if (!pkg %in% names(analyzed)) return(TRUE)   # never analyzed
      stored_v <- analyzed[[pkg]]
      # Packages without a latest_version: skip once analyzed.
      if (is.na(lv)) return(FALSE)
      # New release detected: universe version differs from what is stored.
      !identical(as.character(lv), as.character(stored_v))
    }, logical(1L))
    todo_pkgs <- sort(as.character(universe$package[is_todo]))
  }

  # Take the first shard_size packages from the to-do list (deterministic order).
  shard_pkgs <- if (length(todo_pkgs) > shard_size) {
    todo_pkgs[seq_len(shard_size)]
  } else {
    todo_pkgs
  }

  # ---- 6. Analyze the shard (parallel) -------------------------------------
  shard_summary_list <- list()
  shard_churn_list   <- list()
  shard_api_list     <- list()
  shard_failures     <- character(0L)

  if (!dir.exists(WORK_DIR)) dir.create(WORK_DIR, recursive = TRUE)

  # Worker: clone + analyze one package. No database access.
  # Returns list(package, ok, [summary, churn, api]).
  .pkg_worker <- function(pkg) {
    dest <- file.path(WORK_DIR, pkg)
    on.exit(unlink(dest, recursive = TRUE, force = TRUE), add = TRUE)
    on.exit(setTimeLimit(), add = TRUE)
    setTimeLimit(elapsed = WORKER_TIMEOUT, transient = TRUE)
    ok <- tryCatch(io$clone(pkg, dest), error = function(e) FALSE)
    if (!isTRUE(ok)) return(list(package = pkg, ok = FALSE))
    res <- tryCatch(
      analyze_package(dest, pkg),
      error = function(e) {
        warning(sprintf("analyze_package failed for '%s': %s",
                        pkg, conditionMessage(e)))
        NULL
      }
    )
    if (is.null(res)) return(list(package = pkg, ok = FALSE))
    list(package = pkg, ok = TRUE,
         summary = res$summary, churn = res$churn, api = res$api)
  }

  results <- parallel::mclapply(shard_pkgs, .pkg_worker,
                                mc.cores       = ANALYSIS_CORES,
                                mc.preschedule = FALSE)

  # Collect results in input order (shard_pkgs is sorted, so DB is deterministic).
  # All DB writes happen here in the parent process.
  for (i in seq_along(results)) {
    r   <- results[[i]]
    pkg <- shard_pkgs[[i]]
    # Guard: mclapply may return a try-error on worker crash.
    if (inherits(r, "try-error") || is.null(r[["ok"]]) || !isTRUE(r$ok)) {
      shard_failures <- c(shard_failures, pkg)
      .record_failure(con, pkg)
    } else {
      shard_summary_list[[pkg]] <- r$summary
      shard_churn_list[[pkg]]   <- r$churn
      shard_api_list[[pkg]]     <- r$api
      .reset_failure(con, pkg)
    }
  }

  # ---- 7. Upsert shard into DB in-place (O(shard) memory) ------------------
  fresh_pkgs    <- names(shard_summary_list)
  fresh_summary <- .rbind_union_all(shard_summary_list) %||% .empty_summary()
  fresh_churn   <- .rbind_union_all(shard_churn_list)   %||% .empty_churn()
  fresh_api     <- .rbind_union_all(shard_api_list)     %||% .empty_api()

  if (length(fresh_pkgs) > 0L) {
    upsert_shard(con, fresh_summary, fresh_churn, fresh_api)
  }

  # ---- 8. Manifest ---------------------------------------------------------
  # bioc_code_summary is created lazily by upsert_shard; may not exist yet if
  # this is the first run and every package in the shard failed.
  n_analyzed_pkgs <- {
    tbls <- DBI::dbListTables(con)
    if ("bioc_code_summary" %in% tbls) {
      DBI::dbGetQuery(
        con, "SELECT COUNT(DISTINCT package) AS n FROM bioc_code_summary")$n %||% 0L
    } else {
      0L
    }
  }
  new_fp <- db_fingerprint(con)

  # Re-query permanent failures after this run (some may have just hit the limit).
  n_permanent_failures <- length(.permanent_failures(con))

  manifest_path <- file.path(out_dir, "manifest.json")
  prior_fp <- tryCatch({
    if (file.exists(manifest_path)) {
      m <- jsonlite::fromJSON(manifest_path)
      m[["fingerprint"]]
    } else {
      NULL
    }
  }, error = function(e) NULL)

  # bootstrap_complete: no deferred packages remain AND DB covers the universe
  # minus permanently-failed packages.
  remaining_after    <- setdiff(todo_pkgs, shard_pkgs)
  bootstrap_complete <- length(remaining_after) == 0L &&
    n_analyzed_pkgs >= (n_universe - n_permanent_failures)

  # changed: something substantive happened OR the content hash shifted.
  changed <- isTRUE(force_full) ||
    length(fresh_pkgs) > 0L ||
    !identical(prior_fp, new_fp)

  manifest <- list(
    generated_at         = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    n_universe           = n_universe,
    n_analyzed           = n_analyzed_pkgs,
    n_shard              = length(shard_pkgs),
    shard_failures       = list(
      count    = length(shard_failures),
      packages = head(shard_failures, 20L)
    ),
    permanent_failures   = n_permanent_failures,
    bootstrap_complete   = bootstrap_complete,
    fingerprint          = new_fp,
    changed              = changed
  )

  write_manifest(manifest_path, manifest)
  invisible(manifest)
}

# ---------------------------------------------------------------------------
# CLI entry point
# ---------------------------------------------------------------------------
if (identical(sys.nframe(), 0L)) {
  # Standalone invocation (Rscript scripts/update.R): source the pipeline in
  # dependency order. Locate this script's directory so it works from any cwd.
  .script_dir <- {
    fa <- grep("^--file=", commandArgs(FALSE), value = TRUE)
    if (length(fa) >= 1L) dirname(sub("^--file=", "", fa[1L])) else "scripts"
  }
  source(file.path(.script_dir, "config.R"))
  source(file.path(.script_dir, "git.R"))
  source(file.path(.script_dir, "context.R"))
  source(file.path(.script_dir, "binary.R"))
  for (.f in sort(list.files(file.path(.script_dir, "metrics"),
                             pattern = "[.]R$", full.names = TRUE))) source(.f)
  source(file.path(.script_dir, "analyze.R"))
  source(file.path(.script_dir, "export.R"))

  args <- commandArgs(trailingOnly = TRUE)

  # First non-flag argument is out_dir.
  positional <- args[!startsWith(args, "--")]
  out_dir    <- if (length(positional) >= 1L) {
    positional[1L]
  } else {
    stop(
      "Usage: Rscript scripts/update.R <out_dir> [--shard=N] [--bootstrap]",
      call. = FALSE
    )
  }

  shard_override <- SHARD_SIZE
  force_full     <- FALSE
  recollect      <- FALSE

  for (arg in args[startsWith(args, "--")]) {
    if (startsWith(arg, "--shard=")) {
      n <- suppressWarnings(
        as.integer(sub("^--shard=", "", arg, perl = TRUE))
      )
      if (!is.na(n) && n > 0L) shard_override <- n
    } else if (identical(arg, "--bootstrap")) {
      force_full <- TRUE
    } else if (identical(arg, "--recollect")) {
      recollect <- TRUE
    }
  }

  io <- default_io()
  run_update(io, out_dir, shard_size = shard_override, force_full = force_full,
             recollect = recollect)
  message("Done.")
}
