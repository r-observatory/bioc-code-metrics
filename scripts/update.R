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

#' Packages needing a metrics backfill: those with a stored row where the
#' sentinel column is NULL, or every stored package when that column has not been
#' added yet. Restricted to the current universe and excluding permanent
#' failures.
#'
#' @param latest_only When FALSE (default), a package is flagged if ANY of its
#'   rows has a NULL sentinel. Correct for a per-version sentinel like n_fns_r.
#'   When TRUE, the NULL check is confined to the package's latest-version row
#'   (the row carrying a non-NULL latest_release_date). This is required for a
#'   marker written only on the latest row (e.g. detail_scanned): checking any
#'   row would re-flag every multi-version package forever, so the backfill would
#'   never converge. Packages with no latest_release_date row are not flagged.
.recollect_todo <- function(con, universe_pkgs, perm_fail_pkgs,
                            sentinel = "n_fns_r", table = "bioc_code_summary",
                            latest_only = FALSE) {
  if (!table %in% DBI::dbListTables(con)) return(character(0L))
  fields <- DBI::dbListFields(con, table)
  pkgs <- if (isTRUE(latest_only)) {
    if (!"latest_release_date" %in% fields) {
      character(0L)
    } else if (!sentinel %in% fields) {
      DBI::dbGetQuery(con, sprintf(
        "SELECT DISTINCT package FROM %s WHERE latest_release_date IS NOT NULL",
        table))[["package"]]
    } else {
      DBI::dbGetQuery(con, sprintf(
        'SELECT DISTINCT package FROM %s
         WHERE latest_release_date IS NOT NULL AND "%s" IS NULL',
        table, sentinel))[["package"]]
    }
  } else if (!sentinel %in% fields) {
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
#' Bioconductor package-list repositories to scan (current release).
#'
#' SOFTWARE and WORKFLOW carry code; EXPERIMENT data packages carry example
#' datasets plus some code. All three have github.com/bioc repos with
#' RELEASE_X_Y branches, so they flow through the same clone and version-history
#' path. Annotation data packages are intentionally omitted: they have no git
#' repos (on github.com/bioc or git.bioconductor.org) and no RELEASE branches,
#' so the branch-based version history this pipeline relies on does not apply.
#'
#' @return Character vector of Bioconductor repository base URLs.
bioc_package_repos <- function() {
  c(
    "https://bioconductor.org/packages/release/bioc",
    "https://bioconductor.org/packages/release/workflows",
    "https://bioconductor.org/packages/release/data/experiment"
  )
}

default_io <- function() {
  list(
    package_list = function() {
      # Bioconductor SOFTWARE, WORKFLOW, and EXPERIMENT data packages (current
      # release); see bioc_package_repos() for why annotation is omitted.
      tryCatch({
        repos <- bioc_package_repos()
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
#' Opens (or creates) the code DB at out_dir/DB_FILENAME and the dataset DB at
#' out_dir/DATA_DB_FILENAME, determines which packages need analysis by querying
#' the DB (not by reading whole tables), processes the next shard, upserts only
#' the shard's rows in-place (bounded to O(shard) memory) with dataset rows
#' written before the code summary, and emits code-manifest.json,
#' data-manifest.json, run-status.json, and the changed-packages.txt
#' accumulator.
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

  db_path      <- file.path(out_dir, DB_FILENAME)
  data_db_path <- file.path(out_dir, DATA_DB_FILENAME)

  # ---- 1. Open both DBs (creates tables if absent) --------------------------
  con <- open_or_init_db(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  data_con <- open_or_init_data_db(data_db_path)
  on.exit(DBI::dbDisconnect(data_con), add = TRUE)

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
    changed <- as.character(universe$package[is_todo])
    # Also drain any packages whose rows predate the binary metrics, so a normal
    # scheduled run finishes the one-time backfill and then reverts to just the
    # changed packages once none remain.
    backfill <- .recollect_todo(con, universe$package, perm_fail_pkgs)
    # And drain any packages whose latest-version row was stored before the
    # per-function/per-edge detail scan (detail_scanned IS NULL on that row).
    # Latest-row-scoped so it converges: a package re-analyzed once is marked and
    # never re-flagged, even if it produced zero functions.
    detail_backfill <- .recollect_todo(con, universe$package, perm_fail_pkgs,
                                        sentinel = "detail_scanned",
                                        latest_only = TRUE)
    # And drain any package whose latest-version row predates the dataset reader
    # (datasets_scanned IS NULL), so bioc_datasets fills in without a manual
    # recollect. Also latest-row-scoped, so it converges once re-analyzed.
    dataset_backfill <- .recollect_todo(con, universe$package, perm_fail_pkgs,
                                        sentinel = "datasets_scanned",
                                        latest_only = TRUE)
    todo_pkgs <- sort(unique(c(changed, backfill, detail_backfill, dataset_backfill)))
  }

  # Take the first shard_size packages from the to-do list (deterministic order).
  shard_pkgs <- if (length(todo_pkgs) > shard_size) {
    todo_pkgs[seq_len(shard_size)]
  } else {
    todo_pkgs
  }

  # ---- 5b. Shard plan + wall-clock start ------------------------------------
  # One line printed BEFORE the blocking parallel analyze so the operator sees
  # the shard's size, the to-do pool composition, and resources at the top of the
  # gap. changed/backfill/detail_backfill are the raw (overlapping) to-do pools
  # and exist only on the scheduled path; guard with exists() so --bootstrap and
  # --recollect runs still print (they show 0/0/0).
  t_shard0   <- Sys.time()
  n_changed  <- if (exists("changed",         inherits = FALSE)) length(changed)         else 0L
  n_backfill <- if (exists("backfill",        inherits = FALSE)) length(backfill)        else 0L
  n_detail   <- if (exists("detail_backfill", inherits = FALSE)) length(detail_backfill) else 0L
  cat(sprintf(
    "shard plan: %d pkgs this shard; to-do pool %d (changed %d / backfill %d / detail %d, overlapping), %d will remain; %d cores, %ds/pkg timeout\n",
    length(shard_pkgs), length(todo_pkgs), n_changed, n_backfill, n_detail,
    length(todo_pkgs) - length(shard_pkgs), ANALYSIS_CORES, WORKER_TIMEOUT),
    file = stdout())
  flush(stdout())

  # ---- 6. Analyze the shard (parallel) -------------------------------------
  shard_summary_list   <- list()
  shard_churn_list     <- list()
  shard_api_list       <- list()
  shard_functions_list <- list()
  shard_edges_list     <- list()
  shard_datasets_list  <- list()
  shard_failures       <- character(0L)

  if (!dir.exists(WORK_DIR)) dir.create(WORK_DIR, recursive = TRUE)

  # Worker: clone + analyze one package. No database access.
  # Returns list(package, ok, [summary, churn, api]).
  .pkg_worker <- function(pkg) {
    .t0  <- Sys.time()
    .idx <- match(pkg, shard_pkgs)          # queue position; shard_pkgs is unique
    .n   <- length(shard_pkgs)
    # Thinned per-worker completion line, emitted FROM the fork so it streams live
    # during the otherwise-silent parallel phase. Prints only on every 25th queue
    # position, every failure, every slow (>=30s) package, and the last position.
    # One fully-formed cat() to stdout (< PIPE_BUF): forks reorder whole lines but
    # never byte-interleave, and fd 1 is disjoint from mclapply's result pipe. The
    # emit is wrapped in try() so a broken-stream write can never turn an ok
    # package into a recorded failure.
    .done <- function(ok, stage, nver) {
      el <- as.numeric(difftime(Sys.time(), .t0, units = "secs"))
      if (isTRUE(ok) && .idx %% 25L != 0L && el < 30 && !identical(.idx, .n)) {
        return(invisible())
      }
      try({
        cat(sprintf("[%d/%d] %s %s: %s in %.1fs\n",
                    .idx, .n,
                    if (isTRUE(ok)) "ok" else "FAIL", pkg,
                    if (isTRUE(ok)) sprintf("%d versions", nver)
                    else paste0(stage, " failed"),
                    el),
            file = stdout())
        flush(stdout())
      }, silent = TRUE)
      invisible()
    }
    dest <- file.path(WORK_DIR, pkg)
    on.exit(unlink(dest, recursive = TRUE, force = TRUE), add = TRUE)
    on.exit(setTimeLimit(), add = TRUE)
    setTimeLimit(elapsed = WORKER_TIMEOUT, transient = TRUE)
    ok <- tryCatch(io$clone(pkg, dest), error = function(e) FALSE)
    if (!isTRUE(ok)) {
      .done(FALSE, "clone", 0L)
      return(list(package = pkg, ok = FALSE))
    }
    res <- tryCatch(
      analyze_package(dest, pkg),
      error = function(e) {
        warning(sprintf("analyze_package failed for '%s': %s",
                        pkg, conditionMessage(e)))
        NULL
      }
    )
    if (is.null(res)) {
      .done(FALSE, "analyze", 0L)
      return(list(package = pkg, ok = FALSE))
    }
    .done(TRUE, "ok", nrow(res$summary))
    list(package = pkg, ok = TRUE,
         summary = res$summary, churn = res$churn, api = res$api,
         functions = res$functions, edges = res$edges, datasets = res$datasets)
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
      shard_summary_list[[pkg]]   <- r$summary
      shard_churn_list[[pkg]]     <- r$churn
      shard_api_list[[pkg]]       <- r$api
      shard_functions_list[[pkg]] <- r$functions
      shard_edges_list[[pkg]]     <- r$edges
      shard_datasets_list[[pkg]]  <- r$datasets
      .reset_failure(con, pkg)
    }
  }

  # ---- 7. Upsert shard into DB in-place (O(shard) memory) ------------------
  fresh_pkgs      <- names(shard_summary_list)
  fresh_summary   <- .rbind_union_all(shard_summary_list)   %||% .empty_summary()
  fresh_churn     <- .rbind_union_all(shard_churn_list)     %||% .empty_churn()
  fresh_api       <- .rbind_union_all(shard_api_list)       %||% .empty_api()
  fresh_functions <- .rbind_union_all(shard_functions_list) %||% .empty_functions_df()
  fresh_edges     <- .rbind_union_all(shard_edges_list)     %||% .empty_edges_df()
  fresh_datasets  <- .rbind_union_all(shard_datasets_list)  %||% .empty_datasets_df()

  if (length(fresh_pkgs) > 0L) {
    # Write dataset rows before the code summary stamps datasets_scanned = TRUE,
    # so a scanned code row always implies its dataset rows were written. The
    # dataset write is delete-then-insert (idempotent), so if the code write
    # fails afterwards the package stays on the to-do list and the next run
    # redoes both cleanly, rather than being marked done with datasets missing.
    upsert_datasets(data_con, fresh_datasets, fresh_pkgs)
    upsert_shard(con, fresh_summary, fresh_churn, fresh_api,
                 fresh_functions, fresh_edges)
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

  prior_fp <- tryCatch({
    prev_path <- file.path(out_dir, "prev-code-manifest.json")
    cur_path  <- file.path(out_dir, "code-manifest.json")
    src <- if (file.exists(prev_path)) prev_path else if (file.exists(cur_path)) cur_path else NULL
    if (is.null(src)) NULL else jsonlite::fromJSON(src)[["fingerprint"]]
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
    changed              = changed,
    n_remaining          = length(remaining_after),
    n_fresh              = length(fresh_pkgs),
    n_versions           = nrow(fresh_summary)
  )

  # ---- 8b. Shard receipt ----------------------------------------------------
  # One-line closing summary, printed after the collection loop and before the
  # manifest is written, so the merged CI log shows what this shard accomplished.
  cat(sprintf(
    "shard done in %.0fs: %d/%d ok, %d failed; %d versions, %d functions, %d edges written; DB %d/%d; %d queued; complete=%s\n",
    as.numeric(difftime(Sys.time(), t_shard0, units = "secs")),
    length(fresh_pkgs), length(shard_pkgs), length(shard_failures),
    nrow(fresh_summary), nrow(fresh_functions), nrow(fresh_edges),
    n_analyzed_pkgs, n_universe, length(remaining_after),
    tolower(as.character(bootstrap_complete))),
    file = stdout())
  flush(stdout())

  bootstrap <- list(n_analyzed = n_analyzed_pkgs, n_universe = n_universe,
                    n_remaining = length(remaining_after),
                    bootstrap_complete = bootstrap_complete)
  code_db_bytes <- as.numeric(file.info(db_path)$size %||% 0)
  data_db_bytes <- as.numeric(file.info(data_db_path)$size %||% 0)

  code_manifest <- build_manifest(
    con, series = "code", repo = PUBLISH_REPO, db_filename = DB_FILENAME,
    db_bytes = code_db_bytes,
    tables = c("bioc_code_summary", "bioc_api_history", "bioc_functions",
               "bioc_call_edges", "bioc_code_churn"),
    fp_table = "bioc_code_summary", fp_cols = c("package", "version"),
    pkg_table = "bioc_code_summary", ver_table = "bioc_code_summary",
    stat_table = "bioc_code_summary", stat_cols = c("loc_r", "n_fns_r"),
    bootstrap = bootstrap)

  data_manifest <- build_manifest(
    data_con, series = "data", repo = PUBLISH_REPO, db_filename = DATA_DB_FILENAME,
    db_bytes = data_db_bytes,
    tables = c("bioc_datasets", "bioc_dataset_versions", "bioc_dataset_contents"),
    fp_table = "bioc_datasets", fp_cols = c("package", "name", "current_content_id"),
    pkg_table = "bioc_datasets", ver_table = "bioc_dataset_versions",
    stat_table = "bioc_dataset_contents", stat_cols = c("nrow", "ncol"),
    bootstrap = bootstrap)

  write_manifest(file.path(out_dir, "code-manifest.json"), code_manifest)
  write_manifest(file.path(out_dir, "data-manifest.json"), data_manifest)
  write_manifest(file.path(out_dir, "run-status.json"),
                 list(changed = changed, bootstrap_complete = bootstrap_complete,
                      n_analyzed = n_analyzed_pkgs, n_universe = n_universe,
                      n_remaining = length(remaining_after), n_fresh = length(fresh_pkgs),
                      n_shard = length(shard_pkgs),
                      n_versions = nrow(fresh_summary),
                      shard_failures = length(shard_failures)))

  if (length(fresh_pkgs) > 0L) {
    record_changed_packages(file.path(out_dir, "changed-packages.txt"), fresh_pkgs)
  }

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
