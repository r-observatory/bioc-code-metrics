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

# Collapse a message to one line and cut it to a byte budget, on a whole
# character, so the line a worker prints stays one write.
.clip_bytes <- function(s, max_bytes) {
  s <- gsub("[[:space:]]+", " ", trimws(as.character(s)))
  if (max_bytes <= 0L) return("")
  if (nchar(s, type = "bytes") <= max_bytes) return(s)
  chars <- strsplit(s, "")[[1L]]
  keep  <- cumsum(nchar(chars, type = "bytes")) <= (max_bytes - 3L)
  paste0(paste(chars[keep], collapse = ""), "...")
}

#' The one line a worker prints when it finishes a package.
#'
#' @param elapsed Seconds the worker took; NA for a fork that returned nothing.
#' @param reason Why it failed, appended after a colon and clipped so the whole
#'   line fits in one pipe write. NULL or "" leaves it off.
#' @return A single string ending in one newline.
.worker_line <- function(idx, n, ok, pkg, stage, nver, elapsed, reason = NULL,
                         worker_timeout = WORKER_TIMEOUT) {
  stem <- if (isTRUE(ok)) {
    sprintf("[%d/%d] ok %s: %d versions in %.1fs%s", idx, n, pkg, nver, elapsed,
            if (isTRUE(elapsed >= worker_timeout))
              sprintf(" (past the %ds cap)", as.integer(worker_timeout)) else "")
  } else if (is.na(elapsed)) {
    sprintf("[%d/%d] FAIL %s: %s", idx, n, pkg, stage)
  } else {
    sprintf("[%d/%d] FAIL %s: %s after %.1fs", idx, n, pkg, stage, elapsed)
  }
  if (is.null(reason) || !nzchar(trimws(as.character(reason)))) {
    return(paste0(stem, "\n"))
  }
  # Two bytes for the ": " and one for the newline.
  room <- WORKER_LINE_MAX_BYTES - nchar(stem, type = "bytes") - 3L
  if (room <= 3L) return(paste0(stem, "\n"))
  paste0(stem, ": ", .clip_bytes(reason, room), "\n")
}

# The stage of an error analyze_package raised. An elapsed time at the cap also
# catches a cap swallowed earlier and a later failure.
.classify_failure <- function(e, elapsed, worker_timeout = WORKER_TIMEOUT) {
  if (inherits(e, "extract_failure")) {
    return(if (isTRUE(e$status == 124L)) "git_timeout" else "extract")
  }
  if (inherits(e, "analyzer_parse_incomplete")) return("analyze")
  if ((inherits(e, "condition") && .is_time_limit(e)) ||
      isTRUE(elapsed >= worker_timeout)) {
    return("timeout")
  }
  "analyze"
}

# The stage of a clone that did not succeed: 124 is system2's kill at GIT_TIMEOUT.
.clone_stage <- function(ok) {
  if (isTRUE(as.integer(attr(ok, "status")) == 124L)) "git_timeout" else "clone"
}

# What a clone that did not succeed says: the error it raised, or its exit status.
.clone_reason <- function(ok) {
  if (!is.null(attr(ok, "reason"))) return(attr(ok, "reason"))
  st <- attr(ok, "status")
  if (is.null(st)) "clone failed" else sprintf("git clone exited %d", as.integer(st))
}

# One worker result as the parent records it. A fork that returned nothing, or
# raised outside the worker's handlers, printed no line, so from_parent says
# the parent prints it; its time-limit message makes it a timeout.
.classify_result <- function(r) {
  if (is.null(r)) {
    return(list(ok = FALSE, stage = "crash", elapsed = NA_real_,
                reason = "worker returned no result", from_parent = TRUE))
  }
  if (inherits(r, "try-error")) {
    cond <- attr(r, "condition")
    msg  <- if (inherits(cond, "condition")) conditionMessage(cond) else as.character(r)
    return(list(ok = FALSE,
                stage = if (grepl(.time_limit_msg(), msg, fixed = TRUE)) "timeout" else "crash",
                elapsed = NA_real_, reason = .redact_reason(msg), from_parent = TRUE))
  }
  if (isTRUE(r$ok)) {
    return(list(ok = TRUE, stage = NA_character_, elapsed = r$elapsed %||% NA_real_,
                reason = "", from_parent = FALSE))
  }
  list(ok = FALSE, stage = r$stage %||% "analyze", elapsed = r$elapsed %||% NA_real_,
       reason = r$reason %||% "", from_parent = FALSE)
}

# The run a verdict belongs to: PIPELINE_RUN_ID, which the workflow sets in the
# shard step alone. GITHUB_RUN_ID is never read, since Actions sets it in the
# test steps too. NA outside a run, which keeps one attempt per shard.
.current_run_id <- function() {
  v <- Sys.getenv("PIPELINE_RUN_ID", "")
  if (nzchar(v)) v else NA_character_
}

# The build a verdict names: "" when no binary ran.
.build_key <- function(build) {
  if (is.null(build) || length(build) != 1L || is.na(build)) "" else as.character(build)
}

# The counter a stage counts in. A fetch never reached the analyzer, so its
# count carries across builds; the other two are verdicts on one build.
.failure_class <- function(stage) {
  if (stage %in% c("clone", "extract")) "fetch"
  else if (stage %in% c("timeout", "crash", "git_timeout")) "timeout"
  else "analyze"
}

# Record one failed attempt. fetch_failures restarts on a new latest_version and
# any other verdict zeroes it; analyze_failures and timeout_failures restart on a
# new build or a row from before stages were kept, and timeouts on a new cap.
.record_failure <- function(con, pkg, stage, build, worker_timeout, run_id,
                            elapsed, reason, latest_version) {
  cls <- .failure_class(stage)
  DBI::dbExecute(con, "
    INSERT INTO bioc_metrics_failures
      (package, consecutive_failures, last_attempt, stage, analyzer_version,
       worker_timeout, fetch_failures, fetch_version, analyze_failures,
       timeout_failures, last_run_id, elapsed_s, reason)
    VALUES (:pkg, 1, :now, :stage, :build, :wt, :fetch,
            CASE WHEN :fetch = 1 THEN :lv END, :analyze, :timeout,
            :run_id, :elapsed, :reason)
    ON CONFLICT(package) DO UPDATE SET
      consecutive_failures = consecutive_failures + 1,
      fetch_failures   = CASE WHEN :fetch = 1
                              THEN (CASE WHEN IFNULL(fetch_version, '') = IFNULL(:lv, '')
                                         THEN fetch_failures ELSE 0 END) + 1
                              ELSE 0 END,
      fetch_version    = CASE WHEN :fetch = 1 THEN :lv ELSE fetch_version END,
      analyze_failures = (CASE WHEN stage IS NOT NULL
                                AND IFNULL(analyzer_version, '') = :build
                               THEN analyze_failures ELSE 0 END) + :analyze,
      timeout_failures = (CASE WHEN stage IS NOT NULL
                                AND IFNULL(analyzer_version, '') = :build
                                AND worker_timeout = :wt
                               THEN timeout_failures ELSE 0 END) + :timeout,
      last_attempt     = excluded.last_attempt,
      stage            = excluded.stage,
      analyzer_version = excluded.analyzer_version,
      worker_timeout   = excluded.worker_timeout,
      last_run_id      = excluded.last_run_id,
      elapsed_s        = excluded.elapsed_s,
      reason           = excluded.reason",
    params = list(
      pkg = pkg, now = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      stage = stage, build = .build_key(build), wt = as.integer(worker_timeout),
      fetch = as.integer(cls == "fetch"), analyze = as.integer(cls == "analyze"),
      timeout = as.integer(cls == "timeout"),
      lv = as.character(latest_version %||% NA_character_),
      run_id = as.character(run_id %||% NA_character_),
      elapsed = as.numeric(elapsed %||% NA_real_), reason = .redact_reason(reason)))
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

# ---------------------------------------------------------------------------
# The third state of a scan
# ---------------------------------------------------------------------------
# datasets_scanned answers two of the three states a package can be in: the
# reader ran (whatever it found, zero rows included), or nothing looked. The
# third is a package the reader was asked for and could not read, which the
# marker cannot say without claiming a scan that did not happen. Recorded here
# instead, in the shape this pipeline already uses for a package that cannot be
# cloned: a count, a cap, and no place in the queue past it.
#
# Both backfill queues need it, because both wait on fields only the analyzer
# produces: n_fns_r and the dataset rows. A package the pure-R fallback
# analysed carries neither, so each queue hands it straight back, every run,
# for good. `changed` never goes false and the workflow publishes a dated
# release for a database that has not moved.

# Did the analyzer read this package? datasets_scanned is the answer: the
# dataset reader runs exactly when the binary produced the version's metrics,
# and the marker is written on the latest-version row, so any row carrying it
# says the binary read the version this package's queue position is about.
# Tolerant of the column being absent, NA, logical or integer, because it
# crosses SQLite in both directions.
.analyzer_read_package <- function(summary_df) {
  if (is.null(summary_df) || !is.data.frame(summary_df) ||
      nrow(summary_df) == 0L || !"datasets_scanned" %in% names(summary_df)) {
    return(FALSE)
  }
  v <- suppressWarnings(as.logical(summary_df$datasets_scanned))
  any(!is.na(v) & v)
}

# Count one attempt that did not read the package, against the build that made
# it. The build is part of the record: an attempt says nothing about a reader
# other than the one that made it.
.record_analyzer_read_attempt <- function(con, pkg, analyzer_version = NA_character_) {
  if (!"bioc_analyzer_read_attempts" %in% DBI::dbListTables(con)) return(invisible(NULL))
  ver <- if (is.null(analyzer_version) || length(analyzer_version) != 1L ||
             is.na(analyzer_version) || !nzchar(analyzer_version)) {
    NA_character_
  } else {
    as.character(analyzer_version)
  }
  now_str  <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  existing <- DBI::dbGetQuery(con,
    "SELECT attempts FROM bioc_analyzer_read_attempts WHERE package = ?",
    params = list(pkg))
  if (nrow(existing) == 0L) {
    DBI::dbExecute(con,
      "INSERT INTO bioc_analyzer_read_attempts
         (package, attempts, analyzer_version, last_attempt)
       VALUES (?, 1, ?, ?)",
      params = list(pkg, ver, now_str))
  } else {
    DBI::dbExecute(con,
      "UPDATE bioc_analyzer_read_attempts
       SET attempts = attempts + 1, analyzer_version = ?, last_attempt = ?
       WHERE package = ?",
      params = list(ver, now_str, pkg))
  }
  invisible(NULL)
}

# Forget a package's attempts, because something read it. A count that survived
# a successful read would retire a package that failed once on a bad day sooner
# than the cap says.
.clear_analyzer_read_attempts <- function(con, pkg) {
  if (!"bioc_analyzer_read_attempts" %in% DBI::dbListTables(con)) return(invisible(NULL))
  DBI::dbExecute(con,
    "DELETE FROM bioc_analyzer_read_attempts WHERE package = ?",
    params = list(pkg))
  invisible(NULL)
}

# The builds counted as the running one: all of ANALYZER_SAME_OUTPUT when it
# lists the running build, that build alone when it does not, and none when the
# build cannot be named. Matching is exact, so "0.5.0-test" is not 0.5.0.
.analyzer_output_class <- function(build, same_output = ANALYZER_SAME_OUTPUT) {
  if (is.null(build) || length(build) != 1L || is.na(build) || !nzchar(build)) {
    return(character(0L))
  }
  build <- as.character(build)
  if (build %in% same_output) as.character(same_output) else build
}

# Latest rows written by a build in the running build's class, and all latest
# rows: how far a rescan onto that class has come.
.n_latest_on_class <- function(con, build, same_output = ANALYZER_SAME_OUTPUT) {
  out <- c(on_class = 0L, latest = 0L)
  if (!SUMMARY_TABLE %in% DBI::dbListTables(con)) return(out)
  fields <- DBI::dbListFields(con, SUMMARY_TABLE)
  if (!"latest_release_date" %in% fields) return(out)
  out[["latest"]] <- as.integer(DBI::dbGetQuery(con, sprintf(
    'SELECT COUNT(*) n FROM "%s" WHERE latest_release_date IS NOT NULL',
    SUMMARY_TABLE))$n)
  builds <- .analyzer_output_class(build, same_output)
  if (length(builds) && "analyzer_version" %in% fields) {
    out[["on_class"]] <- as.integer(DBI::dbGetQuery(con, sprintf(
      'SELECT COUNT(*) n FROM "%s" WHERE latest_release_date IS NOT NULL
          AND analyzer_version IN (%s)',
      SUMMARY_TABLE, paste(rep("?", length(builds)), collapse = ",")),
      params = as.list(builds))$n)
  }
  out
}

# Drop attempts made by any build outside the running build's output class.
#
# The count is the verdict of one reader, and a verdict that outlives its
# reader retires a package for good on the say-so of a build nobody runs any
# more. The row it protects is not re-queued by .invalidate_stale_dataset_scans
# either, since that one only clears markers and this package has none, so this
# is the only thing that gives a new build the chance to read it.
#
# Does nothing when the running build cannot be named, for the same reason
# .invalidate_stale_dataset_scans does nothing: a run with no binary records
# its attempts against no build, and clearing those on the next such run would
# reset the count every time and the queue would never drain.
.forget_other_builds_read_attempts <- function(con, current_version,
                                               same_output = ANALYZER_SAME_OUTPUT) {
  if (!"bioc_analyzer_read_attempts" %in% DBI::dbListTables(con)) return(0L)
  builds <- .analyzer_output_class(current_version, same_output)
  if (!length(builds)) return(0L)
  DBI::dbExecute(con, sprintf(
    "DELETE FROM bioc_analyzer_read_attempts
      WHERE analyzer_version IS NULL OR analyzer_version NOT IN (%s)",
    paste(rep("?", length(builds)), collapse = ",")),
    params = as.list(builds))
}

# Packages the backfill queues have stopped asking about.
.analyzer_read_exhausted <- function(con) {
  if (!"bioc_analyzer_read_attempts" %in% DBI::dbListTables(con)) return(character(0L))
  as.character(DBI::dbGetQuery(con,
    "SELECT package FROM bioc_analyzer_read_attempts WHERE attempts >= ?",
    params = list(MAX_ANALYZER_READ_ATTEMPTS))$package)
}

#' How many packages the pipeline has stopped asking for datasets.
#'
#' A subset of .n_datasets_unscanned(): every one of these is honestly unread,
#' because the reader that would have scanned them is the binary that could not
#' read them at all. The difference is that this number does not come down on
#' its own, which is the fact worth publishing. It is what the deliberate slow
#' convergence costs, and if it climbs, the reader is failing on packages
#' rather than on one.
.n_datasets_unreadable <- function(con) {
  length(.analyzer_read_exhausted(con))
}

#' How many packages the dataset scan has never reached.
#'
#' The marker lives on the latest-version row, beside latest_release_date, so
#' the question is scoped the same way .recollect_todo scopes the backfill it
#' feeds: a package counts when its latest row has no marker.
#'
#' Deliberately NOT filtered by permanent failures or by the current universe,
#' unlike the to-do pool. Those are exactly the packages that will never be
#' scanned and so never appear in a queue, which is what makes them invisible:
#' bootstrap_complete goes true and stays true with them still unscanned. This
#' is the number that says how many.
#'
#' @return Package count. Zero when there is nothing to measure yet; every
#'   package when the marker column does not exist, because before the first
#'   write that carries it nothing has been scanned.
.n_datasets_unscanned <- function(con) {
  if (!"bioc_code_summary" %in% DBI::dbListTables(con)) return(0L)
  fields <- DBI::dbListFields(con, "bioc_code_summary")
  if (!"latest_release_date" %in% fields) return(0L)
  sql <- if ("datasets_scanned" %in% fields) {
    "SELECT COUNT(DISTINCT package) n FROM bioc_code_summary
      WHERE latest_release_date IS NOT NULL AND datasets_scanned IS NULL"
  } else {
    "SELECT COUNT(DISTINCT package) n FROM bioc_code_summary
      WHERE latest_release_date IS NOT NULL"
  }
  as.integer(DBI::dbGetQuery(con, sql)$n %||% 0L)
}

#' How many datasets are in the catalog with no profile behind them.
#'
#' A dataset the analyzer described and could not fingerprint keeps its identity
#' row and its version link and gets no content row, because the content table
#' is addressed by fingerprint and there is nothing to key such a record on.
#' That is the right answer and it is also a coverage figure: an S4 object with
#' no reader, a raster packed into bytes, an .R script under data/. A shard
#' where the number climbs is the reader losing objects it used to measure.
#'
#' Taken over every version link rather than the current ones alone, because a
#' version that stopped being measurable is the same finding as a package that
#' never was, and the denominator beside it in the manifest is the count of
#' links the same table holds.
#'
#' Reads the dataset database, not the code one. Zero where the link table does
#' not exist yet, which is a database built from nothing before its first write.
#'
#' @return Count of version links naming no profile.
.n_datasets_unmeasured <- function(con) {
  if (!"bioc_dataset_versions" %in% DBI::dbListTables(con)) return(0L)
  as.integer(DBI::dbGetQuery(con,
    "SELECT COUNT(*) n FROM bioc_dataset_versions WHERE content_id IS NULL")$n %||% 0L)
}

#' Packages needing a metrics backfill: those with a stored row where the
#' sentinel column is NULL, or every stored package when that column has not been
#' added yet. Restricted to the current universe and excluding the packages the
#' caller names.
#'
#' @param perm_fail_pkgs Packages to leave out whatever their sentinel says.
#'   Permanent clone failures on every call, and on the dataset queue the
#'   packages this analyzer build has already asked for and could not read:
#'   both are packages a queue has no way of finishing.
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

#' Clear the dataset-scan marker on rows produced by a build outside the running
#' build's output class (.analyzer_output_class).
#'
#' The marker records that a package was scanned, not what scanned it, so after
#' an upgrade every package looks done and nothing re-runs. Comparing against the
#' version the binary reports puts the stale ones back in the queue.
#'
#' Does nothing when the running version cannot be determined: clearing on a
#' guess would re-scan the archive on every run and never settle.
.invalidate_stale_dataset_scans <- function(con, current_version,
                                            same_output = ANALYZER_SAME_OUTPUT) {
  if (!"bioc_code_summary" %in% DBI::dbListTables(con)) return(0L)
  fields <- DBI::dbListFields(con, "bioc_code_summary")
  if (!"datasets_scanned" %in% fields) return(0L)
  builds <- .analyzer_output_class(current_version, same_output)
  if (!length(builds)) return(0L)
  if (!"analyzer_version" %in% fields) {
    # Nothing on these rows says which build produced them, so none of them can
    # be shown to match the one running now. The column arrives with the first
    # row the analyzer produces, and a database holding nothing but fallback
    # rows never grows it: that database also has no scan marker to clear, so
    # this clears nothing on every run rather than the same rows forever.
    return(DBI::dbExecute(con,
      "UPDATE bioc_code_summary SET datasets_scanned = NULL
        WHERE datasets_scanned IS NOT NULL"))
  }
  DBI::dbExecute(con, sprintf(
    "UPDATE bioc_code_summary SET datasets_scanned = NULL
      WHERE datasets_scanned IS NOT NULL
        AND (analyzer_version IS NULL OR analyzer_version NOT IN (%s))",
    paste(rep("?", length(builds)), collapse = ",")),
    params = as.list(builds))
}

# Address one summary row the way the shard's producers name it. Package names
# and version strings cannot contain a carriage return, so the pair survives
# being flattened into one key.
.analyzer_row_keys <- function(package, versions) {
  versions <- as.character(versions)
  if (length(versions) == 0L) return(character(0L))
  paste(as.character(package), versions, sep = "\r")
}

#' Record on a shard's summary rows which analyzer build scanned them.
#'
#' .invalidate_stale_dataset_scans compares this column against the build about
#' to run, and treats a scanned row that names no build as one it cannot show
#' to be current, so it is cleared again, re-queued, re-analysed, and the run
#' reports a change on a universe where nothing changed. analyze_package stamps
#' the build on the rows the analyzer's own output named and leaves the rest to
#' be filled in here, where the run knows which build it is running.
#'
#' Only the rows the analyzer produced. The pure-R fallback writes rows too,
#' and putting the running build on one of those says the analyzer collected
#' data the analyzer never saw. It is the same false claim datasets_scanned is
#' withheld to avoid, on the same row, so the column would contradict the
#' marker beside it. Nothing is lost by leaving those rows blank:
#' .invalidate_stale_dataset_scans only reads rows that carry a scan marker,
#' and a fallback row carries none, so it is never compared against a build in
#' the first place. What used to keep such a row out of the queue was the
#' stamp; what keeps it out now is the count of attempts that did not read it.
#'
#' What the analyzer reported about itself is left alone: overwriting it would
#' erase the one signal that tells a row collected by an older build from one
#' collected by this one.
#'
#' @param current_version The build about to run, from rpkg_analyzer_version().
#'   NA when there is no binary to ask, in which case nothing is written: a
#'   guess would make every row look current and stop the queue noticing an
#'   upgrade at all.
#' @param produced Keys, from .analyzer_row_keys(), of the rows the analyzer
#'   binary produced. Empty by default, which stamps nothing: a caller that
#'   cannot say which rows the analyzer wrote must not answer for it.
#' @return summary_df, with analyzer_version filled on those rows where it was
#'   missing.
.stamp_analyzer_version <- function(summary_df, current_version,
                                    produced = character(0L)) {
  if (is.null(summary_df) || nrow(summary_df) == 0L) return(summary_df)
  if (is.null(current_version) || length(current_version) != 1L ||
      is.na(current_version) || !nzchar(current_version)) {
    return(summary_df)
  }
  if (length(produced) == 0L) return(summary_df)
  if (!all(c("package", "version") %in% names(summary_df))) return(summary_df)
  mine <- .analyzer_row_keys(summary_df$package, summary_df$version) %in% produced
  stored <- if ("analyzer_version" %in% names(summary_df)) {
    as.character(summary_df$analyzer_version)
  } else {
    rep(NA_character_, nrow(summary_df))
  }
  gap <- mine & (is.na(stored) | !nzchar(stored))
  stored[gap] <- as.character(current_version)
  summary_df$analyzer_version <- stored
  summary_df
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

#' Parse the current Bioconductor release ("X.Y") from config.yaml lines.
#' Returns NA_character_ when no release_version line is present.
.parse_bioc_release <- function(lines) {
  line <- grep("^\\s*release_version:", lines, value = TRUE)
  if (length(line) == 0L) return(NA_character_)
  v <- sub('.*release_version:\\s*"?([0-9]+\\.[0-9]+)"?.*', "\\1", line[[1L]])
  if (grepl("^[0-9]+\\.[0-9]+$", v)) v else NA_character_
}

#' Retry `fn` on error, sleeping RELEASE_RETRY_WAITS_S between attempts. One more
#' attempt is made than there are waits, and the final attempt's error propagates.
#' sleep and rand are injected so the suite asserts the schedule without waiting.
with_retry <- function(fn, waits = RELEASE_RETRY_WAITS_S, sleep = Sys.sleep,
                       rand = function() stats::runif(1, 1, 1.25)) {
  for (w in waits) {
    val <- tryCatch(fn(), error = function(e) e)
    if (!inherits(val, "error")) return(val)
    sleep(w * rand())
  }
  fn()
}

#' The current Bioconductor release number (e.g. "3.23"), from the Bioconductor
#' config. The analyzer stores each package's newest `version` as its max
#' RELEASE_X_Y branch, so this is what a package's stored version must be
#' compared against to decide if it is up to date. NA on failure, which makes
#' the version check a safe no-op (no false re-flagging) rather than an error.
#'
#' That no-op is the right default but a dangerous silence: every analyzed
#' package then looks current, which is indistinguishable in the log from a run
#' that genuinely had nothing to do. On the day Bioconductor cuts a release, a
#' fetch failure here would defer the entire update with nothing to show for it.
#' So the lookup retries first, and announces itself when it still gives up.
#' A config.yaml that arrives without a release_version line counts as a failed
#' attempt too: a gateway error page parses to NA just as an outage does.
.current_bioc_release <- function(url = "https://bioconductor.org/config.yaml",
                                  read = function(u) readLines(u, warn = FALSE),
                                  ...) {
  attempt <- function() {
    v <- .parse_bioc_release(read(url))
    if (is.na(v)) stop("no release_version line in the response")
    v
  }
  tryCatch(
    with_retry(attempt, ...),
    error = function(e) {
      message(sprintf(paste("::warning::Bioconductor release lookup failed (%s): %s.",
                            "Every analyzed package will be treated as up to date this run,",
                            "so a new release will not be picked up until a later run",
                            "reaches config.yaml."),
                      url, conditionMessage(e)))
      NA_character_
    })
}

default_io <- function() {
  list(
    package_list = function() {
      # Bioconductor SOFTWARE, WORKFLOW, and EXPERIMENT data packages (current
      # release); see bioc_package_repos() for why annotation is omitted.
      tryCatch({
        repos <- bioc_package_repos()
        m <- available.packages(repos = repos)
        # latest_version is the current BIOCONDUCTOR RELEASE (e.g. "3.23"), NOT
        # the package's own DESCRIPTION version. The analyzer stores each
        # package's newest `version` as its max RELEASE_X_Y branch, so the
        # up-to-date check in run_update must compare against the release --
        # comparing against the DESCRIPTION version makes every analyzed package
        # look perpetually changed, so the bootstrap re-analyzes the same
        # packages forever and never advances.
        df <- data.frame(
          package        = as.character(m[, "Package"]),
          latest_version = .current_bioc_release(),
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
#'   Not filtered by the analyzer read attempts, unlike the scheduled path: an
#'   operator asking for a backfill by name is asking for the packages the
#'   scheduled run has given up on as well.
#' @return Manifest list (invisibly).
run_update <- function(io, out_dir, shard_size = SHARD_SIZE, force_full = FALSE,
                       recollect = FALSE) {
  # Without the analyzer binary the run still completes and still writes rows,
  # and it makes no progress: the per-package metrics, the per-function detail
  # and every dataset row come from the binary alone, so each package analysed
  # stays in the backfill pool and the next run selects the same shard again.
  # The bootstrap never advances and nothing else in the output says so, which
  # is a silent stall rather than a failure.
  if (!nzchar(rpkg_analyzer_bin())) {
    warning("rpkg-analyzer not found: per-package detail and dataset rows will ",
            "not be written, the backfill pool will not drain, and the shard ",
            "will not advance between runs. Set RPKG_ANALYZER_BIN or install ",
            "the binary.",
            call. = FALSE, immediate. = TRUE)
  }

  # Read once, and use the same answer for both halves of the re-scan queue:
  # the build the stored rows are compared against, and the build stamped on
  # the rows this shard writes. Asking twice would let a binary swapped
  # mid-run clear markers it then never restores.
  analyzer_version <- rpkg_analyzer_version()
  # A build that rejected the flag would exit 2 on every package and leave every
  # row to the R fallback, so a 0.5.0 build proves it reads the flag first.
  if (analyzer_at_least(analyzer_version, "0.5.0") &&
      !rpkg_analyzer_selfcheck(ANALYZER_INPUT_KIND)) {
    stop(sprintf(paste0(
      "rpkg-analyzer %s did not answer --input-kind %s with a summary naming it; ",
      "stopping before any shard"), analyzer_version, ANALYZER_INPUT_KIND),
      call. = FALSE)
  }

  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  db_path      <- file.path(out_dir, DB_FILENAME)
  data_db_path <- file.path(out_dir, DATA_DB_FILENAME)

  # ---- 1. Open both DBs (creates tables if absent) --------------------------
  con <- open_or_init_db(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  data_con <- open_or_init_data_db(data_db_path)
  on.exit(DBI::dbDisconnect(data_con), add = TRUE)
  text_db_path <- file.path(out_dir, RELEASE_TEXT_DB_FILENAME)
  shared_text  <- identical(RELEASE_TEXT_DB_FILENAME, DB_FILENAME)
  text_con <- open_or_init_release_text_db(text_db_path,
                                           con = if (shared_text) con else NULL)
  if (!shared_text) on.exit(DBI::dbDisconnect(text_con), add = TRUE)

  # ---- 2. Analyzed state (O(n_packages) query, not full table read) ---------
  if (isTRUE(force_full)) {
    # Wipe all metric rows so everything is treated as unseen.
    tables <- DBI::dbListTables(con)
    for (tbl in c("bioc_code_summary", "bioc_code_churn", "bioc_api_history")) {
      if (tbl %in% tables) DBI::dbExecute(con, sprintf("DELETE FROM %s", tbl))
    }
    analyzed <- character(0L)
  } else {
    # Before the queues are read, so a gap in the text history is re-read now.
    .reconcile_release_text(con, text_con)
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
  run_id <- .current_run_id()
  lv_of  <- stats::setNames(as.character(universe$latest_version),
                            as.character(universe$package))

  # Which builds count as this one, and how many latest rows they wrote.
  output_class <- .analyzer_output_class(analyzer_version)
  on_class     <- .n_latest_on_class(con, analyzer_version)
  message(sprintf("analyzer %s, output class %s; latest rows on class: %d of %d",
                  analyzer_version %||% "none",
                  if (length(output_class)) paste(output_class, collapse = " ") else "none",
                  on_class[["on_class"]], on_class[["latest"]]))

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
    # An analyzer upgrade changes what a scan finds, so rows produced by an older
    # build are stale even though they are marked scanned. Clearing the marker on
    # those puts them back in the queue below, which drains a shard at a time and
    # settles once every row carries the running build's version.
    n_stale <- .invalidate_stale_dataset_scans(con, analyzer_version)
    message(sprintf("dataset scans invalidated by analyzer change: %d", n_stale))
    # The same change gives back the packages the previous build could not read.
    # Their rows carry no marker to invalidate, so this is the only thing that
    # puts them in front of a new reader. Before the queues are read, so this
    # run is the one that asks again.
    n_retry <- .forget_other_builds_read_attempts(con, analyzer_version)
    message(sprintf("packages to re-read under this analyzer: %d", n_retry))
    # The packages this build has already been given MAX_ANALYZER_READ_ATTEMPTS
    # times and did not read. Both backfill queues below wait on fields only the
    # binary produces, so both would hand these back every run for good. They
    # are excluded the same way and in the same place a package that cannot be
    # cloned is, because it is the same problem: a package with no way out of a
    # queue keeps every run reporting a change. The changed-version path above
    # is deliberately not filtered, because a new release is a new question and
    # answering it clears the record.
    unread_pkgs <- .analyzer_read_exhausted(con)
    # Also drain any packages whose rows predate the binary metrics, so a normal
    # scheduled run finishes the one-time backfill and then reverts to just the
    # changed packages once none remain.
    backfill <- .recollect_todo(con, universe$package,
                                c(perm_fail_pkgs, unread_pkgs))
    # And drain any package with a version row that predates all-versions detail
    # (detail_scanned IS NULL on ANY row). Re-analyzing fills per-version detail
    # and marks every row, so it converges; a package re-analyzed once is never
    # re-flagged, even if some versions produced zero functions. Not filtered by
    # the read attempts: this marker is written by the run itself under either
    # producer, so the queue drains without the analyzer.
    detail_backfill <- .recollect_todo(con, universe$package, perm_fail_pkgs,
                                        sentinel = "detail_scanned",
                                        latest_only = FALSE)
    # And drain any package whose latest-version row predates the dataset reader
    # (datasets_scanned IS NULL), so bioc_datasets fills in without a manual
    # recollect. Also latest-row-scoped, so it converges once re-analyzed.
    dataset_backfill <- .recollect_todo(
      con, universe$package, c(perm_fail_pkgs, unread_pkgs),
      sentinel = "datasets_scanned", latest_only = TRUE)
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
  shard_text_list      <- list()
  shard_failures       <- character(0L)
  shard_stages         <- character(0L)
  # Which of the rows about to be written the analyzer binary produced, keyed
  # by package and version. Only those get the running build stamped on them.
  shard_binary_keys    <- character(0L)

  if (!dir.exists(WORK_DIR)) dir.create(WORK_DIR, recursive = TRUE)

  # Worker: clone + analyze one package. No database access.
  # Returns list(package, ok = TRUE, elapsed, summary, churn, ...) or, when the
  # package failed, list(package, ok = FALSE, stage, elapsed, reason).
  .pkg_worker <- function(pkg) {
    .t0  <- Sys.time()
    .idx <- match(pkg, shard_pkgs)          # queue position; shard_pkgs is unique
    .n   <- length(shard_pkgs)
    .elapsed <- function() as.numeric(difftime(Sys.time(), .t0, units = "secs"))
    # Thinned per-worker completion line, emitted FROM the fork so it streams live
    # during the otherwise-silent parallel phase. Prints only on every 25th queue
    # position, every failure, every slow (>=30s) package, every package past
    # the cap, and the last position.
    # One fully-formed cat() to stdout, kept under PIPE_BUF by .worker_line:
    # forks reorder whole lines but never byte-interleave, and fd 1 is disjoint
    # from mclapply's result pipe. The emit is wrapped in try() so a
    # broken-stream write can never turn an ok package into a recorded failure.
    .done <- function(ok, stage, nver, el, reason = NULL) {
      if (isTRUE(ok) && .idx %% 25L != 0L && el < 30 && el < WORKER_TIMEOUT &&
          !identical(.idx, .n)) {
        return(invisible())
      }
      try({
        cat(.worker_line(.idx, .n, ok, pkg, stage, nver, el, reason),
            file = stdout())
        flush(stdout())
      }, silent = TRUE)
      invisible()
    }
    # A warning() inside the fork reaches nobody, so the reason rides out on the
    # line the failure prints and in the result, redacted first, since the
    # clone URL holds a token.
    .fail <- function(stage, reason) {
      el     <- .elapsed()
      reason <- .redact_reason(reason)
      .done(FALSE, stage, 0L, el, reason)
      list(package = pkg, ok = FALSE, stage = stage, elapsed = el, reason = reason)
    }
    dest <- file.path(WORK_DIR, pkg)
    on.exit(unlink(dest, recursive = TRUE, force = TRUE), add = TRUE)
    on.exit(setTimeLimit(), add = TRUE)
    setTimeLimit(elapsed = WORKER_TIMEOUT, transient = TRUE)
    ok <- tryCatch(io$clone(pkg, dest),
                   error = function(e) structure(FALSE, reason = conditionMessage(e)))
    if (!isTRUE(ok)) return(.fail(.clone_stage(ok), .clone_reason(ok)))
    err <- NULL
    res <- tryCatch(
      analyze_package(dest, pkg),
      error = function(e) {
        err <<- e
        NULL
      }
    )
    if (is.null(res)) {
      return(.fail(.classify_failure(err, .elapsed()),
                   if (is.null(err)) "analyze_package returned nothing"
                   else conditionMessage(err)))
    }
    el <- .elapsed()
    .done(TRUE, "ok", nrow(res$summary), el)
    list(package = pkg, ok = TRUE, elapsed = el,
         summary = res$summary, churn = res$churn, api = res$api,
         functions = res$functions, edges = res$edges, datasets = res$datasets,
         text = res$text, binary_versions = res$binary_versions)
  }

  results <- parallel::mclapply(shard_pkgs, .pkg_worker,
                                mc.cores       = ANALYSIS_CORES,
                                mc.preschedule = FALSE)

  # Collect results in input order (shard_pkgs is sorted, so DB is deterministic).
  # All DB writes happen here in the parent process.
  for (i in seq_along(results)) {
    r   <- results[[i]]
    pkg <- shard_pkgs[[i]]
    # mclapply returns NULL for a fork that died and a try-error for one that
    # raised outside the worker's handlers; neither printed its line.
    v <- .classify_result(r)
    if (!isTRUE(v$ok)) {
      if (isTRUE(v$from_parent)) {
        cat(.worker_line(i, length(shard_pkgs), FALSE, pkg, v$stage, 0L,
                         v$elapsed, v$reason), file = stdout())
        flush(stdout())
      }
      shard_failures <- c(shard_failures, pkg)
      shard_stages   <- c(shard_stages, v$stage)
      .record_failure(con, pkg, v$stage, analyzer_version, WORKER_TIMEOUT, run_id,
                      v$elapsed, v$reason, unname(lv_of[pkg]))
    } else {
      shard_summary_list[[pkg]]   <- r$summary
      shard_churn_list[[pkg]]     <- r$churn
      shard_api_list[[pkg]]       <- r$api
      shard_functions_list[[pkg]] <- r$functions
      shard_edges_list[[pkg]]     <- r$edges
      shard_datasets_list[[pkg]]  <- r$datasets
      shard_text_list[[pkg]]      <- r$text
      shard_binary_keys <- c(shard_binary_keys,
                             .analyzer_row_keys(pkg, r$binary_versions))
      .reset_failure(con, pkg)
      # Analysed, but was it read? A package the analyzer did not read carries
      # none of the fields the backfill queues wait on, and that attempt is
      # what eventually takes it out of them. A package that was read starts
      # over from nothing, so one bad run does not count against the next.
      if (.analyzer_read_package(r$summary)) {
        .clear_analyzer_read_attempts(con, pkg)
      } else {
        .record_analyzer_read_attempt(con, pkg, analyzer_version)
      }
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
  fresh_text      <- .bind_release_text(shard_text_list)

  # Which build scanned these rows is what the next run's staleness check reads,
  # and a scanned row that does not say reads as one an unknown build produced.
  # Recorded here, where the run knows which binary it had, on the rows the
  # analyzer actually produced and on no others.
  fresh_summary <- .stamp_analyzer_version(fresh_summary, analyzer_version,
                                           shard_binary_keys)

  if (length(fresh_pkgs) > 0L) {
    # Write dataset rows before the code summary stamps datasets_scanned = TRUE,
    # so a scanned code row always implies its dataset rows were written. The
    # dataset write is delete-then-insert (idempotent), so if the code write
    # fails afterwards the package stays on the to-do list and the next run
    # redoes both cleanly, rather than being marked done with datasets missing.
    upsert_datasets(data_con, fresh_datasets, fresh_pkgs)
    # Before the code rows, so a failed text write leaves these packages unmarked.
    upsert_release_text(text_con, fresh_text$description,
                        fresh_text$release_notes, fresh_text$versions)
    upsert_shard(con, fresh_summary, fresh_churn, fresh_api,
                 fresh_functions, fresh_edges,
                 description_df = fresh_text$description_latest,
                 release_notes_df = fresh_text$release_notes_latest,
                 analyzer_version = analyzer_version)
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

  prev_manifest <- tryCatch({
    prev_path <- file.path(out_dir, "prev-code-manifest.json")
    cur_path  <- file.path(out_dir, "code-manifest.json")
    src <- if (file.exists(prev_path)) prev_path else if (file.exists(cur_path)) cur_path else NULL
    if (is.null(src)) NULL else jsonlite::fromJSON(src)
  }, error = function(e) NULL)
  prior_fp <- prev_manifest[["fingerprint"]]

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
                    bootstrap_complete = bootstrap_complete,
                    n_datasets_unscanned = .n_datasets_unscanned(con),
                    n_datasets_unreadable = .n_datasets_unreadable(con),
                    # From the dataset database rather than this one: it is a
                    # count of catalog entries, not of packages.
                    n_datasets_unmeasured = .n_datasets_unmeasured(data_con),
                    analyzer_version = analyzer_version,
                    output_class = I(output_class),
                    n_latest_on_build = .n_latest_on_class(con, analyzer_version)[["on_class"]])

  # When this run moved nothing, the moment the data last moved is whatever the
  # previous manifest recorded. Carrying it forward is what lets last_checked
  # advance every run without pretending the data is newer than it is. NULL
  # (no previous manifest, or one predating these fields) means "now", which is
  # correct for a first run and honest for the changeover.
  last_changed <- if (changed) NULL else
    (prev_manifest[["last_changed"]] %||% prev_manifest[["generated_at"]])
  code_db_bytes <- as.numeric(file.info(db_path)$size %||% 0)
  data_db_bytes <- as.numeric(file.info(data_db_path)$size %||% 0)

  # What the dataset columns actually hold. A declared column that is NULL for
  # every row in the corpus is not an honest NA, it is a column nobody is
  # filling, and it reads to a viewer exactly like a fact that happens to be
  # unknown. Said in the run output, so the shard that produced it says so, and
  # counted in the manifest, so the finding outlives the log.
  #
  # Only the dataset side has this to report: the code summary's columns are
  # written from a fixed metric registry and every one of them has a group
  # behind it, where the dataset columns are a spec held against a reader that
  # keeps describing more.
  dataset_coverage <- dataset_column_coverage(data_con)
  dataset_alerts   <- dataset_coverage_alerts(dataset_coverage)
  if (length(dataset_alerts) > 0L) {
    shown <- head(dataset_alerts, 20L)
    cat(sprintf("dataset coverage: %d of %d declared columns hold nothing for anybody\n  %s\n%s",
                length(dataset_alerts), nrow(dataset_coverage),
                paste(shown, collapse = "\n  "),
                if (length(dataset_alerts) > length(shown))
                  sprintf("  ... and %d more\n", length(dataset_alerts) - length(shown)) else ""),
        file = stdout())
    flush(stdout())
  }

  code_manifest <- build_manifest(
    con, series = "code", repo = PUBLISH_REPO, db_filename = DB_FILENAME,
    db_bytes = code_db_bytes,
    tables = c("bioc_code_summary", "bioc_api_history", "bioc_functions",
               "bioc_call_edges", "bioc_code_churn"),
    fp_table = "bioc_code_summary", fp_cols = c("package", "version"),
    pkg_table = "bioc_code_summary", ver_table = "bioc_code_summary",
    stat_table = "bioc_code_summary", stat_cols = c("loc_r", "n_fns_r"),
    bootstrap = bootstrap, last_changed = last_changed)

  data_manifest <- build_manifest(
    data_con, series = "data", repo = PUBLISH_REPO, db_filename = DATA_DB_FILENAME,
    db_bytes = data_db_bytes,
    tables = c("bioc_datasets", "bioc_dataset_versions", "bioc_dataset_contents"),
    fp_table = "bioc_datasets", fp_cols = c("package", "name", "current_content_id"),
    pkg_table = "bioc_datasets", ver_table = "bioc_dataset_versions",
    stat_table = "bioc_dataset_contents", stat_cols = c("nrow", "ncol"),
    bootstrap = bootstrap, last_changed = last_changed,
    coverage = dataset_coverage)

  write_manifest(file.path(out_dir, "code-manifest.json"), code_manifest)
  write_manifest(file.path(out_dir, "data-manifest.json"), data_manifest)
  write_manifest(file.path(out_dir, "run-status.json"),
                 list(changed = changed, bootstrap_complete = bootstrap_complete,
                      n_analyzed = n_analyzed_pkgs, n_universe = n_universe,
                      n_remaining = length(remaining_after), n_fresh = length(fresh_pkgs),
                      n_shard = length(shard_pkgs),
                      n_versions = nrow(fresh_summary),
                      shard_failures = length(shard_failures),
                      analyzer_version = bootstrap$analyzer_version,
                      output_class = bootstrap$output_class,
                      n_latest_on_build = bootstrap$n_latest_on_build))

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
  source(file.path(.script_dir, "release_text.R"))

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
