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
#' @param analyzer_exit The analyzer's non-zero exits, from .analyzer_exit_text;
#'   "" leaves them off.
#' @return A single string ending in one newline.
.worker_line <- function(idx, n, ok, pkg, stage, nver, elapsed, reason = NULL,
                         worker_timeout = WORKER_TIMEOUT, analyzer_exit = "") {
  stem <- if (isTRUE(ok)) {
    sprintf("[%d/%d] ok %s: %d versions in %.1fs%s", idx, n, pkg, nver, elapsed,
            if (isTRUE(elapsed >= worker_timeout))
              sprintf(" (past the %ds cap)", as.integer(worker_timeout)) else "")
  } else if (is.na(elapsed)) {
    sprintf("[%d/%d] FAIL %s: %s", idx, n, pkg, stage)
  } else {
    sprintf("[%d/%d] FAIL %s: %s after %.1fs", idx, n, pkg, stage, elapsed)
  }
  if (nzchar(analyzer_exit)) stem <- sprintf("%s [analyzer exit %s]", stem, analyzer_exit)
  if (is.null(reason) || !nzchar(trimws(as.character(reason)))) {
    return(paste0(stem, "\n"))
  }
  # Two bytes for the ": " and one for the newline.
  room <- WORKER_LINE_MAX_BYTES - nchar(stem, type = "bytes") - 3L
  if (room <= 3L) return(paste0(stem, "\n"))
  paste0(stem, ": ", .clip_bytes(reason, room), "\n")
}

# The stage of an error analyze_package raised. An elapsed time at the cap also
# catches a cap swallowed earlier and a later failure. An analyzer that was
# killed, or failed on a version with analyzer rows, is a crash.
.classify_failure <- function(e, elapsed, worker_timeout = WORKER_TIMEOUT) {
  if (inherits(e, "extract_failure")) {
    return(if (isTRUE(e$status == 124L)) "git_timeout" else "extract")
  }
  if (inherits(e, "analyzer_parse_incomplete")) return("analyze")
  if (inherits(e, c("analyzer_killed", "analyzer_failed"))) return("crash")
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

# Packages that already failed in this run. Nothing outside a run, so tests and
# local runs keep today's one attempt per shard.
.tried_this_run <- function(con, run_id) {
  if (is.na(run_id) || !"bioc_metrics_failures" %in% DBI::dbListTables(con)) {
    return(character(0L))
  }
  as.character(DBI::dbGetQuery(con,
    "SELECT package FROM bioc_metrics_failures WHERE last_run_id = ?",
    params = list(run_id))$package)
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

# Keep the standing over-cap list. A pass past the cap ran uncapped after the cap
# fired somewhere, so it goes on the list; a pass under the cap rewrote every
# stored row without crossing, so it comes off. TRUE when the package is on it.
.note_over_cap <- function(con, pkg, elapsed, build, run_id,
                           worker_timeout = WORKER_TIMEOUT) {
  if (!isTRUE(elapsed >= worker_timeout)) {
    DBI::dbExecute(con, "DELETE FROM bioc_over_cap WHERE package = ?", params = list(pkg))
    return(FALSE)
  }
  DBI::dbExecute(con, "
    INSERT INTO bioc_over_cap (package, elapsed_s, analyzer_version, last_run_id, recorded_at)
    VALUES (?, ?, ?, ?, ?)
    ON CONFLICT(package) DO UPDATE SET
      elapsed_s = excluded.elapsed_s, analyzer_version = excluded.analyzer_version,
      last_run_id = excluded.last_run_id, recorded_at = excluded.recorded_at",
    params = list(pkg, as.numeric(elapsed), .build_key(build),
                  as.character(run_id %||% NA_character_),
                  format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")))
  TRUE
}

# The standing over-cap list, longest analysis first.
.over_cap_packages <- function(con) {
  if (!"bioc_over_cap" %in% DBI::dbListTables(con)) return(character(0L))
  as.character(DBI::dbGetQuery(con,
    "SELECT package FROM bioc_over_cap ORDER BY elapsed_s DESC, package")$package)
}

# The comma-separated words of an --unpark or --requeue value.
.split_spec <- function(spec) {
  if (is.null(spec) || length(spec) != 1L || is.na(spec)) return(character(0L))
  w <- trimws(strsplit(spec, ",", fixed = TRUE)[[1L]])
  unique(w[nzchar(w)])
}

# The words that name a package this pipeline has rows or a verdict for. Any
# other word is warned about and ignored.
.operator_packages <- function(con, words) {
  tables <- intersect(c("bioc_code_summary", "bioc_metrics_failures"),
                      DBI::dbListTables(con))
  known <- vapply(words, function(p) {
    grepl("^[A-Za-z][A-Za-z0-9.]*$", p) && any(vapply(tables, function(t) {
      nrow(DBI::dbGetQuery(con, sprintf("SELECT 1 FROM %s WHERE package = ? LIMIT 1", t),
                           params = list(p))) > 0L
    }, logical(1L)))
  }, logical(1L), USE.NAMES = FALSE)
  if (any(!known)) {
    warning(sprintf("ignoring %s: not a package with rows or a verdict here",
                    paste(words[!known], collapse = ", ")),
            call. = FALSE, immediate. = TRUE)
  }
  words[known]
}

# Zero the three counters and the run id on the rows `where` selects, and stamp
# unparked_at. A row is never deleted.
.release_verdicts <- function(con, where, params = list()) {
  DBI::dbExecute(con, sprintf(
    "UPDATE bioc_metrics_failures
        SET fetch_failures = 0, analyze_failures = 0, timeout_failures = 0,
            last_run_id = NULL, unparked_at = ?
      WHERE %s", where),
    params = c(list(format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")), params))
}

#' Release parked verdicts, for --unpark.
#'
#' @param spec "all", "fetch", "analyze" or "timeout" (the class of a row's last
#'   stage), or package names separated by commas.
#' @return The number of rows released.
.unpark <- function(con, spec) {
  words <- .split_spec(spec)
  if (length(words) == 0L) return(0L)
  stages <- list(fetch = c("clone", "extract"), analyze = "analyze",
                 timeout = c("timeout", "crash", "git_timeout"))
  if (identical(words, "all")) return(.release_verdicts(con, "1 = 1"))
  if (length(words) == 1L && words %in% names(stages)) {
    st <- stages[[words]]
    return(.release_verdicts(con, sprintf("stage IN (%s)",
                                          paste(rep("?", length(st)), collapse = ", ")),
                             as.list(st)))
  }
  pkgs <- .operator_packages(con, words)
  if (length(pkgs) == 0L) return(0L)
  .release_verdicts(con, sprintf("package IN (%s)",
                                 paste(rep("?", length(pkgs)), collapse = ", ")),
                    as.list(pkgs))
}

#' Analyse packages again from scratch, for --requeue.
#'
#' Releases their verdicts, forgets their read attempts and clears
#' datasets_scanned on their latest row, so this run's dataset backfill takes
#' them and keeps them until they pass. Their stored rows stay as they are.
#'
#' @param spec Package names separated by commas; over_cap names the standing
#'   over-cap list.
#' @return The packages requeued.
.requeue <- function(con, spec) {
  words <- .split_spec(spec)
  pkgs  <- unique(c(if ("over_cap" %in% words) .over_cap_packages(con),
                    .operator_packages(con, setdiff(words, "over_cap"))))
  if (length(pkgs) == 0L) return(character(0L))
  ph <- paste(rep("?", length(pkgs)), collapse = ", ")
  .release_verdicts(con, sprintf("package IN (%s)", ph), as.list(pkgs))
  tables <- DBI::dbListTables(con)
  if ("bioc_analyzer_read_attempts" %in% tables) {
    DBI::dbExecute(con, sprintf(
      "DELETE FROM bioc_analyzer_read_attempts WHERE package IN (%s)", ph),
      params = as.list(pkgs))
  }
  if ("bioc_code_summary" %in% tables &&
      all(c("datasets_scanned", "latest_release_date") %in%
          DBI::dbListFields(con, "bioc_code_summary"))) {
    DBI::dbExecute(con, sprintf(
      "UPDATE bioc_code_summary SET datasets_scanned = NULL
        WHERE latest_release_date IS NOT NULL AND package IN (%s)", ph),
      params = as.list(pkgs))
  }
  pkgs
}

# Parked verdicts by class, and the rows from before stages were kept.
.parked_counts <- function(st) {
  list(fetch   = sum(st$class %in% "fetch"),
       analyze = sum(st$class %in% "analyze"),
       timeout = sum(st$class %in% "timeout"),
       legacy  = sum(is.na(st$stage)))
}

# Failures by stage, in the order a package meets the stages; absent ones left out.
.stage_counts <- function(stages) {
  order <- c("clone", "extract", "git_timeout", "analyze", "timeout", "crash")
  n <- vapply(order, function(x) sum(stages == x), integer(1L))
  as.list(n[n > 0L])
}

# The standing over-cap list for a manifest: its size and the first 20 names.
.over_cap_block <- function(con) {
  p <- .over_cap_packages(con)
  list(count = length(p), packages = I(utils::head(p, 20L)))
}

# How many latest rows each analyzer build wrote; "none" for the R fallback.
.latest_by_build <- function(con) {
  empty <- stats::setNames(list(), character(0L))
  if (!"bioc_code_summary" %in% DBI::dbListTables(con)) return(empty)
  fields <- DBI::dbListFields(con, "bioc_code_summary")
  if (!"latest_release_date" %in% fields) return(empty)
  build <- if ("analyzer_version" %in% fields) {
    "IFNULL(NULLIF(analyzer_version, ''), 'none')"
  } else {
    "'none'"
  }
  df <- DBI::dbGetQuery(con, sprintf(
    "SELECT %s AS build, COUNT(DISTINCT package) AS n FROM bioc_code_summary
      WHERE latest_release_date IS NOT NULL GROUP BY 1 ORDER BY 1", build))
  stats::setNames(as.list(as.integer(df$n)), df$build)
}

# The shard plan's verdict line.
.verdict_plan_line <- function(build, n_released, st, n_tried) {
  p <- .parked_counts(st)
  b <- .build_key(build)
  sprintf(paste0("analyzer %s; verdicts released: %d; parked: fetch %d, analyze %d, ",
                 "timeout %d, legacy %d; skipped as tried this run: %d; ",
                 "fetch rechecks due: %d\n"),
          if (nzchar(b)) b else "none", as.integer(n_released), p$fetch, p$analyze,
          p$timeout, p$legacy, as.integer(n_tried), sum(st$recheck_due))
}

# The shard plan's line on the analyzer's address-space limit: limit_mb is the
# limit in force and build the analyzer build the run read.
.analyzer_limit_line <- function(limit_mb, build, configured_mb = ANALYZER_MEMORY_LIMIT_MB) {
  if (limit_mb > 0L) {
    return(sprintf("analyzer memory limit: %d MiB of address space for each analyzer\n",
                   limit_mb))
  }
  b   <- .build_key(build)
  why <- if (!isTRUE(configured_mb > 0L)) {
    ""
  } else if (!nzchar(b)) {
    ", no rpkg-analyzer version was read"
  } else if (!analyzer_at_least(b, "0.5.2")) {
    sprintf(", rpkg-analyzer %s is older than 0.5.2", b)
  } else {
    ", prlimit was not found"
  }
  sprintf("analyzer memory limit: none%s (ANALYZER_MEMORY_LIMIT_MB is %d)\n", why,
          as.integer(configured_mb))
}

# The shard receipt's verdict line.
.verdict_receipt_line <- function(stages, over_cap, n_standing) {
  by <- .stage_counts(stages)
  sprintf("shard verdicts: %d failed%s; passed over the cap: %d%s; standing over-cap list: %d\n",
          length(stages),
          if (length(by)) sprintf(" (%s)", paste(names(by), unlist(by), collapse = ", ")) else "",
          length(over_cap),
          if (length(over_cap)) sprintf(" (%s)", paste(utils::head(over_cap, 20L),
                                                        collapse = ", ")) else "",
          as.integer(n_standing))
}

# Delete a package's failure record (reset after a successful analysis).
.reset_failure <- function(con, pkg) {
  DBI::dbExecute(con,
    "DELETE FROM bioc_metrics_failures WHERE package = ?",
    params = list(pkg))
  invisible(NULL)
}

#' Every failure verdict, and whether it parks its package.
#'
#' A verdict parks by build when this build failed the package
#' MAX_CLONE_FAILURES times at analyze, or MAX_TIMEOUT_FAILURES times at a
#' timeout under this cap. It parks by fetch when the package failed to fetch
#' MAX_CLONE_FAILURES times at its current latest_version (the Bioconductor
#' release), unless it has no stored rows and its last attempt is
#' FETCH_RECHECK_DAYS old. A row from before stages were kept (stage NULL)
#' parks nothing.
#'
#' @param universe data.frame(package, latest_version).
#' @return data.frame(package, stage, class, parked, recheck_due); class is
#'   "fetch", "analyze" or "timeout" for a parked package and NA otherwise.
.verdict_state <- function(con, build, worker_timeout, universe, now = Sys.time()) {
  tables <- DBI::dbListTables(con)
  if (!"bioc_metrics_failures" %in% tables) {
    return(data.frame(package = character(0L), stage = character(0L),
                      class = character(0L), parked = logical(0L),
                      recheck_due = logical(0L), stringsAsFactors = FALSE))
  }
  has_rows <- if ("bioc_code_summary" %in% tables) {
    "EXISTS (SELECT 1 FROM bioc_code_summary s WHERE s.package = f.package)"
  } else {
    "0"
  }
  df <- DBI::dbGetQuery(con, sprintf("
    SELECT f.package, f.stage, f.fetch_failures, f.fetch_version, f.last_attempt,
           %s AS has_rows,
           (IFNULL(f.analyzer_version, '') = :build
              AND f.analyze_failures >= :max_fail) AS by_analyze,
           (IFNULL(f.analyzer_version, '') = :build AND f.worker_timeout = :wt
              AND f.timeout_failures >= :max_timeouts) AS by_timeout
      FROM bioc_metrics_failures f
     ORDER BY f.package", has_rows),
    params = list(build = .build_key(build), wt = as.integer(worker_timeout),
                  max_fail = MAX_CLONE_FAILURES, max_timeouts = MAX_TIMEOUT_FAILURES))
  live <- !is.na(df$stage)
  lv   <- as.character(universe$latest_version)[
    match(df$package, as.character(universe$package))]
  same_release <- ifelse(is.na(df$fetch_version), "", df$fetch_version) ==
    ifelse(is.na(lv), "", lv)
  age <- as.numeric(difftime(
    now, as.POSIXct(df$last_attempt, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    units = "days"))
  fetch_capped <- live & df$fetch_failures >= MAX_CLONE_FAILURES & same_release
  recheck_due  <- fetch_capped & df$has_rows %in% 0L & !is.na(age) &
    age >= FETCH_RECHECK_DAYS
  class <- ifelse(live & df$by_analyze %in% 1L, "analyze",
           ifelse(live & df$by_timeout %in% 1L, "timeout",
           ifelse(fetch_capped & !recheck_due, "fetch", NA_character_)))
  data.frame(package = df$package, stage = df$stage, class = class,
             parked = !is.na(class), recheck_due = recheck_due,
             stringsAsFactors = FALSE)
}

# Packages whose verdict parks them, left out of every queue.
.permanent_failures <- function(con, build, worker_timeout, universe,
                                now = Sys.time()) {
  st <- .verdict_state(con, build, worker_timeout, universe, now)
  st$package[st$parked]
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

# The versions of each package whose stored row names an analyzer build, any
# build. A named list with an entry, possibly empty, for every package asked.
.stamped_versions <- function(con, pkgs) {
  pkgs <- as.character(pkgs)
  out  <- stats::setNames(rep(list(character(0L)), length(pkgs)), pkgs)
  if (!SUMMARY_TABLE %in% DBI::dbListTables(con)) return(out)
  if (!"analyzer_version" %in% DBI::dbListFields(con, SUMMARY_TABLE)) return(out)
  sql <- sprintf('SELECT version FROM "%s" WHERE package = ?
                     AND analyzer_version IS NOT NULL AND analyzer_version != \'\'
                   ORDER BY version', SUMMARY_TABLE)
  for (p in pkgs) {
    out[[p]] <- as.character(DBI::dbGetQuery(con, sql, params = list(p))$version)
  }
  out
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
# Worker telemetry: each package's analyzer directory and phase times
# ---------------------------------------------------------------------------

# Whether RPA_CACHE turns the analyzer's cache off; every spelling of "no" counts.
.analyzer_cache_off <- function(value = Sys.getenv("RPA_CACHE", unset = "")) {
  tolower(trimws(value)) %in% c("off", "false", "no", "0")
}

# Put environment variables back as Sys.getenv(names, unset = NA) read them.
.restore_envvars <- function(old) {
  for (nm in names(old)) {
    if (is.na(old[[nm]])) Sys.unsetenv(nm)
    else do.call(Sys.setenv, stats::setNames(list(old[[nm]]), nm))
  }
  invisible(NULL)
}

# f, adding its elapsed seconds to the worker tally under `name`; f's value,
# attributes included, is unchanged.
.timed_phase <- function(name, f) {
  force(f)
  function(...) {
    t0 <- proc.time()[["elapsed"]]
    on.exit(.tally_add(name, .secs_since(t0)), add = TRUE)
    f(...)
  }
}

# A kB figure of this process from /proc/self/status, such as VmHWM, its peak
# resident size. NA where that file does not exist, which is anywhere but Linux.
.proc_status_kb <- function(key, path = "/proc/self/status") {
  if (!file.exists(path)) return(NA_real_)
  lines <- tryCatch(readLines(path, warn = FALSE), error = function(e) character(0L))
  hit   <- grep(sprintf("^%s:", key), lines, value = TRUE)
  if (length(hit) == 0L) return(NA_real_)
  suppressWarnings(as.numeric(sub("^[^:]*:[[:space:]]*([0-9]+).*$", "\\1", hit[[1L]])))
}

# worker, run in WORK_DIR/.rpa/<pkg> (no clone can land there): RPKG_ANALYZER_STATS
# names a file and, unless RPA_CACHE is off, RPKG_ANALYZER_CACHE_DIR a cache; builds
# before 0.5.1 read neither. A list result gains `tally`, `analyzer_stats` and
# `memory`: the worker's peak resident size and the in-memory size of the result.
.with_worker_telemetry <- function(worker) {
  force(worker)
  function(pkg) {
    t0 <- proc.time()[["elapsed"]]
    .tally_reset()
    rpa   <- file.path(WORK_DIR, ".rpa", pkg)
    stats <- file.path(rpa, "stats.ndjson")
    old   <- Sys.getenv(c("RPKG_ANALYZER_CACHE_DIR", "RPKG_ANALYZER_STATS"),
                        unset = NA_character_, names = TRUE)
    on.exit({
      .restore_envvars(old)
      unlink(rpa, recursive = TRUE, force = TRUE)
    }, add = TRUE)
    Sys.unsetenv(c("RPKG_ANALYZER_CACHE_DIR", "RPKG_ANALYZER_STATS"))
    dir.create(file.path(rpa, "cache"), recursive = TRUE, showWarnings = FALSE)
    if (dir.exists(rpa)) {
      rpa   <- normalizePath(rpa)
      stats <- file.path(rpa, "stats.ndjson")
      Sys.setenv(RPKG_ANALYZER_STATS = stats)
      if (!.analyzer_cache_off()) Sys.setenv(RPKG_ANALYZER_CACHE_DIR = file.path(rpa, "cache"))
    }
    res <- worker(pkg)
    .tally_add("package_s", .secs_since(t0))
    if (is.list(res)) {
      # Read before the result is serialised for the parent, so that copy is not in it.
      res$memory <- list(peak_rss_kb = .proc_status_kb("VmHWM"),
                         result_bytes = as.numeric(utils::object.size(res)))
      res$tally <- .tally_snapshot()
      res$analyzer_stats <- if (file.exists(stats)) readLines(stats, warn = FALSE) else character(0L)
    }
    res
  }
}

# The largest of x and the package beside it; both NA when x holds no figure.
.largest <- function(x, packages) {
  if (length(x) == 0L || all(is.na(x))) return(list(value = NA, package = NA))
  i <- which.max(x)
  list(value = x[[i]], package = packages[[i]])
}

# The memory figures of a shard's statistics lines, each line's package beside
# it: the largest resident and virtual peak with their packages, the most data
# one file kept, the packages with a file over the data budget, and the five
# packages with the largest resident peak. A figure no line carried is NA.
.analyzer_memory <- function(packages, rss, vm, kept, over) {
  top_rss <- .largest(rss, packages)
  top_vm  <- .largest(vm, packages)
  of <- function(x, p) {
    v <- x[packages %in% p & !is.na(x)]
    if (length(v)) max(v) else NA
  }
  peaks <- lapply(unique(packages[!is.na(packages) & !is.na(rss)]), function(p) {
    list(package = p, peak_rss_kb = of(rss, p), peak_vm_kb = of(vm, p))
  })
  peaks <- peaks[order(-vapply(peaks, function(p) p$peak_rss_kb, numeric(1L)))]
  list(peak_rss_kb = top_rss$value, peak_rss_package = top_rss$package,
       peak_vm_kb = top_vm$value, peak_vm_package = top_vm$package,
       data_kept_max = .largest(kept, packages)$value,
       data_over_budget = I(sort(unique(packages[!is.na(packages) & over %in% TRUE]))),
       peaks = utils::head(peaks, 5L))
}

# The analyzer's statistics lines (RPKG_ANALYZER_STATS, 0.5.1 and later) summed
# over a shard; a line that does not parse is counted as unreadable. `packages`
# names the package of each line, for the memory keys 0.5.2 and later write:
# peak_rss_kb, peak_vm_kb, data_kept_max and data_over_budget. Absent or null,
# they add nothing.
.sum_analyzer_stats <- function(lines, packages = rep(NA_character_, length(lines))) {
  out <- list(runs = 0L, unreadable = 0L, builds = "",
              ms = 0, ms_compiled = 0, ms_r = 0, ms_tests = 0, ms_data = 0, ms_other = 0,
              compiled_files = 0, compiled_hits = 0, r_files = 0, tests_files = 0,
              data_files = 0, cache_errors = 0, verify_mismatch = 0)
  num <- function(x) if (is.numeric(x) && length(x) == 1L && !is.na(x)) x else 0
  figure <- function(x) {
    if (is.numeric(x) && length(x) == 1L && !is.na(x)) as.numeric(x) else NA_real_
  }
  builds <- character(0L)
  rss <- vm <- kept <- rep(NA_real_, length(lines))
  over <- rep(NA, length(lines))
  for (i in seq_along(lines)) {
    s <- tryCatch(jsonlite::parse_json(lines[[i]]), error = function(e) NULL)
    if (!is.list(s) || !is.numeric(s$ms)) {
      out$unreadable <- out$unreadable + 1L
      next
    }
    out$runs <- out$runs + 1L
    if (is.character(s$build) && length(s$build) == 1L) builds <- c(builds, s$build)
    for (k in c("ms", "ms_compiled", "ms_r", "ms_tests", "ms_data", "ms_other",
                "cache_errors", "verify_mismatch")) {
      out[[k]] <- out[[k]] + num(s[[k]])
    }
    for (kind in c("compiled", "r", "tests", "data")) {
      out[[paste0(kind, "_files")]] <- out[[paste0(kind, "_files")]] + num(s[[kind]]$files)
    }
    out$compiled_hits <- out$compiled_hits + num(s$compiled$hits)
    rss[[i]]  <- figure(s$peak_rss_kb)
    vm[[i]]   <- figure(s$peak_vm_kb)
    kept[[i]] <- figure(s$data_kept_max)
    over[[i]] <- figure(s$data_over_budget) > 0
  }
  out$builds <- paste(sort(unique(builds)), collapse = " ")
  c(out, .analyzer_memory(as.character(packages), rss, vm, kept, over))
}

# The largest worker peak and the largest result of a shard, each with its
# package. The peak is NA off Linux, and both are when no worker returned.
.worker_memory <- function(rs) {
  pkgs <- vapply(rs, function(r) as.character(r$package %||% NA_character_), character(1L))
  of   <- function(key) {
    vapply(rs, function(r) as.numeric(r$memory[[key]] %||% NA_real_), numeric(1L))
  }
  peak   <- .largest(of("peak_rss_kb"), pkgs)
  result <- .largest(of("result_bytes"), pkgs)
  list(peak_rss_kb = peak$value, peak_rss_package = peak$package,
       result_bytes = result$value, result_package = result$package)
}

# A shard's analyzer statistics, worker time by phase and worker memory, from
# the worker results; a fork that crashed returned no list and adds nothing.
.shard_telemetry <- function(results) {
  rs <- Filter(is.list, results)
  tally <- function(name) {
    sum(vapply(rs, function(r) as.numeric(r$tally[[name]] %||% 0), numeric(1L)))
  }
  analyzer <- .sum_analyzer_stats(
    unlist(lapply(rs, function(r) r$analyzer_stats), use.names = FALSE),
    unlist(lapply(rs, function(r) {
      rep(as.character(r$package %||% NA_character_), length(r$analyzer_stats))
    }), use.names = FALSE))
  analyzer$incomplete_parses <- as.integer(tally("incomplete_parses"))
  versions <- tally("versions_s")
  phases <- list(
    packages   = length(rs),
    clone_s    = tally("clone_s"),
    extract_s  = tally("extract_s"),
    analyzer_s = tally("analyzer_s"),
    parse_s    = tally("parse_s"),
    metrics_s  = max(0, versions - tally("extract_s") - tally("analyzer_s") - tally("parse_s")),
    other_s    = max(0, tally("package_s") - tally("clone_s") - versions),
    dataset_memo_hits   = as.integer(tally("memo_hits")),
    dataset_memo_misses = as.integer(tally("memo_misses")))
  list(analyzer = analyzer, phases = phases, workers = .worker_memory(rs))
}

# The shard's analyzer line for the run log.
.analyzer_stats_line <- function(a) {
  if (a$runs == 0L) {
    return(sprintf("analyzer: no statistics from this build; incomplete parses %d",
                   a$incomplete_parses))
  }
  sprintf(paste0("analyzer: %d versions in %.0f s; compiled %.0f files (%.1f%% reused); ",
                 "cache errors %.0f; verify mismatches %.0f; incomplete parses %d"),
          a$runs, a$ms / 1000, a$compiled_files,
          if (a$compiled_files > 0) 100 * a$compiled_hits / a$compiled_files else 0,
          a$cache_errors, a$verify_mismatch, a$incomplete_parses)
}

# The shard's worker time by phase for the run log.
.worker_phase_line <- function(p) {
  sprintf(paste0("worker time: clone %.1f s, extract %.1f s, analyzer %.1f s, ",
                 "record parse %.1f s, metrics %.1f s, other %.1f s"),
          p$clone_s, p$extract_s, p$analyzer_s, p$parse_s, p$metrics_s, p$other_s)
}

# kB as text in MiB.
.mib <- function(kb) sprintf("%.1f MiB", kb / 1024)

# Whether a memory figure was reported.
.has_figure <- function(x) length(x) == 1L && !is.na(x)

# The shard's analyzer memory line for the run log. The peaks are in kB and
# come from Linux alone; the data figures are bytes and the same everywhere.
.analyzer_memory_line <- function(a) {
  peaks <- c(
    if (.has_figure(a$peak_rss_kb)) {
      sprintf("peak resident %s (%s)", .mib(a$peak_rss_kb), a$peak_rss_package %||% "?")
    },
    if (.has_figure(a$peak_vm_kb)) {
      sprintf("peak virtual %s (%s)", .mib(a$peak_vm_kb), a$peak_vm_package %||% "?")
    })
  data <- if (.has_figure(a$data_kept_max)) {
    over <- as.character(a$data_over_budget)
    c(sprintf("largest data kept %s", .mib(a$data_kept_max / 1024)),
      sprintf("over the data budget: %s",
              if (length(over)) paste(utils::head(over, 20L), collapse = " ") else "none"))
  }
  if (is.null(peaks) && is.null(data)) {
    return("analyzer memory: no memory figures from this build")
  }
  paste0("analyzer memory: ",
         paste(c(peaks %||% "no peak figures on this platform", data), collapse = ", "))
}

# The shard's worker memory line for the run log.
.worker_memory_line <- function(w) {
  peak <- if (.has_figure(w$peak_rss_kb)) {
    sprintf("peak resident %s (%s)", .mib(w$peak_rss_kb), w$peak_rss_package %||% "?")
  }
  result <- if (.has_figure(w$result_bytes)) {
    sprintf("largest result %s (%s)", .mib(w$result_bytes / 1024), w$result_package %||% "?")
  }
  if (is.null(peak) && is.null(result)) return("worker memory: no memory figures")
  paste0("worker memory: ",
         paste(c(peak %||% "no peak figure on this platform", result), collapse = ", "))
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
#' Clone and analyze failures are tracked per package with their stage. A
#' package whose verdict parks it (.permanent_failures) is left out of the
#' to-do list and counted in the manifest permanent_failures field until the
#' analyzer build, the Bioconductor release or WORKER_TIMEOUT changes, or an
#' operator releases it.
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
#' @param unpark  --unpark: verdicts to release before any queue is read (see
#'   .unpark). NULL releases nothing.
#' @param requeue --requeue: packages to analyse again from scratch (see
#'   .requeue). NULL requeues nothing.
#' @return Manifest list (invisibly).
run_update <- function(io, out_dir, shard_size = SHARD_SIZE, force_full = FALSE,
                       recollect = FALSE, unpark = NULL, requeue = NULL) {
  # Without the analyzer binary the run still completes and still writes rows,
  # and it makes no progress: the per-package metrics, the per-function detail
  # and every dataset row come from the binary alone, so each package analysed
  # stays in the backfill pool and the next run selects the same shard again.
  # The bootstrap never advances and nothing else in the output says so, which
  # is a silent stall rather than a failure. A package with analyzer rows
  # fails instead, so those rows stay.
  if (!nzchar(rpkg_analyzer_bin())) {
    warning("rpkg-analyzer not found: per-package detail and dataset rows will ",
            "not be written, the backfill pool will not drain, the shard will ",
            "not advance between runs, and a package with analyzer rows will ",
            "fail. Set RPKG_ANALYZER_BIN or install the binary.",
            call. = FALSE, immediate. = TRUE)
  }

  # Read once, and use the same answer for both halves of the re-scan queue:
  # the build the stored rows are compared against, and the build stamped on
  # the rows this shard writes. Asking twice would let a binary swapped
  # mid-run clear markers it then never restores.
  analyzer_version <- rpkg_analyzer_version()
  # The address-space limit of every analyzer this run starts, the
  # self-check's included, worked out once from that build.
  analyzer_limit <- .analyzer_limit(analyzer_version)
  # A build that rejected the flag would exit 2 on every package: a crash for
  # each one with analyzer rows and the R fallback for the rest. So a 0.5.0
  # build proves it reads the flag first.
  if (analyzer_at_least(analyzer_version, "0.5.0") &&
      !rpkg_analyzer_selfcheck(ANALYZER_INPUT_KIND, analyzer_limit)) {
    limit_mb <- analyzer_limit$limit_mb
    stop(sprintf(paste0(
      "rpkg-analyzer %s did not answer --input-kind %s with a summary naming it%s; ",
      "stopping before any shard"), analyzer_version, ANALYZER_INPUT_KIND,
      if (limit_mb > 0L) sprintf(" under its %d MiB address-space limit", limit_mb) else ""),
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

  # ---- 1b. Operator releases, before any queue is read -----------------------
  n_released <- .unpark(con, unpark)
  if (n_released > 0L) message(sprintf("verdicts released by --unpark: %d", n_released))
  requeued <- .requeue(con, requeue)
  if (length(requeued) > 0L) {
    message(sprintf("requeued %d packages: %s", length(requeued),
                    paste(requeued, collapse = ", ")))
  }
  n_released <- n_released + length(requeued)

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
  verdicts       <- .verdict_state(con, analyzer_version, WORKER_TIMEOUT, universe)
  perm_fail_pkgs <- verdicts$package[verdicts$parked]
  recheck_pkgs   <- verdicts$package[verdicts$recheck_due]
  run_id <- .current_run_id()
  lv_of  <- stats::setNames(as.character(universe$latest_version),
                            as.character(universe$package))
  # A package that failed earlier in this run waits for the next one, at every
  # stage, so a failing package costs one attempt and one publish per run.
  tried_pkgs <- setdiff(.tried_this_run(con, run_id), perm_fail_pkgs)
  skip_pkgs  <- c(perm_fail_pkgs, tried_pkgs)

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
      universe$package[!universe$package %in% skip_pkgs]
    ))
  } else if (isTRUE(recollect)) {
    # Backfill: only packages whose rows predate the binary metrics. No wipe;
    # upsert_shard replaces each package's rows in place.
    todo_pkgs <- .recollect_todo(con, universe$package, skip_pkgs)
  } else {
    is_todo <- vapply(seq_len(n_universe), function(i) {
      pkg <- as.character(universe$package[i])
      if (pkg %in% skip_pkgs) return(FALSE)  # parked, or failed this run
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
                                c(skip_pkgs, unread_pkgs))
    # And drain any package with a version row that predates all-versions detail
    # (detail_scanned IS NULL on ANY row). Re-analyzing fills per-version detail
    # and marks every row, so it converges; a package re-analyzed once is never
    # re-flagged, even if some versions produced zero functions. Not filtered by
    # the read attempts: this marker is written by the run itself under either
    # producer, so the queue drains without the analyzer.
    detail_backfill <- .recollect_todo(con, universe$package, skip_pkgs,
                                        sentinel = "detail_scanned",
                                        latest_only = FALSE)
    # And drain any package whose latest-version row predates the dataset reader
    # (datasets_scanned IS NULL), so bioc_datasets fills in without a manual
    # recollect. Also latest-row-scoped, so it converges once re-analyzed.
    dataset_backfill <- .recollect_todo(
      con, universe$package, c(skip_pkgs, unread_pkgs),
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
  cat(.verdict_plan_line(analyzer_version, n_released, verdicts, length(tried_pkgs)),
      file = stdout())
  cat(.analyzer_limit_line(analyzer_limit$limit_mb, analyzer_version), file = stdout())
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
  # Verdicts this shard wrote. Each is news the next run needs, so the shard
  # publishes; a weekly recheck that failed where it failed before is not.
  n_verdicts_written   <- 0L
  shard_over_cap       <- character(0L)
  # Which of the rows about to be written the analyzer binary produced, keyed
  # by package and version. Only those get the running build stamped on them.
  shard_binary_keys    <- character(0L)

  if (!dir.exists(WORK_DIR)) dir.create(WORK_DIR, recursive = TRUE)

  # Each clone's seconds go to the worker tally; its value and status do not change.
  io$clone <- .timed_phase("clone_s", io$clone)

  # Read here because a worker cannot: an analyzer failure on one of these
  # versions fails its package instead of replacing the stored row.
  shard_stamped <- .stamped_versions(con, shard_pkgs)

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
    # the cap, every package whose analyzer exited non-zero, and the last position.
    # One fully-formed cat() to stdout, kept under PIPE_BUF by .worker_line:
    # forks reorder whole lines but never byte-interleave, and fd 1 is disjoint
    # from mclapply's result pipe. The emit is wrapped in try() so a
    # broken-stream write can never turn an ok package into a recorded failure.
    .done <- function(ok, stage, nver, el, reason = NULL) {
      exits <- .analyzer_exit_text()
      if (isTRUE(ok) && .idx %% 25L != 0L && el < 30 && el < WORKER_TIMEOUT &&
          !identical(.idx, .n) && !nzchar(exits)) {
        return(invisible())
      }
      try({
        cat(.worker_line(.idx, .n, ok, pkg, stage, nver, el, reason,
                         analyzer_exit = exits),
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
      analyze_package(dest, pkg, stamped = shard_stamped[[pkg]], limit = analyzer_limit),
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

  results <- parallel::mclapply(shard_pkgs, .with_worker_telemetry(.pkg_worker),
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
      same_recheck <- pkg %in% recheck_pkgs &&
        identical(verdicts$stage[match(pkg, verdicts$package)], v$stage)
      if (!same_recheck) n_verdicts_written <- n_verdicts_written + 1L
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
      if (.note_over_cap(con, pkg, v$elapsed, analyzer_version, run_id)) {
        shard_over_cap <- c(shard_over_cap, pkg)
      }
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

  # Re-read the verdicts after this shard (some may have just parked).
  verdicts_after       <- .verdict_state(con, analyzer_version, WORKER_TIMEOUT, universe)
  n_permanent_failures <- sum(verdicts_after$parked)
  verdict_counts <- list(
    parked            = .parked_counts(verdicts_after),
    failed_this_run   = if (is.na(run_id)) length(shard_failures)
                        else length(.tried_this_run(con, run_id)),
    over_cap_ok       = .over_cap_block(con),
    over_cap_this_run = if (is.na(run_id)) length(shard_over_cap)
                        else as.integer(DBI::dbGetQuery(con,
                          "SELECT COUNT(*) n FROM bioc_over_cap WHERE last_run_id = ?",
                          params = list(run_id))$n))

  # The manifest a series had before this shard: the prior release's when the
  # workflow fetched one, else the one already in out_dir.
  prior_manifest <- function(name) tryCatch({
    prev_path <- file.path(out_dir, paste0("prev-", name))
    cur_path  <- file.path(out_dir, name)
    src <- if (file.exists(prev_path)) prev_path else if (file.exists(cur_path)) cur_path else NULL
    if (is.null(src)) NULL else jsonlite::fromJSON(src)
  }, error = function(e) NULL)
  prev_manifest      <- prior_manifest("code-manifest.json")
  prev_data_manifest <- prior_manifest("data-manifest.json")
  prior_fp <- prev_manifest[["fingerprint"]]

  # bootstrap_complete: no deferred packages remain AND DB covers the universe
  # minus permanently-failed packages.
  remaining_after    <- setdiff(todo_pkgs, shard_pkgs)
  bootstrap_complete <- length(remaining_after) == 0L &&
    n_analyzed_pkgs >= (n_universe - n_permanent_failures)

  # data_moved: something substantive happened OR the content hash shifted.
  # changed adds a verdict written or released, which only persists if the
  # shard publishes, but moves no data, so last_changed keys on data_moved.
  data_moved <- isTRUE(force_full) ||
    length(fresh_pkgs) > 0L ||
    !identical(prior_fp, new_fp)
  changed <- data_moved || n_verdicts_written > 0L || n_released > 0L

  # The shard's one clock read. Both manifests and the list returned carry it:
  # a read for each could land either side of a second.
  shard_now <- Sys.time()

  manifest <- list(
    generated_at         = format(shard_now, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
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

  # ---- 8a. Analyzer statistics, worker time and memory ----------------------
  telemetry <- .shard_telemetry(results)
  cat(.analyzer_stats_line(telemetry$analyzer), "\n",
      .worker_phase_line(telemetry$phases), "\n",
      .analyzer_memory_line(telemetry$analyzer), "\n",
      .worker_memory_line(telemetry$workers), "\n", sep = "", file = stdout())

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
  cat(.verdict_receipt_line(shard_stages, shard_over_cap,
                            verdict_counts$over_cap_ok$count), file = stdout())
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
                    n_latest_on_build = .n_latest_on_class(con, analyzer_version)[["on_class"]],
                    parked = verdict_counts$parked,
                    failed_this_run = verdict_counts$failed_this_run,
                    over_cap_ok = verdict_counts$over_cap_ok,
                    over_cap_this_run = verdict_counts$over_cap_this_run)

  # When this shard moved nothing, the moment the data last moved is whatever an
  # earlier shard of this run recorded, or else what the previous manifest
  # recorded. Carrying it forward is what lets last_checked advance every run
  # without pretending the data is newer than it is. The workflow fetches the
  # previous manifest once per run, so a later shard reading only that would
  # date the data before an earlier shard moved it. run-status.json counts only
  # when it carries this run's id: out/ can hold one left by another run. NULL
  # (no previous manifest, or one predating these fields) means "now", which is
  # correct for a first run and honest for the changeover.
  #
  # Each manifest carries its own time forward. The two published before a
  # shard read the clock once can sit a second apart, and one written into the
  # other would move that manifest's last_changed. A data manifest with no time
  # of its own to carry takes the code manifest's.
  this_run <- tryCatch(jsonlite::fromJSON(file.path(out_dir, "run-status.json")),
                       error = function(e) NULL)
  same_run <- !is.na(run_id) && identical(this_run[["run_id"]], run_id)
  carried <- function(prev, status_key) if (data_moved) NULL else
    ((if (same_run) this_run[[status_key]]) %||%
       prev[["last_changed"]] %||% prev[["generated_at"]])
  code_last_changed <- carried(prev_manifest, "last_changed")
  data_last_changed <- carried(prev_data_manifest, "data_last_changed") %||%
    code_last_changed
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
    bootstrap = bootstrap, last_changed = code_last_changed, now = shard_now)

  data_manifest <- build_manifest(
    data_con, series = "data", repo = PUBLISH_REPO, db_filename = DATA_DB_FILENAME,
    db_bytes = data_db_bytes,
    tables = c("bioc_datasets", "bioc_dataset_versions", "bioc_dataset_contents"),
    fp_table = "bioc_datasets", fp_cols = c("package", "name", "current_content_id"),
    pkg_table = "bioc_datasets", ver_table = "bioc_dataset_versions",
    stat_table = "bioc_dataset_contents", stat_cols = c("nrow", "ncol"),
    bootstrap = bootstrap, last_changed = data_last_changed,
    coverage = dataset_coverage, now = shard_now)

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
                      n_latest_on_build = bootstrap$n_latest_on_build,
                      failed_by_stage = .stage_counts(shard_stages),
                      parked = verdict_counts$parked,
                      failed_this_run = verdict_counts$failed_this_run,
                      over_cap_ok = verdict_counts$over_cap_ok,
                      over_cap_this_run = verdict_counts$over_cap_this_run,
                      n_released = n_released,
                      n_tried_skipped = length(tried_pkgs),
                      n_recheck_due = length(recheck_pkgs),
                      latest_by_build = .latest_by_build(con),
                      run_id = run_id, last_changed = code_manifest$last_changed,
                      data_last_changed = data_manifest$last_changed,
                      analyzer_memory_limit_mb = analyzer_limit$limit_mb,
                      analyzer_stats = telemetry$analyzer,
                      worker_phases = telemetry$phases,
                      worker_memory = telemetry$workers))

  if (length(fresh_pkgs) > 0L) {
    record_changed_packages(file.path(out_dir, "changed-packages.txt"), fresh_pkgs)
  }

  invisible(manifest)
}

# The flags update.R takes after <out_dir>.
.parse_cli_flags <- function(args) {
  out <- list(shard = SHARD_SIZE, force_full = FALSE, recollect = FALSE,
              unpark = NULL, requeue = NULL)
  for (arg in args[startsWith(args, "--")]) {
    if (startsWith(arg, "--shard=")) {
      n <- suppressWarnings(as.integer(sub("^--shard=", "", arg, perl = TRUE)))
      if (!is.na(n) && n > 0L) out$shard <- n
    } else if (identical(arg, "--bootstrap")) {
      out$force_full <- TRUE
    } else if (identical(arg, "--recollect")) {
      out$recollect <- TRUE
    } else if (startsWith(arg, "--unpark=")) {
      out$unpark <- sub("^--unpark=", "", arg)
    } else if (startsWith(arg, "--requeue=")) {
      out$requeue <- sub("^--requeue=", "", arg)
    }
  }
  out
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
      "Usage: Rscript scripts/update.R <out_dir> [--shard=N] [--bootstrap] [--recollect] [--unpark=all|fetch|analyze|timeout|<pkg,...>] [--requeue=<pkg,...>|over_cap]",
      call. = FALSE
    )
  }

  flags <- .parse_cli_flags(args)

  io <- default_io()
  run_update(io, out_dir, shard_size = flags$shard, force_full = flags$force_full,
             recollect = flags$recollect, unpark = flags$unpark,
             requeue = flags$requeue)
  message("Done.")
}
