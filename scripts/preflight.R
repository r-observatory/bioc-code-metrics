# scripts/preflight.R: settle whether the state this run is meant to build on
# actually came back, before any shard writes to it.
#
# Load order: config.R -> preflight.R
# This file does NOT auto-source its dependencies; the caller controls source order.
#
# The pipeline is incremental: the released database IS the accumulated state.
# Every run downloads the previous release's copy, analyses a shard into it and
# republishes, which makes "publish" and "overwrite months of collection" the
# same gesture. The only thing separating them is whether the run started from
# the prior database. A sibling pipeline, cran-queue, published an empty
# database as latest on 2026-07-16 after a single 503 on the asset download,
# and stayed green for weeks, because `|| true` made a failed download and a
# first run look identical from there on.
#
# The workflow's download step is the first line: a release that advertises an
# asset it cannot hand over fails the run. This file is the second. It is run
# once per run, from that same step, and it cannot move into the shard loop:
# the comparison holds only before the first shard, because every later shard
# has legitimately added rows to the same file while prev-*-manifest.json still
# describes yesterday's release.

# The two series the download step brings back, named once. `expected` names
# the ones whose DATABASE the resolved release advertised, and the downloaded
# prev-*-manifest.json names the ones whose MANIFEST it advertised. Either one
# is proof that there was a prior release for that series, which is what tells
# a cold start (nothing to fetch) apart from a lost download (something to
# fetch that did not arrive). Both are needed, because the publish uploads the
# database and the manifest as separate assets and an interrupted publish can
# leave a release carrying one without the other.
.preflight_specs <- function() list(
  list(series = "code", db = DB_FILENAME,
       manifest = "prev-code-manifest.json", manifest_asset = "code-manifest.json",
       ver_table = "bioc_code_summary", pkg_table = "bioc_code_summary"),
  list(series = "data", db = DATA_DB_FILENAME,
       manifest = "prev-data-manifest.json", manifest_asset = "data-manifest.json",
       ver_table = "bioc_dataset_versions", pkg_table = "bioc_datasets")
)

# Read one key out of a parsed manifest, or NULL when it is absent or is not a
# single number. A manifest written before a field existed is skipped rather
# than read as zero.
.pf_at <- function(x, key) {
  if (!is.list(x)) return(NULL)
  v <- x[[key]]
  if (is.null(v) || length(v) != 1L || !is.numeric(v) || is.na(v)) return(NULL)
  as.numeric(v)
}

# Row counts printed in full. These arrive as doubles, and the default
# rendering turns a round one into scientific notation (100000 prints as
# 1e+05), which is not a number an operator can compare against a release.
.pf_fmt <- function(x) sprintf("%.0f", as.numeric(x))

# Count rows and distinct packages in one database file. A file that is not
# there, a table that is not in it, and a file SQLite refuses to read all count
# zero. The last one matters: a download that stops partway leaves bytes on
# disk that are not a database, and "holds nothing" is both true of it and a
# far more useful thing to say than "file is not a database".
.pf_db_counts <- function(db_path, ver_table, pkg_table) {
  none <- list(n_versions = 0, n_packages = 0)
  if (!file.exists(db_path)) return(none)
  # Opening a file that is not a database succeeds and warns; it is the first
  # query that fails. Both are expected here and both mean the same thing.
  con <- tryCatch(suppressWarnings(DBI::dbConnect(RSQLite::SQLite(), db_path)),
                  error = function(e) NULL)
  if (is.null(con)) return(none)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  present <- tryCatch(DBI::dbListTables(con), error = function(e) character(0L))
  count <- function(sql) {
    tryCatch(as.numeric(DBI::dbGetQuery(con, sql)$n), error = function(e) 0)
  }
  list(
    n_versions = if (ver_table %in% present) {
      count(sprintf('SELECT COUNT(*) n FROM "%s"', ver_table))
    } else 0,
    n_packages = if (pkg_table %in% present) {
      count(sprintf('SELECT COUNT(DISTINCT package) n FROM "%s"', pkg_table))
    } else 0
  )
}

# Whether the manifest that shipped beside a database can be compared against
# it. NULL means there is nothing to compare, a character value is a refusal,
# and TRUE means go ahead.
#
# A manifest carrying no series at all predates the code/data split and cannot
# describe either database, so it is set aside with a note rather than refused:
# an unusable baseline is not evidence that anything was lost. A manifest that
# declares the OTHER series is a real mixup, and comparing the dataset database
# against a code manifest would measure bioc_dataset_versions against a code
# n_versions and call a healthy database broken.
.pf_comparable <- function(series, prior) {
  if (is.null(prior) || length(prior) == 0L) return(NULL)
  declared <- prior[["series"]]
  if (is.null(declared) || !nzchar(as.character(declared)[[1L]])) return(NULL)
  if (!identical(as.character(declared)[[1L]], series)) {
    return(sprintf(
      "the %s baseline manifest declares series \"%s\"; it does not describe this database",
      series, as.character(declared)[[1L]]))
  }
  TRUE
}

#' Whether a downloaded prior database holds LESS than the manifest published
#' beside it recorded.
#'
#' One-sided on purpose. Only `now < was` is the signature this is for: a
#' truncated file, or a database from before the rows the manifest counted. The
#' other direction is what an interrupted `gh release upload --clobber` leaves
#' when this shard's database lands beside the previous shard's manifest, and
#' it costs nothing, so it is reported by prior_db_notes() instead.
#'
#' @param series "code" or "data".
#' @param counts list(n_packages, n_versions) measured from the downloaded DB.
#' @param prior  Manifest published alongside that DB, or NULL.
#' @param tables list(ver_table, pkg_table) for this series.
#' @return Character vector of violations, possibly empty.
prior_db_violations <- function(series, counts, prior, tables) {
  ok <- .pf_comparable(series, prior)
  if (is.null(ok)) return(character(0L))
  if (is.character(ok)) return(ok)

  out <- character(0L)
  was_ver <- .pf_at(prior, "n_versions")
  now_ver <- as.numeric(counts$n_versions %||% 0)
  if (!is.null(was_ver) && now_ver < was_ver) {
    out <- c(out, sprintf(
      "the downloaded %s database holds %s rows in %s; the manifest published with it says %s",
      series, .pf_fmt(now_ver), tables$ver_table, .pf_fmt(was_ver)))
  }
  was_pkg <- .pf_at(prior, "n_packages")
  now_pkg <- as.numeric(counts$n_packages %||% 0)
  if (!is.null(was_pkg) && now_pkg < was_pkg) {
    out <- c(out, sprintf(
      "the downloaded %s database covers %s packages in %s; the manifest published with it says %s",
      series, .pf_fmt(now_pkg), tables$pkg_table, .pf_fmt(was_pkg)))
  }
  out
}

#' What is worth saying about a prior database without stopping the run.
#'
#' A database AHEAD of its manifest says the previous publish was interrupted
#' partway through its assets, and the release will keep handing out that
#' mismatched pair until someone repairs it. A manifest with no series says the
#' baseline predates the split. Neither loses anything, and both are worth an
#' annotation every run so they get noticed before something less benign lands
#' in the same window.
#'
#' @inheritParams prior_db_violations
#' @return Character vector of notes, possibly empty.
prior_db_notes <- function(series, counts, prior, tables) {
  if (is.null(prior) || length(prior) == 0L) return(character(0L))
  ok <- .pf_comparable(series, prior)
  if (is.character(ok)) return(character(0L))
  if (is.null(ok)) {
    return(sprintf(paste0(
      "the %s baseline manifest carries no series and predates the split, so ",
      "it cannot be compared against %s. Building on the database that came ",
      "back anyway."), series, tables$ver_table))
  }

  out <- character(0L)
  was_ver <- .pf_at(prior, "n_versions")
  now_ver <- as.numeric(counts$n_versions %||% 0)
  if (!is.null(was_ver) && now_ver > was_ver) {
    out <- c(out, sprintf(paste0(
      "the downloaded %s database holds %s rows in %s but the manifest ",
      "published with it says %s: the previous publish did not finish ",
      "uploading its assets. Building on it anyway (a smaller baseline only ",
      "loosens the floor), but re-upload the manifest that belongs with that ",
      "database."),
      series, .pf_fmt(now_ver), tables$ver_table, .pf_fmt(was_ver)))
  }
  was_pkg <- .pf_at(prior, "n_packages")
  now_pkg <- as.numeric(counts$n_packages %||% 0)
  if (!is.null(was_pkg) && now_pkg > was_pkg) {
    out <- c(out, sprintf(paste0(
      "the downloaded %s database covers %s packages in %s but the manifest ",
      "published with it says %s"),
      series, .pf_fmt(now_pkg), tables$pkg_table, .pf_fmt(was_pkg)))
  }
  out
}

#' Check every database the resolved release advertised.
#'
#' A series is checked when the resolved release advertised its database OR its
#' manifest. Keying on the database alone is not enough: the two are separate
#' assets of the same release, so a publish interrupted between them leaves a
#' release carrying the manifest and no database, the download step then has
#' nothing to fetch and hands back an empty `expected`, and a gate that reads
#' that as a cold start would let the run rebuild from nothing and publish it
#' as latest. That release stays latest, so it would repeat every day.
#'
#' @param out_dir  Directory the download step wrote into.
#' @param expected Character vector of series ("code", "data") whose database
#'   the release this run resolved actually advertises.
#' @return list(violations = character, notes = character, checked = character).
#'   `checked` names the series that had any evidence of a prior release;
#'   empty is the genuine cold start.
preflight_prior_dbs <- function(out_dir, expected = character(0L)) {
  violations <- character(0L)
  notes      <- character(0L)
  checked    <- character(0L)
  expected   <- as.character(expected %||% character(0L))

  for (spec in .preflight_specs()) {
    m_path     <- file.path(out_dir, spec$manifest)
    advertised <- spec$series %in% expected
    # The filename is what says which series a baseline belongs to; the series
    # field inside it only decides whether the row counts can be compared. A
    # manifest too old to compare against is still proof of a prior release.
    baselined  <- file.exists(m_path)
    if (!advertised && !baselined) next
    checked <- c(checked, spec$series)

    db_path <- file.path(out_dir, spec$db)
    counts  <- .pf_db_counts(db_path, spec$ver_table, spec$pkg_table)
    prior   <- if (baselined) {
      tryCatch(jsonlite::fromJSON(m_path), error = function(e) NULL)
    } else NULL
    tables <- list(ver_table = spec$ver_table, pkg_table = spec$pkg_table)

    # The row-count gate. The release this run resolved left evidence that it
    # carries this series, so an empty database is not a first run: it is a
    # download that failed, a file that arrived truncated past the workflow's
    # size check, or an asset the previous publish never finished uploading.
    # Continuing from here would analyse a shard into nothing and publish that
    # as latest.
    if (counts$n_versions <= 0) {
      why <- if (advertised) {
        sprintf("the release this run resolved advertises %s", spec$db)
      } else {
        sprintf("%s came back from the prior release but %s was not among its assets",
                spec$manifest_asset, spec$db)
      }
      violations <- c(violations, sprintf(
        paste0("%s, and what came back holds no rows in %s. This run would ",
               "rebuild from an empty database and publish it as latest."),
        why, spec$ver_table))
      next
    }

    violations <- c(violations, prior_db_violations(spec$series, counts, prior, tables))
    notes      <- c(notes,      prior_db_notes(spec$series, counts, prior, tables))
  }

  list(violations = violations, notes = notes, checked = checked)
}

#' What an operator should do when this refuses.
#'
#' The message is part of the mechanism. A guard on a daily pipeline has to
#' fail toward recovery: its job is to stop a bad publish, not to become a
#' state nobody can get out of, and force_full is not the way out. force_full
#' deletes bioc_code_summary, bioc_code_churn and bioc_api_history and
#' republishes one shard's worth of packages as latest, which is the outcome
#' this check exists to prevent.
#'
#' @return A single string, ready to append to a refusal.
preflight_repair_advice <- function() {
  paste0(
    "\nLook at the PREVIOUS release first. The publish uploads four assets in ",
    "one `gh release upload --clobber`, which deletes each existing asset ",
    "before uploading its replacement and cannot do so atomically, so an ",
    "interrupted publish can leave one shard's database beside another ",
    "shard's manifest, or a database that never finished uploading.\n",
    "If that is what happened, open the release the download step resolved as ",
    "code src / data src and make its assets agree again: re-upload the ",
    "database and the manifest that belong together, or delete that release ",
    "so the day before it becomes latest again. Then re-run.\n",
    "If that release is consistent, this run really did lose the rows, and ",
    "the cause is upstream of the publish. Do not paper over it here.\n",
    "force_full is not the repair either way. It wipes the metric tables and ",
    "republishes one shard as latest.")
}

if (identical(sys.nframe(), 0L)) {
  # R prints at most warning.length bytes of an error and drops the rest, and
  # the default 1000 cuts the repair instructions off the end of this one.
  options(warning.length = 8170L)

  .script_dir <- {
    fa <- grep("^--file=", commandArgs(FALSE), value = TRUE)
    if (length(fa) >= 1L) dirname(sub("^--file=", "", fa[1L])) else "scripts"
  }
  source(file.path(.script_dir, "config.R"))

  args     <- commandArgs(trailingOnly = TRUE)
  out_dir  <- if (length(args) >= 1L) args[1L] else "out"
  expected <- if (length(args) >= 2L) args[-1L] else character(0L)

  checked <- preflight_prior_dbs(out_dir, expected)
  for (n in checked$notes) cat(sprintf("::warning::%s\n", n), file = stderr())
  if (length(checked$violations) > 0L) {
    for (p in checked$violations) cat(sprintf("::error::%s\n", p), file = stderr())
    stop("the state this run is meant to build on did not come back intact; ",
         "refusing to build a release on top of it.",
         preflight_repair_advice(), call. = FALSE)
  }
  # Report what was actually looked at, not what the release advertised: a
  # series can be checked on the strength of its manifest alone.
  if (length(checked$checked) == 0L) {
    cat("no prior release to build on; this run starts from nothing\n")
  } else {
    cat(sprintf("the prior %s %s came back intact\n",
                paste(checked$checked, collapse = " and "),
                if (length(checked$checked) > 1L) "databases" else "database"))
  }
}
