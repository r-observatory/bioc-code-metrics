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
# asset it cannot hand over fails the run, and a draft is never resolved as a
# release at all (scripts/publish.sh). This file is the second. It is run
# once per run, from that same step, and it cannot move into the shard loop:
# the comparison holds only before the first shard, because every later shard
# has legitimately added rows to the same file while prev-*-manifest.json still
# describes yesterday's release.
#
# Two things happen here, in this order. A release that published a database
# and no manifest gets a baseline measured from that database, because the
# publish replaces four assets one at a time and can be interrupted between
# them. Then each database is compared against the manifest published with
# it, and only a database that did not come back at all, one holding LESS than
# its manifest recorded, or a resolved release that carried neither, stops the
# run.

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

# Count rows and distinct packages in one database file, and say whether the
# file was a database at all.
#
# `readable` is the only thing a file can say for itself about a lost download.
# A download that stops partway leaves bytes on disk that SQLite will not read,
# and that is a fact about the transfer. Zero rows is not: a database this
# pipeline published can genuinely hold none, so the counts are evidence to
# compare against a baseline rather than a verdict on their own.
.pf_db_counts <- function(db_path, ver_table, pkg_table) {
  none <- list(readable = FALSE, n_versions = 0, n_packages = 0)
  if (!file.exists(db_path)) return(none)
  size <- as.numeric(file.info(db_path)$size)
  if (length(size) != 1L || is.na(size) || size <= 0) return(none)
  # Opening a file that is not a database succeeds and warns; it is the first
  # query that fails. Both are expected here and both mean the same thing.
  con <- tryCatch(suppressWarnings(DBI::dbConnect(RSQLite::SQLite(), db_path)),
                  error = function(e) NULL)
  if (is.null(con)) return(none)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  present <- tryCatch(DBI::dbListTables(con), error = function(e) NULL)
  if (is.null(present)) return(none)
  count <- function(sql) {
    tryCatch(as.numeric(DBI::dbGetQuery(con, sql)$n), error = function(e) 0)
  }
  list(
    readable = TRUE,
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

#' A baseline measured from a downloaded database, for a release that
#' published no manifest.
#'
#' The publish is not atomic (four assets replaced one at a time with --clobber,
#' each existing asset deleted before its replacement lands), so a run that
#' died in that window can leave a release carrying its database and no
#' code-manifest.json. There was nothing to check such a database against, so
#' it was checked against the row count alone and a release that lost one asset
#' was refused every day after, since the same release stays latest.
#'
#' The database is right there and it is the thing worth protecting, so measure
#' it. The result is a real floor for prior_db_violations(): a database that
#' then comes back holding less than what was measured is still refused.
#'
#' Returns NULL when there is nothing to measure. An absent, empty or
#' unreadable database is exactly what a lost download leaves, and a baseline
#' of zero would publish a floor of zero as though it were a record of what the
#' release held.
#'
#' @param series  "code" or "data".
#' @param db_path Path to the downloaded database.
#' @return A manifest-shaped list, or NULL.
derive_baseline_manifest <- function(series, db_path) {
  spec <- Filter(function(s) identical(s$series, series), .preflight_specs())
  if (length(spec) != 1L) return(NULL)
  spec <- spec[[1L]]
  counts <- .pf_db_counts(db_path, spec$ver_table, spec$pkg_table)
  if (!isTRUE(counts$readable)) return(NULL)
  if (counts$n_versions <= 0 || counts$n_packages <= 0) return(NULL)

  list(schema_version = 1L, series = series,
       measured_from = basename(db_path),
       db_bytes = round(as.numeric(file.info(db_path)$size)),
       n_packages = counts$n_packages, n_versions = counts$n_versions)
}

#' Give a series a baseline when the prior release published none.
#'
#' Writes prev-<series>-manifest.json from the downloaded database, and only
#' when that file is absent. A manifest that IS present is never replaced, even
#' when it disagrees with the database: absence of a record is not evidence
#' that nothing was lost, but a record saying the database used to be bigger
#' is, and prior_db_violations() has to keep seeing it.
#'
#' @param out_dir Directory holding the downloaded assets.
#' @return Character vector of notes describing what was measured, empty when
#'   every series already had a published manifest or had nothing to measure.
ensure_prior_baseline <- function(out_dir) {
  notes <- character(0L)
  for (spec in .preflight_specs()) {
    mpath <- file.path(out_dir, spec$manifest)
    if (file.exists(mpath)) next
    derived <- derive_baseline_manifest(spec$series, file.path(out_dir, spec$db))
    if (is.null(derived)) next
    jsonlite::write_json(derived, mpath, auto_unbox = TRUE, pretty = TRUE)
    notes <- c(notes, sprintf(paste0(
      "the prior release carries %s but no %s, which is what an interrupted ",
      "`gh release upload --clobber` leaves. The baseline for this run was ",
      "measured from the database instead: %s packages, %s rows in %s. ",
      "Re-upload the manifest that belongs with that database so the next run ",
      "has a published record to check against."),
      spec$db, spec$manifest_asset, .pf_fmt(derived$n_packages),
      .pf_fmt(derived$n_versions), spec$ver_table))
  }
  notes
}

# The tag the download step resolved for one series, or "" when none did.
.pf_resolved_tag <- function(resolved, series) {
  if (length(resolved) == 0L || !(series %in% names(resolved))) return("")
  tag <- as.character(resolved[[series]])
  if (length(tag) != 1L || is.na(tag)) return("")
  trimws(tag)
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
#' Neither is enough when the release carries nothing at all. A release with no
#' database and no manifest advertises nothing and downloads nothing, which is
#' exactly what no release looks like, and a draft left by a failed
#' `gh release create` holds no assets when its uploads and gh's own cleanup
#' both fail. The tag the download step resolved is the one thing that tells the
#' two apart, so a series whose tag resolved and whose release carried neither
#' asset is refused rather than read as a cold start.
#'
#' @param out_dir  Directory the download step wrote into.
#' @param expected Character vector of series ("code", "data") whose database
#'   the release this run resolved actually advertises.
#' @param resolved Named character vector, series to the tag the download step
#'   resolved for it (for example `c(code = "metrics-2026-09-13", data = "")`).
#'   A missing or empty entry means no release resolved for that series.
#' @return list(violations = character, notes = character, checked = character).
#'   `checked` names the series that had any evidence of a prior release;
#'   empty is the genuine cold start.
preflight_prior_dbs <- function(out_dir, expected = character(0L),
                                resolved = character(0L)) {
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
    tag        <- .pf_resolved_tag(resolved, spec$series)
    if (!advertised && !baselined) {
      if (nzchar(tag)) {
        checked <- c(checked, spec$series)
        violations <- c(violations, sprintf(paste0(
          "the release this run resolved for the %s series, %s, carries neither ",
          "%s nor %s. That is not a cold start: a release with neither asset ",
          "lost both after it was published, or was never a finished publish. ",
          "This run would rebuild from nothing and publish it as latest."),
          spec$series, tag, spec$db, spec$manifest_asset))
      }
      next
    }
    checked <- c(checked, spec$series)

    db_path <- file.path(out_dir, spec$db)
    counts  <- .pf_db_counts(db_path, spec$ver_table, spec$pkg_table)
    prior   <- if (baselined) {
      tryCatch(jsonlite::fromJSON(m_path), error = function(e) NULL)
    } else NULL
    tables <- list(ver_table = spec$ver_table, pkg_table = spec$pkg_table)

    # The download gate. The release this run resolved left evidence that it
    # carries this series, so a file that is absent, empty, or not a database
    # is a download that failed, one that arrived truncated past the workflow's
    # size check, or an asset the previous publish never finished uploading.
    # Continuing from here would analyse a shard into nothing and publish that
    # as latest.
    #
    # Row counts are deliberately not part of this. A database that opens and
    # holds no rows is also what the first run of a cold bootstrap publishes
    # for whichever series its first shard had nothing for, and that release
    # stays latest, so refusing on the count made a legitimate empty database a
    # state with no way out. What the rows are compared against is the baseline
    # below, which is measured from the database when no manifest came back.
    if (!isTRUE(counts$readable)) {
      why <- if (advertised) {
        sprintf("the release this run resolved advertises %s", spec$db)
      } else {
        sprintf("%s came back from the prior release but %s was not among its assets",
                spec$manifest_asset, spec$db)
      }
      violations <- c(violations, sprintf(
        paste0("%s, and what came back is not a readable database. This run ",
               "would rebuild from nothing and publish it as latest."),
        why))
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
    "\nLook at the PREVIOUS release first. A same-day publish replaces four ",
    "assets one at a time with `gh release upload --clobber`, which deletes ",
    "each existing asset before uploading its replacement and cannot do so ",
    "atomically, so an interrupted publish can leave one shard's database ",
    "beside another shard's manifest, or a database that never finished ",
    "uploading.\n",
    "If that is what happened, open the release the download step resolved as ",
    "code src / data src and make its assets agree again: re-upload the ",
    "database and the manifest that belong together, or delete that release ",
    "so the day before it becomes latest again. Then re-run.\n",
    "If that release carries no assets at all for the series, there is nothing ",
    "in it to repair. Delete it with `gh release delete TAG --yes --cleanup-tag`, ",
    "so its git tag goes with it, then re-run. If `gh release list` shows a ",
    "draft under the same tag as well, delete the draft by id instead, with ",
    "`gh api -X DELETE repos/{owner}/{repo}/releases/<id>` (the ids are in ",
    "`gh api 'repos/{owner}/{repo}/releases?per_page=100'`): a delete by tag ",
    "can take the published one.\n",
    "If that release is consistent, this run really did lose the rows, and ",
    "the cause is upstream of the publish. Do not paper over it here.\n",
    "force_full is not the repair either way. It wipes the metric tables and ",
    "republishes one shard as latest.")
}

# The command line: the output directory, then --code-src=TAG and
# --data-src=TAG for the releases the download step resolved (either may be
# empty), then the series whose database that release advertised. The tags are
# flags rather than positions so that an empty one survives the shell as
# "--code-src=" instead of disappearing and shifting everything after it.
.pf_parse_args <- function(args) {
  args     <- as.character(args)
  out_dir  <- if (length(args) >= 1L) args[[1L]] else "out"
  rest     <- args[-1L]
  is_src   <- grepl("^--(code|data)-src=", rest)
  resolved <- c(code = "", data = "")
  for (a in rest[is_src]) {
    resolved[[sub("^--(code|data)-src=.*$", "\\1", a)]] <- sub("^--(code|data)-src=", "", a)
  }
  list(out_dir = out_dir, expected = rest[!is_src], resolved = resolved)
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

  args     <- .pf_parse_args(commandArgs(trailingOnly = TRUE))
  out_dir  <- args$out_dir

  derived <- ensure_prior_baseline(out_dir)
  checked <- preflight_prior_dbs(out_dir, args$expected, args$resolved)
  for (n in c(derived, checked$notes)) {
    cat(sprintf("::warning::%s\n", n), file = stderr())
  }
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
