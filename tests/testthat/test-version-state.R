# tests/testthat/test-version-state.R: the commit, tree and deprecation signals
# each stored version was read from, kept beside the rows it gave.

# A git identity and dates from the environment, so no repository config is written.
.vst_git <- function(dir, ..., date = "2025-01-01T00:00:00Z") {
  out <- suppressWarnings(system2(
    "git", c("-C", shQuote(dir), ...), stdout = TRUE, stderr = FALSE,
    env = c("GIT_AUTHOR_NAME=T", "GIT_AUTHOR_EMAIL=t@t.test",
            "GIT_COMMITTER_NAME=T", "GIT_COMMITTER_EMAIL=t@t.test",
            paste0("GIT_AUTHOR_DATE=", date), paste0("GIT_COMMITTER_DATE=", date))))
  status <- attr(out, "status")
  if (!is.null(status) && status != 0L) stop("git ", paste(c(...), collapse = " "), " exited ", status)
  out
}

.vst_branch <- function(ver) paste0("RELEASE_", gsub(".", "_", ver, fixed = TRUE))

# What each release exports and the R code it carries. 1.1 deprecates through
# base R, 1.2 through base R and lifecycle, 1.3 through nothing; 1.2 drops b
# with no warning before it and 1.3 drops a, which 1.2 warned about.
.VST_RELEASES <- list(
  "1.0" = list(exports = c("a", "b"),
               code = c("a <- function() 1", "b <- function() 2")),
  "1.1" = list(exports = c("a", "b", "c"),
               code = c("a <- function() 1", "b <- function() .Deprecated('c')",
                        "c <- function() 3")),
  "1.2" = list(exports = c("a", "c", "d"),
               code = c("a <- function() .Defunct('b')", "c <- function() 3",
                        "d <- function() lifecycle::deprecate_warn('1.2', 'a()')")),
  "1.3" = list(exports = c("c", "d"),
               code = c("c <- function() 3", "d <- function() 4")))

# Commit one release on devel and cut its branch there, as Bioconductor does.
.vst_release <- function(dir, pkg, ver, date) {
  rel <- .VST_RELEASES[[ver]]
  writeLines(c(paste("Package:", pkg), paste("Version:", ver), "Title: T",
               "Description: T.", "Author: T", "Maintainer: T <t@t.test>",
               "License: MIT"), file.path(dir, "DESCRIPTION"))
  writeLines(sprintf("export(%s)", rel$exports), file.path(dir, "NAMESPACE"))
  dir.create(file.path(dir, "R"), showWarnings = FALSE)
  writeLines(rel$code, file.path(dir, "R", "code.R"))
  .vst_git(dir, "add", "-A", date = date)
  .vst_git(dir, "commit", "-q", "-m", shQuote(paste("release", ver)), date = date)
  .vst_git(dir, "branch", .vst_branch(ver), date = date)
}

# A source repository at <base>/<pkg>.git with a release branch per version,
# which clone_package(pkg, dest, base = base) clones as it would github.com/bioc.
.vst_source <- function(base, pkg, versions) {
  dir <- file.path(base, paste0(pkg, ".git"))
  dir.create(dir, recursive = TRUE)
  .vst_git(dir, "init", "-q")
  .vst_git(dir, "checkout", "-q", "-b", "devel")
  writeLines(paste("#", pkg), file.path(dir, "README"))
  .vst_git(dir, "add", "-A")
  .vst_git(dir, "commit", "-q", "-m", "init")
  for (k in seq_along(versions)) {
    .vst_release(dir, pkg, versions[[k]], sprintf("2025-0%d-01T00:00:00Z", k + 1L))
  }
  dir
}

# A fix committed on a release branch after it was cut.
.vst_move_branch <- function(dir, ver) {
  .vst_git(dir, "checkout", "-q", .vst_branch(ver))
  cat("e <- function() 5\n", file = file.path(dir, "R", "code.R"), append = TRUE)
  .vst_git(dir, "commit", "-q", "-am", "fix", date = "2025-09-01T00:00:00Z")
  .vst_git(dir, "checkout", "-q", "devel")
}

.vst_rev_parse <- function(dir, refs) .vst_git(dir, "rev-parse", shQuote(refs))

# One run_update over packages cloned from `base`, each at its `latest`.
.vst_run <- function(base, out_dir, pkgs, latest, shard_size = 10L, ...,
                     frame = parent.frame()) {
  .local_global("WORK_DIR", withr::local_tempdir("vst_work_", .local_envir = frame), frame)
  .local_global("ANALYSIS_CORES", 1L, frame)
  io <- list(
    package_list = function() data.frame(package = pkgs, latest_version = latest,
                                         stringsAsFactors = FALSE),
    clone = function(pkg, dest) clone_package(pkg, dest, base = base))
  out <- NULL
  utils::capture.output(out <- suppressWarnings(run_update(io, out_dir, shard_size = shard_size,
                                                           ...)))
  out
}

.vst_query <- function(out_dir, sql, params = NULL) {
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out_dir, DB_FILENAME))
  on.exit(DBI::dbDisconnect(con))
  DBI::dbGetQuery(con, sql, params = params)
}

# A package's state rows in release order.
.vst_state <- function(out_dir, pkg = "vstpkg") {
  st <- .vst_query(out_dir, sprintf('SELECT * FROM "%s" WHERE package = ?', VERSION_STATE_TABLE),
                   params = list(pkg))
  st[order(numeric_version(st$version)), , drop = FALSE]
}

# Each version's deprecation signals as the walk takes them: the release
# extracted with extract_version and read by deprecation_signals.
.vst_walk_signals <- function(dir, ver) {
  tmp <- withr::local_tempdir("vst_sig_")
  files <- extract_version(dir, .vst_branch(ver), tmp)
  read_fn <- function(path) {
    full <- file.path(tmp, path)
    if (!file.exists(full)) "" else paste(readLines(full, warn = FALSE), collapse = "\n")
  }
  deprecation_signals(build_context(package = "vstpkg", version = ver, ref = .vst_branch(ver),
                                    date = NA_character_, files = files, read_fn = read_fn))
}

# ---- The table --------------------------------------------------------------

.VST_COLUMNS <- data.frame(
  name = c("package", "version", "commit_sha", "tree_sha", "prev_version",
           "prev_commit", "deprecated", "uses_lifecycle", "read_at"),
  type = c("TEXT", "TEXT", "TEXT", "TEXT", "TEXT", "TEXT", "TEXT", "INTEGER", "TEXT"),
  notnull = c(1L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L),
  pk = c(1L, 2L, 0L, 0L, 0L, 0L, 0L, 0L, 0L),
  stringsAsFactors = FALSE)

.vst_expect_schema <- function(con) {
  info <- DBI::dbGetQuery(con, sprintf('PRAGMA table_info("%s")', VERSION_STATE_TABLE))
  expect_identical(info[, c("name", "type", "notnull", "pk")], .VST_COLUMNS)
  sql <- DBI::dbGetQuery(con, "SELECT sql FROM sqlite_master WHERE type = 'table' AND name = ?",
                         params = list(VERSION_STATE_TABLE))$sql
  expect_match(sql, "WITHOUT ROWID", fixed = TRUE)
}

test_that("the version state table is named for Bioconductor", {
  expect_identical(VERSION_STATE_TABLE, "bioc_version_state")
})

test_that("a new code database has the version state table, empty", {
  path <- withr::local_tempfile(fileext = ".db")
  con <- open_or_init_db(path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_true(VERSION_STATE_TABLE %in% DBI::dbListTables(con))
  .vst_expect_schema(con)
  expect_identical(DBI::dbGetQuery(con, sprintf('SELECT COUNT(*) n FROM "%s"',
                                                VERSION_STATE_TABLE))$n, 0L)
})

test_that("a database from before the table gains it on open, and keeps its rows", {
  path <- withr::local_tempfile(fileext = ".db")
  con <- open_or_init_db(path)
  DBI::dbExecute(con, sprintf('DROP TABLE IF EXISTS "%s"', VERSION_STATE_TABLE))
  DBI::dbWriteTable(con, SUMMARY_TABLE, data.frame(package = "p", version = "3.20",
                                                   stringsAsFactors = FALSE))
  DBI::dbDisconnect(con)

  con <- open_or_init_db(path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_true(VERSION_STATE_TABLE %in% DBI::dbListTables(con))
  .vst_expect_schema(con)
  expect_identical(DBI::dbGetQuery(con, sprintf('SELECT version FROM "%s"', SUMMARY_TABLE))$version,
                   "3.20")
})

# ---- What a walk records ----------------------------------------------------

test_that("a commit's tree comes from git, and is NA rather than misplaced when git cannot say", {
  base <- withr::local_tempdir("vst_src_")
  src <- .vst_source(base, "vstpkg", c("1.0", "1.1"))
  commits <- .vst_rev_parse(src, .vst_branch(c("1.0", "1.1")))
  expect_identical(commit_trees(src, commits),
                   .vst_rev_parse(src, paste0(.vst_branch(c("1.0", "1.1")), "^{tree}")))
  expect_identical(commit_trees(src, character(0L)), character(0L))
  expect_identical(commit_trees(src, c(commits[[1L]], strrep("0", 40L))),
                   c(NA_character_, NA_character_))
})

test_that("analyze_package returns a state row for each listed version", {
  base <- withr::local_tempdir("vst_src_")
  src <- .vst_source(base, "vstpkg", c("1.0", "1.1", "1.2"))
  res <- suppressWarnings(analyze_package(src, "vstpkg"))
  st <- res$state
  expect_identical(names(st), .VST_COLUMNS$name)
  expect_identical(st$version, res$summary$version)
  expect_identical(st$package, rep("vstpkg", 3L))
  expect_identical(st$commit_sha, .vst_rev_parse(src, .vst_branch(c("1.0", "1.1", "1.2"))))
  expect_identical(st$uses_lifecycle, c(0L, 0L, 1L))
  expect_match(st$read_at, "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$")
})

test_that("a package that lists no versions has no state rows", {
  repo <- withr::local_tempdir("vst_none_")
  .vst_git(repo, "init", "-q")
  writeLines("x", file.path(repo, "README"))
  .vst_git(repo, "add", "-A")
  .vst_git(repo, "commit", "-q", "-m", "init")
  res <- analyze_package(repo, "vstpkg")
  expect_identical(res$state, .empty_version_state())
})

test_that("a run records every written version's commit, tree, predecessor and deprecations", {
  base <- withr::local_tempdir("vst_src_")
  out_dir <- withr::local_tempdir("vst_out_")
  vers <- c("1.0", "1.1", "1.2")
  src <- .vst_source(base, "vstpkg", vers)
  .vst_run(base, out_dir, "vstpkg", "1.2")

  st <- .vst_state(out_dir)
  written <- .vst_query(out_dir, sprintf('SELECT version FROM "%s" WHERE package = ?',
                                         SUMMARY_TABLE), params = list("vstpkg"))$version
  expect_setequal(st$version, written)
  expect_identical(st$version, vers)
  commits <- .vst_rev_parse(src, .vst_branch(vers))
  expect_identical(st$commit_sha, commits)
  expect_identical(st$tree_sha, .vst_rev_parse(src, paste0(.vst_branch(vers), "^{tree}")))
  expect_identical(st$prev_version, c(NA, "1.0", "1.1"))
  expect_identical(st$prev_commit, c(NA, commits[1:2]))
  expect_identical(st$deprecated, c("[]", "[\"c\"]", "[\"b\",\"a\"]"))
  expect_identical(st$uses_lifecycle, c(0L, 0L, 1L))
  expect_identical(length(unique(st$read_at)), 1L)

  # The same signals the walk takes from each release.
  for (k in seq_along(vers)) {
    sig <- .vst_walk_signals(src, vers[[k]])
    expect_identical(as.character(jsonlite::fromJSON(st$deprecated[[k]])),
                     sig$symbols %||% character(0L), info = vers[[k]])
    expect_identical(st$uses_lifecycle[[k]], as.integer(sig$uses_lifecycle), info = vers[[k]])
  }

  # And the series the walk fed to the cross-version columns: the stored rows
  # and the state rows give back what the summary holds.
  summ <- .vst_query(out_dir, sprintf('SELECT * FROM "%s" WHERE package = ?', SUMMARY_TABLE),
                     params = list("vstpkg"))
  summ <- summ[match(vers, summ$version), , drop = FALSE]
  api <- .vst_query(out_dir, sprintf('SELECT * FROM "%s" WHERE package = ?', API_TABLE),
                    params = list("vstpkg"))
  series <- lapply(seq_along(vers), function(k) {
    list(symbols = as.character(jsonlite::fromJSON(st$deprecated[[k]])),
         uses_lifecycle = st$uses_lifecycle[[k]] == 1L)
  })
  again <- add_cross_version_metrics(summ, api, series)
  expect_identical(summ$cold_removal_rate[[3L]], 1)
  expect_identical(again$cold_removal_rate, summ$cold_removal_rate)
  expect_identical(again$deprecation_infrastructure_maturity,
                   summ$deprecation_infrastructure_maturity)
})

test_that("a re-run replaces the package's state rows, one per version", {
  .local_stepping_clock()
  base <- withr::local_tempdir("vst_src_")
  out_dir <- withr::local_tempdir("vst_out_")
  src <- .vst_source(base, "vstpkg", c("1.0", "1.1", "1.2"))
  .vst_run(base, out_dir, "vstpkg", "1.2")
  first <- .vst_state(out_dir)

  .vst_move_branch(src, "1.1")
  .vst_release(src, "vstpkg", "1.3", "2025-10-01T00:00:00Z")
  .vst_run(base, out_dir, "vstpkg", "1.3")
  st <- .vst_state(out_dir)

  n <- .vst_query(out_dir, sprintf('SELECT COUNT(*) n, COUNT(DISTINCT version) d FROM "%s"',
                                   VERSION_STATE_TABLE))
  expect_identical(c(n$n, n$d), c(4L, 4L))
  vers <- c("1.0", "1.1", "1.2", "1.3")
  commits <- .vst_rev_parse(src, .vst_branch(vers))
  expect_identical(st$version, vers)
  expect_identical(st$commit_sha, commits)
  expect_false(identical(st$commit_sha[[2L]], first$commit_sha[[2L]]))
  expect_identical(st$prev_commit, c(NA, commits[1:3]))
  expect_identical(st$tree_sha, .vst_rev_parse(src, paste0(.vst_branch(vers), "^{tree}")))
  expect_identical(st$deprecated[[4L]], "[]")
  # Every row is the second run's, the unchanged ones included.
  expect_true(all(st$read_at > max(first$read_at)))
})

test_that("a package that fails keeps its rows and its state rows as they were", {
  base <- withr::local_tempdir("vst_src_")
  out_dir <- withr::local_tempdir("vst_out_")
  src <- .vst_source(base, "vstpkg", c("1.0", "1.1"))
  .vst_run(base, out_dir, "vstpkg", "1.1")
  before <- .package_rows(out_dir, "vstpkg")
  state_key <- paste(DB_FILENAME, VERSION_STATE_TABLE)
  expect_identical(nrow(before[[state_key]]), 2L)

  .vst_move_branch(src, "1.0")
  .vst_release(src, "vstpkg", "1.2", "2025-10-01T00:00:00Z")
  real <- extract_version
  .local_global("extract_version", function(repo, ref, dest) {
    if (grepl("RELEASE_1_2$", ref)) stop(.extract_failure("archive", ref, 128L, "fatal: bad object"))
    real(repo, ref, dest)
  })
  m <- .vst_run(base, out_dir, "vstpkg", "1.2")
  expect_identical(m$shard_failures$packages, "vstpkg")
  expect_identical(.package_rows(out_dir, "vstpkg"), before)
})

test_that("a package whose clone lists no versions keeps its rows and its state rows", {
  base <- withr::local_tempdir("vst_src_")
  out_dir <- withr::local_tempdir("vst_out_")
  src <- .vst_source(base, "vstpkg", c("1.0", "1.1"))
  .vst_run(base, out_dir, "vstpkg", "1.1")
  before <- .package_rows(out_dir, "vstpkg")

  for (b in .vst_branch(c("1.0", "1.1"))) .vst_git(src, "branch", "-q", "-D", b)
  m <- .vst_run(base, out_dir, "vstpkg", "1.2")
  expect_identical(m$shard_failures$count, 0L)
  # The read attempt a run that read nothing records is not the package's data.
  data_rows <- function(x) x[!grepl("_analyzer_read_attempts$", names(x))]
  expect_true(paste(DB_FILENAME, VERSION_STATE_TABLE) %in% names(before))
  expect_identical(data_rows(.package_rows(out_dir, "vstpkg")), data_rows(before))
})

test_that("--bootstrap starts the state table empty, as it does the summary", {
  base <- withr::local_tempdir("vst_src_")
  out_dir <- withr::local_tempdir("vst_out_")
  .vst_source(base, "pkgA", c("1.0", "1.1"))
  .vst_source(base, "pkgB", c("1.0"))
  .vst_run(base, out_dir, c("pkgA", "pkgB"), c("1.1", "1.0"))
  expect_setequal(.vst_query(out_dir, sprintf('SELECT DISTINCT package FROM "%s"',
                                              VERSION_STATE_TABLE))$package, c("pkgA", "pkgB"))

  .vst_run(base, out_dir, c("pkgA", "pkgB"), c("1.1", "1.0"), shard_size = 1L,
           force_full = TRUE)
  keys <- function(t) .vst_query(out_dir, sprintf(
    'SELECT package, version FROM "%s" ORDER BY package, version', t))
  expect_identical(keys(VERSION_STATE_TABLE), keys(SUMMARY_TABLE))
  expect_identical(unique(keys(VERSION_STATE_TABLE)$package), "pkgA")
})

# ---- The write --------------------------------------------------------------

.vst_summary <- function(pkg, vers) {
  data.frame(package = rep(pkg, length(vers)), version = vers, loc_r = seq_along(vers),
             stringsAsFactors = FALSE)
}

.vst_state_df <- function(pkg, vers, commit = paste0("c", vers)) {
  n <- length(vers)
  data.frame(package = rep(pkg, n), version = vers, commit_sha = commit,
             tree_sha = paste0("t", vers), prev_version = c(NA, vers[-n]),
             prev_commit = c(NA, commit[-n]), deprecated = rep("[]", n),
             uses_lifecycle = rep(0L, n), read_at = rep("2026-10-03T00:00:00Z", n),
             stringsAsFactors = FALSE)
}

.vst_empty_churn <- function() data.frame(package = character(0L), version = character(0L),
                                          file = character(0L), added = integer(0L),
                                          deleted = integer(0L))
.vst_empty_api <- function() data.frame(package = character(0L), version = character(0L),
                                        exports_added = character(0L),
                                        exports_removed = character(0L),
                                        n_exports = integer(0L))

test_that("upsert_shard writes state rows for exactly the versions whose summary rows it writes", {
  path <- withr::local_tempfile(fileext = ".db")
  con <- open_or_init_db(path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  upsert_shard(con, rbind(.vst_summary("pkgA", c("1.0", "1.1")), .vst_summary("pkgB", "2.0")),
               .vst_empty_churn(), .vst_empty_api(),
               state_df = rbind(.vst_state_df("pkgA", c("1.0", "1.1")),
                                .vst_state_df("pkgB", "2.0")))

  # pkgA again: a version gone, a version twice, and a state row with no
  # summary row beside it.
  summ <- .vst_summary("pkgA", c("1.1", "1.2", "1.2"))
  st <- rbind(.vst_state_df("pkgA", c("1.1", "1.2"), commit = c("x1", "old")),
              .vst_state_df("pkgA", c("1.2", "1.9"), commit = c("new", "x9")))
  upsert_shard(con, summ, .vst_empty_churn(), .vst_empty_api(), state_df = st)

  got <- DBI::dbGetQuery(con, sprintf(
    'SELECT package, version, commit_sha FROM "%s" ORDER BY package, version', VERSION_STATE_TABLE))
  expect_identical(got$package, c("pkgA", "pkgA", "pkgB"))
  expect_identical(got$version, c("1.1", "1.2", "2.0"))
  expect_identical(got$commit_sha, c("x1", "new", "c2.0"))
})

test_that("a write with no state rows leaves the package with none", {
  path <- withr::local_tempfile(fileext = ".db")
  con <- open_or_init_db(path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  upsert_shard(con, .vst_summary("pkgA", "1.0"), .vst_empty_churn(), .vst_empty_api(),
               state_df = .vst_state_df("pkgA", "1.0"))
  upsert_shard(con, .vst_summary("pkgA", "1.0"), .vst_empty_churn(), .vst_empty_api())
  expect_identical(DBI::dbGetQuery(con, sprintf('SELECT COUNT(*) n FROM "%s"',
                                                VERSION_STATE_TABLE))$n, 0L)
})

test_that("a state write that fails rolls back the package's other rows with it", {
  path <- withr::local_tempfile(fileext = ".db")
  con <- open_or_init_db(path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  upsert_shard(con, .vst_summary("pkgA", "1.0"), .vst_empty_churn(), .vst_empty_api(),
               state_df = .vst_state_df("pkgA", "1.0"))
  real <- .write_version_state
  .local_global(".write_version_state", function(con, pkgs, summary_df, state_df) {
    real(con, pkgs, summary_df, state_df)
    stop("state write failed")
  })
  expect_error(upsert_shard(con, .vst_summary("pkgA", c("1.0", "1.1")), .vst_empty_churn(),
                            .vst_empty_api(), state_df = .vst_state_df("pkgA", c("1.0", "1.1"))),
               "state write failed")
  expect_identical(DBI::dbGetQuery(con, sprintf('SELECT version FROM "%s"', SUMMARY_TABLE))$version,
                   "1.0")
  expect_identical(DBI::dbGetQuery(con, sprintf('SELECT version FROM "%s"',
                                                VERSION_STATE_TABLE))$version, "1.0")
})
