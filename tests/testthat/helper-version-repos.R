# tests/testthat/helper-version-repos.R: source repositories with release branches, and runs over them.

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

# One run_update over packages cloned from `base`, each at its `latest`. Its
# manifest, with what it printed as the "log" attribute.
.vst_run <- function(base, out_dir, pkgs, latest, shard_size = 10L, ...,
                     frame = parent.frame()) {
  .local_global("WORK_DIR", withr::local_tempdir("vst_work_", .local_envir = frame), frame)
  .local_global("ANALYSIS_CORES", 1L, frame)
  io <- list(
    package_list = function() data.frame(package = pkgs, latest_version = latest,
                                         stringsAsFactors = FALSE),
    clone = function(pkg, dest) clone_package(pkg, dest, base = base))
  out <- NULL
  log <- utils::capture.output(out <- suppressWarnings(run_update(io, out_dir,
                                                                  shard_size = shard_size, ...)))
  structure(out, log = log)
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

# The stub analyzer, reading `versions`, for the calling test. Each version's
# dataset has a content of its own, so a link to the wrong one shows.
.vs_stub <- function(versions = names(.VST_RELEASES), detail = FALSE, frame = parent.frame()) {
  withr::local_envvar(
    RPKG_ANALYZER_BIN = .stub_analyzer_bin(withr::local_tempdir(.local_envir = frame),
                                           "0.4.0-test", reads = versions, detail = detail,
                                           content_by_version = TRUE),
    .local_envir = frame)
}
