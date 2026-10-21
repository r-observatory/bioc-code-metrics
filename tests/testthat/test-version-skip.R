# tests/testthat/test-version-skip.R: reading only the versions whose stored rows are not current.

.vs_ref <- function(v) paste0("RELEASE_", gsub(".", "_", v, fixed = TRUE))
.vs_versions <- function(v, commits = paste0("c", seq_along(v))) {
  data.frame(version = v, ref = .vs_ref(v), date = "2026-01-01", commit = commits,
             stringsAsFactors = FALSE)
}
.vs_trees <- function(v) paste0("t", seq_along(v))

# A prior as .prior_state() gives it, every stored version current.
.vs_prior <- function(v, commits = paste0("c", seq_along(v)), build = "0.5.2") {
  n <- length(v)
  newest <- seq_len(n) == n
  list(
    stored = data.frame(version = v, analyzer_version = build, n_fns_r_null = FALSE,
                        detail_scanned = 1L,
                        datasets_scanned = ifelse(newest, 1L, NA_integer_), latest = newest,
                        released = "2026-01-01", dep_list = "[]", authors = "[]",
                        stringsAsFactors = FALSE),
    api = data.frame(version = v, exports_added = "[]", exports_removed = "[]",
                     n_exports = 0L, stringsAsFactors = FALSE),
    state = data.frame(version = v, commit_sha = commits, tree_sha = .vs_trees(v),
                       prev_version = c(NA, v[-n]), prev_commit = c(NA, commits[-n]),
                       deprecated = "[]", uses_lifecycle = 0L,
                       read_at = "2026-09-01T00:00:00Z", rows_generation = 1L,
                       stringsAsFactors = FALSE),
    text_ok = v, class = c("0.5.0", "0.5.1", "0.5.2"))
}
.vs_plan <- function(v, prior, commits = paste0("c", seq_along(v)), trees = .vs_trees(v), ...) {
  .read_plan(.vs_versions(v, commits), trees, prior, ...)
}
v3 <- c("1.0", "1.1", "1.2")

test_that("with no prior every version is read", {
  p <- .vs_plan(c("1.0", "1.1"), NULL)
  expect_identical(p$read, c(TRUE, TRUE))
  expect_identical(p$reason, c("all", "all"))
})
test_that("a current package reads nothing and redoes no diff", {
  p <- .vs_plan(v3, .vs_prior(v3))
  expect_false(any(p$read))
  expect_false(any(p$pred_changed))
})
test_that("a new tag is read, and only it", {
  p <- .vs_plan(v3, .vs_prior(v3[1:2]))
  expect_identical(p$reason, c(NA, NA, "new"))
  expect_false(any(p$pred_changed))
})
test_that("a moved tag is read and its successor's diff is redone", {
  p <- .vs_plan(v3, .vs_prior(v3), commits = c("c1", "c2b", "c3"))
  expect_identical(p$reason, c(NA, "moved", NA))
  expect_identical(p$pred_changed, c(FALSE, FALSE, TRUE))
})
test_that("a version with no state row, or no commit in it, is read as unrecorded", {
  pr <- .vs_prior(v3)
  pr$state <- pr$state[-1L, ]
  pr$state$commit_sha[[1L]] <- NA
  expect_identical(.vs_plan(v3, pr)$reason, c("unrecorded", "unrecorded", NA))
})
test_that("an equal commit with another tree is read, and an unknown tree on either side is not", {
  pr <- .vs_prior(v3)
  pr$state$tree_sha[[3L]] <- NA
  expect_identical(.vs_plan(v3, pr, trees = c(NA, "tX", "t3"))$reason, c(NA, "tree", NA))
})
test_that("rows outside the output class are read, a fallback row included", {
  pr <- .vs_prior(v3)
  pr$stored$analyzer_version <- c("0.4.0", NA, "0.5.1")
  expect_identical(.vs_plan(v3, pr)$reason, c("class", "class", NA))
})
test_that("an older rows_generation is read, and a NULL one counts as 1", {
  pr <- .vs_prior(v3)
  pr$state$rows_generation <- c(NA, 0L, 1L)
  expect_identical(.vs_plan(v3, pr)$reason, c(NA, "generation", NA))
  expect_identical(.vs_plan(v3, pr, generation = 2L)$reason, rep("generation", 3L))
})
test_that("a version missing n_fns_r or its text reading is read", {
  pr <- .vs_prior(v3)
  pr$stored$n_fns_r_null[[1L]] <- TRUE
  pr$text_ok <- c("1.0", "1.2")
  expect_identical(.vs_plan(v3, pr)$reason, c("metrics", "text", NA))
})
test_that("a text reading is asked only of rows a 0.5.0 or later build wrote", {
  pr <- .vs_prior(v3, build = "0.4.0-test")
  pr$class <- "0.4.0-test"
  pr$text_ok <- character(0L)
  expect_false(any(.vs_plan(v3, pr)$read))
})
test_that("a missing detail marker on any version reads it", {
  pr <- .vs_prior(v3)
  pr$stored$detail_scanned <- c(NA, 1L, 1L)
  expect_identical(.vs_plan(v3, pr)$reason, c("detail", NA, NA))
})
test_that("a missing detail marker is read on every version when detail is stored for all", {
  pr <- .vs_prior(v3)
  pr$stored$detail_scanned <- NA_integer_
  expect_identical(.vs_plan(v3, pr)$reason, rep("detail", 3L))
  expect_identical(.vs_plan(v3, pr, detail_all_rows = FALSE)$reason, c(NA, NA, "detail"))
})
test_that("the newest is read when it is not the stored newest or has lost its dataset marker", {
  v2 <- c("1.0", "1.1")
  pr <- .vs_prior(v2)
  pr$stored$datasets_scanned[[2L]] <- NA
  expect_identical(.vs_plan(v2, pr)$reason, c(NA, "newest"))
  # 1.2 is gone, so 1.1 is the newest again but was not the stored newest;
  # its dataset marker is set, so only the latest flag fails
  pr <- .vs_prior(v3)
  pr$stored$datasets_scanned <- 1L
  expect_identical(.vs_plan(v2, pr)$reason, c(NA, "newest"))
})
test_that("a tag inserted before a stored one redoes the stored one's diff", {
  p <- .vs_plan(v3, .vs_prior(c("1.0", "1.2")), commits = c("c1", "cx", "c2"),
                trees = c("t1", "tx", "t2"))
  expect_identical(p$reason, c(NA, "new", NA))
  expect_identical(p$pred_changed, c(FALSE, FALSE, TRUE))
})
test_that("a skipped version with no stored API row has its diff redone", {
  pr <- .vs_prior(v3[1:2])
  pr$api <- pr$api[1L, ]
  p <- .vs_plan(v3[1:2], pr)
  expect_false(any(p$read))
  expect_identical(p$pred_changed, c(FALSE, TRUE))
})
test_that("the first rule a version fails names its reason", {
  pr <- .vs_prior(v3)
  pr$stored$analyzer_version[[2L]] <- "0.4.0"
  expect_identical(.vs_plan(v3, pr, commits = c("c1", "cx", "c3"))$reason[[2L]], "moved")
})

.vs_git <- function(dir, ...) {
  suppressWarnings(system2(
    "git", c("-C", shQuote(dir), ...), stdout = TRUE, stderr = FALSE,
    env = c("GIT_AUTHOR_NAME=T", "GIT_AUTHOR_EMAIL=t@t.test",
            "GIT_COMMITTER_NAME=T", "GIT_COMMITTER_EMAIL=t@t.test")))
}

# A repository of release branches whose NAMESPACE differs per release; a NULL one has none.
.vs_repo <- function(dir, namespaces) {
  dir.create(dir, recursive = TRUE)
  .vs_git(dir, "init", "-q")
  for (v in names(namespaces)) {
    unlink(file.path(dir, "NAMESPACE"))
    writeLines(c(paste("Package: p", v), paste("Version:", v)), file.path(dir, "DESCRIPTION"))
    if (!is.null(namespaces[[v]])) writeLines(namespaces[[v]], file.path(dir, "NAMESPACE"))
    .vs_git(dir, "add", "-A")
    .vs_git(dir, "commit", "-q", "--allow-empty", "-m", shQuote(v))
    .vs_git(dir, "branch", .vs_ref(v))
  }
  dir
}

test_that("an API row is the walk's: exports added and removed against the version before", {
  expect_identical(.api_row("p", "1.1", c("a", "c"), c("a", "b")),
                   data.frame(package = "p", version = "1.1", exports_added = '["c"]',
                              exports_removed = '["b"]', n_exports = 2L, stringsAsFactors = FALSE))
  expect_identical(.api_row("p", "1.0", "a", NULL)$exports_added, '["a"]')
  expect_identical(.api_row("p", "1.0", character(0L), NULL)$exports_added, "[]")
})
test_that("a version's exports read alone are the ones a full extraction gives", {
  src <- .vs_repo(file.path(withr::local_tempdir(), "src"),
                  list("1.0" = "export(a)", "1.1" = c("export(a)", "export(b)"),
                       "1.2" = c("exportPattern(\"^z\")", "export(c)")))
  for (ver in c("1.0", "1.1", "1.2")) {
    ref <- .vs_ref(ver)
    tmp <- withr::local_tempdir()
    files <- extract_version(src, ref, tmp)
    ctx <- build_context("p", ver, ref, "2026-01-01", files,
                         function(path) paste(readLines(file.path(tmp, path), warn = FALSE), collapse = "\n"))
    full <- ctx$namespace$exports %||% character(0L)
    expect_true(length(full) > 0L)
    expect_identical(.exports_at(src, ref, "p", ver), full)
  }
})
test_that("a version without a NAMESPACE exports nothing", {
  repo <- .vs_repo(file.path(withr::local_tempdir(), "src"), list("0.1" = NULL))
  expect_identical(.exports_at(repo, .vs_ref("0.1"), "p", "0.1"), character(0L))
})
test_that("a ref git cannot read fails as an extraction does", {
  src <- .vs_repo(file.path(withr::local_tempdir(), "src"), list("1.0" = "export(a)"))
  expect_error(.exports_at(src, "no-such-ref", "p", "x"), class = "extract_failure")
})

# ---- The stored state a plan reads ------------------------------------------

# The code database, which holds the text history too, in a fresh directory.
.vs_db <- function(frame = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = frame)
  con <- open_or_init_db(file.path(dir, DB_FILENAME))
  open_or_init_release_text_db(NULL, con = con)
  withr::defer(DBI::dbDisconnect(con), envir = frame)
  con
}

test_that("the prior holds a package's stored rows, API rows, state rows and text readings", {
  .vs_stub()
  root <- withr::local_tempdir()
  src <- .vst_source(root, "vstpkg", c("1.0", "1.1", "1.2"))
  out <- withr::local_tempdir()
  .vst_run(root, out, "vstpkg", "1.2")
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, DB_FILENAME))
  withr::defer(DBI::dbDisconnect(con))
  p <- .prior_state(con, con, "vstpkg", "0.4.0-test")[["vstpkg"]]
  o <- order(numeric_version(p$stored$version))
  expect_identical(p$stored$version[o], c("1.0", "1.1", "1.2"))
  expect_identical(as.logical(p$stored$latest[o]), c(FALSE, FALSE, TRUE))
  expect_false(any(as.logical(p$stored$n_fns_r_null)))
  expect_identical(sort(p$state$commit_sha), sort(.vst_rev_parse(src, .vst_branch(c("1.0", "1.1", "1.2")))))
  expect_identical(p$state$rows_generation, rep(1L, 3L))
  expect_identical(sort(p$api$version), sort(c("1.0", "1.1", "1.2")))
  expect_identical(p$class, "0.4.0-test")
  lv <- list_versions(src)
  expect_false(any(.read_plan(lv, .version_trees(src, lv$commit), p)$read))
})
test_that("a package with nothing stored gets an empty prior, and every version reads as new", {
  .vs_stub()
  root <- withr::local_tempdir()
  src <- .vst_source(root, "vstpkg", c("1.0", "1.1"))
  out <- withr::local_tempdir()
  .vst_run(root, out, "vstpkg", "1.1")
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, DB_FILENAME))
  withr::defer(DBI::dbDisconnect(con))
  prior <- .prior_state(con, con, c("vstpkg", "newpkg"), "0.4.0-test")
  expect_named(prior, c("vstpkg", "newpkg"))
  e <- prior[["newpkg"]]
  expect_identical(vapply(e[c("stored", "api", "state")], nrow, integer(1L)),
                   c(stored = 0L, api = 0L, state = 0L))
  expect_identical(names(e$state), setdiff(.VERSION_STATE_COLS, "package"))
  expect_identical(e$text_ok, character(0L))
  lv <- list_versions(src)
  expect_identical(.read_plan(lv, .version_trees(src, lv$commit), e)$reason,
                   rep("new", nrow(lv)))
})
test_that("a summary column the database lacks reads as NULL", {
  con <- .vs_db()
  DBI::dbWriteTable(con, SUMMARY_TABLE, data.frame(
    package = "p", version = c("1.0", "1.1"), analyzer_version = "0.5.2",
    released = "2026-01-01", stringsAsFactors = FALSE))
  p <- .prior_state(con, con, "p", "0.5.2")[["p"]]
  expect_identical(p$stored$version, c("1.0", "1.1"))
  expect_identical(p$stored$released, c("2026-01-01", "2026-01-01"))
  expect_true(all(is.na(p$stored$detail_scanned)))
  expect_true(all(is.na(p$stored$datasets_scanned)))
  expect_false(any(p$stored$latest))
  expect_true(all(as.logical(p$stored$n_fns_r_null)))
  # With state rows matching the listing, the metrics rule is the first one they fail.
  DBI::dbAppendTable(con, VERSION_STATE_TABLE, data.frame(
    package = "p", version = c("1.0", "1.1"), commit_sha = c("c1", "c2"),
    tree_sha = c("t1", "t2"), rows_generation = 1L, stringsAsFactors = FALSE))
  p <- .prior_state(con, con, "p", "0.5.2")[["p"]]
  expect_identical(.vs_plan(c("1.0", "1.1"), p)$reason, c("metrics", "metrics"))
})
test_that("only text readings by 0.5.0 or later count", {
  con <- .vs_db()
  DBI::dbAppendTable(con, RELEASE_TEXT_VERSIONS_TABLE, data.frame(
    package = "p", version = c("1.0", "1.1"), analyzer_version = c("0.4.0", "0.5.2"),
    n_fields = 5L, has_release_notes = 0L, read_at = "2026-09-01T00:00:00Z",
    stringsAsFactors = FALSE))
  p <- .prior_state(con, con, "p", "0.5.2")[["p"]]
  expect_identical(p$text_ok, "1.1")
})
test_that("the prior is the same whatever the chunk size", {
  con <- .vs_db()
  for (k in c("a", "b", "c")) {
    DBI::dbWriteTable(con, SUMMARY_TABLE, data.frame(
      package = k, version = c("1.0", "1.1"), analyzer_version = "0.5.2",
      stringsAsFactors = FALSE), append = (k != "a"))
  }
  DBI::dbAppendTable(con, VERSION_STATE_TABLE, data.frame(
    package = "b", version = "1.1", commit_sha = "c1", stringsAsFactors = FALSE))
  DBI::dbAppendTable(con, RELEASE_TEXT_VERSIONS_TABLE, data.frame(
    package = "c", version = "1.0", analyzer_version = "0.5.2", n_fields = 5L,
    stringsAsFactors = FALSE))
  pkgs <- c("a", "b", "c", "d")
  expect_identical(.prior_state(con, con, pkgs, "0.5.2", chunk = 1L),
                   .prior_state(con, con, pkgs, "0.5.2"))
})

# ---- A walk with a prior ------------------------------------------------------

# One run over `versions` under the stub, and the prior it leaves.
.vs_walked <- function(versions = c("1.0", "1.1", "1.2"), frame = parent.frame()) {
  .vs_stub(frame = frame)
  root <- withr::local_tempdir(.local_envir = frame)
  src  <- .vst_source(root, "vstpkg", versions)
  out  <- withr::local_tempdir(.local_envir = frame)
  .vst_run(root, out, "vstpkg", versions[[length(versions)]])
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, DB_FILENAME))
  on.exit(DBI::dbDisconnect(con))
  list(root = root, src = src, prior = .prior_state(con, con, "vstpkg", "0.4.0-test")[["vstpkg"]])
}

# A fresh clone of the fixture's source, as the next run makes it.
.vs_clone <- function(w, frame = parent.frame()) {
  dest <- file.path(withr::local_tempdir(.local_envir = frame), "vstpkg")
  stopifnot(isTRUE(clone_package("vstpkg", dest, base = w$root)))
  dest
}

.vs_no_rownames <- function(df) {
  rownames(df) <- NULL
  df
}

# Calls to .exports_at() in the calling test, by version.
.vs_spy_exports <- function(frame = parent.frame()) {
  seen <- new.env()
  seen$v <- character(0L)
  real <- .exports_at
  .local_global(".exports_at", function(repo, ref, package, version) {
    seen$v <- c(seen$v, version)
    real(repo, ref, package, version)
  }, frame = frame)
  seen
}

test_that("a new release is read alone, and the rest come back as a full walk's derived columns", {
  w <- .vs_walked(); p <- w$prior
  .vst_release(w$src, "vstpkg", "1.3", "2025-05-01T00:00:00Z")
  clone <- .vs_clone(w)
  res  <- analyze_package(clone, "vstpkg", prior = p)
  full <- analyze_package(clone, "vstpkg")
  expect_true(res$skip$partial); expect_false(res$skip$unchanged)
  expect_identical(res$summary$version, "1.3")
  expect_identical(res$skip$row_keys, "1.3"); expect_identical(res$skip$api_keys, "1.3")
  expect_identical(names(res$skip$derived), c("package", "version", .CROSS_VERSION_COLS))
  expect_identical(res$skip$derived$version, c("1.0", "1.1", "1.2"))
  k <- match(res$skip$derived$version, full$summary$version)
  expect_identical(.vs_no_rownames(res$skip$derived[, .CROSS_VERSION_COLS]),
                   .vs_no_rownames(full$summary[k, .CROSS_VERSION_COLS]))
  expect_identical(.vs_no_rownames(res$summary),
                   .vs_no_rownames(full$summary[full$summary$version == "1.3", ]))
  expect_identical(.vs_no_rownames(res$api), .vs_no_rownames(full$api[full$api$version == "1.3", ]))
  # A release's churn is its diff from the release before, so only 1.3's is new.
  expect_identical(unique(res$churn$version), "1.3")
  expect_identical(.vs_no_rownames(res$churn),
                   .vs_no_rownames(full$churn[full$churn$version == "1.3", ]))
  expect_identical(res$state$version, "1.3")
  expect_identical(res$state$rows_generation, ROWS_GENERATION)
})
test_that("a skipped release carries its stored deprecation signals and API row into the package columns", {
  w <- .vs_walked(); p <- w$prior
  .vst_release(w$src, "vstpkg", "1.3", "2025-05-01T00:00:00Z")
  clone <- .vs_clone(w)
  full <- analyze_package(clone, "vstpkg")
  pkg_cols <- c("exports_removed_n", "cold_removal_rate", "deprecation_infrastructure_maturity")
  # 1.2 uses lifecycle and warned about a, which 1.3 drops; 1.2 dropped b cold.
  expect_identical(full$summary$deprecation_infrastructure_maturity[[4L]], 2L)
  expect_identical(full$summary$cold_removal_rate[[4L]], 0.5)
  res <- analyze_package(clone, "vstpkg", prior = p)
  expect_identical(.vs_no_rownames(res$summary[, pkg_cols]),
                   .vs_no_rownames(full$summary[4L, pkg_cols]))
  # Stored values that a read would not give are the ones the walk takes.
  p$state$deprecated <- "[]"
  p$state$uses_lifecycle <- 0L
  p$api$exports_removed[p$api$version == "1.2"] <- '["b","q"]'
  res <- analyze_package(clone, "vstpkg", prior = p)
  expect_identical(res$summary$deprecation_infrastructure_maturity, 0L)
  expect_identical(res$skip$derived$exports_removed_n[res$skip$derived$version == "1.2"], 2L)
  expect_identical(res$summary$cold_removal_rate, 1)
})
test_that("a moved release is read, and its successor's diff is redone from the two NAMESPACE files", {
  w <- .vs_walked(); p <- w$prior; src <- w$src
  .vst_move_branch(src, "1.1")
  # A stored diff of 1.2 that the redo must not reuse.
  p$api$exports_removed[p$api$version == "1.2"] <- '["zz"]'
  clone <- .vs_clone(w)
  seen <- .vs_spy_exports()
  res  <- analyze_package(clone, "vstpkg", prior = p)
  full <- analyze_package(clone, "vstpkg")
  expect_identical(res$skip$plan$reason, c(NA, "moved", NA))
  expect_identical(res$skip$plan$pred_changed, c(FALSE, FALSE, TRUE))
  expect_identical(res$summary$version, "1.1")
  # 1.0's exports for the diff of 1.1 read after it, then 1.2's for its own.
  expect_identical(seen$v, c("1.0", "1.2"))
  expect_setequal(res$skip$api_keys, c("1.1", "1.2"))
  expect_identical(res$skip$row_keys, "1.1")
  expect_identical(.vs_no_rownames(res$api),
                   .vs_no_rownames(full$api[full$api$version %in% c("1.1", "1.2"), ]))
  expect_identical(res$api$exports_removed[res$api$version == "1.2"], '["b"]')
  # 1.2's churn is a diff from 1.1's tip, which moved.
  expect_identical(.vs_no_rownames(res$churn),
                   .vs_no_rownames(full$churn[full$churn$version %in% c("1.1", "1.2"), ]))
  expect_true("1.2" %in% res$churn$version)
  st <- res$state[res$state$version == "1.2", ]
  expect_identical(st$prev_commit, .vst_rev_parse(src, .vst_branch("1.1")))
  for (cn in c("commit_sha", "tree_sha", "deprecated", "uses_lifecycle", "read_at",
               "rows_generation")) {
    expect_identical(st[[cn]], p$state[[cn]][p$state$version == "1.2"], info = cn)
  }
})
test_that("a package with nothing to read comes back unchanged", {
  w <- .vs_walked()
  res <- analyze_package(.vs_clone(w), "vstpkg", prior = w$prior)
  expect_true(res$skip$unchanged); expect_false(res$skip$partial)
  expect_false(any(res$skip$plan$read))
  expect_identical(nrow(res$summary), 0L)
  frames <- c("churn", "api", "state", "functions", "edges", "datasets")
  expect_identical(vapply(res[frames], nrow, integer(1L)), stats::setNames(integer(6L), frames))
  expect_true(all(vapply(res$text, nrow, integer(1L)) == 0L))
  expect_identical(res$skip$row_keys, character(0L)); expect_identical(res$skip$api_keys, character(0L))
  expect_identical(res$skip$derived$version, c("1.0", "1.1", "1.2"))
})
test_that("an empty listing deletes nothing", {
  repo <- withr::local_tempdir()
  .vst_git(repo, "init", "-q")
  writeLines("x", file.path(repo, "README"))
  .vst_git(repo, "add", "-A")
  .vst_git(repo, "commit", "-q", "-m", "init")
  res <- analyze_package(repo, "vstpkg", prior = .vs_prior(c("1.0", "1.1")))
  expect_identical(res$skip$row_keys, character(0L)); expect_true(res$skip$unchanged)
  expect_identical(res$skip$api_keys, character(0L))
  expect_identical(res$skip$newest, NA_character_)
  expect_identical(names(res$skip$derived), c("package", "version", .CROSS_VERSION_COLS))
})
test_that("a release gone from the listing is deleted with its rows", {
  w <- .vs_walked()
  .vst_git(w$src, "branch", "-D", .vst_branch("1.0"))
  clone <- .vs_clone(w)
  res  <- analyze_package(clone, "vstpkg", prior = w$prior)
  full <- analyze_package(clone, "vstpkg")
  expect_true("1.0" %in% res$skip$row_keys); expect_true("1.0" %in% res$skip$api_keys)
  expect_false(res$skip$unchanged); expect_true(res$skip$partial)
  expect_identical(res$skip$plan$version, c("1.1", "1.2"))
  # 1.1 is now first, so its diff is from nothing; 1.2 keeps its stored churn.
  expect_identical(res$skip$plan$pred_changed, c(TRUE, FALSE))
  expect_identical(.vs_no_rownames(res$churn),
                   .vs_no_rownames(full$churn[full$churn$version == "1.1", ]))
  expect_identical(.vs_no_rownames(res$api), .vs_no_rownames(full$api[full$api$version == "1.1", ]))
})
test_that("a redone diff after a release skipped with its stored diff reads that release's exports first", {
  w <- .vs_walked()
  .vst_git(w$src, "branch", "-D", .vst_branch("1.1"))
  clone <- .vs_clone(w)
  seen <- .vs_spy_exports()
  res  <- analyze_package(clone, "vstpkg", prior = w$prior)
  full <- analyze_package(clone, "vstpkg")
  expect_identical(res$skip$plan$pred_changed, c(FALSE, TRUE))
  expect_identical(nrow(res$summary), 0L)
  expect_identical(seen$v, c("1.0", "1.2"))
  expect_identical(.vs_no_rownames(res$api), .vs_no_rownames(full$api[full$api$version == "1.2", ]))
  expect_identical(.vs_no_rownames(res$churn),
                   .vs_no_rownames(full$churn[full$churn$version == "1.2", ]))
  expect_identical(res$state$prev_version, "1.0")
  expect_identical(res$skip$row_keys, "1.1"); expect_setequal(res$skip$api_keys, c("1.1", "1.2"))
})
test_that("without a prior the result has no skip part and every release is read", {
  .vs_stub()
  src <- .vst_source(withr::local_tempdir(), "vstpkg", c("1.0", "1.1", "1.2"))
  res <- analyze_package(src, "vstpkg")
  expect_null(res$skip); expect_false("skip" %in% names(res))
  expect_identical(nrow(res$summary), 3L)
  expect_identical(res$binary_versions, c("1.0", "1.1", "1.2"))
})
test_that("a walk with a prior counts the releases it reads", {
  w <- .vs_walked()
  .vst_move_branch(w$src, "1.1")
  clone <- .vs_clone(w)
  .tally_reset()
  withr::defer(.tally_reset())
  analyze_package(clone, "vstpkg", prior = w$prior)
  tally <- .tally_snapshot()
  expect_identical(tally$versions_read, 1)
  expect_true(tally$skip_s >= 0)
})
test_that("the read marker is the stored newest's dataset marker when the newest is skipped", {
  w <- .vs_walked()
  .vst_move_branch(w$src, "1.1")
  clone <- .vs_clone(w)
  # The stub no longer reads 1.2, so a read of it would leave no dataset marker.
  .vs_stub(c("1.0", "1.1"))
  full <- suppressWarnings(analyze_package(clone, "vstpkg"))
  expect_false(isTRUE(full$summary$datasets_scanned[[3L]]))
  res <- analyze_package(clone, "vstpkg", prior = w$prior)
  expect_identical(res$skip$newest, "1.2")
  expect_false(res$skip$newest_read); expect_true(res$skip$read_marker)
})
test_that("assessed_at and assessed_with are not among the columns written back", {
  w <- .vs_walked()
  .vst_release(w$src, "vstpkg", "1.3", "2025-05-01T00:00:00Z")
  res <- analyze_package(.vs_clone(w), "vstpkg", prior = w$prior)
  expect_identical(nrow(res$skip$derived), 3L)
  expect_true(all(c("assessed_at", "assessed_with") %in% names(res$summary)))
  expect_false(any(c("assessed_at", "assessed_with") %in% names(res$skip$derived)))
})
test_that("functions and edges come from every release read, and from no release skipped", {
  w <- .vs_walked()
  .vst_move_branch(w$src, "1.1")
  clone <- .vs_clone(w)
  .vs_stub(detail = TRUE)
  full <- analyze_package(clone, "vstpkg")
  expect_identical(full$functions$version, c("1.0", "1.1", "1.2"))
  expect_identical(full$edges$version, c("1.0", "1.1", "1.2"))
  res <- analyze_package(clone, "vstpkg", prior = w$prior)
  expect_identical(res$summary$version, "1.1"); expect_true(res$skip$partial)
  expect_identical(.vs_no_rownames(res$functions),
                   .vs_no_rownames(full$functions[full$functions$version == "1.1", ]))
  expect_identical(.vs_no_rownames(res$edges),
                   .vs_no_rownames(full$edges[full$edges$version == "1.1", ]))
  expect_identical(.vs_no_rownames(res$datasets),
                   .vs_no_rownames(full$datasets[full$datasets$version == "1.1", ]))
  expect_identical(res$datasets$is_current, 0L)
})

# git as it is, except that the churn diff ending at `commit` exits as a timeout does.
.vs_fail_diff <- function(commit, frame = parent.frame()) {
  real <- Sys.which("git")
  dir  <- withr::local_tempdir(.local_envir = frame)
  writeLines(c("#!/bin/sh",
               sprintf('case "$*" in *"diff --numstat "*"..%s") exit 124 ;; esac', commit),
               sprintf('exec %s "$@"', shQuote(real))), file.path(dir, "git"))
  Sys.chmod(file.path(dir, "git"), mode = "0755")
  withr::local_envvar(PATH = paste(dir, Sys.getenv("PATH"), sep = .Platform$path.sep),
                      .local_envir = frame)
}

test_that("a release read whose churn diff git did not finish gets no state row, with a prior or without", {
  w <- .vs_walked()
  .vst_release(w$src, "vstpkg", "1.3", "2025-05-01T00:00:00Z")
  clone <- .vs_clone(w)
  .vs_fail_diff(.vst_rev_parse(w$src, .vst_branch("1.3")))
  res <- analyze_package(clone, "vstpkg", prior = w$prior)
  expect_identical(res$summary$version, "1.3")
  expect_identical(res$skip$row_keys, "1.3"); expect_identical(res$skip$api_keys, "1.3")
  expect_identical(nrow(res$churn), 0L)
  expect_identical(nrow(res$state), 0L)
  full <- analyze_package(clone, "vstpkg")
  expect_identical(full$summary$version, c("1.0", "1.1", "1.2", "1.3"))
  expect_false("1.3" %in% full$churn$version)
  expect_identical(full$state$version, c("1.0", "1.1", "1.2"))
})
test_that("a redone diff whose churn git did not finish gets no state row", {
  w <- .vs_walked()
  .vst_move_branch(w$src, "1.1")
  clone <- .vs_clone(w)
  .vs_fail_diff(.vst_rev_parse(w$src, .vst_branch("1.2")))
  res <- analyze_package(clone, "vstpkg", prior = w$prior)
  expect_identical(res$skip$plan$pred_changed, c(FALSE, FALSE, TRUE))
  expect_setequal(res$skip$api_keys, c("1.1", "1.2"))
  expect_identical(unique(res$churn$version), "1.1")
  expect_identical(res$state$version, "1.1")
})
