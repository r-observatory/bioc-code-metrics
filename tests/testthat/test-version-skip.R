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

# ---- run_update with the version skip ------------------------------------------

test_that("the version skip is off for VERSION_SKIP 0, false, off or no in any case, and on otherwise", {
  for (v in c("0", " 0 ", "false", "FALSE", "Off", " no\n", "NO")) {
    expect_false(.version_skip_on(v), info = v)
  }
  for (v in c("", " ", "1", "true", "on", "yes", "nope", "00")) {
    expect_true(.version_skip_on(v), info = v)
  }
})
test_that("a skip run after a new release branch writes the rows a full walk writes", {
  .vs_stub()
  root <- withr::local_tempdir()
  src <- .vst_source(root, "vstpkg", c("1.0", "1.1", "1.2"))
  skip <- withr::local_tempdir(); full <- withr::local_tempdir()
  .vs_run(skip, root, c(vstpkg = "1.2"))
  .vst_release(src, "vstpkg", "1.3", "2025-05-01T00:00:00Z")
  .vs_run(skip, root, c(vstpkg = "1.3"))
  .vs_run(full, root, c(vstpkg = "1.3"), skip = FALSE)
  expect_identical(.vs_rows(skip, "vstpkg"), .vs_rows(full, "vstpkg"))
  rs <- jsonlite::fromJSON(file.path(skip, "run-status.json"))
  expect_identical(as.integer(rs$worker_phases$versions_read), 1L)
  expect_identical(as.integer(rs$n_versions), 1L)
})
test_that("a package queued with nothing to read is ok, not fresh, and does not publish", {
  .vs_stub()
  root <- withr::local_tempdir()
  .vst_source(root, "vstpkg", c("1.0", "1.1", "1.2"))
  out <- withr::local_tempdir()
  .vs_run(out, root, c(vstpkg = "1.2"))
  before <- .package_rows(out, "vstpkg")
  # Stored up to 1.2; the universe names 1.3, which the mirror does not have yet.
  log <- attr(.vs_run(out, root, c(vstpkg = "1.3")), "log")
  rs <- jsonlite::fromJSON(file.path(out, "run-status.json"))
  expect_identical(c(rs$n_shard, rs$n_fresh, rs$shard_failures), c(1L, 0L, 0L))
  expect_false(rs$changed)
  expect_identical(.package_rows(out, "vstpkg"), before)
  expect_true(any(grepl("^shard done in [0-9]+s: 1/1 ok, 0 failed; 0 versions", log)))
})
test_that("a failure verdict cleared on a package with nothing to read is published once", {
  .vs_stub()
  root <- withr::local_tempdir()
  .vst_source(root, "vstpkg", c("1.0", "1.1", "1.2"))
  out <- withr::local_tempdir()
  .vs_run(out, root, c(vstpkg = "1.2"))
  # A clone that failed once, at the release the mirror does not have yet.
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, DB_FILENAME))
  .record_failure(con, "vstpkg", "clone", "0.4.0-test", WORKER_TIMEOUT, NA_character_,
                  1, "clone failed", "1.3")
  DBI::dbDisconnect(con)
  failures <- function() .vst_query(out, "SELECT package FROM bioc_metrics_failures")$package
  expect_identical(failures(), "vstpkg")
  .vs_run(out, root, c(vstpkg = "1.3"))
  rs <- jsonlite::fromJSON(file.path(out, "run-status.json"))
  expect_identical(c(rs$n_shard, rs$n_fresh), c(1L, 0L))
  expect_true(rs$changed)
  expect_identical(failures(), character(0L))
  .vs_run(out, root, c(vstpkg = "1.3"))
  expect_false(jsonlite::fromJSON(file.path(out, "run-status.json"))$changed)
})
test_that("a requeued package that lands in a later shard still reads every version", {
  .vs_stub()
  root <- withr::local_tempdir()
  apkg <- .vst_source(root, "apkg", c("1.0", "1.1", "1.2"))
  .vst_source(root, "vstpkg", c("1.0", "1.1", "1.2"))
  out <- withr::local_tempdir()
  .vs_run(out, root, c(apkg = "1.2", vstpkg = "1.2"))
  # apkg and vstpkg stored; apkg gains a release so both are queued, apkg sorting first.
  .vst_release(apkg, "apkg", "1.3", "2025-05-01T00:00:00Z")
  latest <- c(apkg = "1.3", vstpkg = "1.2")
  .vs_run(out, root, latest, requeue = "vstpkg", shard_size = 1L)   # walks apkg
  rs <- jsonlite::fromJSON(file.path(out, "run-status.json"))
  expect_identical(as.integer(rs$worker_phases$versions_read), 1L)
  .vs_run(out, root, latest, shard_size = 1L)                        # walks vstpkg, no flag
  rs <- jsonlite::fromJSON(file.path(out, "run-status.json"))
  expect_identical(as.integer(rs$worker_phases$versions_read), 3L)
})
test_that("--bootstrap and VERSION_SKIP off read every version and give a full walk's rows", {
  .vs_stub()
  root <- withr::local_tempdir()
  src <- .vst_source(root, "vstpkg", c("1.0", "1.1", "1.2"))
  boot <- withr::local_tempdir(); off <- withr::local_tempdir(); full <- withr::local_tempdir()
  .vs_run(boot, root, c(vstpkg = "1.2")); .vs_run(off, root, c(vstpkg = "1.2"))
  .vst_release(src, "vstpkg", "1.3", "2025-05-01T00:00:00Z")
  .vs_run(boot, root, c(vstpkg = "1.3"), force_full = TRUE)
  .vs_run(off, root, c(vstpkg = "1.3"), skip = FALSE)
  .vs_run(full, root, c(vstpkg = "1.3"), skip = FALSE)
  rs_boot <- jsonlite::fromJSON(file.path(boot, "run-status.json"))
  rs_off  <- jsonlite::fromJSON(file.path(off, "run-status.json"))
  expect_identical(as.integer(rs_boot$worker_phases$versions_read), 4L)
  expect_identical(as.integer(rs_off$worker_phases$versions_read), 4L)
  expect_identical(.vs_rows(boot, "vstpkg"), .vs_rows(full, "vstpkg"))
  expect_identical(.vs_rows(off, "vstpkg"), .vs_rows(full, "vstpkg"))
  expect_identical(unique(.vst_state(off, "vstpkg")$rows_generation), ROWS_GENERATION)
})
test_that("a shard that walks one package whole and skips versions of another writes both as full walks would", {
  # Detail records too, so both packages have latest text.
  .vs_stub(detail = TRUE)
  root <- withr::local_tempdir()
  src <- .vst_source(root, "vstpkg", c("1.0", "1.1", "1.2"))
  skip <- withr::local_tempdir(); full <- withr::local_tempdir()
  .vs_run(skip, root, c(vstpkg = "1.2"))
  # newpkg is new and read whole; vstpkg gains 1.3 and reads it alone.
  .vst_source(root, "newpkg", c("1.0", "1.1"))
  .vst_release(src, "vstpkg", "1.3", "2025-05-01T00:00:00Z")
  latest <- c(newpkg = "1.1", vstpkg = "1.3")
  .vs_run(skip, root, latest)
  .vs_run(full, root, latest, skip = FALSE)
  rs <- jsonlite::fromJSON(file.path(skip, "run-status.json"))
  expect_identical(as.integer(rs$worker_phases$versions_read), 3L)
  for (p in c("newpkg", "vstpkg")) {
    rows <- .vs_rows(skip, p)
    expect_gt(nrow(rows[[paste(DB_FILENAME, DESCRIPTION_FIELDS_TABLE)]]), 0L)
    expect_identical(rows, .vs_rows(full, p), info = p)
  }
})
test_that("a package whose one read version fails keeps every stored row and reads it again next run", {
  .vs_stub()
  root <- withr::local_tempdir()
  src <- .vst_source(root, "vstpkg", c("1.0", "1.1", "1.2"))
  out <- withr::local_tempdir()
  .vs_run(out, root, c(vstpkg = "1.2"))
  before <- .package_rows(out, "vstpkg")
  .vst_release(src, "vstpkg", "1.3", "2025-05-01T00:00:00Z")
  # extract_version fails on release 1.3 for one run
  real <- extract_version
  local({
    .local_global("extract_version", function(repo, ref, dest, ...) {
      if (grepl("RELEASE_1_3$", ref)) stop(.extract_failure("archive", ref, 128L, "fatal: bad object"))
      real(repo, ref, dest, ...)
    })
    .vs_run(out, root, c(vstpkg = "1.3"))
  })
  rs <- jsonlite::fromJSON(file.path(out, "run-status.json"))
  expect_identical(as.integer(rs$shard_failures), 1L)
  expect_identical(as.integer(rs$worker_phases$versions_read), 1L)
  expect_identical(.package_rows(out, "vstpkg"), before)
  # the mock removed, the next run reads 1.3 alone
  .vs_run(out, root, c(vstpkg = "1.3"))
  rs <- jsonlite::fromJSON(file.path(out, "run-status.json"))
  expect_identical(as.integer(rs$shard_failures), 0L)
  expect_identical(as.integer(rs$worker_phases$versions_read), 1L)
  expect_identical(sort(.vst_query(out, "SELECT version FROM bioc_code_summary WHERE package = 'vstpkg'")$version),
                   c("1.0", "1.1", "1.2", "1.3"))
})
test_that("a pass that skips versions under the cap leaves the over-cap note, and a whole walk under it clears it", {
  .vs_stub()
  root <- withr::local_tempdir()
  src <- .vst_source(root, "vstpkg", c("1.0", "1.1", "1.2"))
  out <- withr::local_tempdir()
  .vs_run(out, root, c(vstpkg = "1.2"))
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, DB_FILENAME))
  DBI::dbExecute(con, "INSERT INTO bioc_over_cap (package, elapsed_s) VALUES ('vstpkg', 3000)")
  DBI::dbDisconnect(con)
  noted <- function() .vst_query(out, "SELECT package FROM bioc_over_cap")$package
  .vst_release(src, "vstpkg", "1.3", "2025-05-01T00:00:00Z")
  .vs_run(out, root, c(vstpkg = "1.3"))
  rs <- jsonlite::fromJSON(file.path(out, "run-status.json"))
  expect_identical(as.integer(rs$worker_phases$versions_read), 1L)
  expect_identical(noted(), "vstpkg")
  # --requeue=over_cap still names it, and that walk reads every version.
  .vs_run(out, root, c(vstpkg = "1.3"), requeue = "over_cap")
  rs <- jsonlite::fromJSON(file.path(out, "run-status.json"))
  expect_identical(as.integer(rs$worker_phases$versions_read), 4L)
  expect_identical(noted(), character(0L))
})
test_that("a skipped row keeps its assessed_at and assessed_with", {
  .vs_stub()
  root <- withr::local_tempdir()
  src <- .vst_source(root, "vstpkg", c("1.0", "1.1", "1.2"))
  out <- withr::local_tempdir()
  # Sys.Date() is 2026-10-01 for the first run and 2026-10-20 for the second,
  # and the first runs under another R.
  local({
    .local_global("Sys.Date", function() as.Date("2026-10-01"))
    .local_global("getRversion", function() numeric_version("4.5.0"))
    .vs_run(out, root, c(vstpkg = "1.2"))
  })
  .vst_release(src, "vstpkg", "1.3", "2025-05-01T00:00:00Z")
  local({
    .local_global("Sys.Date", function() as.Date("2026-10-20"))
    .vs_run(out, root, c(vstpkg = "1.3"))
  })
  got <- .vst_query(out, paste("SELECT version, assessed_at, assessed_with FROM bioc_code_summary",
                               "WHERE package = 'vstpkg'"))
  k <- match(c("1.0", "1.1", "1.2", "1.3"), got$version)
  expect_identical(got$assessed_at[k], c(rep("2026-10-01", 3L), "2026-10-20"))
  expect_identical(got$assessed_with[k],
                   c(rep("bioc-code-metrics; R 4.5.0", 3L),
                     paste0("bioc-code-metrics; R ", getRversion())))
})
test_that("the read marker clears read attempts when the newest is skipped", {
  .vs_stub()
  root <- withr::local_tempdir()
  .vst_source(root, "vstpkg", c("1.0", "1.1", "1.2"))
  out <- withr::local_tempdir()
  .vs_run(out, root, c(vstpkg = "1.2"))
  # One attempt that did not read the package, and a 1.1 row the backfill queues.
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, DB_FILENAME))
  .record_analyzer_read_attempt(con, "vstpkg", "0.4.0-test")
  DBI::dbExecute(con, paste("UPDATE bioc_code_summary SET n_fns_r = NULL",
                            "WHERE package = 'vstpkg' AND version = '1.1'"))
  DBI::dbDisconnect(con)
  attempts <- function() .vst_query(out, "SELECT COUNT(*) n FROM bioc_analyzer_read_attempts")$n
  expect_identical(attempts(), 1L)
  .vs_run(out, root, c(vstpkg = "1.2"))
  rs <- jsonlite::fromJSON(file.path(out, "run-status.json"))
  expect_identical(c(rs$n_shard, rs$n_fresh), c(1L, 1L))
  # 1.1 alone is read, and the newest is skipped.
  expect_identical(as.integer(rs$worker_phases$versions_read), 1L)
  expect_false(anyNA(.vst_query(out, "SELECT n_fns_r FROM bioc_code_summary")$n_fns_r))
  expect_identical(attempts(), 0L)
})
test_that("a run with the skip off says so where the version skip line goes, and one with it on does not", {
  .vs_stub()
  root <- withr::local_tempdir()
  .vst_source(root, "vstpkg", c("1.0", "1.1"))
  out <- withr::local_tempdir()
  log <- attr(.vs_run(out, root, c(vstpkg = "1.1"), skip = FALSE), "log")
  expect_identical(grep("^version skip", log, value = TRUE), "version skip: off")
  expect_identical(grep("^version skip: off$", log), grep("^worker time:", log) + 1L)
  log <- attr(.vs_run(out, root, c(vstpkg = "1.1")), "log")
  expect_identical(grep("^version skip: off$", log), integer(0L))
})

# ---- The skip against upstream changes ------------------------------------------

# A copy of out_dir, for the full walk a skip run is compared with.
.vs_copy <- function(out_dir, frame = parent.frame()) {
  dest <- withr::local_tempdir(.local_envir = frame)
  file.copy(list.files(out_dir, full.names = TRUE, all.files = TRUE, no.. = TRUE),
            dest, recursive = TRUE)
  dest
}

.vs_exec <- function(out_dir, sql) {
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out_dir, DB_FILENAME))
  on.exit(DBI::dbDisconnect(con))
  for (s in sql) DBI::dbExecute(con, s)
  invisible(NULL)
}

.vs_versions_read <- function(out_dir) {
  as.integer(jsonlite::fromJSON(file.path(out_dir, "run-status.json"))$worker_phases$versions_read)
}

# Every read plan the calling test's walks make, in order; workers run in this process.
.vs_plans <- function(frame = parent.frame()) {
  seen <- new.env()
  seen$plans <- list()
  real <- .read_plan
  .local_global(".read_plan", function(...) {
    p <- real(...)
    seen$plans[[length(seen$plans) + 1L]] <- p
    p
  }, frame = frame)
  seen
}

# The dataset rows that name the newest: the links marked current, the
# identity row's release and the content that row points at.
.vs_links <- function(out_dir) {
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out_dir, DATA_DB_FILENAME))
  on.exit(DBI::dbDisconnect(con))
  c(current = DBI::dbGetQuery(con, paste("SELECT version FROM bioc_dataset_versions",
                                         "WHERE is_current = 1"))$version,
    identity = DBI::dbGetQuery(con, "SELECT current_version FROM bioc_datasets")$current_version,
    content = DBI::dbGetQuery(con, paste(
      "SELECT c.content_fp FROM bioc_datasets d JOIN bioc_dataset_contents c",
      "ON c.content_id = d.current_content_id"))$content_fp)
}

test_that("moved, deleted, out-of-class and requeued versions give a full walk's rows", {
  .vs_stub()
  root <- withr::local_tempdir()
  pkgs <- c("apkg", "bpkg", "cpkg", "dpkg")
  src  <- vapply(pkgs, function(p) .vst_source(root, p, c("1.0", "1.1", "1.2")), character(1L))
  skip <- withr::local_tempdir()
  .vs_run(skip, root, stats::setNames(rep("1.2", 4L), pkgs))
  full <- .vs_copy(skip)
  # apkg, bpkg and cpkg gain 1.3; dpkg is queued by a NULL n_fns_r on 1.0.
  for (p in pkgs[1:3]) .vst_release(src[[p]], p, "1.3", "2025-05-01T00:00:00Z")
  .vst_move_branch(src[["apkg"]], "1.1")
  .vst_git(src[["bpkg"]], "branch", "-D", .vst_branch("1.1"))
  .vst_move_branch(src[["dpkg"]], "1.2")
  for (out in c(skip, full)) {
    .vs_exec(out, c(
      paste("UPDATE bioc_code_summary SET analyzer_version = '0.3.0-test'",
            "WHERE package = 'bpkg' AND version = '1.0'"),
      "UPDATE bioc_code_summary SET n_fns_r = NULL WHERE package = 'dpkg' AND version = '1.0'"))
  }
  latest <- c(apkg = "1.3", bpkg = "1.3", cpkg = "1.3", dpkg = "1.2")
  seen <- .vs_plans()
  .vs_run(skip, root, latest, requeue = "cpkg")
  .vs_run(full, root, latest, requeue = "cpkg", skip = FALSE)
  plans <- stats::setNames(seen$plans[1:4], pkgs)
  expect_identical(plans$apkg$reason, c(NA, "moved", NA, "new"))
  expect_identical(plans$apkg$pred_changed, c(FALSE, FALSE, TRUE, FALSE))
  expect_identical(plans$bpkg$version, c("1.0", "1.2", "1.3"))
  expect_identical(plans$bpkg$reason, c("class", NA, "new"))
  expect_identical(plans$bpkg$pred_changed, c(FALSE, TRUE, FALSE))
  expect_identical(plans$cpkg$reason, c(rep("unrecorded", 3L), "new"))
  expect_identical(plans$dpkg$reason, c("metrics", NA, "moved"))
  # apkg reads 1.1 and 1.3, bpkg 1.0 and 1.3, cpkg all four, dpkg 1.0 and 1.2;
  # the full walk reads 14.
  expect_identical(.vs_versions_read(skip), 10L)
  expect_identical(.vs_versions_read(full), 14L)
  for (p in pkgs) expect_identical(.vs_rows(skip, p), .vs_rows(full, p), info = p)
})
test_that("dataset is_current flips and identity rows come only from a read newest", {
  .vs_stub(detail = TRUE)
  root <- withr::local_tempdir()
  src <- .vst_source(root, "vstpkg", c("1.0", "1.1", "1.2"))
  skip <- withr::local_tempdir()
  .vs_run(skip, root, c(vstpkg = "1.2"))
  expect_identical(.vs_links(skip), c(current = "1.2", identity = "1.2", content = "cf-1.2"))
  full <- .vs_copy(skip)
  .vst_release(src, "vstpkg", "1.3", "2025-05-01T00:00:00Z")
  .vs_run(skip, root, c(vstpkg = "1.3"))
  .vs_run(full, root, c(vstpkg = "1.3"), skip = FALSE)
  expect_identical(.vs_versions_read(skip), 1L)
  data_q <- function(sql) {
    con <- DBI::dbConnect(RSQLite::SQLite(), file.path(skip, DATA_DB_FILENAME))
    on.exit(DBI::dbDisconnect(con))
    DBI::dbGetQuery(con, sql)
  }
  expect_identical(data_q("SELECT version FROM bioc_dataset_versions WHERE is_current = 1")$version, "1.3")
  expect_identical(data_q("SELECT COUNT(*) n FROM bioc_dataset_versions")$n, 4L)
  expect_identical(data_q("SELECT current_version FROM bioc_datasets")$current_version, "1.3")
  expect_identical(.vs_links(skip), c(current = "1.3", identity = "1.3", content = "cf-1.3"))
  expect_identical(.vs_rows(skip, "vstpkg"), .vs_rows(full, "vstpkg"))
  # A NULL n_fns_r on 1.1 reads 1.1 alone, and the newest is skipped.
  for (out in c(skip, full)) {
    .vs_exec(out, paste("UPDATE bioc_code_summary SET n_fns_r = NULL",
                        "WHERE package = 'vstpkg' AND version = '1.1'"))
  }
  identity <- function() .package_rows(skip, "vstpkg")[[paste(DATA_DB_FILENAME, "bioc_datasets")]]
  before <- identity()
  .vs_run(skip, root, c(vstpkg = "1.3"))
  .vs_run(full, root, c(vstpkg = "1.3"), skip = FALSE)
  expect_identical(.vs_versions_read(skip), 1L)
  expect_identical(.vs_links(skip), c(current = "1.3", identity = "1.3", content = "cf-1.3"))
  expect_identical(identity(), before)
  expect_identical(.vs_rows(skip, "vstpkg"), .vs_rows(full, "vstpkg"))
})
test_that("a deleted branch goes and the skipped newest keeps its current links", {
  .vs_stub(detail = TRUE)
  root <- withr::local_tempdir()
  src <- .vst_source(root, "vstpkg", c("1.0", "1.1", "1.2", "1.3"))
  skip <- withr::local_tempdir()
  .vs_run(skip, root, c(vstpkg = "1.3"))
  full <- .vs_copy(skip)
  # 1.1's rows in each table that holds them, the text history last.
  gone <- c(SUMMARY_TABLE, CHURN_TABLE, API_TABLE, "bioc_functions", "bioc_call_edges",
            VERSION_STATE_TABLE, "bioc_dataset_versions")
  kept <- c(DESCRIPTION_HISTORY_TABLE, RELEASE_TEXT_VERSIONS_TABLE)
  of_1_1 <- function() {
    data_con <- DBI::dbConnect(RSQLite::SQLite(), file.path(skip, DATA_DB_FILENAME))
    on.exit(DBI::dbDisconnect(data_con))
    vapply(c(gone, kept), function(tbl) {
      sql <- sprintf('SELECT COUNT(*) n FROM "%s" WHERE package = ? AND version = ?', tbl)
      key <- list("vstpkg", "1.1")
      if (tbl == "bioc_dataset_versions") DBI::dbGetQuery(data_con, sql, params = key)$n
      else .vst_query(skip, sql, params = key)$n
    }, integer(1L))
  }
  before <- of_1_1()
  expect_true(all(before > 0L))
  identity <- function() .package_rows(skip, "vstpkg")[[paste(DATA_DB_FILENAME, "bioc_datasets")]]
  identity_before <- identity()
  .vst_git(src, "branch", "-D", .vst_branch("1.1"))
  for (out in c(skip, full)) {
    .vs_exec(out, paste("UPDATE bioc_code_summary SET n_fns_r = NULL",
                        "WHERE package = 'vstpkg' AND version = '1.0'"))
  }
  seen <- .vs_plans()
  .vs_run(skip, root, c(vstpkg = "1.3"))
  .vs_run(full, root, c(vstpkg = "1.3"), skip = FALSE)
  # 1.0 is read; 1.2's diff is redone against 1.0; the newest is skipped.
  plan <- seen$plans[[1L]]
  expect_identical(plan$version, c("1.0", "1.2", "1.3"))
  expect_identical(plan$reason, c("metrics", NA, NA))
  expect_identical(plan$pred_changed, c(FALSE, TRUE, FALSE))
  expect_identical(.vs_versions_read(skip), 1L)
  after <- of_1_1()
  expect_identical(after[gone], stats::setNames(integer(length(gone)), gone))
  expect_identical(after[kept], before[kept])
  expect_identical(.vs_links(skip), c(current = "1.3", identity = "1.3", content = "cf-1.3"))
  expect_identical(identity(), identity_before)
  expect_identical(.vs_rows(skip, "vstpkg"), .vs_rows(full, "vstpkg"))
})
test_that("the first skip run on a database from before rows_generation skips its recorded versions", {
  .vs_stub()
  root <- withr::local_tempdir()
  src <- .vst_source(root, "vstpkg", c("1.0", "1.1", "1.2"))
  skip <- withr::local_tempdir()
  .vs_run(skip, root, c(vstpkg = "1.2"))
  # The table as it was before rows_generation, with the rows it held.
  kept <- paste(setdiff(.VERSION_STATE_COLS, "rows_generation"), collapse = ", ")
  .vs_exec(skip, c(
    sprintf('ALTER TABLE "%s" RENAME TO old_state', VERSION_STATE_TABLE),
    sprintf('CREATE TABLE "%s" (
      package TEXT NOT NULL, version TEXT NOT NULL,
      commit_sha TEXT, tree_sha TEXT, prev_version TEXT, prev_commit TEXT,
      deprecated TEXT, uses_lifecycle INTEGER, read_at TEXT,
      PRIMARY KEY (package, version)) WITHOUT ROWID', VERSION_STATE_TABLE),
    sprintf('INSERT INTO "%s" (%s) SELECT %s FROM old_state', VERSION_STATE_TABLE, kept, kept),
    "DROP TABLE old_state"))
  expect_false("rows_generation" %in% names(.vst_state(skip, "vstpkg")))
  expect_identical(nrow(.vst_state(skip, "vstpkg")), 3L)
  full <- .vs_copy(skip)
  .vst_release(src, "vstpkg", "1.3", "2025-05-01T00:00:00Z")
  .vs_run(skip, root, c(vstpkg = "1.3"))
  .vs_run(full, root, c(vstpkg = "1.3"), skip = FALSE)
  rs <- jsonlite::fromJSON(file.path(skip, "run-status.json"))
  expect_identical(as.integer(rs$worker_phases$versions_read), 1L)
  # The skipped rows keep their NULL generation, which counts as 1; a full walk writes 1.
  st <- .vst_state(skip, "vstpkg")
  expect_identical(st$rows_generation[st$version != "1.3"], rep(NA_integer_, 3L))
  expect_identical(st$rows_generation[st$version == "1.3"], ROWS_GENERATION)
  rows <- .vs_rows(skip, "vstpkg")
  k <- paste(DB_FILENAME, VERSION_STATE_TABLE)
  rows[[k]]$rows_generation[is.na(rows[[k]]$rows_generation)] <- 1L
  expect_identical(rows, .vs_rows(full, "vstpkg"))
})
test_that("a package read for a backfill marker leaves the queue", {
  .vs_stub()
  root <- withr::local_tempdir()
  .vst_source(root, "vstpkg", c("1.0", "1.1", "1.2"))
  out <- withr::local_tempdir()
  .vs_run(out, root, c(vstpkg = "1.2"))
  .vs_exec(out, "UPDATE bioc_code_summary SET n_fns_r = NULL WHERE package = 'vstpkg' AND version = '1.0'")
  seen <- .vs_plans()
  .vs_run(out, root, c(vstpkg = "1.2"))
  rs <- jsonlite::fromJSON(file.path(out, "run-status.json"))
  expect_identical(c(rs$n_shard, rs$n_fresh), c(1L, 1L))
  expect_identical(as.integer(rs$worker_phases$versions_read), 1L)
  expect_identical(seen$plans[[1L]]$reason, c("metrics", NA, NA))
  expect_false(anyNA(.vst_query(out, "SELECT n_fns_r FROM bioc_code_summary")$n_fns_r))
  .vs_run(out, root, c(vstpkg = "1.2"))
  expect_identical(jsonlite::fromJSON(file.path(out, "run-status.json"))$n_shard, 0L)
})

# ---- What a run says about the skip ----------------------------------------------

test_that("the version skip line counts versions read by reason, diffs redone and the packages passed in as reading nothing", {
  plans <- list(
    a = data.frame(version = c("1", "2", "3"), read = c(FALSE, FALSE, TRUE),
                   reason = c(NA, NA, "new"), pred_changed = FALSE),
    b = data.frame(version = c("1", "2"), read = c(TRUE, FALSE), reason = c("moved", NA),
                   pred_changed = c(FALSE, TRUE)),
    c = data.frame(version = "1", read = FALSE, reason = NA_character_, pred_changed = FALSE))
  expect_identical(.version_skip_line(plans, 1L),
    "version skip: read 2 of 6 listed versions (new 1, moved 1); diffs redone: 1; queued with nothing read: 1")
  expect_identical(.version_skip_line(list(), 0L),
    "version skip: read 0 of 0 listed versions (none); diffs redone: 0; queued with nothing read: 0")
})
test_that("a plan that reads nothing is not counted as queued with nothing read unless the caller says so", {
  gone_only <- list(a = data.frame(version = "1", read = FALSE, reason = NA_character_,
                                   pred_changed = FALSE))
  expect_match(.version_skip_line(gone_only, 0L), "queued with nothing read: 0$")
})
test_that("the version skip line lists reasons in rule order", {
  plans <- list(a = data.frame(version = c("1", "2", "3", "4"), read = TRUE,
                               reason = c("text", "moved", "new", "detail"),
                               pred_changed = FALSE))
  expect_match(.version_skip_line(plans, 0L), "(new 1, moved 1, detail 1, text 1)", fixed = TRUE)
})

# Two packages, each with its newest read, in databases the caller can alter.
.vs_invariants_dbs <- function(frame = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = frame)
  .vst_source(root, "apkg", c("1.0", "1.1"))
  .vst_source(root, "bpkg", c("1.0", "1.1"))
  out <- withr::local_tempdir(.local_envir = frame)
  .vs_run(out, root, c(apkg = "1.1", bpkg = "1.1"))
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, DB_FILENAME))
  data_con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, DATA_DB_FILENAME))
  withr::defer({ DBI::dbDisconnect(con); DBI::dbDisconnect(data_con) }, envir = frame)
  list(con = con, data_con = data_con)
}

test_that("the invariants are clean after a run", {
  .vs_stub()
  d <- .vs_invariants_dbs()
  expect_identical(.version_skip_invariants(d$con, d$data_con, c("apkg", "bpkg")),
                   c(latest_off = 0L, current_off = 0L))
})
test_that("the invariants find a second latest row and a current row off the newest", {
  .vs_stub()
  d <- .vs_invariants_dbs()
  DBI::dbExecute(d$con, "UPDATE bioc_code_summary SET latest_release_date = '2026-01-01'
                          WHERE package = 'apkg' AND version = '1.0'")
  expect_identical(DBI::dbGetQuery(d$data_con, "SELECT COUNT(*) n FROM bioc_dataset_versions
                                                 WHERE package = 'bpkg' AND version = '1.0'")$n > 0L, TRUE)
  DBI::dbExecute(d$data_con, "UPDATE bioc_dataset_versions SET is_current = 1
                               WHERE package = 'bpkg' AND version = '1.0'")
  expect_identical(.version_skip_invariants(d$con, d$data_con, c("apkg", "bpkg")),
                   c(latest_off = 1L, current_off = 1L))
  expect_identical(.version_skip_invariants(d$con, d$data_con, "bpkg"),
                   c(latest_off = 0L, current_off = 1L))
  expect_identical(.version_skip_invariants(d$con, d$data_con, "apkg"),
                   c(latest_off = 1L, current_off = 0L))
  expect_identical(.version_skip_invariants(d$con, d$data_con),
                   c(latest_off = 1L, current_off = 1L))
})
test_that("no packages written means no invariants broken", {
  .vs_stub()
  d <- .vs_invariants_dbs()
  expect_identical(.version_skip_invariants(d$con, d$data_con, character(0L)),
                   c(latest_off = 0L, current_off = 0L))
})

test_that("a skip run prints the version skip line and its invariants", {
  .vs_stub()
  root <- withr::local_tempdir()
  src <- .vst_source(root, "vstpkg", c("1.0", "1.1"))
  out <- withr::local_tempdir()
  .vs_run(out, root, c(vstpkg = "1.1"))
  .vst_release(src, "vstpkg", "1.2", "2025-05-01T00:00:00Z")
  log <- attr(.vs_run(out, root, c(vstpkg = "1.2")), "log")
  expect_true(any(grepl("^version skip: read 1 of 3 listed versions \\(new 1\\); diffs redone: 0; queued with nothing read: 0$", log)))
  expect_true(any(grepl("^version skip invariants: 0 of 1 written packages with other than one latest row, 0 with current rows off the newest$", log)))
  expect_identical(grep("^version skip: ", log), grep("^worker time:", log) + 1L)
})
test_that("a package queued with nothing read is counted on the skip line and has no written package", {
  .vs_stub()
  root <- withr::local_tempdir()
  .vst_source(root, "vstpkg", c("1.0", "1.1"))
  out <- withr::local_tempdir()
  .vs_run(out, root, c(vstpkg = "1.1"))
  log <- attr(.vs_run(out, root, c(vstpkg = "1.2")), "log")
  expect_true(any(grepl("^version skip: read 0 of 2 listed versions \\(none\\); diffs redone: 0; queued with nothing read: 1$", log)))
  expect_true(any(grepl("^version skip invariants: 0 of 0 written packages", log)))
})
test_that("a bootstrap run reads every version and prints no version skip read line", {
  .vs_stub()
  root <- withr::local_tempdir()
  .vst_source(root, "vstpkg", c("1.0", "1.1"))
  out <- withr::local_tempdir()
  log <- attr(.vs_run(out, root, c(vstpkg = "1.1"), force_full = TRUE), "log")
  expect_identical(grep("^version skip", log), integer(0L))
})
test_that("the code manifest and run status carry both invariants, the data manifest does not", {
  .vs_stub()
  root <- withr::local_tempdir()
  .vst_source(root, "vstpkg", c("1.0", "1.1"))
  out <- withr::local_tempdir()
  .local_global(".version_skip_invariants", function(con, data_con, pkgs = NULL) c(latest_off = 3L, current_off = 2L))
  .vs_run(out, root, c(vstpkg = "1.1"))
  b <- jsonlite::fromJSON(file.path(out, "code-manifest.json"))$bootstrap
  expect_identical(c(b$latest_off, b$current_off), c(3L, 2L))
  rs <- jsonlite::fromJSON(file.path(out, "run-status.json"))
  expect_identical(c(rs$latest_off, rs$current_off), c(3L, 2L))
  for (f in intersect(c("data-manifest.json", "text-manifest.json"), list.files(out))) {
    b <- jsonlite::fromJSON(file.path(out, f))$bootstrap
    expect_null(b$latest_off)
    expect_null(b$current_off)
  }
})
test_that("the published invariants cover the whole database, not the last shard", {
  .vs_stub()
  root <- withr::local_tempdir()
  .vst_source(root, "apkg", c("1.0", "1.1"))
  .vst_source(root, "bpkg", c("1.0", "1.1"))
  out <- withr::local_tempdir()
  latest <- c(apkg = "1.1", bpkg = "1.1")
  .vs_run(out, root, latest, shard_size = 1L)
  .vs_exec(out, "UPDATE bioc_code_summary SET latest_release_date = '2026-01-01'
                  WHERE package = 'apkg' AND version = '1.0'")
  log <- attr(.vs_run(out, root, latest, shard_size = 1L), "log")
  expect_true(any(grepl("^version skip invariants: 0 of 0 written packages", log)))
  b <- jsonlite::fromJSON(file.path(out, "code-manifest.json"))$bootstrap
  expect_identical(c(b$latest_off, b$current_off), c(1L, 0L))
  rs <- jsonlite::fromJSON(file.path(out, "run-status.json"))
  expect_identical(c(rs$latest_off, rs$current_off), c(1L, 0L))
})
