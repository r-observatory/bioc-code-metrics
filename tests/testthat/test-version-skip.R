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
