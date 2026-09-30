# tests/testthat/test-git.R: tests for scripts/git.R (Bioconductor RELEASE branch walker)
#
# All tests are offline. Repos with RELEASE_X_Y local branches are created
# in tempdir(). list_versions() checks both refs/heads/RELEASE_* (local
# branches, used here) and refs/remotes/origin/RELEASE_* (remote-tracking
# refs present in real clones from github.com/bioc).

# Helper: run git inside repo (no user config needed).
.git <- function(repo, ...) {
  system2("git", c("-C", repo, ...), stdout = FALSE, stderr = FALSE)
}
.gitc <- function(repo, ...) {
  system2("git",
          c("-C", repo, "-c", "user.email=t@t.test", "-c", "user.name=T", ...),
          stdout = FALSE, stderr = FALSE)
}

# ---------------------------------------------------------------------------
# list_versions
# ---------------------------------------------------------------------------

test_that("list_versions returns RELEASE_X_Y branches in numeric version order", {
  repo <- tempfile("bcm_git_test_")
  on.exit(unlink(repo, recursive = TRUE), add = TRUE)

  dir.create(repo)
  system2("git", c("init", repo), stdout = FALSE, stderr = FALSE)

  # Initial commit on master
  writeLines("readme", file.path(repo, "README"))
  .git(repo, "add", ".")
  .gitc(repo, "commit", "-m", "init")

  # RELEASE_3_17 -- created first but should sort AFTER 3.10 numerically
  .git(repo, "checkout", "-b", "RELEASE_3_17")
  writeLines("a <- 1", file.path(repo, "a.R"))
  .git(repo, "add", ".")
  .gitc(repo, "commit", "-m", "release-3.17")
  .git(repo, "checkout", "-")

  # RELEASE_3_10 -- has a higher minor than 3.9 but lower than 3.17
  .git(repo, "checkout", "-b", "RELEASE_3_10")
  writeLines("a <- 0", file.path(repo, "a.R"))
  .git(repo, "add", ".")
  .gitc(repo, "commit", "-m", "release-3.10")
  .git(repo, "checkout", "-")

  # RELEASE_3_16
  .git(repo, "checkout", "-b", "RELEASE_3_16")
  writeLines("a <- 1", file.path(repo, "a.R"))
  .git(repo, "add", ".")
  .gitc(repo, "commit", "-m", "release-3.16")
  .git(repo, "checkout", "-")

  v <- list_versions(repo)

  expect_s3_class(v, "data.frame")
  expect_equal(nrow(v), 3L)
  expect_equal(colnames(v), c("version", "ref", "date", "commit"))
  # Must be sorted numerically: 3.10 < 3.16 < 3.17 (NOT lexicographic order)
  expect_equal(v$version, c("3.10", "3.16", "3.17"))
  # ref should name the branch
  expect_true(all(grepl("RELEASE", v$ref)))
  # Dates must be YYYY-MM-DD
  expect_true(all(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", v$date)))
  # Commits must be non-empty strings
  expect_true(all(nzchar(v$commit)))
})

test_that("list_versions ignores non-RELEASE branches (master, devel)", {
  repo <- tempfile("bcm_git_norel_")
  on.exit(unlink(repo, recursive = TRUE), add = TRUE)

  dir.create(repo)
  system2("git", c("init", repo), stdout = FALSE, stderr = FALSE)
  writeLines("x", file.path(repo, "README"))
  .git(repo, "add", ".")
  .gitc(repo, "commit", "-m", "init")

  # master already exists; add devel branch
  .git(repo, "checkout", "-b", "devel")
  writeLines("x <- 1", file.path(repo, "x.R"))
  .git(repo, "add", ".")
  .gitc(repo, "commit", "-m", "devel-commit")
  .git(repo, "checkout", "-")

  # One real RELEASE branch
  .git(repo, "checkout", "-b", "RELEASE_3_18")
  writeLines("x <- 2", file.path(repo, "x.R"))
  .git(repo, "add", ".")
  .gitc(repo, "commit", "-m", "release-3.18")
  .git(repo, "checkout", "-")

  v <- list_versions(repo)
  expect_equal(nrow(v), 1L)
  expect_equal(v$version, "3.18")
})

test_that("list_versions returns empty data.frame for repo with no RELEASE branches", {
  repo <- tempfile("bcm_git_empty_")
  on.exit(unlink(repo, recursive = TRUE), add = TRUE)

  dir.create(repo)
  system2("git", c("init", repo), stdout = FALSE, stderr = FALSE)
  writeLines("x", file.path(repo, "README"))
  .git(repo, "add", ".")
  .gitc(repo, "commit", "-m", "init")

  v <- list_versions(repo)
  expect_s3_class(v, "data.frame")
  expect_equal(nrow(v), 0L)
  expect_equal(colnames(v), c("version", "ref", "date", "commit"))
})

test_that("list_versions deduplicates RELEASE branches pointing at same commit", {
  repo <- tempfile("bcm_git_dedup_")
  on.exit(unlink(repo, recursive = TRUE), add = TRUE)

  dir.create(repo)
  system2("git", c("init", repo), stdout = FALSE, stderr = FALSE)
  writeLines("x <- 1", file.path(repo, "x.R"))
  .git(repo, "add", ".")
  .gitc(repo, "commit", "-m", "init")

  # Two RELEASE branches pointing at the same commit (HEAD)
  .git(repo, "branch", "RELEASE_3_18")
  .git(repo, "branch", "RELEASE_3_19")

  v <- list_versions(repo)
  # Two branches but same SHA -> deduplicated to 1 row (lowest version wins)
  expect_equal(nrow(v), 1L)
  expect_equal(v$version, "3.18")
})

# ---------------------------------------------------------------------------
# package_churn
# ---------------------------------------------------------------------------

test_that("package_churn returns branch-to-branch diffs with Bioc version strings", {
  repo <- tempfile("bcm_churn_")
  on.exit(unlink(repo, recursive = TRUE), add = TRUE)

  dir.create(repo)
  system2("git", c("init", repo), stdout = FALSE, stderr = FALSE)
  writeLines("readme", file.path(repo, "README"))
  .git(repo, "add", ".")
  .gitc(repo, "commit", "-m", "init")

  # RELEASE_3_16: R/a.R with 2 lines
  .git(repo, "checkout", "-b", "RELEASE_3_16")
  dir.create(file.path(repo, "R"), showWarnings = FALSE)
  writeLines(c("a <- 1", "b <- 2"), file.path(repo, "R", "a.R"))
  .git(repo, "add", ".")
  .gitc(repo, "commit", "-m", "release-3.16")
  .git(repo, "checkout", "-")

  # RELEASE_3_17: R/a.R updated, R/new.R added
  .git(repo, "checkout", "-b", "RELEASE_3_17")
  dir.create(file.path(repo, "R"), showWarnings = FALSE)
  writeLines(c("a <- 1", "b <- 99", "c <- 3"), file.path(repo, "R", "a.R"))
  writeLines("new <- TRUE", file.path(repo, "R", "new.R"))
  .git(repo, "add", ".")
  .gitc(repo, "commit", "-m", "release-3.17")
  .git(repo, "checkout", "-")

  ch <- package_churn(repo)

  expect_s3_class(ch, "data.frame")
  expect_true(all(c("commit", "version", "file", "added", "deleted") %in%
                    colnames(ch)))
  expect_true(nrow(ch) >= 1L)
  # Both release versions must appear
  expect_true("3.16" %in% ch$version)
  expect_true("3.17" %in% ch$version)
  expect_true(all(nzchar(ch$commit)))
  expect_true(is.integer(ch$added) || is.numeric(ch$added))
})

test_that("package_churn first release diffs against empty tree (all lines added)", {
  repo <- tempfile("bcm_churn_first_")
  on.exit(unlink(repo, recursive = TRUE), add = TRUE)

  dir.create(repo)
  system2("git", c("init", repo), stdout = FALSE, stderr = FALSE)
  writeLines("readme", file.path(repo, "README"))
  .git(repo, "add", ".")
  .gitc(repo, "commit", "-m", "init")

  .git(repo, "checkout", "-b", "RELEASE_3_16")
  dir.create(file.path(repo, "R"), showWarnings = FALSE)
  writeLines(c("line1", "line2", "line3"), file.path(repo, "R", "a.R"))
  .git(repo, "add", ".")
  .gitc(repo, "commit", "-m", "release-3.16")
  .git(repo, "checkout", "-")

  ch <- package_churn(repo)

  expect_true(nrow(ch) >= 1L)
  r_rows <- ch[ch$file == "R/a.R" & !is.na(ch$version) & ch$version == "3.16", ]
  expect_true(nrow(r_rows) >= 1L)
  # First release: all 3 lines are "added" (diff against empty tree)
  expect_equal(r_rows$added,   3L)
  expect_equal(r_rows$deleted, 0L)
})

test_that("package_churn returns empty data.frame for repo with no RELEASE branches", {
  repo <- tempfile("bcm_churn_empty_")
  on.exit(unlink(repo, recursive = TRUE), add = TRUE)

  dir.create(repo)
  system2("git", c("init", repo), stdout = FALSE, stderr = FALSE)
  writeLines("x", file.path(repo, "README"))
  .git(repo, "add", ".")
  .gitc(repo, "commit", "-m", "init")

  ch <- package_churn(repo)
  expect_s3_class(ch, "data.frame")
  expect_equal(nrow(ch), 0L)
  expect_equal(colnames(ch), c("commit", "version", "file", "added", "deleted"))
})

test_that("package_churn resolves renamed file paths to the new path", {
  repo <- tempfile("bcm_rename_")
  on.exit(unlink(repo, recursive = TRUE), add = TRUE)

  dir.create(repo)
  system2("git", c("init", repo), stdout = FALSE, stderr = FALSE)

  # Initial commit with old.R
  writeLines("old <- 1", file.path(repo, "old.R"))
  .git(repo, "add", ".")
  .gitc(repo, "commit", "-m", "init")

  # RELEASE_3_16: old.R present
  .git(repo, "checkout", "-b", "RELEASE_3_16")
  .gitc(repo, "commit", "--allow-empty", "-m", "release-3.16")
  .git(repo, "checkout", "-")

  # RELEASE_3_17: old.R renamed to new.R
  .git(repo, "checkout", "-b", "RELEASE_3_17")
  .git(repo, "mv", "old.R", "new.R")
  .gitc(repo, "commit", "-m", "rename-old.R-to-new.R")
  .git(repo, "checkout", "-")

  ch <- package_churn(repo)

  expect_s3_class(ch, "data.frame")
  # No file path should contain " => "
  expect_false(any(grepl(" => ", ch$file, fixed = TRUE)))
  # The rename rows for 3.17 must use the new path
  rows_317 <- ch[!is.na(ch$version) & ch$version == "3.17", ]
  if (nrow(rows_317) >= 1L) {
    expect_true(any(rows_317$file == "new.R"))
    expect_false(any(rows_317$file == "old.R"))
  }
})

# ---------------------------------------------------------------------------
# extract_version: works with RELEASE branch refs
# ---------------------------------------------------------------------------

test_that("extract_version extracts correct file tree at a RELEASE branch ref", {
  repo <- tempfile("bcm_extract_")
  dest <- tempfile("bcm_tree_")
  on.exit({
    unlink(repo, recursive = TRUE)
    unlink(dest, recursive = TRUE)
  }, add = TRUE)

  dir.create(repo)
  system2("git", c("init", repo), stdout = FALSE, stderr = FALSE)
  writeLines("readme", file.path(repo, "README"))
  .git(repo, "add", ".")
  .gitc(repo, "commit", "-m", "init")

  # RELEASE_3_16: has v16.R and DESCRIPTION
  .git(repo, "checkout", "-b", "RELEASE_3_16")
  dir.create(file.path(repo, "R"), showWarnings = FALSE)
  writeLines("v16 <- TRUE", file.path(repo, "R", "v16.R"))
  writeLines("Package: mypkg\nVersion: 3.16\n", file.path(repo, "DESCRIPTION"))
  .git(repo, "add", ".")
  .gitc(repo, "commit", "-m", "release-3.16")
  .git(repo, "checkout", "-")

  # RELEASE_3_17: has v17.R in addition
  .git(repo, "checkout", "-b", "RELEASE_3_17")
  dir.create(file.path(repo, "R"), showWarnings = FALSE)
  writeLines("v17 <- TRUE", file.path(repo, "R", "v17.R"))
  writeLines("Package: mypkg\nVersion: 3.17\n", file.path(repo, "DESCRIPTION"))
  .git(repo, "add", ".")
  .gitc(repo, "commit", "-m", "release-3.17")
  .git(repo, "checkout", "-")

  v <- list_versions(repo)
  ref_316 <- v$ref[v$version == "3.16"]

  extracted <- extract_version(repo, ref_316, dest)

  expect_true("R/v16.R" %in% extracted)
  expect_true("DESCRIPTION" %in% extracted)
  expect_false("R/v17.R" %in% extracted)

  # Verify content
  content <- readLines(file.path(dest, "R", "v16.R"), warn = FALSE)
  expect_true(any(grepl("v16", content)))
})

# ---------------------------------------------------------------------------
# read_at: works with RELEASE branch refs
# ---------------------------------------------------------------------------

test_that("read_at returns file content and empty string for absent paths at RELEASE ref", {
  repo <- tempfile("bcm_read_")
  on.exit(unlink(repo, recursive = TRUE), add = TRUE)

  dir.create(repo)
  system2("git", c("init", repo), stdout = FALSE, stderr = FALSE)
  writeLines("readme", file.path(repo, "README"))
  .git(repo, "add", ".")
  .gitc(repo, "commit", "-m", "init")

  .git(repo, "checkout", "-b", "RELEASE_3_18")
  dir.create(file.path(repo, "R"), showWarnings = FALSE)
  writeLines("hello_world <- 42", file.path(repo, "R", "hello.R"))
  .git(repo, "add", ".")
  .gitc(repo, "commit", "-m", "release-3.18")
  .git(repo, "checkout", "-")

  v   <- list_versions(repo)
  ref <- v$ref[1L]

  content <- read_at(repo, ref, "R/hello.R")
  expect_true(grepl("hello_world", content))

  absent <- read_at(repo, ref, "R/does_not_exist.R")
  expect_equal(absent, "")
})

# ---------------------------------------------------------------------------
# clone_package
# ---------------------------------------------------------------------------

test_that("clone_package returns FALSE for a non-existent repo (offline-safe)", {
  dest <- tempfile("bcm_clone_fail_")
  on.exit(unlink(dest, recursive = TRUE), add = TRUE)
  result <- clone_package(
    "this_package_definitely_does_not_exist_9999",
    dest,
    base = "file:///nonexistent/path"
  )
  expect_false(result)
})

# ---------------------------------------------------------------------------
# Extraction that cannot be done is an error, not an empty release
# ---------------------------------------------------------------------------

# A stand-in for git-lfs that needs no git-lfs: its smudge passes a pointer
# through only when GIT_LFS_SKIP_SMUDGE=1, as git-lfs does, and otherwise fails
# the way a missing LFS object does. The global config is replaced for the
# test, since a real git-lfs filter.lfs.process there would take precedence.
.local_fake_lfs <- function(frame = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = frame)
  smudge <- file.path(dir, "fake-lfs-smudge.sh")
  writeLines(c("#!/bin/sh",
               'if [ "$GIT_LFS_SKIP_SMUDGE" = "1" ]; then exec cat; fi',
               'echo "fake-lfs: object not available" >&2',
               "exit 1"), smudge)
  Sys.chmod(smudge, mode = "0755")
  cfg <- file.path(dir, "gitconfig")
  writeLines(c('[filter "lfs"]', "\tclean = cat", paste0("\tsmudge = ", smudge),
               "\trequired = true", "[user]", "\tname = T", "\temail = t@t.test"), cfg)
  withr::local_envvar(GIT_CONFIG_GLOBAL = cfg, GIT_CONFIG_NOSYSTEM = "1",
                      .local_envir = frame)
  invisible(cfg)
}

# A repo whose data/x.RData is tracked by LFS, on its default branch and on
# RELEASE_3_12, as BioTIP's was.
.lfs_repo <- function(path) {
  dir.create(file.path(path, "data"), recursive = TRUE, showWarnings = FALSE)
  system2("git", c("init", "-q", path), stdout = FALSE, stderr = FALSE)
  writeLines("*.RData filter=lfs diff=lfs merge=lfs -text", file.path(path, ".gitattributes"))
  writeLines(c("version https://git-lfs.github.com/spec/v1",
               paste0("oid sha256:", strrep("0", 64L)), "size 3"),
             file.path(path, "data", "x.RData"))
  writeLines(c("Package: lfspkg", "Version: 1.0"), file.path(path, "DESCRIPTION"))
  .git(path, "add", "-A")
  .git(path, "commit", "-q", "-m", "release-3.12")
  .git(path, "branch", "RELEASE_3_12")
  path
}

# Put an executable named `name` in front of PATH until the calling test ends.
.local_shim <- function(name, body, frame = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = frame)
  path <- file.path(dir, name)
  writeLines(c("#!/bin/sh", body), path)
  Sys.chmod(path, mode = "0755")
  withr::local_envvar(PATH = paste(dir, Sys.getenv("PATH"), sep = .Platform$path.sep),
                      .local_envir = frame)
  path
}

.one_release_repo <- function(path) {
  system2("git", c("init", "-q", path), stdout = FALSE, stderr = FALSE)
  writeLines("Package: p\nVersion: 1.0", file.path(path, "DESCRIPTION"))
  .git(path, "add", ".")
  .gitc(path, "commit", "-q", "-m", "release-3.18")
  .git(path, "branch", "RELEASE_3_18")
  path
}

test_that("an LFS-tracked file that cannot be smudged extracts as its pointer", {
  .local_fake_lfs()
  repo <- .lfs_repo(withr::local_tempdir())
  # Plain git archive fails on this repo, as it did on BioTIP 3.12.
  plain <- suppressWarnings(system2("git", c("-C", repo, "archive", "RELEASE_3_12"),
                                    stdout = FALSE, stderr = FALSE))
  expect_false(identical(plain, 0L))
  dest <- withr::local_tempdir()
  files <- extract_version(repo, "RELEASE_3_12", dest)
  expect_setequal(files, c(".gitattributes", "DESCRIPTION", "data/x.RData"))
  expect_identical(readBin(file.path(dest, "data", "x.RData"), "raw", 1000L),
                   readBin(file.path(repo, "data", "x.RData"), "raw", 1000L))
})

test_that("a tree that is really empty extracts to character(0) without an error", {
  # liftOver RELEASE_3_0 is such a tree, and its empty row is a true one.
  repo <- withr::local_tempdir()
  system2("git", c("init", "-q", repo), stdout = FALSE, stderr = FALSE)
  .gitc(repo, "commit", "-q", "--allow-empty", "-m", "empty")
  .git(repo, "branch", "RELEASE_3_0")
  expect_identical(extract_version(repo, "RELEASE_3_0", withr::local_tempdir()),
                   character(0L))
})

test_that("a ref git cannot archive raises extract_failure at the archive step", {
  repo <- .one_release_repo(withr::local_tempdir())
  err <- tryCatch(extract_version(repo, "RELEASE_9_99", withr::local_tempdir()),
                  error = function(e) e)
  expect_s3_class(err, "extract_failure")
  expect_identical(err$step, "archive")
  expect_identical(err$ref, "RELEASE_9_99")
  expect_false(identical(err$status, 0L))
  expect_match(err$stderr, "RELEASE_9_99", fixed = TRUE)
  expect_match(conditionMessage(err), "git archive of RELEASE_9_99 exited", fixed = TRUE)
})

test_that("a tar that fails raises extract_failure at the tar step", {
  repo <- .one_release_repo(withr::local_tempdir())
  .local_shim("tar", c('echo "tar: shimmed failure" >&2', "exit 2"))
  err <- tryCatch(extract_version(repo, "RELEASE_3_18", withr::local_tempdir()),
                  error = function(e) e)
  expect_s3_class(err, "extract_failure")
  expect_identical(err$step, "tar")
  expect_identical(err$status, 2L)
  expect_identical(err$stderr, "tar: shimmed failure")
})

test_that("git archive killed at GIT_TIMEOUT carries status 124", {
  repo <- .one_release_repo(withr::local_tempdir())
  real_git <- unname(Sys.which("git"))
  .local_shim("git", c(
    'for a in "$@"; do',
    '  if [ "$a" = archive ]; then echo "fatal: shimmed timeout" >&2; exit 124; fi',
    "done",
    sprintf('exec %s "$@"', shQuote(real_git))))
  err <- tryCatch(extract_version(repo, "RELEASE_3_18", withr::local_tempdir()),
                  error = function(e) e)
  expect_s3_class(err, "extract_failure")
  expect_identical(err$step, "archive")
  expect_identical(err$status, 124L)
})

test_that("a token git prints on stderr never reaches the condition", {
  .local_shim("git", c(
    'echo "fatal: unable to access https://x-access-token:ghs_abcDEF123@github.com/bioc/p.git/" >&2',
    "exit 128"))
  err <- tryCatch(extract_version(withr::local_tempdir(), "RELEASE_3_18",
                                  withr::local_tempdir()),
                  error = function(e) e)
  expect_s3_class(err, "extract_failure")
  expect_false(grepl("ghs_abcDEF123", conditionMessage(err), fixed = TRUE))
  expect_identical(err$stderr,
                   "fatal: unable to access https://***github.com/bioc/p.git/")
})

# ---------------------------------------------------------------------------
# clone_package keeps LFS pointers and says how the clone ended
# ---------------------------------------------------------------------------

test_that("clone_package checks out a branch whose LFS objects cannot be fetched", {
  .local_fake_lfs()
  base <- withr::local_tempdir()
  .lfs_repo(file.path(base, "lfspkg.git"))
  # A plain clone fails at checkout, as it would with git-lfs and no object.
  plain <- suppressWarnings(system2(
    "git", c("clone", "--quiet", file.path(base, "lfspkg.git"), file.path(base, "plain")),
    stdout = FALSE, stderr = FALSE))
  expect_false(identical(plain, 0L))
  dest <- file.path(base, "clone")
  ok <- clone_package("lfspkg", dest, base = base)
  expect_true(ok)
  expect_identical(attr(ok, "status"), 0L)
  expect_true(file.exists(file.path(dest, "data", "x.RData")))
})

test_that("a clone keeps its checkout, so churn still honours .gitattributes", {
  base <- withr::local_tempdir()
  src <- file.path(base, "diffpkg.git")
  dir.create(src)
  system2("git", c("init", "-q", src), stdout = FALSE, stderr = FALSE)
  writeLines("*.txt -diff", file.path(src, ".gitattributes"))
  writeLines(c("a", "b"), file.path(src, "a.txt"))
  .git(src, "add", "-A")
  .gitc(src, "commit", "-q", "-m", "release-1.0")
  .git(src, "branch", "RELEASE_1_0")
  dest <- file.path(base, "clone")
  expect_true(clone_package("diffpkg", dest, base = base))
  ch  <- package_churn(dest)
  row <- ch[ch$file == "a.txt", , drop = FALSE]
  expect_identical(nrow(row), 1L)
  expect_true(is.na(row$added))
  expect_true(is.na(row$deleted))
})

test_that("clone_package of a missing repo returns FALSE with its exit status", {
  dest <- file.path(withr::local_tempdir(), "clone")
  ok <- clone_package("this_package_definitely_does_not_exist_9999", dest,
                      base = withr::local_tempdir())
  expect_false(ok)
  expect_true(is.integer(attr(ok, "status")))
  expect_false(identical(attr(ok, "status"), 0L))
})

test_that("a clone killed at GIT_TIMEOUT returns FALSE with status 124", {
  .local_global("GIT_TIMEOUT", 1L)
  .local_shim("git", "sleep 5")
  ok <- clone_package("slowpkg", file.path(withr::local_tempdir(), "clone"),
                      base = withr::local_tempdir())
  expect_false(ok)
  expect_identical(attr(ok, "status"), 124L)
})
