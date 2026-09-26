# tests/testthat/test-analyze-release-text.R: the text history end to end, from
# a real analyze_package over two release branches to the code database.

# An analyzer that prints a summary, the DESCRIPTION it finds and a NEWS section
# for that DESCRIPTION's Version. A 0.5.0 build also names its input kind.
.art_stub <- function(dir, version = "0.4.0-test") {
  kind <- if (analyzer_at_least(version, "0.5.0"))
    sprintf(',\\"input_kind\\":\\"%s\\"', ANALYZER_INPUT_KIND) else ""
  stub <- file.path(dir, "stub-text.sh")
  writeLines(c(
    "#!/bin/sh",
    sprintf('if [ "$1" = "--version" ]; then echo "rpkg-analyzer %s"; exit 0; fi', version),
    'dir=$(echo "$1" | tr -d "\'")',
    'v=$(sed -n "s/^Version: *//p" "$dir/DESCRIPTION" | head -1)',
    sprintf('echo "{\\"rec\\":\\"summary\\",\\"loc_r\\":1,\\"n_fns_r\\":1,\\"analyzer_version\\":\\"%s\\"%s}"',
            version, kind),
    'echo "{\\"rec\\":\\"dcf\\",\\"Package\\":\\"pkgA\\",\\"Version\\":\\"$v\\",\\"RoxygenNote\\":\\"7.3.2\\"}"',
    'echo "{\\"rec\\":\\"release_notes\\",\\"package_version\\":\\"$v\\",\\"news_file\\":\\"NEWS.md\\",\\"release_notes_source\\":\\"news_md\\",\\"release_notes\\":\\"- changes in $v\\",\\"release_notes_truncated\\":false}"'),
    stub)
  Sys.chmod(stub, mode = "0755")
  stub
}

# Two Bioconductor release branches whose DESCRIPTION Versions differ from the
# release names, which is the case the version and package_version columns split.
.art_repo <- function(dest) {
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  git <- function(...) system2("git", c("-C", dest, ...), stdout = FALSE, stderr = FALSE)
  system2("git", c("init", dest), stdout = FALSE, stderr = FALSE)
  git("config", "user.email", "t@example.com")
  git("config", "user.name", "Test Bot")
  writeLines("# readme", file.path(dest, "README"))
  git("add", "-A"); git("commit", "-m", "init")
  for (rel in c("3_22", "3_23")) {
    ver <- if (rel == "3_22") "1.0.0" else "1.2.0"
    git("checkout", "-b", paste0("RELEASE_", rel))
    writeLines(c("Package: pkgA", paste("Version:", ver), "Title: T",
                 "Description: D.", "License: MIT"), file.path(dest, "DESCRIPTION"))
    git("add", "-A"); git("commit", "-m", paste0("release-", rel))
  }
  TRUE
}

test_that("each release keeps its own DESCRIPTION and NEWS, keyed on the release", {
  skip_on_os("windows")
  withr::local_envvar(RPKG_ANALYZER_BIN = .art_stub(withr::local_tempdir()))
  repo <- file.path(withr::local_tempdir(), "pkgA")
  .art_repo(repo)

  text <- analyze_package(repo, "pkgA")$text
  expect_identical(text$versions$version, c("3.22", "3.23"))
  expect_identical(text$release_notes$package_version, c("1.0.0", "1.2.0"))
  expect_identical(unique(text$description_latest$version), "3.23")
  expect_identical(text$description_latest$field, "RoxygenNote")
  expect_identical(text$release_notes_latest$package_version, "1.2.0")
})

test_that("a run writes the history into the code database it already publishes", {
  skip_on_os("windows")
  withr::local_envvar(RPKG_ANALYZER_BIN = .art_stub(withr::local_tempdir()))
  io <- list(
    package_list = function() data.frame(package = "pkgA", latest_version = "3.23",
                                         stringsAsFactors = FALSE),
    clone = function(pkg, dest) .art_repo(dest))
  out <- withr::local_tempdir()
  suppressWarnings(run_update(io, out, shard_size = 10L))

  expect_identical(list.files(out, pattern = "[.]db$"),
                   c(DATA_DB_FILENAME, DB_FILENAME)[order(c(DATA_DB_FILENAME, DB_FILENAME))])
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, DB_FILENAME))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  versions <- DBI::dbGetQuery(con, sprintf('SELECT version FROM "%s" ORDER BY version',
                                           RELEASE_TEXT_VERSIONS_TABLE))$version
  expect_identical(versions, c("3.22", "3.23"))
  notes <- DBI::dbGetQuery(con, sprintf('SELECT version, package_version FROM "%s"',
                                        RELEASE_NOTES_TABLE))
  expect_identical(notes$version, "3.23")
  expect_identical(notes$package_version, "1.2.0")
})

test_that("a failed text write fails the shard before the code rows are written", {
  skip_on_os("windows")
  withr::local_envvar(RPKG_ANALYZER_BIN = .art_stub(withr::local_tempdir()))
  env <- environment(run_update)
  old <- get("upsert_release_text", envir = env)
  assign("upsert_release_text", function(...) stop("disk full"), envir = env)
  on.exit(assign("upsert_release_text", old, envir = env), add = TRUE)
  io <- list(
    package_list = function() data.frame(package = "pkgA", latest_version = "3.23",
                                         stringsAsFactors = FALSE),
    clone = function(pkg, dest) .art_repo(dest))
  out <- withr::local_tempdir()
  expect_error(suppressWarnings(run_update(io, out, shard_size = 10L)), "disk full")
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, DB_FILENAME))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_false("bioc_code_summary" %in% DBI::dbListTables(con))
})

test_that("under a 0.5.0 build every analysed release has its history row", {
  # Were the two keyed differently, every rescanned row would read as a gap and
  # more than 2,000 of them would stop each run.
  skip_on_os("windows")
  withr::local_envvar(RPKG_ANALYZER_BIN = .art_stub(withr::local_tempdir(), "0.5.0-test"))
  io <- list(
    package_list = function() data.frame(package = "pkgA", latest_version = "3.23",
                                         stringsAsFactors = FALSE),
    clone = function(pkg, dest) .art_repo(dest))
  out <- withr::local_tempdir()
  suppressWarnings(run_update(io, out, shard_size = 10L))

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, DB_FILENAME))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  rows <- DBI::dbGetQuery(con,
    "SELECT version, analyzer_version FROM bioc_code_summary ORDER BY version")
  expect_identical(rows$version, c("3.22", "3.23"))
  expect_true(all(rows$analyzer_version == "0.5.0-test"))
  expect_identical(.reconcile_release_text(con, con), 0L)
})

test_that("releases the R fallback wrote contribute no text", {
  skip_on_os("windows")
  withr::local_envvar(RPKG_ANALYZER_BIN = "/nonexistent/rpkg-analyzer")
  skip_if(nzchar(unname(Sys.which("rpkg-analyzer"))), "a real rpkg-analyzer is on PATH")
  repo <- file.path(withr::local_tempdir(), "pkgA")
  .art_repo(repo)
  text <- analyze_package(repo, "pkgA")$text
  expect_equal(nrow(text$versions), 0L)
  expect_equal(nrow(text$description_latest), 0L)
  expect_equal(nrow(text$release_notes_latest), 0L)
})
