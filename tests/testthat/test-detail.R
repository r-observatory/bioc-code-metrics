# tests/testthat/test-detail.R: per-function / per-call-edge detail storage.
#
# Covers latest-version gating in analyze_package (via a stub analyzer that emits
# a fixture NDJSON stream) and the bioc_functions / bioc_call_edges export path.
# No real rpkg-analyzer binary is required.

.detail_fixture_lines <- function() {
  c(
    '{"rec":"summary","package":"demo","n_fns_r":2,"lang_breakdown":{"R":40}}',
    '{"rec":"function","lang":"r","name":"foo","exported":true,"file":"R/foo.R","line":1,"loc":10,"n_params":2,"cyclocomp":3}',
    '{"rec":"function","lang":"r","name":"bar","exported":false,"file":"R/bar.R","line":5,"loc":4,"n_params":0,"cyclocomp":1}',
    '{"rec":"function","lang":"c","name":"native_helper","file":"src/helper.c","line":12,"loc":20}',
    '{"rec":"call_edge","graph":"r","from":"foo","to":"bar"}',
    '{"rec":"call_edge","graph":"native","from":"foo","to":"native_helper"}',
    '{"rec":"call_edge","graph":"c","from":"native_helper","to":"malloc"}'
  )
}

.write_detail_stub <- function(dir, ndjson_lines) {
  fixture <- file.path(dir, "fixture.ndjson")
  writeLines(ndjson_lines, fixture)
  stub <- file.path(dir, "stub-analyzer.sh")
  writeLines(c("#!/bin/sh", sprintf("cat %s", shQuote(fixture))), stub)
  Sys.chmod(stub, mode = "0755")
  stub
}

# Build a real 2-version git repo so analyze_package walks both versions.
# Bioconductor versions are RELEASE_X_Y branches (list_versions ignores tags), so
# RELEASE_1_0 -> "1.0" and RELEASE_1_1 -> "1.1", the latter being the latest.
.make_two_version_repo <- function(repo) {
  dir.create(repo)
  system2("git", c("init", repo), stdout = FALSE, stderr = FALSE)
  system2("git", c("-C", repo, "config", "user.email", "t@t.test"),
          stdout = FALSE, stderr = FALSE)
  system2("git", c("-C", repo, "config", "user.name", "T"),
          stdout = FALSE, stderr = FALSE)
  writeLines("# readme", file.path(repo, "README"))
  system2("git", c("-C", repo, "add", "."), stdout = FALSE, stderr = FALSE)
  system2("git", c("-C", repo, "commit", "-m", "init"),
          stdout = FALSE, stderr = FALSE)

  # RELEASE_1_0 branch -> version "1.0"
  system2("git", c("-C", repo, "checkout", "-b", "RELEASE_1_0"),
          stdout = FALSE, stderr = FALSE)
  dir.create(file.path(repo, "R"), showWarnings = FALSE)
  writeLines("foo <- function() 1", file.path(repo, "R", "foo.R"))
  writeLines("Package: mypkg\nVersion: 1.0\n", file.path(repo, "DESCRIPTION"))
  writeLines("export(foo)\n", file.path(repo, "NAMESPACE"))
  system2("git", c("-C", repo, "add", "."), stdout = FALSE, stderr = FALSE)
  system2("git", c("-C", repo, "commit", "-m", "release-1.0"),
          stdout = FALSE, stderr = FALSE)
  system2("git", c("-C", repo, "checkout", "-"), stdout = FALSE, stderr = FALSE)

  # RELEASE_1_1 branch -> version "1.1" (latest)
  system2("git", c("-C", repo, "checkout", "-b", "RELEASE_1_1"),
          stdout = FALSE, stderr = FALSE)
  dir.create(file.path(repo, "R"), showWarnings = FALSE)
  writeLines(c("foo <- function() 1", "bar <- function() 2"),
             file.path(repo, "R", "foo.R"))
  writeLines("Package: mypkg\nVersion: 1.1\n", file.path(repo, "DESCRIPTION"))
  writeLines("export(foo)\nexport(bar)\n", file.path(repo, "NAMESPACE"))
  system2("git", c("-C", repo, "add", "."), stdout = FALSE, stderr = FALSE)
  system2("git", c("-C", repo, "commit", "-m", "release-1.1"),
          stdout = FALSE, stderr = FALSE)
  system2("git", c("-C", repo, "checkout", "-"), stdout = FALSE, stderr = FALSE)
}

# ---------------------------------------------------------------------------
# analyze_package: detail is stored for the latest version only
# ---------------------------------------------------------------------------

test_that("analyze_package returns functions/edges frames", {
  repo <- tempfile("ccm_detail_")
  on.exit(unlink(repo, recursive = TRUE), add = TRUE)
  .make_two_version_repo(repo)

  result <- analyze_package(repo, "mypkg")

  expect_true(all(c("summary", "churn", "api", "functions", "edges") %in%
                    names(result)))
  expect_s3_class(result$functions, "data.frame")
  expect_s3_class(result$edges, "data.frame")
  expect_identical(
    names(result$functions),
    c("package", "version", "lang", "name", "exported", "file", "line",
      "loc", "n_params", "cyclocomp")
  )
  expect_identical(names(result$edges),
                   c("package", "version", "graph", "from", "to"))
})

test_that("analyze_package stores detail only for the latest version", {
  skip_on_os("windows")
  stub_dir <- tempfile("ccm_stub_")
  dir.create(stub_dir)
  on.exit(unlink(stub_dir, recursive = TRUE), add = TRUE)
  stub <- .write_detail_stub(stub_dir, .detail_fixture_lines())
  withr::local_envvar(RPKG_ANALYZER_BIN = stub)

  repo <- tempfile("ccm_detail_")
  on.exit(unlink(repo, recursive = TRUE), add = TRUE)
  .make_two_version_repo(repo)

  result <- analyze_package(repo, "mypkg")

  # Summary still has one row per version.
  expect_equal(nrow(result$summary), 2L)

  # The stub emits the same fixture for BOTH versions, but only the latest
  # version's detail is kept.
  expect_true(nrow(result$functions) > 0L)
  expect_equal(unique(result$functions$version), "1.1")
  expect_equal(unique(result$functions$package), "mypkg")
  expect_false(any(result$functions$version == "1.0"))
  expect_equal(nrow(result$functions), 3L)

  expect_equal(unique(result$edges$version), "1.1")
  expect_false(any(result$edges$version == "1.0"))
  expect_equal(nrow(result$edges), 3L)

  # Honest NA survives the round trip: compiled function.
  nat <- result$functions[result$functions$name == "native_helper", ]
  expect_true(is.na(nat$exported))
  expect_true(is.na(nat$n_params))
  expect_true(is.na(nat$cyclocomp))
})

test_that("analyze_package yields zero detail rows when no binary is available", {
  # Ensure no analyzer is picked up so the R fallback path runs.
  withr::local_envvar(RPKG_ANALYZER_BIN = "/nonexistent/analyzer")
  skip_if(nzchar(unname(Sys.which("rpkg-analyzer"))),
          "a real rpkg-analyzer is on PATH")

  repo <- tempfile("ccm_detail_")
  on.exit(unlink(repo, recursive = TRUE), add = TRUE)
  .make_two_version_repo(repo)

  result <- analyze_package(repo, "mypkg")
  expect_equal(nrow(result$functions), 0L)
  expect_equal(nrow(result$edges), 0L)
})

# ---------------------------------------------------------------------------
# export: bioc_functions and bioc_call_edges tables
# ---------------------------------------------------------------------------

.make_functions <- function(package = "pkgA", version = "1.0") {
  data.frame(
    package   = package,
    version   = version,
    lang      = c("r", "c"),
    name      = c("foo", "helper"),
    exported  = c(TRUE, NA),
    file      = c("R/foo.R", "src/h.c"),
    line      = c(1L, 3L),
    loc       = c(10L, 20L),
    n_params  = c(2L, NA_integer_),
    cyclocomp = c(3L, NA_integer_),
    stringsAsFactors = FALSE
  )
}

.make_edges <- function(package = "pkgA", version = "1.0") {
  data.frame(
    package = package,
    version = version,
    graph   = c("r", "native"),
    from    = c("foo", "foo"),
    to      = c("bar", "helper"),
    stringsAsFactors = FALSE
  )
}

.empty_summary_df <- function() {
  data.frame(package = character(0), version = character(0),
             stringsAsFactors = FALSE)
}
.empty_churn_df <- function() {
  data.frame(package = character(0), version = character(0),
             file = character(0), added = integer(0), deleted = integer(0),
             stringsAsFactors = FALSE)
}
.empty_api_df <- function() {
  data.frame(package = character(0), version = character(0),
             exports_added = character(0), exports_removed = character(0),
             n_exports = integer(0), stringsAsFactors = FALSE)
}

.mini_summary <- function(package = "pkgA", version = "1.0") {
  data.frame(package = package, version = version, loc_r = 1L,
             stringsAsFactors = FALSE)
}

test_that("upsert_shard creates bioc_functions and bioc_call_edges", {
  tmp <- tempfile(fileext = ".db")
  on.exit(unlink(tmp), add = TRUE)
  con <- open_or_init_db(tmp)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  upsert_shard(con, .mini_summary(), .empty_churn_df(), .empty_api_df(),
               .make_functions(), .make_edges())

  tables <- DBI::dbListTables(con)
  expect_true("bioc_functions" %in% tables)
  expect_true("bioc_call_edges" %in% tables)

  fns <- DBI::dbGetQuery(con, "SELECT * FROM bioc_functions")
  expect_equal(nrow(fns), 2L)
  # Logical exported coerced to 0/1 INTEGER, NA preserved.
  helper <- fns[fns$name == "helper", ]
  expect_true(is.na(helper$exported))
  expect_true(is.na(helper$n_params))

  edges <- DBI::dbGetQuery(con, 'SELECT * FROM bioc_call_edges')
  expect_equal(nrow(edges), 2L)
  expect_setequal(edges$graph, c("r", "native"))
})

test_that("upsert_shard replaces a package's detail rows across versions", {
  tmp <- tempfile(fileext = ".db")
  on.exit(unlink(tmp), add = TRUE)
  con <- open_or_init_db(tmp)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # First: pkgA at 1.0 with 2 functions, 2 edges.
  upsert_shard(con, .mini_summary("pkgA", "1.0"),
               .empty_churn_df(), .empty_api_df(),
               .make_functions("pkgA", "1.0"), .make_edges("pkgA", "1.0"))

  # Re-analysis: pkgA now latest 1.1. Detail is stored for 1.1 only; the old
  # 1.0 detail rows must not linger (delete-by-package before append).
  upsert_shard(con, .mini_summary("pkgA", "1.1"),
               .empty_churn_df(), .empty_api_df(),
               .make_functions("pkgA", "1.1"), .make_edges("pkgA", "1.1"))

  fns <- DBI::dbGetQuery(con, "SELECT DISTINCT version FROM bioc_functions")
  expect_equal(fns$version, "1.1")
  edges <- DBI::dbGetQuery(con, "SELECT DISTINCT version FROM bioc_call_edges")
  expect_equal(edges$version, "1.1")

  n_fns <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM bioc_functions")$n
  expect_equal(n_fns, 2L)
})

test_that("upsert_shard remains backward compatible without detail frames", {
  tmp <- tempfile(fileext = ".db")
  on.exit(unlink(tmp), add = TRUE)
  con <- open_or_init_db(tmp)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  expect_no_error(
    upsert_shard(con, .mini_summary(), .empty_churn_df(), .empty_api_df())
  )
  # Detail tables are not forced into existence by a detail-free shard.
  expect_false("bioc_functions" %in% DBI::dbListTables(con))
})
