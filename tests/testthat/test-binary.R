# tests/testthat/test-binary.R: tests for scripts/binary.R
#
# These tests exercise the NDJSON record parser directly (parse_analyzer_records)
# and the analyze_with_binary bridge via a tiny stub that cats a fixture. No real
# rpkg-analyzer binary is required.

# A representative analyzer stream: one summary, some non-detail records, two R
# functions, one compiled function, and three call edges across graphs.
.fixture_lines <- function() {
  c(
    '{"rec":"summary","package":"demo","n_fns_r":2,"lang_breakdown":{"R":40,"C":10}}',
    '{"rec":"dependency","name":"utils"}',
    '{"rec":"export","name":"foo"}',
    '{"rec":"function","lang":"r","name":"foo","exported":true,"file":"R/foo.R","line":1,"loc":10,"n_params":2,"cyclocomp":3}',
    '{"rec":"function","lang":"r","name":"bar","exported":false,"file":"R/bar.R","line":5,"loc":4,"n_params":0,"cyclocomp":1}',
    '{"rec":"function","lang":"c","name":"native_helper","file":"src/helper.c","line":12,"loc":20}',
    '{"rec":"call_edge","graph":"r","from":"foo","to":"bar"}',
    '{"rec":"call_edge","graph":"native","from":"foo","to":"native_helper"}',
    '{"rec":"call_edge","graph":"c","from":"native_helper","to":"malloc"}',
    '{"rec":"dcf","field":"x"}'
  )
}

# The exact summary-flattening the parser must preserve byte-for-byte.
.old_flatten_summary <- function(out) {
  summ <- NULL
  for (line in out) {
    parsed <- tryCatch(
      jsonlite::fromJSON(line, simplifyVector = FALSE),
      error = function(e) NULL
    )
    if (!is.null(parsed) && identical(parsed[["rec"]], "summary")) {
      summ <- parsed
      break
    }
  }
  if (is.null(summ)) return(NULL)
  summ[["rec"]] <- NULL
  lapply(summ, function(v) {
    if (is.null(v)) {
      NA
    } else if (is.list(v) || length(v) != 1L) {
      as.character(jsonlite::toJSON(v, auto_unbox = TRUE, null = "null"))
    } else {
      v[[1L]]
    }
  })
}

# ---------------------------------------------------------------------------
# parse_analyzer_records: summary
# ---------------------------------------------------------------------------

test_that("parse_analyzer_records flattens the summary byte-identically to old behavior", {
  lines  <- .fixture_lines()
  parsed <- parse_analyzer_records(lines)

  expect_identical(parsed$summary, .old_flatten_summary(lines))

  # Spot-check the flattened values.
  expect_identical(parsed$summary$package, "demo")
  expect_identical(parsed$summary$n_fns_r, 2L)
  expect_identical(parsed$summary$lang_breakdown, '{"R":40,"C":10}')
  expect_false("rec" %in% names(parsed$summary))
})

test_that("parse_analyzer_records returns NULL summary when no summary record present", {
  lines  <- c('{"rec":"function","lang":"r","name":"x","exported":true,"file":"R/x.R","line":1,"loc":1,"n_params":0,"cyclocomp":1}')
  parsed <- parse_analyzer_records(lines)
  expect_null(parsed$summary)
  expect_equal(nrow(parsed$functions), 1L)
})

# ---------------------------------------------------------------------------
# parse_analyzer_records: functions
# ---------------------------------------------------------------------------

test_that("parse_analyzer_records extracts one row per function with honest NA for compiled langs", {
  parsed <- parse_analyzer_records(.fixture_lines())
  fns    <- parsed$functions

  expect_s3_class(fns, "data.frame")
  expect_equal(nrow(fns), 3L)
  expect_identical(
    names(fns),
    c("lang", "name", "exported", "file", "line", "loc", "n_params", "cyclocomp")
  )

  foo <- fns[fns$name == "foo", ]
  expect_identical(foo$lang, "r")
  expect_true(foo$exported)
  expect_identical(foo$line, 1L)
  expect_identical(foo$loc, 10L)
  expect_identical(foo$n_params, 2L)
  expect_identical(foo$cyclocomp, 3L)

  bar <- fns[fns$name == "bar", ]
  expect_false(bar$exported)
  expect_identical(bar$n_params, 0L)

  # Compiled function: honest NA (not 0) for exported/n_params/cyclocomp.
  nat <- fns[fns$name == "native_helper", ]
  expect_identical(nat$lang, "c")
  expect_true(is.na(nat$exported))
  expect_true(is.na(nat$n_params))
  expect_true(is.na(nat$cyclocomp))
  expect_identical(nat$loc, 20L)
  expect_identical(nat$line, 12L)
  expect_identical(nat$file, "src/helper.c")
})

# ---------------------------------------------------------------------------
# parse_analyzer_records: edges
# ---------------------------------------------------------------------------

test_that("parse_analyzer_records extracts one row per call edge across graphs", {
  parsed <- parse_analyzer_records(.fixture_lines())
  edges  <- parsed$edges

  expect_s3_class(edges, "data.frame")
  expect_equal(nrow(edges), 3L)
  expect_identical(names(edges), c("graph", "from", "to"))
  expect_setequal(edges$graph, c("r", "native", "c"))

  r_edge <- edges[edges$graph == "r", ]
  expect_identical(r_edge$from, "foo")
  expect_identical(r_edge$to, "bar")
})

test_that("parse_analyzer_records returns zero-row detail frames on an empty stream", {
  parsed <- parse_analyzer_records(character(0L))
  expect_null(parsed$summary)
  expect_equal(nrow(parsed$functions), 0L)
  expect_equal(nrow(parsed$edges), 0L)
  # Columns must still be present so downstream rbind/append is stable.
  expect_identical(
    names(parsed$functions),
    c("lang", "name", "exported", "file", "line", "loc", "n_params", "cyclocomp")
  )
  expect_identical(names(parsed$edges), c("graph", "from", "to"))
})

test_that("parse_analyzer_records raises analyzer_parse_incomplete for a line that does not parse", {
  lines <- c(
    "not json at all",
    '{"rec":"summary","package":"demo"}',
    paste0('{"rec":"function","name":"', strrep("x", 200L)),
    '{"rec":"function","lang":"r","name":"foo","exported":true,"file":"R/f.R","line":1,"loc":2,"n_params":1,"cyclocomp":1}'
  )
  err <- tryCatch(parse_analyzer_records(lines), error = function(e) e)
  expect_s3_class(err, "analyzer_parse_incomplete")
  expect_identical(err$n_bad, 2L)
  expect_identical(err$first_bad, "not json at all")
  expect_match(conditionMessage(err), "2 analyzer line(s) did not parse", fixed = TRUE)
})

test_that("blank lines are skipped, not counted as records that failed", {
  parsed <- parse_analyzer_records(c("", "   ", '{"rec":"summary","package":"demo"}', ""))
  expect_identical(parsed$summary$package, "demo")
})

test_that("the first bad line is kept to 80 bytes of valid UTF-8", {
  ascii <- paste0("{", strrep("y", 200L))
  err <- tryCatch(parse_analyzer_records(ascii), error = function(e) e)
  expect_identical(err$first_bad, substr(ascii, 1L, 80L))
  # 79 ASCII bytes, then a two-byte character the 80-byte cut splits.
  split <- paste0("{", strrep("z", 78L), "é", "tail")
  err <- tryCatch(parse_analyzer_records(split), error = function(e) e)
  expect_true(validUTF8(err$first_bad))
  expect_identical(err$first_bad, paste0("{", strrep("z", 78L), "<c3>"))
})

test_that("a cap while parsing one line parses it again and loses no record", {
  lines <- .fixture_lines()
  want  <- parse_analyzer_records(lines)
  real  <- jsonlite::fromJSON
  # The dcf record: the one curatedCRCData 3.22 lost when a cap was swallowed.
  local_mocked_bindings(
    fromJSON = .fires_cap_once(real, when = function(txt, ...) identical(txt, lines[[10L]])),
    .package = "jsonlite")
  expect_identical(parse_analyzer_records(lines), want)
})

# ---------------------------------------------------------------------------
# analyze_with_binary: end-to-end via a stub binary that cats a fixture
# ---------------------------------------------------------------------------

# Write an executable stub that ignores its argument and prints the fixture.
.write_stub_binary <- function(dir, ndjson_lines) {
  fixture <- file.path(dir, "fixture.ndjson")
  writeLines(ndjson_lines, fixture)
  stub <- file.path(dir, "stub-analyzer.sh")
  writeLines(c("#!/bin/sh", sprintf("cat %s", shQuote(fixture))), stub)
  Sys.chmod(stub, mode = "0755")
  stub
}

test_that("analyze_with_binary returns flattened summary with functions/edges attached", {
  skip_on_os("windows")
  dir <- tempfile("ccm_stub_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  stub <- .write_stub_binary(dir, .fixture_lines())
  withr::local_envvar(RPKG_ANALYZER_BIN = stub)

  metrics <- analyze_with_binary(dir)

  expect_false(is.null(metrics))
  expect_identical(metrics$package, "demo")
  expect_identical(metrics$n_fns_r, 2L)

  fns <- attr(metrics, "functions")
  eg  <- attr(metrics, "edges")
  expect_s3_class(fns, "data.frame")
  expect_equal(nrow(fns), 3L)
  expect_s3_class(eg, "data.frame")
  expect_equal(nrow(eg), 3L)
})

test_that("analyze_with_binary returns NULL when the binary is unavailable", {
  withr::local_envvar(RPKG_ANALYZER_BIN = "/nonexistent/path/to/nothing")
  # Also neutralise any rpkg-analyzer that might be on PATH in CI.
  skip_if(nzchar(unname(Sys.which("rpkg-analyzer"))),
          "a real rpkg-analyzer is on PATH")
  expect_null(analyze_with_binary(tempfile()))
})

# ---------------------------------------------------------------------------
# rpkg_analyzer_version: which build is about to run
# ---------------------------------------------------------------------------

# Write an executable stub that answers --version and nothing else.
.write_version_stub <- function(dir, line) {
  stub <- file.path(dir, "stub-version.sh")
  writeLines(c("#!/bin/sh", sprintf("echo %s", shQuote(line))), stub)
  Sys.chmod(stub, mode = "0755")
  stub
}

test_that("rpkg_analyzer_version reports the version the binary prints", {
  skip_on_os("windows")
  dir <- tempfile("bcm_stub_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  withr::local_envvar(
    RPKG_ANALYZER_BIN = .write_version_stub(dir, "rpkg-analyzer 0.4.0"))
  expect_identical(rpkg_analyzer_version(), "0.4.0")
})

test_that("rpkg_analyzer_version is NA when no binary is available", {
  withr::local_envvar(RPKG_ANALYZER_BIN = "/nonexistent/path/to/nothing")
  skip_if(nzchar(unname(Sys.which("rpkg-analyzer"))),
          "a real rpkg-analyzer is on PATH")
  expect_true(is.na(rpkg_analyzer_version()))
})

test_that("rpkg_analyzer_version is NA when the output is not a version banner", {
  # A guess here would clear the scan marker on every run and never settle, so
  # anything that does not look like the banner has to read as unknown.
  skip_on_os("windows")
  dir <- tempfile("bcm_stub_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  stub <- .write_version_stub(dir, "usage: rpkg-analyzer DIR")
  withr::local_envvar(RPKG_ANALYZER_BIN = stub)
  expect_true(is.na(rpkg_analyzer_version()))
})

# The stale-scan check compares a stored analyzer_version against what
# rpkg_analyzer_version() reads from --version, while analyze.R stores whatever
# the analyzer's own summary record says and only falls back to --version when
# the record is silent. Those are two channels for one fact. If a build ever
# reports itself differently in the two, every stored row reads as stale, every
# run clears every marker, and the pipeline republishes forever. Pin them
# together here so that shows up as a test failure instead.
test_that("the analyzer reports the same version in its record and on --version", {
  skip_if_not(nzchar(rpkg_analyzer_bin()), "needs rpkg-analyzer")

  dir <- tempfile("bcm_ver_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  writeLines(c("Package: verpkg", "Version: 1.0", "Title: T", "Description: D.",
               "License: MIT + file LICENSE"), file.path(dir, "DESCRIPTION"))
  dir.create(file.path(dir, "R"))
  writeLines("f <- function() 1", file.path(dir, "R", "f.R"))

  metrics <- analyze_with_binary(dir)
  skip_if(is.null(metrics), "the analyzer produced no summary record")
  skip_if(is.null(metrics[["analyzer_version"]]),
          "this build does not report its version in the summary record")

  expect_identical(as.character(metrics[["analyzer_version"]]),
                   rpkg_analyzer_version())
})

# ---------------------------------------------------------------------------
# The input kind, and what the analyzer prints besides the summary
# ---------------------------------------------------------------------------

test_that("analyzer_at_least reads the leading version and nothing else", {
  expect_true(analyzer_at_least("0.5.0", "0.5.0"))
  expect_true(analyzer_at_least("0.5.0-test", "0.5.0"))
  expect_true(analyzer_at_least("0.10.0", "0.5.0"))
  for (v in list("0.4.0", "0.4.0-test", NA, NA_character_, NULL, "", "dev")) {
    expect_false(analyzer_at_least(v, "0.5.0"), info = format(v))
  }
})

test_that("parse_analyzer_records keeps the first DESCRIPTION and release-notes records", {
  parsed <- parse_analyzer_records(c(
    '{"rec":"summary","package":"demo"}',
    '{"rec":"dcf","Package":"demo","Version":"1.2.0","Config/testthat/edition":"3"}',
    '{"rec":"dcf","Package":"second"}',
    '{"rec":"release_notes","package_version":"1.2.0","news_file":"NEWS.md","release_notes_source":"news_md","release_notes":"- fixed","release_notes_truncated":false}',
    '{"rec":"release_notes","package_version":"9.9.9"}',
    '{"rec":"something_new","x":1}'))
  expect_identical(parsed$dcf, c(Package = "demo", Version = "1.2.0",
                                 `Config/testthat/edition` = "3"))
  expect_identical(parsed$release_notes$package_version, "1.2.0")
  expect_false(parsed$release_notes$release_notes_truncated)
  expect_identical(parsed$summary$package, "demo")
})

test_that("an empty DESCRIPTION record is kept apart from a missing one", {
  expect_identical(length(parse_analyzer_records('{"rec":"dcf"}')$dcf), 0L)
  expect_false(is.null(parse_analyzer_records('{"rec":"dcf"}')$dcf))
  expect_null(parse_analyzer_records('{"rec":"summary"}')$dcf)
  expect_null(parse_analyzer_records('{"rec":"summary"}')$release_notes)
})

test_that("analyze_with_binary passes the input kind after the directory", {
  skip_on_os("windows")
  dir <- withr::local_tempdir()
  args_file <- file.path(dir, "args.txt")
  stub <- file.path(dir, "stub-args.sh")
  writeLines(c("#!/bin/sh",
               sprintf("printf '%%s\\n' \"$@\" > %s", shQuote(args_file)),
               'echo "{\\"rec\\":\\"summary\\",\\"n_fns_r\\":1}"',
               'echo "{\\"rec\\":\\"dcf\\",\\"Package\\":\\"demo\\"}"'), stub)
  Sys.chmod(stub, mode = "0755")
  withr::local_envvar(RPKG_ANALYZER_BIN = stub)

  pkg <- file.path(dir, "pkg")
  dir.create(pkg)
  metrics <- analyze_with_binary(pkg)
  expect_identical(readLines(args_file), c(pkg, "--input-kind", ANALYZER_INPUT_KIND))
  expect_identical(attr(metrics, "dcf"), c(Package = "demo"))
  expect_null(attr(metrics, "release_notes"))
})

# A stub that answers --version and, on any package, prints one summary naming
# `input_kind` when it is given, the way a 0.5.0 build does.
.write_selfcheck_stub <- function(dir, version, input_kind = NULL) {
  kind <- if (is.null(input_kind)) "" else
    sprintf(',\\"input_kind\\":\\"%s\\"', input_kind)
  stub <- file.path(dir, "stub-selfcheck.sh")
  writeLines(c(
    "#!/bin/sh",
    'if [ "$1" = "--version" ]; then',
    sprintf('  echo "rpkg-analyzer %s"', version),
    "  exit 0",
    "fi",
    sprintf('echo "{\\"rec\\":\\"summary\\",\\"loc_r\\":1%s}"', kind)), stub)
  Sys.chmod(stub, mode = "0755")
  stub
}

test_that("the self-check passes only when the analyzer names the kind it was given", {
  skip_on_os("windows")
  withr::local_envvar(RPKG_ANALYZER_BIN = .write_selfcheck_stub(
    withr::local_tempdir(), "0.5.0-test", input_kind = "git"))
  expect_true(rpkg_analyzer_selfcheck("git"))
  expect_false(rpkg_analyzer_selfcheck("release"))

  withr::local_envvar(RPKG_ANALYZER_BIN = .write_selfcheck_stub(
    withr::local_tempdir(), "0.5.0-test"))
  expect_false(rpkg_analyzer_selfcheck("git"))
})

.sc_io <- function() list(
  package_list = function() data.frame(package = "pkgA", latest_version = "1.0",
                                       stringsAsFactors = FALSE),
  clone = function(pkg, dest) { dir.create(dest, showWarnings = FALSE); TRUE })

test_that("a 0.5.0 analyzer that does not name the kind stops the run before any shard", {
  skip_on_os("windows")
  withr::local_envvar(RPKG_ANALYZER_BIN = .write_selfcheck_stub(
    withr::local_tempdir(), "0.5.0-test"))
  out <- withr::local_tempdir()
  expect_error(run_update(.sc_io(), out, shard_size = 10L), "--input-kind git")
  expect_false(file.exists(file.path(out, DB_FILENAME)))
})

test_that("a build before 0.5.0 is never asked for the self-check", {
  skip_on_os("windows")
  # This stub would fail the self-check, so reaching the shard proves it was skipped.
  withr::local_envvar(RPKG_ANALYZER_BIN = .write_selfcheck_stub(
    withr::local_tempdir(), "0.4.0-test"))
  out <- withr::local_tempdir()
  m <- suppressWarnings(run_update(.sc_io(), out, shard_size = 10L))
  expect_identical(m$n_shard, 1L)
})

test_that("the analyzer CI installs answers the self-check package", {
  # Before 0.5.0 the flag is ignored and a summary still comes back; from 0.5.0 it
  # must name the kind, or every scheduled run stops before its first shard.
  skip_on_os("windows")
  skip_if(!nzchar(rpkg_analyzer_bin()), "needs rpkg-analyzer")
  dir <- withr::local_tempdir()
  writeLines(c("Package: selfcheck", "Version: 0.0.1"), file.path(dir, "DESCRIPTION"))
  expect_false(is.null(analyze_with_binary(dir)))
  if (analyzer_at_least(rpkg_analyzer_version(), "0.5.0")) {
    expect_true(rpkg_analyzer_selfcheck())
  }
})

# ---------------------------------------------------------------------------
# A crashed analyzer, and a cap while asking the analyzer its version
# ---------------------------------------------------------------------------

test_that("an analyzer that prints its summary and then exits 101 gives the R fallback, and one that is killed is raised", {
  skip_on_os("windows")
  dir <- withr::local_tempdir()
  withr::local_envvar(RPKG_ANALYZER_STATS = NA)
  crashed <- function(ending) {
    stub <- file.path(dir, "stub-crash.sh")
    writeLines(c("#!/bin/sh",
                 'echo "{\\"rec\\":\\"summary\\",\\"package\\":\\"demo\\"}"',
                 ending), stub)
    Sys.chmod(stub, mode = "0755")
    withr::local_envvar(RPKG_ANALYZER_BIN = stub)
    tryCatch(analyze_with_binary(dir), error = function(e) e)
  }
  expect_null(crashed("exit 101"))
  # A real signal, which the shell reports as 128 plus its number.
  killed <- crashed("kill -9 $$")
  expect_s3_class(killed, "analyzer_killed")
  expect_identical(killed$status, 137L)
})

test_that("a cap while asking the analyzer its version asks again", {
  skip_on_os("windows")
  dir  <- withr::local_tempdir()
  stub <- file.path(dir, "stub-version.sh")
  writeLines(c("#!/bin/sh", 'echo "rpkg-analyzer 0.5.0-test"'), stub)
  Sys.chmod(stub, mode = "0755")
  withr::local_envvar(RPKG_ANALYZER_BIN = stub)
  .local_global("system2", .fires_cap_once(base::system2))
  expect_identical(rpkg_analyzer_version(), "0.5.0-test")
})

test_that("a cap while the analyzer runs runs it again for that version", {
  skip_on_os("windows")
  dir  <- withr::local_tempdir()
  stub <- .write_stub_binary(dir, .fixture_lines())
  withr::local_envvar(RPKG_ANALYZER_BIN = stub)
  want <- analyze_with_binary(dir)
  .local_global("system2", .fires_cap_once(base::system2))
  expect_identical(analyze_with_binary(dir), want)
})
