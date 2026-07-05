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

test_that("parse_analyzer_records skips unparseable lines without aborting", {
  lines <- c(
    "not json at all",
    '{"rec":"summary","package":"demo"}',
    '{"rec":"function","lang":"r","name":"foo","exported":true,"file":"R/f.R","line":1,"loc":2,"n_params":1,"cyclocomp":1}'
  )
  parsed <- parse_analyzer_records(lines)
  expect_identical(parsed$summary$package, "demo")
  expect_equal(nrow(parsed$functions), 1L)
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
