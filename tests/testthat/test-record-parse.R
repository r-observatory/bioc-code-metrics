# tests/testthat/test-record-parse.R: the one-call record parse, and the
# dataset memo, give exactly what the per-line parser gives.

# A dataset record whose line is at least .RECORD_LONG_BYTES long.
.long_dataset_line <- function(name, fp = "cf", n_cols = 60L, extra = "") {
  cols <- vapply(seq_len(n_cols), function(i) sprintf(
    '{"is_factor":false,"mean":%s,"n_missing":0,"n_unique":%d,"name":"c%03d","type":"numeric"}',
    format(i / 7, digits = 17), i, i), character(1L))
  sprintf(paste0('{"columns":[%s],"content_fp":"%s","file":"data/%s.rda","kind":"table",',
                 '"name":"%s","ncol":%d,"nrow":3,"rec":"dataset"%s}'),
          paste(cols, collapse = ","), fp, name, name, n_cols, extra)
}

# Every record kind, short and long, with blank lines, a JSON null, a second
# summary and a second dcf that must both lose to the first.
.mixed_lines <- function() {
  c(
    '{"lang_breakdown":{"C":10,"R":40},"n_fns_r":2,"package":"demo","rec":"summary"}',
    "",
    '{"name":"utils","rec":"dependency"}',
    '{"cyclocomp":3,"exported":true,"file":"R/foo.R","lang":"r","line":1,"loc":10,"n_params":2,"name":"foo","rec":"function"}',
    '{"file":"src/h.c","lang":"c","line":12,"loc":20,"name":"native_helper","rec":"function"}',
    '{"from":"foo","graph":"r","rec":"call_edge","to":"bar"}',
    "   ",
    "null",
    '{"class":"data.frame","content_fp":"s1","name":"small","nrow":2,"rec":"dataset"}',
    .long_dataset_line("big"),
    '{"n_fns_r":99,"package":"second","rec":"summary"}',
    '{"Package":"demo","Title":"Caf\\u00e9 \\u00e0 la carte","rec":"dcf"}',
    '{"Package":"ignored","rec":"dcf"}',
    sprintf('{"file":"NEWS.md","rec":"release_notes","text":"%s"}', strrep("x", 5000L)),
    '{"from":"bar","graph":"native","rec":"call_edge","to":"native_helper"}'
  )
}

# Streams for the jsonlite number and null quirks, short and long: 1.5e-05 and
# 5e-05 write as 0, -0 as -0, 1e16 in full, 12345.678912 to four decimals.
.quirk_streams <- function() {
  quirks <- paste0('"q1":1.5e-05,"q2":5e-05,"q3":1e-05,"q4":-0,"q5":1e16,',
                   '"q6":12345.678912,"q7":null,"q8":"na\\u00efve"')
  nested <- paste0('"mix":[1.5e-05,5e-05,1e-05,-0,1e16,12345.678912,null,"\\u00e9"]')
  short <- c(
    sprintf('{"name":"a","rec":"dataset",%s,%s}', quirks, nested),
    '{"mix":1.5e-05,"name":"b","q7":3,"rec":"dataset"}',
    '{"mix":{"k":null},"name":"c","rec":"dataset"}')
  long <- c(
    .long_dataset_line("d", extra = paste0(",", quirks, ",", nested)),
    .long_dataset_line("e", extra = ',"mix":"scalar","q7":null'))
  list(short = short, long = long, both = c(short, long), reversed = c(long, short))
}

test_that("the one-call parse gives what the per-line parser gives", {
  lines <- .mixed_lines()
  want  <- .parse_records_per_line(lines)
  expect_identical(parse_analyzer_records(lines), want)
  expect_identical(nrow(want$functions), 2L)
  expect_identical(nrow(want$datasets), 2L)
  expect_identical(want$summary$package, "demo")
})

test_that("an empty stream and a stream of blank lines give the per-line result", {
  for (lines in list(character(0L), c("", "  ", "\t"))) {
    expect_identical(parse_analyzer_records(lines), .parse_records_per_line(lines))
  }
})

test_that("jsonlite's number and null quirks come out the same, short and long", {
  for (lines in .quirk_streams()) {
    expect_identical(parse_analyzer_records(lines), .parse_records_per_line(lines))
  }
})

test_that("R values jsonlite never produces still flatten the same", {
  recs <- list(
    list(rec = "dataset", name = "a", v = NA, w = list(NA, NaN, Inf, -Inf), x = 1L),
    list(rec = "dataset", name = "b", v = TRUE, w = NULL, x = 2.5, y = list(a = NULL)),
    list(rec = "dataset", name = "c", x = NA_real_, y = "z", columns = list(1, 2, 3)),
    list(rec = "dataset", name = "d", x = 3, y = NaN, n_cols = 99L))
  expect_identical(.datasets_frame_flat(lapply(recs, .dataset_flat)), .datasets_frame(recs))
  expect_identical(.datasets_frame_flat(list()), .datasets_frame(list()))
})

test_that("a line that does not parse still ends in analyzer_parse_incomplete", {
  cases <- list(
    short = c('{"package":"p","rec":"summary"}', "not json"),
    long  = c('{"package":"p","rec":"summary"}', substr(.long_dataset_line("x"), 1L, 5000L)),
    # Two values on one line would add a record to the one-call parse.
    two_values = c('{"package":"p","rec":"summary"}', "1,2", '{"name":"f","rec":"function"}'))
  for (nm in names(cases)) {
    lines <- cases[[nm]]
    want <- tryCatch(.parse_records_per_line(lines), error = function(e) e)
    got  <- tryCatch(parse_analyzer_records(lines), error = function(e) e)
    expect_s3_class(got, "analyzer_parse_incomplete")
    expect_identical(got$n_bad, 1L, info = nm)
    expect_identical(got$first_bad, want$first_bad, info = nm)
  }
})

test_that("a cap during the one-call parse parses again and loses no record", {
  lines <- .mixed_lines()
  want  <- .parse_records_per_line(lines)
  real  <- jsonlite::fromJSON
  local_mocked_bindings(
    fromJSON = .fires_cap_once(real, when = function(txt, ...) startsWith(txt, "[")),
    .package = "jsonlite")
  expect_identical(parse_analyzer_records(lines), want)
})

test_that("a cap while parsing a long line parses it again and loses no record", {
  lines <- .mixed_lines()
  want  <- .parse_records_per_line(lines)
  real  <- jsonlite::fromJSON
  local_mocked_bindings(
    fromJSON = .fires_cap_once(real, when = function(txt, ...) identical(txt, lines[[10L]])),
    .package = "jsonlite")
  expect_identical(parse_analyzer_records(lines), want)
})

# ---------------------------------------------------------------------------
# The dataset memo
# ---------------------------------------------------------------------------

# jsonlite::fromJSON counting the calls made on a line of its own (not on the
# one-call array) until the calling test ends. Returns the counter.
.count_line_parses <- function(env = parent.frame()) {
  n <- 0L
  real <- jsonlite::fromJSON
  local_mocked_bindings(fromJSON = function(txt, ...) {
    if (!startsWith(txt, "[")) n <<- n + 1L
    real(txt, ...)
  }, .package = "jsonlite", .env = env)
  function() n
}

test_that("a memo changes nothing on any of these streams", {
  for (lines in c(list(.mixed_lines()), .quirk_streams())) {
    expect_identical(parse_analyzer_records(lines, .record_memo()), .parse_records_per_line(lines))
  }
})

test_that("a memo hit returns what a parse returns, without parsing the line again", {
  lines <- c('{"package":"p","rec":"summary"}', .long_dataset_line("x"),
             .long_dataset_line("y", fp = "other"))
  want <- .parse_records_per_line(lines)
  memo <- .record_memo()
  parses <- .count_line_parses()
  expect_identical(parse_analyzer_records(lines, memo), want)
  expect_identical(parses(), 2L)
  expect_identical(parse_analyzer_records(lines, memo), want)
  expect_identical(parses(), 2L)
})

test_that("the memo keeps two generations and a hit stays for the next version", {
  a <- .long_dataset_line("a")
  b <- .long_dataset_line("b")
  memo <- .record_memo()
  parses <- .count_line_parses()
  parse_analyzer_records(c(a, b), memo)   # version 1: both parsed
  parse_analyzer_records(a, memo)         # version 2: a from version 1
  parse_analyzer_records(a, memo)         # version 3: a again, kept by the hit
  expect_identical(parses(), 2L)
  parse_analyzer_records(b, memo)         # version 4: b was three versions back
  expect_identical(parses(), 3L)
  key <- function(x) digest::digest(x, algo = "sha256", serialize = FALSE)
  expect_identical(ls(memo$cur), key(b))
  expect_identical(ls(memo$prev), key(a))
})

test_that("a cap while parsing a long line with a memo parses it again and keeps it", {
  lines <- .mixed_lines()
  want  <- .parse_records_per_line(lines)
  memo  <- .record_memo()
  real  <- jsonlite::fromJSON
  local_mocked_bindings(
    fromJSON = .fires_cap_once(real, when = function(txt, ...) identical(txt, lines[[10L]])),
    .package = "jsonlite")
  expect_identical(parse_analyzer_records(lines, memo), want)
  expect_identical(parse_analyzer_records(lines, memo), want)
})

test_that("the worker tally counts memo hits and misses, and incomplete parses", {
  .tally_reset()
  memo  <- .record_memo()
  lines <- c(.long_dataset_line("x"), .long_dataset_line("y"))
  parse_analyzer_records(lines, memo)
  parse_analyzer_records(lines, memo)
  expect_s3_class(tryCatch(parse_analyzer_records("not json"), error = function(e) e),
                  "analyzer_parse_incomplete")
  expect_identical(.tally_snapshot()[c("incomplete_parses", "memo_hits", "memo_misses")],
                   list(incomplete_parses = 1, memo_hits = 2, memo_misses = 2))
})
