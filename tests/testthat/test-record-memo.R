# analyze_package gives the same frames with the dataset memo, without it and
# through the per-line parser, on a stub analyzer and on every real build
# present (the installed one and the RPA_TEST_BIN_040 and _050 builds).

# A repository with releases 1.0, 1.1 and 1.2 on RELEASE_ branches, as
# github.com/bioc keeps them; `data(v)` names the frames saved at release v.
# Frames are wide, so their dataset records are long lines.
.wide_frame <- function(n = 60L) {
  as.data.frame(stats::setNames(lapply(seq_len(n), function(i) c(i, i / 7, i * 3)),
                                sprintf("c%02d", seq_len(n))))
}
.default_data <- function(v) {
  list(kept = .wide_frame(), changing = .wide_frame(if (identical(v, "1.2")) 59L else 60L))
}
.memo_fixture_repo <- function(dest, data = .default_data) {
  git <- function(...) system2("git", c("-C", dest, ...), stdout = FALSE, stderr = FALSE)
  system2("git", c("init", dest), stdout = FALSE, stderr = FALSE)
  git("config", "user.email", "test@example.com")
  git("config", "user.name", "Test Bot")
  writeLines("# memofix", file.path(dest, "README"))
  git("add", "-A")
  git("commit", "-m", "init")
  for (v in c("1.0", "1.1", "1.2")) {
    git("checkout", "-b", paste0("RELEASE_", gsub(".", "_", v, fixed = TRUE)))
    dir.create(file.path(dest, "R"), showWarnings = FALSE)
    dir.create(file.path(dest, "data"), showWarnings = FALSE)
    writeLines(c("Package: memofix", paste("Version:", v), "Title: Memo Fixture",
                 "Description: A fixture.", "License: MIT"), file.path(dest, "DESCRIPTION"))
    writeLines("export(f)", file.path(dest, "NAMESPACE"))
    writeLines("f <- function(x) x + 1", file.path(dest, "R", "f.R"))
    for (nm in names(data(v))) {
      assign(nm, data(v)[[nm]])
      save(list = nm, file = file.path(dest, "data", paste0(nm, ".rda")))
    }
    git("add", "-A")
    git("commit", "-m", paste0("release-", v))
    git("checkout", "-")
  }
  dest
}

# A stub analyzer that prints lines-<version>.ndjson for the version it is
# pointed at; `records(v)` lists the dataset records of version v as
# list(name, content_fp, ncol). By default kept never changes and changing
# differs at 1.2.
.default_records <- function(v) {
  list(list("kept", "k", 60L),
       if (identical(v, "1.2")) list("changing", "c2", 59L) else list("changing", "c1", 60L))
}
.stub_lines_bin <- function(dir, records = .default_records) {
  wide <- function(name, fp, n) {
    cols <- paste(sprintf('{"is_factor":false,"mean":%s,"n_missing":0,"n_unique":3,"name":"c%02d","type":"numeric"}',
                          format(seq_len(n) / 7, digits = 17), seq_len(n)), collapse = ",")
    sprintf('{"columns":[%s],"content_fp":"%s","file":"data/%s.rda","name":"%s","ncol":%d,"nrow":3,"rec":"dataset"}',
            cols, fp, name, name, n)
  }
  for (v in c("1.0", "1.1", "1.2")) {
    writeLines(c(
      sprintf('{"loc_r":1,"n_fns_r":1,"package":"memofix","rec":"summary","version":"%s"}', v),
      '{"exported":true,"file":"R/f.R","lang":"r","line":1,"loc":1,"n_params":1,"name":"f","rec":"function"}',
      vapply(records(v), function(r) wide(r[[1L]], r[[2L]], r[[3L]]), character(1))),
      file.path(dir, sprintf("lines-%s.ndjson", v)))
  }
  stub <- file.path(dir, "stub-lines.sh")
  writeLines(c(
    "#!/bin/sh",
    'if [ "$1" = "--version" ]; then echo "rpkg-analyzer 0.4.0-test"; exit 0; fi',
    'd=$(echo "$1" | tr -d "\'")',
    'v=$(sed -n "s/^Version: *//p" "$d/DESCRIPTION" | head -1)',
    sprintf('cat "%s/lines-$v.ndjson"', dir)), stub)
  Sys.chmod(stub, mode = "0755")
  stub
}

# A result without the second each version's text was read at, the one value
# two analyses of the same package are expected to differ in.
.without_read_at <- function(res) {
  res$text <- lapply(res$text, function(d) {
    if (is.data.frame(d)) d$read_at <- NULL
    d
  })
  res
}

# analyze_package with the memo, with no memo, and with the per-line parser.
.analyze_three_ways <- function(repo) {
  memo <- analyze_package(repo, "memofix")
  .local_global(".record_memo", function() NULL)
  no_memo <- analyze_package(repo, "memofix")
  .local_global("parse_analyzer_records",
                function(lines, memo = NULL) .parse_records_per_line(lines))
  per_line <- analyze_package(repo, "memofix")
  lapply(list(memo = memo, no_memo = no_memo, per_line = per_line), .without_read_at)
}

test_that("analyze_package gives the same frames with the memo, without it and per line", {
  skip_on_os("windows")
  repo <- .memo_fixture_repo(withr::local_tempdir())
  withr::local_envvar(RPKG_ANALYZER_BIN = .stub_lines_bin(withr::local_tempdir()))
  n <- 0L
  real <- jsonlite::fromJSON
  local_mocked_bindings(fromJSON = function(txt, ...) {
    if (nchar(txt, type = "bytes") >= .RECORD_LONG_BYTES && !startsWith(txt, "[")) n <<- n + 1L
    real(txt, ...)
  }, .package = "jsonlite")
  with_memo <- .without_read_at(analyze_package(repo, "memofix"))
  n_memo <- n
  got <- .analyze_three_ways(repo)
  expect_identical(with_memo, got$per_line)
  expect_identical(got$memo, got$per_line)
  expect_identical(got$no_memo, got$per_line)
  expect_identical(nrow(with_memo$datasets), 6L)
  # 1.0 parses kept and changing, 1.1 nothing, 1.2 only the new changing.
  expect_identical(n_memo, 3L)
})

test_that("analyze_package gives the same frames on every real analyzer build present", {
  skip_on_os("windows")
  bins <- unique(Filter(function(b) nzchar(b) && file.exists(b),
                        c(rpkg_analyzer_bin(), Sys.getenv("RPA_TEST_BIN_040"),
                          Sys.getenv("RPA_TEST_BIN_050"))))
  skip_if(!length(bins), "no rpkg-analyzer build here")
  repo <- .memo_fixture_repo(withr::local_tempdir())
  for (b in bins) {
    withr::local_envvar(RPKG_ANALYZER_BIN = b)
    got <- .analyze_three_ways(repo)
    expect_identical(got$memo, got$per_line, info = b)
    expect_identical(got$no_memo, got$per_line, info = b)
    expect_gt(nrow(got$memo$datasets), 0L)
  }
})

test_that("a record absent at one version is parsed again when it comes back", {
  skip_on_os("windows")
  gap <- function(v) if (identical(v, "1.1")) list() else list(gap = .wide_frame())
  repo <- .memo_fixture_repo(withr::local_tempdir(), gap)
  withr::local_envvar(RPKG_ANALYZER_BIN = .stub_lines_bin(withr::local_tempdir(), function(v) {
    if (identical(v, "1.1")) list() else list(list("gap", "g", 60L))
  }))
  n <- 0L
  real <- jsonlite::fromJSON
  local_mocked_bindings(fromJSON = function(txt, ...) {
    if (nchar(txt, type = "bytes") >= .RECORD_LONG_BYTES && !startsWith(txt, "[")) n <<- n + 1L
    real(txt, ...)
  }, .package = "jsonlite")
  with_memo <- .without_read_at(analyze_package(repo, "memofix"))
  expect_identical(n, 2L)   # 1.0 and 1.2; 1.1 held nothing to keep for 1.2
  got <- .analyze_three_ways(repo)
  expect_identical(with_memo, got$per_line)
  expect_identical(got$memo, got$per_line)
  expect_identical(got$no_memo, got$per_line)
  expect_identical(nrow(with_memo$datasets), 2L)
})

test_that("a record identical in all three versions is parsed once", {
  skip_on_os("windows")
  same <- function(v) list(kept = .wide_frame())
  repo <- .memo_fixture_repo(withr::local_tempdir(), same)
  withr::local_envvar(RPKG_ANALYZER_BIN = .stub_lines_bin(withr::local_tempdir(), function(v) {
    list(list("kept", "k", 60L))
  }))
  n <- 0L
  real <- jsonlite::fromJSON
  local_mocked_bindings(fromJSON = function(txt, ...) {
    if (nchar(txt, type = "bytes") >= .RECORD_LONG_BYTES && !startsWith(txt, "[")) n <<- n + 1L
    real(txt, ...)
  }, .package = "jsonlite")
  with_memo <- .without_read_at(analyze_package(repo, "memofix"))
  expect_identical(n, 1L)
  got <- .analyze_three_ways(repo)
  expect_identical(with_memo, got$per_line)
  expect_identical(got$memo, got$per_line)
  expect_identical(got$no_memo, got$per_line)
  expect_identical(nrow(with_memo$datasets), 3L)
})

test_that("the same holds on every real build for a gap and for identical versions", {
  skip_on_os("windows")
  bins <- unique(Filter(function(b) nzchar(b) && file.exists(b),
                        c(rpkg_analyzer_bin(), Sys.getenv("RPA_TEST_BIN_040"),
                          Sys.getenv("RPA_TEST_BIN_050"))))
  skip_if(!length(bins), "no rpkg-analyzer build here")
  fixtures <- list(
    gap  = function(v) if (identical(v, "1.1")) list() else list(gap = .wide_frame()),
    same = function(v) list(kept = .wide_frame()))
  for (nm in names(fixtures)) {
    repo <- .memo_fixture_repo(withr::local_tempdir(), fixtures[[nm]])
    for (b in bins) {
      withr::local_envvar(RPKG_ANALYZER_BIN = b)
      got <- .analyze_three_ways(repo)
      expect_identical(got$memo, got$per_line, info = paste(nm, b))
      expect_identical(got$no_memo, got$per_line, info = paste(nm, b))
    }
  }
})
