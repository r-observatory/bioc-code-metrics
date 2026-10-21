# tests/testthat/test-rows-generation.R: the rows R writes for one release, held to
# a golden digest, and the cross-version columns held to the inputs a skipped
# release carries. The fixture package and the digest below make the golden, so
# a change to either regenerates it.

.rg_golden <- function() {
  jsonlite::read_json(test_path("fixtures", "rows-golden.json"), simplifyVector = TRUE)
}

# The fix a digest that moved asks for: a bump, unless the golden is a generation behind.
.rg_fix <- function(g, path) {
  if (!identical(as.integer(g$rows_generation), ROWS_GENERATION)) {
    return("ROWS_GENERATION changed: regenerate tests/testthat/fixtures/rows-golden.json in the same pull request")
  }
  fix <- "rows changed: bump ROWS_GENERATION in scripts/config.R and regenerate the golden"
  if (identical(path, "analyzer")) return(fix)
  paste0(fix, ". A change confined to scripts/metrics/ needs a new golden but no ",
         "ROWS_GENERATION bump: scripts/metrics/ runs only on the R fallback, and the rows ",
         "the fallback writes name no analyzer build, so they are outside every output ",
         "class and are read again whenever their package is next queued")
}

# The update run's unit tests set ROWS_GOLDEN_ADVISORY=1, so there a golden that
# no longer holds skips with its note; the Test workflow is where it fails.
.rg_advise <- function(holds, note) {
  if (!isTRUE(holds) && identical(Sys.getenv("ROWS_GOLDEN_ADVISORY"), "1")) {
    skip(paste("rows golden advisory here (ROWS_GOLDEN_ADVISORY=1), enforced on pull requests:",
               note))
  }
}

.rg_check_fallback <- function(g, got) {
  .rg_advise(identical(got, g$fallback),
             paste("fallback digest", got, "is not the golden's.", .rg_fix(g, "fallback")))
  expect_identical(got, g$fallback, info = .rg_fix(g, "fallback"))
}

.rg_check_analyzer <- function(g, got, cls) {
  class_fix <- "the output class changed: regenerate the golden, with no ROWS_GENERATION bump"
  none_fix  <- paste("no analyzer digest recorded: set \"analyzer\" in",
                     "tests/testthat/fixtures/rows-golden.json to", got)
  in_class  <- identical(cls, as.character(g$output_class))
  .rg_advise(in_class, class_fix)
  .rg_advise(!is.null(g$analyzer), none_fix)
  .rg_advise(identical(got, g$analyzer),
             paste("analyzer digest", got, "is not the golden's.", .rg_fix(g, "analyzer")))
  expect_identical(cls, as.character(g$output_class), info = class_fix)
  expect_false(is.null(g$analyzer), info = none_fix)
  if (in_class && !is.null(g$analyzer)) {
    expect_identical(got, g$analyzer, info = .rg_fix(g, "analyzer"))
  }
}

# Git with no global or system config, so a developer's config cannot change the fixture.
.rg_git <- function(dir, date, ...) {
  err <- tempfile("rg_git_")
  on.exit(unlink(err), add = TRUE)
  out <- suppressWarnings(system2(
    "git", c("-C", shQuote(dir), ...), stdout = TRUE, stderr = err,
    env = c("GIT_CONFIG_GLOBAL=/dev/null", "GIT_CONFIG_NOSYSTEM=1",
            "GIT_AUTHOR_NAME=T", "GIT_AUTHOR_EMAIL=t@t.test",
            "GIT_COMMITTER_NAME=T", "GIT_COMMITTER_EMAIL=t@t.test",
            paste0("GIT_AUTHOR_DATE=", date), paste0("GIT_COMMITTER_DATE=", date))))
  status <- attr(out, "status") %||% 0L
  if (status != 0L) {
    stop(sprintf("rows golden fixture: git %s exited with status %d: %s",
                 ..1, status, paste(readLines(err, warn = FALSE), collapse = " ")),
         call. = FALSE)
  }
  out
}

.RG_ADA   <- 'person("Ada", "Lovelace", email = "ada@example.org", role = c("aut", "cre"))'
.RG_GRACE <- 'person("Grace", "Hopper", email = "grace@example.org", role = "aut")'
.RG_ALAN  <- 'person("Alan", "Turing", role = "ctb")'

# Each release: its branch date, its package version, the DESCRIPTION fields that
# move, its exports, its R code, its test file (1.3 renames it, which churn reads
# as a rename) and the committed data file it ships as data/x.rda.
.RG_RELEASES <- list(
  "1.0" = list(
    date = "2026-01-01T00:00:00Z", version = "1.0.0",
    desc = c(paste0("Authors@R: ", .RG_ADA),
             "Depends: R (>= 4.3.0)", "Imports: stats", "Suggests: testthat"),
    exports = c("add_one", "abs_val"),
    code = c("add_one <- function(x) x + 1",
             "abs_val <- function(x) if (x < 0) -x else x"),
    test = "test-clamp.R", data = "x1.rda"),
  "1.1" = list(
    date = "2026-02-01T00:00:00Z", version = "1.2.0",
    desc = c(paste0("Authors@R: c(", .RG_ADA, ", ", .RG_GRACE, ")"),
             "Depends: R (>= 4.3.0), methods", "Imports: stats, utils, S4Vectors",
             "Suggests: testthat, knitr, rmarkdown, BiocStyle", "LinkingTo: Rcpp",
             "VignetteBuilder: knitr"),
    exports = c("add_one", "abs_val", "clamp"),
    code = c("add_one <- function(x) x + 1",
             "abs_val <- function(x) {", "  .Deprecated(\"clamp\")", "  clamp(abs(x))", "}",
             "clamp <- function(x, lo = 0, hi = 1) {",
             "  if (x < lo) lo else if (x > hi) hi else x", "}"),
    test = "test-clamp.R", data = "x1.rda"),
  "1.2" = list(
    date = "2026-03-01T00:00:00Z", version = "1.4.0",
    desc = c(paste0("Authors@R: c(", .RG_ADA, ", ", .RG_GRACE, ", ", .RG_ALAN, ")"),
             "Depends: R (>= 4.4.0)", "Imports: stats, utils, S4Vectors, lifecycle",
             "Suggests: knitr, rmarkdown, BiocStyle", "LinkingTo: Rcpp, cpp11",
             "VignetteBuilder: knitr"),
    exports = c("add_one", "clamp", "scale01"),
    code = c("add_one <- function(x) {",
             "  lifecycle::deprecate_warn(\"1.4.0\", \"add_one()\", \"scale01()\")", "  x + 1", "}",
             "clamp <- function(x, lo = 0, hi = 1) {",
             "  if (x < lo) lo else if (x > hi) hi else x", "}",
             "scale01 <- function(x) (x - min(x)) / diff(range(x))"),
    test = "test-clamp.R", data = "x2.rda"),
  "1.3" = list(
    date = "2026-04-01T00:00:00Z", version = "1.6.0",
    desc = c(paste0("Authors@R: c(",
                    sub('role = "aut"', 'role = c("aut", "cre")', .RG_GRACE, fixed = TRUE), ", ",
                    sub('role = c("aut", "cre")', 'role = "aut"', .RG_ADA, fixed = TRUE), ", ",
                    .RG_ALAN, ")"),
             "Depends: R (>= 4.5.0)", "Imports: utils, S4Vectors, lifecycle",
             "Suggests: knitr, rmarkdown, BiocStyle, covr", "LinkingTo: cpp11",
             "VignetteBuilder: knitr"),
    exports = c("clamp", "scale01", "tally"),
    code = c("add_one <- function(x) .Defunct(\"scale01\")",
             "clamp <- function(x, lo = 0, hi = 1) pmin(pmax(x, lo), hi)",
             "scale01 <- function(x) (x - min(x)) / diff(range(x))",
             "tally <- function(x, by) {",
             "  if (!missing(by)) lifecycle::deprecate_stop(\"1.6.0\", \"tally(by)\")",
             "  for (i in seq_along(x)) if (is.na(x[[i]])) x[[i]] <- 0",
             "  length(x)", "}"),
    test = "test-rgpkg.R", data = "x2.rda"))

.RG_VIGNETTE <- c(
  "---", "title: \"Introduction to rgpkg\"", "author: \"Ada Lovelace\"",
  "output: BiocStyle::html_document", "vignette: >",
  "  %\\VignetteIndexEntry{Introduction}", "  %\\VignetteEngine{knitr::rmarkdown}",
  "  %\\VignetteEncoding{UTF-8}", "---", "", "```{r}", "library(rgpkg)", "clamp(2)", "```")

# Commit one release of rgpkg on devel in `dir` and cut its branch there, as
# Bioconductor does; NEWS.md keeps every release so far.
.rg_release <- function(dir, ver) {
  rel <- .RG_RELEASES[[ver]]
  writeLines(c("Package: rgpkg", paste("Version:", rel$version), "Title: Rows Golden Fixture",
               "Description: A package whose rows the golden test holds.", rel$desc,
               "License: Artistic-2.0", "Encoding: UTF-8", "LazyData: true",
               "biocViews: Software"),
             file.path(dir, "DESCRIPTION"))
  writeLines(c(paste0("export(", rel$exports, ")"), "importFrom(stats, median)"),
             file.path(dir, "NAMESPACE"))
  writeLines(rel$code, file.path(dir, "R", "code.R"))
  unlink(file.path(dir, "man"), recursive = TRUE)
  dir.create(file.path(dir, "man"))
  for (e in rel$exports) {
    writeLines(c(sprintf("\\name{%s}", e), sprintf("\\alias{%s}", e), sprintf("\\title{%s}", e),
                 sprintf("\\usage{%s(x)}", e), "\\arguments{\\item{x}{A number.}}",
                 "\\value{A number.}", sprintf("\\examples{%s(1)}", e)),
               file.path(dir, "man", paste0(e, ".Rd")))
  }
  tests <- file.path(dir, "tests", "testthat")
  unlink(list.files(tests, full.names = TRUE))
  writeLines(c("test_that(\"clamp holds x to [0, 1]\", {", "  expect_equal(clamp(2), 1)", "})"),
             file.path(tests, rel$test))
  if (ver != "1.0") writeLines(.RG_VIGNETTE, file.path(dir, "vignettes", "intro.Rmd"))
  if (!file.copy(test_path("fixtures", "rows-golden", rel$data), file.path(dir, "data", "x.rda"),
                 overwrite = TRUE)) {
    stop("rows golden fixture: cannot copy ", rel$data, call. = FALSE)
  }
  news <- file.path(dir, "NEWS.md")
  writeLines(c(sprintf("# rgpkg %s", rel$version), "",
               sprintf("* Exports %s.", toString(rel$exports)), "",
               if (file.exists(news)) readLines(news)), news)
  .rg_git(dir, rel$date, "add", "-A")
  .rg_git(dir, rel$date, "commit", "-q", "-m", shQuote(paste("release", ver)))
  .rg_git(dir, rel$date, "branch", paste0("RELEASE_", sub(".", "_", ver, fixed = TRUE)))
  invisible(dir)
}

.rg_source <- function(root) {
  dir <- file.path(root, "rgpkg")
  for (d in c("R", "vignettes", "data", file.path("tests", "testthat"))) {
    dir.create(file.path(dir, d), recursive = TRUE)
  }
  writeLines(c("library(testthat)", "library(rgpkg)", "test_check(\"rgpkg\")"),
             file.path(dir, "tests", "testthat.R"))
  date <- .RG_RELEASES[[1L]]$date
  .rg_git(dir, date, "init", "-q")
  .rg_git(dir, date, "checkout", "-q", "-b", "devel")
  for (v in names(.RG_RELEASES)) .rg_release(dir, v)
  dir
}

# A walk with no global or system git config either, since churn reads git's diff settings.
.rg_walk <- function(src, prior = NULL) {
  withr::local_envvar(GIT_CONFIG_GLOBAL = "/dev/null", GIT_CONFIG_NOSYSTEM = "1")
  analyze_package(src, "rgpkg", prior = prior)
}

.rg_analyze <- function() {
  root <- withr::local_tempdir()
  .rg_walk(.rg_source(root))
}

# Columns a read stamps with its own time, build or commit, or that the write fills later.
.RG_VOLATILE <- c("read_at", "assessed_at", "assessed_with", "analyzer_version", "commit_sha",
                  "tree_sha", "prev_commit", "rows_generation", "content_id")

# Every frame a release's read produces, less what a queued package recomputes
# whole (the cross-version columns) and the volatile columns. Churn stays, since
# each release's churn is its own diff from the release before.
.rg_frozen <- function(res) {
  frames <- c(res[c("summary", "churn", "api", "functions", "edges", "datasets")],
              stats::setNames(res$text, paste0("text_", names(res$text))),
              list(state = res$state))
  frames$summary <- frames$summary[, setdiff(names(frames$summary), .CROSS_VERSION_COLS),
                                   drop = FALSE]
  lapply(frames, function(df) df[, setdiff(names(df), .RG_VOLATILE), drop = FALSE])
}

# Radix sorts, so the order is the C locale's on every machine.
.rg_digest <- function(res) {
  frames <- lapply(.rg_frozen(res), function(df) {
    df <- df[, sort(names(df), method = "radix"), drop = FALSE]
    for (cn in names(df)) if (is.double(df[[cn]])) df[[cn]] <- signif(df[[cn]], 9)
    df <- df[do.call(order, c(unname(as.list(df)), method = "radix")), , drop = FALSE]
    rownames(df) <- NULL
    df
  })
  json <- jsonlite::toJSON(frames, digits = NA, na = "null")
  digest::digest(as.character(json), algo = "sha256", serialize = FALSE)
}

test_that("the fixture is built the same whatever git config is around, and stops on a git failure", {
  head_of <- function(src) {
    .rg_git(src, "2026-01-01T00:00:00Z", "rev-parse", shQuote("RELEASE_1_3^{commit}"))
  }
  plain <- head_of(.rg_source(withr::local_tempdir()))
  # A commit encoding changes every commit id; a failing signer fails every commit.
  cfg <- withr::local_tempfile(lines = c("[i18n]", "  commitEncoding = ISO-8859-1"))
  withr::local_envvar(GIT_CONFIG_GLOBAL = cfg)
  expect_identical(head_of(.rg_source(withr::local_tempdir())), plain)
  writeLines(c("[commit]", "  gpgsign = true", "[gpg]", "  program = false"), cfg)
  expect_identical(head_of(.rg_source(withr::local_tempdir())), plain)
  expect_error(.rg_git(file.path(withr::local_tempdir(), "none"), "2026-01-01T00:00:00Z", "status"),
               "rows golden fixture: git status exited with status")
})

test_that("the walk reads the fixture the same whatever git config is around", {
  .local_global("rpkg_analyzer_bin", function() "")
  src <- .rg_source(withr::local_tempdir())
  plain <- .rg_digest(.rg_walk(src))
  cfg <- withr::local_tempfile(lines = c("[diff]", "  renames = false"))
  withr::local_envvar(GIT_CONFIG_GLOBAL = cfg)
  # The config reaches churn when nothing keeps it out: it splits 1.3's rename in two.
  no_config <- withr::with_envvar(c(GIT_CONFIG_GLOBAL = "/dev/null"), package_churn(src))
  expect_false(identical(package_churn(src), no_config))
  expect_identical(.rg_digest(.rg_walk(src)), plain)
})

test_that("the golden names this generation and two digests", {
  g <- .rg_golden()
  expect_identical(as.integer(g$rows_generation), ROWS_GENERATION,
    info = "ROWS_GENERATION changed: regenerate tests/testthat/fixtures/rows-golden.json in the same pull request")
  expect_match(g$fallback, "^[0-9a-f]{64}$")
  expect_true(is.null(g$analyzer) || grepl("^[0-9a-f]{64}$", g$analyzer))
})

test_that("the rows R writes for a release match the golden on the fallback path", {
  .local_global("rpkg_analyzer_bin", function() "")
  got <- .rg_digest(.rg_analyze())
  g <- .rg_golden()
  if (!identical(got, g$fallback)) cat("fallback digest:", got, "\n")
  .rg_check_fallback(g, got)
})

test_that("the rows R writes for a release match the golden under the pinned analyzer", {
  skip_if_not(nzchar(rpkg_analyzer_bin()), "needs rpkg-analyzer")
  got <- .rg_digest(.rg_analyze())
  cat("analyzer digest:", got, "\n")
  .rg_check_analyzer(.rg_golden(), got, .analyzer_output_class(rpkg_analyzer_version()))
})

test_that("with ROWS_GOLDEN_ADVISORY=1 a golden that no longer holds skips with its note", {
  .local_global("rpkg_analyzer_bin", function() "")
  g <- .rg_golden()
  got <- .rg_digest(.rg_analyze())
  cls <- as.character(g$output_class)
  stale <- g
  stale$fallback <- strrep("0", 64)
  with_analyzer <- g
  with_analyzer$analyzer <- strrep("0", 64)
  withr::local_envvar(ROWS_GOLDEN_ADVISORY = "1")
  expect_condition(.rg_check_fallback(stale, got),
                   "enforced on pull requests.*no ROWS_GENERATION bump", class = "skip")
  expect_success(.rg_check_fallback(g, got))
  expect_condition(.rg_check_analyzer(g, got, "9.9.9"), "output class changed", class = "skip")
  g$analyzer <- NULL
  expect_condition(.rg_check_analyzer(g, got, cls), "no analyzer digest recorded", class = "skip")
  expect_condition(.rg_check_analyzer(with_analyzer, got, cls), "analyzer digest .* is not",
                   class = "skip")
  withr::local_envvar(ROWS_GOLDEN_ADVISORY = NA)
  expect_failure(.rg_check_fallback(stale, got), "bump ROWS_GENERATION")
  # A moved analyzer digest always needs the bump.
  expect_identical(.rg_fix(g, "analyzer"),
                   "rows changed: bump ROWS_GENERATION in scripts/config.R and regenerate the golden")
})

# The prior .prior_state() gives after `walk`, its rows in a stand-in output class.
.rg_prior <- function(walk, class = "rg-test") {
  s <- walk$summary
  list(
    stored = data.frame(version = s$version, analyzer_version = class, n_fns_r_null = FALSE,
                        detail_scanned = as.integer(s$detail_scanned), datasets_scanned = 1L,
                        latest = !is.na(s$latest_release_date), released = s$released,
                        dep_list = s$dep_list, authors = s$authors, stringsAsFactors = FALSE),
    api = walk$api[, c("version", "exports_added", "exports_removed", "n_exports")],
    state = walk$state[, setdiff(.VERSION_STATE_COLS, "package")],
    text_ok = s$version, class = class)
}

# The cross-version columns of `versions` in a summary or derived frame.
.rg_xv <- function(df, versions, cols = .CROSS_VERSION_COLS) {
  out <- df[match(versions, df$version), cols, drop = FALSE]
  rownames(out) <- NULL
  out
}

test_that("a skip on the fallback path carries the skipped releases' inputs into the package columns", {
  .local_global("rpkg_analyzer_bin", function() "")
  root <- withr::local_tempdir()
  src <- .rg_source(root)
  full <- .rg_walk(src)
  # The fixture moves every input the package columns read.
  last <- full$summary[nrow(full$summary), ]
  expect_true(last$dependency_drift > 0L && last$authors_added_later > 0L &&
                last$cold_removal_rate > 0 && last$cold_removal_rate < 1 &&
                last$deprecation_infrastructure_maturity == 2L)
  date <- .RG_RELEASES[["1.3"]]$date

  # Stored through 1.2: 1.0 to 1.2 are skipped and 1.3 is read.
  to12 <- file.path(root, "to12", "rgpkg")
  .rg_git(root, date, "clone", "-q", shQuote(src), shQuote(to12))
  .rg_git(to12, date, "branch", "-q", "-d", "-r", "origin/RELEASE_1_3")
  res <- .rg_walk(src, prior = .rg_prior(.rg_walk(to12)))
  expect_identical(res$skip$plan$read, c(FALSE, FALSE, FALSE, TRUE))
  expect_identical(.rg_xv(res$summary, "1.3"), .rg_xv(full$summary, "1.3"))
  old <- c("1.0", "1.1", "1.2")
  expect_identical(.rg_xv(res$skip$derived, old), .rg_xv(full$summary, old))

  # Stored whole with 1.0 unrecorded: 1.0 is read and the newest is skipped. The
  # stand-in prior marks the newest scanned for datasets, which the fallback never does.
  pr <- .rg_prior(full)
  pr$state <- pr$state[pr$state$version != "1.0", ]
  res <- .rg_walk(src, prior = pr)
  expect_identical(res$skip$plan$read, c(TRUE, FALSE, FALSE, FALSE))
  later <- c("1.1", "1.2", "1.3")
  cols <- setdiff(.CROSS_VERSION_COLS, "datasets_scanned")
  expect_identical(.rg_xv(res$skip$derived, later, cols), .rg_xv(full$summary, later, cols))
})

test_that(".CROSS_VERSION_COLS names every column the cross-version pass adds but its stamps", {
  base <- data.frame(package = "p", version = c("1.0", "1.1"), released = "2026-01-01",
                     dep_list = "[]", authors = "[]", stringsAsFactors = FALSE)
  for (df in list(base, base[0L, ])) {
    added <- setdiff(names(add_cross_version_metrics(df, NULL, list())), names(df))
    expect_setequal(setdiff(added, c("assessed_at", "assessed_with")), .CROSS_VERSION_COLS)
  }
})

test_that("the cross-version columns need only the inputs a skipped release carries", {
  .local_global("rpkg_analyzer_bin", function() "")
  res <- .rg_analyze()
  series <- lapply(seq_len(nrow(res$state)), function(i) list(
    symbols = .xv_json_to_chr(res$state$deprecated[[i]]),
    uses_lifecycle = identical(res$state$uses_lifecycle[[i]], 1L)))
  base <- res$summary[, setdiff(names(res$summary), .CROSS_VERSION_COLS), drop = FALSE]
  full <- add_cross_version_metrics(base, res$api, series)
  cut  <- add_cross_version_metrics(base[, .CROSS_VERSION_INPUTS, drop = FALSE], res$api, series)
  expect_identical(cut[, .CROSS_VERSION_COLS], full[, .CROSS_VERSION_COLS],
    info = paste("a cross-version column reads a summary column a skipped release does not",
                 "carry: add it to .CROSS_VERSION_INPUTS in scripts/analyze.R and to the",
                 "summary columns .prior_state() reads in scripts/update.R"))
})
