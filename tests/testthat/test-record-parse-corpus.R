# The record parse and the memo against the per-line parser over whole corpora of analyzer
# output named by RPA_PARSE_CORPUS_050 and RPA_PARSE_CORPUS_040 (skipped when unset); files
# under mv/<package>/ share one memo, others a memo each; RPA_PARSE_CORPUS_REPORT gets a line per corpus.

# A parse's value, or the class and message of the error it raised.
.parse_outcome <- function(f) {
  tryCatch(f(), error = function(e) list(class = class(e), message = conditionMessage(e)))
}

.check_parse_corpus <- function(root) {
  files <- sort(list.files(root, pattern = "\\.ndjson(\\.gz)?$", recursive = TRUE),
                method = "radix")
  group <- vapply(strsplit(files, "/", fixed = TRUE), function(p) {
    if (length(p) >= 3L && identical(p[[1L]], "mv")) p[[2L]] else NA_character_
  }, character(1L))
  memos  <- list()
  same   <- 0L
  failed <- character(0L)
  for (i in seq_along(files)) {
    g <- group[[i]]
    if (is.na(g)) {
      memo <- .record_memo()
    } else {
      if (is.null(memos[[g]])) memos[[g]] <- .record_memo()
      memo <- memos[[g]]
    }
    lines <- readLines(file.path(root, files[[i]]), warn = FALSE)
    old <- .parse_outcome(function() .parse_records_per_line(lines))
    new <- .parse_outcome(function() parse_analyzer_records(lines, memo))
    if (identical(old, new)) same <- same + 1L else failed <- c(failed, files[[i]])
  }
  list(n = length(files), same = same, failed = failed)
}

for (label in c("v050", "v040")) {
  var <- c(v050 = "RPA_PARSE_CORPUS_050", v040 = "RPA_PARSE_CORPUS_040")[[label]]
  test_that(sprintf("the record parse over the %s corpus equals the per-line parser", label), {
    root <- Sys.getenv(var, unset = "")
    skip_if(!nzchar(root), paste(var, "is not set"))
    got <- .check_parse_corpus(root)
    report <- Sys.getenv("RPA_PARSE_CORPUS_REPORT", unset = "")
    if (nzchar(report)) {
      cat(sprintf("%s\t%d\t%d\n", label, got$n, got$same), file = report, append = TRUE)
    }
    expect_gt(got$n, 0L)
    expect_identical(got$same, got$n, info = paste(head(got$failed, 20L), collapse = "\n"))
  })
}
