# scripts/config.R: pipeline-wide constants and base helpers.
# Source this first; all other scripts assume these are defined.

BIOC_GIT_BASE <- "https://github.com/bioc"
PUBLISH_REPO  <- "r-observatory/bioc-code-metrics"
DB_FILENAME   <- "bioc-code-metrics.db"
DATA_DB_FILENAME <- "bioc-data-metrics.db"
# Backoff between attempts at the bioconductor.org release lookup, in seconds;
# one more attempt is made than there are waits. The first wait is small because
# most failures are a one-off blip; the tail covers a real outage, of the kind
# that served 504 for at least eight minutes on 2026-07-26.
RELEASE_RETRY_WAITS_S <- c(5, 15, 30, 60, 120, 300, 600)

SHARD_SIZE         <- 100L
MAX_CLONE_FAILURES <- 5L
WORK_DIR           <- "work"

# How many times a package may be handed to the analyzer without being read
# before the backfill queues stop asking for it. The same shape as
# MAX_CLONE_FAILURES, for the same reason: a package with no way of leaving a
# queue keeps the pipeline reporting a change forever and publishing a dated
# release for a database that has not moved.
#
# The queues it governs are the ones only the analyzer can satisfy. n_fns_r and
# the dataset rows come from the binary and from nowhere else, so a package the
# pure-R fallback analysed carries neither, and both queues hand it straight
# back. Nothing about the package changes between one such run and the next.
#
# Lower than the clone cap because the two failures are not alike. A clone
# fails on the network, so the next attempt is a genuinely different one and
# five of them are worth making. A read fails on what the package contains, and
# one build's answer is the same every time it is asked: the second attempt is
# there for a run that failed for a reason other than the package, a killed
# worker or a timeout, and a third would only collect the same answer again.
#
# Not a permanent verdict. The record carries the build that could not read the
# package, and a later build clears it, so the retirement lasts exactly as long
# as the reader it was measured against.
MAX_ANALYZER_READ_ATTEMPTS <- 2L

SUMMARY_TABLE <- "bioc_code_summary"
CHURN_TABLE   <- "bioc_code_churn"
API_TABLE     <- "bioc_api_history"

# Per-git-subprocess timeout in seconds. A hard cap so a pathological repo
# cannot stall a parallel shard. Overridable via GIT_TIMEOUT env var.
GIT_TIMEOUT <- as.integer(Sys.getenv("GIT_TIMEOUT", unset = "300"))

# Number of parallel workers for the per-package clone+analyze step.
# Default: all logical cores (overridable via ANALYSIS_CORES env var).
ANALYSIS_CORES <- {
  dc <- suppressWarnings(parallel::detectCores(logical = TRUE))
  max(1L, as.integer(Sys.getenv("ANALYSIS_CORES",
    unset = as.character(if (is.na(dc)) 1L else dc))))
}

# Per-package analysis timeout in seconds. A hard cap so a pathological
# file in a metric group (e.g. a catastrophic regex) cannot stall a shard.
# Overridable via WORKER_TIMEOUT env var.
WORKER_TIMEOUT <- as.integer(Sys.getenv("WORKER_TIMEOUT", unset = "600"))

#' Null/empty coalescing operator.
#' Returns b when a is NULL, length-0, or a scalar NA.
`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0L || (length(a) == 1L && is.na(a))) b else a
}
