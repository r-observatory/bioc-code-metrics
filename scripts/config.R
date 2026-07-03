# scripts/config.R: pipeline-wide constants and base helpers.
# Source this first; all other scripts assume these are defined.

BIOC_GIT_BASE <- "https://github.com/bioc"
PUBLISH_REPO  <- "r-observatory/bioc-code-metrics"
DB_FILENAME   <- "bioc-code-metrics.db"
SHARD_SIZE         <- 100L
MAX_CLONE_FAILURES <- 5L
WORK_DIR           <- "work"

SUMMARY_TABLE <- "bioc_code_summary"
CHURN_TABLE   <- "bioc_code_churn"
API_TABLE     <- "bioc_api_history"

# Number of parallel workers for the per-package clone+analyze step.
# Default: all logical cores (overridable via ANALYSIS_CORES env var).
ANALYSIS_CORES <- {
  dc <- suppressWarnings(parallel::detectCores(logical = TRUE))
  max(1L, as.integer(Sys.getenv("ANALYSIS_CORES",
    unset = as.character(if (is.na(dc)) 1L else dc))))
}

#' Null/empty coalescing operator.
#' Returns b when a is NULL, length-0, or a scalar NA.
`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0L || (length(a) == 1L && is.na(a))) b else a
}
