# tests/testthat/helper-config-defaults.R: scripts/config.R as a run would read
# it under a given environment.

# Source config.R into an environment of its own. vars are set for the read, and
# NA unsets one. WORKER_TIMEOUT, ANALYSIS_CORES and ANALYZER_MEMORY_LIMIT_MB are
# unset unless named.
.config_under <- function(vars = character(0L)) {
  set <- c(WORKER_TIMEOUT = NA, ANALYSIS_CORES = NA, ANALYZER_MEMORY_LIMIT_MB = NA)
  set[names(vars)] <- vars
  env <- new.env(parent = globalenv())
  withr::with_envvar(set, source(test_path("..", "..", "scripts", "config.R"), local = env))
  env
}
