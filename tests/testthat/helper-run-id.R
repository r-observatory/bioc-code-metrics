# tests/testthat/helper-run-id.R: no test sees a run id unless it sets one, so
# a shell that exports PIPELINE_RUN_ID cannot change what the suite finds.
withr::local_envvar(c(PIPELINE_RUN_ID = NA), .local_envir = testthat::teardown_env())
