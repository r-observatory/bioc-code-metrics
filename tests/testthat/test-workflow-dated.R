# tests/testthat/test-workflow-dated.R
test_that("update.yml publishes dated code and data releases, not rolling current", {
  # test_dir() sources this file with the working directory set to
  # tests/testthat/, so reach the repo root the same way other fixtures do.
  workflow_path <- file.path("..", "..", ".github", "workflows", "update.yml")
  yml <- paste(readLines(workflow_path), collapse = "\n")
  expect_true(grepl("code-\\$\\(date", yml) || grepl('code-', yml, fixed = TRUE))
  expect_true(grepl("data-", yml, fixed = TRUE))
  expect_true(grepl("bioc-data-metrics.db", yml, fixed = TRUE))
  # Prior-day immutability: no unconditional clobber of a non-today tag.
  expect_true(grepl("prune.R", yml, fixed = TRUE))
  expect_true(grepl("render_notes.R", yml, fixed = TRUE))
})

# The steps of a workflow, one element per step; element 1 is what comes before
# the first step.
.workflow_steps <- function(name) {
  yml <- readLines(file.path("..", "..", ".github", "workflows", name))
  split(yml, findInterval(seq_along(yml), grep("^      - ", yml)))
}

test_that("PIPELINE_RUN_ID is set in the shard step's env and in no other step", {
  # Actions sets GITHUB_RUN_ID in every step, the unit tests included, so the
  # run id the pipeline reads must reach the shard step alone.
  hits <- Filter(function(s) any(grepl("PIPELINE_RUN_ID", s, fixed = TRUE)),
                 c(.workflow_steps("update.yml"), .workflow_steps("test.yml")))
  expect_length(hits, 1L)
  step <- unname(unlist(hits))
  expect_true(any(grepl("name: Analyze shards and publish after each", step, fixed = TRUE)))
  expect_identical(grep("PIPELINE_RUN_ID", step, value = TRUE, fixed = TRUE),
                   "          PIPELINE_RUN_ID: ${{ github.run_id }}")
})
