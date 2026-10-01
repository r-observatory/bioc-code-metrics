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

test_that("the shard loop stops where shard_loop_done says, and nowhere else", {
  yml <- readLines(file.path("..", "..", ".github", "workflows", "update.yml"))
  expect_true(any(grepl("if shard_loop_done out/run-status.json; then", yml, fixed = TRUE)))
  expect_false(any(grepl('[ "$COMPLETE" = "true" ] || [ "$CHANGED" != "true" ]', yml,
                         fixed = TRUE)))
  expect_true(any(grepl("Time budget reached", yml, fixed = TRUE)))
  sh <- readLines(file.path("..", "..", "scripts", "publish.sh"))
  expect_length(grep("^shard_loop_done\\(\\) \\{", sh), 1L)
})

test_that("unpark and requeue reach the first shard only, through env", {
  yml <- readLines(file.path("..", "..", ".github", "workflows", "update.yml"))
  expect_true(any(grepl("^      unpark:$", yml)))
  expect_true(any(grepl("^      requeue:$", yml)))
  # Read through env, never pasted into the script, so their text cannot run.
  uses <- trimws(grep("inputs\\.(unpark|requeue)", yml, value = TRUE))
  expect_setequal(uses, c("UNPARK: ${{ inputs.unpark }}", "REQUEUE: ${{ inputs.requeue }}"))
  expect_true(any(grepl(
    'Rscript scripts/update.R out/ ${FORCE} ${RECOLLECT} ${RELEASE[@]+"${RELEASE[@]}"}',
    yml, fixed = TRUE)))
  expect_true(any(grepl("^            RELEASE=\\(\\)$", yml)))
})

test_that("the shard step leaves a summary of the run on the Actions page", {
  yml <- readLines(file.path("..", "..", ".github", "workflows", "update.yml"))
  expect_true(any(grepl(
    'write_step_summary out/run-status.json "$SECONDS" "${START_QUEUE:-0}" >> "${GITHUB_STEP_SUMMARY:-/dev/null}"',
    yml, fixed = TRUE)))
})

test_that("the run budget defaults to 16,800 s for a dispatch and for the schedule", {
  yml <- readLines(file.path("..", "..", ".github", "workflows", "update.yml"))
  at <- grep("^      time_budget_seconds:$", yml)
  expect_length(at, 1L)
  expect_identical(yml[at + 1:2], c(
    '        description: "Stop starting new shards after this many seconds (default 16800 = 4h 40m)."',
    '        default: "16800"'))
  expect_true(any(yml == "          BUDGET=\"${{ inputs.time_budget_seconds || '16800' }}\""))
  expect_false(any(grepl("18000", yml, fixed = TRUE)))
})

test_that("the job outlasts the budget by more than one package's time limit", {
  yml <- readLines(file.path("..", "..", ".github", "workflows", "update.yml"))
  update_job <- yml[seq(grep("^  update:$", yml), grep("^  keepalive:$", yml) - 1L)]
  limit_s <- 60L * as.integer(sub("^    timeout-minutes: ", "",
                                  grep("^    timeout-minutes: [0-9]+$", update_job, value = TRUE)))
  expect_identical(limit_s, 21000L)
  expect_gt(limit_s - 16800L, .config_under()$WORKER_TIMEOUT)
})

test_that("the core count input reaches the shards through set_analysis_cores alone", {
  yml <- readLines(file.path("..", "..", ".github", "workflows", "update.yml"))
  expect_true(any(grepl("^      analysis_cores:$", yml)))
  uses <- trimws(grep("inputs.analysis_cores", yml, value = TRUE, fixed = TRUE))
  expect_identical(uses, "CORES_INPUT: ${{ inputs.analysis_cores }}")
  # Never a key of an env block, where an empty input would set it to "".
  expect_false(any(grepl("^\\s*ANALYSIS_CORES\\s*:", yml)))
  set  <- which(yml == '          set_analysis_cores "${CORES_INPUT:-}" || exit 1')
  loop <- which(yml == "          while :; do")
  expect_length(set, 1L)
  expect_length(loop, 1L)
  expect_lt(set, loop)
  src <- which(yml == "          source scripts/publish.sh")
  expect_gt(set, max(src[src < loop]))
})

test_that("each shard prints free memory before it starts", {
  yml <- readLines(file.path("..", "..", ".github", "workflows", "update.yml"))
  banner <- which(yml == '            echo "=== shard ${shard} (elapsed ${SECONDS}s) ==="')
  expect_length(banner, 1L)
  expect_identical(yml[banner + 1L], "            free -m || true")
  expect_true(startsWith(yml[banner + 2L], "            Rscript scripts/update.R out/ "))
})
