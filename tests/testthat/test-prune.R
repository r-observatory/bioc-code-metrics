test_that("releases_to_prune keeps newest N and every first-of-month", {
  days <- sprintf("code-2026-06-%02d", 1:30)          # 30 dailies in June
  extra <- c("code-2026-05-01", "code-2026-05-15", "code-2026-04-01")
  tags <- c(days, extra)
  del <- releases_to_prune(tags, keep = 30L)
  # Newest 30 (all of June) are kept.
  expect_false(any(grepl("2026-06", del)))
  # First-of-month always kept.
  expect_false("code-2026-05-01" %in% del)
  expect_false("code-2026-04-01" %in% del)
  # A non-first-of-month older daily is pruned.
  expect_true("code-2026-05-15" %in% del)
})

test_that("nothing is pruned when under the keep threshold", {
  expect_identical(releases_to_prune(sprintf("code-2026-06-%02d", 1:10), keep = 30L),
                   character(0L))
})

test_that("KEEP all selects nothing among 400 tags", {
  tags <- format(as.Date("2025-01-01") + 0:399, "metrics-%Y-%m-%d")
  expect_identical(releases_to_prune(tags, keep = parse_keep("all")), character(0L))
  expect_identical(releases_to_prune(tags, keep = parse_keep(" ALL ")), character(0L))
})

test_that("a numeric KEEP is unchanged", {
  expect_identical(parse_keep("30"), 30L)
  expect_identical(parse_keep("5"), 5L)
  tags <- format(as.Date("2025-01-01") + 0:399, "metrics-%Y-%m-%d")
  expect_gt(length(releases_to_prune(tags, keep = parse_keep("30"))), 0L)
})

test_that("a KEEP that is neither a number nor all is an error", {
  expect_error(parse_keep("lots"), "KEEP")
})

test_that("the prune step keeps everything but still clears drafts and swap assets", {
  yml <- paste(readLines(file.path("..", "..", ".github", "workflows", "update.yml")),
               collapse = "\n")
  step <- sub("(?s).*- name: Prune old dated releases", "", yml, perl = TRUE)
  step <- sub("(?s)\n  keepalive:.*", "", step, perl = TRUE)
  expect_true(grepl('KEEP: "all"', step, fixed = TRUE))
  expect_true(grepl("delete_stale_drafts metrics", step, fixed = TRUE))
  expect_true(grepl("sweep_swap_leftovers metrics", step, fixed = TRUE))
})
