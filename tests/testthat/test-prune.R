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
