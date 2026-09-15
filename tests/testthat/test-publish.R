# tests/testthat/test-publish.R: resolving and publishing the dated releases,
# against a fake gh (fixtures/gh-release/gh) so no test touches GitHub.
#
# The sibling cran-code-metrics pipeline was stuck from 2026-09-13 by a draft
# release. `gh release create TAG <assets> --latest` creates a draft, uploads
# into it and then publishes it; both database uploads got HTTP 500, gh's own
# cleanup of the draft got a 500 too, and the draft was left holding only the
# two manifests. `gh release list` hands drafts to a token that can push, sorted
# in with the real releases, so every run after that built on the draft. This
# pipeline carried the same code, and here a draft holding no assets at all
# would have read as a cold start and published one shard as latest.
#
# Every case drives scripts/publish.sh the way update.yml does, under
# `set -euo pipefail` with the helper called as `f || exit 1`, because that call
# form is what turns off errexit inside the function and lets a failed upload
# look like success.

.pub_script <- function() {
  normalizePath(file.path("..", "..", "scripts", "publish.sh"), mustWork = FALSE)
}

.pub_asset <- function(name, size) list(name = name, size = size, state = "uploaded")

# Yesterday's release, published, Latest and complete.
.pub_prior <- function() list(
  id = 1L, tagName = "metrics-2026-09-12", isDraft = FALSE, isLatest = TRUE,
  name = "Bioconductor Metrics - 2026-09-12",
  assets = list(.pub_asset("bioc-code-metrics.db", 4000L),
                .pub_asset("bioc-data-metrics.db", 2000L),
                .pub_asset("code-manifest.json", 8L),
                .pub_asset("data-manifest.json", 9L)))

# What a failed `gh release create` left in cran-code-metrics: a draft under
# the next day's tag carrying the two manifests and neither database.
.pub_stranded <- function(tag = "metrics-2026-09-13", assets = list(
  .pub_asset("code-manifest.json", 897L), .pub_asset("data-manifest.json", 1120L))) {
  list(id = 2L, tagName = tag, isDraft = TRUE, isLatest = FALSE, name = "", assets = assets)
}

# A scratch directory holding the fake gh, its state, its call log, the
# failure counters and the four assets a publish uploads, each a distinct size
# so a mix-up between them shows in the sizes.
.pub_world <- function(releases, env = parent.frame()) {
  skip_on_os("windows")
  skip_if(!nzchar(Sys.which("bash")), "bash is needed to run scripts/publish.sh")
  skip_if(!nzchar(Sys.which("jq")), "jq is needed by the fake gh")

  dir <- withr::local_tempdir(.local_envir = env)
  w <- list(dir = dir, bin = file.path(dir, "bin"), fails = file.path(dir, "fails"),
            state = file.path(dir, "state.json"), log = file.path(dir, "gh.log"))
  dir.create(w$bin)
  dir.create(w$fails)
  dir.create(file.path(dir, "out"))
  file.copy(test_path("fixtures", "gh-release", "gh"), file.path(w$bin, "gh"))
  Sys.chmod(file.path(w$bin, "gh"), "755")
  file.create(w$log)

  sizes <- c("bioc-code-metrics.db" = 5000L, "bioc-data-metrics.db" = 3000L,
             "code-manifest.json" = 120L, "data-manifest.json" = 140L)
  for (f in names(sizes)) writeBin(as.raw(rep(65L, sizes[[f]])), file.path(dir, "out", f))
  writeLines("notes", file.path(dir, "out", "release-notes-code.md"))

  jsonlite::write_json(releases, w$state, auto_unbox = TRUE)
  w
}

.pub_fail <- function(w, what, times) writeLines(as.character(times), file.path(w$fails, what))

# Run shell lines against the world with scripts/publish.sh sourced, in the
# same shell options as the workflow's steps.
.pub_sh <- function(w, lines, wait = "0") {
  script <- file.path(w$dir, "run.sh")
  # The first check makes sure no case can reach the real gh, whose release
  # commands would act on whatever repository and credentials it finds.
  writeLines(c("set -euo pipefail",
               sprintf('[ "$(command -v gh)" = %s ] || { echo "the fake gh is not first on PATH" >&2; exit 97; }',
                       shQuote(file.path(w$bin, "gh"))),
               sprintf("source %s", shQuote(.pub_script())),
               sprintf("cd %s", shQuote(w$dir)), lines), script)
  # wait = NA leaves the retry wait unset, so the script's own default applies.
  withr::local_envvar(c(PATH = paste(w$bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
                        GH_STATE = w$state, GH_LOG = w$log, GH_FAILS = w$fails,
                        PUBLISH_RETRY_WAIT_S = wait))
  out <- suppressWarnings(system2("bash", shQuote(script), stdout = TRUE, stderr = TRUE))
  list(status = attr(out, "status") %||% 0L, output = paste(out, collapse = "\n"))
}

.PUB_ASSETS <- "out/bioc-code-metrics.db out/bioc-data-metrics.db out/code-manifest.json out/data-manifest.json"

.pub_publish <- function(w, tag = "metrics-2026-09-13", assets = .PUB_ASSETS) {
  .pub_sh(w, sprintf(
    "publish_release %s 'Bioconductor Metrics - 2026-09-13' out/release-notes-code.md %s || exit 1",
    tag, assets))
}

.pub_latest <- function(w, series = "metrics") {
  r <- .pub_sh(w, sprintf("latest_tag %s", series))
  expect_identical(r$status, 0L)
  r$output
}

.pub_state <- function(w) jsonlite::fromJSON(w$state, simplifyVector = FALSE)

.pub_releases <- function(w, tag) Filter(function(r) identical(r$tagName, tag), .pub_state(w))

.pub_calls <- function(w) readLines(w$log)

.pub_uploads <- function(w) {
  up <- grep("^gh release upload ", .pub_calls(w), value = TRUE)
  basename(sub(" --clobber$", "", sub("^gh release upload \\S+ ", "", up)))
}

# Today's release as a published, Latest release carrying exactly the four
# assets at the sizes on disk.
.pub_expect_published <- function(w, tag = "metrics-2026-09-13") {
  rel <- .pub_releases(w, tag)
  expect_length(rel, 1L)
  rel <- rel[[1L]]
  expect_false(rel$isDraft)
  expect_true(rel$isLatest)
  got <- vapply(rel$assets, function(a) sprintf("%s %d %s", a$name, a$size, a$state), "")
  want <- vapply(c("bioc-code-metrics.db", "bioc-data-metrics.db", "code-manifest.json",
                   "data-manifest.json"),
                 function(f) sprintf("%s %d uploaded", f,
                                     file.size(file.path(w$dir, "out", f))), "")
  expect_setequal(got, unname(want))
}

# ---------------------------------------------------------------------------
# Resolution
# ---------------------------------------------------------------------------

test_that("latest_tag skips a draft that sorts above the published releases", {
  w <- .pub_world(list(.pub_prior(), .pub_stranded()))
  expect_identical(.pub_latest(w), "metrics-2026-09-12")
  expect_true(all(grepl("--exclude-drafts", grep("release list", .pub_calls(w), value = TRUE),
                        fixed = TRUE)))
})

test_that("latest_tag fails when the release list cannot be read", {
  # An empty answer is what a cold start looks like, so a failed listing must
  # stop the step rather than come back empty.
  w <- .pub_world(list(.pub_prior()))
  .pub_fail(w, "list", 99L)
  r <- .pub_sh(w, c('METRICS_TAG=$(latest_tag metrics)', 'echo "resolved <${METRICS_TAG}>"'))
  expect_false(identical(r$status, 0L))
  expect_false(grepl("resolved <", r$output, fixed = TRUE))
  expect_length(grep("^gh release list ", .pub_calls(w)), 5L)
})

test_that("latest_tag reads the release list again when a read fails", {
  # It is the first call of every run, in the download step, so a single
  # GraphQL 500 there stopped a no-op day before the heartbeat, exactly as one
  # on the heartbeat's own listing did. The tag is the function's stdout, which
  # the attempt messages must stay out of.
  w <- .pub_world(list(.pub_prior(), .pub_stranded()))
  .pub_fail(w, "list", 1L)
  r <- .pub_sh(w, c('METRICS_TAG=$(latest_tag metrics)', 'echo "resolved <${METRICS_TAG}>"'))
  expect_identical(r$status, 0L, info = r$output)
  expect_true(grepl("resolved <metrics-2026-09-12>", r$output, fixed = TRUE), info = r$output)
  expect_length(grep("^gh release list ", .pub_calls(w)), 2L)

  .pub_fail(w, "list", 2L)
  slept <- file.path(w$dir, "slept")
  r <- .pub_sh(w, c(sprintf("sleep() { echo \"$1\" >> %s; }", shQuote(slept)),
                    "latest_tag metrics > /dev/null || exit 1"),
               wait = NA)
  expect_identical(r$status, 0L, info = r$output)
  expect_identical(readLines(slept), c("10", "20"))
})

test_that("latest_tag is empty when the series has no published release", {
  w <- .pub_world(list(.pub_stranded()))
  expect_identical(.pub_latest(w), "")
})

# ---------------------------------------------------------------------------
# Publishing
# ---------------------------------------------------------------------------

test_that("a first publish of the day creates an empty draft, fills it, then publishes it", {
  w <- .pub_world(list(.pub_prior()))
  r <- .pub_publish(w)
  expect_identical(r$status, 0L, info = r$output)
  .pub_expect_published(w)
  expect_identical(.pub_latest(w), "metrics-2026-09-13")

  calls <- .pub_calls(w)
  create <- grep("^gh release create ", calls, value = TRUE)
  expect_length(create, 1L)
  expect_true(grepl("--draft", create, fixed = TRUE))
  # The assets go up one at a time, not as arguments to create.
  expect_false(grepl("bioc-code-metrics.db", create, fixed = TRUE))
  expect_length(.pub_uploads(w), 4L)
  expect_true(any(grepl("^gh release edit metrics-2026-09-13 --draft=false --latest", calls)))
  # The prior release is no longer Latest, and it still carries everything.
  prior <- .pub_releases(w, "metrics-2026-09-12")[[1L]]
  expect_false(prior$isLatest)
  expect_length(prior$assets, 4L)
})

test_that("a stranded draft under today's tag is deleted and replaced, not uploaded into", {
  for (assets in list(list(.pub_asset("code-manifest.json", 897L),
                           .pub_asset("data-manifest.json", 1120L)),
                      list())) {
    w <- .pub_world(list(.pub_prior(), .pub_stranded(assets = assets)))
    r <- .pub_publish(w)
    expect_identical(r$status, 0L, info = r$output)
    .pub_expect_published(w)

    calls <- .pub_calls(w)
    del <- grep("^gh release delete ", calls)
    expect_length(del, 1L)
    expect_false(grepl("--cleanup-tag", calls[del], fixed = TRUE))
    # Nothing touched the old draft before it was gone.
    expect_true(del < min(grep("^gh release (create|upload|edit) ", calls)))
    expect_false(any(vapply(.pub_state(w), function(x) isTRUE(x$isDraft), TRUE)))
  }
})

test_that("an upload that fails twice is retried and the release is published", {
  w <- .pub_world(list(.pub_prior()))
  .pub_fail(w, "upload-bioc-data-metrics.db", 2L)
  r <- .pub_publish(w)
  expect_identical(r$status, 0L, info = r$output)
  .pub_expect_published(w)
  expect_identical(sum(.pub_uploads(w) == "bioc-data-metrics.db"), 3L)
})

test_that("an upload that never lands fails the call, and yesterday still resolves", {
  w <- .pub_world(list(.pub_prior()))
  .pub_fail(w, "upload-bioc-code-metrics.db", 99L)
  r <- .pub_publish(w)
  expect_false(identical(r$status, 0L))
  expect_true(grepl("bioc-code-metrics.db", r$output, fixed = TRUE))
  # Five attempts, and nothing after the database that would not land.
  expect_identical(.pub_uploads(w), rep("bioc-code-metrics.db", 5L))
  expect_false(any(grepl("^gh release edit ", .pub_calls(w))))
  expect_true(.pub_releases(w, "metrics-2026-09-13")[[1L]]$isDraft)
  expect_identical(.pub_latest(w), "metrics-2026-09-12")

  # The next attempt finds the draft this one left and gets through.
  file.remove(file.path(w$fails, "upload-bioc-code-metrics.db"))
  r <- .pub_publish(w)
  expect_identical(r$status, 0L, info = r$output)
  .pub_expect_published(w)
  expect_identical(.pub_latest(w), "metrics-2026-09-13")
})

test_that("the retry waits thirty seconds longer after each failed attempt", {
  w <- .pub_world(list(.pub_prior()))
  .pub_fail(w, "upload-bioc-code-metrics.db", 2L)
  slept <- file.path(w$dir, "slept")
  r <- .pub_sh(w, c(sprintf("sleep() { echo \"$1\" >> %s; }", shQuote(slept)),
                    "upload_asset metrics-2026-09-12 out/bioc-code-metrics.db || exit 1"),
               wait = NA)
  expect_identical(r$status, 0L, info = r$output)
  expect_identical(readLines(slept), c("30", "60"))
})

test_that("a failed clobber of today's published release fails the call", {
  # The second shard of a run republishes into the release the first shard
  # published. While the gh calls there were unguarded, a failed upload followed
  # by a successful title edit returned 0 and the step went green.
  w <- .pub_world(list(.pub_prior()))
  expect_identical(.pub_publish(w)$status, 0L)
  file.create(w$log)
  .pub_fail(w, "upload-bioc-code-metrics.db", 99L)

  r <- .pub_publish(w)
  expect_false(identical(r$status, 0L))
  expect_false(any(grepl("^gh release (create|delete|edit) ", .pub_calls(w))))
})

test_that("a release listing that cannot be read stops a republish before anything is created", {
  # An unread listing that came back empty would look like a tag with no
  # release. The next step would create a draft under the tag an earlier shard
  # already published, the uploads and the edit would all land on the published
  # one and the step would go green, and every later publish and heartbeat
  # would then refuse on two releases under one tag.
  w <- .pub_world(list(.pub_prior()))
  expect_identical(.pub_publish(w)$status, 0L)
  file.create(w$log)
  .pub_fail(w, "list", 99L)

  r <- .pub_publish(w)
  expect_false(identical(r$status, 0L))
  expect_false(any(grepl("^gh release (create|delete|upload|edit) ", .pub_calls(w))))
  today <- .pub_releases(w, "metrics-2026-09-13")
  expect_length(today, 1L)
  expect_false(today[[1L]]$isDraft)
})

test_that("a later shard republishes today's release in place", {
  w <- .pub_world(list(.pub_prior()))
  expect_identical(.pub_publish(w)$status, 0L)
  writeBin(as.raw(rep(66L, 6000L)), file.path(w$dir, "out", "bioc-code-metrics.db"))
  file.create(w$log)

  r <- .pub_sh(w, paste(
    "publish_release metrics-2026-09-13 'Bioconductor Metrics - 2026-09-13 (updated 09:00 UTC)'",
    "out/release-notes-code.md", .PUB_ASSETS, "|| exit 1"))
  expect_identical(r$status, 0L, info = r$output)
  .pub_expect_published(w)
  expect_identical(.pub_releases(w, "metrics-2026-09-13")[[1L]]$name,
                   "Bioconductor Metrics - 2026-09-13 (updated 09:00 UTC)")
  expect_false(any(grepl("^gh release (create|delete) ", .pub_calls(w))))
})

test_that("more than one release under today's tag is refused and left alone", {
  published <- .pub_prior()
  published$tagName <- "metrics-2026-09-13"
  w <- .pub_world(list(published, .pub_stranded()))
  r <- .pub_publish(w)
  expect_false(identical(r$status, 0L))
  expect_true(grepl("more than one", r$output, fixed = TRUE))
  expect_false(any(grepl("^gh release (create|delete|upload|edit) ", .pub_calls(w))))
  expect_length(.pub_releases(w, "metrics-2026-09-13"), 2L)
})

# Yesterday's release, today's published release and a draft under today's tag.
.pub_doubled <- function() {
  prior <- .pub_prior()
  prior$isLatest <- FALSE
  today <- .pub_prior()
  today$id <- 2L
  today$tagName <- "metrics-2026-09-13"
  draft <- .pub_stranded()
  draft$id <- 3L
  list(prior, today, draft)
}

test_that("the refusal over two releases under one tag names them by id, not by tag", {
  # `gh release delete TAG` looks the tag up as a published release and as a
  # draft at the same time and deletes whichever answer arrives first, so the
  # advice to run it could take today's published release and keep the draft.
  w <- .pub_world(.pub_doubled())
  r <- .pub_publish(w)
  expect_false(identical(r$status, 0L))
  expect_true(grepl("id 2: published", r$output, fixed = TRUE), info = r$output)
  expect_true(grepl("id 3: draft", r$output, fixed = TRUE), info = r$output)
  expect_false(grepl("id 1:", r$output, fixed = TRUE))
  expect_true(grepl("gh api -X DELETE repos/{owner}/{repo}/releases/<id>", r$output, fixed = TRUE))
  expect_false(grepl("gh release delete", r$output, fixed = TRUE))
  expect_false(any(grepl("^gh release (create|delete|upload|edit) |-X DELETE", .pub_calls(w))))
  expect_length(.pub_state(w), 3L)

  # The ids could not be read: still refused, and it says where to find them.
  .pub_fail(w, "api", 1L)
  r <- .pub_publish(w)
  expect_false(identical(r$status, 0L))
  expect_true(grepl("could not list their ids", r$output, fixed = TRUE), info = r$output)
  expect_length(.pub_state(w), 3L)
})

test_that("databases go up before manifests, whatever order they are passed in", {
  # A manifest must never be newer than the database beside it: preflight reads
  # a database behind its manifest as lost rows.
  w <- .pub_world(list(.pub_prior()))
  r <- .pub_publish(w, assets = paste(
    "out/code-manifest.json out/bioc-code-metrics.db out/data-manifest.json",
    "out/bioc-data-metrics.db"))
  expect_identical(r$status, 0L, info = r$output)
  up <- .pub_uploads(w)
  expect_length(up, 4L)
  expect_true(all(grepl("\\.db$", up[1:2])))
  expect_true(all(grepl("\\.json$", up[3:4])))
})

test_that("an asset that landed at the wrong size is refused before publishing", {
  w <- .pub_world(list(.pub_prior()))
  .pub_fail(w, "short-bioc-code-metrics.db", 1L)
  r <- .pub_publish(w)
  expect_false(identical(r$status, 0L))
  expect_true(grepl("bioc-code-metrics.db 5000", r$output, fixed = TRUE))
  expect_false(any(grepl("--draft=false", .pub_calls(w), fixed = TRUE)))
  expect_true(.pub_releases(w, "metrics-2026-09-13")[[1L]]$isDraft)
  expect_identical(.pub_latest(w), "metrics-2026-09-12")
  # Read five times first, in case the release had not caught up.
  expect_length(grep("^gh release view ", .pub_calls(w)), 5L)
})

test_that("a read-back that has not caught up with the uploads is read again, not refused", {
  # Nothing promises that a release lists an asset the moment its upload
  # returns. Refusing on the first read that disagrees fails a publish whose
  # assets are all there, and the next run repeats the day's analysis.
  w <- .pub_world(list(.pub_prior()))
  .pub_fail(w, "stale-data-manifest.json", 2L)
  r <- .pub_publish(w)
  expect_identical(r$status, 0L, info = r$output)
  .pub_expect_published(w)
  expect_length(grep("^gh release view ", .pub_calls(w)), 3L)
})

test_that("a publish edit that fails once is made again, and the release is published", {
  # The edit comes after every asset has uploaded and been checked, so one 5xx
  # on it used to throw away a verified upload and leave a complete draft.
  w <- .pub_world(list(.pub_prior()))
  .pub_fail(w, "publish", 1L)
  r <- .pub_publish(w)
  expect_identical(r$status, 0L, info = r$output)
  .pub_expect_published(w)
  expect_length(grep("--draft=false", .pub_calls(w), fixed = TRUE), 2L)
  expect_length(grep("^gh release (create|delete) ", .pub_calls(w)), 1L)
  expect_identical(.pub_latest(w), "metrics-2026-09-13")
})

test_that("a publish edit that landed but reported failure is safe to make again", {
  # A PATCH that returns 500 can still have applied. The next attempt finds a
  # published release under the tag and sets the same two fields on it.
  w <- .pub_world(list(.pub_prior()))
  .pub_fail(w, "publish-after", 1L)
  r <- .pub_publish(w)
  expect_identical(r$status, 0L, info = r$output)
  .pub_expect_published(w)
  expect_false(.pub_releases(w, "metrics-2026-09-12")[[1L]]$isLatest)
  expect_length(grep("--draft=false", .pub_calls(w), fixed = TRUE), 2L)
})

test_that("a publish edit that never lands leaves a draft the next call replaces", {
  w <- .pub_world(list(.pub_prior()))
  .pub_fail(w, "publish", 99L)
  r <- .pub_publish(w)
  expect_false(identical(r$status, 0L))
  expect_length(grep("--draft=false", .pub_calls(w), fixed = TRUE), 5L)
  expect_true(.pub_releases(w, "metrics-2026-09-13")[[1L]]$isDraft)
  expect_identical(.pub_latest(w), "metrics-2026-09-12")

  file.remove(file.path(w$fails, "publish"))
  file.create(w$log)
  r <- .pub_publish(w)
  expect_identical(r$status, 0L, info = r$output)
  .pub_expect_published(w)
  expect_length(grep("^gh release delete metrics-2026-09-13 --yes$", .pub_calls(w)), 1L)
})

test_that("a same-day notes edit that fails once is made again", {
  w <- .pub_world(list(.pub_prior()))
  expect_identical(.pub_publish(w)$status, 0L)
  file.create(w$log)
  .pub_fail(w, "edit", 1L)
  r <- .pub_sh(w, paste(
    "publish_release metrics-2026-09-13 'Bioconductor Metrics - 2026-09-13 (updated 09:00 UTC)'",
    "out/release-notes-code.md", .PUB_ASSETS, "|| exit 1"))
  expect_identical(r$status, 0L, info = r$output)
  expect_identical(.pub_releases(w, "metrics-2026-09-13")[[1L]]$name,
                   "Bioconductor Metrics - 2026-09-13 (updated 09:00 UTC)")
  expect_length(grep("^gh release edit ", .pub_calls(w)), 2L)
})

test_that("an edit waits ten seconds longer after each failed attempt", {
  w <- .pub_world(list(.pub_prior()))
  .pub_fail(w, "publish", 2L)
  slept <- file.path(w$dir, "slept")
  r <- .pub_sh(w, c(sprintf("sleep() { echo \"$1\" >> %s; }", shQuote(slept)),
                    "publish_release metrics-2026-09-13 t out/release-notes-code.md out/code-manifest.json || exit 1"),
               wait = NA)
  expect_identical(r$status, 0L, info = r$output)
  expect_identical(readLines(slept), c("10", "20"))
})

test_that("a draft that will not delete is not deleted again in the same call", {
  # The delete goes by tag, and when the listing has not caught up with a
  # release published under the same tag it can take that release instead, so
  # it is not repeated. The draft is left to the next publish.
  w <- .pub_world(list(.pub_prior(), .pub_stranded()))
  .pub_fail(w, "delete", 1L)
  r <- .pub_publish(w)
  expect_false(identical(r$status, 0L))
  expect_length(grep("^gh release delete ", .pub_calls(w)), 1L)
  expect_false(any(grepl("^gh release (create|upload|edit) ", .pub_calls(w))))
  expect_true(.pub_releases(w, "metrics-2026-09-13")[[1L]]$isDraft)
})

test_that("a create that errors is not retried, and its draft is replaced next time", {
  # A POST that returns 500 may still have created the release. Retrying in a
  # loop could leave several drafts under one tag; the next call finds the one.
  w <- .pub_world(list(.pub_prior()))
  .pub_fail(w, "create", 1L)
  r <- .pub_publish(w)
  expect_false(identical(r$status, 0L))
  expect_length(grep("^gh release create ", .pub_calls(w)), 1L)
  expect_length(.pub_uploads(w), 0L)

  r <- .pub_publish(w)
  expect_identical(r$status, 0L, info = r$output)
  .pub_expect_published(w)
})

test_that("a release listing and a read-back that each fail once are read again", {
  # The create is the only call that is unsafe to repeat. A single 5xx on the
  # read-back used to fail the publish after both databases had landed, which
  # threw away the upload and left the release an unpublished draft.
  w <- .pub_world(list(.pub_prior()))
  .pub_fail(w, "list", 1L)
  .pub_fail(w, "view", 1L)
  r <- .pub_publish(w)
  expect_identical(r$status, 0L, info = r$output)
  .pub_expect_published(w)
  calls <- .pub_calls(w)
  expect_length(grep("^gh release create ", calls), 1L)
  expect_length(grep("^gh release list ", calls), 2L)
  expect_length(grep("^gh release view ", calls), 2L)
})

test_that("a listing that fails once during a same-day republish still reads as published", {
  # release_state answers on stdout, so a retry message printed there would turn
  # "published" into two lines and the republish into a refusal.
  w <- .pub_world(list(.pub_prior()))
  expect_identical(.pub_publish(w)$status, 0L)
  file.create(w$log)
  .pub_fail(w, "list", 1L)
  r <- .pub_publish(w)
  expect_identical(r$status, 0L, info = r$output)
  .pub_expect_published(w)
  expect_false(any(grepl("^gh release (create|delete) ", .pub_calls(w))))
})

test_that("a read-back that never succeeds fails the call and leaves today unpublished", {
  w <- .pub_world(list(.pub_prior()))
  .pub_fail(w, "view", 99L)
  r <- .pub_publish(w)
  expect_false(identical(r$status, 0L))
  expect_length(grep("^gh release view ", .pub_calls(w)), 5L)
  expect_false(any(grepl("--draft=false", .pub_calls(w), fixed = TRUE)))
  expect_true(.pub_releases(w, "metrics-2026-09-13")[[1L]]$isDraft)
  expect_identical(.pub_latest(w), "metrics-2026-09-12")
})

test_that("a read waits ten seconds longer after each failed attempt", {
  w <- .pub_world(list(.pub_prior()))
  .pub_fail(w, "list", 2L)
  .pub_fail(w, "view", 2L)
  slept <- file.path(w$dir, "slept")
  r <- .pub_sh(w, c(sprintf("sleep() { echo \"$1\" >> %s; }", shQuote(slept)),
                    "release_state metrics-2026-09-12 > /dev/null || exit 1",
                    "verify_assets metrics-2026-09-12 || exit 1"),
               wait = NA)
  expect_identical(r$status, 0L, info = r$output)
  expect_identical(readLines(slept), c("10", "20", "10", "20"))
})

# ---------------------------------------------------------------------------
# The heartbeat
# ---------------------------------------------------------------------------

.pub_heartbeat <- function(w, tag) {
  .pub_sh(w, sprintf(
    "refresh_heartbeat %s out/code-manifest.json out/data-manifest.json || exit 1",
    shQuote(tag)))
}

test_that("the heartbeat refreshes both manifests on a published release", {
  w <- .pub_world(list(.pub_prior()))
  r <- .pub_heartbeat(w, "metrics-2026-09-12")
  expect_identical(r$status, 0L, info = r$output)
  expect_identical(.pub_uploads(w), c("code-manifest.json", "data-manifest.json"))
  prior <- .pub_releases(w, "metrics-2026-09-12")[[1L]]
  sizes <- setNames(vapply(prior$assets, function(a) as.numeric(a$size), 1),
                    vapply(prior$assets, function(a) a$name, ""))
  expect_equal(sizes[["code-manifest.json"]], 120)
  expect_equal(sizes[["bioc-code-metrics.db"]], 4000)
  expect_false(any(grepl("^gh release (create|delete|edit) ", .pub_calls(w))))
})

test_that("the heartbeat refuses to write into a draft", {
  w <- .pub_world(list(.pub_prior(), .pub_stranded()))
  r <- .pub_heartbeat(w, "metrics-2026-09-13")
  expect_false(identical(r$status, 0L))
  expect_true(grepl("draft", r$output, fixed = TRUE))
  expect_length(.pub_uploads(w), 0L)
})

test_that("the heartbeat refuses a doubled tag and names the releases by id", {
  # Every daily run refuses until someone clears the draft, so the refusal has
  # to say how to do that without a delete by tag.
  w <- .pub_world(.pub_doubled())
  r <- .pub_heartbeat(w, "metrics-2026-09-13")
  expect_false(identical(r$status, 0L))
  expect_true(grepl("id 2: published", r$output, fixed = TRUE), info = r$output)
  expect_true(grepl("id 3: draft", r$output, fixed = TRUE), info = r$output)
  expect_true(grepl("gh api -X DELETE repos/{owner}/{repo}/releases/<id>", r$output, fixed = TRUE))
  expect_length(.pub_uploads(w), 0L)
  expect_length(.pub_state(w), 3L)
})

test_that("the heartbeat fails when its upload never lands", {
  w <- .pub_world(list(.pub_prior()))
  .pub_fail(w, "upload-data-manifest.json", 99L)
  r <- .pub_heartbeat(w, "metrics-2026-09-12")
  expect_false(identical(r$status, 0L))
  expect_identical(sum(.pub_uploads(w) == "data-manifest.json"), 5L)
})

test_that("the heartbeat reads the release again when a read fails once", {
  # Most days publish nothing and end here. A heartbeat that goes red leaves
  # last_checked where it was, and the merger's readiness gate reads freshness
  # from last_checked, so one 5xx would make this pipeline look late.
  w <- .pub_world(list(.pub_prior()))
  .pub_fail(w, "list", 1L)
  .pub_fail(w, "view", 1L)
  r <- .pub_heartbeat(w, "metrics-2026-09-12")
  expect_identical(r$status, 0L, info = r$output)
  expect_identical(.pub_uploads(w), c("code-manifest.json", "data-manifest.json"))
})

test_that("the heartbeat fails when the release list never reads", {
  w <- .pub_world(list(.pub_prior()))
  .pub_fail(w, "list", 99L)
  r <- .pub_heartbeat(w, "metrics-2026-09-12")
  expect_false(identical(r$status, 0L))
  expect_length(grep("^gh release list ", .pub_calls(w)), 5L)
  expect_length(.pub_uploads(w), 0L)
})

test_that("the heartbeat fails when a manifest landed at the wrong size", {
  w <- .pub_world(list(.pub_prior()))
  .pub_fail(w, "short-code-manifest.json", 1L)
  r <- .pub_heartbeat(w, "metrics-2026-09-12")
  expect_false(identical(r$status, 0L))
  expect_true(grepl("code-manifest.json 120", r$output, fixed = TRUE))
})

test_that("the heartbeat has nothing to do without a prior release", {
  w <- .pub_world(list())
  r <- .pub_heartbeat(w, "")
  expect_identical(r$status, 0L, info = r$output)
  expect_length(.pub_calls(w), 0L)
})

# ---------------------------------------------------------------------------
# Drafts no publish comes back for
# ---------------------------------------------------------------------------

.pub_release <- function(id, tag, draft = FALSE, assets = list()) {
  list(id = id, tagName = tag, isDraft = draft, isLatest = FALSE, name = "", assets = assets)
}

.pub_tags <- function(w, drafts) {
  rs <- Filter(function(r) isTRUE(r$isDraft) == drafts, .pub_state(w))
  sort(vapply(rs, function(r) r$tagName, ""))
}

test_that("a draft a failed publish left on an earlier day is deleted once a later day is out", {
  # A publish replaces a draft only under the tag it publishes, and the prune
  # leaves drafts out of its listing. This pipeline runs once a day, so a
  # publish that fails leaves its draft, databases and all, under a tag nothing
  # publishes again.
  w <- .pub_world(list(.pub_prior()))
  .pub_fail(w, "upload-bioc-data-metrics.db", 99L)
  expect_false(identical(.pub_publish(w, tag = "metrics-2026-09-13")$status, 0L))
  file.remove(file.path(w$fails, "upload-bioc-data-metrics.db"))
  .pub_fail(w, "publish", 99L)
  expect_false(identical(.pub_publish(w, tag = "metrics-2026-09-14")$status, 0L))
  file.remove(file.path(w$fails, "publish"))
  expect_identical(.pub_publish(w, tag = "metrics-2026-09-15")$status, 0L)
  expect_identical(.pub_tags(w, drafts = TRUE), c("metrics-2026-09-13", "metrics-2026-09-14"))

  file.create(w$log)
  r <- .pub_sh(w, "delete_stale_drafts metrics metrics-2026-09-15 || exit 1")
  expect_identical(r$status, 0L, info = r$output)
  expect_length(.pub_tags(w, drafts = TRUE), 0L)
  expect_identical(.pub_tags(w, drafts = FALSE), c("metrics-2026-09-12", "metrics-2026-09-15"))
  deletes <- grep("-X DELETE", .pub_calls(w), value = TRUE, fixed = TRUE)
  expect_identical(sub(".*/", "", deletes), c("3", "2"))
  expect_false(any(grepl("^gh release delete", .pub_calls(w))))
})

test_that("clearing drafts leaves today's draft, other series and every published release alone", {
  # By id, so a draft beside a published release under the same tag goes and
  # the published one stays, which a delete by tag cannot promise.
  w <- .pub_world(list(
    .pub_release(10L, "code-2026-07-01", draft = TRUE),
    .pub_prior(),
    .pub_release(4L, "metrics-2026-09-12", draft = TRUE),
    .pub_release(5L, "metrics-2026-09-13"),
    .pub_release(6L, "metrics-2026-09-14", draft = TRUE)))
  r <- .pub_sh(w, "delete_stale_drafts metrics metrics-2026-09-14 || exit 1")
  expect_identical(r$status, 0L, info = r$output)
  expect_identical(vapply(.pub_state(w), function(x) as.integer(x$id), 1L), c(10L, 1L, 5L, 6L))
})

test_that("a draft that will not delete waits for the next run, and a listing that fails stops the step", {
  stale <- list(.pub_prior(), .pub_stranded(),
                .pub_release(3L, "metrics-2026-09-14", draft = TRUE),
                .pub_release(4L, "metrics-2026-09-15"))
  step <- c("delete_stale_drafts metrics metrics-2026-09-15 || exit 1",
            'echo "went on past the drafts"')

  w <- .pub_world(stale)
  .pub_fail(w, "api-delete", 1L)
  r <- .pub_sh(w, step)
  expect_identical(r$status, 0L, info = r$output)
  expect_true(grepl("::warning::could not delete the draft metrics-2026-09-14", r$output, fixed = TRUE),
              info = r$output)
  expect_identical(.pub_tags(w, drafts = TRUE), "metrics-2026-09-14")

  # A listing that cannot be read is not "no drafts".
  w <- .pub_world(stale)
  .pub_fail(w, "api", 1L)
  r <- .pub_sh(w, step)
  expect_false(identical(r$status, 0L))
  expect_false(grepl("went on past the drafts", r$output, fixed = TRUE))
  expect_length(.pub_tags(w, drafts = TRUE), 2L)
})

# ---------------------------------------------------------------------------
# The scripts as written
# ---------------------------------------------------------------------------

# Logical lines, with backslash continuations joined, so a gh call split over
# several lines is read as one.
.pub_logical_lines <- function(path) {
  txt <- paste(readLines(path), collapse = "\n")
  strsplit(gsub("\\\\\n\\s*", " ", txt), "\n", fixed = TRUE)[[1L]]
}

# The lines of a script that call gh, leaving out comments and messages that
# only mention a gh command.
.pub_gh_calls <- function(path) {
  lines <- .pub_logical_lines(path)
  lines <- lines[!grepl("^\\s*#", lines)]
  grep("(^|[\\s$(;])gh (release|api) ", lines, value = TRUE, perl = TRUE)
}

test_that("every gh call in scripts/publish.sh stops the function when it fails", {
  # A function called as `f || exit 1` runs with errexit off, so a failed gh
  # call inside it is ignored unless the line itself returns. The same holds
  # for a call to one of the script's own helpers that can fail.
  gh <- .pub_gh_calls(.pub_script())
  expect_true(length(gh) > 0L)
  lines <- .pub_logical_lines(.pub_script())
  lines <- lines[!grepl("^\\s*#", lines)]
  helpers <- grep("\\b(release_state|release_rows|upload_asset|verify_assets|edit_release|file_bytes)[ )]",
                  lines, value = TRUE, perl = TRUE)
  expect_gte(length(helpers), 8L)
  expect_true(any(grepl("gh api ", gh, fixed = TRUE)))
  calls <- c(gh, helpers)
  guarded <- grepl("\\|\\| return 1", calls) |
    grepl("^\\s*if !? ?([a-z_]+=\\$\\()?gh ", calls)
  expect_true(all(guarded), info = paste(calls[!guarded], collapse = "\n"))
})

test_that("update.yml resolves only published releases and publishes through scripts/publish.sh", {
  yml_path <- file.path("..", "..", ".github", "workflows", "update.yml")
  lines <- .pub_logical_lines(yml_path)
  yml <- paste(lines, collapse = "\n")

  lists <- grep("gh release list", lines, value = TRUE, fixed = TRUE)
  expect_true(all(grepl("--exclude-drafts", lists, fixed = TRUE)),
              info = paste(lists, collapse = "\n"))
  # The only draft-aware listing is release_state's, and it lives in publish.sh.
  expect_false(grepl("latest_tag()", yml, fixed = TRUE))
  expect_false(grepl("gh release create", yml, fixed = TRUE))
  expect_false(grepl("gh release upload", yml, fixed = TRUE))
  expect_gte(lengths(regmatches(yml, gregexpr("source scripts/publish.sh", yml, fixed = TRUE))), 2L)
  expect_true(grepl("publish_release ", yml, fixed = TRUE))

  sh <- paste(.pub_logical_lines(.pub_script()), collapse = "\n")
  latest <- regmatches(sh, regexpr("latest_tag\\(\\) \\{[^}]*\\}", sh))
  expect_length(latest, 1L)
  expect_true(grepl("--exclude-drafts", latest, fixed = TRUE))
  # A draft has no git tag, so --cleanup-tag deletes the release and then fails.
  # Pruning published releases in update.yml is where that flag belongs.
  expect_false(any(grepl("--cleanup-tag", .pub_gh_calls(.pub_script()), fixed = TRUE)))
})

test_that("the prune clears the drafts a failed publish left on an earlier day", {
  # Publishing replaces a draft only under today's tag, and the prune's listing
  # leaves drafts out, so without this a draft from a failed publish stays for
  # good.
  yml <- readLines(file.path("..", "..", ".github", "workflows", "update.yml"))
  start <- grep("- name: Prune old dated releases", yml, fixed = TRUE)
  expect_length(start, 1L)
  prune <- yml[start:length(yml)]
  expect_true(any(grepl("source scripts/publish.sh", prune, fixed = TRUE)))
  expect_true(any(grepl('delete_stale_drafts metrics "metrics-$(date -u +%Y-%m-%d)" || exit 1',
                        prune, fixed = TRUE)))

  sh <- paste(.pub_logical_lines(.pub_script()), collapse = "\n")
  body <- regmatches(sh, regexpr("(?s)delete_stale_drafts\\(\\) \\{.*?\n\\}", sh, perl = TRUE))
  expect_length(body, 1L)
  expect_true(grepl("gh api -X DELETE", body, fixed = TRUE))
  expect_false(grepl("gh release delete", body, fixed = TRUE))
})

test_that("update.yml tells preflight which releases it resolved", {
  yml <- paste(readLines(file.path("..", "..", ".github", "workflows", "update.yml")),
               collapse = "\n")
  call <- regmatches(yml, regexpr("Rscript scripts/preflight.R[^\n]*", yml))
  expect_length(call, 1L)
  expect_true(grepl("--code-src=\"$CODE_SRC\"", call, fixed = TRUE))
  expect_true(grepl("--data-src=\"$DATA_SRC\"", call, fixed = TRUE))
})
