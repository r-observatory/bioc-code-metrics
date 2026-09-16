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

# One asset on a seeded release. A half-written upload ("starter") carries no
# digest, as the release endpoint reports it. The id is filled in by
# .pub_world, so a test only names one when it wants to assert on it.
.pub_asset <- function(name, size, state = "uploaded", id = NULL) {
  a <- list(name = name, size = size, state = state)
  if (!is.null(id)) a$id <- id
  a
}

# Give every seeded asset the id and digest a real release would report. The
# ids run from 1001 so they cannot be confused with a release id in the call
# log, and the digest stands in for bytes this fake does not hold.
.pub_with_asset_ids <- function(releases) {
  next_id <- 1000L
  lapply(releases, function(rel) {
    rel$assets <- lapply(rel$assets %||% list(), function(a) {
      if (is.null(a$id)) {
        next_id <<- next_id + 1L
        a$id <- next_id
      }
      if (identical(a$state, "uploaded") && is.null(a$digest)) {
        a$digest <- paste0("sha256:", digest::digest(paste(a$name, a$size), algo = "sha256"))
      }
      a
    })
    rel
  })
}

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

  jsonlite::write_json(.pub_with_asset_ids(releases), w$state, auto_unbox = TRUE)
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

# The assets of a release, by name.
.pub_assets <- function(w, tag) {
  rel <- .pub_releases(w, tag)
  if (length(rel) != 1L) return(list())
  setNames(rel[[1L]]$assets, vapply(rel[[1L]]$assets, function(a) a$name, ""))
}

# "<id> <size> <state>" for one asset, or NA when the release has no such name.
.pub_asset_line <- function(w, tag, name) {
  a <- .pub_assets(w, tag)[[name]]
  if (is.null(a)) return(NA_character_)
  sprintf("%s %s %s", a$id, a$size, a$state)
}

# Take an asset off a release between two runs, by name. It stands for the
# ways a live name goes missing that no run performs: an upload cut off under
# it and cleared, or a delete by hand.
.pub_drop_asset <- function(w, tag, name) {
  releases <- lapply(.pub_state(w), function(rel) {
    if (identical(rel$tagName, tag)) {
      rel$assets <- Filter(function(a) !identical(a$name, name), rel$assets)
    }
    rel
  })
  jsonlite::write_json(releases, w$state, auto_unbox = TRUE)
}

# Read a release the way the merger does: by name, through gh. Answers the
# fake's stand-in for the bytes, "<name> <size> <digest>", or fails the way gh
# does when nothing carries the name.
.pub_read <- function(w, tag, name) {
  .pub_sh(w, sprintf("gh release download %s -p %s -O - || exit 1", shQuote(tag), shQuote(name)))
}

# What a reader should get for a file on disk.
.pub_bytes_of <- function(w, name) {
  path <- file.path(w$dir, "out", name)
  sprintf("%s %d sha256:%s", name, file.size(path),
          digest::digest(file = path, algo = "sha256"))
}

# The renames the run made, as "<asset id> <new name>", in the order they went.
.pub_renames <- function(w) {
  p <- grep("^gh api -X PATCH ", .pub_calls(w), value = TRUE)
  sub("^gh api -X PATCH repos/\\{owner\\}/\\{repo\\}/releases/assets/([0-9]+) -f name=(\\S+).*$",
      "\\1 \\2", p)
}

# The ids of the assets the run deleted.
.pub_asset_deletes <- function(w) {
  d <- grep("^gh api -X DELETE repos/\\{owner\\}/\\{repo\\}/releases/assets/", .pub_calls(w),
            value = TRUE)
  sub("^.*/assets/([0-9]+).*$", "\\1", d)
}

# Today's release as a published, Latest release carrying exactly the four
# assets at the sizes on disk. A replacement leaves the copy it displaced under
# swap-prev-<name> for the next one to delete, so those are allowed beside the four;
# a swap-next-<name> is not, because the swap ends with that name gone.
.pub_expect_published <- function(w, tag = "metrics-2026-09-13") {
  rel <- .pub_releases(w, tag)
  expect_length(rel, 1L)
  rel <- rel[[1L]]
  expect_false(rel$isDraft)
  expect_true(rel$isLatest)
  spare <- grepl("^swap-(prev|next)-", vapply(rel$assets, function(a) a$name, ""))
  got <- vapply(rel$assets[!spare], function(a) sprintf("%s %d %s", a$name, a$size, a$state), "")
  want <- vapply(c("bioc-code-metrics.db", "bioc-data-metrics.db", "code-manifest.json",
                   "data-manifest.json"),
                 function(f) sprintf("%s %d uploaded", f,
                                     file.size(file.path(w$dir, "out", f))), "")
  expect_setequal(got, unname(want))
  left <- vapply(rel$assets[spare], function(a) a$name, "")
  expect_true(all(grepl("^swap-prev-", left)), info = paste(left, collapse = ", "))
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

test_that("a failed replacement on today's published release fails the call", {
  # The second shard of a run republishes into the release the first shard
  # published. While the gh calls there were unguarded, a failed upload followed
  # by a successful title edit returned 0 and the step went green.
  w <- .pub_world(list(.pub_prior()))
  expect_identical(.pub_publish(w)$status, 0L)
  file.create(w$log)
  .pub_fail(w, "upload-swap-next-bioc-code-metrics.db", 99L)

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

# A `stat` that answers the way GNU coreutils does on the runner, whatever this
# machine has: -c takes a format, and -f means --file-system and takes none, so
# the BSD form `-f%z` is a usage error.
.pub_gnu_stat <- function(w) {
  writeLines(c(
    "#!/usr/bin/env bash",
    'case "$1" in',
    "  -c%s)",
    '    [ -e "$2" ] || { echo "stat: cannot statx $2: No such file or directory" >&2; exit 1; }',
    '    wc -c < "$2" | tr -d " " ;;',
    "  *) echo \"stat: invalid option -- '%'\" >&2; exit 1 ;;",
    "esac"), file.path(w$bin, "stat"))
  Sys.chmod(file.path(w$bin, "stat"), "755")
}

test_that("file_bytes names a missing file on stderr, not with a GNU stat usage error", {
  # file_bytes asks GNU stat for a size and falls back to the BSD form, so the
  # same helper works where the tests run on macOS. On the runner that fallback
  # is a usage error, so the size check in update.yml failed on a database
  # missing from out/ with nothing but "stat: invalid option -- '%'".
  w <- .pub_world(list(.pub_prior()))
  .pub_gnu_stat(w)
  unlink(file.path(w$dir, "out", "bioc-data-metrics.db"))
  r <- .pub_sh(w, c(
    sprintf('[ "$(command -v stat)" = %s ] || exit 98', shQuote(file.path(w$bin, "stat"))),
    'echo "measured <$(file_bytes out/code-manifest.json)>"',
    'bytes=$(file_bytes out/bioc-data-metrics.db) || { echo "answered <${bytes}>"; exit 1; }'))
  expect_identical(r$status, 1L, info = r$output)
  expect_true(grepl("measured <120>", r$output, fixed = TRUE), info = r$output)
  expect_true(grepl("::error::out/bioc-data-metrics.db does not exist", r$output, fixed = TRUE),
              info = r$output)
  # Callers read the size from stdout, so the error stays out of it.
  expect_true(grepl("answered <>", r$output, fixed = TRUE), info = r$output)
  expect_false(grepl("invalid option", r$output, fixed = TRUE))
})

test_that("a file missing from out/ is named before the release is touched", {
  # The databases are measured for the size budget before a publish, but a
  # missing manifest was not measured at all: it got as far as a draft holding
  # both databases and five failed uploads before anything said which file it
  # was. A same-day republish would first have replaced the databases on the
  # published release.
  today <- .pub_prior()
  today$id <- 2L
  today$tagName <- "metrics-2026-09-13"
  prior <- .pub_prior()
  prior$isLatest <- FALSE
  for (releases in list(list(.pub_prior()), list(prior, today))) {
    for (missing in c("bioc-data-metrics.db", "data-manifest.json")) {
      w <- .pub_world(releases)
      .pub_gnu_stat(w)
      unlink(file.path(w$dir, "out", missing))
      r <- .pub_publish(w)
      expect_false(identical(r$status, 0L))
      expect_true(grepl(sprintf("::error::out/%s does not exist", missing), r$output, fixed = TRUE),
                  info = r$output)
      expect_false(grepl("invalid option", r$output, fixed = TRUE))
      expect_length(.pub_calls(w), 0L)
    }
  }
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
# Replacing an asset on a release people are already reading
# ---------------------------------------------------------------------------
#
# Every case here is about the same window. `gh release upload --clobber`
# deletes the live asset and then uploads its replacement, so from the delete
# until the upload finishes the release advertises no database at all, and if
# the upload never lands it never carries one again: the next run resolves that
# release, finds no database, and preflight refuses. Measured against a scratch
# repository, a 300 MiB upload takes 23 to 37 s, and the databases here are
# larger. The replacement below uploads under a temporary name first and then
# renames, which leaves the live asset in place for all of that and swaps the
# names in about half a second.

# Today's release as a first shard published it, at sizes that are not the ones
# on disk, so a replacement is visible in the sizes.
.pub_first_shard <- function(assets = list(
  .pub_asset("bioc-code-metrics.db", 4000L), .pub_asset("bioc-data-metrics.db", 2000L),
  .pub_asset("code-manifest.json", 8L), .pub_asset("data-manifest.json", 9L))) {
  list(id = 2L, tagName = "metrics-2026-09-13", isDraft = FALSE, isLatest = TRUE,
       name = "Bioconductor Metrics - 2026-09-13", assets = assets)
}

# Yesterday's release, and today's carrying what the first shard published.
.pub_shard_world <- function(assets = NULL, env = parent.frame()) {
  prior <- .pub_prior()
  prior$isLatest <- FALSE
  today <- if (is.null(assets)) .pub_first_shard() else .pub_first_shard(assets)
  .pub_world(list(prior, today), env = env)
}

# What a reader gets for a seeded asset, the way .pub_with_asset_ids stamps it.
.pub_seeded_bytes <- function(name, size) {
  sprintf("%s %d sha256:%s", name, size, digest::digest(paste(name, size), algo = "sha256"))
}

test_that("a second shard uploads beside the live asset and renames it into place", {
  w <- .pub_shard_world()
  before <- .pub_assets(w, "metrics-2026-09-13")
  r <- .pub_publish(w)
  expect_identical(r$status, 0L, info = r$output)
  .pub_expect_published(w)

  # Nothing was uploaded under a name a reader asks for.
  expect_identical(.pub_uploads(w), c("swap-next-bioc-code-metrics.db", "swap-next-bioc-data-metrics.db",
                                      "swap-next-code-manifest.json", "swap-next-data-manifest.json"))
  expect_false(any(grepl("upload metrics-2026-09-13 out/bioc-", .pub_calls(w), fixed = TRUE)))
  # The bytes went up once, under a link, not as a second copy of the file.
  expect_true(all(grepl(".swap-stage/", grep("^gh release upload ", .pub_calls(w), value = TRUE),
                        fixed = TRUE)))

  after <- .pub_assets(w, "metrics-2026-09-13")
  # The asset that was live is still on the release, under swap-prev-, with its id,
  # size and digest untouched; the name now belongs to the new upload.
  expect_identical(after[["swap-prev-bioc-code-metrics.db"]]$id, before[["bioc-code-metrics.db"]]$id)
  expect_identical(after[["swap-prev-bioc-code-metrics.db"]]$size, before[["bioc-code-metrics.db"]]$size)
  expect_false(identical(after[["bioc-code-metrics.db"]]$id, before[["bioc-code-metrics.db"]]$id))
  expect_identical(.pub_read(w, "metrics-2026-09-13", "bioc-code-metrics.db")$output,
                   .pub_bytes_of(w, "bioc-code-metrics.db"))
})

test_that("a republish reads the release once for each thing it decides", {
  # Every read is a request against the hourly REST budget the workflow's token
  # gets for this repository, and a changeover day, or a force_full dispatch,
  # publishes after every shard that changed anything. Three reads per asset
  # decide something: what the repair left under the name, whether the upload
  # landed whole, and what the release carries once the names have moved. The
  # release the tag names is resolved once for the whole publish.
  w <- .pub_shard_world()
  r <- .pub_publish(w)
  expect_identical(r$status, 0L, info = r$output)
  calls <- .pub_calls(w)
  expect_length(grep("releases/tags/", calls, fixed = TRUE), 1L)
  expect_length(grep("/assets?per_page=100", calls, fixed = TRUE), 12L)
  expect_length(calls, 28L)
})

test_that("a replacement never changes the extension of the name it uploads under", {
  # gh reads an asset's content type off the file extension as it uploads, and
  # a rename changes the name and nothing else, so a name the replacement
  # invents decides how the release serves those bytes for good. Measured
  # against a scratch repository: out/code-manifest.json uploaded under its own
  # name came back application/json, the same file uploaded as
  # swap-next-code-manifest.json came back application/octet-stream and kept that
  # through the rename that gave it the manifest's name, and uploaded as
  # swap-next-code-manifest.json it came back application/json and kept that.
  w <- .pub_shard_world()
  r <- .pub_publish(w)
  expect_identical(r$status, 0L, info = r$output)
  left <- grep("^swap-(prev|next)-", names(.pub_assets(w, "metrics-2026-09-13")), value = TRUE)
  expect_length(left, 4L)
  names <- c(.pub_uploads(w), left)
  expect_true(all(grepl("\\.(db|json)$", names)), info = paste(names, collapse = ", "))
})

test_that("the live asset is renamed out of the way before the new one takes its name", {
  # The other order, deleting the live asset first, is what hands a reader that
  # listed a moment earlier a hard 404 on an id that is gone.
  w <- .pub_shard_world()
  before <- .pub_assets(w, "metrics-2026-09-13")
  expect_identical(.pub_publish(w)$status, 0L)

  renames <- .pub_renames(w)
  db <- grep("(^| )(swap-prev-)?bioc-code-metrics\\.db$", renames, value = TRUE)
  expect_identical(db, c(sprintf("%s swap-prev-bioc-code-metrics.db", before[["bioc-code-metrics.db"]]$id),
                         sprintf("%s bioc-code-metrics.db",
                                 .pub_assets(w, "metrics-2026-09-13")[["bioc-code-metrics.db"]]$id)))
  # Nothing a reader could be holding was deleted to make room.
  expect_false(before[["bioc-code-metrics.db"]]$id %in% .pub_asset_deletes(w))
})

test_that("databases are replaced before manifests, whatever order they are passed in", {
  w <- .pub_shard_world()
  r <- .pub_publish(w, assets = paste(
    "out/code-manifest.json out/bioc-code-metrics.db out/data-manifest.json",
    "out/bioc-data-metrics.db"))
  expect_identical(r$status, 0L, info = r$output)
  # The renames that give an asset the name a reader asks for, in the order
  # they went; the other four move the displaced copy out of the way.
  swapped <- grep(" swap-(prev|next)-", .pub_renames(w), value = TRUE, invert = TRUE)
  expect_length(swapped, 4L)
  expect_true(all(grepl("\\.db$", swapped[1:2])))
  expect_true(all(grepl("\\.json$", swapped[3:4])))
})

test_that("the previous copy is left for the next replacement to delete", {
  # Deleting an asset cuts off a download of it that is already running, and the
  # merger's is tens of seconds long, so the copy a reader may still be pulling
  # stays until the next publish under this tag has no use for it.
  w <- .pub_shard_world()
  expect_identical(.pub_publish(w)$status, 0L)
  expect_true("swap-prev-bioc-code-metrics.db" %in% names(.pub_assets(w, "metrics-2026-09-13")))
  expect_length(.pub_asset_deletes(w), 0L)

  prev <- .pub_assets(w, "metrics-2026-09-13")[["swap-prev-bioc-code-metrics.db"]]$id
  file.create(w$log)
  expect_identical(.pub_publish(w)$status, 0L)
  expect_true(prev %in% .pub_asset_deletes(w))
  expect_length(grep("^swap-prev-", names(.pub_assets(w, "metrics-2026-09-13"))), 4L)
})

test_that("an upload that never lands leaves the live asset where readers find it", {
  w <- .pub_shard_world()
  .pub_fail(w, "upload-swap-next-bioc-code-metrics.db", 99L)
  before <- .pub_asset_line(w, "metrics-2026-09-13", "bioc-code-metrics.db")
  r <- .pub_publish(w)
  expect_false(identical(r$status, 0L))
  expect_true(grepl("bioc-code-metrics.db", r$output, fixed = TRUE))
  expect_identical(.pub_asset_line(w, "metrics-2026-09-13", "bioc-code-metrics.db"), before)
  expect_length(.pub_renames(w), 0L)
  read <- .pub_read(w, "metrics-2026-09-13", "bioc-code-metrics.db")
  expect_identical(read$status, 0L, info = read$output)
  expect_identical(read$output, .pub_seeded_bytes("bioc-code-metrics.db", 4000L))
})

test_that("a temporary asset that landed short or under the wrong digest never takes the name", {
  for (how in c("short", "corrupt")) {
    w <- .pub_shard_world()
    .pub_fail(w, sprintf("%s-swap-next-bioc-code-metrics.db", how), 1L)
    before <- .pub_asset_line(w, "metrics-2026-09-13", "bioc-code-metrics.db")
    r <- .pub_publish(w)
    expect_false(identical(r$status, 0L))
    expect_true(grepl("swap-next-bioc-code-metrics.db", r$output, fixed = TRUE), info = r$output)
    expect_length(.pub_renames(w), 0L)
    expect_identical(.pub_asset_line(w, "metrics-2026-09-13", "bioc-code-metrics.db"), before)
    # Read five times before refusing: a release that has not caught up with an
    # upload is not the same thing as an asset that landed wrong.
    expect_gte(length(grep("/assets", .pub_calls(w), fixed = TRUE)), 5L)
  }
})

test_that("a temporary asset still half-written after the upload never takes the name", {
  # gh returning 0 is not proof the bytes are servable: measured against a
  # scratch repository, one upload of 300 MiB was still answering BlobNotFound
  # 24.5 s after gh exited 0.
  w <- .pub_shard_world()
  .pub_fail(w, "starter-swap-next-bioc-code-metrics.db", 99L)
  r <- .pub_publish(w)
  expect_false(identical(r$status, 0L))
  expect_true(grepl("starter", r$output, fixed = TRUE), info = r$output)
  expect_length(.pub_renames(w), 0L)
  read <- .pub_read(w, "metrics-2026-09-13", "bioc-code-metrics.db")
  expect_identical(read$output, .pub_seeded_bytes("bioc-code-metrics.db", 4000L))
})

test_that("a staging copy the run refused is taken off the release", {
  # Nothing downloads swap-next-<name>, so bytes left under it serve no reader,
  # and the repair gives that name to <name> when the release has lost <name>
  # itself. The one copy this run measured against the file and refused would
  # then be the copy a later run puts in front of readers, without measuring it
  # again, so the run that refused it clears it.
  for (how in c("short", "corrupt", "starter")) {
    w <- .pub_shard_world()
    .pub_fail(w, sprintf("%s-swap-next-bioc-code-metrics.db", how), 99L)
    r <- .pub_publish(w)
    expect_false(identical(r$status, 0L), info = how)
    expect_false("swap-next-bioc-code-metrics.db" %in%
                   names(.pub_assets(w, "metrics-2026-09-13")), info = how)
    # Said out loud, in the same breath as the refusal, because bytes that were
    # uploaded are being taken away.
    expect_true(grepl("clearing swap-next-bioc-code-metrics.db", r$output, fixed = TRUE),
                info = paste(how, r$output))
    # The asset a reader asks for kept its name and its bytes throughout.
    expect_length(.pub_renames(w), 0L)
    expect_identical(.pub_read(w, "metrics-2026-09-13", "bioc-code-metrics.db")$output,
                     .pub_seeded_bytes("bioc-code-metrics.db", 4000L), info = how)
  }
})

test_that("a staging copy the run cannot clear is named by id for the operator", {
  # Best effort: the run is already failing, and the delete is one more call
  # that can get a 500. What it cannot do quietly is leave bytes nobody has
  # judged under a name the repair reads, so the refusal names the asset by id
  # and says what a later run does with it.
  w <- .pub_shard_world()
  .pub_fail(w, "short-swap-next-bioc-code-metrics.db", 99L)
  .pub_fail(w, "asset-delete", 99L)
  r <- .pub_publish(w)
  expect_false(identical(r$status, 0L))
  left <- .pub_assets(w, "metrics-2026-09-13")[["swap-next-bioc-code-metrics.db"]]
  expect_true(grepl(sprintf("asset %s", left$id), r$output, fixed = TRUE), info = r$output)
  expect_true(grepl("delete it by id", r$output, fixed = TRUE), info = r$output)
  # A delete it could not make is not a reason to touch the live asset.
  expect_length(.pub_renames(w), 0L)
  expect_identical(.pub_read(w, "metrics-2026-09-13", "bioc-code-metrics.db")$output,
                   .pub_seeded_bytes("bioc-code-metrics.db", 4000L))
})

test_that("the first rename failing leaves the release exactly as it was", {
  w <- .pub_shard_world()
  .pub_fail(w, "patch-swap-prev-bioc-code-metrics.db", 99L)
  before <- .pub_asset_line(w, "metrics-2026-09-13", "bioc-code-metrics.db")
  r <- .pub_publish(w)
  expect_false(identical(r$status, 0L))
  expect_identical(.pub_asset_line(w, "metrics-2026-09-13", "bioc-code-metrics.db"), before)
  expect_length(grep("swap-prev-bioc-code-metrics.db$", .pub_renames(w)), 5L)
  read <- .pub_read(w, "metrics-2026-09-13", "bioc-code-metrics.db")
  expect_identical(read$output, .pub_seeded_bytes("bioc-code-metrics.db", 4000L))
})

test_that("a rename that landed but reported failure is safe to make again", {
  # The retry names the same asset id, and renaming an asset to the name it
  # already holds is a 200 that changes nothing.
  w <- .pub_shard_world()
  .pub_fail(w, "patch-after-bioc-code-metrics.db", 1L)
  r <- .pub_publish(w)
  expect_identical(r$status, 0L, info = r$output)
  .pub_expect_published(w)
  expect_length(grep(" bioc-code-metrics.db$", .pub_renames(w)), 2L)
})

test_that("a second rename that never lands puts the live asset back under its name", {
  w <- .pub_shard_world()
  before <- .pub_assets(w, "metrics-2026-09-13")
  .pub_fail(w, "patch-bioc-code-metrics.db", 5L)
  r <- .pub_publish(w)
  expect_false(identical(r$status, 0L))
  # Five attempts at the second rename, then the rollback, all by asset id.
  expect_length(grep(" bioc-code-metrics.db$", .pub_renames(w)), 6L)
  expect_identical(.pub_assets(w, "metrics-2026-09-13")[["bioc-code-metrics.db"]]$id,
                   before[["bioc-code-metrics.db"]]$id)
  read <- .pub_read(w, "metrics-2026-09-13", "bioc-code-metrics.db")
  expect_identical(read$status, 0L, info = read$output)
  expect_identical(read$output, .pub_seeded_bytes("bioc-code-metrics.db", 4000L))
  # Nothing beyond the database was touched after it failed.
  expect_false(any(grepl("bioc-data-metrics", .pub_uploads(w), fixed = TRUE)))
})

test_that("the swap waits longer after each failed rename", {
  w <- .pub_shard_world()
  .pub_fail(w, "patch-swap-prev-bioc-code-metrics.db", 2L)
  .pub_fail(w, "patch-bioc-code-metrics.db", 2L)
  slept <- file.path(w$dir, "slept")
  r <- .pub_sh(w, c(sprintf("sleep() { echo \"$1\" >> %s; }", shQuote(slept)),
                    "replace_asset metrics-2026-09-13 2 out/bioc-code-metrics.db || exit 1"),
               wait = NA)
  expect_identical(r$status, 0L, info = r$output)
  expect_identical(readLines(slept), c("10", "20", "5", "10"))
})

test_that("a name that cannot be put back is restored from the previous copy next time", {
  # The one state that hurts: the release carries no bioc-code-metrics.db, so a
  # by-name read fails and the next run's preflight would refuse the release it
  # resolved. The repair at the start of the next replacement undoes it.
  w <- .pub_shard_world()
  before <- .pub_assets(w, "metrics-2026-09-13")
  .pub_fail(w, "patch-bioc-code-metrics.db", 6L)
  r <- .pub_publish(w)
  expect_false(identical(r$status, 0L))
  expect_true(grepl("swap-prev-bioc-code-metrics.db", r$output, fixed = TRUE), info = r$output)
  left <- names(.pub_assets(w, "metrics-2026-09-13"))
  expect_false("bioc-code-metrics.db" %in% left)
  expect_true(all(c("swap-prev-bioc-code-metrics.db", "swap-next-bioc-code-metrics.db") %in% left))
  expect_false(identical(.pub_read(w, "metrics-2026-09-13", "bioc-code-metrics.db")$status, 0L))

  file.remove(file.path(w$fails, "patch-bioc-code-metrics.db"))
  file.create(w$log)
  r <- .pub_publish(w)
  expect_identical(r$status, 0L, info = r$output)
  .pub_expect_published(w)
  # The copy that came back is the one that was live before any of this.
  renames <- .pub_renames(w)
  expect_identical(renames[[1L]],
                   sprintf("%s bioc-code-metrics.db", before[["bioc-code-metrics.db"]]$id))
  expect_identical(.pub_read(w, "metrics-2026-09-13", "bioc-code-metrics.db")$output,
                   .pub_bytes_of(w, "bioc-code-metrics.db"))
})

test_that("an upload cut off part way is cleared before the next attempt, by id", {
  # A killed upload leaves an asset in state "starter" at the full size. gh
  # cannot see it, so --clobber does not clear it, and it does not clear
  # itself.
  w <- .pub_shard_world(assets = list(
    .pub_asset("bioc-code-metrics.db", 4000L),
    .pub_asset("swap-next-bioc-code-metrics.db", 5000L, state = "starter", id = 900L),
    .pub_asset("bioc-data-metrics.db", 2000L),
    .pub_asset("code-manifest.json", 8L), .pub_asset("data-manifest.json", 9L)))
  r <- .pub_publish(w)
  expect_identical(r$status, 0L, info = r$output)
  .pub_expect_published(w)
  expect_identical(.pub_asset_deletes(w)[[1L]], "900")
})

test_that("a complete temporary asset left by an earlier run is cleared, not collided with", {
  # An upload onto a name that is taken is refused with 422 before the body is
  # read, so this is the leftover that would stop the run outright.
  w <- .pub_shard_world(assets = list(
    .pub_asset("bioc-code-metrics.db", 4000L),
    .pub_asset("swap-next-bioc-code-metrics.db", 5000L, id = 900L),
    .pub_asset("bioc-data-metrics.db", 2000L),
    .pub_asset("code-manifest.json", 8L), .pub_asset("data-manifest.json", 9L)))
  r <- .pub_publish(w)
  expect_identical(r$status, 0L, info = r$output)
  .pub_expect_published(w)
  expect_identical(.pub_asset_deletes(w)[[1L]], "900")
})

test_that("an asset left only under the temporary name is renamed into place", {
  # What the old ordering, deleting before uploading, leaves behind.
  w <- .pub_shard_world(assets = list(
    .pub_asset("swap-next-bioc-code-metrics.db", 4444L, id = 900L),
    .pub_asset("bioc-data-metrics.db", 2000L),
    .pub_asset("code-manifest.json", 8L), .pub_asset("data-manifest.json", 9L)))
  r <- .pub_sh(w, "repair_release metrics-2026-09-13 bioc-code-metrics.db || exit 1")
  expect_identical(r$status, 0L, info = r$output)
  expect_identical(.pub_renames(w), "900 bioc-code-metrics.db")
  expect_identical(.pub_asset_line(w, "metrics-2026-09-13", "bioc-code-metrics.db"),
                   "900 4444 uploaded")
  read <- .pub_read(w, "metrics-2026-09-13", "bioc-code-metrics.db")
  expect_identical(read$status, 0L, info = read$output)

  # And running it again changes nothing.
  file.create(w$log)
  expect_identical(.pub_sh(w, "repair_release metrics-2026-09-13 bioc-code-metrics.db || exit 1")$status, 0L)
  expect_length(.pub_renames(w), 0L)
  expect_length(.pub_asset_deletes(w), 0L)
})

test_that("a half-written asset with nothing under the name is cleared and the name filled again", {
  w <- .pub_shard_world(assets = list(
    .pub_asset("swap-next-bioc-code-metrics.db", 5000L, state = "starter", id = 900L),
    .pub_asset("bioc-data-metrics.db", 2000L),
    .pub_asset("code-manifest.json", 8L), .pub_asset("data-manifest.json", 9L)))
  r <- .pub_publish(w)
  expect_identical(r$status, 0L, info = r$output)
  .pub_expect_published(w)
  expect_identical(.pub_asset_deletes(w)[[1L]], "900")
  # There is nothing under the name to move aside, but the upload still goes up
  # under the temporary name and is measured there. Bytes uploaded straight
  # onto the name a reader asks for are bytes the run has to leave under it when
  # the measurement refuses them.
  expect_true("swap-next-bioc-code-metrics.db" %in% .pub_uploads(w))
  expect_false(any(grepl("upload metrics-2026-09-13 out/bioc-code-metrics.db",
                         .pub_calls(w), fixed = TRUE)))
  # One rename, because nothing had to be moved out of the way first.
  expect_identical(grep("bioc-code-metrics\\.db$", .pub_renames(w), value = TRUE),
                   sprintf("%s bioc-code-metrics.db",
                           .pub_assets(w, "metrics-2026-09-13")[["bioc-code-metrics.db"]]$id))
  expect_identical(.pub_read(w, "metrics-2026-09-13", "bioc-code-metrics.db")$output,
                   .pub_bytes_of(w, "bioc-code-metrics.db"))
})

test_that("a live name the release does not list as whole is cleared before anything else", {
  # An upload that was cut off sits under the name it was uploaded to, at the
  # full declared size and with no digest, and its bytes are not servable: a
  # download of it answers BlobNotFound. Reading that row as the live asset
  # deletes swap-prev-<name>, the only copy on this release a reader can be
  # served, and leaves the name on bytes nobody can download.
  #
  # "starter" is what a cut-off upload measures as, and the documented states,
  # uploaded and open, do not carry it, so the set is not closed and only
  # "uploaded" counts as whole.
  for (state in c("starter", "open")) {
    w <- .pub_shard_world(assets = list(
      .pub_asset("bioc-code-metrics.db", 5000L, state = state, id = 900L),
      .pub_asset("swap-prev-bioc-code-metrics.db", 4000L, id = 901L),
      .pub_asset("bioc-data-metrics.db", 2000L),
      .pub_asset("code-manifest.json", 8L), .pub_asset("data-manifest.json", 9L)))
    aside <- .pub_assets(w, "metrics-2026-09-13")[["swap-prev-bioc-code-metrics.db"]]
    r <- .pub_sh(w, "repair_release metrics-2026-09-13 bioc-code-metrics.db || exit 1")
    expect_identical(r$status, 0L, info = paste(state, r$output))
    # The copy that was not whole goes, and the one that was keeps its bytes
    # and takes the name back.
    expect_identical(.pub_asset_deletes(w), "900", info = state)
    expect_identical(.pub_renames(w), "901 bioc-code-metrics.db", info = state)
    expect_identical(.pub_asset_line(w, "metrics-2026-09-13", "bioc-code-metrics.db"),
                     "901 4000 uploaded", info = state)
    expect_identical(.pub_assets(w, "metrics-2026-09-13")[["bioc-code-metrics.db"]]$digest,
                     aside$digest, info = state)
    read <- .pub_read(w, "metrics-2026-09-13", "bioc-code-metrics.db")
    expect_identical(read$status, 0L, info = paste(state, read$output))
  }
})

test_that("a live name holding an upload that was cut off is cleared and filled again", {
  # Nothing is left to put the name back on, so there is nothing for the
  # replacement to move aside. The upload goes up under the temporary name all
  # the same, and only the copy the release has been asked about takes the name.
  w <- .pub_shard_world(assets = list(
    .pub_asset("bioc-code-metrics.db", 5000L, state = "starter", id = 900L),
    .pub_asset("bioc-data-metrics.db", 2000L),
    .pub_asset("code-manifest.json", 8L), .pub_asset("data-manifest.json", 9L)))
  r <- .pub_publish(w)
  expect_identical(r$status, 0L, info = r$output)
  .pub_expect_published(w)
  expect_identical(.pub_asset_deletes(w), "900")
  expect_true("swap-next-bioc-code-metrics.db" %in% .pub_uploads(w))
  expect_false(any(grepl("upload metrics-2026-09-13 out/bioc-code-metrics.db",
                         .pub_calls(w), fixed = TRUE)))
  expect_identical(grep("bioc-code-metrics\\.db$", .pub_renames(w), value = TRUE),
                   sprintf("%s bioc-code-metrics.db",
                           .pub_assets(w, "metrics-2026-09-13")[["bioc-code-metrics.db"]]$id))
  expect_identical(.pub_read(w, "metrics-2026-09-13", "bioc-code-metrics.db")$output,
                   .pub_bytes_of(w, "bioc-code-metrics.db"))
})

test_that("bytes a run refuses are never left under the name, with or without a copy beside them", {
  # The hazard the whole replacement exists to close, in the one state where a
  # release carries nothing under the name: the run uploads, measures those
  # bytes against the file they came from, refuses them, and the release is left
  # advertising them. A later repair walks past, because an asset that finished
  # uploading at the wrong size is still "uploaded".
  #
  # A cut-off upload under the live name is how a release reaches that state:
  # the repair clears it, and whatever the replacement does next has no live
  # copy to fall back on. The injected failure names both the live name and the
  # temporary one, so the upload lands a byte short whichever it goes up under.
  for (aside in c(FALSE, TRUE)) {
    assets <- list(
      .pub_asset("bioc-code-metrics.db", 5000L, state = "starter", id = 900L),
      .pub_asset("bioc-data-metrics.db", 2000L),
      .pub_asset("code-manifest.json", 8L), .pub_asset("data-manifest.json", 9L))
    # The displaced copy of an earlier replacement, beside the cut-off upload.
    if (aside) assets <- append(assets, list(.pub_asset("swap-prev-bioc-code-metrics.db",
                                                        4000L, id = 901L)))
    w <- .pub_shard_world(assets = assets)
    kept <- .pub_assets(w, "metrics-2026-09-13")[["swap-prev-bioc-code-metrics.db"]]$digest
    .pub_fail(w, "short-bioc-code-metrics.db", 99L)
    .pub_fail(w, "short-swap-next-bioc-code-metrics.db", 99L)
    r <- .pub_publish(w)
    expect_false(identical(r$status, 0L), info = r$output)

    left <- names(.pub_assets(w, "metrics-2026-09-13"))
    if (aside) {
      # The copy set aside took the name back before the upload, so it is what
      # a reader is served and the refused bytes never came near the name.
      expect_identical(.pub_asset_line(w, "metrics-2026-09-13", "bioc-code-metrics.db"),
                       "901 4000 uploaded")
      expect_identical(.pub_read(w, "metrics-2026-09-13", "bioc-code-metrics.db")$output,
                       sprintf("bioc-code-metrics.db 4000 %s", kept))
      expect_false("swap-next-bioc-code-metrics.db" %in% left)
    } else {
      # Nothing was left to put back, so the release carries no database at all,
      # which is what preflight refuses to build on and what the next run
      # replaces. What it must not carry is the copy this run measured.
      expect_false(any(grepl("bioc-code-metrics", left)), info = paste(left, collapse = ", "))
      read <- .pub_read(w, "metrics-2026-09-13", "bioc-code-metrics.db")
      expect_false(identical(read$status, 0L), info = read$output)
      expect_true(grepl("no assets match the file pattern", read$output, fixed = TRUE),
                  info = read$output)
    }

    # And the next run's repair finds nothing it could give the name to.
    file.create(w$log)
    r <- .pub_sh(w, "repair_release metrics-2026-09-13 bioc-code-metrics.db || exit 1")
    expect_identical(r$status, 0L, info = r$output)
    expect_length(.pub_renames(w), 0L)
    expect_identical(.pub_asset_line(w, "metrics-2026-09-13", "bioc-code-metrics.db"),
                     if (aside) "901 4000 uploaded" else NA_character_)
  }
})

test_that("a staging copy that was refused is never the copy a later repair promotes", {
  # The repair gives <name> to a finished swap-next-<name> when the release
  # carries no <name>, which is what saves a day whose run stopped after the
  # upload and before the swap. It cannot tell that copy apart from one a run
  # measured and refused, so the run that refused it is the only thing that can
  # keep it out of that answer.
  w <- .pub_shard_world()
  .pub_fail(w, "short-swap-next-bioc-code-metrics.db", 99L)
  expect_false(identical(.pub_publish(w)$status, 0L))

  # The release then loses the name itself, which is the state that makes the
  # repair reach for a staging copy.
  .pub_drop_asset(w, "metrics-2026-09-13", "bioc-code-metrics.db")
  file.create(w$log)
  r <- .pub_sh(w, "repair_release metrics-2026-09-13 bioc-code-metrics.db || exit 1")
  expect_identical(r$status, 0L, info = r$output)
  expect_length(.pub_renames(w), 0L)
  expect_length(.pub_asset_deletes(w), 0L)
  expect_false("bioc-code-metrics.db" %in% names(.pub_assets(w, "metrics-2026-09-13")))
  # Nobody is handed the refused bytes under the name: a reader asking for it
  # is told there is no such asset, which is what preflight refuses to build on
  # and what the next run replaces.
  read <- .pub_read(w, "metrics-2026-09-13", "bioc-code-metrics.db")
  expect_false(identical(read$status, 0L))
  expect_true(grepl("no assets match the file pattern", read$output, fixed = TRUE),
              info = read$output)
})

test_that("a run stopped between the renames is repaired before the release is read", {
  # The repair is what the download step runs on the release it resolved,
  # before it decides which databases that release carries.
  w <- .pub_shard_world(assets = list(
    .pub_asset("swap-prev-bioc-code-metrics.db", 4000L, id = 900L),
    .pub_asset("swap-next-bioc-code-metrics.db", 5000L, id = 901L),
    .pub_asset("bioc-data-metrics.db", 2000L),
    .pub_asset("code-manifest.json", 8L), .pub_asset("data-manifest.json", 9L)))
  r <- .pub_sh(w, "repair_release metrics-2026-09-13 bioc-code-metrics.db bioc-data-metrics.db || exit 1")
  expect_identical(r$status, 0L, info = r$output)
  # Rolled back, not rolled forward: the manifests were not swapped either, so
  # the old database and the old manifest are the pair the day started with.
  expect_identical(.pub_renames(w), "900 bioc-code-metrics.db")
  expect_identical(.pub_asset_deletes(w), "901")
  expect_identical(.pub_asset_line(w, "metrics-2026-09-13", "bioc-code-metrics.db"),
                   "900 4000 uploaded")
})

test_that("the repair has nothing to say about a release that never carried the asset", {
  # A legacy release carries one series, and a cold start has no release at all.
  w <- .pub_shard_world(assets = list(.pub_asset("bioc-code-metrics.db", 4000L)))
  r <- .pub_sh(w, c("repair_release metrics-2026-09-13 bioc-data-metrics.db data-manifest.json || exit 1",
                    'repair_release "" bioc-code-metrics.db || exit 1',
                    'echo "went on"'))
  expect_identical(r$status, 0L, info = r$output)
  expect_true(grepl("went on", r$output, fixed = TRUE))
  expect_length(.pub_renames(w), 0L)
  expect_length(.pub_asset_deletes(w), 0L)
  expect_length(grep("releases/tags/", .pub_calls(w), fixed = TRUE), 1L)
})

test_that("a release the repair cannot read stops the caller instead of reading as clean", {
  w <- .pub_shard_world()
  .pub_fail(w, "api-assets", 99L)
  r <- .pub_sh(w, c("repair_release metrics-2026-09-13 bioc-code-metrics.db || exit 1",
                    'echo "went on"'))
  expect_false(identical(r$status, 0L))
  expect_false(grepl("went on", r$output, fixed = TRUE))
  expect_length(grep("/assets", .pub_calls(w), fixed = TRUE), 5L)
})

# ---------------------------------------------------------------------------
# The heartbeat
# ---------------------------------------------------------------------------

.pub_heartbeat <- function(w, tag) {
  .pub_sh(w, sprintf(
    "refresh_heartbeat %s out/code-manifest.json out/data-manifest.json || exit 1",
    shQuote(tag)))
}

test_that("the heartbeat swaps each manifest in rather than deleting the live one", {
  # This one writes to the release the whole catalogue is being read from, on
  # every day that publishes nothing, which is most days between Bioconductor
  # releases.
  w <- .pub_world(list(.pub_prior()))
  before <- .pub_assets(w, "metrics-2026-09-12")
  r <- .pub_heartbeat(w, "metrics-2026-09-12")
  expect_identical(r$status, 0L, info = r$output)
  expect_identical(.pub_uploads(w), c("swap-next-code-manifest.json", "swap-next-data-manifest.json"))
  expect_length(.pub_asset_deletes(w), 0L)
  after <- .pub_assets(w, "metrics-2026-09-12")
  expect_equal(as.numeric(after[["code-manifest.json"]]$size), 120)
  expect_identical(after[["swap-prev-code-manifest.json"]]$id, before[["code-manifest.json"]]$id)
  # The databases are not touched at all.
  expect_equal(as.numeric(after[["bioc-code-metrics.db"]]$size), 4000)
  expect_identical(after[["bioc-code-metrics.db"]]$id, before[["bioc-code-metrics.db"]]$id)
  expect_identical(.pub_read(w, "metrics-2026-09-12", "code-manifest.json")$output,
                   .pub_bytes_of(w, "code-manifest.json"))
  expect_false(any(grepl("^gh release (create|delete|edit) ", .pub_calls(w))))
})

test_that("the heartbeat puts back a manifest an earlier one left half-swapped", {
  # Between Bioconductor releases this is the only thing writing to the release
  # the merger reads, so a heartbeat interrupted between the two renames would
  # otherwise leave it without a manifest until something published again.
  prior <- .pub_prior()
  prior$assets <- list(.pub_asset("bioc-code-metrics.db", 4000L),
                       .pub_asset("bioc-data-metrics.db", 2000L),
                       .pub_asset("swap-prev-code-manifest.json", 8L, id = 900L),
                       .pub_asset("swap-next-code-manifest.json", 120L, id = 901L),
                       .pub_asset("data-manifest.json", 9L))
  w <- .pub_world(list(prior))
  r <- .pub_heartbeat(w, "metrics-2026-09-12")
  expect_identical(r$status, 0L, info = r$output)
  expect_identical(.pub_renames(w)[[1L]], "900 code-manifest.json")
  expect_true("901" %in% .pub_asset_deletes(w))
  after <- .pub_assets(w, "metrics-2026-09-12")
  expect_equal(as.numeric(after[["code-manifest.json"]]$size), 120)
  expect_identical(.pub_read(w, "metrics-2026-09-12", "code-manifest.json")$output,
                   .pub_bytes_of(w, "code-manifest.json"))
})

test_that("a manifest the release never carried goes up under the temporary name too", {
  # The legacy code-/data- releases carry one series each, so there is nothing
  # to move aside under a name the release does not have. The upload is still
  # measured under the temporary name before it takes that name, because bytes
  # a check refuses can only be taken away again from a name nothing reads.
  prior <- .pub_prior()
  prior$assets <- list(.pub_asset("bioc-code-metrics.db", 4000L),
                       .pub_asset("code-manifest.json", 8L))
  w <- .pub_world(list(prior))
  r <- .pub_heartbeat(w, "metrics-2026-09-12")
  expect_identical(r$status, 0L, info = r$output)
  expect_identical(.pub_uploads(w),
                   c("swap-next-code-manifest.json", "swap-next-data-manifest.json"))
  after <- .pub_assets(w, "metrics-2026-09-12")
  expect_equal(as.numeric(after[["data-manifest.json"]]$size), 140)
  # One rename for the name that was free, two for the one that was not.
  expect_identical(grep("data-manifest\\.json$", .pub_renames(w), value = TRUE),
                   sprintf("%s data-manifest.json", after[["data-manifest.json"]]$id))
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
  .pub_fail(w, "upload-swap-next-data-manifest.json", 99L)
  r <- .pub_heartbeat(w, "metrics-2026-09-12")
  expect_false(identical(r$status, 0L))
  expect_identical(sum(.pub_uploads(w) == "swap-next-data-manifest.json"), 5L)
  # The manifest a reader asks for is the one that was there before.
  expect_equal(as.numeric(.pub_assets(w, "metrics-2026-09-12")[["data-manifest.json"]]$size), 9)
})

test_that("the heartbeat reads the release again when a read fails once", {
  # Most days publish nothing and end here. A heartbeat that goes red leaves
  # last_checked where it was, and the merger's readiness gate reads freshness
  # from last_checked, so one 5xx would make this pipeline look late.
  w <- .pub_world(list(.pub_prior()))
  for (what in c("list", "view", "api-tags", "api-assets")) .pub_fail(w, what, 1L)
  r <- .pub_heartbeat(w, "metrics-2026-09-12")
  expect_identical(r$status, 0L, info = r$output)
  expect_identical(.pub_uploads(w), c("swap-next-code-manifest.json", "swap-next-data-manifest.json"))
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
  .pub_fail(w, "short-swap-next-code-manifest.json", 1L)
  r <- .pub_heartbeat(w, "metrics-2026-09-12")
  expect_false(identical(r$status, 0L))
  expect_true(grepl("swap-next-code-manifest.json is [uploaded 119", r$output, fixed = TRUE),
              info = r$output)
  expect_true(grepl("wanted [uploaded 120", r$output, fixed = TRUE), info = r$output)
  expect_length(.pub_renames(w), 0L)
})

test_that("the heartbeat names a missing manifest before it touches the release", {
  # The manifests go up one at a time, so the one ahead of a missing one would
  # have refreshed last_checked for its own series alone before the step failed.
  w <- .pub_world(list(.pub_prior()))
  .pub_gnu_stat(w)
  unlink(file.path(w$dir, "out", "data-manifest.json"))
  r <- .pub_heartbeat(w, "metrics-2026-09-12")
  expect_false(identical(r$status, 0L))
  expect_true(grepl("::error::out/data-manifest.json does not exist", r$output, fixed = TRUE),
              info = r$output)
  expect_false(grepl("invalid option", r$output, fixed = TRUE))
  expect_length(.pub_calls(w), 0L)
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

test_that("the prune clears the copies a replacement left on the releases it keeps", {
  # Tomorrow publishes under a new tag and never comes back to today's, so the
  # copy each replacement sets aside would sit on every kept release for good:
  # one extra database per release-day. The prune is the only step that visits
  # a release again after its day.
  w <- .pub_world(list(
    .pub_release(1L, "metrics-2026-09-12", assets = list(
      .pub_asset("bioc-code-metrics.db", 4000L),
      .pub_asset("swap-prev-bioc-code-metrics.db", 3900L, id = 900L),
      .pub_asset("code-manifest.json", 8L),
      .pub_asset("swap-next-code-manifest.json", 9L, state = "starter", id = 901L))),
    .pub_release(2L, "metrics-2026-09-13", assets = list(
      .pub_asset("bioc-code-metrics.db", 4100L),
      .pub_asset("swap-prev-bioc-code-metrics.db", 4000L, id = 902L))),
    .pub_release(3L, "code-2026-01-01", assets = list(
      .pub_asset("swap-prev-bioc-code-metrics.db", 10L, id = 903L)))))
  r <- .pub_sh(w, "sweep_swap_leftovers metrics metrics-2026-09-13 || exit 1")
  expect_identical(r$status, 0L, info = r$output)
  expect_setequal(.pub_asset_deletes(w), c("900", "901"))
  # Today's release is left alone, because a publish may still be part way
  # through a replacement on it, and another series is not this one's business.
  expect_identical(names(.pub_assets(w, "metrics-2026-09-13")),
                   c("bioc-code-metrics.db", "swap-prev-bioc-code-metrics.db"))
  expect_identical(names(.pub_assets(w, "code-2026-01-01")), "swap-prev-bioc-code-metrics.db")
  expect_identical(names(.pub_assets(w, "metrics-2026-09-12")),
                   c("bioc-code-metrics.db", "code-manifest.json"))
})

test_that("the prune restores a name rather than stripping the copy that holds it", {
  # A release left between the two renames carries the bytes only under swap-prev-.
  # Deleting that as a leftover is exactly the loss this is meant to prevent.
  w <- .pub_world(list(
    .pub_release(1L, "metrics-2026-09-12", assets = list(
      .pub_asset("swap-prev-bioc-code-metrics.db", 4000L, id = 900L),
      .pub_asset("swap-next-bioc-code-metrics.db", 5000L, id = 901L))),
    .pub_release(2L, "metrics-2026-09-13")))
  r <- .pub_sh(w, "sweep_swap_leftovers metrics metrics-2026-09-13 || exit 1")
  expect_identical(r$status, 0L, info = r$output)
  expect_identical(.pub_renames(w), "900 bioc-code-metrics.db")
  expect_identical(.pub_asset_deletes(w), "901")
  read <- .pub_read(w, "metrics-2026-09-12", "bioc-code-metrics.db")
  expect_identical(read$status, 0L, info = read$output)
})

test_that("a sweep that cannot read a release stops the step", {
  w <- .pub_world(list(.pub_prior(), .pub_release(2L, "metrics-2026-09-13")))
  .pub_fail(w, "api-assets", 99L)
  r <- .pub_sh(w, c("sweep_swap_leftovers metrics metrics-2026-09-13 || exit 1",
                    'echo "went on past the copies"'))
  expect_false(identical(r$status, 0L))
  expect_false(grepl("went on past the copies", r$output, fixed = TRUE))
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

# The helpers in scripts/publish.sh that can fail, so a call to one has to be
# guarded exactly like a gh call.
.PUB_HELPERS <- paste0(
  "release_state|release_rows|release_id|release_assets|upload_asset|verify_assets|",
  "verify_asset|edit_release|file_bytes|file_sha256|rename_asset|delete_asset|",
  "repair_asset|repair_release|swap_asset|replace_asset")

test_that("every gh call in scripts/publish.sh stops the function when it fails", {
  # A function called as `f || exit 1` runs with errexit off, so a failed gh
  # call inside it is ignored unless the line itself returns. The same holds
  # for a call to one of the script's own helpers that can fail.
  gh <- .pub_gh_calls(.pub_script())
  expect_true(length(gh) > 0L)
  lines <- .pub_logical_lines(.pub_script())
  lines <- lines[!grepl("^\\s*#", lines)]
  helpers <- grep(sprintf("\\b(%s)[ )]", .PUB_HELPERS), lines, value = TRUE, perl = TRUE)
  expect_gte(length(helpers), 20L)
  expect_true(any(grepl("gh api ", gh, fixed = TRUE)))
  calls <- c(gh, helpers)
  # Either the line returns when the call fails, or the call is the condition
  # of an if, where its status is read rather than dropped.
  guarded <- grepl("\\|\\| return 1", calls) |
    grepl(sprintf("^\\s*if !? ?([a-z_]+=\\$\\()?(gh|%s) ", .PUB_HELPERS), calls)
  expect_true(all(guarded), info = paste(calls[!guarded], collapse = "\n"))
})

test_that("nothing in scripts/publish.sh clobbers an asset a reader asks for by name", {
  # --clobber deletes the live asset before uploading its replacement. It is
  # only safe where no reader can be asking for the name: an empty draft, and
  # the temporary name a replacement uploads under.
  lines <- .pub_logical_lines(.pub_script())
  lines <- lines[!grepl("^\\s*#", lines)]
  uploads <- grep("gh release upload ", lines, value = TRUE, fixed = TRUE)
  expect_length(uploads, 1L)
  expect_true(grepl("--clobber", uploads, fixed = TRUE))
  # The one upload takes whatever path it is handed, and both callers of the
  # swap hand it a name nothing reads.
  sh <- paste(lines, collapse = "\n")
  swap <- regmatches(sh, regexpr("(?s)replace_asset\\(\\) \\{.*?\n\\}", sh, perl = TRUE))
  expect_length(swap, 1L)
  expect_true(grepl("swap-next-", swap, fixed = TRUE))
  expect_true(grepl("repair_asset", swap, fixed = TRUE))
  # One upload, under the temporary name, however little the release carries
  # under the real one. An upload onto the name a reader asks for is one the
  # run cannot take back when it measures those bytes and refuses them.
  ups <- grep("upload_asset ", strsplit(swap, "\n", fixed = TRUE)[[1L]], value = TRUE)
  expect_length(ups, 1L)
  expect_true(grepl("swap-next-", ups, fixed = TRUE), info = ups)
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

test_that("the prune also clears the copies a replacement leaves on the releases it keeps", {
  yml <- readLines(file.path("..", "..", ".github", "workflows", "update.yml"))
  start <- grep("- name: Prune old dated releases", yml, fixed = TRUE)
  prune <- yml[start:length(yml)]
  expect_true(any(grepl('sweep_swap_leftovers metrics "metrics-$(date -u +%Y-%m-%d)" || exit 1',
                        prune, fixed = TRUE)))
})

test_that("the download step repairs the release it resolved before reading what it carries", {
  # A replacement stopped between its two renames leaves the release without
  # the asset under its plain name. Nothing else in a later run looks at that:
  # the download step would see a release carrying no database and preflight
  # would refuse it, which is the stranding this whole file is about.
  yml <- paste(.pub_logical_lines(file.path("..", "..", ".github", "workflows", "update.yml")),
               collapse = "\n")
  repairs <- regmatches(yml, gregexpr("repair_release [^\n]*", yml))[[1L]]
  expect_gte(length(repairs), 2L)
  expect_true(all(grepl("|| exit 1", repairs, fixed = TRUE)), info = paste(repairs, collapse = "\n"))
  for (name in c("bioc-code-metrics.db", "bioc-data-metrics.db",
                 "code-manifest.json", "data-manifest.json")) {
    expect_true(any(grepl(name, repairs, fixed = TRUE)), info = name)
  }
  # Before the step decides which series the release advertises.
  expect_lt(min(gregexpr("repair_release ", yml, fixed = TRUE)[[1L]]),
            min(gregexpr("list_assets \"$CODE_SRC\"", yml, fixed = TRUE)[[1L]]))
})

test_that("update.yml tells preflight which releases it resolved", {
  yml <- paste(readLines(file.path("..", "..", ".github", "workflows", "update.yml")),
               collapse = "\n")
  call <- regmatches(yml, regexpr("Rscript scripts/preflight.R[^\n]*", yml))
  expect_length(call, 1L)
  expect_true(grepl("--code-src=\"$CODE_SRC\"", call, fixed = TRUE))
  expect_true(grepl("--data-src=\"$DATA_SRC\"", call, fixed = TRUE))
})
