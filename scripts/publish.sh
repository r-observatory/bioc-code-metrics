# scripts/publish.sh: find the release a run builds on, and publish the one it
# produces. Sourced by the download step and the shard step of update.yml.
# shellcheck shell=bash
#
# The sibling cran-code-metrics pipeline carried exactly this code until it
# stopped on 2026-09-13. `gh release create TAG <assets> --latest` is several
# API calls: it creates a draft, uploads the assets into it and then publishes
# it. Both database uploads got HTTP 500, gh's own delete of the draft got a
# 500 as well, and a draft was left under the new tag holding only the two
# manifests. `gh release list` returns drafts to a token that can push, sorted
# in among the real releases, so every run after that resolved the draft as
# the prior release, downloaded two manifests and no database, and refused.
# Here it would have been worse: a draft holding no assets at all advertises
# nothing, which reads as a cold start, and the run would have published one
# shard's worth of packages as latest.
#
# So a draft is never something to build on, never something to upload into,
# and a publish that fails partway leaves a draft that nothing resolves. A
# later publish under the same tag, which means the same day, deletes it; once
# the day has passed, the prune step does (delete_stale_drafts).
#
# Every gh call inside these functions ends in `|| return 1`, or sits in an
# `if`, on purpose. The workflow calls them as `f || exit 1`, and bash ignores
# `set -e` for the whole body of a function called on the left of `||`, so an
# unguarded failed upload followed by a successful edit returned 0 and left
# the step green over a release missing its database.

# Bytes in a file, on the runner's GNU stat or the BSD stat a Mac has.
#
# A missing file is named before either stat is asked. GNU stat reads the BSD
# form as a usage error, so on the runner a missing file used to print nothing
# but "stat: invalid option -- '%'". The error goes to stderr, because callers
# read the size from stdout.
file_bytes() {
  if [ ! -f "$1" ]; then
    echo "::error::$1 does not exist." >&2
    return 1
  fi
  stat -c%s "$1" 2>/dev/null || stat -f%z "$1"
}

# Wait before attempt $1 + 1, at $2 seconds for each attempt already made.
# PUBLISH_RETRY_WAIT_S replaces $2, and only exists so the tests do not sleep.
publish_backoff() { sleep $(($1 * ${PUBLISH_RETRY_WAIT_S:-$2})); }

# The newest PUBLISHED release of a series ("metrics", or the legacy "code" and
# "data"), or nothing when the series has none. A draft is what an interrupted
# publish leaves, so it is not a prior release whatever its tag says. A failed
# listing fails the call rather than answering empty, because empty is what a
# cold start looks like.
#
# This is the first call of every run, so it is read up to five times like
# release_state's listing, for the same reason: one GraphQL 500 here stopped a
# day that had nothing to publish before its heartbeat. The tag is this
# function's stdout, so the attempt messages go to stderr.
latest_tag() {
  local tags n
  for n in 1 2 3 4 5; do
    if tags=$(gh release list --exclude-drafts --limit 1000 --json tagName -q '.[].tagName'); then
      printf '%s\n' "$tags" | { grep "^$1-" || true; } | sort -r | head -n 1
      return 0
    fi
    echo "attempt ${n}: could not list the releases to find the newest $1 release" >&2
    if [ "$n" -lt 5 ]; then publish_backoff "$n" 10; fi
  done
  echo "::error::five attempts failed to list the releases; cannot tell whether a $1 release exists." >&2
  return 1
}

# "published", "draft", nothing when no release carries the tag, or one line
# per release when more than one does. GitHub does not stop a draft from
# sharing a tag with another release, and gh then picks whichever lookup
# answers first, so that case has no safe reading.
#
# The listing is read up to five times, 10 s, then 20 s and so on apart. Most
# days publish nothing and only get as far as the heartbeat, and a single
# GraphQL 500 here used to turn that run red and leave last_checked where it
# was, which the merger reads as a late pipeline. The answer is this function's
# stdout, so the attempt messages go to stderr.
release_state() {
  local tag="$1" n rows
  for n in 1 2 3 4 5; do
    if rows=$(gh release list --limit 1000 --json tagName,isDraft \
                -q ".[] | select(.tagName == \"${tag}\") | if .isDraft then \"draft\" else \"published\" end"); then
      printf '%s\n' "$rows"
      return 0
    fi
    echo "attempt ${n}: could not list the releases to find ${tag}" >&2
    if [ "$n" -lt 5 ]; then publish_backoff "$n" 10; fi
  done
  echo "::error::five attempts failed to list the releases; cannot tell whether ${tag} exists." >&2
  return 1
}

# Every release as "<id> <tag> draft|published", newest first. REST rather
# than `gh release list`, which has no id to give. An id is the only safe way
# to name one of two releases under the same tag: `gh release delete TAG`
# looks the tag up as a published release and as a draft at the same time and
# acts on whichever answer comes back first.
release_rows() {
  gh api "repos/{owner}/{repo}/releases?per_page=100" --paginate \
    -q '.[] | "\(.id) \(.tag_name) \(if .draft then "draft" else "published" end)"' || return 1
}

# "  id <id>: draft|published" for each release under TAG, for the operator.
release_ids() {
  local rows id t kind
  rows=$(release_rows) || return 1
  while read -r id t kind; do
    if [ "$t" = "$1" ]; then echo "  id ${id}: ${kind}"; fi
  done <<< "$rows"
}

# Refuse a tag that more than one release carries, and tell the operator how to
# clear it without a delete by tag, naming each release under it by id. $2 is
# what is being refused. Always returns 1.
refuse_doubled_tag() {
  echo "::error::more than one release is named $1; $2 Delete the draft ones by id, with \`gh api -X DELETE repos/{owner}/{repo}/releases/<id>\`, then re-run. A delete by tag cannot choose between them and can take the published one."
  release_ids "$1" ||
    echo "  could not list their ids; \`gh api 'repos/{owner}/{repo}/releases?per_page=100' --paginate\` shows them."
  return 1
}

# Upload one file to a release, replacing an asset of the same name. gh makes
# the delete that --clobber sends first only once, then tries the upload four
# times 200 ms apart. A GitHub incident lasting minutes outlives both, so this
# waits 30 s, then 60 s, and so on between five attempts of the whole call.
upload_asset() {
  local tag="$1" file="$2" n
  for n in 1 2 3 4 5; do
    if gh release upload "$tag" "$file" --clobber; then
      return 0
    fi
    echo "attempt ${n}: $(basename "$file") did not upload to ${tag}"
    if [ "$n" -lt 5 ]; then publish_backoff "$n" 30; fi
  done
  echo "::error::five attempts failed to upload $(basename "$file") to ${tag}."
  return 1
}

# Check that a release carries every file, by name and size, as a finished
# upload. An upload that returned success is not the same thing as an asset
# that landed whole, and it is the asset that the next run downloads.
#
# The read is tried five times like the listing. It comes after every upload
# has landed, so one 502 on it would otherwise throw away a whole day's
# databases and leave the release an unpublished draft.
#
# A read that disagrees is read again, like one that failed. Nothing promises
# that a release lists an asset the moment its upload returns, and refusing on
# the first read that lags leaves a complete draft unpublished, so the next run
# repeats the day's analysis. Only the last of five reads decides, so an asset
# that really landed short takes about a hundred seconds longer to refuse.
verify_assets() {
  local tag="$1" got f want wrong n
  shift
  for n in 1 2 3 4 5; do
    wrong=""
    if got=$(gh release view "$tag" --json assets \
               -q '.assets[] | select(.state == "uploaded") | "\(.name) \(.size)"'); then
      for f in "$@"; do
        want="$(basename "$f") $(file_bytes "$f")" || return 1
        if ! printf '%s\n' "$got" | grep -qxF "$want"; then
          wrong="$want"
          break
        fi
      done
      if [ -z "$wrong" ]; then return 0; fi
      echo "attempt ${n}: ${tag} does not list ${wrong} yet"
    else
      got=""
      echo "attempt ${n}: could not read the assets of ${tag}"
    fi
    if [ "$n" -lt 5 ]; then publish_backoff "$n" 10; fi
  done
  if [ -z "$wrong" ]; then
    echo "::error::five attempts failed to read back the assets of ${tag}."
  else
    echo "::error::${tag} does not carry ${wrong} after the upload; it lists: $(printf '%s' "$got" | tr '\n' ',')"
  fi
  return 1
}

# Edit TAG with the gh release edit flags that follow, up to five times, 10 s,
# then 20 s and so on apart.
#
# Both edits the publish makes come after every asset has landed and been
# checked, so a single 5xx on one would otherwise throw that upload away. Each
# is one PATCH that sets the same fields however many times it lands, so
# repeating one that returned 500 and applied anyway changes nothing: gh finds
# the release by its tag again and sets the fields again.
edit_release() {
  local tag="$1" n
  shift
  for n in 1 2 3 4 5; do
    if gh release edit "$tag" "$@"; then
      return 0
    fi
    echo "attempt ${n}: could not edit ${tag}"
    if [ "$n" -lt 5 ]; then publish_backoff "$n" 10; fi
  done
  echo "::error::five attempts failed to edit ${tag}."
  return 1
}

# Publish TAG with TITLE and the notes in NOTES, carrying the files that follow.
#
# A tag with no release gets an empty draft, the files one at a time, a check
# of what landed, and only then the edit that publishes it as latest. A draft
# already under the tag is left from an earlier attempt that failed somewhere
# in that sequence; it can hold any mix of assets, so it is deleted and the
# sequence starts again rather than being uploaded into. A published release is
# an earlier shard of this same run, and its assets are replaced in place.
#
# The create is never retried here. A POST that returns 500 can still have
# created the release, and retrying could leave two drafts under one tag; a
# later publish under the same tag finds the one and replaces it. Nor is the
# delete of a draft, which goes by tag: gh looks the tag up as a published
# release and as a draft at the same time and acts on whichever answer comes
# back first (release_rows). So when the listing has not caught up with a
# release published under the same tag, the delete can take that release
# instead, and each repeat is another chance to. A draft the one attempt leaves
# is deleted by the next publish under the tag, or by delete_stale_drafts once
# the day has passed. The listing, the uploads, the read-back and the edits
# land the same however often they run, and each is retried.
#
# Databases go up before manifests whatever order they are passed in. The
# replacement is not atomic, and a manifest newer than the database beside it
# is what preflight.R refuses as lost rows, while a database newer than its
# manifest costs nothing.
publish_release() {
  local tag="$1" title="$2" notes="$3" state f
  shift 3
  if [ "$#" -eq 0 ]; then
    echo "::error::nothing to publish to ${tag}."
    return 1
  fi
  # Every file is here before the release is touched. The size check in
  # update.yml measures only the databases, so a missing manifest got as far as
  # a draft holding both of them and five failed uploads, about five minutes of
  # backoff, before anything said which file it was.
  for f in "$@"; do
    file_bytes "$f" > /dev/null || return 1
  done
  local ordered=()
  for f in "$@"; do case "$f" in *.db) ordered+=("$f") ;; esac; done
  for f in "$@"; do case "$f" in *.db) ;; *) ordered+=("$f") ;; esac; done

  state=$(release_state "$tag") || return 1
  case "$state" in
    ""|published) ;;
    draft)
      echo "::warning::${tag} is a draft an earlier publish left unfinished; deleting it and publishing again."
      gh release delete "$tag" --yes || return 1
      state="" ;;
    *)
      refuse_doubled_tag "$tag" "refusing to publish into it."
      return 1 ;;
  esac

  if [ -z "$state" ]; then
    gh release create "$tag" --draft --title "$title" --notes-file "$notes" || return 1
  fi
  for f in "${ordered[@]}"; do
    upload_asset "$tag" "$f" || return 1
  done
  verify_assets "$tag" "${ordered[@]}" || return 1
  if [ -z "$state" ]; then
    edit_release "$tag" --draft=false --latest || return 1
  else
    edit_release "$tag" --title "$title" --notes-file "$notes" || return 1
  fi
}

# Put the freshness manifests back on the release the run built on, when the
# run published nothing.
#
# The universe is keyed on the Bioconductor RELEASE, which moves twice a year,
# so between releases every daily run correctly finds nothing to do and
# publishes nothing. Consumers that read freshness off the latest release would
# then watch this pipeline appear to die for months. Refreshing the two small
# manifests on that release lets last_checked advance daily while last_changed
# stays at the moment the data really moved. No database is re-uploaded.
#
# TAG is whatever the download step resolved, and empty on a cold start. Only a
# published release is written to: a draft is never the release anyone reads
# freshness from, and putting new manifests into one makes it look like a
# release that finished.
refresh_heartbeat() {
  local tag="$1" state f
  shift
  if [ -z "$tag" ]; then
    echo "No prior release to carry a heartbeat; nothing to refresh."
    return 0
  fi
  # Every manifest is here before the release is read. They go up one at a
  # time, so a manifest ahead of a missing one would otherwise refresh
  # last_checked for its own series alone before the step failed.
  for f in "$@"; do
    file_bytes "$f" > /dev/null || return 1
  done
  state=$(release_state "$tag") || return 1
  case "$state" in
    published) ;;
    draft)
      echo "::error::${tag} is a draft, not a published release; refusing to put the freshness manifests into it."
      return 1 ;;
    "")
      echo "::error::no release is named ${tag} any more; refusing to refresh the freshness manifests on it."
      return 1 ;;
    *)
      refuse_doubled_tag "$tag" "refusing to guess which one carries the heartbeat."
      return 1 ;;
  esac
  echo "Nothing published this run; refreshing the freshness manifests on ${tag}."
  for f in "$@"; do
    upload_asset "$tag" "$f" || return 1
  done
  verify_assets "$tag" "$@" || return 1
}

# Delete the drafts in a series that no publish will come back for.
#
# publish_release replaces a draft only under the tag it is publishing, and the
# prune lists without drafts. This pipeline runs once a day, so a publish that
# fails in a day's last run leaves its draft under a tag nothing uses again,
# holding up to both databases, for good. The prune step runs this only after
# the shard step succeeded, whether that step published or refreshed the
# heartbeat, and under the workflow's concurrency group, so no publish is part
# way through a draft. Today's tag is skipped all the same: the next publish
# today replaces that draft itself.
#
# By id, for the reason release_rows gives: a draft can share its tag with a
# published release. A delete that fails is left for the next scheduled run. A
# listing that fails stops the step, as the prune's own listing does, rather
# than reading as no drafts.
delete_stale_drafts() {
  local rows id t kind
  rows=$(release_rows) || return 1
  while read -r id t kind; do
    case "$t" in "$1"-*) ;; *) continue ;; esac
    if [ "$kind" != draft ] || [ "$t" = "$2" ]; then continue; fi
    echo "deleting the draft ${t} (release ${id}), left by a publish that did not finish"
    if ! gh api -X DELETE "repos/{owner}/{repo}/releases/${id}"; then
      echo "::warning::could not delete the draft ${t} (release ${id}); the next scheduled run tries again."
    fi
  done <<< "$rows"
}
