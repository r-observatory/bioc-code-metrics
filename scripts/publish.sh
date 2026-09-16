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
# A publish under a tag that already carries a release has the same shape of
# problem one layer down. `gh release upload --clobber` deletes the live asset
# and then uploads its replacement, so the release advertises no database for
# the length of the upload, and for good if every attempt fails: the next run
# resolves that release, finds no database, and preflight refuses to build on
# it. Every asset on a published release therefore goes up under a name of its
# own, swap-next-<name>, and is renamed into place (replace_asset), which
# leaves the live copy readable throughout and narrows the gap to two
# renames. Every state a run
# interrupted in the middle of that can leave is repaired, by id, at the start
# of the next replacement, by the download step before it reads what a release
# carries, and by the prune (repair_asset, repair_release,
# sweep_swap_leftovers).
#
# What those calls do was measured against a scratch repository rather than
# read from the API docs, and the comments below say what was measured where it
# decided something.
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

# The sha256 of a file, as the 64 hex characters GitHub reports in an uploaded
# asset's digest. GNU coreutils calls it sha256sum and the BSD tools a Mac has
# call it shasum; both print "<hash>  <path>" and both follow a symlink, so
# this measures the bytes an upload of that path would send.
file_sha256() {
  if [ ! -f "$1" ]; then
    echo "::error::$1 does not exist." >&2
    return 1
  fi
  { sha256sum "$1" 2>/dev/null || shasum -a 256 "$1"; } | cut -d' ' -f1
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

# The numeric id of the PUBLISHED release a tag names.
#
# Everything that works on one asset is keyed on an asset id, and the listing
# that hands those out is keyed on this one. The tag lookup answers for
# published releases only, which is what every caller here wants: a draft is
# never a release anyone reads, and never one this repairs.
#
# Read up to five times like every other listing, for the same reason: one 500
# must not stop a day that had nothing to publish before its heartbeat. The id
# is this function's stdout, so the attempt messages go to stderr.
release_id() {
  local tag="$1" id n
  for n in 1 2 3 4 5; do
    if id=$(gh api "repos/{owner}/{repo}/releases/tags/${tag}" -q .id); then
      printf '%s\n' "$id"
      return 0
    fi
    echo "attempt ${n}: could not read the release ${tag}" >&2
    if [ "$n" -lt 5 ]; then publish_backoff "$n" 10; fi
  done
  echo "::error::five attempts failed to read the release ${tag}." >&2
  return 1
}

# Every asset of a release as "<id> <state> <size> <digest> <name>", ordered by
# name, from the release's own assets endpoint.
#
# This is the only listing that shows an upload that was cut off part way. Such
# an asset sits there as state "starter", already at the full declared size and
# with no digest, and the tag listing that `gh release view`, `gh release
# download` and `gh release upload --clobber` all read leaves it out, so
# nothing that goes by tag can see it, clear it, or be stopped by it. It does
# not clear itself either. A digest the API does not report reads as "none".
#
# The rows are this function's stdout, so the attempt messages go to stderr.
release_assets() {
  local rel="$1" rows n
  for n in 1 2 3 4 5; do
    if rows=$(gh api "repos/{owner}/{repo}/releases/${rel}/assets?per_page=100" --paginate \
                -q '.[] | "\(.id) \(.state) \(.size) \(.digest // "none") \(.name)"'); then
      printf '%s\n' "$rows"
      return 0
    fi
    echo "attempt ${n}: could not list the assets of release ${rel}" >&2
    if [ "$n" -lt 5 ]; then publish_backoff "$n" 10; fi
  done
  echo "::error::five attempts failed to list the assets of release ${rel}." >&2
  return 1
}

# "<id> <state> <size> <digest>" for the asset named $2 in the rows $1, or
# nothing when the release carries no asset of that name.
asset_row() {
  printf '%s\n' "$1" | awk -v n="$2" '$5 == n { print $1, $2, $3, $4; exit }'
}

# Give asset $1 the name $2. One PATCH, and no retry here: what a failure means
# depends on which half of a swap it was, so the caller decides.
#
# A rename changes the name and nothing else. The id, the bytes, the size, the
# digest, the content type and the download count all stay, and a download by
# name follows the new name. Renaming an asset to the name it already holds is
# a 200 that changes nothing, which is what makes repeating a PATCH that landed
# and then reported failure safe. A name another asset of the same release
# holds is refused with 422, and names are compared case-insensitively.
rename_asset() {
  gh api -X PATCH "repos/{owner}/{repo}/releases/assets/$1" -f "name=$2" --silent || return 1
}

# Delete asset $1.
#
# This cuts off a download of that asset that is already running: measured
# against a scratch repository, a reader 40 MiB into a 300 MiB asset had its
# stream closed about three seconds after the delete returned, and was left
# with a truncated file. The merger's download of a database this size takes
# tens of seconds, so nothing here deletes an asset that a reader can still be
# asking for by name.
delete_asset() {
  gh api -X DELETE "repos/{owner}/{repo}/releases/assets/$1" --silent || return 1
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
#
# --clobber deletes the live asset before the upload that replaces it starts,
# so this is only called where nothing can be asking for the name: into an
# empty draft, or under the temporary name a replacement uploads to
# (replace_asset). The asset is named after the basename of the path, and gh
# follows a symlink for the bytes.
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

# Check one asset of a release against the file it was uploaded from: a
# finished upload, the same size, and the same sha256 when the release reports
# a digest at all.
#
# gh returning 0 is not proof the bytes are servable. Measured against a
# scratch repository, five of six 300 MiB uploads answered a range request in
# the same instant gh exited 0, and the sixth still answered BlobNotFound 24.5
# s later. So the release is asked rather than gh, and a read that disagrees is
# read again up to five times: a listing that has not caught up with an upload
# is not the same thing as an asset that landed wrong, and refusing on the
# first one that lags throws away an upload that was fine.
#
# The row it accepted is the function's stdout, so the caller can name that
# asset by id without reading the listing again; the attempt messages go to
# stderr.
verify_asset() {
  local rel="$1" name="$2" size="$3" sha="$4" n rows row want got id st sz dg
  want="uploaded ${size} sha256:${sha}"
  got="not on the release"
  for n in 1 2 3 4 5; do
    rows=$(release_assets "$rel") || return 1
    row=$(asset_row "$rows" "$name")
    if [ -n "$row" ]; then
      read -r id st sz dg <<< "$row"
      # An asset still being written carries no digest, and neither does one on
      # a GitHub that does not report digests at all; the state and the size
      # still decide.
      if [ "$dg" = none ]; then
        want="uploaded ${size}"; got="${st} ${sz}"
      else
        want="uploaded ${size} sha256:${sha}"; got="${st} ${sz} ${dg}"
      fi
      if [ "$got" = "$want" ]; then
        printf '%s\n' "$row"
        return 0
      fi
    fi
    echo "attempt ${n}: ${name} is [${got}], wanted [${want}]" >&2
    if [ "$n" -lt 5 ]; then publish_backoff "$n" 10; fi
  done
  echo "::error::${name} is [${got}], wanted [${want}]." >&2
  return 1
}

# Put back whatever an interrupted replacement left of NAME on release REL,
# before anything else touches it.
#
# The five states a replacement can be caught in were built on a real published
# release and read back the way the merger reads one:
#
#   NAME there, plus a swap-next-NAME in any state. The upload was cut off or
#   had just finished. A reader is fine. Delete swap-next-NAME, because an
#   upload onto a name that is taken is refused with 422 before the body is
#   read, and delete the swap-prev-NAME the last replacement set aside, which
#   is what bounds the extra storage to one replacement.
#
#   No NAME, swap-prev-NAME holding the published bytes, swap-next-NAME the
#   new ones. Stopped between the two renames, and the only state that hurts:
#   a download by name fails, and the next run's preflight refuses the release
#   it resolved. Rename swap-prev-NAME back, not swap-next-NAME forward,
#   because the manifests had not been swapped yet, so the old database beside
#   the old manifest is the pair the day started with.
#
#   No NAME, only a finished swap-next-NAME. What deleting before uploading
#   leaves. The bytes are whole, so they take the name.
#
#   No NAME, only a half-written swap-next-NAME. Nothing servable is left.
#   Delete it; the caller is about to upload the name again, and when no
#   caller is, preflight is the guard that refuses to build on that release.
#
#   Nothing of either name. Nothing to repair.
#
# All by id, from the assets endpoint, because that is the only listing that
# shows a half-written upload. Running it twice is a no-op the second time.
#
# What the release carries under NAME once this is done is the function's
# stdout, as "<id> <state> <size> <digest>" and empty when it carries nothing,
# so the caller does not read the listing again for an answer this already
# has. A rename moves the name and leaves the id, the size and the digest
# alone, so the row of the copy it just renamed is the row of the name. The
# messages go to stderr, out of that answer.
repair_asset() {
  local rel="$1" name="$2" rows live prev next id state
  rows=$(release_assets "$rel") || return 1
  live=$(asset_row "$rows" "$name")
  prev=$(asset_row "$rows" "swap-prev-${name}")
  next=$(asset_row "$rows" "swap-next-${name}")
  if [ -n "$live" ]; then
    if [ -n "$prev" ]; then
      echo "clearing swap-prev-${name} on release ${rel}, the copy the last replacement set aside" >&2
      delete_asset "${prev%% *}" || return 1
    fi
    if [ -n "$next" ]; then
      echo "clearing swap-next-${name} on release ${rel}, left by a replacement that did not finish" >&2
      delete_asset "${next%% *}" || return 1
    fi
    printf '%s\n' "$live"
    return 0
  fi
  if [ -n "$prev" ]; then
    echo "::warning::release ${rel} carries no ${name}; a replacement stopped between its two renames. Putting swap-prev-${name} back under the name." >&2
    rename_asset "${prev%% *}" "$name" || return 1
    if [ -n "$next" ]; then
      delete_asset "${next%% *}" || return 1
    fi
    printf '%s\n' "$prev"
    return 0
  fi
  if [ -n "$next" ]; then
    read -r id state _ <<< "$next"
    if [ "$state" = uploaded ]; then
      echo "::warning::release ${rel} carries no ${name}; the finished upload under swap-next-${name} takes the name." >&2
      rename_asset "$id" "$name" || return 1
      printf '%s\n' "$next"
      return 0
    fi
    echo "::warning::release ${rel} carries no ${name}, and swap-next-${name} never finished uploading; deleting it." >&2
    delete_asset "$id" || return 1
  fi
  return 0
}

# Repair the named assets of the published release TAG. An empty TAG is a cold
# start, with no release and nothing to repair.
repair_release() {
  local tag="$1" rel name
  shift
  if [ -z "$tag" ]; then
    return 0
  fi
  rel=$(release_id "$tag") || return 1
  for name in "$@"; do
    repair_asset "$rel" "$name" > /dev/null || return 1
  done
}

# Swap the new upload into NAME on release REL: OLD, the asset that holds NAME,
# becomes swap-prev-NAME, and then NEW takes NAME.
#
# Both are asset ids, from the reads the caller has already made: the repair
# answered which asset holds NAME, and the check of the upload answered which
# one holds swap-next-NAME. Nothing has written to the release since, and the
# renames are by id anyway, so reading the listing again here would only add a
# request to the hourly budget.
#
# The old one goes out of the way first rather than being deleted. Measured
# over five trials each, deleting first hands a reader whose listing was taken
# a moment earlier a hard 404 on an id that is gone, which happened in three of
# five trials; renaming first, the worst a reader ever got was "no assets match
# the file pattern" from a listing taken inside the window, and a reader
# holding the older listing still finished on the old bytes under
# swap-prev-NAME.
#
# Two `gh api` calls, rather than both PATCHes down one connection. The window
# in which a by-name reader finds no NAME measured 472 to 1013 ms (median 520)
# this way against 235 to 355 ms (median 288) down one connection, and the
# difference is one process start and one TLS handshake in the middle of it.
# Half a second, a few times a day, does not buy a second way of authenticating
# to GitHub inside this script.
swap_asset() {
  local rel="$1" name="$2" old="$3" new="$4" rows n renamed
  if [ -z "$old" ] || [ -z "$new" ]; then
    echo "::error::release ${rel} does not carry both ${name} and swap-next-${name}; refusing to swap them."
    return 1
  fi

  # Until this lands nothing has changed and the release still carries NAME.
  renamed=""
  for n in 1 2 3 4 5; do
    if rename_asset "$old" "swap-prev-${name}"; then
      renamed=yes
      break
    fi
    echo "attempt ${n}: could not rename ${name} out of the way on release ${rel}"
    if [ "$n" -lt 5 ]; then publish_backoff "$n" 10; fi
  done
  if [ -z "$renamed" ]; then
    echo "::error::five attempts failed to rename ${name} to swap-prev-${name}; the release still carries ${name} as it was."
    return 1
  fi

  # From here until this lands a reader asking for NAME gets nothing, so the
  # attempts are seconds apart rather than tens of seconds.
  for n in 1 2 3 4 5; do
    if rename_asset "$new" "$name"; then
      break
    fi
    echo "attempt ${n}: could not give ${name} to the new upload on release ${rel}"
    if [ "$n" -lt 5 ]; then publish_backoff "$n" 5; fi
  done

  # The release decides, not the PATCH. One that returns 500 can still have
  # applied, and the retry after it renames the same id to the name it already
  # holds, which is a 200 that changes nothing.
  rows=$(release_assets "$rel") || return 1
  if [ "$(asset_row "$rows" "$name" | cut -d' ' -f1)" = "$new" ]; then
    return 0
  fi
  if rename_asset "$old" "$name"; then
    echo "::error::${name} on release ${rel} could not be given to the new upload, so the copy that was live is back under the name. Nothing was lost; the run fails so the next one replaces it again."
  else
    echo "::error::${name} is on release ${rel} under swap-prev-${name} only, and could not be renamed back. The repair at the start of the next run puts it back under the name."
  fi
  return 1
}

# Replace NAME on a published release with the file, without the release ever
# advertising no NAME for longer than the swap takes.
#
# `gh release upload --clobber` was the other way. It deletes the live asset
# and then uploads its replacement, so from that delete until the upload
# finishes, minutes for a database this size, a reader asking for the name gets
# nothing, and if every attempt fails the release never carries one again. That
# is what strands a day: the next run resolves that release, finds no database,
# and preflight refuses to build on it.
#
# So: repair whatever an earlier attempt left, upload under swap-next-NAME,
# ask the release rather than gh whether those bytes are really there, and
# only then swap the names.
#
# REL is the release TAG names, resolved once by the caller: every asset of a
# publish goes to the same release, and each read of it is a request against
# the hourly REST budget the workflow's token gets for the repository.
#
# The upload goes through a symlink because the database is 1.9 GB and gh names
# an asset after the basename of the path it is given: the link gives the file
# the temporary name without a second copy of it.
#
# The temporary name goes in front of the real one rather than after it,
# because the name decides two things that outlive it. GitHub reads the
# content type off the extension as the asset is uploaded and a rename never
# revisits it, so a manifest staged as <name>.json.next would be served as
# application/octet-stream for the life of the release, where
# swap-next-<name>.json keeps application/json; and a reader globbing <name>*
# would be handed the copies a replacement leaves behind, where a prefix keeps
# them out of that answer. Both measured against a scratch repository.
#
# swap-prev-NAME is left behind on purpose. Deleting it here would cut off a
# reader already pulling the old copy, and the repair at the start of the next
# replacement deletes it instead.
replace_asset() {
  local tag="$1" rel="$2" file="$3" name size sha live new stage
  name=$(basename "$file")
  size=$(file_bytes "$file") || return 1
  sha=$(file_sha256 "$file") || return 1
  live=$(repair_asset "$rel" "$name") || return 1

  # Nothing under the name to protect: a release that never carried this asset,
  # or one whose half-written copy the repair just cleared. The upload can take
  # the name directly, and there is nothing to swap.
  if [ -z "$live" ]; then
    upload_asset "$tag" "$file" || return 1
    verify_asset "$rel" "$name" "$size" "$sha" > /dev/null || return 1
    return 0
  fi

  stage="$(dirname "$file")/.swap-stage"
  mkdir -p "$stage" || return 1
  ln -sfn "../${name}" "${stage}/swap-next-${name}" || return 1
  upload_asset "$tag" "${stage}/swap-next-${name}" || return 1
  new=$(verify_asset "$rel" "swap-next-${name}" "$size" "$sha") || return 1
  swap_asset "$rel" "$name" "${live%% *}" "${new%% *}" || return 1
  rm -f "${stage}/swap-next-${name}"
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
# of what landed, and only then the edit that publishes it as latest. Nothing
# reads a draft, so those uploads take their names directly. A draft already
# under the tag is left from an earlier attempt that failed somewhere in that
# sequence; it can hold any mix of assets, so it is deleted and the sequence
# starts again rather than being uploaded into.
#
# A published release is an earlier shard of this same run, and people are
# already reading it, so each of its assets is replaced by replace_asset: a new
# upload under a temporary name, then a rename. Deleting the live asset first,
# which is what `gh release upload --clobber` does, leaves the release
# advertising no database for the length of the upload, and for good if the
# upload never lands.
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
# Databases go up before manifests whatever order they are passed in. The four
# assets are still replaced one at a time, so a run that stops part way leaves
# a release whose assets do not all belong to the same shard, and a manifest
# newer than the database beside it is what preflight.R refuses as lost rows,
# while a database newer than its manifest costs nothing.
publish_release() {
  local tag="$1" title="$2" notes="$3" state f rel
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
    for f in "${ordered[@]}"; do
      upload_asset "$tag" "$f" || return 1
    done
  else
    rel=$(release_id "$tag") || return 1
    for f in "${ordered[@]}"; do
      replace_asset "$tag" "$rel" "$f" || return 1
    done
  fi
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
#
# Between Bioconductor releases this is the only thing that writes to the
# release everything downstream is reading, every day for months, so each
# manifest is replaced the way a database is: uploaded beside the live one and
# renamed into place, never deleted first.
refresh_heartbeat() {
  local tag="$1" state f rel
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
  rel=$(release_id "$tag") || return 1
  for f in "$@"; do
    replace_asset "$tag" "$rel" "$f" || return 1
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

# Clear what a replacement left on every published release of a series, except
# the one today's publish may still be working on.
#
# A replacement leaves the asset it displaced under swap-prev-<name>, because
# deleting it would cut off a reader already pulling it, and the next
# replacement under the same tag deletes it. Tomorrow publishes under a new tag
# and never comes back to today's, so without this every kept release would
# hold a second copy of both databases for good, an extra release-day of
# storage per day the pipeline published more than once.
#
# It runs the repair per name rather than deleting anything whose name starts
# with swap-prev- or swap-next-, because a release stopped between the two
# renames carries the bytes ONLY under swap-prev-<name>: that copy is the one
# the repair puts back under the name, and deleting it is exactly the loss all
# of this exists to prevent.
#
# Today's tag is skipped for the reason delete_stale_drafts skips it, and a
# release whose assets cannot be read or repaired stops the step rather than
# being passed over quietly: the release it could not repair may be the one the
# next run builds on.
sweep_swap_leftovers() {
  local rows id t kind assets names n
  rows=$(release_rows) || return 1
  while read -r id t kind; do
    case "$t" in "$1"-*) ;; *) continue ;; esac
    if [ "$kind" != published ] || [ "$t" = "$2" ]; then continue; fi
    assets=$(release_assets "$id") || return 1
    names=$(printf '%s\n' "$assets" | awk '{print $5}' \
              | sed -n -e 's/^swap-prev-//p' -e 's/^swap-next-//p' | sort -u)
    [ -n "$names" ] || continue
    while read -r n; do
      [ -n "$n" ] || continue
      echo "clearing what a replacement left of ${n} on ${t} (release ${id})"
      repair_asset "$id" "$n" > /dev/null || return 1
    done <<< "$names"
  done <<< "$rows"
}
