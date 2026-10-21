# bioc-code-metrics

This pipeline computes per-release code and quality metrics for Bioconductor
packages by cloning each package's `github.com/bioc` repository and analyzing
every Bioconductor release branch (`RELEASE_X_Y`). It publishes the results as a
SQLite database to the `r-observatory/bioc-code-metrics` GitHub repository for
downstream consumers.

For each package release it records structural metrics (file counts, lines of
code by language, compiled-code share), function counts (exported vs internal),
documentation and testing signals, security and code-health scanners,
portability and licensing fields, and per-file churn (added and deleted lines
between consecutive releases). Cross-release metrics (release cadence, API
stability, dependency drift, and more) are derived from the ordered release
series. The metric definitions match the sibling `cran-code-metrics` pipeline so
CRAN and Bioconductor packages are directly comparable; only the version model
differs (Bioconductor uses release branches rather than per-version tags).

## Output

`bioc-code-metrics.db` (published as an asset of a dated `metrics-YYYY-MM-DD`
release; no run uploads to a release once a later day's release exists, and old
releases are kept; see Retention):

- `bioc_code_summary` - one row per package release, with the metric columns and
  the release date.
- `bioc_code_churn` - added and deleted lines per file between consecutive
  releases.
- `bioc_api_history` - exported-symbol additions and removals per release.
- `bioc_description_fields` - the latest analysed release's RdMacros,
  RoxygenNote, SystemRequirements, Language, LazyData, Date and `Config/*`
  DESCRIPTION fields, each value capped at 16,384 bytes.
- `bioc_release_notes` - the NEWS section for the latest analysed release's
  DESCRIPTION Version, when the analyzer found one, capped at 16,384 bytes.
- `bioc_release_versions` - the DESCRIPTION Version of each package in each
  analysed Bioconductor release. Merged downstream.
- `bioc_version_state` - for each stored version, the release branch commit and
  tree its rows were read from, the version and commit before it in that walk,
  its deprecation signals and its `rows_generation`. Pipeline state: the data
  merger does not copy it, and the code manifest counts the summary rows it
  lacks.
- `bioc_description_history`, `bioc_release_notes_history` and
  `bioc_release_text_versions` - every DESCRIPTION field and NEWS section of
  every analysed release, and which releases were read. Not merged downstream.

`bioc-data-metrics.db` is an asset of the same `metrics-YYYY-MM-DD` release and
holds the dataset-focused tables.

Each dated release also carries `code-manifest.json` and `data-manifest.json`,
one manifest for each database. A separate `run-status.json`, written alongside
but not published, carries the `changed` and `bootstrap_complete` flags that
drive the shard loop.

## Retired columns

These columns are no longer published in `bioc_code_summary`. Each leaves the
database on the first shard written by the analyzer version named.

- `has_website`, `copyright_holder_declared` (analyzer 0.5.0)

## Running

```sh
Rscript tests/testthat.R          # unit tests
Rscript scripts/update.R out/     # analyze the next shard of packages, carry-forward
Rscript scripts/update.R out/ --bootstrap   # re-analyze everything from scratch
```

The update reads the prior databases from `out/`, analyzes a shard of packages
that are new or have a new release, and writes the updated code and dataset
databases plus their manifests. Software, workflow and experiment-data packages
have release-branch repositories; annotation packages do not and are not
covered. Set `GITHUB_TOKEN` so git fetches are authenticated.

When a clone from `github.com/bioc` fails for any reason but its time limit, the package is cloned from `git.bioconductor.org` instead, without the token and under `FALLBACK_GIT_TIMEOUT` (900 s). Most packages `github.com/bioc` lacks hold a file over 100 MiB or more than 2 GB of history, which GitHub refuses. `BIOC_GIT_FALLBACK_SKIP` in `scripts/config.R` names packages too big for a runner. A package cloned from the fallback ends its worker line with `cloned from https://git.bioconductor.org/packages`; a failure names each server tried and git's first error line from each. Each shard prints `df -h` beside `free -m`.

`test-record-memo.R` also runs the rpkg-analyzer builds that `RPA_TEST_BIN_040` and `RPA_TEST_BIN_050` name. `test-record-parse-corpus.R` holds the record parse and the dataset memo to the per-line parser over whole corpora of analyzer output when `RPA_PARSE_CORPUS_050` and `RPA_PARSE_CORPUS_040` name them as absolute paths: every `*.ndjson` or `*.ndjson.gz` file below, where the files under `mv/<package>/` are one package's versions in name order. With `RPA_PARSE_CORPUS_REPORT` set it appends one line per corpus, the label, the files read and the files found identical, separated by tabs.

## Notes

Each worker clones a package, analyzes it across all its release branches, and deletes it before taking the next, so disk holds at most one package per worker (the largest fallback packages take about 3.6 GB each while they run). Metrics are computed from git and the package source; there is no external `cloc` dependency.

Moving the rpkg-analyzer pin re-queues every package unless `ANALYZER_SAME_OUTPUT` in `scripts/config.R` also names the build the stored rows came from. Add a build there only when the analyzer gate has shown that it reproduces the pinned build record for record, and quote the gate report in the pin PR; to rescan on purpose, set the list to the new pin alone. `test-workflow-analyzer-pin.R` fails while the pinned build is missing from the list, so the choice is made in the PR that moves the pin. Each run prints the output class and how many latest rows it covers, and the manifests carry `analyzer_version`, `output_class` and `n_latest_on_build` under `bootstrap`.

Each worker gives rpkg-analyzer a directory of its own for the package it analyses, `work/.rpa/<package>`, and removes it when the package is done. `RPKG_ANALYZER_CACHE_DIR` names a cache there, so a compiled file that did not change between versions is parsed once, and `RPKG_ANALYZER_STATS` names a statistics file. Builds before 0.5.1 read neither variable. Set `RPA_CACHE` to `off` (or `false`, `no`, `0`) in the workflow's environment to leave the cache out; the output is the same either way.

Each shard's log carries an `analyzer:` line (versions analysed, seconds, compiled files and the share taken from the cache, cache errors, verify mismatches, incomplete parses) and a `worker time:` line (clone, extract, analyzer, record parse, metrics, other). `run-status.json` keeps the same figures under `analyzer_stats` and `worker_phases`; the published manifests do not carry them.

A queued package reads only the versions whose stored rows are not current: a new release branch, a branch that moved, a version with no recorded commit, a row from another analyzer build or rows generation, a row with metrics or detail still to fill, and so on. The rest keep their stored rows, including `assessed_at` and `assessed_with`. Set the repository variable `VERSION_SKIP` to `0`, `false`, `off` or `no`, in any case, to turn this off, and every queued package reads all its versions again. Any other value, an empty one included, leaves it on. Turning it off does not rewrite rows a faulty skip already wrote: a package's rows are read whole again the next time it is queued, or at once with `--requeue=<pkg,...>` (the `requeue` input of a manual run). A pull request that changes the rows R writes for one version bumps `ROWS_GENERATION` in `scripts/config.R` and regenerates the golden in `tests/testthat/fixtures/rows-golden.json`, so stored rows from before the change are read again at each package's next queue event.

To regenerate the golden, set `rows_generation` to `ROWS_GENERATION` and `output_class` to the output class of the pinned build, then take the two digests from the test log. `test-rows-generation.R` prints `fallback digest:` when the R-only digest no longer matches, on any machine, and prints `analyzer digest:` on every run with an analyzer. The `analyzer` digest is recorded from the Test workflow's log, since it is taken on Linux x86_64 with the pinned build, and the Test workflow fails while the golden has none. A new output class alone needs a new golden and no bump, because rows outside the class are read again anyway. So does a change confined to `scripts/metrics/`, which moves only the `fallback` digest: `scripts/metrics/` runs only on the R fallback, and a row the fallback writes names no analyzer build, so it is outside every output class and is read again whenever its package is next queued. The Test workflow, on pull requests and pushes to main, is where the golden is enforced. The update workflow's unit tests set `ROWS_GOLDEN_ADVISORY=1`, and there a digest or output class that no longer matches, or a missing `analyzer` digest, skips with the same note, so it cannot stop the daily run. The fixture is a package with four release branches cut from devel, built and walked with no global or system git config, and the digests cover each release's churn as well as its other rows. The `.rda` files under `tests/testthat/fixtures/rows-golden/` are committed rather than written at test time, because `save()` records the running R version in the file.

With the skip on, each shard's log carries a `version skip:` line after the `worker time:` line: how many of the listed versions were read, the reasons in the order the rules ask them (`new 2, moved 1`, or `none`), how many export diffs were redone, and how many packages were queued with nothing read. The `version skip invariants:` line under it counts, among the packages the shard wrote, those with other than one latest row and those with a current dataset link on a version other than the newest. Both counts should be 0. The log line is the shard's own. `latest_off` and `current_off` in `run-status.json` and in the code manifest under `bootstrap` make the same two counts over the whole database, and `n_versions_unrecorded` there counts stored versions with no recorded commit, which the skip reads again. With `VERSION_SKIP` off, a shard prints `version skip: off` in place of both lines. A `--bootstrap` run reads every version and prints neither.

Each shard's log also carries an `analyzer memory:` line and a `worker memory:` line. The first gives the largest resident and virtual peak of an analyzer process, each with its package, the most data one file kept, and the packages with a file over the analyzer's data budget. They come from the `peak_rss_kb`, `peak_vm_kb`, `data_kept_max` and `data_over_budget` keys of the statistics line: builds before 0.5.2 write none of them, and the line says so, and the two peaks are reported on Linux alone. The second gives the largest resident peak of a worker (`VmHWM`, Linux alone), read when its package is done and before its result is sent to the parent, and the largest result a worker returned. `run-status.json` keeps the analyzer figures, and the five packages with the largest resident peak, under `analyzer_stats`, and the worker figures under `worker_memory`. A worker's line ends with `[analyzer exit <status> x<count>]` when the analyzer exited non-zero on a release of its package, and such a package always prints its line.

Each analyzer process, the self-check's included, runs under an address-space limit set with `prlimit --as`: `ANALYZER_MEMORY_LIMIT_MB` in `scripts/config.R`, in MiB. An analyzer past it aborts with exit status 134 and no statistics line, so its package fails as a `crash`, nothing is written for it, and its stored rows stay as they were. The `prlimit` call also passes `--core=0`, so an analyzer aborted at the limit leaves no core file on the runner. The verdict's reason names the limit the analyzer ran under. Set the `ANALYZER_MEMORY_LIMIT_MB` environment variable to change the limit, or to `0` for none. After surrounding whitespace is trimmed, it takes a whole number written in digits alone, at most 2147483647, and any other value, an empty one included, leaves the default. Where `prlimit` is not on the path, which is anywhere but Linux, no limit applies. A build before 0.5.2 runs with no limit either: when an allocation fails it can write a wrong record and exit 0, and that record would replace the stored row. The run reads the analyzer's version once and works out the limit from it, so a pin back to an older build, or an older build on the path of a local run, turns the limit off. Each shard's plan says which limit is in force, and why there is none when there is none, and `run-status.json` carries it as `analyzer_memory_limit_mb`.

## Retention

The update workflow's prune step runs only on a scheduled run whose earlier steps succeeded. It lists the published `metrics-` releases and passes their tags to `scripts/prune.R` with `KEEP: "all"`, which selects none of them, so the step deletes no published release. The legacy `code-` and `data-` releases are not in that list. After surrounding whitespace is trimmed, `KEEP` takes `all` in any case or a whole number written in digits alone, at most 2147483647, and the step fails on any other value. A number keeps that many of the newest `metrics-` releases and every one whose tag ends in `-01`, and selects the rest for deletion.

Still deleted:

- Drafts under a `metrics-` tag other than today's, by `delete_stale_drafts` in the prune step. A draft under today's tag, left by a publish that did not finish, is deleted by the next publish that day, which starts the release again.
- `swap-prev-` and `swap-next-` staging assets, by `repair_asset` in `scripts/publish.sh`. It runs in the download step on the release a run builds on, ahead of each replacement of an asset on a published release, and in the prune step's sweep (`sweep_swap_leftovers`) of every published `metrics-` release other than today's. When the release has lost the asset itself, its `swap-prev-` copy takes the name back, or else a `swap-next-` copy whose upload finished takes it, instead of being deleted.
- An asset whose upload did not finish, by the same `repair_asset` calls: on the release a run builds on, on a published release where the asset of that name is about to be replaced, and in the sweep on an earlier release that still carries a staging copy of that name. A run also deletes its own `swap-next-` upload when it cannot confirm from the release that the upload finished at the local file's size and, where the release reports a digest, with its sha256 (`discard_asset`).
- The copy of an asset that a replacement displaces. Each shard whose `run-status.json` reports `changed` publishes under the day's tag, and every publish after the day's first replaces both databases and both manifests on that release. A run that publishes nothing and saw a non-empty package universe replaces the two manifests on the release it built on. Each displaced copy stays under `swap-prev-` until a `repair_asset` call clears it.

The newest dated release covers every analysed Bioconductor release of a package, not only the latest: one row each in `bioc_code_summary` and `bioc_api_history`, the per-file churn between consecutive releases in `bioc_code_churn`, and per-function and call-graph detail in `bioc_functions` and `bioc_call_edges` for each release the analyzer reported any for. It keeps no copy of the rows a later analysis replaced. A package is analysed again, across all its release branches, when Bioconductor has a new release, among other reasons. `upsert_shard` in `scripts/export.R` then deletes all of the package's rows from those five tables and writes the new ones, `upsert_datasets` does the same in `bioc_dataset_versions` and `bioc_datasets` in the dataset database, and a retired column is dropped from `bioc_code_summary` (see Retired columns). What an earlier analysis wrote is then only in the dated releases published before the package was analysed again. `KEEP: "all"` stays until a retention rule for these metrics releases is approved on its own.

## Feedback

Found a bug, a wrong number, or a missing package? Report it at [r-observatory/feedback](https://github.com/r-observatory/feedback/issues/new/choose). All feedback about R Observatory, the site, the data, and the pipelines, is tracked in one place.
