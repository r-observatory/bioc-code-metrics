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
databases plus their manifests. Only Bioconductor software and workflow
packages have release-branch repositories; data packages are not covered. Set
`GITHUB_TOKEN` so git fetches are authenticated.

`test-record-memo.R` also runs the rpkg-analyzer builds that `RPA_TEST_BIN_040` and `RPA_TEST_BIN_050` name. `test-record-parse-corpus.R` holds the record parse and the dataset memo to the per-line parser over whole corpora of analyzer output when `RPA_PARSE_CORPUS_050` and `RPA_PARSE_CORPUS_040` name them as absolute paths: every `*.ndjson` or `*.ndjson.gz` file below, where the files under `mv/<package>/` are one package's versions in name order. With `RPA_PARSE_CORPUS_REPORT` set it appends one line per corpus, the label, the files read and the files found identical, separated by tabs.

## Notes

Each package is cloned, analyzed across all its release branches, and deleted
before the next one, so peak disk stays small. Metrics are computed from git and
the package source; there is no external `cloc` dependency.

Moving the rpkg-analyzer pin re-queues every package unless `ANALYZER_SAME_OUTPUT` in `scripts/config.R` also names the build the stored rows came from. Add a build there only when the analyzer gate has shown that it reproduces the pinned build record for record, and quote the gate report in the pin PR; to rescan on purpose, set the list to the new pin alone. `test-workflow-analyzer-pin.R` fails while the pinned build is missing from the list, so the choice is made in the PR that moves the pin. Each run prints the output class and how many latest rows it covers, and the manifests carry `analyzer_version`, `output_class` and `n_latest_on_build` under `bootstrap`.

Each worker gives rpkg-analyzer a directory of its own for the package it analyses, `work/.rpa/<package>`, and removes it when the package is done. `RPKG_ANALYZER_CACHE_DIR` names a cache there, so a compiled file that did not change between versions is parsed once, and `RPKG_ANALYZER_STATS` names a statistics file. Builds before 0.5.1 read neither variable. Set `RPA_CACHE` to `off` (or `false`, `no`, `0`) in the workflow's environment to leave the cache out; the output is the same either way.

Each shard's log carries an `analyzer:` line (versions analysed, seconds, compiled files and the share taken from the cache, cache errors, verify mismatches, incomplete parses) and a `worker time:` line (clone, extract, analyzer, record parse, metrics, other). `run-status.json` keeps the same figures under `analyzer_stats` and `worker_phases`; the published manifests do not carry them.

## Retention

The update workflow's prune step runs only on a scheduled run whose earlier steps succeeded. It lists the published `metrics-` releases and passes their tags to `scripts/prune.R` with `KEEP: "all"`, which selects none of them, so the step deletes no published release. The legacy `code-` and `data-` releases are not in that list. After surrounding whitespace is trimmed, `KEEP` takes `all` in any case or a whole number written in digits alone, at most 2147483647, and the step fails on any other value. A number keeps that many of the newest `metrics-` releases and every one whose tag ends in `-01`, and selects the rest for deletion.

Still deleted:

- Drafts under a `metrics-` tag other than today's, by `delete_stale_drafts` in the prune step. A draft under today's tag, left by a publish that did not finish, is deleted by the next publish that day, which starts the release again.
- `swap-prev-` and `swap-next-` staging assets, by `repair_asset` in `scripts/publish.sh`. It runs in the download step on the release a run builds on, ahead of each replacement of an asset on a published release, and in the prune step's sweep (`sweep_swap_leftovers`) of every published `metrics-` release other than today's. When the release has lost the asset itself, its `swap-prev-` copy takes the name back, or else a `swap-next-` copy whose upload finished takes it, instead of being deleted.
- An asset whose upload did not finish, by the same `repair_asset` calls: on the release a run builds on, on a published release where the asset of that name is about to be replaced, and in the sweep on an earlier release that still carries a staging copy of that name. A run also deletes its own `swap-next-` upload when it cannot confirm from the release that the upload finished at the local file's size and, where the release reports a digest, with its sha256 (`discard_asset`).
- The copy of an asset that a replacement displaces. Each shard whose `run-status.json` reports `changed` publishes under the day's tag, and every publish after the day's first replaces both databases and both manifests on that release. A run that publishes nothing and saw a non-empty package universe replaces the two manifests on the release it built on. Each displaced copy stays under `swap-prev-` until a `repair_asset` call clears it.

Per-function and call-graph detail (`bioc_functions`, `bioc_call_edges`) is kept only for each package's latest version, the newest Bioconductor release it was analysed on, so an older version's detail lives only in the dated releases published while it was the latest. Per-file churn (`bioc_code_churn`) is kept for every pair of consecutive Bioconductor releases of a package, and the per-version summaries are in the newest dated release. `KEEP: "all"` stays until a retention rule for these metrics releases is approved on its own.

## Feedback

Found a bug, a wrong number, or a missing package? Report it at [r-observatory/feedback](https://github.com/r-observatory/feedback/issues/new/choose). All feedback about R Observatory, the site, the data, and the pipelines, is tracked in one place.
