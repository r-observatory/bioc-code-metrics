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

`bioc-code-metrics.db` (published as a dated `code-YYYY-MM-DD` release; a
release is immutable once a later day's release exists, and old releases are
kept; see Retention):

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

`bioc-data-metrics.db` is published the same way, as a dated `data-YYYY-MM-DD`
release, and holds the dataset-focused tables.

Each dated release carries its own `manifest.json` asset (copied from
`code-manifest.json` or `data-manifest.json`). A separate `run-status.json`,
written alongside but not published, carries the `changed` and
`bootstrap_complete` flags that drive the shard loop.

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

No dated release or asset is deleted. The update's prune step is set to keep
all releases, and only a run replacing its own same-day release, drafts, and
`swap-prev-`/`swap-next-` staging assets are cleaned up. Per-function and
per-file detail is kept only for the latest version, so an older version's
detail lives only in the dated release where it was latest. The per-version
summaries are in the newest release. Keeping all
releases stays until a retention rule for these metrics is approved on its own.

## Feedback

Found a bug, a wrong number, or a missing package? Report it at [r-observatory/feedback](https://github.com/r-observatory/feedback/issues/new/choose). All feedback about R Observatory, the site, the data, and the pipelines, is tracked in one place.
