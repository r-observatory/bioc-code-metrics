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
pruned on a retention schedule):

- `bioc_code_summary` - one row per package release, with the metric columns and
  the release date.
- `bioc_code_churn` - added and deleted lines per file between consecutive
  releases.
- `bioc_api_history` - exported-symbol additions and removals per release.

`bioc-data-metrics.db` is published the same way, as a dated `data-YYYY-MM-DD`
release, and holds the dataset-focused tables.

Each dated release carries its own `manifest.json` asset (copied from
`code-manifest.json` or `data-manifest.json`). A separate `run-status.json`,
written alongside but not published, carries the `changed` and
`bootstrap_complete` flags that drive the shard loop.

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

## Notes

Each package is cloned, analyzed across all its release branches, and deleted
before the next one, so peak disk stays small. Metrics are computed from git and
the package source; there is no external `cloc` dependency.
