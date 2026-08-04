# Refresh Local Finalization Smoke Artifacts

This is the trusted-human workflow for preparing the local artifacts consumed
by `tools/local-finalize-smoke.R`. It requires live Qualtrics credentials and
must not run in CI or ordinary agent finalization.

## Files

- `tools/local-finalize-smoke-surveys.json` lists the fixed survey aliases and
  survey IDs. The committed file uses placeholder survey IDs; replace them with
  local surveys before fetching artifacts.
- `tools/fetch-local-finalize-smoke.R` downloads metadata, description, and
  response data. It writes metadata and description as downloaded, sanitizes
  response data in memory, and persists only sanitized responses.
- `.local/finalize-smoke/` stores downloaded artifacts, run outputs, and
  baselines. It is ignored by Git.

## Refresh Artifacts

Configure credentials locally for `qualtRics`, edit
`tools/local-finalize-smoke-surveys.json`, then run:

```sh
Rscript tools/fetch-local-finalize-smoke.R
```

To fetch a single survey while iterating:

```sh
Rscript tools/fetch-local-finalize-smoke.R --survey survey_a
```

To fetch only a small response sample when the smoke pass only needs response
column shape and labels:

```sh
Rscript tools/fetch-local-finalize-smoke.R --response-limit 1 --allow-small-sample
```

Samples below 100 rows require `--allow-small-sample` and produce artifacts the
smoke check refuses (see "Level universe observation" below). Fetch 300-500 rows
- or omit `--response-limit` entirely - for artifacts the check will accept.

The fetch script writes:

```text
.local/finalize-smoke/source/<survey-alias>/
  metadata.rds
  description.rds
  responses_raw_columns.rds
  manifest.json
```

`responses_raw_columns.rds` preserves response shape, column names, missingness,
and broad type behavior, but replaces non-missing values with deterministic
synthetic values. `manifest.json` records per-column sanitization strategies.

## Level universe observation

Sanitization makes `responses_raw_columns.rds` useless for one specific check:
`sanitize_allowed_levels()` replaces every non-missing value with values cycled
from the declared Level universe, so the stored responses are in-universe by
construction and any Level universe assertion against them passes vacuously.

The fetch script therefore records a separate `level_universe` block in
`manifest.json`, computed on the RAW responses before sanitization:

```text
level_universe
  schema         observation schema version
  response_rows  rows the observation was taken over
  totals         columns with a universe / observed / values / out of universe
  observed[]     per column: response_column_id, declared_levels, shape,
                 n_non_missing, n_out_of_universe_at_fetch, and - only when the
                 retention rule allows - codes + counts
```

The retention rule keeps a column's value histogram only when the column has at
most 12 distinct values, all of them numeric and at most 6 characters wide.
Anything else is recorded as `redacted` with counts only. The rule cannot admit
free text, identifiers, dates, or high-cardinality numerics, so no raw
participant value is persisted.

`declared_levels` is recorded at fetch time so the check can tell a dictionary
that changed since the fetch (`declared_universe_drift`) from a column whose
stored values were never inside its declared universe (`data_violation`).
