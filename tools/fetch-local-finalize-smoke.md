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
Rscript tools/fetch-local-finalize-smoke.R --response-limit 1
```

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
