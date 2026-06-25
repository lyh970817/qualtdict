# Local Finalization Smoke Check

This tooling is for the final local pass after ordinary tests pass and after
any requested review is complete. It replays a fixed pair of Qualtrics surveys
from local artifacts, runs the exported package functions, and compares
hashable summaries against local baselines.

The smoke check is not a unit test. Refactors should keep hashes unchanged.
Feature work that intentionally changes dictionaries, labelled exports,
validation findings, or split outputs may change hashes and requires inspection
before updating baselines.

## Files

- `tools/local-finalize-smoke-surveys.json` lists the fixed survey aliases and
  survey IDs. The committed file uses placeholder survey IDs; replace them with
  the two local surveys before fetching artifacts.
- `tools/fetch-local-finalize-smoke.R` is a trusted-human script that downloads
  metadata, description, and response data. It writes metadata and description
  as downloaded, sanitizes response data in memory, and persists only sanitized
  responses.
- `tools/local-finalize-smoke.R` is the finalization script. It uses local
  artifacts only, verifies Response Column ID parity against raw fetched
  response columns, runs the exported functions, and supports `check` and
  `bless`.
- `.local/finalize-smoke/` stores downloaded artifacts, run outputs, and
  baselines. It is ignored by Git.

## Prepare Artifacts

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

## Check

Run this after `devtools::test()` passes and after any requested review:

```sh
Rscript tools/local-finalize-smoke.R check
```

To run one survey:

```sh
Rscript tools/local-finalize-smoke.R check --survey survey_a
```

`check` writes current summaries and replayed objects under:

```text
.local/finalize-smoke/runs/<timestamp>/
```

Before comparing or writing hash baselines, `check` and `bless` verify one hard
Response Column ID invariant for each survey:

- every Variable Dictionary `response_column_id` is present in the raw fetched
  response data.

The reverse check, where QID/scoring raw response columns must also be present
in the Variable Dictionary, is intentionally disabled in the script until ADR
0005's Metadata-defined Export Variable contract is implemented. Parity
diagnostics are written as `<survey-alias>-response-column-id-parity.json` in
the current run directory. Parity mismatches are hard failures for both `check`
and `bless`; they cannot be accepted by updating baselines.

Exit statuses:

- `0`: all local baselines match.
- `1`: the script completed, but Response Column ID parity failed, baselines
  are missing, or hashes differ.
- `2`: script usage, config, or artifact setup failed.

## Bless

After inspecting changed output and deciding the change is intended, update
local baselines:

```sh
Rscript tools/local-finalize-smoke.R bless
```

Baselines are local to the fixed surveys and are not committed.
