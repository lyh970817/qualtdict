# Local Finalization Smoke Check

This tooling is for the final local pass after ordinary tests pass and after
any requested review is complete. It runs all seven configured Qualtrics surveys
from local artifacts, runs the `question_name` Variable Dictionary route, runs
the exported package functions, and compares hashable summaries against local
baselines.

The smoke check is not a unit test. Refactors should keep hashes unchanged.
Feature work that intentionally changes dictionaries, labelled exports,
validation findings, or split outputs may change hashes and requires inspection
before updating baselines.

## Files

- `tools/local-finalize-smoke-surveys.json` lists the fixed survey aliases and
  survey IDs. The committed file uses placeholder survey IDs; replace them with
  local surveys before fetching artifacts.
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

When an agent runs this smoke check, it should use one self-contained script
invocation for the relevant finalization surface. Select the affected
smoke-covered exported functions with `--functions` and the relevant Variable
Dictionary route set with `--variable-name`; the script still runs prerequisite
steps needed for those outputs. Inspect the terminal output and saved run
artifacts after the command finishes.

The default check runs all seven configured surveys and the `question_name`
route.

To run only smoke-covered exported functions affected by a code change, pass
a comma-separated `--functions` list:

```sh
Rscript tools/local-finalize-smoke.R check --functions fetch_labelled_survey_data
Rscript tools/local-finalize-smoke.R check --functions fetch_labelled_survey_data,dict_split_blocks
```

The caller decides the affected functions after inspecting the code diff and
the dependency path to exported outputs. Select the smoke-covered exported
functions whose returned outputs could change; do not broaden `--functions`
merely because a prerequisite runs internally. The script still runs
prerequisite functions needed to produce selected downstream outputs, but
compares only the selected output summaries against local baselines. For
example, `--functions fetch_labelled_survey_data` generates a Variable
Dictionary as setup, then compares only Labelled Survey Data summaries.

To select the Variable Dictionary naming route, pass `--variable-name`:

```sh
Rscript tools/local-finalize-smoke.R check --variable-name question_name
```

The local finalization smoke check only supports the `question_name` route.
The Semantic Name route is disabled because it is too expensive for the
finalization workflow. Changes that affect Semantic Name behavior should be
covered by ordinary tests and package checks rather than local finalization
smoke. Do not pass `--variable-name semantic_name` or `--variable-name all`;
the script rejects both.

For example, a change limited to question-name dictionary generation should use
a focused invocation such as:

```sh
Rscript tools/local-finalize-smoke.R check --functions dict_generate --variable-name question_name
```

Smoke runs can take several minutes. When an agent is running the workflow,
wait with a longer timeout, do not repeatedly poll the process, and inspect
output once the smoke command exits before treating the agent as idle.

`check` writes current summaries and replayed objects under:

```text
.local/finalize-smoke/runs/<timestamp>/
```

Each scenario writes a JSON summary and an RDS file containing the replayed R
objects. Inspect these local artifacts before blessing changed hashes. It is
acceptable to write temporary, uncommitted R code to load an `*-objects.rds`
file with `readRDS()` and compare the relevant objects.

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

Selective blessing updates only the selected output summaries inside each
baseline record:

```sh
Rscript tools/local-finalize-smoke.R bless --functions fetch_labelled_survey_data
Rscript tools/local-finalize-smoke.R bless --variable-name question_name
```

Baselines are local to the fixed surveys and are not committed.
