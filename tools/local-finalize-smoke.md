# Local Finalization Smoke Check

This document is the agent-facing guide for the final local smoke check. It is
about replaying already prepared local artifacts, not fetching new Qualtrics
data. Artifact refresh is a trusted-human workflow documented in
`tools/fetch-local-finalize-smoke.md`.

Use this smoke check only during feature finalization for changes that affect
or could affect exported behavior. It is not a unit test and does not need to
run after every edit. A good time to run it is after ordinary tests pass and
after any requested review work is complete.

## What It Checks

`tools/local-finalize-smoke.R check` replays local Qualtrics smoke artifacts,
runs smoke-covered exported functions, verifies Response Column ID parity
against the stored raw response-column shape, and compares hashable summaries
against local baselines.

Refactors should usually keep hashes unchanged. Intentional changes to Variable
Dictionaries, Labelled Survey Data, Validation Findings, Labelled Export
Findings, or split outputs may change hashes. Inspect changed output before
blessing baselines.

## Artifact Requirement

The smoke check needs local artifacts under `.local/finalize-smoke/`. This
directory is ignored by Git. Missing artifacts are not a failure of the feature
work; report that the smoke check could not be run.

Local artifacts may exist only in the main checkout, not in a separate Git
worktree. When running from a non-main worktree, pass the main checkout's
artifact root explicitly:

```sh
main_worktree="$(git worktree list --porcelain | awk '/^worktree /{wt=substr($0, 10)} /^branch refs\/heads\/main$/{print wt; exit}')"

Rscript tools/local-finalize-smoke.R check \
  --root "$main_worktree/.local/finalize-smoke" \
  --functions dict_generate \
  --variable-name question_name
```

Do not fetch live Qualtrics data as part of ordinary agent finalization.

## Run One Smoke Invocation

Run the smoke script as one self-contained invocation for the relevant
finalization surface. Do not run manually separated prerequisite steps.

Default check:

```sh
Rscript tools/local-finalize-smoke.R check
```

The default check runs every configured survey and the `question_name`
Dictionary Variable Name route.

Select smoke-covered exported functions whose returned outputs could change:

```sh
Rscript tools/local-finalize-smoke.R check --functions dict_generate
Rscript tools/local-finalize-smoke.R check --functions fetch_labelled_survey_data
Rscript tools/local-finalize-smoke.R check --functions fetch_labelled_survey_data,dict_split_blocks
```

The script still runs prerequisite functions needed to produce selected
downstream outputs, but it compares only the selected output summaries against
local baselines. Do not broaden `--functions` merely because a prerequisite
runs internally. For example, `--functions fetch_labelled_survey_data`
generates a Variable Dictionary as setup, then compares only Labelled Survey
Data summaries.

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

## Inspect Results

Smoke runs can take several minutes. Wait with a longer timeout, do not
repeatedly poll the process, and inspect output once the command exits before
treating the process as idle or stuck.

Each run writes summaries and replayed objects under:

```text
.local/finalize-smoke/runs/<timestamp>/
```

Inspect the terminal output and saved run artifacts. It is acceptable to write
temporary, uncommitted R code to load an `*-objects.rds` file with `readRDS()`
and compare relevant objects.

Before comparing or writing hash baselines, `check` and `bless` verify one hard
Response Column ID invariant for each survey:

- every Variable Dictionary `response_column_id` is present in the raw fetched
  response data.

Parity diagnostics are written as
`<survey-alias>-response-column-id-parity.json` in the current run directory.
Parity mismatches are hard failures for both `check` and `bless`; they cannot
be accepted by updating baselines.

Exit statuses:

- `0`: all local baselines match.
- `1`: the script completed, but Response Column ID parity failed, baselines
  are missing, or hashes differ.
- `2`: script usage, config, or artifact setup failed.

## Bless Intended Changes

Hash mismatches are expected for intentional behavior changes. After inspecting
changed output and deciding the change is intended, update local baselines:

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
