# Local Finalization Smoke Check

Agent-facing guide for the final local smoke check. It replays prepared local
artifacts; it must not fetch live Qualtrics data. Trusted-human artifact
refresh is documented in `tools/fetch-local-finalize-smoke.md`.

Use this only during feature finalization for changes that affect or could
affect exported behavior, usually after ordinary tests and requested review
work are complete. Missing local artifacts are not a feature failure; report
that the smoke check could not be run.

## Scope

`tools/local-finalize-smoke.R check` replays local Qualtrics smoke artifacts,
runs smoke-covered exported functions, verifies Response Column ID parity
against the stored raw response-column shape, and compares hashable summaries
against local baselines.

The smoke check supports only the `question_name` Dictionary Variable Name
route. The Semantic Name route is disabled because it is too expensive for this
workflow. Do not pass `--variable-name semantic_name` or `--variable-name all`;
the script rejects both. Changes that affect Semantic Name behavior belong in
ordinary tests and package checks.

## Artifacts

The smoke check expects `.local/finalize-smoke/`, which is ignored by Git.
Artifacts may exist only in the main checkout. From another worktree, pass the
main checkout's artifact root:

```sh
main_worktree="$(git worktree list --porcelain | awk '/^worktree /{wt=substr($0, 10)} /^branch refs\/heads\/main$/{print wt; exit}')"

Rscript tools/local-finalize-smoke.R check \
  --root "$main_worktree/.local/finalize-smoke" \
  --functions dict_generate \
  --variable-name question_name
```

Each run writes summaries and replayed objects under:

```text
.local/finalize-smoke/runs/<timestamp>/
```

## Run

Run one self-contained smoke invocation for the relevant finalization surface:

```sh
Rscript tools/local-finalize-smoke.R check
Rscript tools/local-finalize-smoke.R check --functions dict_generate
Rscript tools/local-finalize-smoke.R check --functions fetch_labelled_survey_data
Rscript tools/local-finalize-smoke.R check --functions fetch_labelled_survey_data,dict_split_blocks
Rscript tools/local-finalize-smoke.R check --variable-name question_name
Rscript tools/local-finalize-smoke.R check --functions dict_generate --variable-name question_name
```

The script runs prerequisites needed for selected downstream outputs, but
compares only the selected output summaries. Do not broaden `--functions` only
because a prerequisite runs internally. For example,
`--functions fetch_labelled_survey_data` generates a Variable Dictionary as
setup, then compares only Labelled Survey Data summaries.

Smoke runs can take several minutes. Use a longer timeout, wait for the command
to exit, then inspect terminal output and saved run artifacts.

Exit statuses:

- `0`: all local baselines match.
- `1`: the script completed, but Response Column ID parity failed, baselines
  are missing, or hashes differ.
- `2`: script usage, config, or artifact setup failed.

## Required Artifact Inspection

Before blessing changed hashes or missing baselines, inspect the saved run
artifacts relevant to the feature/change. This is mandatory: do not bless from
hash output alone. In the final response, state what artifacts and content were
inspected.

Inspect both:

- JSON summaries from the current run, including selected output summaries and
  `<survey-alias>-response-column-id-parity.json`.
- Relevant replayed R objects from `*-objects.rds`, loaded with `readRDS()`.

Content checks should match the selected `--functions` and the feature/change.
Examples:

- For `dict_generate`, inspect affected Variable Dictionary rows, including
  Response Column ID and Dictionary Variable Name values.
- For validation work, inspect Validation Findings in summaries and objects.
- For Labelled Export work, inspect Labelled Export Findings and Labelled
  Survey Data columns, labels, and value labels.
- For `dict_split_blocks`, inspect split outputs and the affected Variable
  Dictionary rows in each split.

`check` and `bless` both verify hard Response Column ID invariants for each
survey before comparing or writing hash baselines:

- every Variable Dictionary `response_column_id` is present in the raw fetched
  response data.
- every checked raw response column is represented in the Variable Dictionary.

The checked raw response set includes ordinary question columns listed in the
stored Qualtrics response `column_map`, plus metadata-defined export variables
that this package represents: Embedded Data fields, exported Scoring Variables,
and Text-analysis Sidecars. Qualtrics helper columns that are not in the response
`column_map`, such as display-order helpers, are not treated as dictionary rows.
If an older local artifact has no stored `column_map`, ordinary raw question
raw-to-dictionary parity is skipped for that artifact, but metadata-defined raw
columns are still checked where their metadata is available.

Parity mismatches are hard failures for both `check` and `bless`; they cannot
be accepted by updating baselines.

## Bless Intended Changes

Hash mismatches can be expected for intentional behavior changes. Bless only
after the required artifact-content inspection shows the changed or newly
missing baseline is correct for the feature/change:

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
