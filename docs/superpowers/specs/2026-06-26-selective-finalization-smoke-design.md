# Selective Finalization Smoke Checks

## Context

`tools/local-finalize-smoke.R` replays local Qualtrics smoke artifacts, runs
the smoke-covered exported functions, and compares hashable summaries against
local baselines. It currently runs the full chain for every smoke scenario:

- `dict_generate()`
- `dict_validate()`
- `get_survey_data()`
- `labelled_export_findings()`
- `dict_split_blocks()`
- `survey_split_blocks()`

That is stronger than needed when a code change affects only one exported
function or one downstream behavior. Agents already inspect diffs while
finalizing feature work, so the smoke script should let the caller choose the
affected smoke-covered exported functions explicitly.

## Goals

- Add a CLI option that limits baseline comparison to selected exported
  functions.
- Preserve existing behavior when no selection option is provided.
- Keep prerequisite execution available for downstream functions without
  treating prerequisite outputs as affected outputs.
- Keep Response Column ID parity as a hard invariant when the smoke run
  generates the `question_name` Variable Dictionary.

## Non-goals

- The smoke script will not infer affected functions from Git diffs.
- The smoke script will not build or maintain a full dependency graph for all R
  source files.
- Local smoke baselines will remain local artifacts and will not be committed.
- The fetch script is unchanged.

## CLI

Add:

```sh
Rscript tools/local-finalize-smoke.R check --functions get_survey_data
Rscript tools/local-finalize-smoke.R bless --functions get_survey_data,dict_split_blocks
```

`--functions` accepts a comma-separated list of smoke-covered exported function
names. Whitespace around names is ignored. Unknown names are usage errors with
exit status 2.

Supported names are:

- `dict_generate`
- `dict_validate`
- `get_survey_data`
- `labelled_export_findings`
- `dict_split_blocks`
- `survey_split_blocks`

When `--functions` is absent, all supported functions are selected. This keeps
today's `check` and `bless` behavior intact.

## Execution Model

Selecting a downstream function still runs the prerequisites needed to produce
its input objects. Those prerequisites are setup work only unless their exported
function names were also selected.

Examples:

- `--functions dict_generate` generates dictionaries and compares only
  dictionary summaries.
- `--functions get_survey_data` generates dictionaries and any required
  validation objects as setup, then compares only Labelled Survey Data
  summaries.
- `--functions labelled_export_findings` generates the Variable Dictionary and
  Labelled Survey Data as setup, then compares only Labelled Export Finding
  summaries.
- `--functions dict_split_blocks` generates the Variable Dictionary as setup,
  then compares only dictionary block split summaries.
- `--functions survey_split_blocks` generates the Variable Dictionary and
  Labelled Survey Data as setup, then compares only labelled survey block split
  summaries.

The `question_name` scenario keeps the existing
`get_survey_data(exclude_findings = "validation")` smoke output only when
`get_survey_data` is selected. This output is part of the `get_survey_data`
coverage, not a separate selectable function.

## Baselines And Run Artifacts

The current baseline file shape is scenario-level: one JSON summary per survey
alias and variable-name strategy, with nested summaries and object hashes for
all smoke outputs. Selective runs should write and compare records containing
only the selected summaries and object hashes. This keeps `check --functions`
from failing because unselected outputs changed.

`bless --functions` updates only the selected summaries and hashes inside the
existing baseline record when that baseline exists. If the baseline is missing,
it writes a baseline record containing the selected outputs from the current
run.

Run artifacts should mirror the selected output set for JSON summaries. RDS
objects may include prerequisite objects because they are useful for inspection
and already exist in memory, but unselected objects must not drive hash
comparison.

## Response Column ID Parity

Response Column ID parity remains a hard invariant, not a selectable baseline
output. It runs for each survey whenever the `question_name` Variable Dictionary
is generated, including selective runs that need that dictionary only as setup.
Parity mismatches remain hard failures for both `check` and `bless`.

## Error Handling

- Unknown `--functions` values fail before loading artifacts.
- Empty selections, such as `--functions ""` or only commas/whitespace, fail
  before loading artifacts.
- Duplicate function names are accepted and de-duplicated.
- Existing usage, config, and artifact setup failures keep exit status 2.
- Baseline mismatches, missing selected baselines, and parity failures keep
  exit status 1.

## Testing

Unit tests should cover the new selection helpers without requiring local smoke
artifacts:

- parsing comma-separated function selections;
- rejecting unknown or empty selections;
- projecting scenario records down to selected summaries and hashes;
- merging selective `bless` output into an existing baseline record.

The local finalization smoke check itself remains optional because it depends
on local artifacts. If artifacts are available after implementation, run a
targeted selective command; otherwise report that the smoke check could not be
run.
