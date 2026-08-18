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
against the stored raw response-column shape, compares the declared Level
universe against the raw value universe recorded in the artifact manifest, and
compares hashable summaries against local baselines.

The smoke check supports only the `question_name` Dictionary Variable Name
route. The Semantic Name route is disabled because it is too expensive for this
workflow. Do not pass `--variable-name semantic_name` or `--variable-name all`;
the script rejects both. Changes that affect Semantic Name behavior belong in
ordinary tests and package checks.

## Labelled Scenario Policy

The smoke's only `fetch_labelled_survey_data` scenario calls it with
`exclude_findings = "validation"`. Its summary is recorded as
`labelled_excluding_validation`; there is no default-path scenario and no
`labelled` summary any more. Live survey metadata can legitimately carry
Definite Validation Findings, and the severity gate (commit `fb05802`) aborts
the default path before download when it does, so a default-path scenario
would block the full smoke on a healthy corpus. The smoke's job is Response
Column ID parity and labelled-export shape, not re-testing the abort gate;
definite-finding aborts are unit-tested in
`tests/testthat/test-fetch_labelled_survey_data.R`, not smoke-tested. The
Labelled Export Findings and survey block outputs are derived from this same
excluding-validation Labelled Survey Data.

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
setup, then compares only the excluding-validation Labelled Survey Data
summary.

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
- every non-system raw response column is represented in the Variable
  Dictionary.

The raw-to-dictionary side excludes only Qualtrics system columns such as
`StartTime`, `EndDate`, and `Q_URL`. Question auxiliary columns, display-order
helpers, and unknown raw response columns are not exempt from parity. The saved
`<survey-alias>-response-column-id-parity.json` artifact records
`system_raw_response_columns`, `non_system_raw_response_columns`, and
`raw_response_column_classification` so missing columns can still be diagnosed
by Response Column Map Classification row source and reason. The classification
is diagnostic only; it does not filter the raw-side parity obligation.

Parity mismatches are hard failures for both `check` and `bless`; they cannot
be accepted by updating baselines.

## Level Universe

`check` and `bless` also compare each Variable Dictionary `level` universe with
the raw value universe the fetch script recorded in `manifest.json` (see
`tools/fetch-local-finalize-smoke.md`). The dictionary side is recomputed from
the current dictionary; the response side is the pinned observation, because the
sanitized responses are in-universe by construction and cannot express the
property.

Two gates are hard failures that cannot be blessed:

- a missing or wrong-schema `level_universe` observation (exit `2`) - the
  artifacts predate the check and must be refetched, otherwise stale artifacts
  would silently disable it.
- a vacuous observation: fewer than 100 response rows, or no observed values
  (exit `1`).

Everything else rides the blessed `level_universe` summary, hashed like every
other output summary. Each column is classified as:

- `data_violation` - stored values outside the current declared universe. This
  outranks every other non-text status: raw values do not change when a
  dictionary does, so a code outside the current universe is a real violation
  even when the declaration also drifted. `drifted` is reported alongside.
- `declared_universe_drift` - the dictionary changed since the fetch, and the
  stored values are still inside the current universe. A "refetch your
  artifacts" signal, never a reason to zero `violating_columns`; the count of
  drifted columns is reported separately as `drifted_columns`.
- `redacted_carry_forward` - the column's values were not safe to persist, so
  the fetch-time counts are carried forward.
- `text_column` - the declared universe is only a `_TEXT` marker, so the column
  holds free text and has no universe to violate.
- `no_declared_universe` - the column declares no `level` at all, so there is
  nothing for its stored values to fall outside of. Display-order columns are
  the deliberate case: the cell holds a display position Qualtrics never
  labels. A universe deleted by accident shows up as drift only **until the
  next refetch**: `drifted` is computed against what was declared at fetch
  time, so these columns count towards `drifted_columns` while the artifacts
  still carry the old declaration, and stop counting once the artifacts catch
  up. Past that point declared and recorded are both empty and there are no
  violations to report, so the standing signal is the blessed
  `no_declared_universe_columns` count: the columns that legitimately declare
  no universe are few and deliberate, so one more of them moves the summary
  hash.
- `clean`.

The summary therefore carries two counts alongside `violating_columns`:
`drifted_columns` (your artifacts are stale) and `no_declared_universe_columns`
(how many columns declare nothing at all).

An earlier wording of this section, and the rationale of commit `fc4f708`,
claimed the drift signal "cannot hide a deleted declaration" without
qualification. That is true only before the next refetch; the commit message is
immutable, so the correction lives here.

The saved `<survey-alias>-level-universe.json` artifact records every
comparison, so a violation can be traced to the Response Column ID, its declared
universe, and the out-of-universe codes. A real fix (violations falling) and a
regression (violations rising) both surface as a summary hash mismatch and are
blessed the normal way. Asserting zero violations is not possible today: text
and constant-sum columns still declare choice recodes they cannot store.

### Known violation classes

The violations the 12-survey set reports are not one defect. Before treating a
new number as a regression, check it against this breakdown, measured over the
12 surveys at 400 responses each (13,791 columns observed, 1,300,832 values):
**115 `data_violation` columns before the display-order fix, 67 after**, and
13,431 -> 11,464 out-of-universe values.

- **Display-order columns - FIXED.** 48 of the 115. The choice RecodeValue
  names the column and the cell holds the position at which that choice was
  displayed, so the recode was never that column's Level universe. They now
  declare none: 51 columns report `no_declared_universe` (the 48 that
  violated plus 3 whose observed positions happened to fall inside the
  declared singleton), and all 51 also report `drifted`, because the
  artifacts still carry the declaration they were fetched under. See
  `display_order_response_column_rows()`.
- **Columns whose `level` carries COLUMN IDENTITY rather than a value
  universe - NOT FIXED, escalated.** The remaining 62 `data_violation`
  columns, in five families: `CS`/`HR`/`TX` constant-sum boxes (16),
  `Matrix`/`TE` text-entry grids (31), `Slider`/`HSLIDER` (8),
  `Slider`/`STAR` (7). The declared level is a statement id, a box recode or a
  grid-column recode; the cell holds a slider position, a star count or a
  validated number, which raw metadata carries elsewhere
  (`Configuration$StarCount`, `CSSliderMin`/`CSSliderMax`,
  `validation$type == "ValidNumber"`). The naive fix is unsafe: for these
  families `item` is empty, so `label` is the only thing that distinguishes
  the sibling columns of one question, and emptying it fuses them. Fixing them
  moves exported names and value labels, so it is a scientific-interface
  decision, not a rendering one; it is filed for the consuming pipeline's team
  review (ilovedata
  `docs/review/team/qualtdict-level-universe-identity-carriers.md`) and must
  not be changed here without that ruling.
- **`Matrix`/`Likert`/`DL` - the family is right; one question is not.** 5 of
  the 115, all `ramp` QID124931274. As a family this is the control case: the
  declaration and the renderer agree, and its apparent violations elsewhere in
  the corpus are the consuming pipeline's Sentinel Level `-77`, excluded from
  substantive comparison there. The `ramp` question is a different and sharper
  defect from the identity-carrier class: the Qualtrics metadata endpoint
  returned `recode == answer id` for it (declared `{4,21,5,6,7,8,13}` - the
  answer ids), contradicting the survey definition's `RecodeValues`, and the
  export stores the definition's recodes (`-99,1,2,3` observed). The values are
  therefore MISLABELLED, not merely undeclared - a stored `5` currently reads
  as `"2"`. `resolve_dynamic_choice_recode_override()` consults the definition
  only for `DynamicChoices` questions; widening it to every question would
  change exactly this one question across the 38 offline artifact surveys
  (4,573 of 7,501 questions carry `RecodeValues`; 5 disagree with the metadata
  recodes, and 4 of those disagree only by an empty or absent metadata entry
  that already resolves to the same value). Not fixed here: the
  fix merges these five columns into the Anchors of the identically-worded
  question in three other surveys, which is a "same measurement?" question. It
  rides the same team brief.

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

**Use a selective bless only to update summaries you have just inspected, and
never as the way you refresh stale baselines.** A selective bless rewrites the
selected summaries and leaves the rest of the record untouched at whatever
state it was last written in, so the baseline silently becomes a mixture of
eras. That is how the 2026-08-05 refresh was owed: a
`bless --functions dict_generate` left the six labelled-family summaries of
that era (`validation`, `labelled`, `labelled_export_findings`,
`dict_blocks`, `survey_blocks`, `labelled_excluding_validation`; the plain
`labelled` summary has since been removed by the labelled scenario policy
above) behind at a much older
fixture, so `check` exited 1 on all 12 surveys for reasons unrelated to any
change under review, and could not have signalled a real labelled-export
regression. When in doubt, bless the full function set.

Baselines are local to the fixed surveys and are not committed.
