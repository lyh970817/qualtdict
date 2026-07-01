Status: DONE_WITH_CONCERNS

Task: Full Verification and Local Smoke Attempt
Worktree: /home/andongni/Yandex.Disk/Projects/Research/packages/qualtdict/.worktrees/response-column-render-refactor
Date: 2026-07-01

## Summary

All required focused renderer tests, adjacent regression tests, and the full
`devtools::test()` suite passed. The local finalize smoke check ran against
prepared local artifacts from the main checkout and exited `0`, with hash
matches for all selected surveys and Response Column ID parity matches for all
smoke-covered `dict_generate(question_name)` outputs.

Status is `DONE_WITH_CONCERNS` because the final obsolete-vocabulary search did
not limit findings to historical documentation. It also found live
`response_column_qid` references in package code and tests, plus one roxygen
mention of "bare QID".

## Commands Run

Focused renderer tests:

```sh
Rscript -e 'testthat::test_file("tests/testthat/test-response_column_render.R")'
Rscript -e 'testthat::test_file("tests/testthat/test-response_column_render_mc.R")'
Rscript -e 'testthat::test_file("tests/testthat/test-response_column_render_item_level.R")'
Rscript -e 'testthat::test_file("tests/testthat/test-response_column_render_sbs.R")'
Rscript -e 'testthat::test_file("tests/testthat/test-response_column_render_special.R")'
```

Adjacent regressions:

```sh
Rscript -e 'testthat::test_file("tests/testthat/test-loop_question_facts.R")'
Rscript -e 'testthat::test_file("tests/testthat/test-dict_generate.R")'
Rscript -e 'testthat::test_file("tests/testthat/test-fetch_labelled_survey_data.R")'
```

Full suite:

```sh
Rscript -e 'devtools::test()'
```

Smoke workflow read:

```sh
sed -n '1,220p' tools/local-finalize-smoke.md
```

Smoke attempt, using the main checkout artifact root because this worktree had
no `.local/finalize-smoke/` directory:

```sh
Rscript tools/local-finalize-smoke.R check \
  --root "/home/andongni/Yandex.Disk/Projects/Research/packages/qualtdict/.local/finalize-smoke" \
  --functions dict_generate \
  --variable-name question_name
```

Final checks:

```sh
git status --short
rg -n "response_column_qid|bare QID|test-response_columns" R tests docs/superpowers
```

## Results

Focused renderer tests:

- `test-response_column_render.R`: pass, 23 expectations
- `test-response_column_render_mc.R`: pass, 29 expectations
- `test-response_column_render_item_level.R`: pass, 20 expectations
- `test-response_column_render_sbs.R`: pass, 21 expectations
- `test-response_column_render_special.R`: pass, 13 expectations

Adjacent regressions:

- `test-loop_question_facts.R`: pass, 54 expectations
- `test-dict_generate.R`: pass, 110 expectations
- `test-fetch_labelled_survey_data.R`: pass, 100 expectations

Full suite:

- `devtools::test()` passed
- Duration: about 5.6 seconds
- Totals: `FAIL 0 | WARN 0 | SKIP 0 | PASS 830`

Smoke attempt:

- Exit status: `0`
- Artifact source: main checkout local artifacts at
  `/home/andongni/Yandex.Disk/Projects/Research/packages/qualtdict/.local/finalize-smoke`
- Run directory:
  `/home/andongni/Yandex.Disk/Projects/Research/packages/qualtdict/.local/finalize-smoke/runs/20260701T095245Z`
- Selected surveys:
  `glad_sa8_signup`, `edgi_signup_official`, `glad_sa7_optional`,
  `edgi_optional_all`, `glad_sa6_signup`, `edgi_medications`,
  `glad_medications`
- Selected route: `question_name`
- All seven surveys reported matching `dict` hashes and Response Column ID
  parity matches.

Per-survey parity output observed from the smoke run:

- `glad_sa8_signup`: parity matched `dict_ids=6013`, `raw_cols=6127`
- `edgi_signup_official`: parity matched `dict_ids=1447`, `raw_cols=1464`
- `glad_sa7_optional`: parity matched `dict_ids=4662`, `raw_cols=4689`
- `edgi_optional_all`: parity matched `dict_ids=5473`, `raw_cols=5542`
- `glad_sa6_signup`: parity matched `dict_ids=932`, `raw_cols=955`
- `edgi_medications`: parity matched `dict_ids=3485`, `raw_cols=3654`
- `glad_medications`: parity matched `dict_ids=3525`, `raw_cols=3671`

## Required Smoke Artifact Inspection

Newest run directory inspection:

- Confirmed newest run directory with
  `ls -td .../.local/finalize-smoke/runs/* | head -1`
- Confirmed seven `*-response-column-id-parity.json` files exist
- Confirmed seven `*-question_name-objects.rds` files exist

Replayed object inspection:

- Loaded all seven `*-objects.rds` files with `readRDS()`
- Confirmed each replayed object contains a `dict` qualtdict object
- Confirmed each `dict` has a populated `response_column_id` column
- Confirmed non-missing `response_column_id` counts equal total row counts in
  all seven replayed dictionaries
- Inspected representative rows showing preserved `response_column_id` values
  alongside `variable_name` and `question_name`

Representative replayed object checks:

- `edgi_medications-question_name-objects.rds`:
  4489 rows, 15 columns, 4489 non-missing `response_column_id` values,
  3485 distinct IDs
- `edgi_optional_all-question_name-objects.rds`:
  10207 rows, 15 columns, 10207 non-missing `response_column_id` values,
  5473 distinct IDs
- `edgi_signup_official-question_name-objects.rds`:
  3082 rows, 14 columns, 3082 non-missing `response_column_id` values,
  1447 distinct IDs
- `glad_medications-question_name-objects.rds`:
  4522 rows, 15 columns, 4522 non-missing `response_column_id` values,
  3525 distinct IDs
- `glad_sa6_signup-question_name-objects.rds`:
  2429 rows, 14 columns, 2429 non-missing `response_column_id` values,
  932 distinct IDs
- `glad_sa7_optional-question_name-objects.rds`:
  8126 rows, 15 columns, 8126 non-missing `response_column_id` values,
  4662 distinct IDs
- `glad_sa8_signup-question_name-objects.rds`:
  10664 rows, 15 columns, 10664 non-missing `response_column_id` values,
  6013 distinct IDs

Parity JSON inspection:

- Loaded parity JSON files with `jsonlite::read_json(..., simplifyVector = FALSE)`
- Confirmed each JSON includes `alias`, `survey_id`, `ok`,
  `dict_response_column_ids`, `raw_response_columns`,
  `checked_raw_response_columns`, `question_auxiliary_exported_columns`,
  `missing_from_raw_response`, and `missing_from_dict`
- Confirmed `ok` is `TRUE` for inspected parity artifacts
- Confirmed `missing_from_raw_response` and `missing_from_dict` are empty lists
  in the inspected parity JSON output

## Final Git And Search Checks

`git status --short`:

- clean working tree
- no tracked files changed during verification
- no commit created

Obsolete vocabulary search interpretation:

Historical documentation hits in `docs/superpowers/specs/` appear intentional
and describe old naming or planned refactors.

However, the search also found live references outside historical specs:

- `R/loop_expand.R`
- `R/response_column_map_classification.R`
- `R/variable_dictionary.R`
- `R/dict_generate.R` roxygen text mentioning "bare QID"
- `tests/testthat/test-loop_question_facts.R`

These findings do not break the current verification run, but they do mean the
final search outcome is not limited to historical references. That is the sole
reason this task is reported with concerns.

## Commits

None. No tracked files changed during verification, so no verification commit
was created.

## Task 9 Final Vocabulary Fix

- Renamed the loop-resolved rendering stem in
  `loop_expanded_question_fact()` from `response_column_qid` to
  `base_response_column_id` without changing the rendered Response Column IDs.
- Updated `variable_dictionary_question_row()` and
  `ordinary_question_response_column_ids()` to read
  `base_response_column_id %||% qid` and pass that local stem into
  `render_response_columns()`.
- Updated `tests/testthat/test-loop_question_facts.R` expectations and the
  text-entry test wording to use `base_response_column_id`.
- Reworded `R/dict_generate.R` roxygen from `bare QID` to `bare Qualtrics
  question identifier`.

Requested verification:

- `Rscript -e 'testthat::test_file("tests/testthat/test-loop_question_facts.R")'`
  passed
- `Rscript -e 'testthat::test_file("tests/testthat/test-dict_generate.R")'`
  passed
- `Rscript -e 'testthat::test_file("tests/testthat/test-response_column_map_classification.R")'`
  passed
- `Rscript -e 'testthat::test_file("tests/testthat/test-fetch_labelled_survey_data.R")'`
  passed

Final search:

- `rg -n "response_column_qid|bare QID|test-response_columns" R tests docs/superpowers`
  found no live hits in `R/` or `tests/`
- Remaining hits are historical/spec references in `docs/superpowers/specs/`,
  which are acceptable for final review
