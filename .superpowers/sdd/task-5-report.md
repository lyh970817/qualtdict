## What I implemented

- Added `expand_then_render_question_response_columns()` in
  `R/loop_expand.R` as the shared adapter from Normalised Question Facts
  through Loop-expanded Question Facts to rendered ordinary question Response
  Column IDs.
- Kept Loop and Merge expansion upstream from Response Column ID Rendering:
  the adapter expands first, then renders with an already-resolved Base
  Response Column ID.
- Used the Task 5 Step 6 list-of-records shape:
  `question_fact`, unique `response_column_id`, and full row-aligned
  `response_columns`. This preserves the classifier-facing ordinary Response
  Column IDs while keeping Variable Dictionary assembly on the full rendered
  rows.
- Updated `variable_dictionary_from_normalised_metadata()` and
  `variable_dictionary_question_rows()` to consume the shared adapter instead
  of performing their own expand-then-render sequence.
- Updated `ordinary_question_response_column_ids()` to consume the shared
  adapter instead of performing its own expand-then-render sequence.
- Added regression tests for the adapter boundary and Response Column Map
  Classification with loop-expanded question IDs.

## What I tested and test results

- `Rscript -e 'testthat::test_file("tests/testthat/test-loop_question_facts.R")'`
  - PASS
- `Rscript -e 'testthat::test_file("tests/testthat/test-response_column_map_classification.R")'`
  - PASS
- `Rscript -e 'testthat::test_file("tests/testthat/test-dict_generate.R")'`
  - PASS
- `Rscript -e 'testthat::test_file("tests/testthat/test-coverage-branches.R")'`
  - PASS
- `rg -n "expand_loop_question_facts|render_response_columns\\(" R/variable_dictionary.R R/response_column_map_classification.R R/loop_expand.R`
  - Confirmed the shared adapter owns the expand-then-render sequence for the
    Task 5 callers; the remaining direct `render_response_columns()` in
    `variable_dictionary_question_row()` is the non-expanding fallback path.
- `git diff --check`
  - PASS

## TDD Evidence: RED and GREEN command/output summaries

### RED

- Ran `Rscript -e 'testthat::test_file("tests/testthat/test-loop_question_facts.R")'`
  after adding the adapter regression test.
  - Result: FAIL
  - Expected failure observed:
    `could not find function "expand_then_render_question_response_columns"`
- Ran `Rscript -e 'testthat::test_file("tests/testthat/test-response_column_map_classification.R")'`
  after adding the classifier regression test.
  - Result: PASS
  - This confirmed the existing duplicated classifier path still worked before
    the shared adapter existed.

### GREEN

- Implemented the shared adapter and wired both callers.
- Initial tibble-attribute version exposed duplicate `QID1` IDs in the direct
  adapter regression test because row-aligned rendered rows repeat the same
  Response Column ID for single-column questions.
- Revised to the Step 6 list-of-records shape so the shared adapter exposes
  unique ordinary Response Column IDs while still carrying full
  `response_columns` for Variable Dictionary assembly.
- Re-ran the focused tests:
  - `test-loop_question_facts.R`: PASS
  - `test-response_column_map_classification.R`: PASS
  - `test-dict_generate.R`: PASS
- After the first commit attempt, the `goodpractice` hook surfaced a
  `test-coverage-branches.R` failure from a `local_mocked_bindings()` override
  that still used the legacy one-argument
  `variable_dictionary_question_rows()` signature.
- Added a small dispatch helper so `variable_dictionary_from_normalised_metadata()`
  passes `rendered_question_columns` only when the active binding supports it.
- Re-ran `test-coverage-branches.R`: PASS

## Files changed

- `R/loop_expand.R`
- `R/variable_dictionary.R`
- `R/response_column_map_classification.R`
- `tests/testthat/test-loop_question_facts.R`
- `tests/testthat/test-response_column_map_classification.R`

## Self-review findings

- The shared adapter now centralizes the sequence the task asked for without
  moving Loop Option selection or Loop and Merge text substitution into
  Response Column ID Rendering.
- Variable Dictionary assembly still consumes full row-aligned rendered facts,
  so repeated Response Column IDs for level/label rows are preserved where the
  Variable Dictionary needs them.
- Response Column Map Classification now depends on the same shared expand and
  render path as Variable Dictionary assembly for ordinary question IDs.
- No public API, generated documentation, or `NAMESPACE` changes were needed.

## Issues or concerns

- None.
