# Task 1 Report: Introduce Loop and Merge Expansion Context

## Scope

- Modified `R/loop_expand.R`
- Modified `tests/testthat/test-loop_question_facts.R`
- Left unrelated worktree entries untouched, including untracked `docs/superpowers/plans/`

## TDD Evidence

### RED attempt with exact plan command

Command:

```sh
Rscript -e 'testthat::test_file("tests/testthat/test-loop_question_facts.R")'
```

Result:

- Exit status: `0`
- Not actionable in this checkout because package code was not loaded first
- Exact blocker examples from output:
  - `could not find function "new_raw_qualtrics_metadata"`
  - `could not find function "loop_options_from_static_choices"`
  - `could not find function "normalise_qualtrics_metadata"`

This matched the task note that direct `test_file()` may fail before package code loads.

### Actionable RED

Command:

```sh
Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-loop_question_facts.R")'
```

Result before implementation:

- Exit status: `0`
- Existing tests loaded and ran
- New failing signal was the expected missing context helper:

```text
Error in `new_loop_expansion_context(question_fact = normalised_metadata$questions$QID2,
    survey_question_facts = normalised_metadata$questions)`: could not find function "new_loop_expansion_context"
```

### GREEN

Command:

```sh
Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-loop_question_facts.R")'
```

Result after implementation:

- Exit status: `0`
- `test-loop_question_facts.R`: `PASS 28`, `FAIL 0`, `WARN 0`, `SKIP 0`

## Implementation Summary

- Replaced top-level Loop and Merge expansion flow with a context-based adapter:
  - `new_loop_expansion_context()`
  - `loop_question_fact_should_expand()`
  - `expand_loop_question_fact()`
  - `loop_rows_for_context()`
  - `loop_options_for_context()`
  - `loop_expanded_question_fact()`
  - `mark_question_fact_not_looping()`
- Preserved the existing lower-level helpers for Loop Option resolution, extra field parsing, and Response Column ID normalization.
- Kept Loop and Merge expansion upstream from Response Column ID Rendering, consistent with ADR 0003.
- Preserved current text-entry response column normalization behavior validated by commit `de38b63`.

## Self-review

- Confirmed the new context object separates the current Normalised Question Fact from its Loop and Merge source fact.
- Confirmed loop-expanded text-entry facts still normalize `response_column_qid` to `x1_QID2` / `x2_QID2`.
- Confirmed only the owned files are modified and ready to stage.

## Review fix

### What changed

- Preserved the static Loop and Merge fallback in `loop_question_fact_should_expand()` by only returning `FALSE` for a missing `looping_source_fact` when there are no static Loop and Merge rows to expand.
- Added focused regression coverage in `tests/testthat/test-loop_question_facts.R` for the context case where `looping_qid` is present, `looping_source_fact` is absent, and static Loop and Merge rows must still expand.
- Renamed the internal `loop_field_values_for_question()` parameter and its call site from generic `question` to `question_fact` to match the naming requirement without changing the function name.

### Test commands and results

Command:

```sh
Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-loop_question_facts.R")'
```

Result:

- Exit status: `0`
- `test-loop_question_facts.R`: `PASS 29`, `FAIL 0`, `WARN 0`, `SKIP 0`

Command:

```sh
Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-normalise_metadata.R")'
```

Result:

- Exit status: `0`
- `test-normalise_metadata.R`: `PASS 149`, `FAIL 0`, `WARN 0`, `SKIP 0`

Command:

```sh
Rscript -e 'testthat::test_file("tests/testthat/test-loop_question_facts.R")'
```

Result:

- Exit status: `0`
- The package-load blocker remains for the exact plan command in this checkout because package helpers are not loaded first.
- Representative failures from output:
  - `could not find function "new_raw_qualtrics_metadata"`
  - `could not find function "loop_options_from_static_choices"`
  - `could not find function "normalise_qualtrics_metadata"`
