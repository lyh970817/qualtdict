# Task 3 Report: Split Deterministic Loop Option Source Resolution

## What I implemented

- Split `loop_options_for_context()` into explicit source-case dispatch:
  - `loop_options_from_matrix_source(context)`
  - `loop_options_from_choice_source_context(context)`
  - `loop_options_from_static_only_source(context)`
- Added deterministic Matrix-source ordering through
  `reconcile_matrix_source_order(static_prefixes, source_ids)`.
- Renamed choice-prefix reconciliation to
  `reconcile_choice_source_omitted_ids(choices, static_prefixes)`.
- Extracted small choice-source reconciliation helpers to keep the deterministic
  omitted-ID logic readable and within repo quality gates:
  - `reconcile_choice_source_position()`
  - `is_supported_choice_source_prefix()`
  - `is_stale_choice_source_prefix()`
- Removed the old choice-prefix helper chain that supported the previous
  fallback shape:
  - `reconcile_loop_static_choice_prefix()`
  - `resolve_unresolved_static_choice_prefix()`
  - `is_supported_unresolved_static()`
  - `is_stale_x_prefixed_static()`
- Kept source-backed Loop and Merge behavior deterministic:
  - static-only Loop and Merge rows resolve only when there is no source QID
  - source-backed loops with absent source facts return `NULL` and become
    unsupported with internal diagnostics
  - unresolved choice-source cases no longer fall back to static prefix labels
- Added the private helper named in the brief,
  `choice_source_from_static_prefixes(choices, static_prefixes)`, and removed
  the old `loop_choice_source_from_prefixes()` name from Task 3 code/tests.
- Adjusted choice-source resolution so any unresolved source-backed static
  prefix that cannot be reconciled through the explicit supported rules returns
  missing/`NULL`; there is no trailing-prefix tolerance.
- Updated tests to cover:
  - deterministic unresolved source-choice behavior
  - explicit Matrix / choice / static-only dispatch
  - source-backed loops not falling back to static prefix labels
  - Matrix-source `NULL` branches for empty items and all-`NA` item text
  - coverage-branch expectation for missing source QIDs now producing
    unsupported internal diagnostics rather than a non-looping outcome

## What I tested and test results

- `Rscript -e 'testthat::test_file("tests/testthat/test-loop_question_facts.R")'`
  - PASS
- `Rscript -e 'testthat::test_file("tests/testthat/test-dict_generate.R")'`
  - PASS
- `Rscript -e 'testthat::test_file("tests/testthat/test-coverage-branches.R")'`
  - PASS
- `rg -n "mean\\(resolved\\)|fallback_static|reconcile_loop_static_prefixes|reconcile_loop_static_choice_prefixes" R tests`
  - no matches
- `Rscript -e 'lintr::lint("R/loop_expand.R")'`
  - no lints

## TDD Evidence

### RED

- After updating `tests/testthat/test-loop_question_facts.R`, I ran:
  - `Rscript -e 'testthat::test_file("tests/testthat/test-loop_question_facts.R")'`
- Result:
  - FAIL at `Loop and Merge options fail when source choices are unresolved`
    because the current code returned a resolved subset for
    `c("x1", "x2", "missing")`
  - FAIL at `Loop and Merge options keep static non-analysed source choices`
    because the current code still tolerated the unresolved trailing prefix

### GREEN

- After implementing the source-case split and helper renames in
  `R/loop_expand.R`, I reran:
  - `Rscript -e 'testthat::test_file("tests/testthat/test-loop_question_facts.R")'`
  - This exposed remaining Task 3 alignment gaps in:
    - source-backed missing-source handling
    - numeric source-backed static-prefix skipping
    - an outdated non-analysed-source unit expectation
- After tightening source-backed null handling and updating stale coverage/unit
  expectations, I reran:
  - `Rscript -e 'testthat::test_file("tests/testthat/test-loop_question_facts.R")'`
    - PASS
  - `Rscript -e 'testthat::test_file("tests/testthat/test-coverage-branches.R")'`
    - PASS
  - `Rscript -e 'testthat::test_file("tests/testthat/test-dict_generate.R")'`
    - PASS
  - `rg -n "mean\\(resolved\\)|fallback_static|reconcile_loop_static_prefixes|reconcile_loop_static_choice_prefixes" R tests`
    - no matches

## Files changed

- `R/loop_expand.R`
- `tests/testthat/test-loop_question_facts.R`
- `tests/testthat/test-coverage-branches.R`
- `.superpowers/sdd/task-3-report.md`

## Self-review findings

- The source-case split stays inside Loop and Merge expansion and does not
  change Response Column ID Rendering boundaries.
- Public package APIs, public arguments, generated documentation, and
  `NAMESPACE` were not changed.
- Unsupported behavior remains internal-only through
  `unsupported_loop_diagnostics`.
- The old confidence heuristic and legacy helper names requested by the brief
  are absent from `R/` and `tests/`.
- I did not edit downstream adapter/accessor files.

## Issues or concerns

- None at the implementation level.

## Task 3 review-fix addendum

- Removed the unauthorized rule that allowed one trailing unresolved static
  prefix in source-backed choice resolution.
- Renamed the private helper to
  `choice_source_from_static_prefixes(choices, static_prefixes)` and updated
  internal/coverage references so the old helper name no longer remains.
- Updated Task 3 tests so the unresolved trailing-prefix case now expects
  missing/`NULL` instead of a resolved subset.
- Kept the supported explicit reconciliation behavior intact:
  stale numeric `x` prefixes can still be replaced or expanded through
  `reconcile_choice_source_omitted_ids()` when they map back onto source choice
  IDs, but any leftover unresolved prefix now makes the source missing.
