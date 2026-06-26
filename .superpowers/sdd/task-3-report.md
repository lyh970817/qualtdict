## What I implemented

- Added `smoke_scenario_requirements()` to `tools/local-finalize-smoke-lib.R` so the smoke script can derive which setup steps are required for a selected function set.
- Wired `tools/local-finalize-smoke.R` to:
  - source `tools/local-finalize-smoke-lib.R` after repository root validation,
  - parse `--functions` early, before artifact loading,
  - advertise `--functions` in `usage()`,
  - run only the required smoke outputs inside `run_scenario()`,
  - pass `selected_functions` through `run_survey()`,
  - keep result and mismatch output safe when selected summaries do not include both `dict` and `labelled`,
  - project and merge selective baseline records during `check` and `bless`.
- Added the requested dependency helper test to `tests/testthat/test-local_finalize_smoke.R`.

## What I tested and test results

1. Focused smoke helper test file:

   ```sh
   Rscript -e 'testthat::test_file("tests/testthat/test-local_finalize_smoke.R")'
   ```

   Result: PASS, 21 tests passed, 0 failed.

2. No-artifact CLI validation:

   ```sh
   Rscript tools/local-finalize-smoke.R check --functions unknown_function
   ```

   Result: exited with status 2 and failed with the unknown-function error before artifact loading.

3. Commit-time hooks:

   - `lintr`: passed
   - `parsable-R`: passed
   - `Run testthat suite`: passed

## TDD Evidence

### RED

Command:

```sh
Rscript -e 'testthat::test_file("tests/testthat/test-local_finalize_smoke.R")'
```

Relevant failing output:

```text
── Error ('test-local_finalize_smoke.R:152:3'): smoke_scenario_requirements marks setup needed by selected functions ──
Error in `smoke_scenario_requirements("dict_generate")`: could not find function "smoke_scenario_requirements"
```

Why this failure was expected:

- The new test was added before implementing `smoke_scenario_requirements()`.
- The brief explicitly expected the initial failure because that helper did not exist yet.

### GREEN

Command:

```sh
Rscript -e 'testthat::test_file("tests/testthat/test-local_finalize_smoke.R")'
```

Relevant passing output:

```text
[ FAIL 0 | WARN 0 | SKIP 0 | PASS 21 ] Done!
```

## No-artifact CLI validation result

Command:

```sh
Rscript tools/local-finalize-smoke.R check --functions unknown_function
```

Output:

```text
Error: Unknown smoke-covered exported function: unknown_function. Supported values: dict_generate, dict_validate, get_survey_data, labelled_export_findings, dict_split_blocks, survey_split_blocks.
```

Exit status: `2`

Validation note:

- The script rejected the unknown function before attempting to load local smoke artifacts, which matches the task requirement.

## Files changed

- `tools/local-finalize-smoke.R`
- `tools/local-finalize-smoke-lib.R`
- `tests/testthat/test-local_finalize_smoke.R`

## Self-review findings, if any

- No functional issues found in the final pass.
- I removed one unnecessary fallback branch during review so the final `run_scenario()` summary behavior matches the task brief exactly.

## Any issues or concerns

- No open concerns from this task.
- One minor note from the RED phase: `testthat::test_file()` surfaced the expected failing test in output, which was sufficient for the TDD proof even though the command itself did not return a non-zero shell status in that red run.

## Follow-up fix for review finding

### What I fixed

- Added focused helper coverage for selective-safe smoke output formatting in `tests/testthat/test-local_finalize_smoke.R`.
- Implemented `smoke_result_line()` and `smoke_mismatch_lines()` in `tools/local-finalize-smoke-lib.R`.
- Updated `tools/local-finalize-smoke.R` so `print_result_line()` and `print_mismatch()` delegate to those helpers.
- Kept full-summary formatting intact for `dict` and `labelled`, while making selective summaries report `outputs=` or mismatch outputs without assuming missing summaries exist.
- Tightened helper lookups to use exact summary names so selective outputs like `dict_blocks` do not partially match `dict`.

### RED evidence

Command:

```sh
Rscript -e 'testthat::test_file("tests/testthat/test-local_finalize_smoke.R")'
```

Relevant failing output before the helpers existed:

```text
── Error ('test-local_finalize_smoke.R:200:3'): smoke_result_line includes detailed row counts for full summaries ──
Error in `smoke_result_line(result, "matched")`: could not find function "smoke_result_line"

── Error ('test-local_finalize_smoke.R:217:3'): smoke_result_line falls back to outputs for selective summaries ──
Error in `smoke_result_line(result, "current")`: could not find function "smoke_result_line"

── Error ('test-local_finalize_smoke.R:245:3'): smoke_mismatch_lines includes detailed summary deltas when available ──
Error in `smoke_mismatch_lines(current, baseline)`: could not find function "smoke_mismatch_lines"

── Error ('test-local_finalize_smoke.R:273:3'): smoke_mismatch_lines reports selective outputs without missing-summary assumptions ──
Error in `smoke_mismatch_lines(current, baseline)`: could not find function "smoke_mismatch_lines"
```

### GREEN evidence

Command:

```sh
Rscript -e 'testthat::test_file("tests/testthat/test-local_finalize_smoke.R")'
```

Passing output after implementation:

```text
[ FAIL 0 | WARN 0 | SKIP 0 | PASS 25 ] Done!
```

### No-artifact CLI validation result

Command:

```sh
Rscript tools/local-finalize-smoke.R check --functions unknown_function
```

Output:

```text
Error: Unknown smoke-covered exported function: unknown_function. Supported values: dict_generate, dict_validate, get_survey_data, labelled_export_findings, dict_split_blocks, survey_split_blocks.
```

Exit status: `2`

Validation note:

- The script still rejects unknown functions before artifact loading.

### Commit created

- `Add smoke output formatting tests`

### Any concerns

- No open concerns.

## Fix for remaining bless replacement review finding

### What I fixed

- Added focused regression tests for a pure `bless_smoke_record()` helper so bless-mode record selection is specified independently of the CLI branch.
- Implemented `bless_smoke_record(existing, selected, selective = FALSE)` in `tools/local-finalize-smoke-lib.R`.
- Updated `tools/local-finalize-smoke.R` to distinguish between:
  - `functions_filter <- arg_value("--functions")`
  - `selected_functions <- parse_smoke_functions(functions_filter)`
  - `selective_functions <- !is.null(functions_filter)`
- Changed the `bless` branch so it only merges into an existing baseline when `selective_functions` is `TRUE`; default full bless now writes the current full record directly and drops stale baseline entries.
- Preserved the existing selective merge ordering behavior and projection behavior.

### RED evidence for bless-mode regression

Command:

```sh
Rscript -e 'testthat::test_file("tests/testthat/test-local_finalize_smoke.R")'
```

Relevant failing output before the implementation fix:

```text
── Error ('test-local_finalize_smoke.R:207:3'): bless_smoke_record replaces stale baseline entries in full mode ──
Error in `bless_smoke_record(existing, selected, selective = FALSE)`: could not find function "bless_smoke_record"

── Error ('test-local_finalize_smoke.R:242:3'): bless_smoke_record merges selected summaries in selective mode ──
Error in `bless_smoke_record(existing, selected, selective = TRUE)`: could not find function "bless_smoke_record"
```

Why this failure was expected:

- The regression tests were added first.
- The helper did not exist yet, so the red run proved the new bless-selection behavior was not already covered or implemented.

### GREEN evidence after implementation

Command:

```sh
Rscript -e 'testthat::test_file("tests/testthat/test-local_finalize_smoke.R")'
```

Passing output after the implementation fix:

```text
[ FAIL 0 | WARN 0 | SKIP 0 | PASS 40 ] Done!
```

### No-artifact CLI validation result

Command:

```sh
Rscript tools/local-finalize-smoke.R check --functions unknown_function
```

Output:

```text
Error: Unknown smoke-covered exported function: unknown_function. Supported values: dict_generate, dict_validate, get_survey_data, labelled_export_findings, dict_split_blocks, survey_split_blocks.
```

Exit status: `2`

Validation note:

- The script still rejects an unknown function before any smoke artifact loading.

### Commit created

- `Restore full bless baseline replacement`

### Any concerns

- No open concerns.

## Follow-up fix for remaining ordering review findings

### What I fixed

- Updated the full-selection ordering regression coverage in `tests/testthat/test-local_finalize_smoke.R` so the historical full-run order is asserted as:
  `dict`, `validation`, `labelled`, `labelled_export_findings`, `dict_blocks`, `survey_blocks`, `labelled_excluding_validation`.
- Added a merge regression test proving that `merge_smoke_baseline()` restores canonical full-summary order when a selective bless introduces a newly selected summary between existing known summaries.
- Added `smoke_full_summary_names()` and `order_smoke_record_summaries()` in `tools/local-finalize-smoke-lib.R`.
- Updated `project_smoke_record()` so full selections use canonical full-summary ordering while partial selections still follow `smoke_summary_names(selected_functions)` and still omit unavailable optional summaries.
- Updated `merge_smoke_baseline()` to reorder known summaries and object hashes into canonical full-summary order after merging, while leaving any unknown summary names after the known names in their existing order.
- Updated `run_scenario()` in `tools/local-finalize-smoke.R` so `labelled_excluding_validation` is appended after `labelled_export_findings`, `dict_blocks`, and `survey_blocks`, matching the historical full-run output order.

### RED evidence

Command:

```sh
Rscript -e 'testthat::test_file("tests/testthat/test-local_finalize_smoke.R")'
```

Relevant failing output before the implementation fix:

```text
── Failure ('test-local_finalize_smoke.R:147:5'): project_smoke_record uses historical full-run order for full selection ──
names(projected$summaries) (`actual`) not identical to c(...) (`expected`).

    actual                          | expected
[1] "dict"                          | "dict"                          [1]
[2] "validation"                    | "validation"                    [2]
[3] "labelled"                      | "labelled"                      [3]
[4] "labelled_excluding_validation" - "labelled_export_findings"      [4]
[5] "labelled_export_findings"      - "dict_blocks"                   [5]
[6] "dict_blocks"                   - "survey_blocks"                 [6]
[7] "survey_blocks"                 - "labelled_excluding_validation" [7]

── Failure ('test-local_finalize_smoke.R:159:5'): project_smoke_record uses historical full-run order for full selection ──
projected$object_hashes (`actual`) not identical to list(...) (`expected`).

── Failure ('test-local_finalize_smoke.R:168:5'): project_smoke_record uses historical full-run order for full selection ──
projected$scenario_hash does not match "^[0-9a-f]{32}$".

── Failure ('test-local_finalize_smoke.R:169:5'): project_smoke_record uses historical full-run order for full selection ──
identical(projected$scenario_hash, "full-hash") is not FALSE

── Failure ('test-local_finalize_smoke.R:240:3'): merge_smoke_baseline restores canonical order for known summaries ──
names(merged$summaries) (`actual`) not identical to c("dict", "labelled", "survey_blocks") (`expected`).

── Failure ('test-local_finalize_smoke.R:244:3'): merge_smoke_baseline restores canonical order for known summaries ──
merged$object_hashes (`actual`) not identical to list(dict = "old-dict", labelled = "new-labelled", survey_blocks = "old-survey-blocks") (`expected`).
```

Why these failures were expected:

- Full-selection projection was preserving the input record's wrong full-run order, leaving `labelled_excluding_validation` ahead of `labelled_export_findings`, `dict_blocks`, and `survey_blocks`.
- Baseline merging was appending newly selected summaries in selective bless order, so known summaries could remain in history-dependent order rather than canonical full-summary order.

### GREEN evidence

Command:

```sh
Rscript -e 'testthat::test_file("tests/testthat/test-local_finalize_smoke.R")'
```

Passing output after implementation:

```text
[ FAIL 0 | WARN 0 | SKIP 0 | PASS 32 ] Done!
```

### No-artifact CLI validation result

Command:

```sh
Rscript tools/local-finalize-smoke.R check --functions unknown_function
```

Output:

```text
Error: Unknown smoke-covered exported function: unknown_function. Supported values: dict_generate, dict_validate, get_survey_data, labelled_export_findings, dict_split_blocks, survey_split_blocks.
```

Exit status: `2`

Validation note:

- The script still rejects the unknown function before any smoke artifact loading.

### Commit created

- `a1b6e91 Fix smoke summary ordering`

### Any concerns

- No open concerns.

## Follow-up fix for second re-review finding

### What I fixed

- Added a regression test proving that `project_smoke_record(record, parse_smoke_functions(NULL))` preserves an already-full smoke record's summary order, object hash order, and `scenario_hash`.
- Implemented the minimal full-selection guard in `project_smoke_record()` so records are returned unchanged when the selected summary set already covers every summary present in the record.
- Left partial projections unchanged: they still use `smoke_summary_names(selected_functions)` order and still omit unavailable optional summaries.

### RED evidence

Command:

```sh
Rscript -e 'testthat::test_file("tests/testthat/test-local_finalize_smoke.R")'
```

Relevant failing output before the implementation fix:

```text
── Failure ('test-local_finalize_smoke.R:147:5'): project_smoke_record preserves full record order and hash for full selection ──
names(projected$summaries) (`actual`) not identical to names(record$summaries) (`expected`).

    actual                          | expected
[1] "dict"                          | "dict"                          [1]
[2] "validation"                    | "validation"                    [2]
[3] "labelled"                      | "labelled"                      [3]
[4] "labelled_export_findings"      - "labelled_excluding_validation" [4]
[5] "dict_blocks"                   - "labelled_export_findings"      [5]
[6] "survey_blocks"                 - "dict_blocks"                   [6]
[7] "labelled_excluding_validation" - "survey_blocks"                 [7]

── Failure ('test-local_finalize_smoke.R:148:5'): project_smoke_record preserves full record order and hash for full selection ──
names(projected$object_hashes) (`actual`) not identical to names(record$object_hashes) (`expected`).

── Failure ('test-local_finalize_smoke.R:149:5'): project_smoke_record preserves full record order and hash for full selection ──
projected$scenario_hash (`actual`) not identical to "full-hash" (`expected`).
```

Why this failure was expected:

- Full-mode selection was being projected through `smoke_summary_names()`, which moves `labelled_excluding_validation` to the end.
- That reorder changed `object_hashes` order and forced a different recomputed `scenario_hash`, breaking default `check`/`bless` behavior when `--functions` is absent.

### GREEN evidence

Command:

```sh
Rscript -e 'testthat::test_file("tests/testthat/test-local_finalize_smoke.R")'
```

Passing output after the implementation fix:

```text
[ FAIL 0 | WARN 0 | SKIP 0 | PASS 28 ] Done!
```

### No-artifact CLI validation result

Command:

```sh
Rscript tools/local-finalize-smoke.R check --functions unknown_function
```

Output:

```text
Error: Unknown smoke-covered exported function: unknown_function. Supported values: dict_generate, dict_validate, get_survey_data, labelled_export_findings, dict_split_blocks, survey_split_blocks.
```

Exit status: `2`

Validation note:

- The script still rejects the unknown function before any smoke artifact loading.

### Commit created

- `560f236 Preserve full smoke projection hashes`

### Any concerns

- No open concerns.
