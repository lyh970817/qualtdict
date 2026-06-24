# Cyclomatic Complexity Refactor Design

Date: 2026-06-24

## Context

`docs/ropensci/pkgcheck-remaining-issues.md` records three remaining
`goodpractice` cyclomatic complexity findings:

- `sbs_qid()` in `R/response_column_render.R`
- `loop_options_from_static_choices()` in `R/loop_expand.R`
- `loop_field_values_from_column_names()` in `R/loop_expand.R`

This design addresses only those three findings. It does not address the
separate long-function findings, unused internal functions, package metadata,
documentation, CI, website, coverage, or branch-name findings.

The refactor should preserve public behavior. The package API remains
unchanged, and existing rendered response-column IDs and Loop and Merge
expansion outputs should remain byte-for-byte equivalent for covered fixtures.

## Design Goals

- Reduce the cyclomatic complexity of the three named functions.
- Replace nested, implicit branch logic with named internal domain cases.
- Keep helpers in the capability modules that own the behavior:
  - SBS response-column rendering stays in `R/response_column_render.R`.
  - Loop and Merge expansion stays in `R/loop_expand.R`.
- Add targeted tests before refactoring so edge-case behavior is locked down.
- Update `docs/ropensci/pkgcheck-remaining-issues.md` after verification to mark
  the high-complexity finding as resolved.

## Non-Goals

- Do not refactor long functions unless required to complete the three
  complexity fixes.
- Do not rename exported functions or change public arguments.
- Do not reorganize files beyond adding internal helpers beside the affected
  functions.
- Do not remove unused internal functions in this pass.
- Do not change Qualtrics metadata fixtures except where a focused new test
  requires a small synthetic fixture variation.

## SBS Response Column Rendering

Target function: `sbs_qid()`.

The current implementation combines carried-forward SBS handling, column ID
normalisation, choice ID derivation, text-entry column behavior, text-entry row
behavior, multiple-answer behavior, and single-answer fallback behavior in one
nested function.

The refactor will make `sbs_qid()` a dispatcher over named SBS cases:

```r
sbs_qid()
  -> render_carried_forward_sbs_qids()
  -> sbs_rendering_columns()
  -> render_sbs_column_qids()
  -> render_sbs_row_qid()
```

`sbs_rendering_columns()` will build small internal column contexts containing
the column ID, column type, column sub-selector, row IDs, row names, choice IDs,
and choice count needed for rendering. `render_sbs_column_qids()` will turn each
column context into row contexts. `render_sbs_row_qid()` will dispatch over
domain cases:

- text-entry SBS column: append choice IDs unless the row is already a
  text-entry row.
- text-entry SBS row: return the row ID as-is.
- multiple-answer SBS column: append each choice ID.
- single-answer SBS column: repeat the row ID once per choice.

Carried-forward SBS remains a separate top-level case because the metadata has
no SBS columns. If row items are available, it returns `qid` plus item names;
otherwise it returns the base `qid`, matching current behavior.

## Loop and Merge Choice Resolution

Target function: `loop_options_from_static_choices()`.

The current implementation resolves static prefixes against source choices by
choice ID or recode, falls back to synthetic prefix choices in one branch, and
returns `NULL` for unresolved direct matching in another branch. The refactor
will make those policy decisions explicit through an internal choice-source
object.

The main flow will be:

```r
loop_options_from_static_choices()
  -> loop_choice_source()
  -> loop_options_from_choice_source()
```

`loop_choice_source()` will return a small internal list with a `type` and, when
applicable, resolved `choices`. Supported source types:

- `missing`: no usable choices or no static prefixes; the public result is
  `NULL`.
- `direct`: no looping prefixes are present, and all static prefixes match
  source choice IDs directly.
- `resolved`: looping prefixes are present, and all static prefixes match
  source choices by choice ID or recode.
- `fallback`: looping prefixes are present, but not all static prefixes can be
  resolved; synthetic choices are built from the static prefixes.

This preserves the current asymmetry: unresolved static prefixes fall back only
when looping prefixes are present. Without looping prefixes, unresolved direct
matching returns `NULL`.

`loop_options_from_choice_source()` will convert the selected choices into named
loop options. Each option uses `description` when present and non-empty, falling
back to `choiceText`, matching current behavior.

## Loop and Merge Field Extraction

Target function: `loop_field_values_from_column_names()`.

The current implementation parses valid `fieldN` names, skips invalid or
length-mismatched fields, and assigns non-empty scalar values into a nested list
inside one guarded loop.

The refactor will separate parsing, filtering, and output construction:

```r
loop_field_values_from_column_names()
  -> loop_column_field_records()
  -> valid_loop_column_field_record()
  -> loop_field_values_from_records()
```

`loop_column_field_records()` will parse `column_names` into records such as:

```r
list(
  field_number = "2",
  values = c("Red fruit", "Yellow fruit")
)
```

Invalid records are excluded before output construction:

- field names not matching `fieldN`
- value vectors whose length does not match the prefix count

`loop_field_values_from_records()` will initialize the current output shape,
then apply each valid record by prefix. `NA` and empty scalar values remain
skipped at assignment time, matching current behavior.

## Tests

Add tests before refactoring.

For SBS rendering:

- carried-forward SBS IDs from `synthetic_sbs_carried_forward_raw_metadata()`
- mixed text-entry, single-answer, and multiple-answer SBS IDs from
  `synthetic_sbs_multiple_answer_raw_metadata()`
- existing text-subquestion SBS fixture remains unchanged

For Loop and Merge choice resolution:

- static prefixes matching source choice IDs
- static prefixes matching source recodes
- fallback to prefix labels when looping prefixes are present but not all source
  choices resolve
- `NULL` when looping prefixes are absent and static prefixes do not all match
  source choice IDs

For Loop and Merge field extraction:

- valid `fieldN` records populate values by prefix
- non-`fieldN` names are ignored
- fields with value counts not matching prefixes are ignored
- `NA` and empty values are skipped

Tests may exercise internal helpers directly because the affected functions are
already internal and the package test suite currently tests internal
normalisation and rendering behavior.

## Verification

Run focused tests first:

```sh
Rscript -e 'devtools::test(filter = "response_columns|loop_question_facts|normalise_metadata")'
```

Run the full test suite:

```sh
Rscript -e 'devtools::test()'
```

Check the cyclomatic complexity finding after the refactor with the available
local tooling. A suitable command is:

```sh
Rscript -e 'goodpractice::gp(checks = "cyclocomp_linter")'
```

If the local `goodpractice` version does not support that exact command, use the
nearest available `goodpractice` or `pkgcheck` command that reports cyclomatic
complexity and record the result in the final implementation notes.

## Risks

The main risk is accidentally changing R list/vector name preservation while
introducing intermediate context objects. The implementation should avoid
unnecessary changes to `unlist()`, `map()`, `vapply()`, and name assignment
semantics. Tests should compare exact output vectors where possible.

The second risk is over-expanding the scope into the adjacent long-function
findings. This pass should stop once the three high-complexity functions are
refactored, tested, and documented as resolved.
