# Response Column ID Rendering Refactor Design

Date: 2026-07-01

## Purpose

Refactor Response Column ID Rendering so the source files, tests, fixtures, and
private helper names match the package language in `CONTEXT.md` and ADR 0003.

This work is behavior-preserving. It should make the renderer easier to reason
about without changing public package behavior, rendered Response Column IDs,
Variable Dictionary output, Labelled Export behavior, or exported APIs.

## Existing Decisions

This design follows existing accepted decisions rather than replacing them:

- ADR 0001: qualtdict normalises raw Qualtrics metadata into a package-owned
  intermediate representation before Variable Dictionary generation.
- ADR 0003: internal functions should be organized by package capability and
  canonical glossary language.
- ADR 0005: `response_column_id` is the universal exported-column identifier
  and row provenance key for every Variable Dictionary row.

No new ADR is needed for phase 2 unless implementation uncovers a harder
architectural decision. The file split, private helper names, and fixture
layout are implementation design decisions inside the existing Response Column
ID Rendering capability.

## Scope

Refactor `R/response_column_render.R` and its tests. The current file is large
within one capability rather than large because it mixes unrelated package
capabilities.

Keep the high-level internal renderer entry point:

```r
render_response_columns(question_fact, base_response_column_id = NULL)
```

This is a private argument rename from the current `response_column_qid`
vocabulary. The rename is allowed because `render_response_columns()` is an
internal helper; exported package APIs and public arguments remain unchanged.

`base_response_column_id` is the Base Response Column ID supplied to rendering
before question-shape-specific suffixes are added. It is usually a bare QID,
but it may already include a Loop and Merge prefix such as `x1_QID2`.

The entry point should keep returning the same row-aligned fact table shape:

```text
response_column_id
question
item
level
label
```

Aggressively renaming private renderer internals is allowed when the new names
clarify the distinction between QID, Base Response Column ID, and rendered
Response Column ID. Public package APIs remain unchanged.

## Non-Goals

Do not:

- rename exported functions;
- change public function arguments;
- change Variable Dictionary schema or column order;
- change rendered Response Column IDs;
- change row-aligned renderer output columns or row order;
- change Loop and Merge expansion behavior;
- move Loop and Merge expansion into Response Column ID Rendering;
- change Semantic Name generation behavior;
- change Labelled Export behavior;
- change unsupported-renderer fallback behavior;
- introduce new user-facing warnings or errors;
- update generated `.Rd` files or `NAMESPACE` unless exported roxygen comments
  actually change;
- broaden the work into unrelated dictionary, validation, labelled-export, or
  package-wide style refactors.

## Target Source Layout

Use a question-family split rather than a pipeline-stage split.

Target files:

```text
R/response_column_render.R
R/response_column_render_mc.R
R/response_column_render_item_level.R
R/response_column_render_sbs.R
R/response_column_render_special.R
```

The family files are private implementation modules. Do not introduce
downstream-callable family interfaces such as `render_mc_response_columns()` or
`render_sbs_response_columns()` unless a real non-test caller needs them.

### `R/response_column_render.R`

Owns the Response Column ID Rendering entry point and shared private machinery:

- `render_response_columns()`;
- Base Response Column ID resolution;
- render context construction;
- shared shape construction for non-SBS facts;
- final row-aligned fact assembly;
- dispatch composition;
- generic renderer primitives.

Final row-aligned fact assembly belongs in the root renderer. Family renderers
may return rendered Response Column ID vectors or family-specific shape facts,
but they should not each build their own output tibble.

Keep generic renderer primitives in this file rather than creating a broad
helper module. Shared private grammar such as repeated Base Response Column
IDs, level suffixes, item suffixes, text suffixes, item-or-level fallback, and
row-alignment primitives belongs with the root Response Column ID Rendering
orchestrator.

### `R/response_column_render_mc.R`

Owns multiple-choice-specific dispatch and helper code, such as:

- MC renderer table;
- text-entry choice behavior;
- choice ID resolution;
- recode ID resolution;
- MC-only renderer specializations.

### `R/response_column_render_item_level.R`

Owns item/level response-column grammar for matrix-like multi-row renderers.
This file should include the Matrix, Likert, Slider, and Constant Sum dispatch
tables and any item/level-specific helpers that are not generic root
primitives.

The `item_level` name is implementation vocabulary for this phase. It does not
introduce a new glossary term.

One-off root dispatch entries such as `SS`, `DD`, and `HL` may stay composed in
the root renderer unless they accumulate enough family-specific behavior to
justify moving.

### `R/response_column_render_sbs.R`

Owns the whole Side-by-Side rendering cluster, including SBS shape preparation
and SBS Response Column ID grammar.

Move SBS shape preparation with SBS rendering. SBS shape functions encode
SBS-specific column, row, level, text-entry, and carried-forward behavior, so
leaving shape in the root renderer would split one Qualtrics grammar across
two files.

The root `response_column_shape()` or its renamed equivalent may still call
into `response_column_sbs_shape()` as a private family-specific hook.

### `R/response_column_render_special.R`

Owns special fixed or exceptional renderers:

- display/no-row rendering;
- Timing fixed columns;
- FileUpload and Signature fixed columns;
- unsupported renderer fallback.

Text Entry rendering should stay in the root renderer for phase 2 unless it
grows real family-specific behavior. It is ordinary question-backed rendering
that currently dispatches to generic primitives.

## Naming Policy

Use canonical glossary language in private renderer names.

Prefer names that say `response_column_id` over names that call rendered
response-column strings `qid`. For example, private primitives may move from
abbreviated names such as:

```r
suf_level_qid()
suf_item_suf_level_qid()
rep_level_qid()
suf_text_qid()
suf_nmlabel_qid()
```

toward explicit names such as:

```r
render_response_column_id_with_level_suffix()
render_response_column_id_with_item_and_level_suffixes()
render_response_column_id_repeated_by_level()
render_response_column_id_with_text_suffix()
render_response_column_id_with_named_label_suffix()
```

The exact names may be adjusted during implementation. The important rule is
that top-level private helper names should distinguish:

- QID: the bare Qualtrics question identifier;
- Base Response Column ID: the stem supplied to rendering;
- Response Column ID: the rendered exported-column identifier.

Rename `response_column_qid` vocabulary aggressively where appropriate.
`render_response_columns()` should use `base_response_column_id` for the
already-resolved rendering stem.

The high-level entry point name `render_response_columns()` may remain because
it returns row-aligned response-column facts, not only a vector of IDs. Rename
lower-level helpers such as `render_response_column_ids()` if needed to clarify
when a helper returns only an ID vector.

## Constructor Policy

Do not introduce a classed Response Column Render Context in phase 2. The
render context and render shape may remain plain private lists.

Small constructor or builder helpers are acceptable when they make the split
clearer, but they must not become a new validation subsystem or add user-facing
behavior.

## Loop and Merge Boundary

Preserve the existing boundary:

- Loop and Merge Support expands Normalised Question Facts into Loop-expanded
  Question Facts.
- Response Column ID Rendering consumes Normalised Question Facts or
  Loop-expanded Question Facts.
- Response Column ID Rendering may receive an already-prefixed Base Response
  Column ID.
- Response Column ID Rendering does not choose Loop Options.
- Response Column ID Rendering does not substitute Loop and Merge question
  text.

Renderer family files must not inspect loop metadata to decide prefixes.

## Fallback Policy

Preserve unsupported renderer fallback behavior exactly:

- same fallback Response Column ID: the supplied Base Response Column ID;
- same warning condition;
- same warning text except for replacing the inaccurate "bare QID" fallback
  wording with "Base Response Column ID";
- no new Unsupported Structure Finding;
- no conversion from warning to finding;
- no new quiet or suppress argument;
- no new classification of unsupported renderer cases.

## Target Test Layout

Retire `tests/testthat/test-response_columns.R` and replace it with
capability-aligned renderer tests:

```text
tests/testthat/test-response_column_render.R
tests/testthat/test-response_column_render_mc.R
tests/testthat/test-response_column_render_item_level.R
tests/testthat/test-response_column_render_sbs.R
tests/testthat/test-response_column_render_special.R
```

Use the files as follows:

- `test-response_column_render.R`: entry point behavior, required Base Response
  Column ID handling, output shape, Base Response Column ID override, generic
  root primitives, and row alignment.
- `test-response_column_render_mc.R`: MC selector and sub-selector cases,
  text-entry choices, recode versus choice ID behavior, and non-exported choice
  filtering where it affects independent columns.
- `test-response_column_render_item_level.R`: Matrix, Likert, Slider, Constant
  Sum, item/level ordering variants, and item-or-level fallback cases.
- `test-response_column_render_sbs.R`: SBS shape preparation, SBS columns,
  SBS rows, carried-forward rows, text-entry subquestions, multiple-answer
  columns, and row metadata alignment.
- `test-response_column_render_special.R`: Display Block no-row behavior,
  Timing fixed columns, FileUpload/Signature fixed columns, and unsupported
  fallback warning.

Keep Loop and Merge and dictionary tests as downstream regression coverage:

```text
tests/testthat/test-loop_question_facts.R
tests/testthat/test-dict_generate.R
tests/testthat/test-fetch_labelled_survey_data.R
```

Those tests should still run during phase 2, but they should not be the main
place where response-column grammar is specified.

Prefer testing through `render_response_columns()` wherever behavior can be
observed through the row-aligned output. Direct private-helper tests are still
allowed for dense branch logic where end-to-end setup is too noisy, but they
should protect current rendering concepts rather than freeze obsolete helper
names.

## Fixture Refactor

Phase 2 may refactor the synthetic fixture architecture. The current shared
helper file mixes fixtures for metadata normalisation, metadata-defined export
variables, Loop and Merge, and Response Column ID Rendering. Splitting fixture
ownership along the renderer test layout is in scope.

Possible helper files:

```text
tests/testthat/helper-normalised-metadata.R
tests/testthat/helper-response_column_render.R
tests/testthat/helper-response_column_render_mc.R
tests/testthat/helper-response_column_render_item_level.R
tests/testthat/helper-response_column_render_sbs.R
tests/testthat/helper-response_column_render_special.R
tests/testthat/helper-loop_question_facts.R
```

Keep genuinely shared base metadata builders in the shared helper file. Move
renderer-specific synthetic metadata into renderer helper files when that makes
ownership clearer.

Fixture constraints:

- keep fixtures synthetic, local, and readable;
- avoid `.rds` or opaque large fixture blobs;
- avoid live Qualtrics dependencies;
- keep fixture names aligned to the behavior they exercise;
- allow small shared builder helpers when they reduce copy-paste;
- do not refactor unrelated labelled-export or validation fixtures unless they
  are tangled with renderer fixtures.

A small builder layer is allowed for repeated boilerplate, for example helpers
that normalise one question or render one fixture. Do not build a full survey
fixture DSL unless implementation proves it pays for itself.

## Characterization Strategy

Before disruptive moves, add focused renderer-output characterization
snapshots. Snapshot deliberate summaries of row-aligned renderer output rather
than whole Variable Dictionaries or raw Qualtrics metadata.

Representative cases:

- MC with text-entry choice;
- Matrix or Likert item-level rendering;
- SBS mixed columns;
- Timing fixed columns;
- FileUpload fixed columns;
- unsupported fallback to the supplied Base Response Column ID;
- loop-prefixed MC text IDs.

Renderer snapshots should live in renderer-specific snapshot files, such as:

```text
tests/testthat/_snaps/response_column_render.md
tests/testthat/_snaps/response_column_render_mc.md
tests/testthat/_snaps/response_column_render_item_level.md
tests/testthat/_snaps/response_column_render_sbs.md
tests/testthat/_snaps/response_column_render_special.md
```

Do not extend `_snaps/metadata_normalise.md` for renderer behavior.

## Implementation Sequence

Implement phase 2 as behavior-preserving stages. Focused tests should pass
after each meaningful move.

1. Add focused renderer characterization snapshots.
2. Refactor renderer test fixtures and helpers enough to support the new test
   layout.
3. Split `test-response_columns.R` into `test-response_column_render*.R`.
4. Rename private renderer primitives and Base Response Column ID vocabulary.
5. Move MC-specific rendering into `R/response_column_render_mc.R`.
6. Move item-level rendering into `R/response_column_render_item_level.R`.
7. Move SBS shape and rendering into `R/response_column_render_sbs.R`.
8. Move special rendering into `R/response_column_render_special.R`.
9. Shrink `R/response_column_render.R` to entry point, context, shared shape,
   row assembly, dispatch composition, and generic primitives.
10. Run focused renderer tests after each move.
11. Run Loop and Merge plus dictionary regression tests.
12. Run the full test suite.
13. Attempt the local finalization smoke workflow.

A different order is acceptable during implementation when it reduces churn,
but characterization should precede disruptive moves, and downstream regression
tests plus the smoke workflow must run or be reported before completion.

## Verification

Run focused renderer tests during implementation:

```sh
Rscript -e 'testthat::test_file("tests/testthat/test-response_column_render.R")'
Rscript -e 'testthat::test_file("tests/testthat/test-response_column_render_mc.R")'
Rscript -e 'testthat::test_file("tests/testthat/test-response_column_render_item_level.R")'
Rscript -e 'testthat::test_file("tests/testthat/test-response_column_render_sbs.R")'
Rscript -e 'testthat::test_file("tests/testthat/test-response_column_render_special.R")'
```

Run adjacent regression tests because the renderer consumes Loop-expanded
Question Facts and feeds downstream dictionary and labelled-export behavior:

```sh
Rscript -e 'testthat::test_file("tests/testthat/test-loop_question_facts.R")'
Rscript -e 'testthat::test_file("tests/testthat/test-dict_generate.R")'
Rscript -e 'testthat::test_file("tests/testthat/test-fetch_labelled_survey_data.R")'
```

Run the full suite before completion:

```sh
Rscript -e 'devtools::test()'
```

Read `tools/local-finalize-smoke.md` during finalization and follow its
workflow. If the required local smoke artifacts are available, run the smoke
check before declaring phase 2 complete. If the artifacts are unavailable,
phase 2 may still be completed only after reporting that the required smoke
workflow was attempted and could not be run because artifacts were missing.
Missing artifacts are not a feature failure, but silently skipping the smoke
workflow is not allowed.

If exported roxygen comments are not changed, do not run `devtools::document()`
as part of phase 2.

## Acceptance Criteria

- `render_response_columns()` remains the high-level internal renderer entry
  point.
- The renderer uses Base Response Column ID vocabulary instead of
  `response_column_qid` vocabulary where appropriate.
- Rendered Response Column IDs are unchanged for covered MC, item-level, SBS,
  special, fallback, and loop-prefixed cases.
- Row-aligned facts keep the same columns and row order.
- `R/response_column_render.R` no longer contains family-specific MC,
  item-level, SBS, or special renderer clusters.
- New renderer files own their family-specific dispatch tables and helpers.
- Tests are split into `test-response_column_render*.R` files.
- Renderer characterization snapshots exist and are reviewed.
- Synthetic renderer fixtures are organized around renderer behavior and remain
  local, readable, and offline.
- Loop and Merge regression tests pass.
- Dictionary generation regression tests pass.
- Full test suite passes.
- Local finalization smoke workflow is attempted and run when artifacts exist.

## Risks

The main risk is accidentally changing rendered Response Column IDs while
renaming private helpers and moving renderer families. Characterization
snapshots and precise renderer expectations should make these changes visible.

The second risk is splitting row alignment into family-specific mini-pipelines.
Keep final row-aligned fact assembly in the root renderer.

The third risk is smuggling Loop and Merge expansion into rendering while
fixing loop-prefixed cases. Keep Base Response Column ID resolution as the
boundary: Loop and Merge Support chooses prefixes upstream, and rendering
consumes the already-resolved stem.
