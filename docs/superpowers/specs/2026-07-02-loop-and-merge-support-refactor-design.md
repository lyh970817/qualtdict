# Loop and Merge Support Refactor Design

Date: 2026-07-02

## Purpose

Refactor Loop and Merge Support so the internal implementation is deterministic,
explicit about unsupported structures, and aligned with the package domain model
in `CONTEXT.md`.

This work follows the metadata normalisation and Response Column ID Rendering
refactors. It should preserve public package APIs while making
`R/loop_expand.R`, its tests, and its fixture ownership easier to reason about.

The main behavior design change is to remove weak partial-match heuristics from
Loop Option resolution. Source-backed Loop and Merge structures should resolve
through explicit supported rules or be treated as unsupported. They should not
silently fall back to static prefix labels when source resolution fails.

## Existing Decisions

This design follows existing accepted decisions:

- ADR 0001: qualtdict normalises raw Qualtrics metadata into a package-owned
  intermediate representation before Variable Dictionary generation.
- ADR 0003: internal files should be organized by package capability and
  canonical glossary language.
- ADR 0004: Unsupported Structure Findings are not a public findings surface.
- ADR 0008: strict local smoke parity treats every non-system raw response
  header as part of the raw-to-dictionary parity obligation, and dictionary
  generation should render ordinary question-backed Response Column IDs from
  Qualtrics metadata rather than raw response headers.

## Domain Boundary

Preserve the existing seam:

- Loop and Merge Support expands Normalised Question Facts into Loop-expanded
  Question Facts.
- Response Column ID Rendering consumes Normalised Question Facts or
  Loop-expanded Question Facts.
- Response Column ID Rendering may receive an already-prefixed Base Response
  Column ID.
- Response Column ID Rendering does not choose Loop Options.
- Response Column ID Rendering does not substitute Loop and Merge question text.

Phase 3 should improve Loop and Merge Support. It should not move Loop and
Merge decisions into Response Column ID Rendering.

## Current State

The current implementation already has the main context split:

- `expand_loop_question_facts()` coordinates survey-wide expansion.
- `new_loop_expansion_context()` separates the current Normalised Question Fact
  from the Loop and Merge source fact.
- `loop_rows_for_context()` resolves Loop Options and extra field values.
- `loop_expanded_question_fact()` builds the Loop-expanded Question Fact and
  supplies an already-prefixed Base Response Column ID.

Local smoke artifacts indicate that the earlier loop-prefix parity gap has been
fixed for the configured smoke aliases. The remaining smoke work is baseline
finalization: inspect changed output summaries and objects, bless local
baselines only after review, and rerun the finalization check.

Even with clean parity for configured aliases, the implementation still has
refactor targets: Loop Option source resolution mixes source cases, static
prefix reconciliation policies are hard to distinguish, partial-match fallback
uses a confidence threshold, and Loop-expanded Question Fact construction is an
implicit list mutation contract.

## Design Policy

Loop and Merge expansion should be deterministic. It may use supported metadata
sources, but it must not infer Loop Options from weak partial matches.

Supported Loop Option sources are:

- Matrix source response items.
- Choice source response choices, resolved by choice ID or recode according to
  explicit reconciliation rules.
- Static-only Loop and Merge rows when no source QID is available and the
  static rows themselves are the source of Loop Options.

Do not use confidence thresholds such as "fewer than half of source choices
resolved" to decide whether to fall back. If a source-backed Loop and Merge
structure cannot resolve its required Loop Options through the supported source
path, the looped QID is unsupported. Do not silently fall back to static prefix
labels for a failed source-backed loop.

For unsupported Loop and Merge structures:

- skip Variable Dictionary row generation for the unsupported looped QID rather
  than rendering a bare-QID fallback row;
- record an internal diagnostic that identifies the QID, Question Name when
  available, Loop and Merge source QID, and reason;
- do not introduce a public Unsupported Structure Finding in phase 3, because
  ADR 0004 removed Unsupported Structure Findings as a public findings surface;
- use strict smoke parity to expose any resulting missing raw response columns
  as Loop and Merge coverage targets.

Possible unsupported reasons include:

- missing Loop and Merge source QID and no usable static Loop Options;
- source QID present but absent from the Normalised Question Facts;
- source question has no supported choices or response items for Loop Option
  resolution;
- static prefixes cannot be resolved against source choices or response items
  by an explicit supported rule;
- Loop and Merge field vectors from `loopAndMerge$columnNames` are not aligned
  to the resolved Loop Options;
- unsupported Loop and Merge placeholder syntax;
- nested or chained Loop and Merge structures that require expanding a source
  that is itself loop-expanded.

## Target Source Layout

Keep `R/loop_expand.R` as the Loop and Merge Support module for phase 3. Do not
split it unless implementation proves a separate file is clearer. A split is
allowed only along Loop and Merge capability boundaries, not renderer
boundaries.

Likely private helper groups inside `R/loop_expand.R`:

- expansion orchestration and outcome handling;
- Loop expansion context construction;
- Loop Option source resolution;
- static prefix reconciliation policies;
- Loop and Merge field value extraction and merge precedence;
- Loop-expanded Question Fact construction;
- internal unsupported-loop diagnostics.

Add post-expansion accessors in `R/question_facts.R` when downstream callers
need Loop-expanded Question Fact fields. Keep accessors private.

Use a dedicated helper file for Loop and Merge synthetic metadata when it makes
tests clearer:

```text
tests/testthat/helper-loop_question_facts.R
```

This helper may provide small test utilities such as:

```r
looped_question_facts()
looped_dictionary_rows()
```

Do not build a large survey fixture framework.

## Refactor Targets

### Characterization First

Before code motion, add characterization coverage for the current and intended
Loop and Merge contract:

- a compact Loop-expanded Question Fact summary snapshot covering bare QID,
  Loop and Merge source QID, Loop Option, loop prefix, substituted question
  text, templated question text, Base Response Column ID, and `looping`;
- end-to-end coverage for `metadata$loopAndMerge[[block_id]]$columnNames`
  field values, not only direct helper tests;
- package-level coverage for source-backed Loop and Merge metadata that cannot
  resolve required Loop Options;
- dictionary snapshots or assertions that include Loop and Merge columns when
  the fixture is explicitly testing Loop and Merge behavior;
- Semantic Name output coverage for Loop Option suffixes.

### Explicit Expansion Outcomes

Make `expand_loop_question_fact()` distinguish its outcomes explicitly:

- not looping: no usable Loop and Merge metadata;
- unsupported loop: Loop and Merge metadata exists but cannot be resolved by a
  supported source path;
- expanded loop: one or more Loop-expanded Question Facts were produced.

The unsupported outcome should not be represented by a bare-QID fallback row.
It should be skipped with an internal diagnostic.

### Explicit Loop-expanded Question Fact Contract

Make Loop-expanded Question Fact construction an explicit internal adapter
contract. It should preserve the bare QID and set:

- `looping_question`;
- templated `question_text`;
- `looping_option`;
- `looping_prefix`;
- `looping_qid`;
- `base_response_column_id`;
- `looping = TRUE`.

Add private accessors for post-expansion fields such as Base Response Column
ID, substituted loop question text, Loop Option, and looping status. Use those
accessors in Variable Dictionary assembly and Response Column Map
Classification instead of direct list access where practical.

### Deterministic Loop Option Resolution

Split Loop Option resolution by source case:

- Matrix source;
- choice source;
- static-only source.

Replace heuristic partial-match fallback with fail-fast unsupported diagnostics
for source-backed loops. The implementation should not keep logic equivalent to
`mean(resolved) < 0.5`.

Give static-prefix reconciliation policies names that describe the rule they
apply. In particular, distinguish Matrix source order reconciliation from
choice-source omitted-ID insertion.

### Field Values And Base Response Column ID

Isolate Loop and Merge field merge precedence in a named helper. If static
field values override `loopAndMerge$columnNames` field values, make that policy
visible in helper naming and tests.

Rename `loop_response_column_id()` to Base Response Column ID vocabulary, such
as `normalise_loop_base_response_column_id()`. This helper belongs in Loop and
Merge Support because Loop and Merge chooses the already-prefixed Base Response
Column ID.

### Shared Expand-then-render Adapter

Centralize the expand-then-render adapter used by Variable Dictionary assembly
and Response Column Map Classification so both callers preserve the same Loop
and Merge boundary.

The adapter should not make Response Column ID Rendering choose Loop Options.
It should only enforce the sequence:

```text
Normalised Question Facts
-> Loop-expanded Question Facts
-> rendered ordinary question Response Column IDs
```

## Non-Goals

Do not:

- move Loop Option selection or Loop and Merge text substitution into Response
  Column ID Rendering;
- use raw response headers as a dictionary construction source;
- add a new public Unsupported Structure Finding without a separate ADR;
- broaden display-order helper rendering into Loop and Merge Support;
- broaden the narrowed non-analysed `MC/MAVR/TX` renderer rule as part of Loop
  and Merge work;
- change public exported functions, public arguments, generated documentation,
  or `NAMESPACE`;
- bless local smoke baselines without inspecting changed summaries and replayed
  objects.

## Smoke Finalization

Smoke baseline finalization is part of phase 3 finalization, not a Loop and
Merge code refactor.

During finalization:

- read `tools/local-finalize-smoke.md`;
- inspect status-`1` aliases whose hashes changed;
- inspect `*-question_name-summary.json`,
  `*-response-column-id-parity.json`, and replayed
  `*-question_name-objects.rds` where available;
- confirm changed rows are intentional;
- confirm strict parity remains clean in both directions;
- bless local baselines only after object inspection;
- do not commit `.local/finalize-smoke/` artifacts.

Missing local smoke artifacts are not a feature failure, but silently skipping
the workflow is not acceptable for phase 3.

## Verification

Run focused Loop and Merge checks during implementation:

```sh
Rscript -e 'testthat::test_file("tests/testthat/test-loop_question_facts.R")'
Rscript -e 'testthat::test_file("tests/testthat/test-question_metadata_normalise.R")'
Rscript -e 'testthat::test_file("tests/testthat/test-response_column_map_classification.R")'
```

Run adjacent renderer and downstream checks because Loop-expanded Question
Facts feed Response Column ID Rendering, Variable Dictionary assembly, Semantic
Name generation, and Labelled Export:

```sh
Rscript -e 'testthat::test_file("tests/testthat/test-response_column_render.R")'
Rscript -e 'testthat::test_file("tests/testthat/test-response_column_render_mc.R")'
Rscript -e 'testthat::test_file("tests/testthat/test-response_column_render_item_level.R")'
Rscript -e 'testthat::test_file("tests/testthat/test-dict_generate.R")'
Rscript -e 'testthat::test_file("tests/testthat/test-semantic_name.R")'
Rscript -e 'testthat::test_file("tests/testthat/test-fetch_labelled_survey_data.R")'
```

Before completion, run the full suite:

```sh
Rscript -e 'devtools::test()'
```

If local smoke artifacts are available, run the local finalization smoke check
after the full suite and report whether baseline blessing is still required.

## Acceptance Criteria

- Loop and Merge Support does not use partial-match confidence thresholds to
  resolve Loop Options.
- Source-backed Loop and Merge structures resolve through explicit supported
  rules or are skipped with internal unsupported-loop diagnostics.
- Unsupported source-backed looped QIDs do not render bare-QID fallback rows.
- Loop-expanded Question Fact construction has focused characterization and an
  explicit internal contract.
- Variable Dictionary assembly and Response Column Map Classification share the
  same expand-then-render boundary.
- Response Column ID Rendering still does not choose Loop Options or substitute
  Loop and Merge question text.
- Focused Loop and Merge tests, adjacent renderer/downstream tests, and the
  full suite pass.
- Finalization smoke parity is reviewed when local artifacts are available.
