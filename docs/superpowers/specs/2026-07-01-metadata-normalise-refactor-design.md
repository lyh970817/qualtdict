# Metadata Normalisation Refactor Design

Date: 2026-07-01

## Purpose

Refactor the metadata normalisation area so its internal modules match the
package capabilities named in `CONTEXT.md` and ADR 0003. The first focus is
`R/metadata_normalise.R`, which currently mixes metadata normalisation
orchestration, question metadata extraction, Embedded Data Field normalisation,
Scoring Variable normalisation, Response Column Map Classification, and
Text-analysis Sidecar normalisation in one file.

This work is behavior-preserving. It should make internal ownership clearer
without changing public package behavior, Variable Dictionary output, or
exported APIs.

## Existing Decisions

This design follows existing accepted decisions rather than replacing them:

- ADR 0001: qualtdict normalises raw Qualtrics metadata into a package-owned
  intermediate representation before Variable Dictionary generation.
- ADR 0003: internal functions should be organized by package capability and
  canonical glossary language.
- ADR 0005: Embedded Data Fields, Scoring Variables, and Text-analysis
  Sidecars are Metadata-defined Export Variables represented as first-class
  Variable Dictionary rows when Qualtrics metadata defines them.

## Phase 1 Scope

Phase 1 is a behavior-preserving internal refactor of metadata normalisation.

Keep the main internal seam:

```r
normalise_qualtrics_metadata(raw_metadata)
```

Keep the current normalised metadata shape and Normalised Question Fact shape.
Keep the current Variable Dictionary rows and column order. Do not change
public exported functions, public arguments, generated documentation, or
`NAMESPACE`.

## Phase 1 Non-Goals

Do not:

- redesign the normalised metadata representation;
- rename exported functions;
- change Variable Dictionary schema or column order;
- change Response Column ID Rendering behavior;
- change Loop and Merge expansion behavior;
- change Semantic Name generation behavior;
- change Labelled Export behavior;
- update generated `.Rd` files or `NAMESPACE`;
- introduce new user-facing warnings or errors;
- broaden the work into package-wide lint or style cleanup.

## Target Source Layout

After phase 1, `R/metadata_normalise.R` should remain as the small orchestration
module. It should coordinate the capability modules and construct the whole
normalised metadata object.

Target files:

```text
R/metadata_normalise.R
R/question_metadata_normalise.R
R/embedded_data_normalise.R
R/scoring_normalise.R
R/response_column_map_classification.R
R/text_analysis_normalise.R
```

Existing files keep their narrower roles:

```text
R/question_facts.R
R/variable_dictionary.R
R/response_column_render.R
R/loop_expand.R
```

### `R/metadata_normalise.R`

Owns the top-level normalisation flow and the whole normalised metadata
constructor.

Expected shape:

```r
normalise_qualtrics_metadata <- function(raw_metadata) {
  survey_question_facts <- normalise_qualtrics_questions(
    raw_metadata$metadata,
    raw_metadata$description
  )
  embedded_data_fields <- normalise_embedded_data_fields(
    raw_metadata$metadata,
    raw_metadata$description
  )
  response_column_map <- raw_metadata$response_column_map
  embedded_data_fields <- filter_exported_embedded_data_fields(
    embedded_data_fields,
    response_column_map
  )
  scoring_variables <- normalise_scoring_variables(
    raw_metadata$description,
    response_column_map = response_column_map
  )
  response_column_classification <- classify_response_column_map(
    response_column_map,
    questions = survey_question_facts,
    embedded_data = embedded_data_fields,
    scoring = scoring_variables
  )
  text_analysis_sidecars <- normalise_text_analysis_sidecars(
    survey_question_facts,
    response_column_classification = response_column_classification
  )

  new_normalised_metadata(...)
}
```

The exact argument names for `classify_response_column_map()` may be updated in
the implementation, but public behavior must not change.

### `R/question_metadata_normalise.R`

Owns the raw Qualtrics `metadata()` and `fetch_description()` adapter that
turns survey question payloads into survey-wide Normalised Question Facts.

Move this cluster here:

- `normalise_qualtrics_questions()`
- `normalise_question_block_metadata()`
- `block_metadata()`
- `question_metadata()`
- `normalise_question_content_types()`
- `default_question_block_metadata()`

Keep `R/question_facts.R` focused on the owned Normalised Question Fact shape,
constructors, and accessors.

### `R/embedded_data_normalise.R`

Owns Embedded Data Field normalisation, including flat metadata records,
Survey Flow adjacency parsing, merge behavior, and export filtering.

Move this cluster here:

- `normalise_embedded_data_fields()`
- `normalise_flat_embedded_data_fields()`
- `normalise_survey_flow_embedded_data_fields()`
- `empty_normalised_embedded_data_fields()`
- `merge_embedded_data_fields()`
- `filter_exported_embedded_data_fields()`
- `survey_flow_items()`
- `survey_flow_block_lookup()`
- `survey_flow_block_name()`
- `survey_flow_block_id()`
- `survey_flow_item_type()`
- `survey_flow_embedded_data_field_locations()`
- `survey_flow_embedded_data_field_names()`
- `previous_survey_flow_block()`
- `next_survey_flow_block()`
- `normalise_survey_flow_embedded_data_field()`
- `valid_embedded_data_field_names()`
- `embedded_data_field_names()`
- `embedded_data_field_name()`
- `embedded_data_flow_field_name()`

Survey Flow parsing stays in this module for phase 1 because its only current
job is Embedded Data Field block-adjacency metadata. Do not extract a generic
Survey Flow module until another real caller needs that seam.

### `R/scoring_normalise.R`

Owns Scoring Variable normalisation and export filtering.

Move this cluster here:

- `normalise_scoring_variables()`
- `scoring_categories()`
- `empty_normalised_scoring_variables()`
- `filter_exported_scoring_variables()`
- `normalise_scoring_variable()`
- `scoring_category_name()`
- `scoring_category_response_column_id()`

### `R/response_column_map_classification.R`

Owns Response Column Map Classification as a package capability. It classifies
response-column-map rows as question-backed rows, Metadata-defined Export
Variables, system metadata, question auxiliary columns, or unknown rows without
changing Response Column IDs.

Move this cluster here:

- `response_column_map_ids()`
- `response_column_map_row_ids()`
- `response_column_map_id_columns()`
- `classify_response_column_map()`
- `empty_response_column_map_classification()`
- `classify_response_column_map_row()`
- `response_column_map_row_class()`
- `response_column_map_classification_rules()`
- `response_column_map_missing_id_class()`
- `response_column_map_embedded_data_class()`
- `response_column_map_scoring_class()`
- `response_column_map_question_class()`
- `response_column_map_system_class()`
- `response_column_map_display_order_class()`
- `response_column_map_missing_parent_class()`
- `response_column_map_loop_prefixed_class()`
- `response_column_map_ordinary_qid_class()`
- `response_column_map_text_analysis_class()`
- `response_column_map_class()`
- `ordinary_question_response_column_ids()`
- `normalised_response_column_ids()`
- `is_system_response_column()`
- `system_response_column_ids()`
- `is_ordinary_qid_response_column()`
- `is_display_order_response_column()`
- `is_loop_prefixed_qid_response_column()`
- `has_derived_response_column_map_fields()`
- `response_column_map_display_name()`
- `response_column_map_parent_qid()`
- `response_column_map_scalar()`

Text-analysis Sidecar normalisation should consume the classification output.
It should not own the classification rules.

### `R/text_analysis_normalise.R`

Owns Text-analysis Sidecar normalisation from Response Column Map
Classification and parent Normalised Question Fact context.

Move this cluster here:

- `normalise_text_analysis_sidecars()`
- `text_analysis_sidecars_from_response_column_map()`
- `normalise_text_analysis_sidecar()`
- `text_analysis_sidecar_parent_context()`
- `empty_text_analysis_sidecar_parent_context()`
- `empty_normalised_text_analysis_sidecars()`

## Constructor Policy

Introduce small internal constructors where a classed normalised shape is
created in more than one place or forms a capability interface. Constructors
should assign class and protect structural basics only. They must not become a
new validation subsystem and must not add user-facing behavior.

Likely constructors:

```r
new_normalised_metadata()
new_normalised_embedded_data_fields()
new_normalised_embedded_data_field()
new_normalised_scoring_variables()
new_normalised_scoring_variable()
new_normalised_text_analysis_sidecars()
new_normalised_text_analysis_sidecar()
new_response_column_map_classification()
```

Do not add constructors for every tiny internal list if there is only one local
creator and no meaningful interface benefit.

## Naming Policy

Improve names at module interfaces and high-value internal contexts. Avoid a
package-wide local-variable rename pass.

Preferred names:

- `metadata_payload` instead of `mt`
- `description_payload` instead of `mt_d`
- `survey_question_facts` for the QID-keyed collection of Normalised Question
  Facts
- `question_fact` for one Normalised Question Fact
- `embedded_data_fields` for normalised Embedded Data Fields
- `scoring_variables` for normalised Scoring Variables
- `response_column_classification` for Response Column Map Classification
  output

Keep local churn proportional to clarity. The diff should remain easy to audit
as behavior-preserving.

## Target Test Layout

Split `tests/testthat/test-normalise_metadata.R` along the same capability
seams as the source files.

Target files:

```text
tests/testthat/test-metadata_normalise.R
tests/testthat/test-question_metadata_normalise.R
tests/testthat/test-embedded_data_normalise.R
tests/testthat/test-scoring_normalise.R
tests/testthat/test-response_column_map_classification.R
tests/testthat/test-text_analysis_normalise.R
```

Move tests that are really about other capabilities to existing test files:

- Variable Dictionary assembly tests to dictionary-focused tests.
- Semantic Name tests to Semantic Name-focused tests.
- Response Column ID Rendering tests to `test-response_columns.R`.
- Loop and Merge tests to `test-loop_question_facts.R`.
- Labelled Export tests to labelled-export or fetch-labelled-survey-data tests.

The goal is for tests to cross the same seams as callers. Do not keep a broad
metadata-normalise test file as a catch-all.

## Characterization Strategy

Before moving large clusters, add reviewable characterization coverage for
representative synthetic metadata. Prefer testthat snapshots of deliberate
summaries over opaque `.rds` fixtures.

The characterization should cover:

- ordinary question-backed rows;
- flat Embedded Data Fields;
- Survey Flow Embedded Data Field adjacency;
- Scoring Variables;
- Text-analysis Sidecars;
- Loop and Merge rows;
- SBS response columns.

For nested classed lists, snapshot a stable summary list or tibble rather than
dumping every raw nested field. For Variable Dictionary output, snapshot the
relevant rows and columns needed to prove behavior is preserved.

## Implementation Sequence

Implement phase 1 as small behavior-preserving stages. Focused tests should
pass after each meaningful move.

1. Add characterization snapshots for representative synthetic metadata and
   Variable Dictionary output.
2. Add or move constructors while preserving existing shapes.
3. Move Scoring Variable normalisation and its tests.
4. Move Embedded Data Field normalisation and its tests.
5. Move Response Column Map Classification and its tests.
6. Move Text-analysis Sidecar normalisation and its tests.
7. Move question metadata extraction into `R/question_metadata_normalise.R` and
   its tests.
8. Shrink `R/metadata_normalise.R` to orchestration and whole-object
   construction.
9. Move stray tests from `test-normalise_metadata.R` to the capability files
   that own the behavior.
10. Run focused and full verification.

## Verification

Run focused tests during implementation:

```sh
Rscript -e 'testthat::test_file("tests/testthat/test-metadata_normalise.R")'
Rscript -e 'testthat::test_file("tests/testthat/test-question_metadata_normalise.R")'
Rscript -e 'testthat::test_file("tests/testthat/test-embedded_data_normalise.R")'
Rscript -e 'testthat::test_file("tests/testthat/test-scoring_normalise.R")'
Rscript -e 'testthat::test_file("tests/testthat/test-response_column_map_classification.R")'
Rscript -e 'testthat::test_file("tests/testthat/test-text_analysis_normalise.R")'
```

Also run adjacent regression tests because the moved code feeds downstream
capabilities:

```sh
Rscript -e 'testthat::test_file("tests/testthat/test-response_columns.R")'
Rscript -e 'testthat::test_file("tests/testthat/test-loop_question_facts.R")'
Rscript -e 'testthat::test_file("tests/testthat/test-dict_generate.R")'
Rscript -e 'testthat::test_file("tests/testthat/test-fetch_labelled_survey_data.R")'
```

Run the full suite before completion:

```sh
Rscript -e 'devtools::test()'
```

If source comments or generated documentation are not changed, do not run
`devtools::document()` as part of phase 1.

## Risks

The main risk is changing classed list shapes while moving construction code.
Constructors and characterization snapshots should make these changes visible.

The second risk is accidentally changing Response Column Map Classification
order or fallback reasons. Tests should assert row sources and reasons for
known edge cases before and after the move.

The third risk is broadening the refactor into downstream behavior changes.
Phase 1 should stop when metadata normalisation ownership and tests are
aligned. Response Column ID Rendering and Loop and Merge Support have their own
phases.

## Follow-On Phases

### Phase 2: Response Column ID Rendering

Phase 2 now has its own design spec:

- `docs/superpowers/specs/2026-07-01-response-column-render-refactor-design.md`

Treat that file as canonical for Response Column ID Rendering implementation
decisions.

### Phase 3: Loop and Merge Support

Treat `R/loop_expand.R` as a separate refactor after Response Column ID
Rendering. Preserve the existing domain seam:

- Loop and Merge Support expands Normalised Question Facts into Loop-expanded
  Question Facts.
- Response Column ID Rendering consumes Normalised Question Facts or
  Loop-expanded Question Facts.
- Response Column ID Rendering may receive an already-prefixed Base Response
  Column ID, but it does not choose Loop Options or substitute Loop and Merge
  text.

Phase 3 should focus on Loop and Merge expansion clarity and tests, not on
Response Column ID Rendering internals.
