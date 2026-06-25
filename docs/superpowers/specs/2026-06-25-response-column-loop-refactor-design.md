# Response Column Rendering and Loop Expansion Refactor Design

Date: 2026-06-25

## Purpose

Reduce the long-function and unclear-interface issues around Response Column ID
Rendering and Loop and Merge Support without changing public package behavior.
The refactor replaces broad positional helper signatures with internal context
objects whose names match the package domain model.

This design addresses the `goodpractice` long-function findings noted in
`docs/ropensci/pkgcheck-remaining-issues.md`, especially:

- `R/loop_expand.R:4`
- `R/response_column_render.R:10`
- `R/response_column_render.R:175`

The work is behavior-preserving. It must not intentionally change generated
`response_column_id`, `qid`, `question`, `loop_option`, `item`, `level`,
`label`, or Variable Dictionary column order. It must also preserve existing
Loop and Merge `response_column_qid` normalization for text-entry cases.

## Domain Boundaries

The refactor keeps the boundary documented in `CONTEXT.md` and ADR 0003:

- Loop and Merge Support expands Normalised Question Facts into
  Loop-expanded Question Facts.
- Response Column ID Rendering consumes Normalised Question Facts or
  Loop-expanded Question Facts.
- Response Column ID Rendering may receive an already-prefixed
  `response_column_qid`, but it does not choose Loop Options or substitute Loop
  and Merge question text.
- `variable_dictionary_from_normalised_metadata()` remains the adapter that
  runs Loop and Merge expansion, renders response columns, and assembles the
  Variable Dictionary.

## Naming

New or changed internal interfaces should avoid generic `question` and
`questions` parameters.

Use:

- `question_fact`: one Normalised Question Fact or Loop-expanded Question Fact.
- `survey_question_facts`: the survey-wide collection of question facts, keyed
  by bare QID.
- `looping_source_fact`: the Normalised Question Fact that supplies Loop Options
  for another question.
- `response_column_qid`: the QID-like prefix used by Response Column ID
  Rendering. It is the bare QID for ordinary questions and may be loop-prefixed
  for Loop-expanded Question Facts.

The public dictionary columns `qid` and `response_column_id` keep their current
names and meanings.

## Loop and Merge Support

`expand_loop_question_facts()` should become a short coordinator:

```r
expand_loop_question_facts <- function(survey_question_facts) {
  imap(survey_question_facts, function(question_fact, bare_qid) {
    context <- new_loop_expansion_context(
      question_fact = question_fact,
      survey_question_facts = survey_question_facts
    )

    expand_loop_question_fact(context)
  }) %>%
    unlist(recursive = FALSE)
}
```

Add a Loop and Merge expansion context:

```r
new_loop_expansion_context <- function(question_fact, survey_question_facts) {
  looping_qid <- scalar_character(question_fact_looping_qid(question_fact))
  looping_static <- question_fact_looping_static(question_fact)

  list(
    question_fact = question_fact,
    looping_qid = looping_qid,
    looping_source_fact = if (!is.na(looping_qid)) {
      survey_question_facts[[looping_qid]]
    } else {
      NULL
    },
    looping_static = looping_static,
    static_prefixes = unlist(
      question_fact_looping_prefix(question_fact),
      use.names = FALSE
    )
  )
}
```

Expected helper split:

- `new_loop_expansion_context(question_fact, survey_question_facts)` builds the
  context for one question fact.
- `loop_question_fact_should_expand(context)` handles "no loop metadata" and
  "missing source QID" decisions.
- `expand_loop_question_fact(context)` coordinates expansion for one question
  fact and preserves current fallback behavior.
- `loop_rows_for_context(context)` builds loop row records with `prefix`,
  `option`, and field values.
- `loop_options_for_context(context)` resolves static, choice-source, and
  matrix-source Loop Options.
- `loop_expanded_question_fact(context, loop_row)` constructs one
  Loop-expanded Question Fact.
- `mark_question_fact_not_looping(question_fact)` records `looping = FALSE`.

`loop_expanded_question_fact()` owns assignment of:

- `looping_question`
- templated `question_text`
- `looping_option`
- `looping_prefix`
- `looping_qid`
- `response_column_qid`
- `looping = TRUE`

It must preserve the bare QID in `question_fact$qid` while setting
`response_column_qid` to values such as `x1_QID2` or `1_QID2`.
`response_column_qid` construction must preserve the existing
`loop_response_column_id()` normalization, including the `_TEXT_TEXT` rewrite
used by looped text-entry cases.

`loop_rows_for_context()` must preserve supported extra-field substitution. It
continues to combine field values from static Loop and Merge rows and metadata
column names before `loop_expanded_question_fact()` assigns substituted
`looping_question` text and templated `question_text`.

## Response Column ID Rendering

`render_response_columns()` should remain the public internal entry point for
rendering rows from one question fact. Its implementation should become a short
coordinator:

```r
render_response_columns <- function(question_fact, response_column_qid = NULL) {
  response_column_qid <- resolve_response_column_qid(
    question_fact,
    response_column_qid
  )

  question_type <- question_fact_question_type(question_fact)
  shape <- response_column_shape(question_fact)

  context <- new_response_column_render_context(
    question_fact = question_fact,
    response_column_qid = response_column_qid,
    shape = shape,
    question_type = question_type
  )

  response_column_id <- response_column_row_vector(
    render_response_column_ids(context)
  )

  response_column_rows(context, response_column_id)
}
```

Add a Response Column ID Rendering context:

```r
new_response_column_render_context <- function(question_fact,
                                               response_column_qid,
                                               shape,
                                               question_type) {
  list(
    question_fact = question_fact,
    response_column_qid = response_column_qid,
    shape = shape,
    render_facts = response_column_render_facts(shape, question_type$type),
    type = question_type$type,
    selector = question_type$selector,
    sub_selector = question_type$sub_selector
  )
}
```

Expected helper split:

- `resolve_response_column_qid(question_fact, response_column_qid)` resolves the
  QID-like rendering prefix without adding format validation.
- `new_response_column_render_context()` builds the renderer context.
- `response_column_render_facts(shape, type)` centralizes the existing renderer
  input normalization. For non-SBS question types, it unwraps `shape$level[[1]]`
  and `shape$label[[1]]` before renderer dispatch. For SBS, it keeps the
  list-shaped `level` and `label` values. This preserves the current
  `qid_recode()` normalization rule while removing broad positional arguments.
- `render_response_column_ids(context)` invokes the selected renderer.
- `response_column_renderer_for_context(context)` resolves the renderer
  function from type, selector, and sub-selector.
- `response_column_renderer_table()` owns the dispatch table that currently
  lives inside `qid_recode()`. The final implementation should replace the
  active `qid_recode()` dispatch path with domain-named Response Column ID
  Rendering helpers; because `qid_recode()` is internal, no compatibility
  wrapper is required unless it is useful during an intermediate commit.
- `response_column_rows(context, response_column_id)` builds the final tibble
  with row-aligned facts.
- `render_response_column_items(context, response_column_id)`,
  `render_response_column_levels(context, response_column_id)`, and
  `render_response_column_labels(context, response_column_id)` replace
  positional item, level, label helper calls.

Renderer helpers should accept one context instead of broad positional
arguments. For example:

```r
render_mc_level_qids <- function(context) {
  level <- context$render_facts$level
  add_text_mc(
    paste(context$response_column_qid, mc_choice_ids(level), sep = "_"),
    level
  )
}
```

```r
render_timing_qids <- function(context) {
  paste0(context$response_column_qid, c(
    "_FIRST_CLICK",
    "_LAST_CLICK",
    "_PAGE_SUBMIT",
    "_CLICK_COUNT"
  ))
}
```

`resolve_response_column_qid()` is intentionally narrow. It should preserve the
current precedence and validation surface:

- use the explicit `response_column_qid` argument when supplied;
- otherwise use `question_fact$qid`;
- if the resolved value is `NULL`, length zero, or `NA`, stop with the existing
  "`qid` is required to render response columns." error;
- do not add new format validation for loop-prefixed values.

SBS rendering can also become context-based:

```r
render_sbs_qids <- function(context) {
  if (length(context$shape$col_type) == 0) {
    return(render_carried_forward_sbs_qids(context))
  }

  sbs_rendering_columns(context) %>%
    map(render_sbs_column_qids) %>%
    unlist()
}
```

This pass does not need to split `R/response_column_render.R` into multiple
files. If the file remains hard to scan after the context refactor, a later
change may move SBS-specific rendering helpers into
`R/response_column_render_sbs.R`.

## Testing

Run the full test suite when possible:

```sh
Rscript -e 'devtools::test()'
```

If the full suite is unavailable, run the targeted files:

```sh
Rscript -e 'testthat::test_file("tests/testthat/test-response_columns.R")'
Rscript -e 'testthat::test_file("tests/testthat/test-loop_question_facts.R")'
Rscript -e 'testthat::test_file("tests/testthat/test-dict_generate.R")'
```

Keep the existing behavioral tests that pin:

- MC text-choice rendering.
- Matrix row ordering.
- already-prefixed Loop and Merge `response_column_qid` values.
- looped text-entry `response_column_qid` normalization.
- SBS matrix, mixed-column, and carried-forward row rendering.
- Timing and file-upload sidecar response columns.
- static, matrix-source, and static-without-source Loop and Merge expansion.
- supported extra-field substitution from static rows and metadata column names.
- dictionary-level Loop and Merge output.

Add direct tests for the new context objects where they protect important
domain distinctions:

```r
test_that("response column render context separates bare and rendering qids", {
  question_fact <- normalise_qualtrics_metadata(
    synthetic_looped_mc_text_raw_metadata()
  )$questions$QID2

  shape <- response_column_shape(question_fact)
  question_type <- question_fact_question_type(question_fact)

  context <- new_response_column_render_context(
    question_fact = question_fact,
    response_column_qid = "x1_QID2",
    shape = shape,
    question_type = question_type
  )

  expect_identical(context$response_column_qid, "x1_QID2")
  expect_identical(context$question_fact$qid, "QID2")
})
```

```r
test_that("Loop expansion context separates current and source facts", {
  normalised_metadata <- normalise_qualtrics_metadata(
    synthetic_loop_and_merge_raw_metadata()
  )

  context <- new_loop_expansion_context(
    question_fact = normalised_metadata$questions$QID2,
    survey_question_facts = normalised_metadata$questions
  )

  expect_identical(context$question_fact$qid, "QID2")
  expect_identical(context$looping_source_fact$qid, "QID1")
  expect_identical(context$looping_qid, "QID1")
})
```

## Error Handling

Do not introduce new user-facing warnings or errors in this refactor.

Preserve the existing row-alignment guard:

```r
stop(
  "Rendered response-column facts are not row-aligned.",
  call. = FALSE
)
```

Preserve fallback behavior for unsupported question types through
`not_applicable_qid()`.

## Out of Scope

This refactor does not:

- change the public API;
- change Variable Dictionary columns or column order;
- add new Loop and Merge structures;
- change Semantic Name generation;
- change Labelled Export matching;
- change validation behavior;
- remove internal functions reported as unused by `goodpractice`;
- update local smoke baselines.

## Acceptance Criteria

- Long coordinator functions in Loop and Merge Support and Response Column ID
  Rendering are split around context-based helpers.
- New internal parameter names use `question_fact`, `survey_question_facts`,
  `looping_source_fact`, and `response_column_qid` where applicable.
- Existing targeted tests for Loop and Merge Support, Response Column ID
  Rendering, and dictionary generation pass.
- New context tests document the distinction between bare QID and
  `response_column_qid`, and between the current question fact and its Loop and
  Merge source fact.
- Existing Loop and Merge text-entry normalization and supported extra-field
  substitution behavior are explicitly preserved.
- No intentional changes occur in rendered Response Column IDs or Variable
  Dictionary output.
