# Rename the Variable Naming API

Status: accepted

qualtdict will replace the user-facing `easy_name` terminology with
`semantic_name`, rename the public `dict_generate()` naming argument from `name`
to `variable_name`, and rename the Variable Dictionary output column from
`name` to `variable_name`. It will also rename the existing Variable Dictionary
output column from `qid` to `response_column_id`, because that column identifies
expanded Qualtrics response/import columns rather than only bare Qualtrics QIDs,
and add or retain a separate `qid` column for the bare Qualtrics question
identifier. The Variable Dictionary will always preserve `question_name` as a
Qualtrics naming reference, preserve `semantic_name` only when semantic naming
is selected, use `response_column_id` as the dictionary row provenance key, and
use `variable_name` for the selected analyst-facing name, with `variable_name`
normalised for export safety and uniqueness. The vague
`get_survey_data(skip = TRUE)` export control will be replaced by an explicit
finding-exclusion argument. `dict_validate()` will return a stable structured
validation object rather than switching shape depending on whether findings are
present. `dict_generate()` will always return one Variable Dictionary and
`get_survey_data()` will always return one Labelled Survey Data object; block
splitting will move to separate helpers instead of changing return types with
`split_by_block`. The `get_survey_data()` argument `keys` will be renamed to
`extra_columns` because it controls additional returned columns rather than
block-splitting keys. The `dict_generate()` argument `preprocess` will be
renamed to `semantic_name_preprocess` because it only prepares text for Semantic
Name generation, and this replacement will remain a documented public argument
when the API migration lands. qualtdict will own the
`qualtRics::fetch_survey()`
settings that make dictionary-to-response-column mapping reliable, including
`import_id = TRUE`, `label = FALSE`, `convert = FALSE`, and
`breakout_sets = TRUE`. These are deliberate breaking changes made before
rOpenSci readiness work so that the API, output schema, documentation, and
glossary use one canonical language for Variable Dictionaries and findings.

## Considered Options

- Keep `name = c("question_name", "easy_name")`.
- Use `name = c("question_name", "semantic_name")`.
- Use `name_source = c("question_name", "semantic_name")`.
- Use `variable_name = c("question_name", "semantic_name")`.
- Rename `qid` to `response_column_id`.
- Rename `qid` to `import_id`.
- Use `qid` for the bare Qualtrics question ID and `import_id` for the response
  column identifier.
- Use `qid` for the bare Qualtrics question ID and `response_column_id` for the
  response column identifier.
- Preserve only the selected `variable_name`.
- Preserve `question_name` as a naming reference column, and preserve
  `semantic_name` only when semantic naming is requested.
- Store the selected analyst-facing name in `variable_name`.
- Keep `question_name` raw, but normalise `variable_name` for labelled export
  use.
- Reuse the semantic-name uniqueness repair behaviour for `variable_name` when
  it is sourced from raw `question_name`, adapted to the new
  `response_column_id` identity column.
- Report automatic `variable_name` uniqueness repair as a Validation Finding.
- Include repaired-name variables in Labelled Exports by default.
- Validate the final `variable_name` contract regardless of whether it was
  sourced from `question_name` or `semantic_name`.
- Include the original candidate name in repaired-name Validation Findings.
- Rename `preprocess` to `semantic_name_preprocess`.
- Pass the full intermediate dictionary into `semantic_name_preprocess`.

## Consequences

Examples, tests, snapshots, documentation, argument validation, and generated
manual pages must be updated together. Code that validates, splits, renames, or
exports dictionaries must migrate from the `name` column to `variable_name`, from
the current expanded `qid` column to `response_column_id`, and use `qid` when
question-level Qualtrics identifiers are needed. `response_column_id` is
intentionally used over `import_id` even though qualtdict joins against data
downloaded with `qualtRics::fetch_survey(import_id = TRUE)`: `import_id` is a
fetch/export setting, while the dictionary column's role is to identify the
response column being labelled. Documentation must state that Semantic Names are
readable conveniences and not stability guarantees. Preserving `question_name`
and conditionally preserving `semantic_name` means dictionary outputs contain
the selected analyst-facing `variable_name` and relevant naming references;
downstream code should treat `response_column_id` as the row provenance key,
`variable_name` as the export name, and the other naming columns as reference
metadata when present. `question_name` is retained as the raw Qualtrics naming
reference, not the exported column name. The name-repair behaviour currently
used for generated easy names should become reusable and apply to
`variable_name` regardless of whether the selected naming source is
`question_name` or `semantic_name`. Automatic name repair should be visible as a
Validation Finding because it changes dictionary/export naming, but it is not an
Unsupported Structure Finding. Since `exclude_findings` defaults to `"none"`,
variables with repaired-name findings remain in Labelled Exports unless users
explicitly exclude validation findings. Validation should focus on the final
`variable_name` values because they are the names used in Labelled Exports; raw
`question_name` values remain Qualtrics naming references and may duplicate.
Repaired-name Validation Findings should include the affected
`response_column_id`, the original candidate name, the repaired `variable_name`,
and a reason such as duplicate or unsafe name.
`question_name` remains the canonical option for Qualtrics API `questionName`;
only `easy_name` is replaced. `get_survey_data()` must avoid silently dropping
columns by default; exclusion of variables with findings should be explicit.
Validation results must always expose Validation Findings and level-label
pairings through a stable structure. Variable Dictionary generation and Labelled
Survey Data retrieval should have stable return types; alternate views such as
block splits should be separate operations.
`semantic_name_preprocess` should remain a narrow hook for transforming the text
used to generate Semantic Names, not a general-purpose way to alter
question-level identifiers, response-column identifiers, labels, levels, or the
returned Variable Dictionary structure. It should run only when semantic naming
is requested, but it should receive the full post-normalisation dictionary so
callers can clean naming text conditionally on the stable internal metadata
shape. It may add temporary helper columns for Semantic Name generation, but
non-canonical helper columns should not appear in the final Variable Dictionary.
`get_survey_data(extra_columns = ...)` should name extra raw response columns to
retain in the single returned Labelled Survey Data object. Users may pass
filtering and download options through to `fetch_survey()`, but not options that
would change the response-column naming, labelling, conversion, or multi-value
column breakout contract owned by qualtdict.
