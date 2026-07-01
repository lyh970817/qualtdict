# Generate a Variable Dictionary for a survey

Generate a Variable Dictionary from Qualtrics survey metadata retrieved
via `qualtRics`.

## Usage

``` r
dict_generate(
  surveyID,
  variable_name = c("question_name", "semantic_name"),
  name = NULL,
  block_pattern = NULL,
  block_sep = ".",
  semantic_name_preprocess = NULL,
  preprocess = NULL,
  embedded_data_block_assignment = c("none", "previous", "next"),
  quiet = TRUE
)
```

## Arguments

- surveyID:

  String. Unique Qualtrics survey ID. Live metadata retrieval requires
  Qualtrics credentials configured for `qualtRics`.

- variable_name:

  String. Source for the final `variable_name` column in the Variable
  Dictionary. Use `question_name` for the raw Qualtrics Question Name or
  `semantic_name` for a generated Semantic Name based on question text
  and response metadata.

- name:

  Deprecated compatibility alias for `variable_name`. The legacy
  `easy_name` value is accepted as `semantic_name` with a warning.

- block_pattern:

  Function. A function that given the name of a Survey Block, returns a
  Block Prefix to prepend to Semantic Names in that block. Defaults to
  `NULL`.

- block_sep:

  String. Separator between variable names and block prefixes returned
  by `block_pattern`. Defaults to ".".

- semantic_name_preprocess:

  Function. An optional function that receives the full
  post-normalisation dictionary and returns a modified dictionary for
  Semantic Name generation. It runs only when
  `variable_name = "semantic_name"`. Temporary helper columns added by
  this function are not included in the returned Variable Dictionary.

- preprocess:

  Deprecated compatibility alias for `semantic_name_preprocess`.

- embedded_data_block_assignment:

  String. Survey Flow adjacency policy for assigning Embedded Data
  Fields to Survey Blocks. Use `"none"` to leave Embedded Data Fields
  unassigned, `"previous"` to assign fields with an unambiguous Survey
  Flow location to the nearest previous Survey Block, or `"next"` to
  assign them to the nearest next Survey Block.

- quiet:

  Boolean. If `TRUE`, suppress routine progress messages and progress
  bars. Defaults to `TRUE`.

## Value

A Variable Dictionary: a `qualtdict` data frame.

## Details

The returned Variable Dictionary preserves `response_column_id` as the
downloaded Response Column ID, `qid` as the bare QID, `row_source` as
the Dictionary Row Source, `question_name` as the raw Qualtrics naming
reference, and `variable_name` as the final export-safe Dictionary
Variable Name used by Labelled Survey Data. Question-backed rows use
`row_source = "question"`. Flat Embedded Data Fields defined by
Qualtrics metadata use `row_source = "embedded_data"`. Scoring Variables
defined by survey description metadata use `row_source = "scoring"`.
Text-analysis Sidecars discovered from Response Column Map
Classification use `row_source = "text_analysis"` and inherit parent
`qid`, `question_name`, and `block` when a clear parent QID can be
determined. Embedded Data Fields remain unassigned by Survey Block
unless `embedded_data_block_assignment` explicitly requests Survey Flow
adjacency assignment. That assignment is based on Survey Flow returned
by `fetch_description()`, where Embedded Data nodes carry field
payloads; the type-only Survey Flow returned by `metadata()` is not used
for Embedded Data Field locations. Scoring Variables remain unassigned
by Survey Block.

When `variable_name = "semantic_name"`, the Variable Dictionary also
includes `semantic_name`. Semantic Names are readable best-effort
conveniences generated from survey text and metadata; they are not
stable guarantees across package versions or survey text changes. For
long text, Semantic Names select important words from ranked keywords
and preserve those selected words in the order they appear in the naming
text.

## Examples

``` r
if (interactive()) {
  survey_id <- "SV_XXXXXXXXXXXXXXXX"

  # Create a function for \code{block_pattern}
  # that returns the first three letters of a string
  block_pattern <- function(x) {
    substring(x, 1, 3)
  }


  mydict <- dict_generate(survey_id,
    variable_name = "semantic_name",
    block_pattern = block_pattern,
    block_sep = "."
  )
}
```
