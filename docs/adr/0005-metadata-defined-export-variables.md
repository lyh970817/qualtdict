# Include Metadata-defined Export Variables in Variable Dictionaries

Status: accepted

qualtdict will include Metadata-defined Export Variables in Variable
Dictionaries alongside ordinary question-backed rows. Embedded Data Fields,
Scoring Variables, and Text-analysis Sidecars are first-class Variable
Dictionary rows when Qualtrics metadata and response column metadata define
them. They use the same `response_column_id` provenance key as question-backed
rows, and the public Variable Dictionary schema will add `row_source`
immediately after `response_column_id` to identify the kind of fact that
produced each row.

Initial `row_source` values are `question`, `embedded_data`, `scoring`, and
`text_analysis`.

## Considered Options

- Keep Embedded Data Fields, Scoring Variables, and Text-analysis Sidecars out
  of Variable Dictionaries and retain them only through `extra_columns`.
- Discover these columns only from response-export headers.
- Treat Qualtrics metadata as the gold standard for Variable Dictionary rows,
  as qualtdict already does for question-backed rows.
- Reuse existing columns such as `type` to distinguish non-question rows.
- Add a public `row_source` column.
- Assign Embedded Data Fields to Survey Blocks only when they are exact block
  members.
- Provide a user-controlled Survey Flow adjacency policy for Embedded Data
  Fields.
- Assign Scoring Variables to Survey Blocks by derived questions or nearest
  flow location.
- Keep Scoring Variables unassigned unless future metadata proves exact block
  membership.

## Decisions

`response_column_id` remains the universal exported-column identifier and row
provenance key for every Variable Dictionary row.

`row_source` is a public Variable Dictionary column placed immediately after
`response_column_id`. It is required for every row.

Question-backed rows use `row_source = "question"` and keep the existing
question metadata contract. Their `qid`, `question_name`, `block`, `type`,
`selector`, `sub_selector`, item, level, and label fields continue to be derived
from normalised question facts and Response Column ID Rendering.

Embedded Data Fields use `row_source = "embedded_data"`. They keep `qid = NA`
and `question_name = NA` unless future Qualtrics metadata gives an exact parent
question relationship. Their `variable_name` starts from the raw embedded field
name and participates in the same export-safety and uniqueness repair as every
other Variable Dictionary row.

Scoring Variables use `row_source = "scoring"`. They keep `qid = NA`,
`question_name = NA`, and `block = NA` by default. Scoring metadata is treated
as survey-level or scoring-configuration metadata unless future fixtures prove a
clear single-block relationship.

Text-analysis Sidecars use `row_source = "text_analysis"`. When a sidecar has a
clear parent QID, it inherits that parent question's `qid`, `question_name`, and
`block`. If the parent cannot be determined, those fields remain `NA`.
Text-analysis Sidecars are discovered from the response column map
classification, not from the direct `metadata()` comments payload.

Metadata-defined Export Variables use existing display metadata columns.
`question` carries a human-readable description, while `item`, `level`, and
`label` remain `NA` unless Qualtrics metadata provides real item or value-label
facts.

Metadata-defined Export Variables do not use Semantic Name generation. Even
when `dict_generate(variable_name = "semantic_name")` is requested for
question-backed rows, metadata-defined rows start from their raw field or output
name and then pass through global `variable_name` repair.

Metadata-defined Export Variables participate in `variable_name` safety and
uniqueness repair and in the corresponding Validation Findings. They do not
participate in question-specific level-label validation.

`get_survey_data()` includes metadata-defined rows by default because they are
represented in the Variable Dictionary. `extra_columns` remains only for raw
downloaded columns that are not represented in the Variable Dictionary.

Labelled Export sets variable labels for metadata-defined rows from their
Variable Dictionary display metadata. It does not set value labels for those
rows unless future metadata provides real level-label pairs.

## Embedded Data Block Assignment

`dict_generate()` will expose:

```r
embedded_data_block_assignment = c("none", "previous", "next")
```

The default is `"none"`, which leaves Embedded Data Fields with `block = NA`.

`"previous"` assigns an Embedded Data Field to the nearest previous Survey Block
in Survey Flow when the field has an unambiguous Survey Flow location.

`"next"` assigns an Embedded Data Field to the nearest next Survey Block in
Survey Flow when the field has an unambiguous Survey Flow location.

This policy applies only to Embedded Data Fields with a specific Survey Flow
location. Embedded Data Fields known only from a flat metadata list remain
`block = NA` regardless of the assignment policy.

Embedded Data Field locations are read from the Survey Flow returned by
`fetch_description()`, where Embedded Data nodes include
`EmbeddedData[[i]]$Field`. The Survey Flow returned by `metadata()` can contain
type-only Embedded Data nodes and is not used for field locations. The flat
`metadata()` `embedded_data` list remains the complete source of Embedded Data
Field rows; `fetch_description()` Survey Flow only adds `previous_block` and
`next_block` candidates for those rows.

If `"previous"` or `"next"` is requested and some Embedded Data Fields cannot be
assigned, qualtdict leaves those fields with `block = NA` and emits one
aggregate warning when `quiet = FALSE`. The warning is suppressed when
`quiet = TRUE`.

Scoring Variables do not get an analogous assignment argument because the
available Qualtrics scoring metadata is not located in Survey Flow.

## Block Splitting

`block` remains exact Survey Block metadata or a user-requested Embedded Data
Field assignment from Survey Flow adjacency. qualtdict will not infer Survey
Block membership from nearest flow proximity unless
`embedded_data_block_assignment` explicitly requests it for Embedded Data
Fields.

Rows with `block = NA` are not dropped by block splitting. `dict_split_blocks()`
and `survey_split_blocks()` return an additional `"..unassigned"` split only
when at least one row has `block = NA`.

`survey_split_blocks()` mirrors `dict_split_blocks()`. The `"..unassigned"`
Labelled Survey Data split includes the usual `extra_columns` plus the
unassigned variables represented in the Variable Dictionary.

No public argument is added to change `"..unassigned"` behavior for now.

## Consequences

The public Variable Dictionary schema changes by adding `row_source` immediately
after `response_column_id`.

Dictionary generation must request and normalise additional Qualtrics metadata:
flat embedded data from `metadata()`, Embedded Data Field Survey Flow locations
and scoring metadata from `fetch_description()`, and Text-analysis Sidecars from
response column map classification. Text-analysis parent-QID-derived block
assignment is allowed only when the parent can be determined.

Validation and Labelled Export must treat metadata-defined rows as represented
dictionary rows while avoiding question-specific assumptions about levels,
labels, QIDs, and Survey Blocks.

Block splitting must become safe for `NA` block rows before or alongside adding
metadata-defined rows, because base `split(dict, dict$block)` drops `NA` groups.

Documentation must distinguish exact Survey Block membership from optional
Embedded Data Field assignment by Survey Flow adjacency.
