# Use Strict Non-system Response Column Smoke Parity

Status: accepted

The local finalization smoke check will treat every non-system raw response
header as part of the raw-to-dictionary parity obligation. Qualtrics system
columns are the only raw-header exclusion; categories such as
`question_auxiliary` and `unknown` remain useful diagnostics but no longer
remove a raw column from `missing_from_dict`.

Package dictionary generation should continue to render ordinary
question-backed Response Column IDs from Qualtrics metadata rather than from
raw response headers or the response column map. Response-schema metadata may
still be used as a Response-schema Filter for Embedded Data Fields, Scoring
Variables, and Text-analysis Sidecars, where ordinary survey metadata does not
fully identify exported columns.

## Consequences

Strict smoke parity is expected to fail when metadata-only Response Column ID
Rendering misses a non-system raw export column. Those failures are useful
renderer coverage targets, not smoke false positives.

The saved parity artifact should make the checked set explicit as
`non_system_raw_response_columns`, record `system_raw_response_columns`
separately, and include classification details only as diagnostics.
