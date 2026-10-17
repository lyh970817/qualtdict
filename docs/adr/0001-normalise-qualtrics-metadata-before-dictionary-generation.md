# Normalise Qualtrics Metadata Before Dictionary Generation

Status: accepted

qualtdict will introduce a package-owned intermediate representation of
Qualtrics question metadata before generating Variable Dictionary rows. The
current direct transformation from raw Qualtrics metadata to dictionary rows is
too hard to reason about for full Loop and Merge Support, because loop fields,
choices, subquestions, matrix columns, text entries, timing fields, and naming
components are handled in one pass.

## Considered Options

- Keep patching the direct transformation in `recode_json()`.
- Normalise raw Qualtrics metadata first, then generate Variable Dictionary rows
  from the normalised representation.

## Consequences

The normalised representation becomes the main unit for testing Qualtrics
question structures. Dictionary generation, Semantic Name generation, validation,
and Labelled Export behaviour should be tested against this representation
instead of relying only on end-to-end recorded API fixtures. Unsupported
Structure Findings produced during metadata normalisation should travel with the
generated Variable Dictionary as attributes; a helper accessor should expose
them so users do not need to depend on the raw attribute. Validation Findings
remain derived from the dictionary and are computed on demand.
