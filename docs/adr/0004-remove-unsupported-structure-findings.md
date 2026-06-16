# Remove Unsupported Structure Findings

Status: accepted

qualtdict will remove Unsupported Structure Findings as a public findings
surface because the remaining cases were defensive Loop and Merge consistency
checks rather than observed user-facing survey structures. Validation Findings
remain the public consistency screen for generated Variable Dictionaries, and
Labelled Export Findings remain the public screen for dictionary-to-response
matching after download.

## Considered Options

- Keep `unsupported_structure_findings()` as an always-empty compatibility
  surface.
- Generalise Unsupported Structure Findings to also cover Response Column
  Rendering fallbacks.
- Remove Unsupported Structure Findings and keep renderer fallbacks local to
  Response Column ID Rendering.

## Consequences

`unsupported_structure_findings()` and `exclude_findings = "unsupported"` are
removed from the public API. Question shapes without a specific response-column
renderer may still warn and fall back to the bare QID, but that is local
Response Column ID Rendering behaviour rather than a structured Finding.
