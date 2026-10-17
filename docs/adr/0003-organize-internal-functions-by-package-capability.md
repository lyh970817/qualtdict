# Organize Internal Functions by Package Capability

Status: accepted

qualtdict will organize internal R files around the package's canonical
capabilities and glossary language rather than around legacy helper names, raw
data shapes, or one large file per exported function. Public workflow files may
remain wrapper-oriented where that improves scanability, but the implementation
machinery should live beside the capability it supports.

This means separate internal modules for metadata fetching, metadata
normalisation, question facts, Loop and Merge expansion, Response Column ID
Rendering, Variable Dictionary assembly, Dictionary Variable Name repair,
Semantic Name generation, Validation Findings, Unsupported Structure Findings,
and Labelled Export. Shared dictionary schema accessors may live in a small
shared module because they are used across multiple capabilities.

## Considered Options

- Keep the existing historical file layout.
- Organize files primarily by exported public functions.
- Organize files by data shapes or processing layers, such as raw,
  normalised, rendered, and exported.
- Organize files by package capabilities named in the glossary.

## Consequences

Internal helpers should move toward capability files with names that match the
package language in `CONTEXT.md`. Legacy names such as easy-name generation,
JSON recoding, or QID recoding should not remain as top-level module names when
the package now describes those capabilities as Semantic Name generation,
Variable Dictionary assembly, and Response Column ID Rendering.

The public API remains stable unless a separate API decision says otherwise.
`dict_generate()`, `dict_validate()`, `get_survey_data()`,
`dict_split_blocks()`, `survey_split_blocks()`,
`unsupported_structure_findings()`, and `labelled_export_findings()` keep their
current user-facing contracts during this reorganization.

This layout creates more source files, but it makes ownership clearer: Loop and
Merge expansion stays upstream from Response Column ID Rendering; Validation
Findings stay derived from Variable Dictionaries; Unsupported Structure
Findings stay tied to metadata normalisation; and Labelled Export owns matching
and labelling downloaded survey data. Future changes should prefer extending
the relevant capability module over adding broad utility files or reviving
legacy transformation names.
