# Accept the Private `slowraker` Internals Coupling

Status: accepted

qualtdict will keep retrieving six non-exported `slowraker` functions
through `getFromNamespace()` in `R/slowraker.R` rather than eliminate that
coupling now. This is a knowingly accepted risk, reviewed and closed on
2026-08-18, not an oversight or an outstanding action item.

`slowraker_internal()` (`R/slowraker.R`) lazily resolves and memoises
`get_cand_words`, `filter_words`, `process_keyword_df`, `get_pos_tags`,
`handle_pos_error`, and `stop_pos_tags` from the `slowraker` namespace. None
of the six is exported by `slowraker`, and none is stable API: `slowraker`
makes no compatibility promise about them. The concrete breakage mode is
that an internal upstream refactor of `slowraker` — one that does not
touch its public surface and so does not require an API-breaking
`slowraker` release — can silently break qualtdict's RAKE/Semantic Name
generation, because there is no version constraint on `slowraker` in
`DESCRIPTION` that would catch such a change before it reaches this code.

This finding was raised by an internal audit in 2026
(`docs/ropensci/resubmission-audit-2026-08-15.md`), not by any rOpenSci
reviewer or editor: issue #572 never had external reviewers assigned, and
`getFromNamespace` appears nowhere in the 2023 editor/`pkgcheck` feedback on
that issue. It was the maintainer's call to make, not an unresolved editor
request — this has been misread once already, so it is stated explicitly
here.

## Why accepted rather than fixed

- The RAKE/Semantic Name feature this coupling supports is optional, not
  core: `openNLP`, `slowraker`, and `SnowballC` moved from `Imports` to
  `Suggests` in commit `e144a55` ("Move the Semantic Name dependency stack
  to Suggests"), and the feature is guarded with `requireNamespace()`
  checks accordingly.
- The README already describes the package as not being a stable Semantic
  Name generator, so users of this feature are not being promised
  implementation stability qualtdict itself does not have.
- Because of the two points above, the blast radius of an upstream
  `slowraker` refactor is one optional, already-caveated feature, not the
  package's core dictionary/extraction/labelling job.

## Considered Options

- Vendor the six functions locally, with attribution to `slowraker` and
  dedicated tests. Not done now: it duplicates and has to track
  `slowraker`'s implementation, and was judged not worth doing for an
  optional feature at this time.
- Work with the `slowraker` maintainers to export a supported public API
  covering these six functions, then depend on that. Not pursued now: it
  depends on upstream's willingness and timeline, outside qualtdict's
  control.
- Redesign Semantic Name generation so the private internals are
  unnecessary. Not pursued now: a larger design change than the finding by
  itself justifies.
- Accept the coupling as a documented, reviewed risk and leave
  `R/slowraker.R` unchanged. **Chosen**, given the mitigating context above.

## Consequences

`R/slowraker.R` is unchanged: it still retrieves the same six private
`slowraker` functions with `getFromNamespace()`, at the `slowraker_internal`
memoised resolver. A code comment there records the accepted risk and
points back to this ADR so it is not mistaken for an oversight and
re-investigated from scratch.

This should be revisited if either condition changes: an upstream
`slowraker` release changes or removes any of the six functions (which
would be a live breakage report, not a hypothetical one), or Semantic Name
generation is promoted from an optional, caveated feature to core
qualtdict functionality, at which point the current mitigating context no
longer holds and one of the three exit routes above should be taken.
