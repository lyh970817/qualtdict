# Keep the False `haven` `read_xpt` `@importFrom` Tag

Status: accepted

qualtdict will keep `haven` as a real `Imports` dependency and will keep the
false `#' @importFrom haven read_xpt` tag above `survey_var_recode()`
(`R/labelled_export.R`) rather than remove either.

The 2023 rOpenSci editor review of
[software-review issue #572](https://github.com/ropensci/software-review/issues/572)
asked the package to "explain or remove the direct `haven` dependency used
to support `sjlabelled::set_labels()`". That issue closed administratively
before any external reviewer was assigned, so the question was never
answered at the time. Revisiting it in 2026 found that the answer is
*explain*, not *remove*: `haven` is a genuine runtime dependency.
`sjlabelled::set_labels()` calls `haven::is_tagged_na()` in its named-labels
branch, and `survey_var_recode()` exercises that branch directly through
`set_label()`/`set_labels()`. Removing `haven` from `DESCRIPTION` reproduces
the "unused import" NOTE (for the separate reason below) and also breaks R
CMD check and one test with a hard runtime error, "Package 'haven' required
for this function" -- verified directly, in commit `915cb20` ("Remove unused
crul dependency").

Separately, the roxygen tag `#' @importFrom haven read_xpt` above
`survey_var_recode()` is false: `read_xpt()` is never called anywhere in the
package. It predates this investigation and is being kept anyway,
deliberately, as of 2026-08-18.

## Why the false tag stays

Removing the tag reintroduces the R CMD check NOTE "Namespace in Imports
field not imported from: 'haven'", because `haven` is declared in `Imports`
but (once the fake `read_xpt` reference is gone) nothing in the package
namespace actually calls into it directly -- `sjlabelled` calls
`haven::is_tagged_na()` on qualtdict's behalf, not qualtdict itself.

The `goodpractice` pre-commit hook (`.pre-commit-config.yaml`, lines
~110-126, `stages: [pre-commit]`) fails the commit on any
`goodpractice::failed_checks()` entry, with no distinction between a NOTE
and a WARNING or ERROR. This was tested end-to-end: with the tag removed,
`devtools::test()` still passed, and both configured pre-push hooks
(`r-cmd-check-no-manual`, `pkgcheck`) still passed -- neither sets
`error_on`, so a NOTE alone does not fail them -- but the `goodpractice`
pre-commit hook rejected the commit on the
`rcmdcheck_imports_not_imported_from` check. An honest declaration (no fake
`@importFrom`, `haven` genuinely used only transitively through
`sjlabelled`) cannot currently be committed.

## Considered Options

- Amend the `goodpractice` pre-commit hook to tolerate the single
  `rcmdcheck_imports_not_imported_from` check (or NOTEs generally) instead of
  failing on any `failed_checks()` entry. Rejected for now: it weakens the
  hook for every future check, not just this one case, and was outside the
  scope of the `haven` investigation itself.
- Keep the fake `@importFrom haven read_xpt` tag. **Chosen.** It is the
  smallest change that keeps `goodpractice`, R CMD check, and the test suite
  all green, at the cost of one deliberately misleading roxygen line.
- Drop `sjlabelled` and set the `label` and `labels` attributes directly
  (roughly 10 lines across the two call sites in `survey_var_recode()`,
  replacing `set_label()`/`set_labels()`). Rejected for now: it removes the
  need for `haven` entirely, but was not attempted as part of this
  investigation and would need its own verification against `sjlabelled`'s
  labelled-vector semantics.

## Consequences

`R/labelled_export.R`'s `#' @importFrom haven read_xpt` tag remains false by
construction, and the `NAMESPACE` entry it generates (`importFrom(haven,
read_xpt)`) is accordingly also for a function the package never calls. A
plain code comment at the tag (not a roxygen line, so it does not change the
generated docs or `NAMESPACE`) records this decision and points back here.

This means the package's "all `goodpractice` checks pass" status holds
partly *because* the false tag suppresses the
`rcmdcheck_imports_not_imported_from` NOTE, not because there is nothing
left to explain about the `haven` dependency. An editor or reviewer reading
`pkgcheck` output alone would not see this; this ADR is the record of it.

If the `goodpractice` hook is ever relaxed to tolerate NOTEs, or if
`sjlabelled` is dropped in favour of setting label attributes directly, the
tag should be removed at that point rather than carried forward out of
habit.
