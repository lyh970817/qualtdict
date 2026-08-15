# rOpenSci Resubmission Audit

Date: 2026-08-15

## Executive summary

`qualtdict` passes its substantive package checks in both the pre-migration
environment and the new pinned Nix-flake development environment. The migration
from `shell.nix` to [`flake.nix`](../../flake.nix) and
[`flake.lock`](../../flake.lock) is integrated on `main` as commit
`99df75da745a08f8435f44ddf31aee3fcb2a0f1e` (`Pin development environment with
a flake`). At the time of the migration handoff, `main` was one commit ahead of
`origin/main`; the commit had not been pushed.

The previous upstream submission was
[rOpenSci software-review issue #572](https://github.com/ropensci/software-review/issues/572),
not a pull request. It was closed administratively after inactivity, before
external reviewers were assigned. Most of the automated and editor findings from
that submission have since been fixed. Two stale direct dependency/import
findings remain (`haven` and `crul`), and a more significant issue of the same
general kind is now visible: the implementation retrieves six private
`slowraker` functions from its namespace.

This report records the audit and reproducibility work only. No package-source
fixes or exported-behaviour changes were made as part of this work.

## Package-check status

### Baseline before the flake migration

The old `shell.nix` supplied development tools but did not supply every R package
listed under Imports and Suggests. After the declared dependencies were installed
in the existing ignored project library, the baseline test suite passed:

```text
devtools::test()
FAIL 0 | WARN 0 | SKIP 1 | PASS 1158
```

The one skip is the expected check for unavailable local smoke artifacts, not a
package failure.

The configured pre-push checks also passed:

```text
r-cmd-check-no-manual: passed
pkgcheck:             passed
```

A direct uncached `pkgcheck::pkgcheck(use_cache = FALSE)` reported:

- R CMD check: zero errors and zero warnings;
- test coverage: 100%; and
- all `goodpractice` checks passed.

### Checks in the pinned flake

The flake evaluated for every declared system without building all outputs:

```sh
nix flake check --all-systems --no-build
```

Result: passed.

The full package tests passed inside the development shell:

```sh
nix develop --command Rscript -e 'devtools::test()'
```

```text
FAIL 0 | WARN 0 | SKIP 1 | PASS 1158
```

Both configured pre-push checks passed inside the flake:

```sh
nix develop --command \
  pre-commit run --hook-stage pre-push --all-files r-cmd-check-no-manual
```

```text
passed
```

```sh
nix develop --command \
  pre-commit run --hook-stage pre-push --all-files pkgcheck
```

```text
passed
```

A fresh `pkgcheck::pkgcheck(use_cache = FALSE)` also completed successfully in
the flake. The full hook suite other than Air passed:

```sh
SKIP=air-format pre-commit run --all-files
```

This covered roxygen, DESCRIPTION normalization, spelling, lintr, README
consistency, parsability, dependency checks, the complete test suite, and
`goodpractice`.

### Check limitations and qualifications

An unrestricted `pre-commit run --all-files` is not fully green because the
pinned Air formatter wants to reformat the pre-existing
[`tools/prune-pkgdown-internal-pages.R`](../../tools/prune-pkgdown-internal-pages.R).
The formatter-generated edit was reverted because it is unrelated to the
environment migration and changing that existing program was outside the
approved scope. This is formatting drift, not a package-check failure.

The local exported-behaviour smoke replay was not run. The repository's smoke
instructions reserve it for feature work that affects or could affect exported
behaviour; the migration changed only the development environment and supporting
documentation.

## Migration integration state

The Nix migration was integrated by a clean fast-forward:

- branch: `main`;
- commit: `99df75da745a08f8435f44ddf31aee3fcb2a0f1e`;
- subject: `Pin development environment with a flake`;
- state at handoff: one commit ahead of `origin/main`, not pushed;
- conflicts or merge commit: none; and
- tracked working tree at migration handoff: clean.

The migration commit:

- added [`flake.nix`](../../flake.nix);
- added [`flake.lock`](../../flake.lock);
- removed `shell.nix`;
- updated [`.Rbuildignore`](../../.Rbuildignore);
- updated [`.github/CONTRIBUTING.md`](../../.github/CONTRIBUTING.md);
- updated [`package-standard-checklist.md`](package-standard-checklist.md); and
- updated [`pkgcheck-remaining-issues.md`](pkgcheck-remaining-issues.md).

The following pre-existing untracked local artifacts were preserved and not
committed:

- `.claude/`;
- `.local/`;
- `docs/refactor-structure-visualization.html`; and
- `tools/local-finalize-smoke-surveys-all32.json`.

## Reproducibility design

The old `shell.nix` was removed so that the package has one development entry
point and does not fall back to an ambient `<nixpkgs>` channel.

The lock and package definition pin:

- `nixpkgs` revision:
  `0e251e24a4f24e036a084b6b4b2d2491af4167f4`;
- `nixpkgs` NAR hash:
  `sha256-yNJd40f11EzXBjSByCB7IPpeFFAdeoSKKM67dGkfFoU=`;
- lock input date: 2026-08-13;
- `pkgcheck` revision:
  `edef80ac8544b2b5d002f4ceafbd4cb45adf05ef`; and
- `pkgcheck` source hash:
  `14irbaaz5fxf06ygc0cs64ap83k9jal0m0bz3vc2bjsjl33h8b5w`.

`pkgcheck` is fixed separately in [`flake.nix`](../../flake.nix) because it was
not available from the selected pinned `nixpkgs` revision.

The realised Linux development environment contains:

- R 4.6.1;
- devtools 2.5.2;
- testthat 3.3.2;
- pkgcheck 0.1.3.13;
- goodpractice 1.1.0;
- pkgstats 0.2.3; and
- srr 1.0.0.

The pinned Nix closure provides package Imports, Suggests, and review/development
tools, as well as the JDK, TeX, V8, `libdeflate`, and the system libraries needed
for clean R, rJava, and pre-commit builds. The ignored `.R-lib` remains available
as an optional project-local overlay for manually installed packages; normal
development no longer depends on populating it first.

Declared flake systems are:

- `x86_64-linux`;
- `aarch64-linux`; and
- `aarch64-darwin`.

All three outputs evaluated successfully. Only `x86_64-linux` was built and used
for the R and package checks. `x86_64-darwin` is intentionally not declared
because the pinned `nixpkgs` revision no longer supports that platform.

The normal development entry point is:

```sh
nix develop
```

Deliberate dependency updates use:

```sh
nix flake update
```

The resulting `flake.lock` should then be reviewed and committed.

## Previous rOpenSci submission

The previous submission was
[rOpenSci software-review issue #572](https://github.com/ropensci/software-review/issues/572):

- opened on 2023-02-02;
- closed on 2024-06-20;
- no external reviewers were assigned;
- closure followed prolonged inactivity and non-response; and
- the closure was neither an acceptance nor a substantive rejection.

The actionable feedback came from the
[automated `pkgcheck` report](https://github.com/ropensci/software-review/issues/572#issuecomment-1413943626)
and
[handling editor Mauro Lepore's initial assessment](https://github.com/ropensci/software-review/issues/572#issuecomment-1426701494).
There are no external reviewer reports to revisit.

Current process references are the
[rOpenSci author guide](https://devguide.ropensci.org/softwarereview_author.html),
[editor guide](https://devguide.ropensci.org/softwarereview_editor.html),
[editor checklist](https://devguide.ropensci.org/editortemplate.html), and
[reviewer guide](https://devguide.ropensci.org/softwarereview_reviewer.html).

## Status of the historical findings

| Historical review point | Current status | Current assessment and evidence |
|---|---|---|
| Suggest three reviewers or describe the required expertise. | Process issue, unresolved | No reviewer was ultimately assigned. A new submission should provide plausible candidates or a precise expertise description covering survey research, Qualtrics metadata, and R data-labelling workflows. |
| Explain or remove the direct `haven` dependency used to support `sjlabelled::set_labels()`. | **Unfixed** | [`haven` remains an Import](../../DESCRIPTION#L21), and [`haven::read_xpt` remains imported](../../NAMESPACE#L28), but there is no executable `read_xpt()` use in package or test code. The stale roxygen declaration is in [`survey_var_recode()`](../../R/labelled_export.R#L338), whose implementation calls [`set_label()` and `set_labels()`](../../R/labelled_export.R#L365). |
| Replace whole-package imports of `dplyr`, `purrr`, and `stringr`. | **Fixed** | The current [`NAMESPACE`](../../NAMESPACE) uses individual `importFrom()` declarations and the implementation also uses explicit namespace calls where appropriate. |
| Resolve automated call/import findings covering `crul`, `dplyr`, `haven`, `purrr`, `slowraker`, `SnowballC`, and `stringr`. | **Partly fixed** | Most calls/imports were corrected. `haven` remains stale, and [`crul::Async` remains imported](../../NAMESPACE#L11) without executable use. |
| Avoid `library(vcr)` in tests. | **No longer applicable** | The current package has no `vcr` dependency, `library(vcr)`, or `vcr::` call. |
| Suppress noisy test output. | **Fixed** | The full suite now runs without warnings. `dict_validate()` defaults to quiet operation and that behaviour is tested. |
| Resolve or suppress 591 test warnings. | **Fixed** | The current full suite reports zero warnings and 1,158 passing expectations. |
| Add an `.Rproj` file. | Unfixed but obsolete/non-blocking | There is no `.Rproj`, but current rOpenSci guidance does not require RStudio or an RStudio project file. |
| Respond to editor follow-ups. | Historical process issue | The old submission closed because communication stopped. A new submission needs an actively monitored discussion channel. |

## Analogous current concerns

These are audit recommendations only. No package source was patched.

### 1. High priority: private `slowraker` implementation dependencies

[`R/slowraker.R`](../../R/slowraker.R#L172) retrieves six non-exported functions
with `getFromNamespace()` and stores them in a local `slowraker2` structure:
`get_cand_words`, `filter_words`, `process_keyword_df`, `get_pos_tags`,
`handle_pos_error`, and `stop_pos_tags`.

This is the most important current issue of the same general nature as the
editor's earlier dependency concern. The package is coupled to implementation
details that `slowraker` does not expose as stable API. An internal upstream
refactor can therefore break `qualtdict` without an API-breaking release.

The design should choose one of these routes:

1. Own the required implementation locally, with attribution and focused tests.
2. Work with `slowraker` to expose a supported public API, then depend on it.
3. Redesign Semantic Name generation so the private internals are unnecessary.

### 2. Medium priority: stale `crul` dependency

[`crul` remains an Import](../../DESCRIPTION#L18), and `crul::Async` remains
imported through [`R/utils.R`](../../R/utils.R#L10) and
[`NAMESPACE`](../../NAMESPACE#L11). There is no executable use of `Async` in the
package or tests. This appears to be the same class of unnecessary direct
dependency as `haven`.

### 3. Medium priority: make an explicit Semantic Name dependency decision

[`openNLP`, `slowraker`, and `SnowballC` are mandatory Imports](../../DESCRIPTION#L22).
However:

- the public Semantic Name route invokes the local RAKE implementation with
  [`stop_pos = NULL`](../../R/semantic_name.R#L88);
- the [`openNLP` tagger branch](../../R/slowraker.R#L130) is therefore not
  exercised by that public route; and
- the README describes the package as not being a
  [stable Semantic Name generator](../../README.Rmd#L68).

Because `openNLP` brings a Java/rJava/JDK installation burden, the package should
decide on design merits whether Semantic Name generation is core functionality
that justifies mandatory dependencies, or an optional feature that should be
isolated behind Suggests or a smaller owned implementation.

### 4. Medium priority: package size and internal complexity

Current `pkgcheck` is green, but reports notable size statistics:

- 4,943 R lines of code: 97.2nd percentile;
- 9,077 test lines: 99.4th percentile;
- 395 functions: 99.1st percentile;
- 388 non-exported functions: 99.8th percentile; and
- 14.1 functions per R file: 96.9th percentile.

The largest source files at the time of the audit were:

- `R/response_column_render.R`: 794 lines;
- `R/loop_expand.R`: 735 lines;
- `R/response_column_render_sbs.R`: 552 lines; and
- `R/labelled_export.R`: 510 lines.

This is not a check failure. Current rOpenSci editor guidance nevertheless asks
editors to inspect noteworthy line and function counts. A resubmission should
either explain why the existing decomposition is appropriate or reorganize
coherent internal modules to make review easier.

### 5. Low priority: formatter drift

Air would reformat
[`tools/prune-pkgdown-internal-pages.R`](../../tools/prune-pkgdown-internal-pages.R).
This does not affect package correctness, but resolving it would make the literal
`pre-commit run --all-files` command fully green.

### 6. Low priority: stale `LazyData` metadata

[`DESCRIPTION` declares `LazyData: true`](../../DESCRIPTION#L44), while current
`pkgcheck` reports no internal package data. This is harmless but unnecessary
metadata.

## Recommended resubmission sequence

Based on the design merits of the alternatives, the recommended order is:

1. Decide how to eliminate reliance on private `slowraker` internals.
2. Remove the stale `haven` and `crul` dependencies/imports if no supported
   behaviour needs them.
3. Decide whether Semantic Name generation and its Java-heavy dependency chain
   should remain mandatory.
4. Resolve the low-risk Air and `LazyData` items.
5. Prepare a concise architectural explanation for the package's unusually
   large internal implementation and test suite.
6. Run authenticated `pkgcheck`, R CMD check, and the full pinned pre-commit
   suite again.
7. Open a fresh rOpenSci submission rather than trying to revive issue #572.

Implementing steps 1--4 would modify existing package or tool source and should
begin only after explicit approval.
