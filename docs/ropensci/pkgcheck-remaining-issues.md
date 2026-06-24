# Remaining pkgcheck Issues

This note records the remaining `pkgcheck` findings after the recent fixes for:

- missing examples on exported helper functions
- missing `utils::head` import
- missing TeX support for PDF manual generation in the project Nix environment
- single-quoted package/software names in `DESCRIPTION`
- missing `NEWS.md`
- missing explicit `@noRd` tag in `R/slowraker.R`
- simple internal code-quality findings for `anyDuplicated()`, `inherits()`,
  and obsolete `stringsAsFactors`
- mechanical style/lint findings, including literal string matching, condition
  simplification, line wrapping, test expectation precision, and related
  goodpractice cleanups

The exact finding set can vary depending on whether the current shell has
reloaded the updated Nix/direnv environment. In shells that have not reloaded
`shell.nix`, `pkgcheck` may still print `pdflatex not found! Not building PDF
manual.`

## Repository Readiness Findings

### Repository has no website

`pkgcheck` reports:

```text
Repository has no website
```

Likely fix: add a package website, typically with `pkgdown`, and expose the
site URL in the package metadata.

### Package has no continuous integration checks

`pkgcheck` reports:

```text
Package has no continuous integration checks.
```

The repository has GitHub Actions files, but `pkgcheck` did not recognize them
as satisfying its check in the latest local run. This may require aligning the
workflow names/triggers with what `pkgcheck` expects, or confirming whether the
check is being run against a local checkout state that lacks remote CI metadata.

### Default GitHub branch is `master`

`pkgcheck` reports:

```text
Default GitHub branch of 'master' is not acceptable.
```

Fix requires changing the default branch on GitHub, usually from `master` to
`main`, then updating local branch names and any branch references in workflows,
documentation, and repository settings.

## Package Metadata Findings

### Whole-package imports

`goodpractice` reports:

```text
not import packages as a whole, as this can cause name clashes between the
imported packages, especially over time as packages change. Instead, import
only the specific functions you need.
```

Likely fix: replace whole-package roxygen imports with targeted `@importFrom`
directives, then regenerate `NAMESPACE`.

## Documentation Findings

### Duplicate parameter documentation should use `@inheritParams`

`pkgcheck` reports duplicate parameter docs at:

```text
R/dict_validate.R:38
R/get_survey_data.R:54
R/split_blocks.R:20
```

Likely fix: define canonical parameter documentation in one roxygen block and
use `@inheritParams` where repeated.

### Examples should avoid `\dontrun`

The latest run no longer reported missing examples, but still noted:

```text
Examples should not use `\dontrun` unless really necessary.
```

Likely fix: review generated `.Rd` examples and convert `\dontrun{}` examples
to runnable examples, `\donttest{}` examples, or prose where appropriate.

## Code Quality Findings

### Long functions

`goodpractice` reports functions over its default length limit, including:

```text
R/dict_generate.R:65
R/loop_expand.R:4
R/metadata_normalise.R:28
R/response_column_render.R:10
R/response_column_render.R:175
```

Likely fix: split the longest functions around existing domain seams. This is a
refactor and should be handled separately from audit configuration.

### Unused internal functions

`goodpractice` reports unused internal functions, including:

```text
R/loop_expand.R:311
R/question_facts.R:112
R/raw_response_columns.R:4
R/response_column_render.R:409
R/validation_findings.R:159
```

Likely fix: verify whether these are genuinely dead code. Remove them if dead,
or add tests/call sites if they are intended extension points.

### Coverage below pkgcheck expectation

`pkgcheck` reports package coverage around:

```text
86%
```

It still flags uncovered lines. Fixing this requires targeted tests for the
reported uncovered branches rather than broad snapshot-only tests.

## R CMD Check Findings

After the example/import fixes, a direct `devtools::check(args = "--no-manual")`
passed with:

```text
0 errors | 0 warnings | 1 note
```

The remaining note was:

```text
checking for future file timestamps ... NOTE
unable to verify current time
```

In hook runs where the active environment had not reloaded TeX Live from
`shell.nix`, `pkgcheck` still reported:

```text
pdflatex not found! Not building PDF manual.
R CMD check found 1 error.
R CMD check found 1 warning.
```

The project-local environment has since been updated with TeX Live support in
`shell.nix`. Existing shells may need:

```sh
direnv reload
```

or a fresh shell before `pdflatex` is visible.

## Duplicate Function Name

`pkgcheck` reports:

```text
The following function name is duplicated in other packages:
- `get_survey_data` from ipanema
```

This may be acceptable if the exported API name is intentional. If avoiding the
finding is important, the fix would be an API rename with lifecycle/deprecation
planning.

## Suggested Prioritization

1. Fix hook/runtime configuration separately from package audit findings.
2. Add or align recognized CI checks.
3. Decide whether to rename the default branch from `master` to `main`.
4. Add package website metadata.
5. Fix low-risk documentation issues such as duplicate parameter docs with
   `@inheritParams`.
6. Treat long-function findings as refactor tickets, not one-off lint cleanup.
