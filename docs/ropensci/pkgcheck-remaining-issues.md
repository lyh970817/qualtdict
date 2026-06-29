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
- whole-package roxygen imports for `dplyr`, `purrr`, `stringr`, and
  `slowraker`
- duplicate roxygen parameter documentation that could use `@inheritParams`
- examples that used non-running roxygen blocks
- clearly unused internal helpers in Response Column ID Rendering and
  Validation Finding code
- the `goodpractice` coverage advisory for uncovered package lines

The exact finding set can vary depending on whether the current shell has
reloaded the updated Nix/direnv environment. In shells that have not reloaded
`shell.nix`, `pkgcheck` may still print `pdflatex not found! Not building PDF
manual.`

An uncached local worktree run after the fixes above reported:

```text
Package coverage is 85.9%.
R CMD check found no errors.
R CMD check found no warnings.
```

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

The repository has GitHub Actions files. The standard R package workflows now
target the `main` branch, and the repository includes the rOpenSci
`pkgcheck-action` workflow in `.github/workflows/pkgcheck.yaml`.

### Default GitHub branch is `main`

`pkgcheck` reports:

```text
Default GitHub branch of 'master' is not acceptable.
```

Fix requires changing the default branch on GitHub from `master` to `main`,
then updating local branch names and any branch references in workflows,
documentation, and repository settings.

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

Earlier uncached local worktree runs reported unused internal functions,
including:

```text
R/loop_expand.R:400
R/question_facts.R:112
R/raw_response_columns.R:4
R/zzz.R:2
```

The clearly obsolete helpers were removed. The `%||%` helper in
`R/question_facts.R` is used as an infix helper and may still be reported by
some static checks as a false positive.

## R CMD Check Findings

After the example/import fixes, a direct `devtools::check(args = "--no-manual")`
passed with:

```text
0 errors | 0 warnings | 2 notes
```

The remaining notes in the local worktree were:

```text
checking for future file timestamps ... NOTE
unable to verify current time

checking for hidden files and directories ... NOTE
Found the following hidden files and directories:
  .git
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

## Resolved Duplicate Function Name

Earlier `pkgcheck` runs reported:

```text
The following function name is duplicated in other packages:
- `get_survey_data` from ipanema
```

The exported API was renamed to `fetch_labelled_survey_data()` without a
deprecated exported alias to resolve this finding. See ADR 0006.

## Suggested Prioritization

1. Fix hook/runtime configuration separately from package audit findings.
2. Add or align recognized CI checks.
3. Verify GitHub reports `main` as the default branch.
4. Add package website metadata.
5. Verify the remaining unused-internal-function findings.
6. Treat long-function findings as refactor tickets, not one-off lint cleanup.
