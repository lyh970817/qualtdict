# pkgcheck Status

This note records the current `pkgcheck` status after the recent fixes for:

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
- missing package website metadata

An authenticated local worktree run reported:

```text
Package has continuous integration checks, but no badges on README
Package coverage is 100%.
R CMD check found no errors.
R CMD check found no warnings.
```

`pkgcheck` discovers GitHub Actions status through the GitHub API and only
performs that lookup when a GitHub token is available in the environment. In an
unauthenticated local shell, the same package state may still report:

```text
Package has no continuous integration checks.
```

Run local `pkgcheck` with a token when checking CI status, for example:

```sh
GITHUB_PAT="$(gh auth token)" Rscript -e 'pkgcheck::pkgcheck(".")'
```

The repository includes the rOpenSci `pkgcheck-action` workflow in
`.github/workflows/pkgcheck.yaml`. That workflow grants explicit `actions: read`
and `contents: read` permissions so GitHub-hosted `pkgcheck` runs can inspect
repository contents and Actions run status.

## Historical Findings

### Repository has no website

Earlier `pkgcheck` runs reported:

```text
Repository has no website
```

The package website is configured at
<https://lyh970817.github.io/qualtdict/>. The site URL is recorded in
`DESCRIPTION`, and `.github/workflows/pkgdown.yaml` builds and deploys the
`pkgdown` site to GitHub Pages.

The current authenticated run reports:

```text
Repository has a website
```

### Default GitHub branch is `main`

Earlier `pkgcheck` runs reported:

```text
Default GitHub branch of 'master' is not acceptable.
```

The current authenticated run reports:

```text
Default branch: main
```

### Long functions

Earlier `goodpractice` runs reported functions over its default length limit,
including:

```text
R/dict_generate.R:65
R/loop_expand.R:4
R/metadata_normalise.R:28
R/response_column_render.R:10
R/response_column_render.R:175
```

The current local `pkgcheck` run reports:

```text
All goodpractice linters passed.
```

### Unused internal functions

Earlier uncached local worktree runs reported unused internal functions,
including:

```text
R/loop_expand.R:400
R/question_facts.R:112
R/raw_response_columns.R:4
R/zzz.R:2
```

The clearly obsolete helpers were removed. The current local `pkgcheck` run
reports:

```text
All goodpractice linters passed.
```

## R CMD Check Findings

After the example/import fixes, direct checks passed. The current local
`pkgcheck` run reports:

```text
R CMD check found no errors.
R CMD check found no warnings.
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

## Remaining Local Caveats

1. `pkgcheck` CI status requires a GitHub token in the local environment.
2. The current local branch is ahead of `origin/main`; GitHub-hosted pkgcheck
   results will reflect the older remote state until these commits are pushed.
3. `pkgcheck` currently reports CI as present but says there are no README
   badges because it fetches the README from GitHub's default branch. The local
   README already includes GitHub Actions badges, so this should clear once the
   local commits are pushed.
