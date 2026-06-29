# Repository Guidelines

## Project Structure & Module Organization

This is an R package for generating Qualtrics variable dictionaries and
labelled data exports. Core code lives in `R/`, with one file per main
feature or helper area, such as `R/dict_generate.R`,
`R/dict_validate.R`, and `R/get_survey_data.R`. Package metadata is in
`DESCRIPTION`, exports are in `NAMESPACE`, and generated help files are
in `man/`. Tests live under `tests/testthat/`; snapshots are in
`tests/testthat/_snaps/`. Vignettes are in `vignettes/`, and `README.md`
is generated from `README.Rmd`, so edit the `.Rmd` source.

## Build, Test, and Development Commands

- `Rscript -e 'devtools::install_dev_deps()'`: install package
  development dependencies.
- `Rscript -e 'devtools::document()'`: regenerate `NAMESPACE` and
  `man/*.Rd` from roxygen comments.
- `Rscript -e 'devtools::test()'`: run the `testthat` suite.
- `pre-commit run --hook-stage pre-push r-cmd-check-no-manual`: run the
  configured local equivalent of R CMD check.
- `Rscript -e 'rmarkdown::render("README.Rmd")'`: update `README.md`
  after README source changes.
- `pre-commit run --all-files`: run configured hooks, including roxygen,
  lintr, spelling, dependency checks, and README rendering.

## Coding Style & Naming Conventions

Follow tidyverse R style: two-space indentation, clear snake_case names,
and readable pipe chains. Keep exported functions documented with
roxygen2 Markdown comments. Prefer helpers in `R/utils.R` or nearby
feature files when they are feature-specific. Do not restyle unrelated
code. The pre-commit configuration runs `lintr`, `parsable-R`,
`no-browser-statement`, and `no-debug-statement`; code should pass these
before review.

## Testing Guidelines

Tests use `testthat` edition 3 and run through `tests/testthat.R`. Name
new tests `tests/testthat/test-<feature>.R`. For stable structured
output, add or update snapshots in `tests/testthat/_snaps/`. Keep
ordinary tests offline with synthetic dictionaries and synthetic
metadata. Do not commit Qualtrics API cassettes or Participant Response
Data; live Qualtrics checks belong in explicit local-only finalization
tooling, not CI.

## Local Finalization Smoke Check

For feature work that changes or could affect exported behavior,
consider the local finalization smoke check during the feature
finalization phase when local smoke artifacts are available. Read
`tools/local-finalize-smoke.md` for the agent-facing smoke-check
workflow. Missing artifacts are not a failure of the feature work;
report that the smoke check could not be run.

## Commit & Pull Request Guidelines

Ignore the non-failing `pkgcheck` suffix “but no badges on README” when
the same check reports “Package has continuous integration checks”.

Recent commits use short imperative summaries, for example
`Fix question type CS-HR-TX` or
`Add qid recode for text fields in SBS questions`. Keep commit subjects
concise and focused on the user-visible or package behavior change. For
pull requests, follow `.github/CONTRIBUTING.md`: open an issue for
larger changes, include a minimal reprex for bugs, run the configured
pre-push R CMD check hook, and link issues with `Fixes #<issue-number>`.
Include tests for changed behavior and update documentation or generated
files when roxygen or README sources change.

Do not commit local planning issue files under `docs/issues/` unless the
user explicitly asks for those files to be committed. Treat them as
working context by default and keep implementation commits focused on
code, tests, documentation, and generated artifacts required for the
behavior change.

## Security & Configuration Tips

Do not commit Qualtrics API keys, `.Renviron`, `.Rhistory`, `.RData`,
`.Rds`, or downloaded private survey data. Use
[`qualtRics::qualtrics_api_credentials()`](https://docs.ropensci.org/qualtRics/reference/qualtrics_api_credentials.html)
locally for credentials. The pre-commit hooks block common R artifacts,
but contributors should still inspect staged files before committing.

## Agent skills

### Issue tracker

Issues and PRDs are tracked in GitHub Issues for `lyh970817/qualtdict`.
See `docs/agents/issue-tracker.md`.

### Triage labels

Triage labels use this repo’s GitHub labels, with `question` for
needs-info and `help wanted` for ready-for-human. See
`docs/agents/triage-labels.md`.

### Domain docs

Single-context repo: use root `CONTEXT.md` and root `docs/adr/` when
present. See `docs/agents/domain.md`.
