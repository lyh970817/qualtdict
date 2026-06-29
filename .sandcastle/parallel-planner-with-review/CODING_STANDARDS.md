# Coding Standards

## Style

- Follow tidyverse R style with two-space indentation and clear snake_case names.
- Keep exported R functions documented with roxygen2 Markdown comments.
- Prefer readable pipe chains over compact cleverness.
- Do not restyle unrelated code.
- Use canonical package language from `CONTEXT.md`: Variable Dictionary, Response Column ID, Dictionary Variable Name, Semantic Name, Validation Finding, Labelled Export Finding, and Labelled Survey Data.

## Testing

- Tests use `testthat` edition 3 under `tests/testthat/`.
- Name new tests `tests/testthat/test-<feature>.R`.
- Prefer synthetic dictionaries, synthetic metadata, fixtures, and snapshots.
- Keep ordinary tests offline. Do not require live Qualtrics credentials.
- Do not commit Qualtrics API cassettes or Participant Response Data.
- For stable structured output, add or update snapshots in `tests/testthat/_snaps/`.

## Verification

- Use `Rscript -e 'devtools::test()'` as the default test suite.
- Use `Rscript -e 'devtools::check()'` for exported behavior or broad package changes.
- Regenerate docs with `Rscript -e 'devtools::document()'` after roxygen changes.
- Render `README.Rmd` with `Rscript -e 'rmarkdown::render("README.Rmd")'` after README source changes.
- Consider local finalization smoke checks during finalization when local smoke
  artifacts are available. Missing artifacts are not a failure.
- Run the smoke script as one self-contained invocation for the relevant
  finalization surface. Select affected exported outputs with `--functions`
  based on the dependency path from the changed code to smoke-covered exported
  functions; do not broaden the selection just because prerequisites run
  internally. Use the default `question_name` Variable Dictionary route; the
  Semantic Name route is disabled for local finalization smoke because it is too
  expensive for this workflow. Cover Semantic Name changes with ordinary tests
  and package checks. For example:
  `Rscript tools/local-finalize-smoke.R check --functions dict_generate --variable-name question_name`.
- Inspect the terminal output and saved RDS object artifacts under
  `.local/finalize-smoke/runs/<timestamp>/`; temporary uncommitted R code is
  acceptable for local inspection.
- Smoke runs can take several minutes. Wait with a longer timeout, do not
  repeatedly poll the process, and inspect output once the smoke command exits
  before treating the agent as idle.

## Architecture

- Keep helpers near the package capability they support.
- Prefer existing helper APIs and established local patterns over new abstractions.
- Preserve Response Column ID identity through Variable Dictionary generation and Labelled Export.
- Keep Loop and Merge expansion upstream from Response Column ID Rendering.
- Treat Validation Findings and Labelled Export Findings as distinct public finding surfaces.
