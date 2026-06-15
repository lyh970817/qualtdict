# rOpenSci Package Standard Checklist for qualtdict

This is a working checklist distilled from the local upstream snapshots. It is
not a substitute for the rOpenSci Dev Guide.

## Scope and Submission Readiness

- [x] Confirm the package is in rOpenSci scope as a tool for retrieving,
  validating, transforming, and documenting Qualtrics survey metadata/data.
- [x] Check package overlap with `qualtRics` and clearly explain the added
  value: variable dictionaries, validation, labelled exports, and naming.
- [ ] Decide whether to open a pre-submission enquiry before formal submission.
- [x] Confirm the package has a clear lifecycle badge and maintenance plan.
- [ ] Prepare a concise submission note using the review request template.

2026-06-15 audit note: `CONTEXT.md` and `docs/prd/0001-ropensci-readiness-and-package-contract.md`
frame `qualtdict` as a metadata and labelled-export companion to `qualtRics`,
not a downloader replacement. The README uses an Active lifecycle badge. A
pre-submission enquiry remains a maintainer decision.

Primary references:

- [softwarereview_policies.Rmd](upstream/dev-guide/softwarereview_policies.Rmd)
- [softwarereview_author.Rmd](upstream/dev-guide/softwarereview_author.Rmd)
- [reviewrequesttemplate.Rmd](upstream/dev-guide/reviewrequesttemplate.Rmd)

## Metadata and Package Surface

- [x] Package title, description, URL, bug tracker, license, authors, and ORCID
  fields are complete and accurate.
- [ ] Package name is searchable, pronounceable, and not misleading.
- [ ] Public functions have a stable, minimal API with clear argument names.
- [ ] Console messages are intentional, informative, and suppressible when
  appropriate.
- [ ] Inputs are validated early with useful error messages.
- [ ] API-client behavior is clear about credentials, network calls, rate
  limits, caching, and failure modes.
- [ ] External service dependencies on Qualtrics are documented, including what
  requires credentials and what can be tested offline.

2026-06-15 audit note: `DESCRIPTION` points to
`https://github.com/lyh970817/qualtdict`, the matching bug tracker, MIT license,
and the maintainer ORCID. Credential and offline-test wording should be expanded
with the documentation refresh/API cleanup work.

Primary reference:

- [pkg_building.Rmd](upstream/dev-guide/pkg_building.Rmd)

## Documentation

- [ ] README explains the problem, installation, credentials setup, and the main
  workflow from dictionary generation to labelled data export.
- [ ] README examples use correct package names and runnable code where possible.
- [ ] Function documentation covers inputs, outputs, side effects, credentials,
  errors, and examples.
- [ ] Vignette demonstrates a full workflow using safe fixtures or simulated
  data, not private survey data.
- [ ] Documentation distinguishes Qualtrics IDs, question IDs, import IDs,
  dictionary variable names, labels, blocks, and recoded values.
- [ ] URLs in documentation are current and use canonical upstream locations.
- [ ] Generated `README.md`, `NAMESPACE`, and `man/*.Rd` are refreshed after
  roxygen or README source changes.

Primary reference:

- [pkg_building.Rmd](upstream/dev-guide/pkg_building.Rmd)

## Testing and CI

- [ ] Unit tests cover dictionary generation, validation, JSON recoding,
  labelled export behavior, and error cases.
- [x] API-dependent tests use recorded fixtures and do not require live Qualtrics
  credentials in CI.
- [x] Recorded requests are scrubbed of secrets and private survey content.
- [ ] CI runs checks across the expected R platforms.
- [ ] Coverage is tracked, with gaps justified for live-service or fixture-heavy
  behavior.
- [ ] Local `devtools::check()` passes before submission.

2026-06-15 audit note: API-dependent tests use `vcr::use_cassette()` and
`tests/testthat/helper-qualtdict.R` injects placeholder Qualtrics credentials
only when none are set, so CI does not need live Qualtrics credentials. Recorded
cassettes under `tests/fixtures/` were reviewed; request credentials and base
URLs are placeholders, and account-specific owner, division, response-set, block,
organisation, and brand URL identifiers were scrubbed from
`tests/fixtures/dict_generate.yml`. `tests/testthat/test-hygiene.R` now guards
that scrub. CI runs R CMD check on macOS, Windows, Ubuntu devel, Ubuntu release,
and Ubuntu oldrel-1, plus coverage on Ubuntu. Local checks still need to be run
inside `nix-shell` or another environment with package development dependencies
installed.

Primary references:

- [pkg_building.Rmd](upstream/dev-guide/pkg_building.Rmd)
- [pkg_ci.Rmd](upstream/dev-guide/pkg_ci.Rmd)
- [pkg_security.Rmd](upstream/dev-guide/pkg_security.Rmd)

## Security and Privacy

- [x] No Qualtrics API keys, `.Renviron`, downloaded private survey data, or
  sensitive `.Rds` artifacts are committed.
- [x] Credential setup is delegated to `qualtRics` or documented secure local
  mechanisms.
- [x] Tests and examples avoid real participant data and avoid leaking survey
  metadata that should remain private.
- [ ] Failure paths do not print secrets or raw private API responses.
- [x] Fixtures are reviewed before commit.

2026-06-15 audit note: `.gitignore`, `.Rbuildignore`, and pre-commit hooks cover
local/private artifacts. `tmp/` contains local spreadsheets and logs, but it is
git-ignored and build-ignored. The recorded response-data fixture is a tiny test
survey export; the remaining survey metadata fixture uses generic survey text.
Any real Qualtrics credential that was used to record the original 2023 fixture
should be considered rotated by the credential owner outside this repository;
no live credential value is present in the checked-in fixture.

Primary reference:

- [pkg_security.Rmd](upstream/dev-guide/pkg_security.Rmd)

## Dependencies and CRAN/rOpenSci Hygiene

- [ ] Imports are necessary, current, and no heavier than needed for the public
  API.
- [ ] Suggested packages are used only in tests, vignettes, examples, or optional
  features.
- [ ] Examples avoid long-running network requests and use `\dontrun{}` or
  alternatives where credentials are required.
- [ ] CRAN checks pass without local state, secrets, or live external services.
- [ ] Any bundled algorithmic code or copied logic has clear provenance and
  compatible licensing.

2026-06-15 remediation path: run `nix-shell` and then
`Rscript -e 'devtools::document()'`, `Rscript -e 'devtools::test()'`, and
`Rscript -e 'devtools::check()'`. If checks report documentation/API mismatches,
resolve them in the documentation and API cleanup issues already planned in
`docs/issues/0004-semantic-name-api-and-preprocess-hook.md`,
`docs/issues/0006-labelled-export-api-cleanup.md`, and
`docs/issues/0007-documentation-and-examples-refresh.md`.

Primary references:

- [pkg_building.Rmd](upstream/dev-guide/pkg_building.Rmd)
- [pkg_security.Rmd](upstream/dev-guide/pkg_security.Rmd)

## Statistical Software Standards

`qualtdict` does not currently look like a statistical estimation package. The
statistical software standards are therefore a secondary reference unless the
package begins to claim statistical methodology, model fitting, inference,
prediction, simulation, or algorithmic estimation behavior.

If those claims are added:

- [ ] Determine the applicable statistical software category.
- [ ] Apply the general statistical software standards.
- [ ] Apply at least one category-specific standard set.
- [ ] Add algorithmic correctness tests, including edge cases and reference
  outputs.

Primary references:

- [standards.Rmd](upstream/statistical-software-review/standards.Rmd)
- [stat-software-categories.md](upstream/statistical-software-review/stat-software-categories.md)
- [standards/general.Rmd](upstream/statistical-software-review/standards/general.Rmd)
