# rOpenSci Package Standards

This directory keeps a local reference copy of the rOpenSci package review
standards and adjacent guidance relevant to `qualtdict`.

Use the upstream rendered docs as the source of truth when submitting or
reviewing a package. The local copies are for offline reading, search, and
repo-specific planning.

## Quick Links

- [Local checklist](package-standard-checklist.md): condensed working checklist
  for aligning `qualtdict` with rOpenSci expectations.
- [Source manifest](sources.md): upstream repositories, commit pins, license
  notes, and fetched timestamp.
- [rOpenSci Dev Guide snapshot](upstream/dev-guide/): package development,
  software review policies, author guide, CI, security, and review templates.
- [Statistical software review snapshot](upstream/statistical-software-review/):
  statistical software review process, category guidance, and standards.

## Most Relevant Upstream Files

For this package, start with these files:

- [pkg_building.Rmd](upstream/dev-guide/pkg_building.Rmd): package metadata,
  API design, documentation, testing, dependencies, examples, and CRAN issues.
- [pkg_ci.Rmd](upstream/dev-guide/pkg_ci.Rmd): CI and coverage guidance.
- [pkg_security.Rmd](upstream/dev-guide/pkg_security.Rmd): credentials,
  secrets, recorded API requests, and user protection.
- [softwarereview_policies.Rmd](upstream/dev-guide/softwarereview_policies.Rmd):
  scope, overlap, ownership, quality commitment, and review process.
- [softwarereview_author.Rmd](upstream/dev-guide/softwarereview_author.Rmd):
  author preparation and submission workflow.
- [reviewrequesttemplate.Rmd](upstream/dev-guide/reviewrequesttemplate.Rmd):
  pre-submission enquiry template.
- [reviewtemplate.Rmd](upstream/dev-guide/reviewtemplate.Rmd): reviewer
  checklist structure.

The statistical software standards are probably secondary for `qualtdict`.
The package generates survey metadata dictionaries and labelled exports; it
does not appear to claim to implement statistical estimation software. If that
scope changes, begin with:

- [standards/general.Rmd](upstream/statistical-software-review/standards/general.Rmd)
- [stat-software-categories.md](upstream/statistical-software-review/stat-software-categories.md)

## Refreshing

To refresh the upstream snapshot, re-download the files listed in
[sources.md](sources.md), then update the commit SHAs and fetched timestamp.
Keep this directory focused on standards and review material; avoid mirroring
the entire upstream books unless the package needs it.
