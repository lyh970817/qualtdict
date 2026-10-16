# Use Object-Verb Names for Exported Functions

Status: accepted

qualtdict names every exported function `<object>_<verb>`, where the object
prefix names the package-owned result the function works on: `dict_` for
Variable Dictionary functions and `survey_` for Labelled Survey Data
functions. The rOpenSci editor's review noted that the reference index mixed
object-verb names (`dict_generate()`, `dict_validate()`, `dict_split_blocks()`,
`survey_split_blocks()`) with verb-object names, against the rOpenSci
guidance on consistent function naming
(<https://devguide.ropensci.org/pkg_building.html#function-and-argument-naming>).

Three exported functions are renamed outright:

| Old name | New name |
|---|---|
| `assert_dict_valid()` | `dict_assert_valid()` |
| `fetch_labelled_survey_data()` | `survey_fetch_labelled()` |
| `labelled_export_findings()` | `survey_export_findings()` |

The attribute that carries Labelled Export Findings on Labelled Survey Data
is renamed with its accessor, from `"labelled_export_findings"` to
`"survey_export_findings"`. The error class
`qualtdict_export_blocking_findings` is unchanged.

This supersedes the naming part of ADR 0006, which chose
`fetch_labelled_survey_data()`. ADR 0006's other decisions stand: the old
`get_survey_data()` name stays removed, and the function still signals a
network fetch and names Labelled Survey Data as its result.

## Considered Options

- Keep deprecated exported aliases for the old names. Not done: qualtdict is
  at 0.1.0 and not on CRAN, so there are few users to protect, and aliases
  would keep the mixed naming visible in the reference index the change is
  meant to clean up.
- Rename outright. **Chosen.**

## Consequences

Code calling the old names fails with "could not find function" and must be
updated. New exported functions take a `dict_` or `survey_` prefix, or
another object prefix that names the package-owned result they act on. The
`qualtrics_api_credentials()` re-export keeps its upstream `qualtRics` name.
