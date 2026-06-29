# qualtdict

**License:** [MIT](https://opensource.org/licenses/MIT)

[Qualtrics](https://www.qualtrics.com/) is an online survey and data
collection software platform. `qualtdict` is a metadata and
labelled-export companion to
[qualtRics](https://github.com/ropensci/qualtRics) for Qualtrics
surveys.

`qualtRics` owns API access to Qualtrics. `qualtdict` owns the package
workflow that turns Qualtrics metadata and import-ID response exports
into:

- a **Variable Dictionary** with `response_column_id`, `row_source`,
  `qid`, `question_name`, `variable_name`, and survey metadata needed
  for analysis;
- **Validation Findings** that make dictionary consistency issues
  visible; and
- **Labelled Survey Data** whose Export Variables are renamed to
  `variable_name` and labelled from the Variable Dictionary.

`qualtdict` is not a replacement downloader for `qualtRics`, a general
survey cleaning framework, or a stable Semantic Name generator.

## Installation

This package can be installed with the
[remotes](https://cran.r-project.org/package=remotes) package.

``` r

install.packages("remotes")
remotes::install_github("lyh970817/qualtdict")
```

## Credentials And Offline Work

Live calls to
[`dict_generate()`](https://lyh970817.github.io/qualtdict/reference/dict_generate.md)
and
[`fetch_labelled_survey_data()`](https://lyh970817.github.io/qualtdict/reference/fetch_labelled_survey_data.md)
require Qualtrics API credentials because they retrieve metadata and
responses through `qualtRics`. Register credentials with
[`qualtRics::qualtrics_api_credentials()`](https://docs.ropensci.org/qualtRics/reference/qualtrics_api_credentials.html),
which `qualtdict` also re-exports for convenience.

``` r

library(qualtdict)

qualtRics::qualtrics_api_credentials(
  api_key = "<YOUR-QUALTRICS_API_KEY>",
  base_url = "<YOUR-QUALTRICS_BASE_URL>",
  install = TRUE
)
```

Package tests and documentation examples avoid private survey data.
Validation, dictionary schema checks, block splitting, and
labelled-export mechanics can be tested offline with synthetic
dictionaries and synthetic metadata. Only live Qualtrics metadata or
response downloads require credentials.

## Variable Dictionary To Labelled Survey Data

Generate a Variable Dictionary from a survey ID. The dictionary keeps
`response_column_id` as the downloaded response-column key, `row_source`
as the Dictionary Row Source, `qid` as the bare Qualtrics question
identifier, `question_name` as the raw Qualtrics naming reference, and
`variable_name` as the final export-safe Dictionary Variable Name.
Question-backed rows use `row_source = "question"`, and flat Embedded
Data Fields defined by Qualtrics metadata use
`row_source = "embedded_data"`.

``` r

survey_id <- "SV_XXXXXXXXXXXXXXXX"

dict <- dict_generate(
  survey_id,
  variable_name = "question_name"
)
```

Use
[`dict_validate()`](https://lyh970817.github.io/qualtdict/reference/dict_validate.md)
to return structured Validation Findings.

``` r

validation <- dict_validate(dict)
validation$validation_findings
```

Then create Labelled Survey Data. `qualtdict` calls
[`qualtRics::fetch_survey()`](https://docs.ropensci.org/qualtRics/reference/fetch_survey.html)
with the response-column settings needed to match downloaded data back
to `response_column_id`, then renames Export Variables to
`variable_name` and attaches labels from the Variable Dictionary.

``` r

survey_dat <- fetch_labelled_survey_data(
  dict,
  extra_columns = c("externalDataReference", "startDate", "endDate"),
  exclude_findings = "none",
  unanswer_recode = -77,
  unanswer_recode_multi = 0
)
```

If Qualtrics Question Names are not suitable as analysis names, you can
ask `qualtdict` to generate Semantic Names. Semantic Names are readable
conveniences generated from survey text and metadata; they are not
stable guarantees across package versions or survey text changes. For
long text, Semantic Names select important words from ranked keywords
and preserve those selected words in the order they appear in the naming
text.

``` r

block_pattern <- function(block) {
  substring(block, 1, 3)
}

dict <- dict_generate(
  survey_id,
  variable_name = "semantic_name",
  block_pattern = block_pattern,
  block_sep = "."
)
```
