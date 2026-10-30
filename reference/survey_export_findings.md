# Return Labelled Export Findings

Labelled Export Findings describe issues detected while matching a
Variable Dictionary to downloaded survey data.

## Usage

``` r
survey_export_findings(x)
```

## Arguments

- x:

  A data frame of Labelled Survey Data, such as the tibble returned by
  [`survey_fetch_labelled()`](https://lyh970817.github.io/qualtdict/reference/survey_fetch_labelled.md).

## Value

A data frame of Labelled Export Findings.

## Examples

``` r
# Every Variable Dictionary column was found in the example download
survey_export_findings(glad_example$labelled_survey_data)
#> # A tibble: 0 × 5
#> # ℹ 5 variables: finding <chr>, response_column_id <chr>, qid <chr>,
#> #   variable_name <chr>, reason <chr>

# A download that lacked a dictionary column would report it like this
dat <- data.frame(q1 = "1")
attr(dat, "survey_export_findings") <- data.frame(
  finding = "missing_response_column_id",
  response_column_id = "QID2",
  qid = "QID2",
  variable_name = "q2",
  reason = "not_found_in_downloaded_survey_data"
)

survey_export_findings(dat)
#>                      finding response_column_id  qid variable_name
#> 1 missing_response_column_id               QID2 QID2            q2
#>                                reason
#> 1 not_found_in_downloaded_survey_data
```
