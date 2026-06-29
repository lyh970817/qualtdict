# Return Labelled Export Findings

Labelled Export Findings describe issues detected while matching a
Variable Dictionary to downloaded survey data.

## Usage

``` r
labelled_export_findings(x)
```

## Arguments

- x:

  Labelled Survey Data returned by
  [`fetch_labelled_survey_data()`](https://lyh970817.github.io/qualtdict/reference/fetch_labelled_survey_data.md).

## Value

A data frame of Labelled Export Findings.

## Examples

``` r
dat <- data.frame(q1 = "1")
attr(dat, "labelled_export_findings") <- data.frame(
  finding = "missing_response_column_id",
  response_column_id = "QID2",
  qid = "QID2",
  variable_name = "q2",
  reason = "not_found_in_downloaded_survey_data"
)

labelled_export_findings(dat)
#>                      finding response_column_id  qid variable_name
#> 1 missing_response_column_id               QID2 QID2            q2
#>                                reason
#> 1 not_found_in_downloaded_survey_data
```
