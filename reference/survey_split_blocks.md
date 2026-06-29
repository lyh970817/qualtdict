# Split Labelled Survey Data by Survey Block

Create block-specific views of Labelled Survey Data without changing the
return type of
[`fetch_labelled_survey_data`](https://lyh970817.github.io/qualtdict/reference/fetch_labelled_survey_data.md).

## Usage

``` r
survey_split_blocks(
  dat,
  dict = attr(dat, "dict", exact = TRUE),
  extra_columns = c("externalDataReference", "startDate", "endDate")
)
```

## Arguments

- dat:

  Labelled Survey Data returned by
  [`fetch_labelled_survey_data`](https://lyh970817.github.io/qualtdict/reference/fetch_labelled_survey_data.md).

- dict:

  A Variable Dictionary returned by
  [`dict_generate`](https://lyh970817.github.io/qualtdict/reference/dict_generate.md).

- extra_columns:

  A character vector of raw Labelled Survey Data columns to retain in
  each block data set. Defaults to
  `c("externalDataReference", "startDate", "endDate")`. Missing
  user-specified columns error; missing default columns warn and are
  skipped. Use `NULL` to retain no extra columns.

## Value

A named list of data frames, one per Survey Block. When dictionary rows
have no Survey Block assignment, the list includes `"..unassigned"`.

## Details

When `dict` is not supplied, `survey_split_blocks()` uses the `dict`
attribute attached by
[`fetch_labelled_survey_data`](https://lyh970817.github.io/qualtdict/reference/fetch_labelled_survey_data.md).

## Examples

``` r
dict <- data.frame(
  response_column_id = c("QID1", "QID2"),
  variable_name = c("q1", "q2"),
  block = c("Block A", "Block B")
)
class(dict) <- c("qualtdict", class(dict))

dat <- data.frame(
  externalDataReference = "R_1",
  startDate = "2026-06-01",
  endDate = "2026-06-01",
  q1 = "1",
  q2 = "2"
)
attr(dat, "dict") <- dict

survey_split_blocks(dat)
#> $`Block A`
#>   externalDataReference  startDate    endDate q1
#> 1                   R_1 2026-06-01 2026-06-01  1
#> 
#> $`Block B`
#>   externalDataReference  startDate    endDate q2
#> 1                   R_1 2026-06-01 2026-06-01  2
#> 
```
