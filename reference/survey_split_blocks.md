# Split Labelled Survey Data by Survey Block

Create block-specific views of Labelled Survey Data without changing the
return type of
[`survey_fetch_labelled`](https://lyh970817.github.io/qualtdict/reference/survey_fetch_labelled.md).

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

  A data frame of Labelled Survey Data: a tibble as returned by
  [`survey_fetch_labelled`](https://lyh970817.github.io/qualtdict/reference/survey_fetch_labelled.md).

- dict:

  A Variable Dictionary: a `qualtdict` object (a tibble with class
  `qualtdict`) as returned by
  [`dict_generate`](https://lyh970817.github.io/qualtdict/reference/dict_generate.md).

- extra_columns:

  A character vector of raw Labelled Survey Data columns to retain in
  each block data set. Defaults to
  `c("externalDataReference", "startDate", "endDate")`. Columns absent
  from the data warn (naming the missing columns) and are skipped. Use
  `NULL` to retain no extra columns.

## Value

A named list of data frames, one per Survey Block. When dictionary rows
have no Survey Block assignment, the list includes `"..unassigned"`.

## Details

When `dict` is not supplied, `survey_split_blocks()` uses the `dict`
attribute attached by
[`survey_fetch_labelled`](https://lyh970817.github.io/qualtdict/reference/survey_fetch_labelled.md).

## Examples

``` r
survey_blocks <- survey_split_blocks(glad_example$labelled_survey_data)
names(survey_blocks)
#> [1] "..unassigned" "Demographics" "GAD7"         "PHQ9"        
survey_blocks$GAD7
#> # A tibble: 20 × 11
#>    externalDataReference startDate           endDate              HEAD HEAD.1
#>    <chr>                 <dttm>              <dttm>              <dbl>  <dbl>
#>  1 example-001           2026-01-08 13:05:38 2026-01-08 13:44:46     0      3
#>  2 example-002           2026-01-31 19:57:56 2026-01-31 20:26:22     3      1
#>  3 example-003           2026-02-02 00:05:34 2026-02-02 00:39:55     1      3
#>  4 example-004           2026-02-25 19:24:52 2026-02-25 19:49:55     3      3
#>  5 example-005           2026-03-06 23:48:20 2026-03-07 00:12:21     3      2
#>  6 example-006           2026-03-07 15:16:38 2026-03-07 15:39:07     3      2
#>  7 example-007           2026-04-14 14:41:01 2026-04-14 15:15:04     3      0
#>  8 example-008           2026-04-25 09:06:24 2026-04-25 09:43:35     0      3
#>  9 example-009           2026-05-04 15:39:47 2026-05-04 16:03:29     0      1
#> 10 example-010           2026-05-12 11:20:02 2026-05-12 11:58:47     1      3
#> 11 example-011           2026-05-18 08:05:52 2026-05-18 08:25:51     3      3
#> 12 example-012           2026-05-20 01:49:44 2026-05-20 02:01:03     0      3
#> 13 example-013           2026-05-22 03:31:18 2026-05-22 03:39:12     3      0
#> 14 example-014           2026-05-30 20:47:12 2026-05-30 21:15:59     3      3
#> 15 example-015           2026-06-03 20:06:03 2026-06-03 20:37:21     0      3
#> 16 example-016           2026-06-16 00:28:56 2026-06-16 00:41:41     3      1
#> 17 example-017           2026-06-21 02:43:51 2026-06-21 03:13:59     0      2
#> 18 example-018           2026-06-24 23:52:52 2026-06-25 00:30:48     2      3
#> 19 example-019           2026-06-29 06:37:48 2026-06-29 07:06:21     1      3
#> 20 example-020           2026-08-27 04:08:24 2026-08-27 04:41:12     1      0
#> # ℹ 6 more variables: HEAD.2 <dbl>, HEAD.3 <dbl>, HEAD.4 <dbl>, HEAD.5 <dbl>,
#> #   HEAD.6 <dbl>, Q5 <dbl>

# Pass the Variable Dictionary explicitly and keep only the respondent
# identifier alongside each block's Export Variables
survey_blocks <- survey_split_blocks(
  glad_example$labelled_survey_data,
  dict = glad_example$dictionary,
  extra_columns = "externalDataReference"
)
survey_blocks$GAD7
#> # A tibble: 20 × 9
#>    externalDataReference  HEAD HEAD.1 HEAD.2 HEAD.3 HEAD.4 HEAD.5 HEAD.6    Q5
#>    <chr>                 <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl> <dbl>
#>  1 example-001               0      3      2      1      2      3      1     2
#>  2 example-002               3      1      1      1      0      2      1     1
#>  3 example-003               1      3      0      1      0      2      0     3
#>  4 example-004               3      3      0      0      3      0      2     1
#>  5 example-005               3      2      2      3      0      0      0     4
#>  6 example-006               3      2      3      3      1      2      2     2
#>  7 example-007               3      0      2      3      2      3      1     4
#>  8 example-008               0      3      2      2      3      2      1     3
#>  9 example-009               0      1      0      2      2      2      3     4
#> 10 example-010               1      3      1      0      2      0      1     3
#> 11 example-011               3      3      3      2      1      1      3     2
#> 12 example-012               0      3      1      1      0      1      2     2
#> 13 example-013               3      0      1      0      1      3      3     1
#> 14 example-014               3      3      0      3      0      3      0     3
#> 15 example-015               0      3      0      2      3      3      1     3
#> 16 example-016               3      1      1      1      0      3      0     1
#> 17 example-017               0      2      3      0      3      2      3     3
#> 18 example-018               2      3      0      2      1      0      0     4
#> 19 example-019               1      3      1      1      2      3      1     3
#> 20 example-020               1      0      1      0      2      3      0     3
```
