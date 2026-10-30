# Return Validation Findings for a Variable Dictionary

Validate Variable Dictionary consistency and return a stable validation
object. Validation Findings report repaired `variable_name` values,
non-unique or unsafe Dictionary Variable Names, and level-label coding
issues. The validation result is a consistency screen, not proof that
the source Qualtrics metadata is correct.

## Usage

``` r
dict_validate(dict, quiet = TRUE)
```

## Arguments

- dict:

  A Variable Dictionary: a `qualtdict` object (a tibble with class
  `qualtdict`) as returned by
  [`dict_generate`](https://lyh970817.github.io/qualtdict/reference/dict_generate.md).

- quiet:

  Boolean. If `TRUE`, suppress routine validation messages. Defaults to
  `TRUE`.

## Value

A list with stable components:

- `validation_findings` - A data frame of Validation Findings for
  Variable Dictionary consistency issues, including repaired variable
  names, non-unique or unsafe final `variable_name` values, and
  level-label coding findings. Each row carries a `severity`:
  `"definite"` when the finding makes the affected export column
  uninterpretable or its identity unreliable (the Export-blocking
  level-label codings, and inconsistent, duplicate, or unsafe final
  `variable_name` values), `"suggestive"` when the finding only reports
  something worth review while the column's data and identity stay sound
  (a repaired `variable_name`; a level-label finding, such as a gapped
  level run, none of whose tripped tests is Export-blocking).

- `level_label_pairs` - A data frame containing the unique level-label
  pairings.

## Details

`dict_validate()` is total: it reports every Validation Finding and
never errors, so it can be used to survey the damage in a defective
Variable Dictionary. Use
[`dict_assert_valid()`](https://lyh970817.github.io/qualtdict/reference/dict_assert_valid.md)
when you want the Export-blocking subset to error instead.

## Examples

``` r
# quiet = FALSE would also print routine progress messages
validation <- dict_validate(glad_example$dictionary, quiet = TRUE)
names(validation)
#> [1] "validation_findings" "level_label_pairs"  

# For the example survey, every finding is a suggestive variable-name repair
findings <- validation$validation_findings
table(findings$severity)
#> 
#> suggestive 
#>         23 
findings[, c(
  "severity", "response_column_id", "original_candidate", "variable_name",
  "reason"
)]
#> # A tibble: 23 × 5
#>    severity   response_column_id original_candidate variable_name reason   
#>    <chr>      <chr>              <chr>              <chr>         <chr>    
#>  1 suggestive QID1147_2          DEM.EDU.1.0        DEM.EDU.1.0.1 duplicate
#>  2 suggestive QID1147_3          DEM.EDU.1.0        DEM.EDU.1.0.2 duplicate
#>  3 suggestive QID1147_4          DEM.EDU.1.0        DEM.EDU.1.0.3 duplicate
#>  4 suggestive QID1147_6          DEM.EDU.1.0        DEM.EDU.1.0.4 duplicate
#>  5 suggestive QID1147_5          DEM.EDU.1.0        DEM.EDU.1.0.5 duplicate
#>  6 suggestive QID1147_5_TEXT     DEM.EDU.1.0        DEM.EDU.1.0.6 duplicate
#>  7 suggestive QID1147_-999       DEM.EDU.1.0        DEM.EDU.1.0.7 duplicate
#>  8 suggestive QID127303424_2     PHQ.DEP.1.0        PHQ.DEP.1.0.1 duplicate
#>  9 suggestive QID127303424_3     PHQ.DEP.1.0        PHQ.DEP.1.0.2 duplicate
#> 10 suggestive QID127303424_4     PHQ.DEP.1.0        PHQ.DEP.1.0.3 duplicate
#> # ℹ 13 more rows
```
