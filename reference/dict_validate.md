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

  A Variable Dictionary returned by
  [`dict_generate`](https://lyh970817.github.io/qualtdict/reference/dict_generate.md).

- quiet:

  Boolean. If `TRUE`, suppress routine validation messages. Defaults to
  `TRUE`.

## Value

A `qualtdict_validation` object. This is a list with stable components:

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
[`assert_dict_valid()`](https://lyh970817.github.io/qualtdict/reference/assert_dict_valid.md)
when you want the Export-blocking subset to error instead.

## Examples

``` r
dict <- data.frame(
  response_column_id = c("QID1", "QID2"),
  variable_name = c("q1", "q2"),
  label = c("Yes", "No"),
  level = c("1", "2")
)
class(dict) <- c("qualtdict", class(dict))

validation <- dict_validate(dict)
```
