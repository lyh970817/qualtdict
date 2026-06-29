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
  level-label coding findings.

- `level_label_pairs` - A data frame containing the unique level-label
  pairings.

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
