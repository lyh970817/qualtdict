# Require a Variable Dictionary that can be exported

Error when a Variable Dictionary carries Export-blocking Validation
Findings: level-label codings that make Labelled Export wrong or
impossible. `assert_dict_valid()` is the gate
[`fetch_labelled_survey_data()`](https://lyh970817.github.io/qualtdict/reference/fetch_labelled_survey_data.md)
applies before downloading responses, so a defective Variable Dictionary
is reported by Response Column ID instead of aborting deep inside
labelling after the download is paid for.

## Usage

``` r
assert_dict_valid(dict)
```

## Arguments

- dict:

  A Variable Dictionary returned by
  [`dict_generate`](https://lyh970817.github.io/qualtdict/reference/dict_generate.md).

## Value

`dict`, invisibly, when it carries no Export-blocking Validation
Findings. Otherwise an error of class
`qualtdict_export_blocking_findings`, whose `findings` field holds the
Export-blocking Validation Findings.

## Details

Export-blocking Validation Findings are the level-label findings where
label and level are not a one-to-one mapping, one label is carried by
several rows, or one level is carried by several rows. A level carried
by several rows is the shape that aborts Labelled Export: Qualtrics keys
an export column on the choice recode, so two choices sharing a recode
are exported as one column carrying both meanings.

A gapped level sequence is not Export-blocking. Gapped recodes are
ordinary Qualtrics survey design and label correctly.

Levels and labels are reported as the Qualtrics survey defines them.
qualtdict neither merges the rows nor repairs the coding, so the finding
stays visible as the survey-authoring error it is.
[`dict_validate()`](https://lyh970817.github.io/qualtdict/reference/dict_validate.md)
stays total: it reports every Validation Finding and never errors, so it
can be used to survey the damage.

## See also

[`dict_validate()`](https://lyh970817.github.io/qualtdict/reference/dict_validate.md)
for the total, never-erroring report.

## Examples

``` r
dict <- data.frame(
  response_column_id = c("QID1", "QID1"),
  variable_name = "q1",
  label = c("Yes", "No"),
  level = c("1", "2")
)
class(dict) <- c("qualtdict", class(dict))

assert_dict_valid(dict)

# Two choices sharing one Qualtrics recode are exported as one column.
broken <- dict
broken$level <- c("1", "1")

try(assert_dict_valid(broken))
#> Error in assert_dict_valid(broken) : 
#>   Variable Dictionary has level-label codings that break Labelled Export.
#> ✖ QID1 (q1): level 1 carries 2 labels — "Yes", "No"
#> ℹ Levels and labels are reported as the Qualtrics survey defines them; qualtdict does not repair them.
#> ℹ Use `dict_validate()` to list every Validation Finding without erroring.
#> ℹ To download anyway, use `exclude_findings = "definite"` to drop the affected Export Variables, or `require_valid_dict = FALSE` to keep them with unreliable value labels.
```
