# Example survey metadata, Variable Dictionary, and Labelled Survey Data

A small offline example of everything qualtdict reads and produces, so
its output can be explored without Qualtrics credentials. The survey
structure is a subset of the GLAD Study sign-up questionnaire: four
questions of the `Demographics` Survey Block and the whole `PHQ9` and
`GAD7` Survey Blocks, with six Embedded Data Fields and two Scoring
Variables. Together they cover a text entry question, a drop-down list,
single-answer and multi-answer multiple choice (including a choice with
its own text entry column), and two rating-scale matrices, with rows
from the `question`, `embedded_data`, and `scoring` Dictionary Row
Sources. The source survey defines no Text-analysis Sidecars, so the
`text_analysis` Dictionary Row Source does not occur.

## Usage

``` r
glad_example
```

## Format

A list with six components:

- `survey_metadata`:

  A list shaped like the return value of
  [`qualtRics::metadata()`](https://docs.ropensci.org/qualtRics/reference/metadata.html),
  with components `metadata`, `questions`, `responsecounts`, `blocks`,
  `flow`, and `embedded_data`. Response counts are zero and Embedded
  Data default values are blank.

- `survey_description`:

  A list shaped like the return value of
  [`qualtRics::fetch_description()`](https://docs.ropensci.org/qualtRics/reference/fetch_description.html),
  with components `metadata`, `flow`, `blocks`, `questions`, and
  `scoring`. Embedded Data values in the Survey Flow are blank.

- `response_column_map`:

  A data frame: the `column_map` attribute of a
  [`qualtRics::fetch_survey()`](https://docs.ropensci.org/qualtRics/reference/fetch_survey.html)
  download, with one row per downloaded column. `qname` holds the
  Response Column ID.

- `responses`:

  A tibble of 20 rows and 48 columns of synthetic Participant Response
  Data, shaped like a
  [`qualtRics::fetch_survey()`](https://docs.ropensci.org/qualtRics/reference/fetch_survey.html)
  download with `import_id = TRUE`, `convert = FALSE`, `label = FALSE`,
  and `breakout_sets = TRUE`. Columns are named by Response Column ID,
  and `response_column_map` is attached as its `column_map` attribute.

- `dictionary`:

  The Variable Dictionary (a `qualtdict` object, 97 rows describing 37
  Response Column IDs) that
  [`dict_generate()`](https://lyh970817.github.io/qualtdict/reference/dict_generate.md)
  returns for this survey with `variable_name = "question_name"`.

- `labelled_survey_data`:

  The Labelled Survey Data that
  [`survey_fetch_labelled()`](https://lyh970817.github.io/qualtdict/reference/survey_fetch_labelled.md)
  returns when `dictionary` is applied to `responses`.

## Source

Derived from the survey metadata of the GLAD Study sign-up
questionnaire. Survey, owner, brand, and account identifiers, dates,
response counts, question JavaScript, and Embedded Data values were
removed or replaced with fixed fake values; the survey ID is the fake
`"SV_glad0example00000"`. The responses are fully synthetic. The script
that builds the data is `data-raw/glad_example.R` in the package source
repository.

## Details

The Participant Response Data in `responses` are synthetic: 20 responses
drawn at random from each column's valid levels, with Embedded Data
Fields and Scoring Variables computed from those answers, fake
identifiers, and 2026 dates. They are not answers from GLAD Study
participants.

## Examples

``` r
names(glad_example)
#> [1] "survey_metadata"      "survey_description"   "response_column_map" 
#> [4] "responses"            "dictionary"           "labelled_survey_data"

# The Variable Dictionary: one row per Response Column ID and level
dict <- glad_example$dictionary
dict[, c(
  "response_column_id", "row_source", "variable_name", "level", "label"
)]
#> # A tibble: 97 × 5
#>    response_column_id row_source variable_name level label                      
#>    <chr>              <chr>      <chr>         <chr> <chr>                      
#>  1 QID1125_TEXT       question   DEM.AGE.1.0   NA    NA                         
#>  2 QID1126            question   DEM.COR.1.0   1     England                    
#>  3 QID1126            question   DEM.COR.1.0   2     Scotland                   
#>  4 QID1126            question   DEM.COR.1.0   3     Wales                      
#>  5 QID1126            question   DEM.COR.1.0   4     Northern Ireland           
#>  6 QID1126            question   DEM.COR.1.0   5     Outside UK                 
#>  7 QID1127            question   DEM.SEX.1.0   0     Male                       
#>  8 QID1127            question   DEM.SEX.1.0   1     Female                     
#>  9 QID1147_1          question   DEM.EDU.1.0   1     College or university degr…
#> 10 QID1147_2          question   DEM.EDU.1.0.1 1     A levels/AS levels or equi…
#> # ℹ 87 more rows

# Validation Findings: here only suggestive name repairs
findings <- dict_validate(dict)$validation_findings
findings[, c("finding", "severity", "response_column_id", "reason")]
#> # A tibble: 23 × 4
#>    finding                severity   response_column_id reason   
#>    <chr>                  <chr>      <chr>              <chr>    
#>  1 repaired_variable_name suggestive QID1147_2          duplicate
#>  2 repaired_variable_name suggestive QID1147_3          duplicate
#>  3 repaired_variable_name suggestive QID1147_4          duplicate
#>  4 repaired_variable_name suggestive QID1147_6          duplicate
#>  5 repaired_variable_name suggestive QID1147_5          duplicate
#>  6 repaired_variable_name suggestive QID1147_5_TEXT     duplicate
#>  7 repaired_variable_name suggestive QID1147_-999       duplicate
#>  8 repaired_variable_name suggestive QID127303424_2     duplicate
#>  9 repaired_variable_name suggestive QID127303424_3     duplicate
#> 10 repaired_variable_name suggestive QID127303424_4     duplicate
#> # ℹ 13 more rows

# Labelled Survey Data: Export Variables renamed to `variable_name`
glad_example$labelled_survey_data[, 1:6]
#> # A tibble: 20 × 6
#>    externalDataReference startDate           endDate             DEM.AGE.1.0
#>    <chr>                 <dttm>              <dttm>                    <dbl>
#>  1 example-001           2026-01-08 13:05:38 2026-01-08 13:44:46          34
#>  2 example-002           2026-01-31 19:57:56 2026-01-31 20:26:22          53
#>  3 example-003           2026-02-02 00:05:34 2026-02-02 00:39:55          63
#>  4 example-004           2026-02-25 19:24:52 2026-02-25 19:49:55          37
#>  5 example-005           2026-03-06 23:48:20 2026-03-07 00:12:21          68
#>  6 example-006           2026-03-07 15:16:38 2026-03-07 15:39:07          70
#>  7 example-007           2026-04-14 14:41:01 2026-04-14 15:15:04          59
#>  8 example-008           2026-04-25 09:06:24 2026-04-25 09:43:35          30
#>  9 example-009           2026-05-04 15:39:47 2026-05-04 16:03:29          54
#> 10 example-010           2026-05-12 11:20:02 2026-05-12 11:58:47          20
#> 11 example-011           2026-05-18 08:05:52 2026-05-18 08:25:51          21
#> 12 example-012           2026-05-20 01:49:44 2026-05-20 02:01:03          26
#> 13 example-013           2026-05-22 03:31:18 2026-05-22 03:39:12          18
#> 14 example-014           2026-05-30 20:47:12 2026-05-30 21:15:59          68
#> 15 example-015           2026-06-03 20:06:03 2026-06-03 20:37:21          41
#> 16 example-016           2026-06-16 00:28:56 2026-06-16 00:41:41          23
#> 17 example-017           2026-06-21 02:43:51 2026-06-21 03:13:59          19
#> 18 example-018           2026-06-24 23:52:52 2026-06-25 00:30:48          49
#> 19 example-019           2026-06-29 06:37:48 2026-06-29 07:06:21          79
#> 20 example-020           2026-08-27 04:08:24 2026-08-27 04:41:12          45
#> # ℹ 2 more variables: DEM.COR.1.0 <dbl>, DEM.SEX.1.0 <dbl>
```
