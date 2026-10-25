
<!-- README.md is generated from README.Rmd. Please edit that file -->

# qualtdict

**License:** [MIT](https://opensource.org/licenses/MIT)

<!-- badges: start -->

[![Project Status: Active - The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![check-standard](https://github.com/lyh970817/qualtdict/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/lyh970817/qualtdict/actions/workflows/check-standard.yaml)
[![test-coverage](https://github.com/lyh970817/qualtdict/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/lyh970817/qualtdict/actions/workflows/test-coverage.yaml)
[![Codecov test
coverage](https://codecov.io/gh/lyh970817/qualtdict/branch/main/graph/badge.svg)](https://app.codecov.io/gh/lyh970817/qualtdict?branch=main)
<!-- badges: end -->

[Qualtrics](https://www.qualtrics.com/) is widely used to collect
questionnaire and follow-up data, and the analyst’s work begins when the
survey closes. Qualtrics describes a survey in a deeply nested JSON
structure: questions, choices, matrix items, loops, embedded fields and
scoring rules are spread across several API objects. The downloaded
response file refers back to them only through identifiers like
`QID12_3_TEXT`, and can contain hundreds of such columns covering
question choices, matrix items, text-entry fields, Loop and Merge
iterations, embedded data, scoring outputs and text-analysis sidecars.
Before any analysis, every column has to be matched back to the
metadata, renamed, labelled with its response options, and documented.
Done by hand or in ad hoc scripts, that mapping is slow, error-prone,
and hard for anyone else to check, especially for surveys with matrix,
side-by-side or multi-select questions and repeated waves.

qualtdict makes that mapping explicit, readable and reproducible. From
the Qualtrics metadata it builds a Variable Dictionary: a flat table
with one row per exported response column, showing the question, item,
response level and label it belongs to, plus a generated variable name.
The dictionary is easy to inspect and validate, and it doubles as a
codebook you can review with collaborators or publish alongside the
data.

Applying the dictionary to downloaded responses attaches that structure
to the data itself. Every column is renamed to its variable name; choice
columns carry their response options as value labels, and text and
numeric columns carry the question text as a variable label. You can
tell what each column is just by looking at it, so the response data are
analysis-ready from the first `glimpse()`.

[qualtRics](https://github.com/ropensci/qualtRics) handles the Qualtrics
API; qualtdict handles everything between the raw export and your
analysis. It is not a replacement downloader for qualtRics, and not a
general survey-cleaning framework.

## Installation

``` r
# install.packages("pak")
pak::pak("lyh970817/qualtdict")
```

qualtdict does not need Java. The `SystemRequirements` field of its
`DESCRIPTION` mentions Java only because the suggested openNLP package
needs it for POS-tag filtering, which the Semantic Names in qualtdict do
not use.

## Credentials

`dict_generate()` and `survey_fetch_labelled()` read your survey from
Qualtrics, so they need Qualtrics API credentials. Your institution must
allow API access, and it must be enabled for your account; the person
who manages your Qualtrics account can give you your API token and base
URL. Store them once with `qualtrics_api_credentials()`, which qualtdict
re-exports from qualtRics:

``` r
library(qualtdict)

qualtrics_api_credentials(
  api_key = "<YOUR-QUALTRICS_API_KEY>",
  base_url = "<YOUR-QUALTRICS_BASE_URL>",
  install = TRUE
)
```

The rest of the functions work on objects you already have and need no
credentials.

## A Variable Dictionary for your survey

Pass the survey ID to `dict_generate()`:

``` r
survey_id <- "SV_XXXXXXXXXXXXXXXX"

dict <- dict_generate(survey_id, variable_name = "question_name")
```

The rest of this README uses `glad_example`, a small example bundled
with the package: the metadata of a subset of the GLAD Study sign-up
survey (four demographic questions, the PHQ-9 and GAD-7 blocks, embedded
data and scoring variables) with synthetic responses.
`glad_example$dictionary` is exactly what `dict_generate()` returns for
that survey.

``` r
library(qualtdict)

dict <- glad_example$dictionary
dict
#> # A tibble: 97 × 14
#>    response_column_id row_source qid     question_name variable_name block      
#>    <chr>              <chr>      <chr>   <chr>         <chr>         <chr>      
#>  1 QID1125_TEXT       question   QID1125 DEM.AGE.1.0   DEM.AGE.1.0   Demographi…
#>  2 QID1126            question   QID1126 DEM.COR.1.0   DEM.COR.1.0   Demographi…
#>  3 QID1126            question   QID1126 DEM.COR.1.0   DEM.COR.1.0   Demographi…
#>  4 QID1126            question   QID1126 DEM.COR.1.0   DEM.COR.1.0   Demographi…
#>  5 QID1126            question   QID1126 DEM.COR.1.0   DEM.COR.1.0   Demographi…
#>  6 QID1126            question   QID1126 DEM.COR.1.0   DEM.COR.1.0   Demographi…
#>  7 QID1127            question   QID1127 DEM.SEX.1.0   DEM.SEX.1.0   Demographi…
#>  8 QID1127            question   QID1127 DEM.SEX.1.0   DEM.SEX.1.0   Demographi…
#>  9 QID1147_1          question   QID1147 DEM.EDU.1.0   DEM.EDU.1.0   Demographi…
#> 10 QID1147_2          question   QID1147 DEM.EDU.1.0   DEM.EDU.1.0.1 Demographi…
#> # ℹ 87 more rows
#> # ℹ 8 more variables: question <chr>, item <chr>, level <chr>, label <chr>,
#> #   type <chr>, selector <chr>, sub_selector <chr>, content_type <chr>
```

Every column of the dictionary, with its type:

``` r
dplyr::glimpse(dict)
#> Rows: 97
#> Columns: 14
#> $ response_column_id <chr> "QID1125_TEXT", "QID1126", "QID1126", "QID1126", "Q…
#> $ row_source         <chr> "question", "question", "question", "question", "qu…
#> $ qid                <chr> "QID1125", "QID1126", "QID1126", "QID1126", "QID112…
#> $ question_name      <chr> "DEM.AGE.1.0", "DEM.COR.1.0", "DEM.COR.1.0", "DEM.C…
#> $ variable_name      <chr> "DEM.AGE.1.0", "DEM.COR.1.0", "DEM.COR.1.0", "DEM.C…
#> $ block              <chr> "Demographics", "Demographics", "Demographics", "De…
#> $ question           <chr> "How old are you now?", "Please confirm where you a…
#> $ item               <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ level              <chr> NA, "1", "2", "3", "4", "5", "0", "1", "1", "1", "1…
#> $ label              <chr> NA, "England", "Scotland", "Wales", "Northern Irela…
#> $ type               <chr> "TE", "MC", "MC", "MC", "MC", "MC", "MC", "MC", "MC…
#> $ selector           <chr> "SL", "DL", "DL", "DL", "DL", "DL", "SAVR", "SAVR",…
#> $ sub_selector       <chr> NA, NA, NA, NA, NA, NA, "TX", "TX", "TX", "TX", "TX…
#> $ content_type       <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
```

The columns that identify a row are:

- `response_column_id`: the column name in the downloaded Qualtrics
  response file, such as `QID1147_5_TEXT`. This is how the dictionary is
  matched to your data.
- `row_source`: what the column comes from: a survey `question`, an
  `embedded_data` field, a `scoring` variable, or a `text_analysis`
  sidecar.
- `qid`: the Qualtrics question the column belongs to. One question can
  produce many columns, such as one per matrix item or per ticked
  choice.
- `question_name`: the question name you set in the Qualtrics survey
  editor.
- `variable_name`: the name the column gets in your labelled data,
  unique and safe for export.

The other columns describe the content: the Survey Block, question text,
item text, response level and label, and the Qualtrics question type. A
column with response options takes one row per option. The first PHQ-9
item, “Little interest or pleasure in doing things”, is one response
column with four levels:

``` r
dict[
  dict$variable_name == "PHQ.DEP.1.0",
  c("response_column_id", "variable_name", "level", "label")
]
#> # A tibble: 4 × 4
#>   response_column_id variable_name level label                  
#>   <chr>              <chr>         <chr> <chr>                  
#> 1 QID127303424_1     PHQ.DEP.1.0   0     Not at all             
#> 2 QID127303424_1     PHQ.DEP.1.0   1     Several days           
#> 3 QID127303424_1     PHQ.DEP.1.0   2     More than half the days
#> 4 QID127303424_1     PHQ.DEP.1.0   3     Nearly every day
```

Embedded data fields and scoring variables appear alongside the
questions:

``` r
dict[
  dict$row_source != "question",
  c("response_column_id", "row_source", "variable_name", "question")
]
#> # A tibble: 8 × 4
#>   response_column_id row_source    variable_name     question                   
#>   <chr>              <chr>         <chr>             <chr>                      
#> 1 Age                embedded_data Age               Embedded Data: Age         
#> 2 sex                embedded_data sex               Embedded Data: sex         
#> 3 PHQRaw             embedded_data PHQRaw            Embedded Data: PHQRaw      
#> 4 CurrentDepression  embedded_data CurrentDepression Embedded Data: CurrentDepr…
#> 5 GADRaw             embedded_data GADRaw            Embedded Data: GADRaw      
#> 6 CurrentGAD         embedded_data CurrentGAD        Embedded Data: CurrentGAD  
#> 7 SC_5gvP3Yxax8TWHcN scoring       GAD_7             Scoring Variable: GAD-7    
#> 8 SC_3rtKxr7Lzd9OHhX scoring       PHQ_9             Scoring Variable: PHQ-9
```

## Check the dictionary

`dict_validate()` reports Validation Findings: dictionary rows worth a
second look before you analyse the data.

``` r
findings <- dict_validate(dict)$validation_findings
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

All findings here are suggestive: they report variable names that
qualtdict repaired. The items of the PHQ-9 and GAD-7 matrices and the
choices of the multi-select qualifications question share one Question
Name, so their names are numbered to keep them unique, and the hyphen in
the scoring variable names `PHQ-9` and `GAD-7` is not safe in an export
variable name, so they become `PHQ_9` and `GAD_7`.

A definite finding, such as two choices sharing one response code, would
mean the column cannot be labelled reliably. `dict_assert_valid()`
raises an error for such level-label findings, and
`survey_fetch_labelled()` runs it before downloading anything.

## Labelled survey data

`survey_fetch_labelled()` downloads the responses through qualtRics and
applies the dictionary:

``` r
survey_dat <- survey_fetch_labelled(dict)
```

For the example survey, that call returns
`glad_example$labelled_survey_data`:

``` r
survey_dat <- glad_example$labelled_survey_data
dim(survey_dat)
#> [1] 20 43
head(survey_dat[, c(
  "externalDataReference", "DEM.AGE.1.0", "DEM.SEX.1.0", "PHQ.DEP.1.0",
  "HEAD", "PHQRaw", "PHQ_9"
)])
#> # A tibble: 6 × 7
#>   externalDataReference DEM.AGE.1.0 DEM.SEX.1.0 PHQ.DEP.1.0  HEAD PHQRaw PHQ_9
#>   <chr>                       <dbl>       <dbl>       <dbl> <dbl>  <dbl> <dbl>
#> 1 example-001                    34           0           3     0     16    16
#> 2 example-002                    53           0           1     3     14    14
#> 3 example-003                    63           1           0     1     13    13
#> 4 example-004                    37           0           1     3     12    12
#> 5 example-005                    68           0           3     3     15    15
#> 6 example-006                    70           0           3     3     14    14
```

Columns are named by `variable_name`. Choice columns carry their
response options as value labels, and text and numeric columns carry the
question text as a variable label:

``` r
attr(survey_dat$PHQ.DEP.1.0, "labels")
#>              Not at all            Several days More than half the days 
#>                       0                       1                       2 
#>        Nearly every day 
#>                       3
attr(survey_dat$DEM.AGE.1.0, "label")
#> [1] "How old are you now?"
```

so tools that read labels, such as sjlabelled, can use them directly:

``` r
table(sjlabelled::as_label(survey_dat$DEM.SEX.1.0))
#> 
#>   Male Female 
#>     11      9
```

`survey_export_findings()` lists dictionary columns that were missing
from the download. For the example there are none:

``` r
survey_export_findings(survey_dat)
#> # A tibble: 0 × 5
#> # ℹ 5 variables: finding <chr>, response_column_id <chr>, qid <chr>,
#> #   variable_name <chr>, reason <chr>
```

## One block at a time

`dict_split_blocks()` and `survey_split_blocks()` split the dictionary
and the data by Survey Block. Embedded data fields and scoring variables
are not assigned to a block by default, so they are kept under
`"..unassigned"`.

``` r
dict_blocks <- dict_split_blocks(dict)
names(dict_blocks)
#> [1] "..unassigned" "Demographics" "GAD7"         "PHQ9"
unique(dict_blocks$GAD7[, c("response_column_id", "variable_name", "item")])
#> # A tibble: 8 × 3
#>   response_column_id variable_name item                                         
#>   <chr>              <chr>         <chr>                                        
#> 1 QID127303430_1     HEAD          Feeling nervous, anxious, or on edge         
#> 2 QID127303430_2     HEAD.1        Not being able to stop or control worrying   
#> 3 QID127303430_3     HEAD.2        Worrying too much about different things     
#> 4 QID127303430_4     HEAD.3        Trouble relaxing                             
#> 5 QID127303430_5     HEAD.4        Being so restless that it is hard to sit sti…
#> 6 QID127303430_6     HEAD.5        Becoming easily annoyed or irritable         
#> 7 QID127303430_7     HEAD.6        Feeling afraid as if something awful might h…
#> 8 QID127303448       Q5            <NA>

survey_blocks <- survey_split_blocks(survey_dat)
names(survey_blocks$GAD7)
#>  [1] "externalDataReference" "startDate"             "endDate"              
#>  [4] "HEAD"                  "HEAD.1"                "HEAD.2"               
#>  [7] "HEAD.3"                "HEAD.4"                "HEAD.5"               
#> [10] "HEAD.6"                "Q5"
```

## Semantic Names

If the Question Names in your survey are not good analysis names, ask
for Semantic Names, generated from the question text. Here
`block_pattern` turns each Survey Block name into a prefix:

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

Semantic Names are a readable convenience, not stable identifiers: they
can change with the survey text or the package version. They need the
suggested packages slowraker, SnowballC, stringi and tidyr.

## How To Cite

If `qualtdict` contributes to published work, please cite it. The
citation for the installed version is returned by `citation()`.

``` r
citation("qualtdict")
```

## Learn More

- [Introduction to
  qualtdict](https://lyh970817.github.io/qualtdict/articles/qualtdict.html)
  is the package vignette, which works through the same Variable
  Dictionary to Labelled Survey Data path in more detail.
- The [reference site](https://lyh970817.github.io/qualtdict/) documents
  every exported function.
- The [contributing
  guide](https://github.com/lyh970817/qualtdict/blob/main/.github/CONTRIBUTING.md)
  describes how to propose a change.
- This project is released with a [Contributor Code of
  Conduct](https://github.com/lyh970817/qualtdict/blob/main/.github/CODE_OF_CONDUCT.md).
  By contributing to this project you agree to abide by its terms.
