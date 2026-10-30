# Introduction to qualtdict

A [Qualtrics](https://www.qualtrics.com/) response download names its
columns with identifiers such as `QID12_3_TEXT`, and what each
identifier means is spread across several nested metadata objects:
questions, choices, matrix items, loops, embedded data and scoring
rules. Before any analysis, every column has to be matched back to that
metadata, renamed, labelled and documented.

qualtdict does that mapping for you. From the survey metadata it builds
a Variable Dictionary, one table describing every exported response
column, which you can inspect, validate and share as a codebook. It then
applies the dictionary to the responses downloaded through
[qualtRics](https://github.com/ropensci/qualtRics), so that every column
carries its variable name and labels.

The authors and contributors for this R package are not affiliated with
Qualtrics, and Qualtrics does not offer support for this R package.

## The workflow

1.  [`dict_generate()`](https://lyh970817.github.io/qualtdict/reference/dict_generate.md)
    builds a Variable Dictionary from the survey metadata.
2.  [`dict_validate()`](https://lyh970817.github.io/qualtdict/reference/dict_validate.md)
    reports Validation Findings in the dictionary.
3.  [`survey_fetch_labelled()`](https://lyh970817.github.io/qualtdict/reference/survey_fetch_labelled.md)
    downloads the responses and applies the dictionary, returning
    Labelled Survey Data.
4.  [`dict_split_blocks()`](https://lyh970817.github.io/qualtdict/reference/dict_split_blocks.md)
    and
    [`survey_split_blocks()`](https://lyh970817.github.io/qualtdict/reference/survey_split_blocks.md)
    give one view per Survey Block.

Steps 1 and 3 read your survey from Qualtrics and need API credentials.
Your institution must allow API access, and it must be enabled for your
account; the person who manages your Qualtrics account can give you your
API token and base URL. Store them once with
[`qualtrics_api_credentials()`](https://docs.ropensci.org/qualtRics/reference/qualtrics_api_credentials.html),
which qualtdict re-exports from qualtRics:

``` r

library(qualtdict)

qualtrics_api_credentials(
  api_key = "<YOUR-QUALTRICS_API_KEY>",
  base_url = "<YOUR-QUALTRICS_BASE_URL>",
  install = TRUE
)
```

## The example survey

This vignette uses `glad_example`, an example bundled with the package.
It holds the metadata of a subset of the GLAD Study sign-up survey (four
demographic questions, the PHQ-9 and GAD-7 blocks, six embedded data
fields and two scoring variables), 20 synthetic responses, and the
objects qualtdict produces from them.

``` r

library(qualtdict)

names(glad_example)
#> [1] "survey_metadata"      "survey_description"   "response_column_map" 
#> [4] "responses"            "dictionary"           "labelled_survey_data"
```

For your own survey, the calls below would download the metadata and the
responses from Qualtrics. They are not run here; for the example survey
they return exactly `glad_example$dictionary` and
`glad_example$labelled_survey_data`, which the rest of this vignette
uses.

``` r

survey_id <- "SV_XXXXXXXXXXXXXXXX"

dict <- dict_generate(survey_id, variable_name = "question_name")
survey_dat <- survey_fetch_labelled(dict)
```

``` r

dict <- glad_example$dictionary
survey_dat <- glad_example$labelled_survey_data
```

## The Variable Dictionary

``` r

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

Each row describes one downloaded response column, or one response level
of it. The columns are:

- `response_column_id`: the column name in the downloaded Qualtrics
  response file. This is how the dictionary is matched to your data.
- `row_source`: what the column comes from (see below).
- `qid`: the Qualtrics question the column belongs to, such as
  `QID1125`. One question can produce many response columns.
- `question_name`: the question name set in the Qualtrics survey editor.
- `variable_name`: the name the column gets in Labelled Survey Data. It
  is unique and safe for export.
- `block`: the Survey Block the question belongs to.
- `question` and `item`: the question text and, for matrix and
  multi-column questions, the text of the row or choice.
- `level` and `label`: a response value and its label. A column with
  response options takes one row per option.
- `type`, `selector`, `sub_selector` and `content_type`: the Qualtrics
  question type and its settings.

`variable_name = "question_name"` builds `variable_name` from the
Qualtrics Question Name, repaired where needed so that every name is
unique and export-safe. The original `question_name` stays in the
dictionary.

A multi-select question gets one response column per choice, and a
choice with a text box gets an extra `_TEXT` column:

``` r

dict[
  which(dict$qid == "QID1147"),
  c("response_column_id", "variable_name", "level", "label")
]
#> # A tibble: 8 × 4
#>   response_column_id variable_name level  label                                 
#>   <chr>              <chr>         <chr>  <chr>                                 
#> 1 QID1147_1          DEM.EDU.1.0   1      College or university degree          
#> 2 QID1147_2          DEM.EDU.1.0.1 1      A levels/AS levels or equivalent      
#> 3 QID1147_3          DEM.EDU.1.0.2 1      O levels/GCSEs or equivalent          
#> 4 QID1147_4          DEM.EDU.1.0.3 1      CSEs or equivalent                    
#> 5 QID1147_6          DEM.EDU.1.0.4 1      NVQ or HND or HNC or equivalent       
#> 6 QID1147_5          DEM.EDU.1.0.5 1      Other professional qualifications     
#> 7 QID1147_5_TEXT     DEM.EDU.1.0.6 5_TEXT Other professional qualifications_TEXT
#> 8 QID1147_-999       DEM.EDU.1.0.7 1      Prefer not to answer
```

A matrix question gets one response column per item, each with the
scale’s levels:

``` r

dict[
  which(dict$qid == "QID127303424" & dict$level == "0"),
  c("response_column_id", "variable_name", "item")
]
#> # A tibble: 9 × 3
#>   response_column_id variable_name item                                         
#>   <chr>              <chr>         <chr>                                        
#> 1 QID127303424_1     PHQ.DEP.1.0   Little interest or pleasure in doing things  
#> 2 QID127303424_2     PHQ.DEP.1.0.1 Feeling down, depressed or hopeless          
#> 3 QID127303424_3     PHQ.DEP.1.0.2 Trouble falling or staying asleep, or sleepi…
#> 4 QID127303424_4     PHQ.DEP.1.0.3 Feeling tired or having little energy        
#> 5 QID127303424_5     PHQ.DEP.1.0.4 Poor appetite or overeating                  
#> 6 QID127303424_6     PHQ.DEP.1.0.5 Feeling bad about yourself or that you are a…
#> 7 QID127303424_7     PHQ.DEP.1.0.6 Trouble concentrating on things, such as rea…
#> 8 QID127303424_8     PHQ.DEP.1.0.7 Moving or speaking so slowly that other peop…
#> 9 QID127303424_9     PHQ.DEP.1.0.8 Thoughts that you would be better off dead o…
```

### Row sources

Most rows come from survey questions, but Qualtrics also exports columns
that are defined elsewhere in the survey. `row_source` says which:

- `question`: a response to a survey question.
- `embedded_data`: an Embedded Data Field set in the Survey Flow.
- `scoring`: a Scoring Variable computed by Qualtrics scoring.
- `text_analysis`: a Text-analysis Sidecar added by Qualtrics text
  analysis of a text response. The example survey has none.

``` r

table(dict$row_source)
#> 
#> embedded_data      question       scoring 
#>             6            89             2

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

## Validation Findings

[`dict_validate()`](https://lyh970817.github.io/qualtdict/reference/dict_validate.md)
returns a list with two components: `validation_findings`, one row per
finding, and `level_label_pairs`, the distinct level-label codings of
the survey’s questions.

``` r

validation <- dict_validate(dict)
names(validation)
#> [1] "validation_findings" "level_label_pairs"

validation$validation_findings[, c(
  "finding", "severity", "response_column_id", "original_candidate",
  "variable_name", "reason"
)]
#> # A tibble: 23 × 6
#>    finding   severity response_column_id original_candidate variable_name reason
#>    <chr>     <chr>    <chr>              <chr>              <chr>         <chr> 
#>  1 repaired… suggest… QID1147_2          DEM.EDU.1.0        DEM.EDU.1.0.1 dupli…
#>  2 repaired… suggest… QID1147_3          DEM.EDU.1.0        DEM.EDU.1.0.2 dupli…
#>  3 repaired… suggest… QID1147_4          DEM.EDU.1.0        DEM.EDU.1.0.3 dupli…
#>  4 repaired… suggest… QID1147_6          DEM.EDU.1.0        DEM.EDU.1.0.4 dupli…
#>  5 repaired… suggest… QID1147_5          DEM.EDU.1.0        DEM.EDU.1.0.5 dupli…
#>  6 repaired… suggest… QID1147_5_TEXT     DEM.EDU.1.0        DEM.EDU.1.0.6 dupli…
#>  7 repaired… suggest… QID1147_-999       DEM.EDU.1.0        DEM.EDU.1.0.7 dupli…
#>  8 repaired… suggest… QID127303424_2     PHQ.DEP.1.0        PHQ.DEP.1.0.1 dupli…
#>  9 repaired… suggest… QID127303424_3     PHQ.DEP.1.0        PHQ.DEP.1.0.2 dupli…
#> 10 repaired… suggest… QID127303424_4     PHQ.DEP.1.0        PHQ.DEP.1.0.3 dupli…
#> # ℹ 13 more rows
```

Each finding has a `severity`:

- **Suggestive** findings are worth a look but leave the column’s data
  and identity sound. All findings here are of this kind: matrix items
  and multi-select choices share one Question Name, so their variable
  names are numbered, and the hyphens in `PHQ-9` and `GAD-7` are
  replaced so the scoring variables get export-safe names.
- **Definite** findings mean the column cannot be interpreted or
  identified reliably: duplicate or unsafe final variable names, or
  level-label codings that are not one-to-one, such as two choices
  sharing one response code.

A clean result is a consistency check, not proof that the survey itself
is correct.

[`dict_assert_valid()`](https://lyh970817.github.io/qualtdict/reference/dict_assert_valid.md)
is the strict version for the definite findings that make labelling
wrong or impossible, the Export-blocking level-label codings. It returns
the dictionary invisibly when there are none, and raises an error naming
the offending columns otherwise. The example dictionary passes:

``` r

dict_assert_valid(dict)
```

[`survey_fetch_labelled()`](https://lyh970817.github.io/qualtdict/reference/survey_fetch_labelled.md)
runs this check before it downloads anything, so a dictionary that
cannot be labelled fails before the download.

## Labelled Survey Data

``` r

dplyr::glimpse(survey_dat)
#> Rows: 20
#> Columns: 43
#> $ externalDataReference <chr> "example-001", "example-002", "example-003", "ex…
#> $ startDate             <dttm> 2026-01-08 13:05:38, 2026-01-31 19:57:56, 2026-…
#> $ endDate               <dttm> 2026-01-08 13:44:46, 2026-01-31 20:26:22, 2026-…
#> $ DEM.AGE.1.0           <dbl> 34, 53, 63, 37, 68, 70, 59, 30, 54, 20, 21, 26, …
#> $ DEM.COR.1.0           <dbl> 4, 3, 3, 3, 4, 1, 1, 2, 3, 5, 2, 2, 5, 2, 3, 3, …
#> $ DEM.SEX.1.0           <dbl> 0, 0, 1, 0, 0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 0, 0, …
#> $ DEM.EDU.1.0           <dbl> NA, 1, 1, NA, NA, 1, NA, 1, NA, NA, NA, 1, 1, 1,…
#> $ DEM.EDU.1.0.1         <dbl> NA, NA, 1, NA, NA, 1, NA, NA, NA, NA, NA, NA, 1,…
#> $ DEM.EDU.1.0.2         <dbl> NA, NA, 1, 1, NA, NA, NA, NA, 1, NA, NA, 1, NA, …
#> $ DEM.EDU.1.0.3         <dbl> NA, NA, NA, NA, NA, 1, 1, NA, 1, NA, 1, NA, NA, …
#> $ DEM.EDU.1.0.4         <dbl> NA, 1, NA, 1, NA, 1, 1, NA, 1, NA, NA, 1, NA, 1,…
#> $ DEM.EDU.1.0.5         <dbl> 1, NA, 1, NA, NA, 1, 1, NA, NA, NA, 1, NA, NA, N…
#> $ DEM.EDU.1.0.6         <chr> "example note 1", NA, "example note 3", NA, NA, …
#> $ DEM.EDU.1.0.7         <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, 1, NA, NA, N…
#> $ PHQ.DEP.1.0           <dbl> 3, 1, 0, 1, 3, 3, 1, 0, 1, 0, 1, 3, 3, 1, 1, 1, …
#> $ PHQ.DEP.1.0.1         <dbl> 3, 1, 1, 1, 0, 0, 1, 1, 3, 3, 0, 0, 3, 3, 2, 1, …
#> $ PHQ.DEP.1.0.2         <dbl> 3, 0, 3, 1, 3, 2, 2, 0, 1, 0, 0, 3, 1, 3, 0, 0, …
#> $ PHQ.DEP.1.0.3         <dbl> 1, 2, 3, 1, 1, 0, 3, 3, 3, 3, 3, 0, 2, 2, 2, 0, …
#> $ PHQ.DEP.1.0.4         <dbl> 3, 2, 1, 3, 0, 3, 3, 2, 2, 0, 3, 1, 0, 3, 3, 2, …
#> $ PHQ.DEP.1.0.5         <dbl> 0, 2, 1, 2, 3, 0, 2, 3, 3, 1, 1, 1, 2, 1, 3, 0, …
#> $ PHQ.DEP.1.0.6         <dbl> 0, 1, 1, 1, 0, 1, 2, 0, 3, 1, 3, 0, 2, 2, 0, 3, …
#> $ PHQ.DEP.1.0.7         <dbl> 1, 3, 2, 1, 2, 3, 0, 3, 0, 2, 1, 0, 1, 1, 3, 3, …
#> $ PHQ.DEP.1.0.8         <dbl> 2, 2, 1, 1, 3, 2, 0, 0, 2, 0, 3, 1, 1, 1, 3, 1, …
#> $ PHQ.EFT.1.0           <dbl> 1, 3, 5, 1, 3, 4, 4, 3, 4, 1, 5, 1, 5, 5, 5, 4, …
#> $ HEAD                  <dbl> 0, 3, 1, 3, 3, 3, 3, 0, 0, 1, 3, 0, 3, 3, 0, 3, …
#> $ HEAD.1                <dbl> 3, 1, 3, 3, 2, 2, 0, 3, 1, 3, 3, 3, 0, 3, 3, 1, …
#> $ HEAD.2                <dbl> 2, 1, 0, 0, 2, 3, 2, 2, 0, 1, 3, 1, 1, 0, 0, 1, …
#> $ HEAD.3                <dbl> 1, 1, 1, 0, 3, 3, 3, 2, 2, 0, 2, 1, 0, 3, 2, 1, …
#> $ HEAD.4                <dbl> 2, 0, 0, 3, 0, 1, 2, 3, 2, 2, 1, 0, 1, 0, 3, 0, …
#> $ HEAD.5                <dbl> 3, 2, 2, 0, 0, 2, 3, 2, 2, 0, 1, 1, 3, 3, 3, 3, …
#> $ HEAD.6                <dbl> 1, 1, 0, 2, 0, 2, 1, 1, 3, 1, 3, 2, 3, 0, 1, 0, …
#> $ Q5                    <dbl> 2, 1, 3, 1, 4, 2, 4, 3, 4, 3, 2, 2, 1, 3, 3, 1, …
#> $ Age                   <dbl> 34, 53, 63, 37, 68, 70, 59, 30, 54, 20, 21, 26, …
#> $ sex                   <dbl> 0, 0, 1, 0, 0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 0, 0, …
#> $ PHQRaw                <dbl> 16, 14, 13, 12, 15, 14, 14, 12, 18, 10, 15, 9, 1…
#> $ CurrentDepression     <dbl> 3, 2, 2, 2, 3, 2, 2, 2, 3, 2, 3, 1, 3, 3, 3, 2, …
#> $ GADRaw                <dbl> 12, 9, 7, 11, 10, 16, 14, 13, 10, 8, 16, 8, 11, …
#> $ CurrentGAD            <dbl> 1, 0, 0, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, …
#> $ GAD_7                 <dbl> 12, 9, 7, 11, 10, 16, 14, 13, 10, 8, 16, 8, 11, …
#> $ PHQ_9                 <dbl> 16, 14, 13, 12, 15, 14, 14, 12, 18, 10, 15, 9, 1…
#> $ recordedDate          <dttm> 2026-01-08 13:44:47, 2026-01-31 20:26:23, 2026-…
#> $ ResponseId            <chr> "R_0EzWRxwAbYshrZF", "R_VAw94UQKF3Yt6xr", "R_B48…
#> $ Progress              <dbl> 100, 100, 100, 100, 100, 100, 100, 100, 100, 100…
```

Export Variables are renamed from `response_column_id` to
`variable_name`. By default
[`survey_fetch_labelled()`](https://lyh970817.github.io/qualtdict/reference/survey_fetch_labelled.md)
also keeps `externalDataReference`, `startDate` and `endDate`, and
appends the response metadata columns `recordedDate`, `ResponseId` and
`Progress`.

Choice columns carry their response options as value labels, and text
and numeric columns carry the question text as a variable label:

``` r

attr(survey_dat$DEM.COR.1.0, "labels")
#>          England         Scotland            Wales Northern Ireland 
#>                1                2                3                4 
#>       Outside UK 
#>                5
attr(survey_dat$DEM.AGE.1.0, "label")
#> [1] "How old are you now?"
```

Packages that read these attributes, such as sjlabelled, can use them
directly, for example to turn a coded column into a factor:

``` r

table(sjlabelled::as_label(survey_dat$DEM.COR.1.0))
#> 
#>          England         Scotland            Wales Northern Ireland 
#>                3                6                6                3 
#>       Outside UK 
#>                2
```

The Variable Dictionary used to build the data travels with it as the
`dict` attribute, so the question and item text of every column stay at
hand:

``` r

survey_dict <- attr(survey_dat, "dict")
unique(survey_dict[
  survey_dict$variable_name == "PHQ.DEP.1.0.2",
  c("variable_name", "item")
])
#> # A tibble: 1 × 2
#>   variable_name item                                                   
#>   <chr>         <chr>                                                  
#> 1 PHQ.DEP.1.0.2 Trouble falling or staying asleep, or sleeping too much
```

### Labelled Export Findings

While applying the dictionary,
[`survey_fetch_labelled()`](https://lyh970817.github.io/qualtdict/reference/survey_fetch_labelled.md)
records Labelled Export Findings: for example, a dictionary column that
is missing from the downloaded data.
[`survey_export_findings()`](https://lyh970817.github.io/qualtdict/reference/survey_export_findings.md)
returns them. The example has none:

``` r

survey_export_findings(survey_dat)
#> # A tibble: 0 × 5
#> # ℹ 5 variables: finding <chr>, response_column_id <chr>, qid <chr>,
#> #   variable_name <chr>, reason <chr>
```

### Download options

[`survey_fetch_labelled()`](https://lyh970817.github.io/qualtdict/reference/survey_fetch_labelled.md)
calls
[`qualtRics::fetch_survey()`](https://docs.ropensci.org/qualtRics/reference/fetch_survey.html)
with the settings it needs to match columns to `response_column_id`:
`import_id = TRUE`, `label = FALSE`, `convert = FALSE` and
`breakout_sets = TRUE`. You may pass other `fetch_survey()` arguments,
such as `include_questions`, but not ones that override those settings.

By default, variables with Validation Findings stay in the data.
`exclude_findings = "definite"` drops the variables with definite
findings, and `exclude_findings = "validation"` drops every variable
with any finding:

``` r

survey_dat <- survey_fetch_labelled(
  dict,
  extra_columns = c("externalDataReference", "startDate", "endDate"),
  exclude_findings = "definite"
)
```

## Survey Block views

[`dict_generate()`](https://lyh970817.github.io/qualtdict/reference/dict_generate.md)
returns one Variable Dictionary and
[`survey_fetch_labelled()`](https://lyh970817.github.io/qualtdict/reference/survey_fetch_labelled.md)
returns one data frame. To work with one Survey Block at a time, split
them. Rows without a block, here the embedded data fields and scoring
variables, are kept under `"..unassigned"`.

``` r

dict_by_block <- dict_split_blocks(dict)
names(dict_by_block)
#> [1] "..unassigned" "Demographics" "GAD7"         "PHQ9"
unique(dict_by_block$PHQ9[, c("response_column_id", "variable_name")])
#> # A tibble: 10 × 2
#>    response_column_id variable_name
#>    <chr>              <chr>        
#>  1 QID127303424_1     PHQ.DEP.1.0  
#>  2 QID127303424_2     PHQ.DEP.1.0.1
#>  3 QID127303424_3     PHQ.DEP.1.0.2
#>  4 QID127303424_4     PHQ.DEP.1.0.3
#>  5 QID127303424_5     PHQ.DEP.1.0.4
#>  6 QID127303424_6     PHQ.DEP.1.0.5
#>  7 QID127303424_7     PHQ.DEP.1.0.6
#>  8 QID127303424_8     PHQ.DEP.1.0.7
#>  9 QID127303424_9     PHQ.DEP.1.0.8
#> 10 QID127303425       PHQ.EFT.1.0

data_by_block <- survey_split_blocks(survey_dat)
names(data_by_block)
#> [1] "..unassigned" "Demographics" "GAD7"         "PHQ9"
names(data_by_block$PHQ9)
#>  [1] "externalDataReference" "startDate"             "endDate"              
#>  [4] "PHQ.DEP.1.0"           "PHQ.DEP.1.0.1"         "PHQ.DEP.1.0.2"        
#>  [7] "PHQ.DEP.1.0.3"         "PHQ.DEP.1.0.4"         "PHQ.DEP.1.0.5"        
#> [10] "PHQ.DEP.1.0.6"         "PHQ.DEP.1.0.7"         "PHQ.DEP.1.0.8"        
#> [13] "PHQ.EFT.1.0"
```

By default each block’s data keeps `externalDataReference`, `startDate`
and `endDate` so the blocks can be joined back together.

## Semantic Names

If the Question Names in your survey are not good analysis names, ask
for Semantic Names. They are generated from the question text, block
information and response metadata, and need the suggested packages
slowraker, SnowballC, stringi and tidyr. `block_pattern` turns each
Survey Block name into a Block Prefix, which is prepended to the
Semantic Names in that block so that variables from different blocks
stay distinct:

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

Semantic Names are best-effort readable conveniences. They are not
stable across package versions or survey text changes, so do not treat
them as permanent identifiers. For long text, Semantic Names select
important words from ranked keywords and keep them in the order they
appear in the text.

You can also preprocess the text used only for Semantic Name generation:

``` r

drop_intro_text <- function(dict) {
  dict$question <- sub("^Please answer: ", "", dict$question)
  dict
}

dict <- dict_generate(
  survey_id,
  variable_name = "semantic_name",
  semantic_name_preprocess = drop_intro_text
)
```

`semantic_name_preprocess` receives the full dictionary and runs only on
the Semantic Name path. Temporary helper columns it creates are not
included in the returned Variable Dictionary.

## Learn more

The help page of each function, such as
[`?dict_generate`](https://lyh970817.github.io/qualtdict/reference/dict_generate.md)
or
[`?survey_fetch_labelled`](https://lyh970817.github.io/qualtdict/reference/survey_fetch_labelled.md),
documents its arguments in full.
