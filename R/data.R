#' Example survey metadata, Variable Dictionary, and Labelled Survey Data
#'
#' A small offline example of everything qualtdict reads and produces, so its
#' output can be explored without Qualtrics credentials. The survey structure
#' is a subset of the GLAD Study sign-up questionnaire: four questions of the
#' `Demographics` Survey Block and the whole `PHQ9` and `GAD7` Survey Blocks,
#' with six Embedded Data Fields and two Scoring Variables. Together they
#' cover a text entry question, a drop-down list, single-answer and
#' multi-answer multiple choice (including a choice with its own text entry
#' column), and two rating-scale matrices, with rows from the `question`,
#' `embedded_data`, and `scoring` Dictionary Row Sources. The source survey
#' defines no Text-analysis Sidecars, so the `text_analysis` Dictionary Row
#' Source does not occur.
#'
#' The Participant Response Data in `responses` are synthetic: 20 responses
#' drawn at random from each column's valid levels, with Embedded Data
#' Fields and Scoring Variables computed from those answers, fake
#' identifiers, and 2026 dates. They are not answers from GLAD Study
#' participants.
#'
#' @format A list with six components:
#' \describe{
#'   \item{`survey_metadata`}{A list shaped like the return value of
#'     [qualtRics::metadata()], with components `metadata`, `questions`,
#'     `responsecounts`, `blocks`, `flow`, and `embedded_data`. Response
#'     counts are zero and Embedded Data default values are blank.}
#'   \item{`survey_description`}{A list shaped like the return value of
#'     [qualtRics::fetch_description()], with components `metadata`, `flow`,
#'     `blocks`, `questions`, and `scoring`. Embedded Data values in the
#'     Survey Flow are blank.}
#'   \item{`response_column_map`}{A data frame: the `column_map` attribute of
#'     a [qualtRics::fetch_survey()] download, with one row per downloaded
#'     column. `qname` holds the Response Column ID.}
#'   \item{`responses`}{A tibble of 20 rows and 48 columns of synthetic
#'     Participant Response Data, shaped like a [qualtRics::fetch_survey()]
#'     download with `import_id = TRUE`, `convert = FALSE`, `label = FALSE`,
#'     and `breakout_sets = TRUE`. Columns are named by Response Column ID,
#'     and `response_column_map` is attached as its `column_map`
#'     attribute.}
#'   \item{`dictionary`}{The Variable Dictionary (a `qualtdict` object, 97
#'     rows describing 37 Response Column IDs) that [dict_generate()] returns
#'     for this survey with `variable_name = "question_name"`.}
#'   \item{`labelled_survey_data`}{The Labelled Survey Data that
#'     [fetch_labelled_survey_data()] returns when `dictionary` is applied to
#'     `responses`.}
#' }
#'
#' @source Derived from the survey metadata of the GLAD Study sign-up
#'   questionnaire. Survey, owner, brand, and account identifiers, dates,
#'   response counts, question JavaScript, and Embedded Data values were
#'   removed or replaced with fixed fake values; the survey ID is the fake
#'   `"SV_glad0example00000"`. The responses are fully synthetic. The script
#'   that builds the data is `data-raw/glad_example.R` in the package source
#'   repository.
#'
#' @examples
#' names(glad_example)
#'
#' # The Variable Dictionary: one row per Response Column ID and level
#' dict <- glad_example$dictionary
#' dict[, c(
#'   "response_column_id", "row_source", "variable_name", "level", "label"
#' )]
#'
#' # Validation Findings: here only suggestive name repairs
#' findings <- dict_validate(dict)$validation_findings
#' findings[, c("finding", "severity", "response_column_id", "reason")]
#'
#' # Labelled Survey Data: Export Variables renamed to `variable_name`
#' glad_example$labelled_survey_data[, 1:6]
"glad_example"
