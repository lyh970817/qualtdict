#' Generate a Variable Dictionary for a survey
#'
#' Generate a Variable Dictionary from Qualtrics survey metadata retrieved via
#' \code{qualtRics}.
#'
#' @param surveyID String. Unique Qualtrics survey ID. Live metadata retrieval
#' requires Qualtrics credentials configured for \code{qualtRics}.
#' @param variable_name String. Source for the final \code{variable_name}
#' column in the Variable Dictionary. Use \code{question_name} for the raw
#' Qualtrics Question Name or \code{semantic_name} for a generated Semantic
#' Name based on question text and response metadata.
#' @param name Deprecated compatibility alias for \code{variable_name}. The
#' legacy \code{easy_name} value is accepted as \code{semantic_name} with a
#' warning.
#' @param block_pattern Function. A function that given the name of a
#' Survey Block, returns a Block Prefix to prepend to Semantic Names in that
#' block. Defaults to \code{NULL}.
#' @param semantic_name_preprocess Function. An optional function that receives
#' the full post-normalisation dictionary and returns a modified dictionary for
#' Semantic Name generation. It runs only when
#' \code{variable_name = "semantic_name"}. Temporary helper columns added by
#' this function are not included in the returned Variable Dictionary.
#' @param preprocess Deprecated compatibility alias for
#' \code{semantic_name_preprocess}.
#' @param quiet Boolean. If \code{TRUE}, suppress routine progress messages and
#' progress bars. Defaults to \code{TRUE}.
#' @param block_sep String. Separator between variable names and block
#' prefixes returned by \code{block_pattern}. Defaults to ".".
#' @details
#' The returned Variable Dictionary preserves \code{response_column_id} as the
#' downloaded response-column key, \code{qid} as the bare Qualtrics question
#' identifier, \code{question_name} as the raw Qualtrics naming reference, and
#' \code{variable_name} as the final export-safe Dictionary Variable Name used
#' by Labelled Survey Data.
#'
#' When \code{variable_name = "semantic_name"}, the Variable Dictionary also
#' includes \code{semantic_name}. Semantic Names are readable best-effort
#' conveniences generated from survey text and metadata; they are not stable
#' guarantees across package versions or survey text changes. For long text,
#' Semantic Names select important words from ranked keywords and preserve those
#' selected words in the order they appear in the naming text.
#'
#' @return
#' A Variable Dictionary: a \code{qualtdict} data frame.
#'
#' @export
#' @examples
#' \dontrun{
#' survey_id <- "SV_XXXXXXXXXXXXXXXX"
#'
#' # Create a function for \code{block_pattern}
#' # that returns the first three letters of a string
#' block_pattern <- function(x) {
#'   substring(x, 1, 3)
#' }
#'
#'
#' mydict <- dict_generate(survey_id,
#'   variable_name = "semantic_name",
#'   block_pattern = block_pattern,
#'   block_sep = "."
#' )
#' }
#'
dict_generate <- function(surveyID,
                          variable_name = c("question_name", "semantic_name"),
                          name = NULL,
                          block_pattern = NULL,
                          block_sep = ".",
                          semantic_name_preprocess = NULL,
                          preprocess = NULL,
                          quiet = TRUE) {
  checkarg_isstring(surveyID, null_okay = FALSE)
  checkarg_isfunction(block_pattern)
  checkarg_isstring(block_sep, null_okay = FALSE)
  checkarg_isfunction(semantic_name_preprocess)
  checkarg_isfunction(preprocess)
  checkarg_isboolean(quiet)

  if (!is.null(name)) {
    checkarg_isname(name)
    warning(
      "`name` is deprecated; use `variable_name` instead.",
      call. = FALSE
    )
    if (identical(name, "easy_name")) {
      warning(
        "`easy_name` is deprecated; use `semantic_name` instead.",
        call. = FALSE
      )
    }
    variable_name <- ifelse(name == "easy_name", "semantic_name", name)
  } else {
    variable_name <- match.arg(variable_name)
    checkarg_isvariable_name(variable_name)
  }

  if (!is.null(preprocess)) {
    warning(
      "`preprocess` is deprecated; use `semantic_name_preprocess` instead.",
      call. = FALSE
    )
    if (is.null(semantic_name_preprocess)) {
      semantic_name_preprocess <- preprocess
    }
  }

  use_semantic_name <- variable_name == "semantic_name"

  survey_metadata <- fetch_dictionary_metadata(surveyID)
  normalised_metadata <- normalise_qualtrics_metadata(survey_metadata)

  dict <- variable_dictionary_from_normalised_metadata(
    normalised_metadata,
    use_semantic_name = use_semantic_name,
    block_pattern = block_pattern,
    block_sep = block_sep,
    semantic_name_preprocess = semantic_name_preprocess,
    quiet = quiet
  )

  dict_columns <- c(
    "response_column_id", "qid", "question_name", "variable_name",
    "block", "question",
    "item", "level", "label", "type", "selector", "sub_selector", "content_type"
  )
  if ("loop_option" %in% names(dict) && any(!is.na(dict$loop_option))) {
    dict_columns <- c(
      "response_column_id", "qid", "question_name", "variable_name",
      "block", "question",
      "loop_option", "item", "level", "label", "type", "selector",
      "sub_selector", "content_type"
    )
  }
  if (use_semantic_name) {
    dict_columns <- append(dict_columns, "semantic_name", after = 4)
  }
  variable_name_findings <- attr(dict, "variable_name_findings", exact = TRUE)
  if (is.null(variable_name_findings)) {
    variable_name_findings <- empty_variable_name_findings()
  }

  dict <- dict[dict_columns]
  attr(dict, "variable_name_findings") <- variable_name_findings

  attr(dict, "class") <- c("qualtdict", class(dict))

  dict
}
