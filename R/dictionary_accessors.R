#' Return Response Column IDs from a Variable Dictionary
#' @noRd
dict_response_column_id <- function(dict) {
  dict[["response_column_id"]]
}

#' Return Dictionary Variable Names from a Variable Dictionary
#' @noRd
dict_variable_name <- function(dict) {
  dict[["variable_name"]]
}

#' Return Dictionary Row Sources from a Variable Dictionary
#' @noRd
dict_row_source <- function(dict) {
  if ("row_source" %in% names(dict)) {
    return(dict[["row_source"]])
  }

  rep("question", nrow(dict))
}

#' Return row positions for question-backed Variable Dictionary rows
#' @noRd
dict_question_rows <- function(dict) {
  row_source <- dict_row_source(dict)
  !is.na(row_source) & row_source == "question"
}

#' Return row positions for metadata-defined Variable Dictionary rows
#' @noRd
dict_metadata_defined_rows <- function(dict) {
  row_source <- dict_row_source(dict)
  !is.na(row_source) & row_source != "question"
}
