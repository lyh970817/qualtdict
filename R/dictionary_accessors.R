dict_response_column_id <- function(dict) {
  if ("response_column_id" %in% names(dict)) {
    return(dict[["response_column_id"]])
  }

  dict[["qid"]]
}

dict_variable_name <- function(dict) {
  if ("variable_name" %in% names(dict)) {
    return(dict[["variable_name"]])
  }

  dict[["name"]]
}

dict_row_source <- function(dict) {
  if ("row_source" %in% names(dict)) {
    return(dict[["row_source"]])
  }

  rep("question", nrow(dict))
}

dict_question_rows <- function(dict) {
  row_source <- dict_row_source(dict)
  !is.na(row_source) & row_source == "question"
}

dict_metadata_defined_rows <- function(dict) {
  row_source <- dict_row_source(dict)
  !is.na(row_source) & row_source != "question"
}
