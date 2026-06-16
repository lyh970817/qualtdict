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
