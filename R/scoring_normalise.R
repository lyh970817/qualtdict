#' Build one normalised Scoring Variable record
#' @noRd
new_normalised_scoring_variable <- function(
  output_name,
  response_column_id,
  question_text = paste("Scoring Variable:", output_name)
) {
  list(
    output_name = output_name,
    response_column_id = response_column_id,
    question_text = question_text
  )
}

#' Normalise Scoring Variables from Qualtrics metadata
#' @noRd
normalise_scoring_variables <- function(mt_d, response_column_map = NULL) {
  categories <- scoring_categories(mt_d$scoring)
  if (length(categories) == 0) {
    return(list())
  }

  variables <- map(categories, normalise_scoring_variable) |>
    discard(is.null)
  names(variables) <- map_chr(variables, "output_name")

  filter_exported_scoring_variables(
    variables,
    response_column_map
  )
}

#' Return Qualtrics scoring categories
#' @noRd
scoring_categories <- function(scoring) {
  if (is.null(scoring) || !is.list(scoring)) {
    return(list())
  }

  categories <- scoring$ScoringCategories
  if (is.null(categories) || length(categories) == 0) {
    return(list())
  }

  categories
}

#' Keep Scoring Variables represented by exported Response Column IDs
#' @noRd
filter_exported_scoring_variables <- function(variables, response_column_map) {
  response_column_ids <- response_column_map_ids(response_column_map)
  if (length(response_column_ids) == 0) {
    return(variables)
  }

  keep <- map_lgl(variables, function(variable) {
    variable$response_column_id %in% response_column_ids
  })
  variables[keep]
}

#' Normalise one Qualtrics scoring category
#' @noRd
normalise_scoring_variable <- function(category) {
  if (is.null(category) || !is.list(category)) {
    return(NULL)
  }

  output_name <- scalar_character(category$Name)
  response_column_id <- scalar_character(category$ID)
  identifiers <- c(output_name, response_column_id)
  if (anyNA(identifiers) || !all(nzchar(identifiers))) {
    return(NULL)
  }

  new_normalised_scoring_variable(
    output_name = output_name,
    response_column_id = response_column_id
  )
}
