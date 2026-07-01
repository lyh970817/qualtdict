#' Build normalised Scoring Variable records
#' @noRd
new_normalised_scoring_variables <- function(variables = list()) {
  structure(
    variables,
    class = c("qualtdict_normalised_scoring_variables", "list")
  )
}

#' Build one normalised Scoring Variable record
#' @noRd
new_normalised_scoring_variable <- function(
  output_name,
  response_column_id,
  question_text = paste("Scoring Variable:", output_name)
) {
  structure(
    list(
      output_name = output_name,
      response_column_id = response_column_id,
      question_text = question_text
    ),
    class = c("qualtdict_normalised_scoring_variable", "list")
  )
}

#' Normalise Scoring Variables from Qualtrics metadata
#' @noRd
normalise_scoring_variables <- function(mt_d, response_column_map = NULL) {
  categories <- scoring_categories(mt_d$scoring)
  if (length(categories) == 0) {
    return(empty_normalised_scoring_variables())
  }

  variables <- map(categories, normalise_scoring_variable) |>
    discard(is.null)
  names(variables) <- map_chr(variables, "output_name")
  variables <- filter_exported_scoring_variables(
    variables,
    response_column_map
  )

  new_normalised_scoring_variables(variables)
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

#' Empty normalised Scoring Variable records
#' @noRd
empty_normalised_scoring_variables <- function() {
  new_normalised_scoring_variables()
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
  output_name <- scoring_category_name(category)
  response_column_id <- scoring_category_response_column_id(category)
  if (
    is.na(output_name) ||
      !nzchar(output_name) ||
      is.na(response_column_id) ||
      !nzchar(response_column_id)
  ) {
    return(NULL)
  }

  new_normalised_scoring_variable(
    output_name = output_name,
    response_column_id = response_column_id
  )
}

#' Resolve a Scoring Variable name
#' @noRd
scoring_category_name <- function(category) {
  if (is.null(category) || !is.list(category)) {
    return(NA_character_)
  }

  scalar_character(category$Name)
}

#' Resolve a Scoring Variable Response Column ID
#' @noRd
scoring_category_response_column_id <- function(category) {
  if (is.null(category) || !is.list(category)) {
    return(NA_character_)
  }

  scalar_character(category$ID)
}
