#' Fetch raw Qualtrics metadata for Variable Dictionary generation
#'
#' This keeps the Qualtrics API calls separate from the package-owned metadata
#' model so synthetic fixtures can exercise dictionary generation without live
#' API access.
#'
#' @noRd
fetch_dictionary_metadata <- function(surveyID) {
  mt <- metadata2(
    surveyID,
    c(
      "questions",
      "metadata",
      "blocks",
      "responsecounts",
      "flow",
      "embedded_data"
    )
  )

  mt_d <- fetch_description2(
    surveyID,
    c(
      "questions",
      "metadata",
      "blocks",
      "flow",
      "scoring"
    )
  )
  response_column_map <- tryCatch(
    fetch_response_column_map(surveyID),
    error = function(error) {
      warning(
        "Failed to fetch the Qualtrics response column map for `",
        surveyID,
        "`. Dictionaries will still be generated from survey metadata, ",
        "but scoring and text-analysis sidecar filtering may be incomplete. ",
        conditionMessage(error),
        call. = FALSE
      )
      NULL
    }
  )

  new_raw_qualtrics_metadata(surveyID, mt, mt_d, response_column_map)
}

#' Fetch Qualtrics data for Response Column Map Classification
#' @noRd
fetch_response_column_map <- function(surveyID) {
  response_schema <- fetch_survey2(
    surveyID = surveyID,
    limit = 1,
    import_id = TRUE,
    convert = FALSE,
    label = FALSE,
    breakout_sets = TRUE,
    verbose = FALSE
  )

  attr(response_schema, "column_map", exact = TRUE)
}

#' Build a raw Qualtrics metadata bundle
#'
#' Tests can construct this object from synthetic Qualtrics-shaped lists and
#' feed it into `normalise_qualtrics_metadata()`.
#'
#' @noRd
new_raw_qualtrics_metadata <- function(
  surveyID,
  metadata,
  description,
  response_column_map = NULL
) {
  structure(
    list(
      surveyID = surveyID,
      survey_name = as.character(metadata$metadata$name),
      metadata = metadata,
      description = description,
      response_column_map = response_column_map
    ),
    class = c("qualtdict_raw_metadata", "list")
  )
}
