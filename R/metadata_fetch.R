#' Fetch raw Qualtrics metadata for Variable Dictionary generation
#'
#' This keeps the Qualtrics API calls separate from the package-owned metadata
#' model so synthetic fixtures can exercise dictionary generation without live
#' API access.
#'
#' @keywords internal
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
      "embedded_data",
      "comments"
    )
  )

  mt_d <- fetch_description2(
    surveyID,
    c(
      "questions",
      "metadata",
      "blocks",
      "flow"
    )
  )

  new_raw_qualtrics_metadata(surveyID, mt, mt_d)
}

#' Build a raw Qualtrics metadata bundle
#'
#' Tests can construct this object from synthetic Qualtrics-shaped lists and
#' feed it into `normalise_qualtrics_metadata()`.
#'
#' @keywords internal
#' @noRd
new_raw_qualtrics_metadata <- function(surveyID, metadata, description) {
  structure(
    list(
      surveyID = surveyID,
      survey_name = as.character(metadata$metadata$name),
      metadata = metadata,
      description = description
    ),
    class = c("qualtdict_raw_metadata", "list")
  )
}
