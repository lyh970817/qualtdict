new_normalised_metadata <- function(
  survey_id,
  survey_name,
  survey_question_facts,
  embedded_data_fields,
  scoring_variables,
  text_analysis_sidecars
) {
  structure(
    list(
      surveyID = survey_id,
      survey_name = survey_name,
      questions = survey_question_facts,
      embedded_data = embedded_data_fields,
      scoring = scoring_variables,
      text_analysis = text_analysis_sidecars
    ),
    class = c("qualtdict_normalised_metadata", "list")
  )
}

#' Normalise raw Qualtrics metadata
#'
#' The normalised metadata model is internal. It normalises raw Qualtrics
#' metadata into package-owned Normalised Question Facts plus
#' Metadata-defined Export Variables and assembles
#' `qualtdict_normalised_metadata`.
#'
#' @keywords internal
#' @noRd
normalise_qualtrics_metadata <- function(raw_metadata) {
  survey_question_facts <- normalise_qualtrics_questions(
    raw_metadata$metadata,
    raw_metadata$description
  )
  embedded_data_fields <- normalise_embedded_data_fields(
    raw_metadata$metadata,
    raw_metadata$description
  )
  response_column_map <- raw_metadata$response_column_map
  embedded_data_fields <- filter_exported_embedded_data_fields(
    embedded_data_fields,
    response_column_map
  )
  scoring_variables <- normalise_scoring_variables(
    raw_metadata$description,
    response_column_map = response_column_map
  )
  response_column_classification <- classify_response_column_map(
    response_column_map,
    questions = survey_question_facts,
    embedded_data = embedded_data_fields,
    scoring = scoring_variables
  )
  text_analysis_sidecars <- normalise_text_analysis_sidecars(
    survey_question_facts,
    response_column_classification = response_column_classification
  )

  new_normalised_metadata(
    survey_id = raw_metadata$surveyID,
    survey_name = raw_metadata$survey_name,
    survey_question_facts = survey_question_facts,
    embedded_data_fields = embedded_data_fields,
    scoring_variables = scoring_variables,
    text_analysis_sidecars = text_analysis_sidecars
  )
}
