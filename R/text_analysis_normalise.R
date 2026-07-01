#' Build normalised Text-analysis Sidecar records
#' @noRd
new_normalised_text_analysis_sidecars <- function(sidecars = list()) {
  structure(
    sidecars,
    class = c("qualtdict_normalised_text_analysis_sidecars", "list")
  )
}

#' Build one normalised Text-analysis Sidecar record
#' @noRd
new_normalised_text_analysis_sidecar <- function(
  sidecar_name,
  response_column_id,
  parent_context
) {
  structure(
    list(
      sidecar_name = sidecar_name,
      response_column_id = response_column_id,
      question_text = paste("Text Analysis:", sidecar_name),
      parent_qid = parent_context$parent_qid,
      parent_question_name = parent_context$parent_question_name,
      parent_block = parent_context$parent_block
    ),
    class = c("qualtdict_normalised_text_analysis_sidecar", "list")
  )
}

#' Normalise Text-analysis Sidecars from Response Column Map Classification
#' rows
#' @noRd
normalise_text_analysis_sidecars <- function(
  questions,
  response_column_classification = NULL
) {
  sidecar_records <- text_analysis_sidecars_from_response_column_map(
    response_column_classification
  )
  if (length(sidecar_records) == 0) {
    return(empty_normalised_text_analysis_sidecars())
  }

  sidecars <- map(
    sidecar_records,
    normalise_text_analysis_sidecar,
    questions
  ) |>
    discard(is.null)

  names(sidecars) <- map_chr(sidecars, "sidecar_name")

  new_normalised_text_analysis_sidecars(sidecars)
}

#' Extract Text-analysis Sidecar facts from Response Column Map Classification
#' rows
#' @noRd
text_analysis_sidecars_from_response_column_map <- function(
  response_column_classification
) {
  if (is.null(response_column_classification)) {
    return(list())
  }
  if (nrow(response_column_classification) == 0) {
    return(list())
  }

  sidecar_rows <- response_column_classification$row_source == "text_analysis"
  if (!any(sidecar_rows, na.rm = TRUE)) {
    return(list())
  }

  sidecars <- response_column_classification[sidecar_rows, , drop = FALSE]
  map(
    seq_len(nrow(sidecars)),
    function(row_index) {
      sidecar <- sidecars[row_index, , drop = FALSE]
      list(
        sidecar_name = sidecar$display_name,
        response_column_id = sidecar$response_column_id,
        parent_qid = sidecar$parent_qid,
        main = sidecar$main,
        sub = sidecar$sub,
        description = sidecar$description
      )
    }
  )
}

#' Normalise one Text-analysis Sidecar
#' @noRd
normalise_text_analysis_sidecar <- function(
  sidecar,
  questions
) {
  sidecar_name <- scalar_character(sidecar$sidecar_name)
  response_column_id <- scalar_character(sidecar$response_column_id)
  if (
    is.na(sidecar_name) ||
      !nzchar(sidecar_name) ||
      is.na(response_column_id) ||
      !nzchar(response_column_id)
  ) {
    return(NULL)
  }

  parent_context <- text_analysis_sidecar_parent_context(
    scalar_character(sidecar$parent_qid),
    questions
  )

  new_normalised_text_analysis_sidecar(
    sidecar_name = sidecar_name,
    response_column_id = response_column_id,
    parent_context = parent_context
  )
}

#' Resolve parent question context for a Text-analysis Sidecar
#' @noRd
text_analysis_sidecar_parent_context <- function(parent_qid, questions) {
  if (is.na(parent_qid)) {
    return(empty_text_analysis_sidecar_parent_context())
  }

  parent_question <- questions[[parent_qid]]
  if (is.null(parent_question)) {
    return(empty_text_analysis_sidecar_parent_context())
  }

  list(
    parent_qid = parent_qid,
    parent_question_name = parent_question$question_name,
    parent_block = parent_question$survey_block
  )
}

#' Empty parent question context for a Text-analysis Sidecar
#' @noRd
empty_text_analysis_sidecar_parent_context <- function() {
  list(
    parent_qid = NA_character_,
    parent_question_name = NA_character_,
    parent_block = NA_character_
  )
}

#' Empty normalised Text-analysis Sidecar records
#' @noRd
empty_normalised_text_analysis_sidecars <- function() {
  new_normalised_text_analysis_sidecars()
}
