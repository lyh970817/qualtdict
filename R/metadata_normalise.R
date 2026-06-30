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

new_normalised_text_analysis_sidecars <- function(sidecars = list()) {
  structure(
    sidecars,
    class = c("qualtdict_normalised_text_analysis_sidecars", "list")
  )
}

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

#' Normalise raw Qualtrics metadata
#'
#' The normalised metadata model is internal and question-level for this tracer
#' bullet. It owns the merged question, block, loop, and content-type metadata
#' that dictionary row generation consumes.
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

response_column_map_ids <- function(response_column_map) {
  ids <- response_column_map_row_ids(response_column_map)
  ids[!is.na(ids) & nzchar(ids)]
}

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

empty_text_analysis_sidecar_parent_context <- function() {
  list(
    parent_qid = NA_character_,
    parent_question_name = NA_character_,
    parent_block = NA_character_
  )
}

empty_normalised_text_analysis_sidecars <- function() {
  new_normalised_text_analysis_sidecars()
}

#' Merge raw question, block, and content-type metadata
#' @keywords internal
#' @noRd
normalise_qualtrics_questions <- function(mt, mt_d) {
  question_meta <- question_metadata(mt)
  qids <- names(question_meta)

  block_meta <- normalise_question_block_metadata(mt, mt_d, qids)
  content_type_meta <- normalise_question_content_types(mt_d, qids)

  question_meta <- question_meta[qids] |>
    order_name()

  question_meta <- imap(question_meta, function(question, qid) {
    normalise_question_fact(
      qid = qid,
      question = question,
      block = block_meta[[qid]] %||% default_question_block_metadata(),
      content_type = content_type_meta[[qid]]
    )
  })

  structure(question_meta, class = c("qualtdict_normalised_questions", "list"))
}

normalise_question_block_metadata <- function(mt, mt_d, qids) {
  block_meta <- block_metadata(mt, mt_d) |>
    map(function(block) {
      map(
        block$qid,
        ~ list(
          qid = .x,
          description = block$description,
          looping_prefix = block$looping_prefix,
          looping_qid = block$looping_qid,
          looping_static = block$looping_static,
          looping_column_names = block$looping_column_names
        )
      )
    }) |>
    # Use 'c' to combine multiple lists into one list
    # Previously the lists are nested in block and then QID
    do.call(c, args = _)

  block_meta <- setNames(block_meta, map_chr(block_meta, ~ .x$qid))

  block_meta[qids] |>
    order_name()
}

block_metadata <- function(mt, mt_d) {
  imap(mt_d[["blocks"]], function(block, block_id) {
    looping_options <- block$Options$LoopingOptions
    list(
      description = block$Description,
      qid = unlist(map(block$BlockElements, "QuestionID")),
      looping_prefix = names(looping_options$Static),
      looping_qid = looping_options$QID,
      looping_static = looping_options$Static,
      looping_column_names = mt$loopAndMerge[[block_id]]$columnNames
    )
  })
}

question_metadata <- function(mt) {
  map(
    mt$questions,
    `[`,
    c(
      "questionName",
      "questionType",
      "questionText",
      "blocks",
      "columns",
      "choices",
      "subQuestions"
    )
  )
}

normalise_question_content_types <- function(mt_d, qids) {
  content_type_meta <- mt_d[["questions"]] |>
    map("Validation") |>
    map("Settings") |>
    map("ContentType") |>
    map(null_na) |>
    map(str_remove, "Valid")

  content_type_meta[qids] |>
    order_name()
}

default_question_block_metadata <- function() {
  list(
    description = NA_character_,
    looping_prefix = character(),
    looping_qid = NA_character_,
    looping_static = NULL,
    looping_column_names = NULL
  )
}
