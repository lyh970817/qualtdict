to_dataframe <- function(json) {
  map_df(json, map_df, unlist)
}

variable_dictionary_base_columns <- c(
  "qid",
  "response_column_id",
  "row_source",
  "question_name",
  "variable_name",
  "block",
  "question",
  "looping_question",
  "item",
  "level",
  "label",
  "type",
  "selector",
  "content_type",
  "sub_selector",
  "looping_option",
  "looping",
  "loop_option"
)

variable_dictionary_semantic_columns <- append(
  variable_dictionary_base_columns,
  "semantic_name",
  after = match("question_name", variable_dictionary_base_columns)
)

#' Generate dictionary rows from normalised metadata
#'
#' This renders the current Variable Dictionary schema plus tracer-bullet
#' identity columns. The internal normalised metadata model is allowed to
#' evolve; this function is the adapter that keeps `dict_generate()` small.
#'
#' @keywords internal
#' @noRd
variable_dictionary_from_normalised_metadata <- function(
  normalised_metadata,
  use_semantic_name,
  block_pattern,
  block_sep,
  semantic_name_preprocess,
  quiet = TRUE
) {
  question_meta <- normalised_metadata$questions
  if (length(question_meta) > 0) {
    question_meta <- expand_loop_question_facts(question_meta)
  }

  json <- c(
    variable_dictionary_question_rows(question_meta),
    variable_dictionary_embedded_data_rows(normalised_metadata$embedded_data)
  )
  if (length(json) == 0) {
    return(empty_variable_dictionary_from_normalised_metadata(
      normalised_metadata,
      use_semantic_name = use_semantic_name
    ))
  }

  json <- prepare_variable_dictionary_rows(json)

  if (use_semantic_name) {
    json <- generate_semantic_names(
      json,
      normalised_metadata$surveyID,
      block_pattern,
      block_sep,
      semantic_name_preprocess,
      quiet = quiet
    )
  }

  finalise_variable_dictionary_rows(
    json,
    normalised_metadata,
    use_semantic_name = use_semantic_name
  )
}

variable_dictionary_question_rows <- function(question_meta) {
  imap(question_meta, variable_dictionary_question_row) |>
    discard(is.null)
}

variable_dictionary_question_row <- function(qjson, qid) {
  question_type <- question_fact_question_type(qjson)
  question_name <- question_fact_question_name(qjson)
  response_column_qid <- qjson$response_column_qid %||% qid
  response_columns <- render_response_columns(qjson, response_column_qid)
  if (nrow(response_columns) == 0) {
    return(NULL)
  }

  question_name <- rep(
    question_name,
    length(response_columns$response_column_id)
  )

  list(
    qid = rep(qjson$qid %||% qid, length(response_columns$response_column_id)),
    response_column_id = response_columns$response_column_id,
    row_source = rep("question", length(response_columns$response_column_id)),
    question_name = null_na(question_name),
    block = question_fact_survey_block(qjson),
    question = response_columns$question,
    looping_question = qjson$looping_question %||% NA_character_,
    item = response_columns$item,
    level = response_columns$level,
    label = response_columns$label,
    type = question_type$type,
    selector = question_type$selector,
    content_type = qjson$content_type,
    sub_selector = null_na(question_type$sub_selector),
    looping_option = qjson$looping_option %||% NA_character_,
    looping = isTRUE(qjson$looping)
  )
}

variable_dictionary_embedded_data_rows <- function(embedded_data) {
  imap(embedded_data %||% list(), variable_dictionary_embedded_data_row)
}

variable_dictionary_embedded_data_row <- function(field, field_name) {
  field_name <- field$field_name %||% field_name

  list(
    qid = NA_character_,
    response_column_id = field$response_column_id %||% field_name,
    row_source = "embedded_data",
    question_name = NA_character_,
    variable_name = field_name,
    block = NA_character_,
    question = field$question_text %||% paste("Embedded Data:", field_name),
    looping_question = NA_character_,
    item = NA_character_,
    level = NA_character_,
    label = NA_character_,
    type = NA_character_,
    selector = NA_character_,
    content_type = NA_character_,
    sub_selector = NA_character_,
    looping_option = NA_character_,
    looping = FALSE
  )
}

prepare_variable_dictionary_rows <- function(json) {
  json <- json |>
    to_dataframe() |>
    convert_html()

  json$looping <- as.logical(json$looping)
  json
}

finalise_variable_dictionary_rows <- function(
  json,
  normalised_metadata,
  use_semantic_name
) {
  json <- clean_variable_dictionary_rows(json, use_semantic_name)
  attr(json, "survey_name") <- normalised_metadata$survey_name
  attr(json, "surveyID") <- normalised_metadata$surveyID

  repair_variable_dictionary_names(json)
}

clean_variable_dictionary_rows <- function(json, use_semantic_name) {
  if (!"variable_name" %in% names(json)) {
    json$variable_name <- NA_character_
  }
  json$item[json$item == json$question] <- NA
  json$qid <- unname(json$qid)
  json$response_column_id <- unname(json$response_column_id)
  json$question_name <- unname(json$question_name)
  if (use_semantic_name && !"semantic_name" %in% names(json)) {
    json$semantic_name <- NA_character_
  }
  looping_questions <- json$looping_question
  json$question[!is.na(looping_questions)] <-
    looping_questions[!is.na(looping_questions)]
  if (!use_semantic_name) {
    question_rows <- dict_question_rows(json)
    json$variable_name[question_rows] <- json$question_name[question_rows]
  }
  json$variable_name <- unname(json$variable_name)
  json$loop_option <- json$looping_option

  json
}

#' Empty Variable Dictionary from normalised metadata
#' @keywords internal
#' @noRd
empty_variable_dictionary_from_normalised_metadata <- function(
  normalised_metadata,
  use_semantic_name = FALSE
) {
  columns <- variable_dictionary_base_columns
  values <- rep(list(character()), length(columns))
  names(values) <- columns
  values$looping <- logical()
  json <- as_tibble(values)

  if (use_semantic_name) {
    json$semantic_name <- character()
    json <- json[variable_dictionary_semantic_columns]
  }

  attr(json, "survey_name") <- normalised_metadata$survey_name
  attr(json, "surveyID") <- normalised_metadata$surveyID
  attr(json, "variable_name_findings") <- empty_variable_name_findings()

  json
}
