to_dataframe <- function(json) {
  map_df(json, function(qmeta) {
    map_df(qmeta, unlist)
  })
}

#' Generate dictionary rows from normalised metadata
#'
#' This renders the current Variable Dictionary schema plus tracer-bullet
#' identity columns. The internal normalised metadata model is allowed to
#' evolve; this function is the adapter that keeps `dict_generate()` small.
#'
#' @keywords internal
#' @noRd
variable_dictionary_from_normalised_metadata <- function(normalised_metadata,
                                                         use_semantic_name,
                                                         block_pattern,
                                                         block_sep,
                                                         semantic_name_preprocess,
                                                         quiet = TRUE) {
  question_meta <- normalised_metadata$questions
  question_meta <- expand_loop_question_facts(question_meta)
  if (length(question_meta) == 0) {
    return(empty_variable_dictionary_from_normalised_metadata(
      normalised_metadata,
      use_semantic_name = use_semantic_name
    ))
  }

  json <- imap(question_meta, function(qjson, qid) {
    question_type <- question_fact_question_type(qjson)
    question_name <- question_fact_question_name(qjson)
    type <- question_type$type
    selector <- question_type$selector
    block <- question_fact_survey_block(qjson)
    content_type <- qjson$content_type
    sub_selector <- question_type$sub_selector
    qid_base <- qjson$qid %||% qid
    response_column_qid <- qjson$response_column_qid %||% qid
    response_columns <- render_response_columns(qjson, response_column_qid)
    looping_question <- qjson$looping_question %||% NA_character_
    looping_option <- qjson$looping_option %||% NA_character_
    looping <- isTRUE(qjson$looping)

    question_name <- rep(
      question_name,
      length(response_columns$response_column_id)
    )

    list(
      qid = rep(qid_base, length(response_columns$response_column_id)),
      response_column_id = response_columns$response_column_id,
      row_source = rep("question", length(response_columns$response_column_id)),
      question_name = null_na(question_name),
      block = block,
      question = response_columns$question,
      looping_question = looping_question,
      item = response_columns$item,
      level = response_columns$level,
      label = response_columns$label,
      type = type,
      selector = selector,
      content_type = content_type,
      sub_selector = null_na(sub_selector),
      looping_option = looping_option,
      looping = looping
    )
  }) %>%
    discard(is.null) %>%
    to_dataframe() %>%
    convert_html()

  json$looping <- as.logical(json$looping)

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

  # Remove duplicated question text in item before Semantic Name generation.
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
    json$variable_name <- json$question_name
  }
  json$variable_name <- unname(json$variable_name)
  json$loop_option <- json$looping_option

  attr(json, "survey_name") <- normalised_metadata$survey_name
  attr(json, "surveyID") <- normalised_metadata$surveyID
  json <- repair_variable_dictionary_names(json)

  json
}

#' Empty Variable Dictionary from normalised metadata
#' @keywords internal
#' @noRd
empty_variable_dictionary_from_normalised_metadata <- function(
    normalised_metadata,
    use_semantic_name = FALSE) {
  json <- tibble(
    qid = character(),
    response_column_id = character(),
    row_source = character(),
    question_name = character(),
    variable_name = character(),
    block = character(),
    question = character(),
    looping_question = character(),
    item = character(),
    level = character(),
    label = character(),
    type = character(),
    selector = character(),
    content_type = character(),
    sub_selector = character(),
    looping_option = character(),
    looping = logical(),
    loop_option = character()
  )
  if (use_semantic_name) {
    json <- json[c(
      "qid", "response_column_id", "row_source", "question_name",
      "variable_name", "block", "question", "looping_question", "item",
      "level", "label", "type", "selector", "content_type", "sub_selector",
      "looping_option", "looping", "loop_option"
    )]
    json$semantic_name <- character()
    json <- json[c(
      "qid", "response_column_id", "row_source", "question_name",
      "semantic_name", "variable_name", "block", "question",
      "looping_question", "item", "level", "label", "type", "selector",
      "content_type", "sub_selector", "looping_option", "looping",
      "loop_option"
    )]
  }

  attr(json, "survey_name") <- normalised_metadata$survey_name
  attr(json, "surveyID") <- normalised_metadata$surveyID
  attr(json, "variable_name_findings") <- empty_variable_name_findings()

  json
}
