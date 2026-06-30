metadata_normalise_summary <- function(normalised_metadata) {
  list(
    surveyID = normalised_metadata$surveyID,
    survey_name = normalised_metadata$survey_name,
    questions = metadata_question_summary(normalised_metadata$questions),
    embedded_data = metadata_export_variable_summary(
      normalised_metadata$embedded_data
    ),
    scoring = metadata_export_variable_summary(normalised_metadata$scoring),
    text_analysis = metadata_text_analysis_summary(
      normalised_metadata$text_analysis
    )
  )
}

metadata_question_summary <- function(survey_question_facts) {
  lapply(survey_question_facts, function(question_fact) {
    list(
      qid = question_fact$qid,
      question_name = question_fact$question_name,
      survey_block = question_fact$survey_block,
      question_text = question_fact$question_text,
      type = question_fact$question_type$type,
      selector = question_fact$question_type$selector,
      sub_selector = question_fact$question_type$sub_selector,
      content_type = question_fact$content_type,
      choice_names = names(question_fact$response_choices),
      item_names = names(question_fact$response_items),
      column_fact_count = length(question_fact$column_facts),
      looping_qid = question_fact$looping_qid,
      looping_prefix = question_fact$looping_prefix
    )
  })
}

metadata_export_variable_summary <- function(records) {
  lapply(records, function(record) {
    record[intersect(
      names(record),
      c(
        "field_name",
        "output_name",
        "response_column_id",
        "question_text",
        "previous_block",
        "next_block"
      )
    )]
  })
}

metadata_text_analysis_summary <- function(sidecars) {
  lapply(sidecars, function(sidecar) {
    sidecar[c(
      "sidecar_name",
      "response_column_id",
      "question_text",
      "parent_qid",
      "parent_question_name",
      "parent_block"
    )]
  })
}

metadata_dictionary_summary <- function(dict) {
  dict[,
    c(
      "response_column_id",
      "row_source",
      "qid",
      "question_name",
      "variable_name",
      "block",
      "question",
      "item",
      "level",
      "label",
      "type",
      "selector",
      "sub_selector",
      "content_type"
    )
  ]
}
