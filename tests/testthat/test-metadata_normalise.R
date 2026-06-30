test_that("metadata normalisation summary stays stable", {
  raw_metadata <- synthetic_column_map_sidecar_raw_metadata()
  normalised_metadata <- normalise_qualtrics_metadata(raw_metadata)

  expect_snapshot_value(
    metadata_normalise_summary(normalised_metadata),
    style = "json2"
  )
})

test_that("metadata normalisation preserves representative dictionary rows", {
  raw_metadata <- synthetic_loop_and_merge_raw_metadata()
  normalised_metadata <- normalise_qualtrics_metadata(raw_metadata)

  dict <- variable_dictionary_from_normalised_metadata(
    normalised_metadata,
    use_semantic_name = FALSE,
    block_pattern = NULL,
    block_sep = ".",
    semantic_name_preprocess = NULL
  )

  expect_snapshot_value(metadata_dictionary_summary(dict), style = "json2")
})

test_that("normalised metadata constructor preserves current object shape", {
  raw_metadata <- synthetic_mc_text_raw_metadata()
  survey_question_facts <- normalise_qualtrics_questions(
    raw_metadata$metadata,
    raw_metadata$description
  )

  normalised_metadata <- new_normalised_metadata(
    survey_id = "SV_TEST",
    survey_name = "Constructor Survey",
    survey_question_facts = survey_question_facts,
    embedded_data_fields = empty_normalised_embedded_data_fields(),
    scoring_variables = empty_normalised_scoring_variables(),
    text_analysis_sidecars = empty_normalised_text_analysis_sidecars()
  )

  expect_s3_class(normalised_metadata, "qualtdict_normalised_metadata")
  expect_named(
    normalised_metadata,
    c(
      "surveyID",
      "survey_name",
      "questions",
      "embedded_data",
      "scoring",
      "text_analysis"
    )
  )
  expect_identical(normalised_metadata$surveyID, "SV_TEST")
  expect_identical(normalised_metadata$survey_name, "Constructor Survey")
  expect_identical(normalised_metadata$questions, survey_question_facts)
})

test_that("normalised capability constructors preserve current object shapes", {
  embedded_data_field <- new_normalised_embedded_data_field("Source Channel")
  flow_embedded_data_field <- new_normalised_embedded_data_field(
    "Source Channel",
    previous_block = NA_character_,
    next_block = NA_character_
  )
  embedded_data_fields <- new_normalised_embedded_data_fields(
    list("Source Channel" = embedded_data_field)
  )
  scoring_variable <- new_normalised_scoring_variable(
    "SC_TOTAL",
    "SC_TOTAL"
  )
  scoring_variables <- new_normalised_scoring_variables(
    list(SC_TOTAL = scoring_variable)
  )
  sidecar <- new_normalised_text_analysis_sidecar(
    "Q1 Other - Sentiment",
    "QID1_3_TEXT_SENTIMENT",
    list(
      parent_qid = "QID1",
      parent_question_name = "Q1",
      parent_block = "Main Block"
    )
  )
  text_analysis_sidecars <- new_normalised_text_analysis_sidecars(
    list("Q1 Other - Sentiment" = sidecar)
  )
  empty_context_sidecar <- new_normalised_text_analysis_sidecar(
    "Q1 Other - Sentiment",
    "QID1_3_TEXT_SENTIMENT",
    empty_text_analysis_sidecar_parent_context()
  )
  classification <- new_response_column_map_classification()

  expect_s3_class(
    embedded_data_field,
    "qualtdict_normalised_embedded_data_field"
  )
  expect_named(
    embedded_data_field,
    c("field_name", "response_column_id", "question_text")
  )
  expect_identical(
    embedded_data_field$response_column_id,
    "Source Channel"
  )
  expect_identical(
    embedded_data_field$question_text,
    "Embedded Data: Source Channel"
  )
  expect_named(
    flow_embedded_data_field,
    c(
      "field_name",
      "response_column_id",
      "question_text",
      "previous_block",
      "next_block"
    )
  )
  expect_true(
    all(is.na(flow_embedded_data_field[c("previous_block", "next_block")]))
  )

  expect_s3_class(
    embedded_data_fields,
    "qualtdict_normalised_embedded_data_fields"
  )
  expect_named(embedded_data_fields, "Source Channel")
  expect_identical(embedded_data_fields[[1]], embedded_data_field)

  expect_s3_class(
    scoring_variable,
    "qualtdict_normalised_scoring_variable"
  )
  expect_named(
    scoring_variable,
    c("output_name", "response_column_id", "question_text")
  )
  expect_identical(scoring_variable$output_name, "SC_TOTAL")
  expect_identical(scoring_variable$question_text, "Scoring Variable: SC_TOTAL")

  expect_s3_class(
    scoring_variables,
    "qualtdict_normalised_scoring_variables"
  )
  expect_named(scoring_variables, "SC_TOTAL")
  expect_identical(scoring_variables[[1]], scoring_variable)

  expect_s3_class(
    sidecar,
    "qualtdict_normalised_text_analysis_sidecar"
  )
  expect_named(
    sidecar,
    c(
      "sidecar_name",
      "response_column_id",
      "question_text",
      "parent_qid",
      "parent_question_name",
      "parent_block"
    )
  )
  expect_identical(sidecar$parent_qid, "QID1")
  expect_identical(sidecar$parent_question_name, "Q1")
  expect_identical(sidecar$parent_block, "Main Block")
  expect_true(
    all(
      is.na(
        empty_context_sidecar[c(
          "parent_qid",
          "parent_question_name",
          "parent_block"
        )]
      )
    )
  )

  expect_s3_class(
    text_analysis_sidecars,
    "qualtdict_normalised_text_analysis_sidecars"
  )
  expect_named(text_analysis_sidecars, "Q1 Other - Sentiment")
  expect_identical(text_analysis_sidecars[[1]], sidecar)

  expect_s3_class(classification, "tbl_df")
  expect_named(
    classification,
    c(
      "response_column_id",
      "row_source",
      "parent_qid",
      "display_name",
      "main",
      "sub",
      "description",
      "reason"
    )
  )
  expect_length(classification, 8)
  expect_identical(nrow(classification), 0L)

  rows <- data.frame(
    response_column_id = "QID1_3_TEXT_SENTIMENT",
    row_source = "text_analysis",
    parent_qid = "QID1",
    display_name = "Q1 Other - Sentiment",
    main = "Q1 Other",
    sub = "Sentiment",
    description = "Q1 Other - Sentiment",
    reason = "derived_question",
    stringsAsFactors = FALSE
  )
  expect_identical(new_response_column_map_classification(rows), rows)
})

test_that("raw Qualtrics metadata normalises into package-owned metadata", {
  raw_metadata <- synthetic_mc_text_raw_metadata()

  normalised_metadata <- normalise_qualtrics_metadata(raw_metadata)

  expect_s3_class(raw_metadata, "qualtdict_raw_metadata")
  expect_s3_class(normalised_metadata, "qualtdict_normalised_metadata")
  expect_named(
    normalised_metadata,
    c(
      "surveyID",
      "survey_name",
      "questions",
      "embedded_data",
      "scoring",
      "text_analysis"
    )
  )
  expect_identical(normalised_metadata$surveyID, "SV_SYNTHETIC")
  expect_identical(normalised_metadata$survey_name, "Synthetic Survey")
  expect_s3_class(
    normalised_metadata$questions,
    "qualtdict_normalised_questions"
  )
  expect_named(normalised_metadata$questions, "QID1")
  expect_s3_class(
    normalised_metadata$embedded_data,
    "qualtdict_normalised_embedded_data_fields"
  )
  expect_length(normalised_metadata$embedded_data, 0)
  expect_s3_class(
    normalised_metadata$scoring,
    "qualtdict_normalised_scoring_variables"
  )
  expect_length(normalised_metadata$scoring, 0)
  expect_s3_class(
    normalised_metadata$text_analysis,
    "qualtdict_normalised_text_analysis_sidecars"
  )
  expect_length(normalised_metadata$text_analysis, 0)
})
