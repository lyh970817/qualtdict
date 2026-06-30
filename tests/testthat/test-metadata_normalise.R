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
