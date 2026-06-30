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

  question <- normalised_metadata$questions$QID1
  expect_s3_class(question, "qualtdict_normalised_question")
  expect_identical(question$survey_block, "Main Block")
  expect_identical(question$content_type, "Number")
  expect_null(question$looping_qid)
  expect_identical(question$question_name, "Q1")
  expect_identical(question$question_text, "Choose one")
  expect_identical(question$question_type$type, "MC")
  expect_identical(question$question_type$selector, "SAVR")
  expect_identical(question$question_type$sub_selector, "TX")
  expect_named(question$response_choices, c("1", "2", "3"))
  expect_length(question$response_items, 0)
  expect_length(question$column_facts, 0)
})

test_that("flat Embedded Data Fields normalise into package-owned metadata", {
  raw_metadata <- synthetic_flat_embedded_data_raw_metadata()

  normalised_metadata <- normalise_qualtrics_metadata(raw_metadata)
  embedded_data <- normalised_metadata$embedded_data

  expect_s3_class(
    embedded_data,
    "qualtdict_normalised_embedded_data_fields"
  )
  expect_named(embedded_data, c("Source Channel", "Q1"))
  expect_s3_class(
    embedded_data[["Source Channel"]],
    "qualtdict_normalised_embedded_data_field"
  )
  expect_identical(
    embedded_data[["Source Channel"]]$response_column_id,
    "Source Channel"
  )
  expect_identical(
    embedded_data[["Source Channel"]]$question_text,
    "Embedded Data: Source Channel"
  )
})

test_that("Text-analysis Sidecars normalise with parent question context", {
  raw_metadata <- synthetic_text_analysis_raw_metadata()

  text_analysis <- normalise_qualtrics_metadata(raw_metadata)$text_analysis

  expect_s3_class(
    text_analysis,
    "qualtdict_normalised_text_analysis_sidecars"
  )
  expect_named(
    text_analysis,
    c("Q1 Other - Sentiment", "Q1 Other - Parent Topics")
  )
  expect_s3_class(
    text_analysis[["Q1 Other - Sentiment"]],
    "qualtdict_normalised_text_analysis_sidecar"
  )
  expect_identical(
    text_analysis[["Q1 Other - Sentiment"]]$response_column_id,
    "QID1_3_TEXT_SENTIMENT"
  )
  expect_identical(text_analysis[["Q1 Other - Sentiment"]]$parent_qid, "QID1")
  expect_identical(
    text_analysis[["Q1 Other - Sentiment"]]$parent_question_name,
    "Q1"
  )
  expect_identical(
    text_analysis[["Q1 Other - Sentiment"]]$parent_block,
    "Main Block"
  )
  expect_identical(
    text_analysis[["Q1 Other - Parent Topics"]]$response_column_id,
    "QID1_3_e476cefa310845248231594eParTopics"
  )
  expect_identical(
    text_analysis[["Q1 Other - Parent Topics"]]$sidecar_name,
    "Q1 Other - Parent Topics"
  )
})

test_that("Text-analysis Sidecars normalise from response column maps", {
  raw_metadata <- synthetic_mc_text_raw_metadata()
  raw_metadata$response_column_map <- tibble::tibble(
    ImportId = c(
      "QID1_3_TEXT",
      "QID1_3_TEXT_SENTIMENT",
      "QID1_3_e476cefa310845248231594eParTopics"
    ),
    description = c(
      "Choose one - Other",
      "Q1 Other - Sentiment",
      "Q1 Other - Parent Topics"
    ),
    main = c("Choose one", "Q1 Other", "Q1 Other"),
    sub = c("", "Sentiment", "Parent Topics")
  )

  text_analysis <- normalise_qualtrics_metadata(raw_metadata)$text_analysis

  expect_named(
    text_analysis,
    c("Q1 Other - Sentiment", "Q1 Other - Parent Topics")
  )
  expect_identical(
    text_analysis[["Q1 Other - Sentiment"]]$response_column_id,
    "QID1_3_TEXT_SENTIMENT"
  )
  expect_identical(
    text_analysis[["Q1 Other - Parent Topics"]]$response_column_id,
    "QID1_3_e476cefa310845248231594eParTopics"
  )
  expect_identical(text_analysis[["Q1 Other - Sentiment"]]$parent_qid, "QID1")
  expect_identical(
    text_analysis[["Q1 Other - Sentiment"]]$parent_question_name,
    "Q1"
  )
  expect_identical(
    text_analysis[["Q1 Other - Sentiment"]]$parent_block,
    "Main Block"
  )
})

test_that("column-map classification identifies metadata-defined exports", {
  raw_metadata <- synthetic_column_map_sidecar_raw_metadata()
  questions <- normalise_qualtrics_questions(
    raw_metadata$metadata,
    raw_metadata$description
  )
  embedded_data <- normalise_embedded_data_fields(
    raw_metadata$metadata,
    raw_metadata$description
  )
  scoring <- normalise_scoring_variables(
    raw_metadata$description,
    response_column_map = raw_metadata$response_column_map
  )

  classified <- classify_response_column_map(
    raw_metadata$response_column_map,
    questions = questions,
    embedded_data = embedded_data,
    scoring = scoring
  )

  row_source <- stats::setNames(
    classified$row_source,
    classified$response_column_id
  )
  expect_identical(row_source[["Source Channel"]], "embedded_data")
  expect_identical(row_source[["SC_TOTAL"]], "scoring")
  expect_false("SC_HIDDEN" %in% classified$response_column_id)
  expect_identical(row_source[["StartTime"]], "system")
  expect_identical(row_source[["EndDate"]], "system")
  expect_identical(row_source[["Q_URL"]], "system")
  expect_identical(row_source[["QID508_TEXT"]], "question")
  expect_identical(row_source[["QID626_TEXT"]], "question")
  expect_identical(row_source[["QID429_TEXT"]], "question")
  expect_identical(row_source[["QID700_1"]], "question")
  expect_identical(row_source[["QID121_1"]], "question")
  expect_identical(row_source[["QID700_DO_1"]], "question_auxiliary")
  expect_identical(row_source[["x27_QID700_1"]], "question_auxiliary")
  expect_identical(row_source[["8_QID508_TEXT"]], "question_auxiliary")
  expect_true(all(
    glad_sa6_text_analysis_sidecar_ids() %in%
      classified$response_column_id[classified$row_source == "text_analysis"]
  ))
  expect_true(all(
    edgi_signup_text_analysis_sidecar_ids() %in%
      classified$response_column_id[classified$row_source == "text_analysis"]
  ))
})

test_that("Text-analysis Sidecars use column-map classification", {
  raw_metadata <- synthetic_column_map_sidecar_raw_metadata()

  text_analysis <- normalise_qualtrics_metadata(raw_metadata)$text_analysis
  text_analysis_ids <- map_chr(text_analysis, "response_column_id")

  expect_setequal(
    text_analysis_ids,
    c(
      glad_sa6_text_analysis_sidecar_ids(),
      edgi_signup_text_analysis_sidecar_ids()
    )
  )
  expect_length(text_analysis_ids, 15)
  expect_false("QID508_TEXT" %in% text_analysis_ids)
  expect_false("QID626_TEXT" %in% text_analysis_ids)
  expect_false("QID429_TEXT" %in% text_analysis_ids)
  expect_false("QID700_1" %in% text_analysis_ids)
  expect_false("QID121_1" %in% text_analysis_ids)
  expect_false("QID700_DO_1" %in% text_analysis_ids)
  expect_false("x27_QID700_1" %in% text_analysis_ids)
  expect_false("8_QID508_TEXT" %in% text_analysis_ids)
  expect_identical(text_analysis[[1]]$parent_qid, "QID694")
  expect_identical(text_analysis[[14]]$parent_qid, "QID121")
})

test_that("Text-analysis Sidecars do not require known metric suffixes", {
  raw_metadata <- synthetic_column_map_sidecar_raw_metadata()
  raw_metadata$response_column_map <- tibble::tibble(
    qname = "QID694_TEXT_9079b4e7_gaygfh795aeaModelLabel",
    ImportId = "QID694_TEXT_9079b4e7_gaygfh795aeaModelLabel",
    description = "CAM.TV.1txt.0 - Model Label",
    main = "CAM.TV.1txt.0",
    sub = "Model Label"
  )

  text_analysis <- normalise_qualtrics_metadata(raw_metadata)$text_analysis

  expect_named(text_analysis, "CAM.TV.1txt.0 - Model Label")
  expect_identical(
    text_analysis[[1]]$response_column_id,
    "QID694_TEXT_9079b4e7_gaygfh795aeaModelLabel"
  )
})

test_that("ordinary SBS response columns are not Text-analysis Sidecars", {
  raw_metadata <- synthetic_sbs_multiple_answer_raw_metadata()
  raw_metadata$response_column_map <- tibble::tibble(
    qname = c("QID2#3_2_1", "QID2#3_4_2", "QID2#1_4_TEXT"),
    ImportId = c("QID2#3_2_1", "QID2#3_4_2", "QID2#1_4_TEXT"),
    description = c(
      "Side by side - Multiple column - Second row - Checked A",
      "Side by side - Multiple column - Fourth row - Checked B",
      "Side by side - Text column - Fourth row"
    ),
    main = rep("Side by side", 3),
    sub = c(
      "Multiple column - Second row - Checked A",
      "Multiple column - Fourth row - Checked B",
      "Text column - Fourth row"
    )
  )

  normalised_metadata <- normalise_qualtrics_metadata(raw_metadata)
  classified <- classify_response_column_map(
    raw_metadata$response_column_map,
    questions = normalised_metadata$questions,
    embedded_data = normalised_metadata$embedded_data,
    scoring = normalised_metadata$scoring
  )

  expect_length(normalised_metadata$text_analysis, 0)
  expect_identical(
    classified$row_source,
    c("question", "question", "question_auxiliary")
  )
  expect_false(any(classified$row_source == "text_analysis"))
})

test_that("Text-analysis Sidecar column-map helpers normalise", {
  expect_identical(
    classify_response_column_map(
      tibble::tibble(),
      questions = list(),
      embedded_data = list(),
      scoring = list()
    ),
    empty_response_column_map_classification()
  )
  expect_identical(
    ordinary_question_response_column_ids(list()),
    character()
  )
  expect_identical(
    text_analysis_sidecars_from_response_column_map(NULL),
    list()
  )
  expect_identical(
    response_column_map_ids(tibble::tibble(other = "QID1_TEXT_SENTIMENT")),
    character()
  )
  expect_null(
    normalise_text_analysis_sidecar(
      list(
        sidecar_name = "",
        response_column_id = "QID1_TEXT_SENTIMENT",
        parent_qid = "QID1"
      ),
      questions = list()
    )
  )
  expect_identical(
    text_analysis_sidecar_parent_context(NA_character_, list()),
    list(
      parent_qid = NA_character_,
      parent_question_name = NA_character_,
      parent_block = NA_character_
    )
  )

  classification <- tibble::tibble(
    response_column_id = "QID1_TEXT_SENTIMENT",
    row_source = "text_analysis",
    parent_qid = "QID1",
    display_name = "Q1 Sentiment",
    main = "Q1",
    sub = "Sentiment",
    description = "Q1 Sentiment",
    reason = "derived_question"
  )
  records <- text_analysis_sidecars_from_response_column_map(classification)

  expect_length(records, 1)
  expect_identical(
    records[[1]][c("sidecar_name", "response_column_id", "parent_qid")],
    list(
      sidecar_name = "Q1 Sentiment",
      response_column_id = "QID1_TEXT_SENTIMENT",
      parent_qid = "QID1"
    )
  )
})

test_that("response-column map classifier reports edge-case unknown reasons", {
  raw_metadata <- synthetic_column_map_sidecar_raw_metadata()
  raw_metadata$response_column_map <- tibble::tibble(
    qname = c(NA_character_, "QID999_derived", "QID694_derived_empty"),
    ImportId = c(NA_character_, "QID999_derived", "QID694_derived_empty"),
    description = c(NA_character_, "Unknown parent derived row", ""),
    main = c(NA_character_, "Unknown parent", ""),
    sub = c(NA_character_, "Derived row", "")
  )
  normalised_metadata <- normalise_qualtrics_metadata(raw_metadata)

  classified <- classify_response_column_map(
    raw_metadata$response_column_map,
    questions = normalised_metadata$questions,
    embedded_data = normalised_metadata$embedded_data,
    scoring = normalised_metadata$scoring
  )

  expect_identical(
    classified$reason,
    c(
      "missing_response_column_id",
      "no_known_parent_qid",
      "no_derived_column_map_fields"
    )
  )
  expect_identical(classified$row_source, rep("unknown", 3))
})

test_that("description Survey Flow Embedded Data Fields add block candidates", {
  raw_metadata <- synthetic_survey_flow_embedded_data_raw_metadata()

  normalised_metadata <- normalise_qualtrics_metadata(raw_metadata)
  embedded_data <- normalised_metadata$embedded_data

  expect_named(
    embedded_data,
    c("Before Main", "Between Blocks", "After Follow-up")
  )
  expect_identical(
    embedded_data[["Before Main"]]$previous_block,
    NA_character_
  )
  expect_identical(
    embedded_data[["Before Main"]]$next_block,
    "Main Block"
  )
  expect_identical(
    embedded_data[["Between Blocks"]]$previous_block,
    "Main Block"
  )
  expect_identical(
    embedded_data[["Between Blocks"]]$next_block,
    "Follow-up Block"
  )
})

test_that("nested Survey Flow Embedded Data Fields normalise with candidates", {
  raw_metadata <- synthetic_nested_survey_flow_embedded_data_raw_metadata()

  normalised_metadata <- normalise_qualtrics_metadata(raw_metadata)
  embedded_data <- normalised_metadata$embedded_data

  expect_named(
    embedded_data,
    c("Before Main", "Between Blocks", "After Follow-up")
  )
  expect_identical(
    embedded_data[["Between Blocks"]]$previous_block,
    "Main Block"
  )
  expect_identical(
    embedded_data[["Between Blocks"]]$next_block,
    "Follow-up Block"
  )
})

test_that("metadata flow does not locate Embedded Data Fields", {
  raw_metadata <- synthetic_mc_text_raw_metadata()
  raw_metadata$metadata$flow <- list(
    Flow = list(
      list(type = "EmbeddedData"),
      list(Type = "EmbeddedData", Field = "Ignored Flow Field")
    )
  )
  raw_metadata$description$flow <- list()

  expect_length(
    normalise_qualtrics_metadata(raw_metadata)$embedded_data,
    0
  )
})

test_that("description Survey Flow locations merge only onto flat fields", {
  raw_metadata <- synthetic_survey_flow_embedded_data_raw_metadata()
  raw_metadata$metadata$embedded_data <- embedded_data_records(
    c("Before Main", "Flat Only")
  )

  embedded_data <- normalise_qualtrics_metadata(raw_metadata)$embedded_data

  expect_named(embedded_data, c("Before Main", "Flat Only"))
  expect_identical(
    embedded_data[["Before Main"]]$next_block,
    "Main Block"
  )
  expect_null(embedded_data[["Flat Only"]]$previous_block)
  expect_null(embedded_data[["Flat Only"]]$next_block)
})

test_that("description Survey Flow Embedded Data skips missing block lookups", {
  raw_metadata <- synthetic_survey_flow_embedded_data_raw_metadata()
  raw_metadata$description$blocks <- NULL

  embedded_data <- normalise_survey_flow_embedded_data_fields(
    raw_metadata$description
  )

  expect_named(
    embedded_data,
    c("Before Main", "Between Blocks", "After Follow-up")
  )
  expect_identical(
    embedded_data[["Between Blocks"]]$previous_block,
    NA_character_
  )
  expect_identical(
    embedded_data[["Between Blocks"]]$next_block,
    NA_character_
  )
})

test_that("Embedded Data Field names normalise from flat metadata records", {
  expect_identical(
    embedded_data_field_names(list(
      list(name = "Wave"),
      list(name = ""),
      list(name = NA_character_)
    )),
    "Wave"
  )
  expect_identical(
    embedded_data_field_names(list(
      "Standalone",
      list(fieldName = "Legacy Field"),
      list(Field = "Flow Field")
    )),
    character()
  )
  expect_identical(
    embedded_data_field_names("Standalone"),
    character()
  )
  expect_identical(
    embedded_data_flow_field_name(NULL),
    NA_character_
  )
})

test_that("description Survey Flow helpers cover empty artifact branches", {
  expect_identical(
    normalise_survey_flow_embedded_data_fields(list(
      flow = list(Type = "EmbeddedData", EmbeddedData = list())
    )),
    empty_normalised_embedded_data_fields()
  )
  expect_length(
    survey_flow_items(list(named = list(Type = "Standard", ID = "BL_1"))),
    1
  )
  expect_identical(survey_flow_item_type("Root"), NA_character_)
  expect_identical(
    survey_flow_embedded_data_field_names(list(Type = "EmbeddedData")),
    character()
  )
})

test_that("Scoring Variables normalise from survey description metadata", {
  scoring <- normalise_qualtrics_metadata(
    synthetic_scoring_raw_metadata()
  )$scoring

  expect_named(scoring, c("Total Score", "Q1"))
  expect_identical(
    scoring[["Total Score"]]$response_column_id,
    "SC_TOTAL"
  )
  expect_identical(
    scoring[["Total Score"]]$question_text,
    "Scoring Variable: Total Score"
  )
})

test_that("Scoring Variables normalise from nested scoring categories", {
  scoring <- normalise_qualtrics_metadata(
    synthetic_nested_scoring_raw_metadata()
  )$scoring

  expect_named(scoring, c("Total Score", "SC_SCREEN"))
  expect_identical(
    scoring[["Total Score"]]$response_column_id,
    "SC_TOTAL"
  )
  expect_identical(
    scoring[["SC_SCREEN"]]$response_column_id,
    "SC_SCREEN"
  )
})

test_that("Scoring Variables skip non-exported response columns when known", {
  raw_metadata <- synthetic_nested_scoring_raw_metadata()
  raw_metadata$response_column_map <- tibble::tibble(
    ImportId = "SC_TOTAL"
  )

  scoring <- normalise_qualtrics_metadata(raw_metadata)$scoring

  expect_named(scoring, "Total Score")
  expect_identical(scoring[["Total Score"]]$response_column_id, "SC_TOTAL")
})

test_that("Scoring Variables require artifact-backed category fields", {
  scoring <- normalise_scoring_variables(list(
    scoring = list(
      ScoringCategories = list(
        list(ID = "SC_1", Name = "Exported"),
        list(ID = "SC_MISSING_NAME"),
        list(Name = "Missing ID")
      )
    )
  ))

  expect_named(scoring, "Exported")
  expect_identical(scoring[["Exported"]]$response_column_id, "SC_1")
  expect_identical(scoring[["Exported"]]$output_name, "Exported")
  expect_identical(scoring_categories(list(ScoringCategories = list())), list())
  expect_identical(scoring_category_name(NULL), NA_character_)
  expect_identical(scoring_category_response_column_id(NULL), NA_character_)
  expect_identical(
    text_analysis_sidecar_parent_context("QID999", list()),
    empty_text_analysis_sidecar_parent_context()
  )
})
