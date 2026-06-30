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
  expect_identical(
    text_analysis_sidecar_parent_context("QID999", list()),
    empty_text_analysis_sidecar_parent_context()
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
