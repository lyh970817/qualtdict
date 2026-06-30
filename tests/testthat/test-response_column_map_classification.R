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
