test_that("SBS mixed columns preserve row metadata alignment", {
  rendered <- render_response_column_fixture(
    synthetic_sbs_text_subquestion_raw_metadata(),
    "QID2"
  )

  expect_renderer_rows_aligned(rendered)
  expect_snapshot(compact_response_column_render(rendered))
})

test_that("render_response_columns renders SBS and sidecar columns", {
  sbs_question <- normalise_qualtrics_metadata(
    synthetic_sbs_text_subquestion_raw_metadata()
  )$questions$QID2
  timing_question <- normalise_qualtrics_metadata(
    synthetic_timing_raw_metadata()
  )$questions$QID1
  file_upload_question <- normalise_qualtrics_metadata(
    synthetic_file_upload_raw_metadata()
  )$questions$QID1

  sbs_rendered <- render_response_columns(sbs_question, "QID2")
  timing_rendered <- render_response_columns(timing_question, "QID1")
  file_upload_rendered <- render_response_columns(file_upload_question, "QID1")

  expect_identical(
    sbs_rendered$response_column_id,
    c(
      "QID2#1_2_1",
      "QID2#1_4_1",
      "QID2#1_4_TEXT",
      "QID2#1_9_1",
      "QID2#2_2",
      "QID2#2_2",
      "QID2#2_4",
      "QID2#2_4",
      "QID2#2_4_TEXT",
      "QID2#2_9",
      "QID2#2_9",
      "QID2#3_2",
      "QID2#3_2",
      "QID2#3_4",
      "QID2#3_4",
      "QID2#3_4_TEXT",
      "QID2#3_9",
      "QID2#3_9"
    )
  )
  expect_true(
    all(vapply(sbs_rendered, length, integer(1)) == nrow(sbs_rendered))
  )

  expect_identical(
    timing_rendered$response_column_id,
    c(
      "QID1_FIRST_CLICK",
      "QID1_LAST_CLICK",
      "QID1_PAGE_SUBMIT",
      "QID1_CLICK_COUNT"
    )
  )
  expect_identical(
    file_upload_rendered$response_column_id,
    c("QID1_FILE_ID", "QID1_FILE_NAME", "QID1_FILE_SIZE", "QID1_FILE_TYPE")
  )
})

test_that("SBS carried-forward rows use subquestion response column IDs", {
  rendered <- render_response_column_fixture(
    synthetic_sbs_carried_forward_raw_metadata(),
    "QID2"
  )

  expect_identical(
    rendered$response_column_id,
    c("QID2_x1", "QID2_x2", "QID2_x3")
  )
  expect_snapshot(compact_response_column_render(rendered))
})

test_that("SBS shape preparation carries text-entry item rows", {
  question_fact <- normalise_qualtrics_metadata(
    synthetic_sbs_text_subquestion_raw_metadata()
  )$questions$QID2

  item_shape <- response_column_item_shape(question_fact)
  sbs_items <- response_column_sbs_item_shape(
    question_fact,
    item_shape$has_text_sub
  )

  expect_identical(
    unname(sbs_items),
    c("Second row", "Fourth row", "Fourth row_TEXT", "Ninth row")
  )
  expect_named(sbs_items, c("2", "4", "4_TEXT", "9"))
})

test_that("SBS multiple-answer columns include column, row, and choice IDs", {
  raw_metadata <- synthetic_sbs_multiple_answer_raw_metadata()
  normalised_metadata <- normalise_qualtrics_metadata(raw_metadata)

  dict <- variable_dictionary_from_normalised_metadata(
    normalised_metadata,
    use_semantic_name = FALSE,
    block_pattern = NULL,
    block_sep = ".",
    semantic_name_preprocess = NULL
  )

  expect_true(all(
    c(
      "QID2#3_2_1",
      "QID2#3_2_2",
      "QID2#3_4_1",
      "QID2#3_4_2"
    ) %in%
      dict$response_column_id
  ))
  expect_identical(
    grep("^QID2#3_", dict$response_column_id, value = TRUE),
    c("QID2#3_2_1", "QID2#3_2_2", "QID2#3_4_1", "QID2#3_4_2")
  )
  expect_identical(
    unname(dict$level[grepl("^QID2#3_", dict$response_column_id)]),
    c("1", "2", "1", "2")
  )
  expect_true(all(lengths(dict) == nrow(dict)))
})

test_that("carried-forward SBS rows use subquestion response column IDs", {
  raw_metadata <- synthetic_sbs_carried_forward_raw_metadata()
  normalised_metadata <- normalise_qualtrics_metadata(raw_metadata)

  dict <- variable_dictionary_from_normalised_metadata(
    normalised_metadata,
    use_semantic_name = FALSE,
    block_pattern = NULL,
    block_sep = ".",
    semantic_name_preprocess = NULL
  )

  expect_identical(
    dict$response_column_id,
    c("QID2_x1", "QID2_x2", "QID2_x3")
  )
  expect_identical(unname(dict$item), c("First row", "Second row", "Third row"))
  expect_true(all(lengths(dict) == nrow(dict)))
})

test_that("SBS text-entry subquestions keep row metadata lengths aligned", {
  raw_metadata <- synthetic_sbs_text_subquestion_raw_metadata()
  normalised_metadata <- normalise_qualtrics_metadata(raw_metadata)

  expect_no_error(
    dict <- variable_dictionary_from_normalised_metadata(
      normalised_metadata,
      use_semantic_name = FALSE,
      block_pattern = NULL,
      block_sep = ".",
      semantic_name_preprocess = NULL
    )
  )

  expect_true(all(lengths(dict) == nrow(dict)))
  expect_identical(
    dict$response_column_id,
    c(
      "QID2#1_2_1",
      "QID2#1_4_1",
      "QID2#1_4_TEXT",
      "QID2#1_9_1",
      "QID2#2_2",
      "QID2#2_2",
      "QID2#2_4",
      "QID2#2_4",
      "QID2#2_4_TEXT",
      "QID2#2_9",
      "QID2#2_9",
      "QID2#3_2",
      "QID2#3_2",
      "QID2#3_4",
      "QID2#3_4",
      "QID2#3_4_TEXT",
      "QID2#3_9",
      "QID2#3_9"
    )
  )
  expect_identical(
    unname(dict$item),
    c(
      "Second row",
      "Fourth row",
      "Fourth row_TEXT",
      "Ninth row",
      "Second row",
      "Second row",
      "Fourth row",
      "Fourth row",
      "Fourth row_TEXT",
      "Ninth row",
      "Ninth row",
      "Second row",
      "Second row",
      "Fourth row",
      "Fourth row",
      "Fourth row_TEXT",
      "Ninth row",
      "Ninth row"
    )
  )
})
