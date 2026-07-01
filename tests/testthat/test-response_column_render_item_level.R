test_that("matrix item-level rows render in stable order", {
  rendered <- render_response_column_fixture(
    synthetic_matrix_raw_metadata(),
    "QID1"
  )

  expect_identical(
    rendered$response_column_id,
    c("QID1_x1", "QID1_x1", "QID1_x2", "QID1_x2")
  )
  expect_snapshot(compact_response_column_render(rendered))
})

test_that("render_response_columns renders matrix rows in stable order", {
  raw_metadata <- synthetic_matrix_raw_metadata()
  question <- normalise_qualtrics_metadata(raw_metadata)$questions$QID1

  rendered <- render_response_columns(question, "QID1")

  expect_identical(
    rendered$response_column_id,
    c("QID1_x1", "QID1_x1", "QID1_x2", "QID1_x2")
  )
  expect_identical(
    unname(rendered$item),
    c("Apples", "Apples", "Bananas", "Bananas")
  )
  expect_identical(unname(rendered$level), c("1", "2", "1", "2"))
  expect_identical(unname(rendered$label), c("Low", "High", "Low", "High"))
  expect_true(all(vapply(rendered, length, integer(1)) == nrow(rendered)))
})

test_that("slider items render as one response column per item", {
  rendered <- render_response_column_fixture(
    synthetic_slider_raw_metadata(),
    "QID1"
  )

  expect_identical(
    rendered$response_column_id,
    c("QID1_1", "QID1_2", "QID1_3")
  )
  expect_snapshot(compact_response_column_render(rendered))
})

test_that("matrix response columns use Qualtrics subquestion IDs", {
  raw_metadata <- synthetic_matrix_raw_metadata()
  normalised_metadata <- normalise_qualtrics_metadata(raw_metadata)

  dict <- variable_dictionary_from_normalised_metadata(
    normalised_metadata,
    use_semantic_name = FALSE,
    block_pattern = NULL,
    block_sep = ".",
    semantic_name_preprocess = NULL
  )

  expect_identical(
    unique(dict$response_column_id),
    c("QID1_x1", "QID1_x2")
  )
  expect_identical(dict$qid, rep("QID1", 4))
  expect_identical(
    unname(dict$item),
    c("Apples", "Apples", "Bananas", "Bananas")
  )
  expect_identical(unname(dict$level), c("1", "2", "1", "2"))
  expect_identical(unname(dict$label), c("Low", "High", "Low", "High"))
})

test_that("horizontal sliders generate one response column per slider item", {
  raw_metadata <- synthetic_slider_raw_metadata()
  normalised_metadata <- normalise_qualtrics_metadata(raw_metadata)

  dict <- variable_dictionary_from_normalised_metadata(
    normalised_metadata,
    use_semantic_name = FALSE,
    block_pattern = NULL,
    block_sep = ".",
    semantic_name_preprocess = NULL
  )

  expect_identical(dict$response_column_id, c("QID1_1", "QID1_2", "QID1_3"))
  expect_identical(unname(dict$level), c("1", "2", "3"))
  expect_true(all(lengths(dict) == nrow(dict)))
})

test_that("numeric loop-prefixed text columns match raw export IDs", {
  raw_metadata <- synthetic_numeric_looped_text_raw_metadata()
  normalised_metadata <- normalise_qualtrics_metadata(raw_metadata)

  dict <- variable_dictionary_from_normalised_metadata(
    normalised_metadata,
    use_semantic_name = FALSE,
    block_pattern = NULL,
    block_sep = ".",
    semantic_name_preprocess = NULL
  )

  target_rows <- dict[grepl("QID3", dict$response_column_id, fixed = TRUE), ]

  expect_identical(
    target_rows$response_column_id,
    paste0(1:12, "_QID3_TEXT")
  )
  expect_identical(target_rows$loop_option, paste("Loop", 1:12))
})
