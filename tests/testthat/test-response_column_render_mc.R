test_that("MC text-entry choices render current response column IDs", {
  rendered <- render_response_column_fixture(
    synthetic_mc_text_raw_metadata(),
    "QID1"
  )

  expect_identical(
    rendered$response_column_id,
    c("QID1", "QID1", "QID1", "QID1_3_TEXT")
  )
  expect_snapshot(compact_response_column_render(rendered))
})

test_that("render_response_columns renders MC rows with aligned facts", {
  raw_metadata <- synthetic_mc_text_raw_metadata()
  question <- normalise_qualtrics_metadata(raw_metadata)$questions$QID1

  rendered <- render_response_columns(question, "QID1")

  expect_identical(
    rendered$response_column_id,
    c("QID1", "QID1", "QID1", "QID1_3_TEXT")
  )
  expect_identical(unname(rendered$level), c("1", "2", "3", "3_TEXT"))
  expect_identical(
    unname(rendered$label),
    c("Yes", "No", "Other", "Other_TEXT")
  )
  expect_true(all(vapply(rendered, length, integer(1)) == nrow(rendered)))
})

test_that("MC independent columns preserve recode suffix behavior", {
  rendered <- render_response_column_fixture(
    synthetic_mc_x_choice_raw_metadata(),
    "QID126879611"
  )

  expect_identical(
    rendered$response_column_id,
    paste0("QID126879611_", c("1", "2", "3", "4", "6"))
  )
  expect_snapshot(compact_response_column_render(rendered))
})

test_that(
  paste(
    "render_response_columns uses recode suffixes",
    "for x-named MC choices"
  ),
  {
    question <- normalise_qualtrics_metadata(
      synthetic_mc_x_choice_raw_metadata()
    )$questions$QID126879611

    rendered <- render_response_columns(question, "QID126879611")

    expect_identical(
      rendered$response_column_id,
      paste0("QID126879611_", c("1", "2", "3", "4", "6"))
    )
    expect_identical(unname(rendered$level), c("1", "2", "3", "4", "6"))
    expect_identical(
      unname(rendered$label),
      paste("Choice", c("1", "2", "3", "4", "6"))
    )
  }
)

test_that("multiple-answer columns use numeric choice recodes", {
  raw_metadata <- synthetic_mc_recode_raw_metadata()
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
    c(
      "QID1_1",
      "QID1_2",
      "QID1_-88",
      "QID1_-99",
      "QID1_0",
      "QID1_9_TEXT"
    )
  )
  expect_identical(
    unname(dict$level),
    c("1", "2", "-88", "-99", "0", "0_TEXT")
  )
  expect_true(all(lengths(dict) == nrow(dict)))
})

test_that("MC renderer preserves recodes and text-entry choice IDs", {
  rendered <- render_response_column_fixture(
    synthetic_mc_recode_raw_metadata(),
    "QID1"
  )

  expect_identical(
    rendered$response_column_id,
    c(
      "QID1_1",
      "QID1_2",
      "QID1_-88",
      "QID1_-99",
      "QID1_0",
      "QID1_9_TEXT"
    )
  )
  expect_identical(
    unname(rendered$level),
    c("1", "2", "-88", "-99", "0", "0_TEXT")
  )
})

test_that("multiple-answer response columns use recodes for x choice IDs", {
  raw_metadata <- synthetic_mc_x_choice_raw_metadata()
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
    paste0("QID126879611_", c("1", "2", "3", "4", "6"))
  )
  expect_identical(unname(dict$level), c("1", "2", "3", "4", "6"))
  expect_identical(
    unname(dict$label),
    paste("Choice", c("1", "2", "3", "4", "6"))
  )
})

test_that("non-analysed multiple-answer choices are not response columns", {
  raw_metadata <- synthetic_mc_recode_raw_metadata()
  raw_metadata$metadata$questions$QID1$choices[["8"]]$analyze <- FALSE

  dict <- variable_dictionary_from_normalised_metadata(
    normalise_qualtrics_metadata(raw_metadata),
    use_semantic_name = FALSE,
    block_pattern = NULL,
    block_sep = ".",
    semantic_name_preprocess = NULL
  )

  expect_false("QID1_-99" %in% dict$response_column_id)
  expect_false("-99" %in% dict$level)
  expect_true(all(lengths(dict) == nrow(dict)))
})

test_that("non-analysed single-answer choices remain value labels", {
  raw_metadata <- synthetic_mc_text_raw_metadata()
  raw_metadata$metadata$questions$QID1$choices[["1"]]$analyze <- FALSE

  dict <- variable_dictionary_from_normalised_metadata(
    normalise_qualtrics_metadata(raw_metadata),
    use_semantic_name = FALSE,
    block_pattern = NULL,
    block_sep = ".",
    semantic_name_preprocess = NULL
  )

  expect_true("QID1" %in% dict$response_column_id)
  expect_true("1" %in% dict$level)
  expect_true("Yes" %in% dict$label)
  expect_true(all(lengths(dict) == nrow(dict)))
})

test_that("looped MC text columns keep loop prefix before QID", {
  raw_metadata <- synthetic_looped_mc_text_raw_metadata()
  normalised_metadata <- normalise_qualtrics_metadata(raw_metadata)

  dict <- variable_dictionary_from_normalised_metadata(
    normalised_metadata,
    use_semantic_name = FALSE,
    block_pattern = NULL,
    block_sep = ".",
    semantic_name_preprocess = NULL
  )

  target_rows <- dict[grepl("QID2", dict$response_column_id, fixed = TRUE), ]

  expect_identical(
    target_rows$response_column_id,
    c(
      "x1_QID2",
      "x1_QID2",
      "x1_QID2_2_TEXT",
      "x2_QID2",
      "x2_QID2",
      "x2_QID2_2_TEXT"
    )
  )
  expect_identical(
    target_rows$loop_option,
    rep(c("Apples", "Bananas"), each = 3)
  )
  expect_identical(unname(target_rows$level), rep(c("1", "2", "2_TEXT"), 2))
})
