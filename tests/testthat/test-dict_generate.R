test_that("dict_generate", {
  local_mocked_bindings(
    fetch_dictionary_metadata = function(surveyID) {
      synthetic_loop_and_merge_raw_metadata()
    }
  )

  suppressWarnings(
    x <- dict_generate("SV_SYNTHETIC", variable_name = "semantic_name")
  )

  legacy_columns <- c(
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
  snapshot_x <- x[legacy_columns]
  attr(snapshot_x, "variable_name_findings") <- NULL

  expect_s3_class(snapshot_x, "data.frame")
  expect_identical(names(x)[1:2], c("response_column_id", "row_source"))
  expect_true("response_column_id" %in% names(x))
  expect_true(all(x$row_source == "question"))
  expect_true(any(x$response_column_id != x$qid))
  expect_true("question_name" %in% names(x))
  expect_true("variable_name" %in% names(x))
  expect_true("loop_option" %in% names(x))
  expect_false(all(is.na(x$loop_option)))
})

test_that("dict_generate accepts variable_name question_name", {
  local_mocked_bindings(
    fetch_dictionary_metadata = function(surveyID) {
      synthetic_mc_text_raw_metadata()
    }
  )

  suppressWarnings(
    x <- dict_generate(
      "SV_SYNTHETIC",
      variable_name = "question_name"
    )
  )

  expect_true("question_name" %in% names(x))
  expect_true("variable_name" %in% names(x))
  expect_identical(names(x)[1:2], c("response_column_id", "row_source"))
  expect_true(all(x$row_source == "question"))
  expect_false(anyNA(x$question_name))
  expect_identical(
    anyDuplicated(unique(x[c("response_column_id", "variable_name")])),
    0L
  )
})
