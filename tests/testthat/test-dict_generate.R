test_that("dict_generate", {
  vcr::use_cassette("dict_generate", {
    suppressWarnings(
      x <- dict_generate("SV_0AQg1pFepA0V2d0", variable_name = "semantic_name")
    )
  })

  legacy_columns <- c(
    "response_column_id", "qid", "question_name", "variable_name", "block",
    "question", "item", "level", "label", "type", "selector", "sub_selector",
    "content_type"
  )
  snapshot_x <- x[legacy_columns]
  attr(snapshot_x, "variable_name_findings") <- NULL

  expect_s3_class(snapshot_x, "data.frame")
  expect_true("response_column_id" %in% names(x))
  expect_true(any(x$response_column_id != x$qid))
  expect_true("question_name" %in% names(x))
  expect_true("variable_name" %in% names(x))
  expect_true("loop_option" %in% names(x))
  expect_true(any(!is.na(x$loop_option)))
})

test_that("dict_generate accepts variable_name question_name", {
  vcr::use_cassette("dict_generate", {
    suppressWarnings(
      x <- dict_generate(
        "SV_0AQg1pFepA0V2d0",
        variable_name = "question_name"
      )
    )
  })

  expect_true("question_name" %in% names(x))
  expect_true("variable_name" %in% names(x))
  expect_true(all(!is.na(x$question_name)))
  expect_equal(
    anyDuplicated(unique(x[c("response_column_id", "variable_name")])),
    0
  )
})
