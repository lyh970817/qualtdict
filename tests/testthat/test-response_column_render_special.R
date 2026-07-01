test_that("display block questions render no rows", {
  question <- normalise_question_fact(
    qid = "QID1",
    question = list(
      questionName = "Intro",
      questionType = list(type = "DB", selector = "TB", subSelector = NULL),
      questionText = "Intro text",
      choices = list(),
      subQuestions = list(),
      columns = list()
    ),
    block = list(description = "Main Block"),
    content_type = NULL
  )

  rendered <- render_response_columns(question, "QID1")

  expect_identical(nrow(rendered), 0L)
  expect_named(
    rendered,
    c("response_column_id", "question", "item", "level", "label")
  )
})

test_that("Timing fixed columns preserve current response column IDs", {
  rendered <- render_response_column_fixture(
    synthetic_timing_raw_metadata(),
    "QID1"
  )

  expect_identical(
    rendered$response_column_id,
    c(
      "QID1_FIRST_CLICK",
      "QID1_LAST_CLICK",
      "QID1_PAGE_SUBMIT",
      "QID1_CLICK_COUNT"
    )
  )
  expect_snapshot(compact_response_column_render(rendered))
})

test_that("FileUpload fixed columns preserve current response column IDs", {
  rendered <- render_response_column_fixture(
    synthetic_file_upload_raw_metadata(),
    "QID1"
  )

  expect_identical(
    rendered$response_column_id,
    c(
      "QID1_FILE_ID",
      "QID1_FILE_NAME",
      "QID1_FILE_SIZE",
      "QID1_FILE_TYPE"
    )
  )
  expect_snapshot(compact_response_column_render(rendered))
})

test_that(
  paste(
    "render_response_columns warns and falls back",
    "for unrendered shapes"
  ),
  {
    raw_metadata <- synthetic_mc_text_raw_metadata()
    question <- normalise_qualtrics_metadata(raw_metadata)$questions$QID1
    question$question_type$type <- "Meta"
    question$question_type$selector <- "Browser"
    question$question_type$sub_selector <- NULL

    expect_warning(
      rendered <- render_response_columns(question, "QID1"),
      "without a specific response-column renderer"
    )
    expect_identical(rendered$response_column_id, "QID1")
  }
)

test_that("unsupported shapes warn and fall back to Base Response Column ID", {
  question <- normalise_qualtrics_metadata(
    synthetic_mc_text_raw_metadata()
  )$questions$QID1
  question$question_type$type <- "Meta"
  question$question_type$selector <- "Browser"
  question$question_type$sub_selector <- NULL

  expect_warning(
    rendered <- render_response_columns(question, "x1_QID1"),
    "falling back to the Base Response Column ID.",
    fixed = TRUE
  )
  expect_identical(rendered$response_column_id, "x1_QID1")
  expect_snapshot(compact_response_column_render(rendered))
})

test_that(
  paste(
    "response column renderer context preserves unsupported type",
    "fallback"
  ),
  {
    question_fact <- normalise_qualtrics_metadata(
      synthetic_mc_text_raw_metadata()
    )$questions$QID1
    question_fact$question_type <- list(
      type = "UnsupportedType",
      selector = "UnsupportedSelector",
      sub_selector = NULL
    )
    shape <- response_column_shape(question_fact)
    context <- new_response_column_render_context(
      question_fact = question_fact,
      base_response_column_id = "QID1",
      shape = shape,
      question_type = question_fact$question_type
    )

    expect_warning(
      rendered <- render_response_column_ids(context),
      "QID1 uses a question type without a specific response-column renderer",
      fixed = TRUE
    )
    expect_identical(rendered, "QID1")
  }
)
