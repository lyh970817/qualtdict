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

test_that("unsupported shapes warn and fall back to Base Response Column ID", {
  question <- normalise_qualtrics_metadata(
    synthetic_mc_text_raw_metadata()
  )$questions$QID1
  question$question_type$type <- "Meta"
  question$question_type$selector <- "Browser"
  question$question_type$sub_selector <- NULL

  expect_warning(
    rendered <- render_response_columns(question, "x1_QID1"),
    "falling back to the bare QID.",
    fixed = TRUE
  )
  expect_identical(rendered$response_column_id, "x1_QID1")
  expect_snapshot(compact_response_column_render(rendered))
})
