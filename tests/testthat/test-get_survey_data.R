vcr::use_cassette("dict_generate", {
  suppressWarnings(
    x <- dict_generate("SV_0AQg1pFepA0V2d0", name = "question_name")
  )
})

test_that("get_survey_data", {
  vcr::use_cassette("get_survey_data", {
    expect_warning(x_dat <- get_survey_data(x), regexp = NA)
  })
  expect_s3_class(x_dat, c("spec_tbl_df", "tbl_df", "tbl", "data.frame"))
})

test_that("validation mistake qids are extracted for skipping", {
  validation <- list(
    errors = list(
      mistake = data.frame(
        qid = c("QID1_7", "QID1_7", "QID2_1"),
        stringsAsFactors = FALSE
      )
    )
  )

  expect_equal(
    validation_mistake_qids(validation),
    c("QID1_7", "QID2_1")
  )
  expect_equal(validation_mistake_qids(data.frame()), character())
})

test_that("validation mistakes are removed from include_questions", {
  dict <- data.frame(
    qid = c("QID1", "QID2", "QID3"),
    name = c("q1", "q2", "q3"),
    label = c("A", "B", "C"),
    level = c("1", "2", "3"),
    stringsAsFactors = FALSE
  )
  attr(dict, "class") <- c("qualtdict", class(dict))

  local_mocked_bindings(
    dict_validate = function(dict) {
      list(
        errors = list(
          mistake = data.frame(qid = "QID2", stringsAsFactors = FALSE)
        )
      )
    }
  )

  expect_equal(
    skip_validation_mistakes(NULL, dict, keys = "QID4"),
    c("QID1", "QID3", "QID4")
  )
  expect_equal(
    skip_validation_mistakes(c("QID1", "QID2"), dict, keys = NULL),
    "QID1"
  )
})
