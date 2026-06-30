test_that("question metadata normalises question facts", {
  raw_metadata <- synthetic_mc_text_raw_metadata()

  survey_question_facts <- normalise_qualtrics_questions(
    raw_metadata$metadata,
    raw_metadata$description
  )

  expect_s3_class(
    survey_question_facts,
    "qualtdict_normalised_questions"
  )
  expect_named(survey_question_facts, "QID1")
  expect_s3_class(
    survey_question_facts$QID1,
    "qualtdict_normalised_question"
  )
  expect_identical(survey_question_facts$QID1$qid, "QID1")
  expect_identical(survey_question_facts$QID1$question_name, "Q1")
  expect_identical(survey_question_facts$QID1$survey_block, "Main Block")
  expect_identical(survey_question_facts$QID1$content_type, "Number")
})

test_that("question metadata uses default block metadata", {
  raw_metadata <- synthetic_mc_text_raw_metadata()
  raw_metadata$description$blocks <- list()

  survey_question_facts <- normalise_qualtrics_questions(
    raw_metadata$metadata,
    raw_metadata$description
  )

  expect_identical(
    survey_question_facts$QID1$survey_block,
    NA_character_
  )
  expect_null(survey_question_facts$QID1$looping_static)
})
