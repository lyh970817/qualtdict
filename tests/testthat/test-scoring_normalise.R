test_that("Scoring Variables normalise from survey description metadata", {
  scoring <- normalise_qualtrics_metadata(
    synthetic_scoring_raw_metadata()
  )$scoring

  expect_named(scoring, c("Total Score", "Q1"))
  expect_identical(
    scoring[["Total Score"]]$response_column_id,
    "SC_TOTAL"
  )
  expect_identical(
    scoring[["Total Score"]]$question_text,
    "Scoring Variable: Total Score"
  )
})

test_that("Scoring Variables normalise from nested scoring categories", {
  scoring <- normalise_qualtrics_metadata(
    synthetic_nested_scoring_raw_metadata()
  )$scoring

  expect_named(scoring, c("Total Score", "SC_SCREEN"))
  expect_identical(
    scoring[["Total Score"]]$response_column_id,
    "SC_TOTAL"
  )
  expect_identical(
    scoring[["SC_SCREEN"]]$response_column_id,
    "SC_SCREEN"
  )
})

test_that("Scoring Variables skip non-exported response columns when known", {
  raw_metadata <- synthetic_nested_scoring_raw_metadata()
  raw_metadata$response_column_map <- tibble::tibble(
    ImportId = "SC_TOTAL"
  )

  scoring <- normalise_qualtrics_metadata(raw_metadata)$scoring

  expect_named(scoring, "Total Score")
  expect_identical(scoring[["Total Score"]]$response_column_id, "SC_TOTAL")
})

test_that("Scoring Variables require artifact-backed category fields", {
  scoring <- normalise_scoring_variables(list(
    scoring = list(
      ScoringCategories = list(
        list(ID = "SC_1", Name = "Exported"),
        list(ID = "SC_MISSING_NAME"),
        list(Name = "Missing ID")
      )
    )
  ))

  expect_named(scoring, "Exported")
  expect_identical(scoring[["Exported"]]$response_column_id, "SC_1")
  expect_identical(scoring[["Exported"]]$output_name, "Exported")
  expect_identical(scoring_categories(list(ScoringCategories = list())), list())
  expect_identical(scoring_category_name(NULL), NA_character_)
  expect_identical(scoring_category_response_column_id(NULL), NA_character_)
  expect_identical(
    text_analysis_sidecar_parent_context("QID999", list()),
    empty_text_analysis_sidecar_parent_context()
  )
})
