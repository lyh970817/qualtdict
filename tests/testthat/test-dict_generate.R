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

test_that("dict_generate represents flat Embedded Data Fields", {
  local_mocked_bindings(
    fetch_dictionary_metadata = function(surveyID) {
      synthetic_flat_embedded_data_raw_metadata()
    }
  )

  dict <- dict_generate("SV_SYNTHETIC", variable_name = "question_name")
  embedded_rows <- dict[dict$row_source == "embedded_data", ]

  expect_identical(
    embedded_rows$response_column_id,
    c("Source Channel", "Q1")
  )
  expect_identical(embedded_rows$row_source, rep("embedded_data", 2))
  expect_true(all(is.na(embedded_rows$qid)))
  expect_true(all(is.na(embedded_rows$question_name)))
  expect_true(all(is.na(embedded_rows$block)))
  expect_identical(
    embedded_rows$variable_name,
    c("Source_Channel", "Q1.2")
  )
  expect_identical(
    embedded_rows$question,
    c("Embedded Data: Source Channel", "Embedded Data: Q1")
  )
  expect_true(all(is.na(embedded_rows$level)))
  expect_true(all(is.na(embedded_rows$label)))

  findings <- attr(dict, "variable_name_findings", exact = TRUE)
  embedded_findings <- findings[
    findings$response_column_id %in% embedded_rows$response_column_id,
  ]
  expect_identical(
    embedded_findings$response_column_id,
    c("Source Channel", "Q1")
  )
  expect_identical(
    unname(embedded_findings$reason),
    c("unsafe", "duplicate")
  )
})

test_that("dict_generate represents Scoring Variables", {
  local_mocked_bindings(
    fetch_dictionary_metadata = function(surveyID) {
      synthetic_scoring_raw_metadata()
    }
  )

  dict <- dict_generate("SV_SYNTHETIC", variable_name = "question_name")
  scoring_rows <- dict[dict$row_source == "scoring", ]

  expect_identical(
    scoring_rows$response_column_id,
    c("Total Score", "Q1")
  )
  expect_identical(scoring_rows$row_source, rep("scoring", 2))
  expect_true(all(is.na(scoring_rows$qid)))
  expect_true(all(is.na(scoring_rows$question_name)))
  expect_true(all(is.na(scoring_rows$block)))
  expect_identical(
    scoring_rows$variable_name,
    c("Total_Score", "Q1.2")
  )
  expect_identical(
    scoring_rows$question,
    c("Scoring Variable: Total Score", "Scoring Variable: Q1")
  )
  expect_true(all(is.na(scoring_rows$level)))
  expect_true(all(is.na(scoring_rows$label)))

  findings <- dict_validate(dict)$validation_findings
  scoring_findings <- findings[
    findings$response_column_id %in% scoring_rows$response_column_id,
  ]
  expect_identical(
    scoring_findings$finding,
    c("repaired_variable_name", "repaired_variable_name")
  )
  expect_identical(
    scoring_findings$reason,
    c("unsafe", "duplicate")
  )
})

test_that("dict_generate represents nested Scoring Categories", {
  local_mocked_bindings(
    fetch_dictionary_metadata = function(surveyID) {
      synthetic_nested_scoring_raw_metadata()
    }
  )

  dict <- dict_generate("SV_SYNTHETIC", variable_name = "question_name")
  scoring_rows <- dict[dict$row_source == "scoring", ]

  expect_identical(
    scoring_rows$response_column_id,
    c("SC_TOTAL", "SC_SCREEN")
  )
  expect_identical(
    scoring_rows$variable_name,
    c("Total_Score", "SC_SCREEN")
  )
  expect_true(all(is.na(scoring_rows$qid)))
  expect_true(all(is.na(scoring_rows$block)))
})

test_that("dict_generate assigns Embedded Data Fields to previous blocks", {
  local_mocked_bindings(
    fetch_dictionary_metadata = function(surveyID) {
      synthetic_survey_flow_embedded_data_raw_metadata()
    }
  )

  dict <- dict_generate(
    "SV_SYNTHETIC",
    variable_name = "question_name",
    embedded_data_block_assignment = "previous"
  )
  embedded_rows <- dict[dict$row_source == "embedded_data", ]

  expect_identical(
    embedded_rows$response_column_id,
    c("Before Main", "Between Blocks", "After Follow-up")
  )
  expect_identical(
    embedded_rows$block,
    c(NA_character_, "Main Block", "Follow-up Block")
  )
})

test_that("dict_generate defaults flow Embedded Data Fields to no block", {
  local_mocked_bindings(
    fetch_dictionary_metadata = function(surveyID) {
      synthetic_survey_flow_embedded_data_raw_metadata()
    }
  )

  dict <- dict_generate("SV_SYNTHETIC", variable_name = "question_name")
  embedded_rows <- dict[dict$row_source == "embedded_data", ]

  expect_true(all(is.na(embedded_rows$block)))
})

test_that("dict_generate assigns Embedded Data Fields to next blocks", {
  local_mocked_bindings(
    fetch_dictionary_metadata = function(surveyID) {
      synthetic_survey_flow_embedded_data_raw_metadata()
    }
  )

  dict <- dict_generate(
    "SV_SYNTHETIC",
    variable_name = "question_name",
    embedded_data_block_assignment = "next"
  )
  embedded_rows <- dict[dict$row_source == "embedded_data", ]

  expect_identical(
    embedded_rows$block,
    c("Main Block", "Follow-up Block", NA_character_)
  )
})

test_that("dict_generate assigns nested Embedded Data Fields", {
  local_mocked_bindings(
    fetch_dictionary_metadata = function(surveyID) {
      synthetic_nested_survey_flow_embedded_data_raw_metadata()
    }
  )

  dict <- dict_generate(
    "SV_SYNTHETIC",
    variable_name = "question_name",
    embedded_data_block_assignment = "previous"
  )
  embedded_rows <- dict[dict$row_source == "embedded_data", ]

  expect_identical(
    embedded_rows$block,
    c(NA_character_, "Main Block", "Follow-up Block")
  )
})

test_that("dict_generate leaves flat Embedded Data Fields unassigned", {
  local_mocked_bindings(
    fetch_dictionary_metadata = function(surveyID) {
      synthetic_flat_embedded_data_raw_metadata()
    }
  )

  dict <- dict_generate(
    "SV_SYNTHETIC",
    variable_name = "question_name",
    embedded_data_block_assignment = "previous"
  )
  embedded_rows <- dict[dict$row_source == "embedded_data", ]

  expect_true(all(is.na(embedded_rows$block)))
})

test_that("dict_generate warns once for partial Embedded Data assignment", {
  local_mocked_bindings(
    fetch_dictionary_metadata = function(surveyID) {
      synthetic_survey_flow_embedded_data_raw_metadata()
    }
  )

  expect_warning(
    dict <- dict_generate(
      "SV_SYNTHETIC",
      variable_name = "question_name",
      embedded_data_block_assignment = "previous",
      quiet = FALSE
    ),
    "Some Embedded Data Fields could not be assigned"
  )
  embedded_rows <- dict[dict$row_source == "embedded_data", ]

  expect_identical(
    embedded_rows$block,
    c(NA_character_, "Main Block", "Follow-up Block")
  )
  expect_silent(
    dict_generate(
      "SV_SYNTHETIC",
      variable_name = "question_name",
      embedded_data_block_assignment = "previous",
      quiet = TRUE
    )
  )
})

test_that("dict_generate leaves ambiguous Embedded Data Fields unassigned", {
  local_mocked_bindings(
    fetch_dictionary_metadata = function(surveyID) {
      synthetic_ambiguous_survey_flow_embedded_data_raw_metadata()
    }
  )

  dict <- suppressWarnings(dict_generate(
    "SV_SYNTHETIC",
    variable_name = "question_name",
    embedded_data_block_assignment = "previous",
    quiet = FALSE
  ))
  embedded_rows <- dict[dict$row_source == "embedded_data", ]

  expect_identical(embedded_rows$response_column_id, "Duplicated Field")
  expect_true(is.na(embedded_rows$block))
})

test_that("dict_validate reports repaired Embedded Data Field names", {
  local_mocked_bindings(
    fetch_dictionary_metadata = function(surveyID) {
      synthetic_flat_embedded_data_raw_metadata()
    }
  )

  dict <- dict_generate("SV_SYNTHETIC", variable_name = "question_name")
  findings <- dict_validate(dict)$validation_findings
  embedded_findings <- findings[
    findings$response_column_id %in% c("Source Channel", "Q1"),
  ]

  expect_identical(
    embedded_findings$finding,
    c("repaired_variable_name", "repaired_variable_name")
  )
  expect_identical(
    embedded_findings$response_column_id,
    c("Source Channel", "Q1")
  )
  expect_identical(
    embedded_findings$reason,
    c("unsafe", "duplicate")
  )
})
