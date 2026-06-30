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

test_that("dict_generate represents response column-map sidecars", {
  local_mocked_bindings(
    fetch_dictionary_metadata = function(surveyID) {
      raw_metadata <- synthetic_flat_embedded_data_raw_metadata()
      raw_metadata$description$scoring <- list(
        ScoringCategories = list(
          list(ID = "SC_TOTAL", Name = "Total Score"),
          list(ID = "SC_HIDDEN", Name = "Hidden Score")
        )
      )
      raw_metadata$response_column_map <- tibble::tibble(
        ImportId = c(
          "QID1",
          "Source Channel",
          "SC_TOTAL",
          "QID1_3_TEXT_SENTIMENT",
          "QID1_3_e476cefa310845248231594eParTopics"
        ),
        description = c(
          "Choose one",
          "Source Channel",
          "Total Score",
          "Q1 Other - Sentiment",
          "Q1 Other - Parent Topics"
        ),
        main = c(
          "Choose one",
          "Source Channel",
          "Total Score",
          "Q1 Other",
          "Q1 Other"
        ),
        sub = c("", "", "", "Sentiment", "Parent Topics")
      )
      raw_metadata
    }
  )

  dict <- dict_generate("SV_SYNTHETIC", variable_name = "question_name")
  represented_rows <- dict[
    dict$row_source %in% c("embedded_data", "scoring", "text_analysis"),
  ]

  expect_setequal(
    represented_rows$row_source,
    c("embedded_data", "scoring", "text_analysis")
  )
  expect_true("Source Channel" %in% represented_rows$response_column_id)
  expect_true("SC_TOTAL" %in% represented_rows$response_column_id)
  expect_false("SC_HIDDEN" %in% represented_rows$response_column_id)
  expect_true(
    "QID1_3_TEXT_SENTIMENT" %in% represented_rows$response_column_id
  )
  expect_true(
    "QID1_3_e476cefa310845248231594eParTopics" %in%
      represented_rows$response_column_id
  )
})

test_that("dict_generate uses column-map classification for sidecars", {
  local_mocked_bindings(
    fetch_dictionary_metadata = function(surveyID) {
      synthetic_column_map_sidecar_raw_metadata()
    }
  )

  dict <- dict_generate("SV_SYNTHETIC", variable_name = "question_name")
  sidecar_rows <- dict[dict$row_source == "text_analysis", ]
  expected_sidecars <- c(
    glad_sa6_text_analysis_sidecar_ids(),
    edgi_signup_text_analysis_sidecar_ids()
  )

  expect_setequal(sidecar_rows$response_column_id, expected_sidecars)
  expect_identical(nrow(sidecar_rows), 15L)
  expect_true(all(sidecar_rows$qid %in% c("QID694", "QID121")))
  expect_false("QID508_TEXT" %in% sidecar_rows$response_column_id)
  expect_false("QID626_TEXT" %in% sidecar_rows$response_column_id)
  expect_false("QID429_TEXT" %in% sidecar_rows$response_column_id)
  expect_false("QID700_1" %in% sidecar_rows$response_column_id)
  expect_false("QID121_1" %in% sidecar_rows$response_column_id)

  ordinary_ids <- c(
    "QID508_TEXT",
    "QID626_TEXT",
    "QID429_TEXT",
    "QID700_1",
    "QID121_1"
  )
  ordinary_rows <- dict[dict$response_column_id %in% ordinary_ids, ]
  expect_true(all(ordinary_rows$row_source == "question"))

  metadata_rows <- dict[
    dict$row_source %in% c("embedded_data", "scoring"),
  ]
  expect_true("Source Channel" %in% metadata_rows$response_column_id)
  expect_true("SC_TOTAL" %in% metadata_rows$response_column_id)
  expect_false("Cohort" %in% metadata_rows$response_column_id)
  expect_false("SC_HIDDEN" %in% metadata_rows$response_column_id)
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
    c("SC_TOTAL", "SC_Q1")
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

test_that("dict_generate represents Text-analysis Sidecars", {
  local_mocked_bindings(
    fetch_dictionary_metadata = function(surveyID) {
      synthetic_text_analysis_raw_metadata()
    }
  )

  dict <- dict_generate("SV_SYNTHETIC", variable_name = "question_name")
  sidecar_rows <- dict[dict$row_source == "text_analysis", ]

  expect_identical(
    sidecar_rows$response_column_id,
    c(
      "QID1_3_TEXT_SENTIMENT",
      "QID1_3_e476cefa310845248231594eParTopics"
    )
  )
  expect_identical(sidecar_rows$row_source, rep("text_analysis", 2))
  expect_identical(sidecar_rows$qid, c("QID1", "QID1"))
  expect_identical(sidecar_rows$question_name, c("Q1", "Q1"))
  expect_identical(
    sidecar_rows$block,
    c("Main Block", "Main Block")
  )
  expect_identical(
    sidecar_rows$variable_name,
    c("Q1_Other_Sentiment", "Q1_Other_Parent_Topics")
  )
  expect_identical(
    sidecar_rows$question,
    c(
      "Text Analysis: Q1 Other - Sentiment",
      "Text Analysis: Q1 Other - Parent Topics"
    )
  )
  expect_true(all(is.na(sidecar_rows$level)))
  expect_true(all(is.na(sidecar_rows$label)))

  findings <- attr(dict, "variable_name_findings", exact = TRUE)
  sidecar_findings <- findings[
    findings$response_column_id %in% sidecar_rows$response_column_id,
  ]
  expect_identical(
    sidecar_findings$response_column_id,
    c(
      "QID1_3_TEXT_SENTIMENT",
      "QID1_3_e476cefa310845248231594eParTopics"
    )
  )
  expect_identical(
    unname(sidecar_findings$reason),
    c("unsafe", "unsafe")
  )

  validation_findings <- dict_validate(dict)$validation_findings
  repaired_findings <- validation_findings[
    validation_findings$finding == "repaired_variable_name" &
      validation_findings$response_column_id %in%
        sidecar_rows$response_column_id,
  ]
  expect_identical(
    repaired_findings$response_column_id,
    c(
      "QID1_3_TEXT_SENTIMENT",
      "QID1_3_e476cefa310845248231594eParTopics"
    )
  )
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

test_that("normalised metadata renders the current Variable Dictionary rows", {
  raw_metadata <- synthetic_mc_text_raw_metadata()
  normalised_metadata <- normalise_qualtrics_metadata(raw_metadata)

  dict <- variable_dictionary_from_normalised_metadata(
    normalised_metadata,
    use_semantic_name = FALSE,
    block_pattern = NULL,
    block_sep = ".",
    semantic_name_preprocess = NULL
  )

  dict_subset <- dict[c(
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
  )]

  expect_identical(
    dict_subset$response_column_id,
    c("QID1", "QID1", "QID1", "QID1_3_TEXT")
  )
  expect_identical(dict_subset$row_source, rep("question", 4))
  expect_identical(dict_subset$qid, rep("QID1", 4))
  expect_identical(
    dict_subset$question_name,
    c("Q1", "Q1", "Q1", "Q1")
  )
  expect_identical(
    dict_subset$variable_name,
    c("Q1", "Q1", "Q1", "Q1.1")
  )
  expect_identical(dict_subset$block, rep("Main Block", 4))
  expect_identical(dict_subset$question, rep("Choose one", 4))
  expect_true(all(is.na(dict_subset$item)))
  expect_identical(unname(dict_subset$level), c("1", "2", "3", "3_TEXT"))
  expect_identical(
    unname(dict_subset$label),
    c("Yes", "No", "Other", "Other_TEXT")
  )
  expect_identical(dict_subset$type, rep("MC", 4))
  expect_identical(dict_subset$selector, rep("SAVR", 4))
  expect_identical(dict_subset$sub_selector, rep("TX", 4))
  expect_identical(dict_subset$content_type, rep("Number", 4))
  expect_identical(attr(dict, "survey_name"), "Synthetic Survey")
  expect_identical(attr(dict, "surveyID"), "SV_SYNTHETIC")
})

test_that("question-name Variable Dictionaries repair only variable_name", {
  raw_metadata <- synthetic_mc_text_raw_metadata()
  raw_metadata$metadata$questions$QID1$questionName <- "1 Bad Name"
  raw_metadata$metadata$questions$QID2 <- raw_metadata$metadata$questions$QID1
  raw_metadata$metadata$questions$QID2$questionName <- "1 Bad Name"
  raw_metadata$description$blocks$BL_1$BlockElements <- list(
    list(QuestionID = "QID1"),
    list(QuestionID = "QID2")
  )
  raw_metadata$description$questions$QID2 <-
    raw_metadata$description$questions$QID1
  normalised_metadata <- normalise_qualtrics_metadata(raw_metadata)

  dict <- variable_dictionary_from_normalised_metadata(
    normalised_metadata,
    use_semantic_name = FALSE,
    block_pattern = NULL,
    block_sep = ".",
    semantic_name_preprocess = NULL
  )
  attr(dict, "class") <- c("qualtdict", class(dict))
  findings <- dict_validate(dict)$validation_findings
  findings <- findings[findings$finding == "repaired_variable_name", ]

  first_rows <- !duplicated(dict$response_column_id)

  expect_identical(
    dict$question_name[first_rows],
    c("1 Bad Name", "1 Bad Name", "1 Bad Name", "1 Bad Name")
  )
  expect_identical(
    dict$variable_name[first_rows],
    c("X1_Bad_Name", "X1_Bad_Name.1", "X1_Bad_Name.2", "X1_Bad_Name.3")
  )
  expect_named(
    findings,
    c(
      "finding",
      "response_column_id",
      "variable_name",
      "original_candidate",
      "reason",
      "item_name",
      "mistake",
      "label",
      "level"
    )
  )
  expect_identical(
    findings$finding,
    rep("repaired_variable_name", 4)
  )
  expect_identical(
    findings$response_column_id,
    c("QID1", "QID1_3_TEXT", "QID2", "QID2_3_TEXT")
  )
  expect_identical(
    findings$original_candidate,
    c("1 Bad Name", "1 Bad Name", "1 Bad Name", "1 Bad Name")
  )
  expect_identical(findings$variable_name, dict$variable_name[first_rows])
  expect_identical(
    findings$reason,
    c("unsafe", "unsafe;duplicate", "unsafe;duplicate", "unsafe;duplicate")
  )
})
