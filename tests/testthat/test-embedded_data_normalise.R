test_that("flat Embedded Data Fields normalise into package-owned metadata", {
  raw_metadata <- synthetic_flat_embedded_data_raw_metadata()

  normalised_metadata <- normalise_qualtrics_metadata(raw_metadata)
  embedded_data <- normalised_metadata$embedded_data

  expect_s3_class(
    embedded_data,
    "qualtdict_normalised_embedded_data_fields"
  )
  expect_named(embedded_data, c("Source Channel", "Q1"))
  expect_s3_class(
    embedded_data[["Source Channel"]],
    "qualtdict_normalised_embedded_data_field"
  )
  expect_identical(
    embedded_data[["Source Channel"]]$response_column_id,
    "Source Channel"
  )
  expect_identical(
    embedded_data[["Source Channel"]]$question_text,
    "Embedded Data: Source Channel"
  )
})

test_that("description Survey Flow Embedded Data Fields add block candidates", {
  raw_metadata <- synthetic_survey_flow_embedded_data_raw_metadata()

  embedded_data <- normalise_survey_flow_embedded_data_fields(
    raw_metadata$description
  )

  expect_named(
    embedded_data,
    c("Before Main", "Between Blocks", "After Follow-up")
  )
  expect_identical(
    embedded_data[["Before Main"]]$previous_block,
    NA_character_
  )
  expect_identical(
    embedded_data[["Before Main"]]$next_block,
    "Main Block"
  )
  expect_identical(
    embedded_data[["Between Blocks"]]$previous_block,
    "Main Block"
  )
  expect_identical(
    embedded_data[["Between Blocks"]]$next_block,
    "Follow-up Block"
  )
})

test_that("nested Survey Flow Embedded Data Fields normalise with candidates", {
  raw_metadata <- synthetic_nested_survey_flow_embedded_data_raw_metadata()

  embedded_data <- normalise_survey_flow_embedded_data_fields(
    raw_metadata$description
  )

  expect_named(
    embedded_data,
    c("Before Main", "Between Blocks", "After Follow-up")
  )
  expect_identical(
    embedded_data[["Between Blocks"]]$previous_block,
    "Main Block"
  )
  expect_identical(
    embedded_data[["Between Blocks"]]$next_block,
    "Follow-up Block"
  )
})

test_that("metadata flow does not locate Embedded Data Fields", {
  raw_metadata <- synthetic_mc_text_raw_metadata()
  raw_metadata$metadata$flow <- list(
    Flow = list(
      list(type = "EmbeddedData"),
      list(Type = "EmbeddedData", Field = "Ignored Flow Field")
    )
  )
  raw_metadata$description$flow <- list()

  expect_length(
    normalise_qualtrics_metadata(raw_metadata)$embedded_data,
    0
  )
})

test_that("description Survey Flow locations merge only onto flat fields", {
  raw_metadata <- synthetic_survey_flow_embedded_data_raw_metadata()
  raw_metadata$metadata$embedded_data <- embedded_data_records(
    c("Before Main", "Flat Only")
  )

  embedded_data <- normalise_qualtrics_metadata(raw_metadata)$embedded_data

  expect_named(embedded_data, c("Before Main", "Flat Only"))
  expect_identical(
    embedded_data[["Before Main"]]$next_block,
    "Main Block"
  )
  expect_null(embedded_data[["Flat Only"]]$previous_block)
  expect_null(embedded_data[["Flat Only"]]$next_block)
})

test_that("description Survey Flow Embedded Data skips missing block lookups", {
  raw_metadata <- synthetic_survey_flow_embedded_data_raw_metadata()
  raw_metadata$description$blocks <- NULL

  embedded_data <- normalise_survey_flow_embedded_data_fields(
    raw_metadata$description
  )

  expect_named(
    embedded_data,
    c("Before Main", "Between Blocks", "After Follow-up")
  )
  expect_identical(
    embedded_data[["Between Blocks"]]$previous_block,
    NA_character_
  )
  expect_identical(
    embedded_data[["Between Blocks"]]$next_block,
    NA_character_
  )
})

test_that("Embedded Data Field names normalise from flat metadata records", {
  expect_identical(
    embedded_data_field_names(list(
      list(name = "Wave"),
      list(name = ""),
      list(name = NA_character_)
    )),
    "Wave"
  )
  expect_identical(
    embedded_data_field_names(list(
      "Standalone",
      list(fieldName = "Legacy Field"),
      list(Field = "Flow Field")
    )),
    character()
  )
  expect_identical(
    embedded_data_field_names("Standalone"),
    character()
  )
  expect_identical(
    embedded_data_flow_field_name(NULL),
    NA_character_
  )
})

test_that("description Survey Flow helpers cover empty artifact branches", {
  expect_identical(
    normalise_survey_flow_embedded_data_fields(list(
      flow = list(Type = "EmbeddedData", EmbeddedData = list())
    )),
    empty_normalised_embedded_data_fields()
  )
  expect_length(
    survey_flow_items(list(named = list(Type = "Standard", ID = "BL_1"))),
    1
  )
  expect_identical(survey_flow_item_type("Root"), NA_character_)
  expect_identical(
    survey_flow_embedded_data_field_names(list(Type = "EmbeddedData")),
    character()
  )
})
