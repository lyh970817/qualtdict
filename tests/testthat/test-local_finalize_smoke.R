source(testthat::test_path(
  "..",
  "..",
  "tools",
  "local-finalize-smoke-lib.R"
))

test_that("parse_smoke_functions defaults to all smoke-covered functions", {
  expect_identical(
    parse_smoke_functions(NULL),
    c(
      "dict_generate",
      "dict_validate",
      "get_survey_data",
      "labelled_export_findings",
      "dict_split_blocks",
      "survey_split_blocks"
    )
  )
})

test_that("parse_smoke_functions trims whitespace and removes duplicates", {
  expect_identical(
    parse_smoke_functions(" get_survey_data, dict_split_blocks,get_survey_data "),
    c("get_survey_data", "dict_split_blocks")
  )
})

test_that("parse_smoke_functions rejects empty selections", {
  expect_error(
    parse_smoke_functions(" , "),
    "Select at least one smoke-covered exported function"
  )
})

test_that("parse_smoke_functions rejects unknown selections", {
  expect_error(
    parse_smoke_functions("get_survey_data,unknown_function"),
    "Unknown smoke-covered exported function"
  )
})

test_that("smoke_summary_names maps functions to summary object names", {
  expect_identical(
    smoke_summary_names(c("dict_generate", "get_survey_data")),
    c("dict", "labelled", "labelled_excluding_validation")
  )
  expect_identical(
    smoke_summary_names(c("dict_split_blocks", "survey_split_blocks")),
    c("dict_blocks", "survey_blocks")
  )
})

test_that("project_smoke_record keeps only selected summaries and hashes", {
  record <- list(
    alias = "survey_a",
    survey_id = "SV_123",
    variable_name = "question_name",
    generated_at = "2026-06-26T00:00:00Z",
    summaries = list(
      dict = list(object_hash = "dict-hash"),
      validation = list(object_hash = "validation-hash"),
      labelled = list(object_hash = "labelled-hash"),
      labelled_excluding_validation = list(object_hash = "labelled-ex-hash"),
      dict_blocks = list(object_hash = "dict-blocks-hash")
    ),
    object_hashes = list(
      dict = "dict-hash",
      validation = "validation-hash",
      labelled = "labelled-hash",
      labelled_excluding_validation = "labelled-ex-hash",
      dict_blocks = "dict-blocks-hash"
    ),
    scenario_hash = "full-hash"
  )

  projected <- project_smoke_record(
    record,
    c("get_survey_data", "dict_split_blocks")
  )

  expect_identical(
    names(projected$summaries),
    c("labelled", "labelled_excluding_validation", "dict_blocks")
  )
  expect_identical(projected$object_hashes, list(
    labelled = "labelled-hash",
    labelled_excluding_validation = "labelled-ex-hash",
    dict_blocks = "dict-blocks-hash"
  ))
  expect_match(projected$scenario_hash, "^[0-9a-f]{32}$")
  expect_false(identical(projected$scenario_hash, "full-hash"))
})

test_that("project_smoke_record omits unavailable optional summaries", {
  record <- list(
    alias = "survey_a",
    survey_id = "SV_123",
    variable_name = "semantic_name",
    generated_at = "2026-06-26T00:00:00Z",
    summaries = list(labelled = list(object_hash = "labelled-hash")),
    object_hashes = list(labelled = "labelled-hash"),
    scenario_hash = "full-hash"
  )

  projected <- project_smoke_record(record, "get_survey_data")

  expect_identical(names(projected$summaries), "labelled")
  expect_identical(projected$object_hashes, list(labelled = "labelled-hash"))
})

test_that("merge_smoke_baseline updates only selected summaries and hashes", {
  existing <- list(
    alias = "survey_a",
    survey_id = "SV_123",
    variable_name = "question_name",
    generated_at = "old-time",
    summaries = list(
      dict = list(object_hash = "old-dict"),
      labelled = list(object_hash = "old-labelled")
    ),
    object_hashes = list(
      dict = "old-dict",
      labelled = "old-labelled"
    ),
    scenario_hash = "old-scenario"
  )
  selected <- list(
    alias = "survey_a",
    survey_id = "SV_123",
    variable_name = "question_name",
    generated_at = "new-time",
    summaries = list(labelled = list(object_hash = "new-labelled")),
    object_hashes = list(labelled = "new-labelled"),
    scenario_hash = "selected-scenario"
  )

  merged <- merge_smoke_baseline(existing, selected)

  expect_identical(merged$summaries$dict, list(object_hash = "old-dict"))
  expect_identical(merged$summaries$labelled, list(object_hash = "new-labelled"))
  expect_identical(merged$object_hashes, list(
    dict = "old-dict",
    labelled = "new-labelled"
  ))
  expect_match(merged$scenario_hash, "^[0-9a-f]{32}$")
  expect_false(identical(merged$scenario_hash, "old-scenario"))
  expect_identical(merged$generated_at, "new-time")
})
