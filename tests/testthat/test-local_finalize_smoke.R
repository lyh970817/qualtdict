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
