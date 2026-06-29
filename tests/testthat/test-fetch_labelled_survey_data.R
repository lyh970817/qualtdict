minimal_export_dict <- function(
  response_column_id = c("QID1", "QID2"),
  variable_name = c("q1", "q2"),
  block = c("Block A", "Block B"),
  label = c("Yes", "No"),
  level = c("1", "2")
) {
  dict <- tibble::tibble(
    response_column_id = response_column_id,
    row_source = "question",
    qid = sub("_.*$", "", response_column_id),
    question_name = variable_name,
    variable_name = variable_name,
    block = block,
    question = paste("Question", variable_name),
    item = NA_character_,
    level = level,
    label = label,
    type = "MC",
    selector = "SAVR",
    sub_selector = "TX",
    content_type = NA_character_
  )
  attr(dict, "class") <- c("qualtdict", class(dict))
  attr(dict, "surveyID") <- "SV_TEST"
  attr(dict, "survey_name") <- "Test Survey"
  dict
}

minimal_survey_data <- function() {
  tibble::tibble(
    externalDataReference = "R_1",
    startDate = "2026-06-01",
    endDate = "2026-06-01",
    QID1 = "1",
    QID2 = "2"
  )
}

test_that("fetch_labelled_survey_data returns one labelled data frame", {
  captured_args <- new.env(parent = emptyenv())
  local_mocked_bindings(
    fetch_survey2 = function(...) {
      captured_args$value <- list(...)
      minimal_survey_data()
    }
  )

  dat <- fetch_labelled_survey_data(
    minimal_export_dict(),
    include_questions = "QID1"
  )

  expect_s3_class(dat, "data.frame")
  expect_named(
    dat,
    c(
      "externalDataReference",
      "startDate",
      "endDate",
      "q1",
      "q2"
    )
  )
  expect_s3_class(attr(dat, "dict"), "qualtdict")
  expect_identical(captured_args$value$include_questions, "QID1")
  expect_identical(captured_args$value$surveyID, "SV_TEST")
  expect_true(captured_args$value$import_id)
  expect_false(captured_args$value$convert)
  expect_false(captured_args$value$label)
  expect_true(captured_args$value$breakout_sets)
})

test_that("fetch_labelled_survey_data reports missing Response Column IDs", {
  local_mocked_bindings(
    fetch_survey2 = function(...) {
      tibble::tibble(
        externalDataReference = "R_1",
        startDate = "2026-06-01",
        endDate = "2026-06-01",
        QID1 = "1"
      )
    }
  )

  dat <- fetch_labelled_survey_data(minimal_export_dict())
  findings <- labelled_export_findings(dat)

  expect_named(
    dat,
    c(
      "externalDataReference",
      "startDate",
      "endDate",
      "q1"
    )
  )
  expect_named(
    findings,
    c(
      "finding",
      "response_column_id",
      "qid",
      "variable_name",
      "reason"
    )
  )
  expect_identical(nrow(findings), 1L)
  expect_identical(findings$finding, "missing_response_column_id")
  expect_identical(findings$response_column_id, "QID2")
  expect_identical(findings$qid, "QID2")
  expect_identical(findings$variable_name, "q2")
  expect_identical(findings$reason, "not_found_in_downloaded_survey_data")
})

test_that("exclude_findings removes validation findings after download", {
  captured_args <- new.env(parent = emptyenv())
  captured_quiet <- new.env(parent = emptyenv())
  local_mocked_bindings(
    fetch_survey2 = function(...) {
      captured_args$value <- list(...)
      minimal_survey_data()
    },
    dict_validate = function(dict, quiet = TRUE) {
      captured_quiet$value <- quiet
      list(
        validation_findings = tibble::tibble(
          finding = "level_label_mistake",
          response_column_id = "QID2"
        )
      )
    }
  )

  dat <- fetch_labelled_survey_data(
    minimal_export_dict(),
    exclude_findings = "validation",
    include_questions = c("QID1", "QID2")
  )

  expect_identical(captured_args$value$include_questions, c("QID1", "QID2"))
  expect_true(captured_quiet$value)
  expect_named(
    dat,
    c(
      "externalDataReference",
      "startDate",
      "endDate",
      "q1"
    )
  )
  expect_identical(attr(dat, "dict")$response_column_id, "QID1")
})

test_that("fetch_labelled_survey_data forwards quiet to validation exclusion", {
  captured_quiet <- new.env(parent = emptyenv())
  local_mocked_bindings(
    fetch_survey2 = function(...) {
      minimal_survey_data()
    },
    dict_validate = function(dict, quiet = TRUE) {
      captured_quiet$value <- quiet
      list(validation_findings = tibble::tibble(response_column_id = "QID2"))
    }
  )

  fetch_labelled_survey_data(
    minimal_export_dict(),
    exclude_findings = "validation",
    quiet = FALSE
  )

  expect_false(captured_quiet$value)
})

test_that("fetch_labelled_survey_data omits quiet from fetch_survey", {
  captured_args <- new.env(parent = emptyenv())
  local_mocked_bindings(
    fetch_survey2 = function(...) {
      captured_args$value <- list(...)
      minimal_survey_data()
    }
  )

  fetch_labelled_survey_data(
    minimal_export_dict(),
    NULL,
    "none",
    "positional_fetch_arg",
    quiet = FALSE
  )

  expect_identical(captured_args$value[[1]], "positional_fetch_arg")
  expect_null(captured_args$value$quiet)
})

test_that("extra_columns distinguish user-specified columns from defaults", {
  dict <- minimal_export_dict(
    response_column_id = "QID1",
    variable_name = "q1",
    block = "Block A",
    label = "Yes",
    level = "1"
  )

  local_mocked_bindings(
    fetch_survey2 = function(...) {
      tibble::tibble(QID1 = "1")
    }
  )

  expect_warning(
    dat <- fetch_labelled_survey_data(dict),
    "Missing default `extra_columns`"
  )
  expect_named(dat, "q1")

  expect_error(
    fetch_labelled_survey_data(dict, extra_columns = "IPAddress"),
    "Missing user-specified `extra_columns`"
  )
})

test_that("users cannot override qualtdict-owned fetch settings", {
  expect_error(
    fetch_labelled_survey_data(minimal_export_dict(), import_id = FALSE),
    "owned by qualtdict"
  )
  expect_error(
    fetch_labelled_survey_data(minimal_export_dict(), breakout_sets = FALSE),
    "owned by qualtdict"
  )
})

test_that("dict_split_blocks returns block-specific Variable Dictionaries", {
  dict <- minimal_export_dict()
  attr(dict, "variable_name_findings") <- tibble::tibble(
    response_column_id = "QID2",
    original_candidate = "q2",
    variable_name = "q2",
    reason = "duplicate"
  )

  block_dicts <- dict_split_blocks(dict)

  expect_named(block_dicts, c("Block A", "Block B"))
  expect_s3_class(block_dicts[["Block A"]], "qualtdict")
  expect_identical(block_dicts[["Block A"]]$response_column_id, "QID1")
  expect_identical(block_dicts[["Block B"]]$response_column_id, "QID2")
  expect_identical(
    nrow(attr(block_dicts[["Block A"]], "variable_name_findings")),
    0L
  )
  expect_identical(
    attr(
      block_dicts[["Block B"]],
      "variable_name_findings"
    )$response_column_id,
    "QID2"
  )
})

test_that("dict_split_blocks preserves unassigned Variable Dictionary rows", {
  dict <- minimal_export_dict(
    response_column_id = c("QID1", "ED1"),
    variable_name = c("q1", "embedded_field"),
    block = c("Block A", NA_character_),
    label = c("Yes", NA_character_),
    level = c("1", NA_character_)
  )
  attr(dict, "variable_name_findings") <- tibble::tibble(
    response_column_id = "ED1",
    original_candidate = "Embedded Field",
    variable_name = "embedded_field",
    reason = "unsafe"
  )

  block_dicts <- dict_split_blocks(dict)

  expect_named(block_dicts, c("..unassigned", "Block A"))
  expect_s3_class(block_dicts[["..unassigned"]], "qualtdict")
  expect_identical(
    attr(block_dicts[["..unassigned"]], "surveyID", exact = TRUE),
    "SV_TEST"
  )
  expect_identical(
    block_dicts[["..unassigned"]]$response_column_id,
    "ED1"
  )
  expect_identical(
    attr(
      block_dicts[["..unassigned"]],
      "variable_name_findings"
    )$response_column_id,
    "ED1"
  )
  expect_identical(
    nrow(attr(block_dicts[["Block A"]], "variable_name_findings")),
    0L
  )
})

test_that("dict_split_blocks keeps unassigned rows separate from block names", {
  dict <- minimal_export_dict(
    response_column_id = c("QID1", "ED1"),
    variable_name = c("q1", "embedded_field"),
    block = c("..unassigned", NA_character_),
    label = c("Yes", NA_character_),
    level = c("1", NA_character_)
  )

  block_dicts <- dict_split_blocks(dict)

  expect_named(block_dicts, c("..unassigned", "..unassigned"))
  expect_identical(block_dicts[[1]]$response_column_id, "ED1")
  expect_identical(block_dicts[[2]]$response_column_id, "QID1")
})

test_that("survey_split_blocks returns block-specific Labelled Survey Data", {
  dat <- tibble::tibble(
    externalDataReference = "R_1",
    startDate = "2026-06-01",
    endDate = "2026-06-01",
    q1 = "1",
    q2 = "2"
  )
  attr(dat, "dict") <- minimal_export_dict()

  block_data <- survey_split_blocks(dat)

  expect_named(block_data, c("Block A", "Block B"))
  expect_named(
    block_data[["Block A"]],
    c(
      "externalDataReference",
      "startDate",
      "endDate",
      "q1"
    )
  )
  expect_named(
    block_data[["Block B"]],
    c(
      "externalDataReference",
      "startDate",
      "endDate",
      "q2"
    )
  )
  expect_s3_class(attr(block_data[["Block A"]], "dict"), "qualtdict")
})

test_that("survey_split_blocks preserves unassigned Export Variables", {
  dict <- minimal_export_dict(
    response_column_id = c("QID1", "ED1"),
    variable_name = c("q1", "embedded_field"),
    block = c("Block A", NA_character_),
    label = c("Yes", NA_character_),
    level = c("1", NA_character_)
  )
  dat <- tibble::tibble(
    IPAddress = "127.0.0.1",
    q1 = "1",
    embedded_field = "wave_1"
  )
  attr(dat, "dict") <- dict

  block_data <- survey_split_blocks(dat, extra_columns = "IPAddress")

  expect_named(block_data, c("..unassigned", "Block A"))
  expect_named(block_data[["..unassigned"]], c("IPAddress", "embedded_field"))
  expect_named(block_data[["Block A"]], c("IPAddress", "q1"))
  expect_s3_class(attr(block_data[["..unassigned"]], "dict"), "qualtdict")
  expect_identical(
    attr(block_data[["..unassigned"]], "dict")$response_column_id,
    "ED1"
  )
})
