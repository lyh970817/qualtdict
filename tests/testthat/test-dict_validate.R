minimal_validation_dict <- function(response_column_id = c("QID1", "QID1"),
                                    variable_name = c("q1", "q1"),
                                    label = c("Yes", "No"),
                                    level = c("1", "2")) {
  dict <- tibble::tibble(
    response_column_id = response_column_id,
    qid = sub("_.*$", "", response_column_id),
    question_name = variable_name,
    variable_name = variable_name,
    block = "Main Block",
    question = "Question text",
    item = NA_character_,
    level = level,
    label = label,
    type = "MC",
    selector = "SAVR",
    sub_selector = "TX",
    content_type = NA_character_
  )
  attr(dict, "class") <- c("qualtdict", class(dict))
  dict
}

test_that("dict_validate always returns a stable validation object", {
  validation <- dict_validate(minimal_validation_dict())

  expect_s3_class(validation, "qualtdict_validation")
  expect_named(validation, c("validation_findings", "level_label_pairs"))
  expect_named(validation$validation_findings, c(
    "finding",
    "response_column_id",
    "variable_name",
    "original_candidate",
    "reason",
    "item_name",
    "mistake",
    "label",
    "level"
  ))
  expect_identical(nrow(validation$validation_findings), 0L)
  expect_s3_class(validation$level_label_pairs, "data.frame")
})

test_that("dict_validate is quiet by default with opt-in progress messages", {
  dict <- minimal_validation_dict(
    response_column_id = c("QID1", "QID2", "QID3"),
    variable_name = c("bad name", "dup", "dup"),
    label = c("A", "B", "C"),
    level = c("1", "1", "1")
  )

  expect_silent(dict_validate(dict))
  progress_messages <- NULL
  capture.output(
    progress_messages <- capture.output(
      dict_validate(dict, quiet = FALSE),
      type = "message"
    )
  )
  expect_match(paste(progress_messages, collapse = "\n"), "Validating dictionary")
})

test_that("dict_validate reports progress for long validation phases", {
  dict <- minimal_validation_dict(
    response_column_id = paste0("QID", seq_len(3)),
    variable_name = paste0("q", seq_len(3)),
    label = c("A", "B", "C"),
    level = c("1", "1", "1")
  )

  expect_silent(dict_validate(dict))
  progress_messages <- NULL
  progress_output <- capture.output(
    progress_messages <- capture.output(
      dict_validate(dict, quiet = FALSE),
      type = "message"
    )
  )

  progress_messages <- paste(progress_messages, collapse = "\n")
  expect_match(progress_messages, "Validating dictionary")
  expect_match(progress_messages, "Checking level-label pairs")
  expect_match(progress_messages, "Checking level-label consistency")
  expect_match(paste(progress_output, collapse = "\n"), "100%")
})

test_that("dict_validate validates final variable_name export consistency", {
  dict <- minimal_validation_dict(
    response_column_id = c("QID1", "QID2", "QID3"),
    variable_name = c("bad name", "dup", "dup"),
    label = c("A", "B", "C"),
    level = c("1", "1", "1")
  )

  findings <- dict_validate(dict)$validation_findings

  duplicate_findings <- findings[
    findings$finding == "duplicate_variable_name",
  ]
  unsafe_findings <- findings[findings$finding == "unsafe_variable_name", ]

  expect_identical(
    duplicate_findings$response_column_id,
    c("QID2", "QID3")
  )
  expect_identical(
    duplicate_findings$reason,
    c("variable_name_not_unique", "variable_name_not_unique")
  )
  expect_identical(unsafe_findings$response_column_id, "QID1")
  expect_identical(unsafe_findings$variable_name, "bad name")
  expect_identical(unsafe_findings$reason, "unsafe")
})

test_that("dict_validate reports repaired names as Validation Findings", {
  dict <- minimal_validation_dict(
    response_column_id = "QID1",
    variable_name = "bad_name",
    label = "A",
    level = "1"
  )
  attr(dict, "variable_name_findings") <- tibble::tibble(
    response_column_id = "QID1",
    original_candidate = "bad name",
    variable_name = "bad_name",
    reason = "unsafe"
  )

  findings <- dict_validate(dict)$validation_findings

  expect_identical(findings$finding, "repaired_variable_name")
  expect_identical(findings$response_column_id, "QID1")
  expect_identical(findings$original_candidate, "bad name")
  expect_identical(findings$variable_name, "bad_name")
  expect_identical(findings$reason, "unsafe")
})

test_that("dict_validate preserves level-label mistake findings", {
  dict <- minimal_validation_dict(
    response_column_id = c("QID1", "QID1", "QID1"),
    variable_name = c("q1", "q1", "q1"),
    label = c("A", "A", "B"),
    level = c("1", "1", "3")
  )

  findings <- dict_validate(dict)$validation_findings
  mistake_findings <- findings[findings$finding == "level_label_mistake", ]

  expect_identical(nrow(mistake_findings), 3L)
  expect_identical(unique(mistake_findings$response_column_id), "QID1")
  expect_identical(unique(mistake_findings$variable_name), "q1")
  expect_identical(unique(mistake_findings$mistake), "234")
  expect_identical(mistake_findings$label, c("A", "A", "B"))
  expect_identical(mistake_findings$level, c("1", "1", "3"))
})
