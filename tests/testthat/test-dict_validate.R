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
  expect_equal(nrow(validation$validation_findings), 0)
  expect_s3_class(validation$level_label_pairs, "data.frame")
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

  expect_equal(
    duplicate_findings$response_column_id,
    c("QID2", "QID3")
  )
  expect_equal(
    duplicate_findings$reason,
    c("variable_name_not_unique", "variable_name_not_unique")
  )
  expect_equal(unsafe_findings$response_column_id, "QID1")
  expect_equal(unsafe_findings$variable_name, "bad name")
  expect_equal(unsafe_findings$reason, "unsafe")
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

  expect_equal(findings$finding, "repaired_variable_name")
  expect_equal(findings$response_column_id, "QID1")
  expect_equal(findings$original_candidate, "bad name")
  expect_equal(findings$variable_name, "bad_name")
  expect_equal(findings$reason, "unsafe")
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

  expect_equal(nrow(mistake_findings), 3)
  expect_equal(unique(mistake_findings$response_column_id), "QID1")
  expect_equal(unique(mistake_findings$variable_name), "q1")
  expect_equal(unique(mistake_findings$mistake), "234")
  expect_equal(mistake_findings$label, c("A", "A", "B"))
  expect_equal(mistake_findings$level, c("1", "1", "3"))
})

vcr::use_cassette("dict_generate", {
  suppressWarnings(
    x <- dict_generate("SV_0AQg1pFepA0V2d0", variable_name = "question_name")
  )
})

test_that("dict_validate exposes recorded level-label findings", {
  x_validate <- dict_validate(x)
  expect_s3_class(x_validate, "qualtdict_validation")
  expect_true("level_label_pairs" %in% names(x_validate))
  expect_true("validation_findings" %in% names(x_validate))
  expect_true(any(
    x_validate$validation_findings$finding == "level_label_mistake"
  ))
  expect_true("response_column_id" %in% names(x_validate$validation_findings))
})
