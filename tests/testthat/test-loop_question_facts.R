test_that("Loop and Merge adapter expands static source question facts", {
  raw_metadata <- synthetic_loop_and_merge_raw_metadata()
  normalised_metadata <- normalise_qualtrics_metadata(raw_metadata)

  expanded <- expand_loop_question_facts(normalised_metadata$questions)
  looped <- expanded[
    vapply(expanded, function(x) isTRUE(x$looping), logical(1))
  ]

  expect_length(looped, 2)
  expect_identical(
    unname(vapply(looped, `[[`, character(1), "qid")),
    c("QID2", "QID2")
  )
  expect_identical(
    unname(vapply(looped, `[[`, character(1), "response_column_qid")),
    c("x1_QID2", "x2_QID2")
  )
  expect_identical(
    unname(vapply(looped, `[[`, character(1), "looping_option")),
    c("Apples", "Bananas")
  )
  expect_identical(
    unname(vapply(looped, `[[`, character(1), "looping_question")),
    c("Why did you choose Apples?", "Why did you choose Bananas?")
  )
})

test_that("Loop and Merge adapter expands matrix source question facts", {
  raw_metadata <- synthetic_matrix_source_looped_text_raw_metadata()
  normalised_metadata <- normalise_qualtrics_metadata(raw_metadata)

  expanded <- expand_loop_question_facts(normalised_metadata$questions)
  looped <- expanded[
    vapply(expanded, function(x) isTRUE(x$looping), logical(1))
  ]

  expect_length(looped, 3)
  expect_identical(
    unname(vapply(looped, `[[`, character(1), "response_column_qid")),
    c("1_QID2", "2_QID2", "3_QID2")
  )
  expect_identical(
    unname(vapply(looped, `[[`, character(1), "looping_option")),
    c("Condition 1", "Condition 2", "Condition 3")
  )
})

test_that("Loop and Merge adapter substitutes supported extra fields", {
  raw_metadata <- synthetic_multi_field_loop_and_merge_raw_metadata()
  normalised_metadata <- normalise_qualtrics_metadata(raw_metadata)

  expanded <- expand_loop_question_facts(normalised_metadata$questions)
  looped <- expanded[
    vapply(expanded, function(x) isTRUE(x$looping), logical(1))
  ]

  expect_length(looped, 2)
  expect_identical(
    unname(vapply(looped, `[[`, character(1), "looping_question")),
    c("Compare Apples with Red fruit", "Compare Bananas with Yellow fruit")
  )
  expect_identical(
    unname(vapply(looped, `[[`, character(1), "question_text")),
    c("Compare {} with {}", "Compare {} with {}")
  )
})

test_that("Loop and Merge adapter expands static rows without source", {
  raw_metadata <- synthetic_static_loop_and_merge_raw_metadata()
  normalised_metadata <- normalise_qualtrics_metadata(raw_metadata)

  expanded <- expand_loop_question_facts(normalised_metadata$questions)
  looped <- expanded[
    vapply(expanded, function(x) isTRUE(x$looping), logical(1))
  ]

  expect_length(looped, 2)
  expect_identical(
    unname(vapply(looped, `[[`, character(1), "response_column_qid")),
    c("1_QID2", "2_QID2")
  )
  expect_identical(
    unname(vapply(looped, `[[`, character(1), "looping_option")),
    c("Apples", "Bananas")
  )
  expect_identical(
    unname(vapply(looped, `[[`, character(1), "looping_question")),
    c("Compare Apples with Red fruit", "Compare Bananas with Yellow fruit")
  )
})

test_that("Loop and Merge options resolve direct source choice IDs", {
  choices <- list(
    a = list(recode = "3", description = "Alpha"),
    b = list(recode = "4", choiceText = "Bravo")
  )

  expect_identical(
    loop_options_from_static_choices(NULL, choices, c("a", "b")),
    c(a = "Alpha", b = "Bravo")
  )
})

test_that("Loop and Merge options resolve source choice recodes", {
  choices <- list(
    a = list(recode = "3", description = "Three"),
    b = list(recode = "4", choiceText = "Four")
  )

  expect_identical(
    loop_options_from_static_choices(c("3", "4"), choices, c("3", "4")),
    c(`3` = "Three", `4` = "Four")
  )
})

test_that("Loop and Merge options fall back only for prefixed sources", {
  choices <- list(
    x1 = list(recode = "1", description = "One")
  )

  expect_identical(
    loop_options_from_static_choices(
      c("x1", "missing"),
      choices,
      c("x1", "missing")
    ),
    c(x1 = "x1", missing = "missing")
  )
  expect_null(
    loop_options_from_static_choices(NULL, choices, c("x1", "missing"))
  )
})

test_that("Loop and Merge field values parse valid column name records", {
  column_names <- list(
    field1 = c("A", "B"),
    field2 = c("", NA),
    field3 = c("C"),
    other = c("X", "Y")
  )

  expect_identical(
    loop_field_values_from_column_names(column_names, c("x1", "x2")),
    list(
      x1 = list(`1` = "A"),
      x2 = list(`1` = "B")
    )
  )
})
