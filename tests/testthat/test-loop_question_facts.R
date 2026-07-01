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
    unname(vapply(looped, `[[`, character(1), "base_response_column_id")),
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
    unname(vapply(looped, `[[`, character(1), "base_response_column_id")),
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
    unname(vapply(looped, `[[`, character(1), "base_response_column_id")),
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

test_that("Loop and Merge options handle partial prefixed source matches", {
  choices <- list(
    x1 = list(recode = "1", description = "One"),
    x2 = list(recode = "2", description = "Two")
  )

  expect_identical(
    loop_options_from_static_choices(
      c("x1", "x2", "missing"),
      choices,
      c("x1", "x2", "missing")
    ),
    c(x1 = "One", x2 = "Two")
  )
  expect_identical(
    loop_options_from_static_choices(
      c("x1", "missing", "other"),
      choices["x1"],
      c("x1", "missing", "other")
    ),
    c(x1 = "x1", missing = "missing", other = "other")
  )
  expect_null(
    loop_options_from_static_choices(NULL, choices, c("x1", "missing"))
  )
})

test_that("Loop and Merge options skip non-exported source choices", {
  choices <- list(
    x1 = list(recode = "1", description = "One", analyze = TRUE),
    x2 = list(recode = "2", description = "Two", analyze = FALSE)
  )

  expect_identical(
    loop_options_from_static_choices(
      c("x1", "x2", "missing"),
      choices,
      c("x1", "x2", "missing")
    ),
    c(x1 = "One")
  )
})

test_that("Loop and Merge field values parse valid column name records", {
  column_names <- list(
    field1 = c("A", "B"),
    field2 = c("", NA),
    field3 = "C",
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

test_that("Loop expansion context separates current and source facts", {
  normalised_metadata <- normalise_qualtrics_metadata(
    synthetic_loop_and_merge_raw_metadata()
  )

  context <- new_loop_expansion_context(
    question_fact = normalised_metadata$questions$QID2,
    survey_question_facts = normalised_metadata$questions
  )

  expect_identical(context$question_fact$qid, "QID2")
  expect_identical(context$looping_source_fact$qid, "QID1")
  expect_identical(context$looping_qid, "QID1")
  expect_identical(context$static_prefixes, c("x1", "x2"))
})

test_that(
  paste(
    "Loop expansion context keeps static fallback",
    "when source fact is absent"
  ),
  {
    normalised_metadata <- normalise_qualtrics_metadata(
      synthetic_loop_and_merge_raw_metadata()
    )

    context <- new_loop_expansion_context(
      question_fact = normalised_metadata$questions$QID2,
      survey_question_facts = normalised_metadata$questions["QID2"]
    )

    expect_null(context$looping_source_fact)
    expect_true(loop_question_fact_should_expand(context))
  }
)

test_that("Loop expansion preserves text-entry base response column id", {
  raw_metadata <- synthetic_looped_mc_text_raw_metadata()
  normalised_metadata <- normalise_qualtrics_metadata(raw_metadata)

  expanded <- expand_loop_question_facts(normalised_metadata$questions)
  looped <- expanded[
    vapply(expanded, function(x) isTRUE(x$looping), logical(1))
  ]

  expect_identical(
    unname(vapply(looped, `[[`, character(1), "base_response_column_id")),
    c("x1_QID2", "x2_QID2")
  )
})

test_that("normalised metadata renders supported Loop and Merge rows", {
  raw_metadata <- synthetic_loop_and_merge_raw_metadata()
  normalised_metadata <- normalise_qualtrics_metadata(raw_metadata)

  dict <- variable_dictionary_from_normalised_metadata(
    normalised_metadata,
    use_semantic_name = FALSE,
    block_pattern = NULL,
    block_sep = ".",
    semantic_name_preprocess = NULL
  )

  target_rows <- dict[grepl("QID2", dict$response_column_id, fixed = TRUE), ]

  expect_identical(
    target_rows$response_column_id,
    c("x1_QID2_TEXT", "x2_QID2_TEXT")
  )
  expect_identical(target_rows$qid, c("QID2", "QID2"))
  expect_identical(target_rows$question_name, c("Q2", "Q2"))
  expect_identical(target_rows$variable_name, c("Q2", "Q2.1"))
  expect_identical(
    target_rows$question,
    c("Why did you choose Apples?", "Why did you choose Bananas?")
  )
  expect_identical(target_rows$loop_option, c("Apples", "Bananas"))
  expect_true(all(is.na(target_rows$item)))
  expect_identical(target_rows$type, c("TE", "TE"))
  expect_identical(target_rows$selector, c("SL", "SL"))
  expect_identical(attr(dict, "survey_name"), "Loop Survey")
  expect_identical(attr(dict, "surveyID"), "SV_LOOP")
})


test_that("normalised metadata renders supported extra Loop and Merge fields", {
  raw_metadata <- synthetic_multi_field_loop_and_merge_raw_metadata()
  normalised_metadata <- normalise_qualtrics_metadata(raw_metadata)

  dict <- variable_dictionary_from_normalised_metadata(
    normalised_metadata,
    use_semantic_name = FALSE,
    block_pattern = NULL,
    block_sep = ".",
    semantic_name_preprocess = NULL
  )

  target_rows <- dict[grepl("QID2", dict$response_column_id, fixed = TRUE), ]

  expect_identical(
    target_rows$response_column_id,
    c("x1_QID2_TEXT", "x2_QID2_TEXT")
  )
  expect_identical(
    target_rows$question,
    c("Compare Apples with Red fruit", "Compare Bananas with Yellow fruit")
  )
  expect_identical(target_rows$loop_option, c("Apples", "Bananas"))
})


test_that("static Loop and Merge rows render when source QID is absent", {
  raw_metadata <- synthetic_loop_and_merge_raw_metadata()
  raw_metadata$metadata$questions$QID1 <- NULL

  dict <- variable_dictionary_from_normalised_metadata(
    normalise_qualtrics_metadata(raw_metadata),
    use_semantic_name = FALSE,
    block_pattern = NULL,
    block_sep = ".",
    semantic_name_preprocess = NULL
  )

  target_rows <- dict[grepl("QID2", dict$response_column_id, fixed = TRUE), ]

  expect_identical(
    target_rows$response_column_id,
    c("x1_QID2_TEXT", "x2_QID2_TEXT")
  )
  expect_identical(target_rows$loop_option, c("x1", "x2"))
})


test_that("normalised metadata renders static Loop and Merge rows", {
  raw_metadata <- synthetic_static_loop_and_merge_raw_metadata()
  normalised_metadata <- normalise_qualtrics_metadata(raw_metadata)

  dict <- variable_dictionary_from_normalised_metadata(
    normalised_metadata,
    use_semantic_name = FALSE,
    block_pattern = NULL,
    block_sep = ".",
    semantic_name_preprocess = NULL
  )

  target_rows <- dict[grepl("QID2", dict$response_column_id, fixed = TRUE), ]

  expect_identical(
    target_rows$response_column_id,
    c("1_QID2_TEXT", "2_QID2_TEXT")
  )
  expect_identical(
    target_rows$question,
    c("Compare Apples with Red fruit", "Compare Bananas with Yellow fruit")
  )
  expect_identical(target_rows$loop_option, c("Apples", "Bananas"))
})


test_that("static numeric loop prefixes do not fall back to source QIDs", {
  raw_metadata <- synthetic_static_numeric_looped_text_raw_metadata()
  normalised_metadata <- normalise_qualtrics_metadata(raw_metadata)

  dict <- variable_dictionary_from_normalised_metadata(
    normalised_metadata,
    use_semantic_name = FALSE,
    block_pattern = NULL,
    block_sep = ".",
    semantic_name_preprocess = NULL
  )

  target_rows <- dict[grepl("QID3", dict$response_column_id, fixed = TRUE), ]

  expect_identical(
    target_rows$response_column_id,
    paste0(1:12, "_QID3_TEXT")
  )
  expect_false(any(grepl(
    "^QID[0-9]+_QID3_TEXT$",
    target_rows$response_column_id
  )))
  expect_identical(target_rows$loop_option, as.character(1:12))
})


test_that("matrix source Loop and Merge prefixes use source response rows", {
  raw_metadata <- synthetic_matrix_source_looped_text_raw_metadata()
  normalised_metadata <- normalise_qualtrics_metadata(raw_metadata)

  dict <- variable_dictionary_from_normalised_metadata(
    normalised_metadata,
    use_semantic_name = FALSE,
    block_pattern = NULL,
    block_sep = ".",
    semantic_name_preprocess = NULL
  )

  target_rows <- dict[grepl("QID2", dict$response_column_id, fixed = TRUE), ]

  expect_identical(
    target_rows$response_column_id,
    c("1_QID2_TEXT", "2_QID2_TEXT", "3_QID2_TEXT")
  )
  expect_false(any(grepl("^QID1_QID2_TEXT$", target_rows$response_column_id)))
  expect_identical(
    target_rows$loop_option,
    c("Condition 1", "Condition 2", "Condition 3")
  )
})
