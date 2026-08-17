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

test_that("unsupported source-backed Loop and Merge is skipped", {
  raw_metadata <- synthetic_unresolved_source_loop_and_merge_raw_metadata()
  normalised_metadata <- normalise_qualtrics_metadata(raw_metadata)

  expanded <- expand_loop_question_facts(normalised_metadata$questions)
  diagnostics <- attr(expanded, "unsupported_loop_diagnostics")

  expect_false("QID2" %in% names(expanded))
  expect_length(diagnostics, 1)
  expect_identical(diagnostics[[1]]$qid, "QID2")
  expect_identical(diagnostics[[1]]$question_name, "Q2")
  expect_identical(diagnostics[[1]]$looping_qid, "QID1")
  expect_identical(
    diagnostics[[1]]$reason,
    "static prefixes cannot be resolved against source choices"
  )
})

test_that("expand-then-render adapter preserves Loop and Merge boundary", {
  normalised_metadata <- normalise_qualtrics_metadata(
    synthetic_loop_and_merge_raw_metadata()
  )

  rendered <- expand_then_render_question_response_columns(
    normalised_metadata$questions
  )

  expect_identical(
    unname(unlist(
      purrr::map(rendered, "response_column_id"),
      use.names = FALSE
    )),
    c("QID1", "x1_QID2_TEXT", "x2_QID2_TEXT")
  )
})

test_that("question row assembly defaults to shared adapter", {
  normalised_metadata <- normalise_qualtrics_metadata(
    synthetic_loop_and_merge_raw_metadata()
  )
  question_meta <- normalised_metadata$questions["QID1"]

  local_mocked_bindings(
    expand_then_render_question_response_columns = function(question_facts) {
      expect_named(question_facts, "QID1")
      list(
        QID1 = list(
          question_fact = question_facts$QID1,
          response_column_id = "QID1",
          response_columns = tibble::tibble(
            response_column_id = "QID1",
            question = "Which options did you consider?",
            item = NA_character_,
            level = "1",
            label = "Apples"
          )
        )
      )
    }
  )

  rows <- variable_dictionary_question_rows(question_meta)

  expect_length(rows, 1)
  expect_identical(rows[[1]]$response_column_id, "QID1")
  expect_identical(rows[[1]]$qid, "QID1")
})

test_that("Loop and Merge options resolve direct source choice IDs", {
  choices <- list(
    a = list(choice_id = "a", recode = "3", label = "Alpha"),
    b = list(choice_id = "b", recode = "4", label = "Bravo")
  )

  expect_identical(
    loop_options_from_static_choices(NULL, choices, c("a", "b")),
    c(a = "Alpha", b = "Bravo")
  )
})

test_that("Loop and Merge options resolve source choice recodes", {
  choices <- list(
    a = list(choice_id = "a", recode = "3", label = "Three"),
    b = list(choice_id = "b", recode = "4", label = "Four")
  )

  expect_identical(
    loop_options_from_static_choices(c("3", "4"), choices, c("3", "4")),
    c(`3` = "Three", `4` = "Four")
  )
})

test_that("Loop and Merge options fail when source choices are unresolved", {
  choices <- list(
    x1 = list(choice_id = "x1", recode = "1", label = "One"),
    x2 = list(choice_id = "x2", recode = "2", label = "Two")
  )

  expect_null(
    loop_options_from_static_choices(
      c("x1", "x2", "missing"),
      choices,
      c("x1", "x2", "missing")
    )
  )
  expect_null(
    loop_options_from_static_choices(
      c("x1", "missing", "other"),
      choices["x1"],
      c("x1", "missing", "other")
    )
  )
  expect_null(
    loop_options_from_static_choices(NULL, choices, c("x1", "missing"))
  )
})

test_that("Loop and Merge options reconcile stale Static prefixes", {
  expect_identical(
    reconcile_matrix_source_order(character(), c("x1", "x2")),
    character()
  )
  expect_identical(
    reconcile_matrix_source_order(c("x1", "stale"), c("x1", "x2")),
    c("x1", "x2")
  )

  choices <- list(
    x1 = list(choice_id = "x1", recode = "1", label = "One"),
    x2 = list(choice_id = "x2", recode = "2", label = "Two"),
    x27 = list(choice_id = "x27", recode = "27", label = "Twenty-seven"),
    x26 = list(choice_id = "x26", recode = "0", label = "Zero")
  )

  expect_identical(
    loop_options_from_static_choices(
      c("x1", "x2", "x11", "x26"),
      choices,
      c("x1", "x2", "x11", "x26")
    ),
    c(
      x1 = "One",
      x2 = "Two",
      x27 = "Twenty-seven",
      x26 = "Zero"
    )
  )
})

test_that("Loop and Merge options insert omitted source choice IDs", {
  choices <- list(
    x1 = list(choice_id = "x1", recode = "1", label = "One"),
    x2 = list(choice_id = "x2", recode = "2", label = "Two"),
    x16 = list(choice_id = "x16", recode = "16", label = "Sixteen"),
    x15 = list(choice_id = "x15", recode = "0", label = "Zero")
  )

  expect_identical(
    loop_options_from_static_choices(
      c("x1", "x2", "x15"),
      choices,
      c("x1", "x2", "x15")
    ),
    c(
      x1 = "One",
      x2 = "Two",
      x16 = "Sixteen",
      x15 = "Zero"
    )
  )
})

test_that("Loop and Merge options keep static non-analysed source choices", {
  choices <- list(
    x1 = list(choice_id = "x1", recode = "1", label = "One", analyze = TRUE),
    x2 = list(choice_id = "x2", recode = "2", label = "Two", analyze = FALSE)
  )

  expect_null(
    loop_options_from_static_choices(
      c("x1", "x2", "missing"),
      choices,
      c("x1", "x2", "missing")
    )
  )
})

test_that("Loop Option resolution dispatches by source case", {
  matrix_metadata <- normalise_qualtrics_metadata(
    synthetic_matrix_source_looped_text_raw_metadata()
  )
  matrix_context <- new_loop_expansion_context(
    question_fact = matrix_metadata$questions$QID2,
    survey_question_facts = matrix_metadata$questions
  )

  expect_identical(
    loop_options_for_context(matrix_context),
    c(`1` = "Condition 1", `2` = "Condition 2", `3` = "Condition 3")
  )

  choice_metadata <- normalise_qualtrics_metadata(
    synthetic_loop_and_merge_raw_metadata()
  )
  choice_context <- new_loop_expansion_context(
    question_fact = choice_metadata$questions$QID2,
    survey_question_facts = choice_metadata$questions
  )

  expect_identical(
    loop_options_for_context(choice_context),
    c(x1 = "Apples", x2 = "Bananas")
  )

  static_metadata <- normalise_qualtrics_metadata(
    synthetic_static_loop_and_merge_raw_metadata()
  )
  static_context <- new_loop_expansion_context(
    question_fact = static_metadata$questions$QID2,
    survey_question_facts = static_metadata$questions
  )

  expect_identical(
    loop_options_for_context(static_context),
    c(`1` = "Apples", `2` = "Bananas")
  )
})

test_that("blank source choice labels fall back to the choice ID", {
  raw_metadata <- synthetic_loop_and_merge_raw_metadata()
  raw_metadata$metadata$questions$QID1$choices$x2$description <- ""
  normalised_metadata <- normalise_qualtrics_metadata(raw_metadata)
  context <- new_loop_expansion_context(
    question_fact = normalised_metadata$questions$QID2,
    survey_question_facts = normalised_metadata$questions
  )

  expect_identical(
    loop_options_for_context(context),
    c(x1 = "Apples", x2 = "x2")
  )

  expect_identical(
    loop_option_label(list(choice_id = "x3", label = NA_character_)),
    "x3"
  )
})

test_that("source-backed loops do not fall back to static prefix labels", {
  raw_metadata <- synthetic_unresolved_source_loop_and_merge_raw_metadata()
  normalised_metadata <- normalise_qualtrics_metadata(raw_metadata)
  context <- new_loop_expansion_context(
    question_fact = normalised_metadata$questions$QID2,
    survey_question_facts = normalised_metadata$questions
  )

  expect_null(loop_options_for_context(context))
})

test_that(
  paste(
    "source-backed loops with absent source facts use static",
    "LoopingOptions rows"
  ),
  {
    raw_metadata <- synthetic_absent_source_static_loop_and_merge_raw_metadata()
    normalised_metadata <- normalise_qualtrics_metadata(raw_metadata)
    context <- new_loop_expansion_context(
      question_fact = normalised_metadata$questions$QID2,
      survey_question_facts = normalised_metadata$questions
    )

    expect_identical(
      loop_options_for_context(context),
      c(`1` = "Apples", `2` = "Bananas")
    )
  }
)

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

test_that(
  paste(
    "static Loop and Merge prefixes render when source choices",
    "are non-analysed"
  ),
  {
    raw_metadata <- synthetic_loop_and_merge_raw_metadata()

    raw_metadata$metadata$questions$QID1$questionType$selector <- "MACOL"
    raw_metadata$metadata$questions$QID1$choices$x1$analyze <- FALSE
    raw_metadata$metadata$questions$QID1$choices$x2$analyze <- FALSE
    raw_metadata$metadata$questions$QID1$choices$x3 <- list(
      recode = "3",
      description = "Cherries",
      analyze = TRUE
    )

    dict <- variable_dictionary_from_normalised_metadata(
      normalise_qualtrics_metadata(raw_metadata),
      use_semantic_name = FALSE,
      block_pattern = NULL,
      block_sep = ".",
      semantic_name_preprocess = NULL
    )

    expect_false("QID1_1" %in% dict$response_column_id)
    expect_false("QID1_2" %in% dict$response_column_id)
    expect_true("QID1_3" %in% dict$response_column_id)

    target_rows <- dict[grepl("QID2", dict$response_column_id, fixed = TRUE), ]

    expect_identical(
      target_rows$response_column_id,
      c("x1_QID2_TEXT", "x2_QID2_TEXT")
    )
    expect_identical(
      target_rows$question,
      c("Why did you choose Apples?", "Why did you choose Bananas?")
    )
    expect_identical(target_rows$loop_option, c("Apples", "Bananas"))
  }
)


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


test_that(
  paste(
    "source-backed Loop and Merge uses static rows",
    "when source fact is absent"
  ),
  {
    raw_metadata <- synthetic_absent_source_static_loop_and_merge_raw_metadata()

    expanded <- expand_loop_question_facts(
      normalise_qualtrics_metadata(raw_metadata)$questions
    )
    diagnostics <- attr(expanded, "unsupported_loop_diagnostics")

    expect_identical(
      unname(vapply(
        looped_question_facts(expanded),
        `[[`,
        character(1),
        "qid"
      )),
      c("QID2", "QID2")
    )
    expect_identical(
      unname(vapply(
        looped_question_facts(expanded),
        `[[`,
        character(1),
        "base_response_column_id"
      )),
      c("1_QID2", "2_QID2")
    )
    expect_length(diagnostics, 0)
  }
)

test_that(
  paste(
    "source-backed Loop and Merge is skipped",
    "when source fact is absent and static rows are unusable"
  ),
  {
    raw_metadata <-
      synthetic_absent_source_unusable_static_loop_and_merge_raw_metadata()

    expanded <- expand_loop_question_facts(
      normalise_qualtrics_metadata(raw_metadata)$questions
    )
    diagnostics <- attr(expanded, "unsupported_loop_diagnostics")

    expect_false("QID2" %in% names(expanded))
    expect_length(diagnostics, 1)
    expect_identical(
      diagnostics[[1]]$reason,
      "source QID is absent from Normalised Question Facts"
    )
  }
)

test_that(
  paste(
    "normalised metadata renders prefixed source-backed loops",
    "when source fact is absent but static rows are usable"
  ),
  {
    raw_metadata <- synthetic_absent_source_static_loop_and_merge_raw_metadata()
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
    expect_false("QID2_TEXT" %in% target_rows$response_column_id)
    expect_identical(
      target_rows$question,
      c("Why did you choose Apples?", "Why did you choose Bananas?")
    )
    expect_identical(target_rows$loop_option, c("Apples", "Bananas"))
  }
)


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


test_that("static numeric source-backed loop prefixes are skipped", {
  raw_metadata <- synthetic_static_numeric_looped_text_raw_metadata()
  normalised_metadata <- normalise_qualtrics_metadata(raw_metadata)

  expanded <- expand_loop_question_facts(normalised_metadata$questions)
  diagnostics <- attr(expanded, "unsupported_loop_diagnostics")

  expect_false("QID3" %in% names(expanded))
  expect_length(diagnostics, 1)
  expect_identical(diagnostics[[1]]$qid, "QID3")
  expect_identical(
    diagnostics[[1]]$reason,
    "static prefixes cannot be resolved against source choices"
  )
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

test_that("Loop-expanded Question Facts have compact contract summary", {
  raw_metadata <- synthetic_multi_field_loop_and_merge_raw_metadata()
  normalised_metadata <- normalise_qualtrics_metadata(raw_metadata)

  expanded <- expand_loop_question_facts(normalised_metadata$questions)

  expect_snapshot_value(
    compact_loop_question_facts(expanded),
    style = "json2"
  )
})

test_that("Loop-expanded Question Fact construction has explicit contract", {
  normalised_metadata <- normalise_qualtrics_metadata(
    synthetic_multi_field_loop_and_merge_raw_metadata()
  )
  context <- new_loop_expansion_context(
    question_fact = normalised_metadata$questions$QID2,
    survey_question_facts = normalised_metadata$questions
  )
  loop_rows <- loop_rows_for_context(context)

  looped <- new_loop_expanded_question_fact(
    question_fact = context$question_fact,
    loop_row = loop_rows[[1]],
    context = context
  )

  expect_identical(looped$qid, "QID2")
  expect_identical(looped$looping_question, "Compare Apples with Red fruit")
  expect_identical(looped$question_text, "Compare {} with {}")
  expect_identical(looped$looping_option, "Apples")
  expect_identical(looped$looping_prefix, "x1")
  expect_identical(looped$looping_qid, "QID1")
  expect_identical(looped$base_response_column_id, "x1_QID2")
  expect_true(looped$looping)
})

test_that("Loop and Merge field merge precedence is named and stable", {
  normalised_metadata <- normalise_qualtrics_metadata(
    synthetic_multi_field_loop_and_merge_raw_metadata()
  )
  prefixes <- c("x1", "x2")

  expect_identical(
    loop_field_values_static_overrides_column_names(
      normalised_metadata$questions$QID2,
      prefixes
    ),
    list(
      x1 = c(`2` = "Red fruit"),
      x2 = c(`2` = "Yellow fruit")
    )
  )
})

test_that("Loop Base Response Column ID normalisation preserves text entries", {
  expect_identical(
    normalise_loop_base_response_column_id("x1_QID2_2_TEXT_TEXT"),
    "x1_QID2_TEXT"
  )
  expect_identical(
    normalise_loop_base_response_column_id("x1_QID2"),
    "x1_QID2"
  )
})
