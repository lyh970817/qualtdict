test_that("raw Qualtrics metadata normalises into package-owned metadata", {
  raw_metadata <- synthetic_mc_text_raw_metadata()

  normalised_metadata <- normalise_qualtrics_metadata(raw_metadata)

  expect_s3_class(raw_metadata, "qualtdict_raw_metadata")
  expect_s3_class(normalised_metadata, "qualtdict_normalised_metadata")
  expect_named(normalised_metadata, c(
    "surveyID",
    "survey_name",
    "questions"
  ))
  expect_equal(normalised_metadata$surveyID, "SV_SYNTHETIC")
  expect_equal(normalised_metadata$survey_name, "Synthetic Survey")
  expect_s3_class(
    normalised_metadata$questions,
    "qualtdict_normalised_questions"
  )
  expect_equal(names(normalised_metadata$questions), "QID1")

  question <- normalised_metadata$questions$QID1
  expect_s3_class(question, "qualtdict_normalised_question")
  expect_equal(question$survey_block, "Main Block")
  expect_equal(question$content_type, "Number")
  expect_null(question$looping_qid)
  expect_equal(question$question_name, "Q1")
  expect_equal(question$question_text, "Choose one")
  expect_equal(question$question_type$type, "MC")
  expect_equal(question$question_type$selector, "SAVR")
  expect_equal(question$question_type$sub_selector, "TX")
  expect_equal(names(question$response_choices), c("1", "2", "3"))
  expect_equal(length(question$response_items), 0)
  expect_equal(length(question$column_facts), 0)
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

  expect_equal(
    dict_subset$response_column_id,
    c("QID1", "QID1", "QID1", "QID1_3_TEXT")
  )
  expect_equal(dict_subset$qid, rep("QID1", 4))
  expect_equal(
    dict_subset$question_name,
    c("Q1", "Q1", "Q1", "Q1")
  )
  expect_equal(
    dict_subset$variable_name,
    c("Q1", "Q1", "Q1", "Q1.1")
  )
  expect_equal(dict_subset$block, rep("Main Block", 4))
  expect_equal(dict_subset$question, rep("Choose one", 4))
  expect_true(all(is.na(dict_subset$item)))
  expect_equal(unname(dict_subset$level), c("1", "2", "3", "3_TEXT"))
  expect_equal(
    unname(dict_subset$label),
    c("Yes", "No", "Other", "Other_TEXT")
  )
  expect_equal(dict_subset$type, rep("MC", 4))
  expect_equal(dict_subset$selector, rep("SAVR", 4))
  expect_equal(dict_subset$sub_selector, rep("TX", 4))
  expect_equal(dict_subset$content_type, rep("Number", 4))
  expect_equal(attr(dict, "survey_name"), "Synthetic Survey")
  expect_equal(attr(dict, "surveyID"), "SV_SYNTHETIC")
})

test_that("question-name Variable Dictionaries repair only variable_name", {
  raw_metadata <- synthetic_mc_text_raw_metadata()
  raw_metadata$metadata$questions$QID1$questionName <- "1 Bad Name"
  raw_metadata$metadata$questions$QID2 <- raw_metadata$metadata$questions$QID1
  raw_metadata$metadata$questions$QID2$questionName <- "1 Bad Name"
  raw_metadata$description$block$BL_1$BlockElements <- list(
    list(QuestionID = "QID1"),
    list(QuestionID = "QID2")
  )
  raw_metadata$description$question$QID2 <-
    raw_metadata$description$question$QID1
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

  expect_equal(
    dict$question_name[first_rows],
    c("1 Bad Name", "1 Bad Name", "1 Bad Name", "1 Bad Name")
  )
  expect_equal(
    dict$variable_name[first_rows],
    c("X1_Bad_Name", "X1_Bad_Name.1", "X1_Bad_Name.2",
      "X1_Bad_Name.3")
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
  expect_equal(
    findings$finding,
    rep("repaired_variable_name", 4)
  )
  expect_equal(
    findings$response_column_id,
    c("QID1", "QID1_3_TEXT", "QID2", "QID2_3_TEXT")
  )
  expect_equal(
    findings$original_candidate,
    c("1 Bad Name", "1 Bad Name", "1 Bad Name", "1 Bad Name")
  )
  expect_equal(findings$variable_name, dict$variable_name[first_rows])
  expect_equal(
    findings$reason,
    c("unsafe", "unsafe;duplicate", "unsafe;duplicate", "unsafe;duplicate")
  )
})

test_that("Semantic Names are generated only on the semantic naming path", {
  raw_metadata <- synthetic_mc_text_raw_metadata()
  normalised_metadata <- normalise_qualtrics_metadata(raw_metadata)

  question_name_dict <- variable_dictionary_from_normalised_metadata(
    normalised_metadata,
    use_semantic_name = FALSE,
    block_pattern = NULL,
    block_sep = ".",
    semantic_name_preprocess = NULL
  )
  semantic_name_dict <- variable_dictionary_from_normalised_metadata(
    normalised_metadata,
    use_semantic_name = TRUE,
    block_pattern = NULL,
    block_sep = ".",
    semantic_name_preprocess = NULL
  )

  expect_false("semantic_name" %in% names(question_name_dict))
  expect_equal(
    question_name_dict$variable_name,
    c("Q1", "Q1", "Q1", "Q1.1")
  )
  expect_true("semantic_name" %in% names(semantic_name_dict))
  expect_true(any(!is.na(semantic_name_dict$semantic_name)))
  expect_equal(
    semantic_name_dict$semantic_name,
    c("choose_one", "choose_one", "choose_one", "choose_one.txt")
  )
  expect_equal(
    semantic_name_dict$variable_name,
    c("choose_one", "choose_one", "choose_one", "choose_one.txt")
  )
})

test_that("Semantic Name generation is quiet by default with opt-in progress messages", { # nolint
  quiet_metadata <- synthetic_mc_text_raw_metadata()
  quiet_metadata$metadata$questions$QID1$questionText <-
    "Quiet semantic naming check"
  quiet_metadata <- normalise_qualtrics_metadata(quiet_metadata)

  expect_silent(
    variable_dictionary_from_normalised_metadata(
      quiet_metadata,
      use_semantic_name = TRUE,
      block_pattern = NULL,
      block_sep = ".",
      semantic_name_preprocess = NULL
    )
  )

  verbose_metadata <- synthetic_mc_text_raw_metadata()
  verbose_metadata$metadata$questions$QID1$questionText <-
    "Verbose semantic naming check"
  verbose_metadata <- normalise_qualtrics_metadata(verbose_metadata)

  expect_message(
    variable_dictionary_from_normalised_metadata(
      verbose_metadata,
      use_semantic_name = TRUE,
      block_pattern = NULL,
      block_sep = ".",
      semantic_name_preprocess = NULL,
      quiet = FALSE
    ),
    "Generating Semantic Names"
  )
})

test_that("Semantic Name keyword extraction suppresses progress bars by default", {
  expect_silent(
    slowrake(
      c("First progress check", "Second progress check"),
      all_words = "First progress check Second progress check",
      stop_pos = NULL
    )
  )
})

test_that("Semantic Names preserve source order for selected keywords", {
  raw_metadata <- synthetic_mc_text_raw_metadata()
  raw_metadata$metadata$questions$QID1$questionText <-
    "Felt like I had a lot of energy"
  normalised_metadata <- normalise_qualtrics_metadata(raw_metadata)

  dict <- variable_dictionary_from_normalised_metadata(
    normalised_metadata,
    use_semantic_name = TRUE,
    block_pattern = NULL,
    block_sep = ".",
    semantic_name_preprocess = NULL
  )

  expect_equal(
    dict$semantic_name,
    c("felt_lot_energy", "felt_lot_energy", "felt_lot_energy",
      "felt_lot_energy.txt")
  )
})

test_that("Semantic Name keyword cache keys include the scoring corpus", {
  semantic_json <- function(questions) {
    tibble(
      qid = paste0("QID", seq_along(questions)),
      response_column_id = paste0("QID", seq_along(questions)),
      question_name = paste0("Q", seq_along(questions)),
      block = "Main Block",
      question = questions,
      looping_question = NA_character_,
      item = NA_character_,
      level = NA_character_,
      label = NA_character_,
      type = "MC",
      selector = "SAVR",
      content_type = "Number",
      sub_selector = "TX",
      looping_option = NA_character_,
      looping = FALSE
    )
  }
  cache_path <- function(questions) {
    texts <- questions
    unique_texts <- clean_semantic_name_text(unique(texts))
    all_words <- paste(texts, collapse = " ")

    paste0(tempdir(), "/", hash(list(
      algorithm = "semantic-name-source-order-v1",
      unique_texts = unique_texts,
      all_words = all_words
    )), ".rds")
  }

  target <- paste(
    "Alpha and beta and gamma and delta and epsilon and zeta and eta",
    "and theta"
  )
  first_questions <- c(target, "Alpha repeated context")
  second_questions <- c(
    target,
    "Alpha repeated context",
    "Alpha repeated context"
  )
  first_cache_path <- cache_path(first_questions)
  second_cache_path <- cache_path(second_questions)
  unlink(c(first_cache_path, second_cache_path))

  generate_semantic_names(
    semantic_json(first_questions),
    surveyID = "SV_TEST",
    block_pattern = NULL,
    block_sep = ".",
    semantic_name_preprocess = NULL
  )
  expect_true(file.exists(first_cache_path))
  expect_false(file.exists(second_cache_path))

  generate_semantic_names(
    semantic_json(second_questions),
    surveyID = "SV_TEST",
    block_pattern = NULL,
    block_sep = ".",
    semantic_name_preprocess = NULL
  )
  expect_true(file.exists(second_cache_path))
})

test_that("semantic_name_preprocess runs only for semantic naming", {
  raw_metadata <- synthetic_mc_text_raw_metadata()
  normalised_metadata <- normalise_qualtrics_metadata(raw_metadata)
  calls <- 0
  semantic_name_preprocess <- function(dict) {
    calls <<- calls + 1
    dict$question <- "Renamed by hook"
    dict
  }

  variable_dictionary_from_normalised_metadata(
    normalised_metadata,
    use_semantic_name = FALSE,
    block_pattern = NULL,
    block_sep = ".",
    semantic_name_preprocess = semantic_name_preprocess
  )
  expect_equal(calls, 0)

  dict <- variable_dictionary_from_normalised_metadata(
    normalised_metadata,
    use_semantic_name = TRUE,
    block_pattern = NULL,
    block_sep = ".",
    semantic_name_preprocess = semantic_name_preprocess
  )

  expect_equal(calls, 1)
  expect_true(all(grepl("^renamed_by_hook", dict$semantic_name)))
})

test_that("semantic_name_preprocess receives the full post-normalisation dictionary", { # nolint
  raw_metadata <- synthetic_loop_and_merge_raw_metadata()
  normalised_metadata <- normalise_qualtrics_metadata(raw_metadata)
  seen_names <- NULL
  seen_loop_rows <- NULL
  semantic_name_preprocess <- function(dict) {
    seen_names <<- names(dict)
    seen_loop_rows <<- dict[dict$looping, c(
      "qid", "response_column_id", "question_name", "block", "question",
      "looping_option", "item", "label", "type", "selector", "sub_selector",
      "content_type"
    )]
    dict
  }

  variable_dictionary_from_normalised_metadata(
    normalised_metadata,
    use_semantic_name = TRUE,
    block_pattern = NULL,
    block_sep = ".",
    semantic_name_preprocess = semantic_name_preprocess
  )

  expect_true(all(c(
    "qid", "response_column_id", "question_name", "block", "question",
    "looping_question", "item", "level", "label", "type", "selector",
    "content_type", "sub_selector", "looping_option", "looping"
  ) %in% seen_names))
  expect_equal(
    seen_loop_rows$response_column_id,
    c("x1_QID2_TEXT", "x2_QID2_TEXT")
  )
  expect_equal(seen_loop_rows$question_name, c("Q2", "Q2"))
  expect_equal(seen_loop_rows$looping_option, c("Apples", "Bananas"))
})

test_that("temporary semantic_name_preprocess columns do not leak", {
  raw_metadata <- synthetic_mc_text_raw_metadata()
  normalised_metadata <- normalise_qualtrics_metadata(raw_metadata)
  semantic_name_preprocess <- function(dict) {
    dict$temporary_helper <- "helper"
    dict$question <- dict$temporary_helper
    dict
  }

  dict <- variable_dictionary_from_normalised_metadata(
    normalised_metadata,
    use_semantic_name = TRUE,
    block_pattern = NULL,
    block_sep = ".",
    semantic_name_preprocess = semantic_name_preprocess
  )

  expect_true(all(grepl("^helper", dict$semantic_name)))
  expect_false("temporary_helper" %in% names(dict))
})

test_that("semantic-name Variable Dictionaries use final variable_name repair", {
  raw_metadata <- synthetic_mc_text_raw_metadata()
  raw_metadata$metadata$questions$QID1$questionText <- "123 bad name"
  raw_metadata$metadata$questions$QID2 <- raw_metadata$metadata$questions$QID1
  raw_metadata$metadata$questions$QID2$questionName <- "Q2"
  raw_metadata$description$block$BL_1$BlockElements <- list(
    list(QuestionID = "QID1"),
    list(QuestionID = "QID2")
  )
  raw_metadata$description$question$QID2 <-
    raw_metadata$description$question$QID1
  normalised_metadata <- normalise_qualtrics_metadata(raw_metadata)

  dict <- variable_dictionary_from_normalised_metadata(
    normalised_metadata,
    use_semantic_name = TRUE,
    block_pattern = NULL,
    block_sep = ".",
    semantic_name_preprocess = NULL
  )
  attr(dict, "class") <- c("qualtdict", class(dict))
  first_rows <- !duplicated(dict$response_column_id)
  findings <- dict_validate(dict)$validation_findings
  findings <- findings[findings$finding == "repaired_variable_name", ]

  expect_equal(
    dict$semantic_name[first_rows],
    c("123_bad_name", "123_bad_name.txt", "123_bad_name",
      "123_bad_name.txt")
  )
  expect_equal(
    dict$variable_name[first_rows],
    c("X123_bad_name", "X123_bad_name.txt", "X123_bad_name.1",
      "X123_bad_name.txt.1")
  )
  expect_equal(
    findings$response_column_id,
    c("QID1", "QID1_3_TEXT", "QID2", "QID2_3_TEXT")
  )
  expect_equal(
    findings$finding,
    rep("repaired_variable_name", 4)
  )
  expect_equal(findings$original_candidate, dict$semantic_name[first_rows])
  expect_equal(findings$variable_name, dict$variable_name[first_rows])
  expect_equal(
    findings$reason,
    c("unsafe", "unsafe", "unsafe;duplicate", "unsafe;duplicate")
  )
})

test_that("matrix response columns use Qualtrics subquestion IDs", {
  raw_metadata <- synthetic_matrix_raw_metadata()
  normalised_metadata <- normalise_qualtrics_metadata(raw_metadata)

  dict <- variable_dictionary_from_normalised_metadata(
    normalised_metadata,
    use_semantic_name = FALSE,
    block_pattern = NULL,
    block_sep = ".",
    semantic_name_preprocess = NULL
  )

  expect_equal(
    unique(dict$response_column_id),
    c("QID1_x1", "QID1_x2")
  )
  expect_equal(dict$qid, rep("QID1", 4))
  expect_equal(unname(dict$item), c("Apples", "Apples", "Bananas", "Bananas"))
  expect_equal(unname(dict$level), c("1", "2", "1", "2"))
  expect_equal(unname(dict$label), c("Low", "High", "Low", "High"))
})

test_that("multiple-answer response columns use Qualtrics x choice IDs", {
  raw_metadata <- synthetic_mc_x_choice_raw_metadata()
  normalised_metadata <- normalise_qualtrics_metadata(raw_metadata)

  dict <- variable_dictionary_from_normalised_metadata(
    normalised_metadata,
    use_semantic_name = FALSE,
    block_pattern = NULL,
    block_sep = ".",
    semantic_name_preprocess = NULL
  )

  expect_equal(
    dict$response_column_id,
    paste0("QID126879611_", c("x1", "x2", "x3", "x4", "x6"))
  )
  expect_equal(unname(dict$level), c("1", "2", "3", "4", "6"))
  expect_equal(
    unname(dict$label),
    paste("Choice", c("1", "2", "3", "4", "6"))
  )
})

test_that("multiple-answer response columns use recodes when choice IDs are numeric", { # nolint
  raw_metadata <- synthetic_mc_recode_raw_metadata()
  normalised_metadata <- normalise_qualtrics_metadata(raw_metadata)

  dict <- variable_dictionary_from_normalised_metadata(
    normalised_metadata,
    use_semantic_name = FALSE,
    block_pattern = NULL,
    block_sep = ".",
    semantic_name_preprocess = NULL
  )

  expect_equal(
    dict$response_column_id,
    c("QID1_1", "QID1_2", "QID1_-88", "QID1_-99", "QID1_0",
      "QID1_9_TEXT")
  )
  expect_equal(
    unname(dict$level),
    c("1", "2", "-88", "-99", "0", "0_TEXT")
  )
  expect_true(all(lengths(dict) == nrow(dict)))
})

test_that("SBS multiple-answer columns include column, row, and choice IDs", {
  raw_metadata <- synthetic_sbs_multiple_answer_raw_metadata()
  normalised_metadata <- normalise_qualtrics_metadata(raw_metadata)

  dict <- variable_dictionary_from_normalised_metadata(
    normalised_metadata,
    use_semantic_name = FALSE,
    block_pattern = NULL,
    block_sep = ".",
    semantic_name_preprocess = NULL
  )

  expect_true(all(c(
    "QID2#3_2_1", "QID2#3_2_2",
    "QID2#3_4_1", "QID2#3_4_2"
  ) %in%
    dict$response_column_id))
  expect_equal(
    dict$response_column_id[grepl("^QID2#3_", dict$response_column_id)],
    c("QID2#3_2_1", "QID2#3_2_2", "QID2#3_4_1", "QID2#3_4_2")
  )
  expect_equal(
    unname(dict$level[grepl("^QID2#3_", dict$response_column_id)]),
    c("1", "2", "1", "2")
  )
  expect_true(all(lengths(dict) == nrow(dict)))
})

test_that("carried-forward SBS rows use subquestion response column IDs", {
  raw_metadata <- synthetic_sbs_carried_forward_raw_metadata()
  normalised_metadata <- normalise_qualtrics_metadata(raw_metadata)

  dict <- variable_dictionary_from_normalised_metadata(
    normalised_metadata,
    use_semantic_name = FALSE,
    block_pattern = NULL,
    block_sep = ".",
    semantic_name_preprocess = NULL
  )

  expect_equal(
    dict$response_column_id,
    c("QID2_x1", "QID2_x2", "QID2_x3")
  )
  expect_equal(unname(dict$item), c("First row", "Second row", "Third row"))
  expect_true(all(lengths(dict) == nrow(dict)))
})

test_that("SBS text-entry subquestions keep row metadata lengths aligned", {
  raw_metadata <- synthetic_sbs_text_subquestion_raw_metadata()
  normalised_metadata <- normalise_qualtrics_metadata(raw_metadata)

  expect_no_error(
    dict <- variable_dictionary_from_normalised_metadata(
      normalised_metadata,
      use_semantic_name = FALSE,
      block_pattern = NULL,
      block_sep = ".",
      semantic_name_preprocess = NULL
    )
  )

  expect_true(all(lengths(dict) == nrow(dict)))
  expect_equal(
    dict$response_column_id,
    c(
      "QID2#1_2_1",
      "QID2#1_4_1",
      "QID2#1_4_TEXT",
      "QID2#1_9_1",
      "QID2#2_2",
      "QID2#2_2",
      "QID2#2_4",
      "QID2#2_4",
      "QID2#2_4_TEXT",
      "QID2#2_9",
      "QID2#2_9",
      "QID2#3_2",
      "QID2#3_2",
      "QID2#3_4",
      "QID2#3_4",
      "QID2#3_4_TEXT",
      "QID2#3_9",
      "QID2#3_9"
    )
  )
  expect_equal(
    unname(dict$item),
    c(
      "Second row",
      "Fourth row",
      "Fourth row_TEXT",
      "Ninth row",
      "Second row",
      "Second row",
      "Fourth row",
      "Fourth row",
      "Fourth row_TEXT",
      "Ninth row",
      "Ninth row",
      "Second row",
      "Second row",
      "Fourth row",
      "Fourth row",
      "Fourth row_TEXT",
      "Ninth row",
      "Ninth row"
    )
  )
})

test_that("horizontal sliders generate one response column per slider item", {
  raw_metadata <- synthetic_slider_raw_metadata()
  normalised_metadata <- normalise_qualtrics_metadata(raw_metadata)

  dict <- variable_dictionary_from_normalised_metadata(
    normalised_metadata,
    use_semantic_name = FALSE,
    block_pattern = NULL,
    block_sep = ".",
    semantic_name_preprocess = NULL
  )

  expect_equal(dict$response_column_id, c("QID1_1", "QID1_2", "QID1_3"))
  expect_equal(unname(dict$level), c("1", "2", "3"))
  expect_true(all(lengths(dict) == nrow(dict)))
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

  target_rows <- dict[grepl("QID2", dict$response_column_id), ]

  expect_equal(
    target_rows$response_column_id,
    c("x1_QID2_TEXT", "x2_QID2_TEXT")
  )
  expect_equal(target_rows$qid, c("QID2", "QID2"))
  expect_equal(target_rows$question_name, c("Q2", "Q2"))
  expect_equal(target_rows$variable_name, c("Q2", "Q2.1"))
  expect_equal(
    target_rows$question,
    c("Why did you choose Apples?", "Why did you choose Bananas?")
  )
  expect_equal(target_rows$loop_option, c("Apples", "Bananas"))
  expect_true(all(is.na(target_rows$item)))
  expect_equal(target_rows$type, c("TE", "TE"))
  expect_equal(target_rows$selector, c("SL", "SL"))
  expect_equal(attr(dict, "survey_name"), "Loop Survey")
  expect_equal(attr(dict, "surveyID"), "SV_LOOP")
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

  target_rows <- dict[grepl("QID2", dict$response_column_id), ]

  expect_equal(
    target_rows$response_column_id,
    c("x1_QID2_TEXT", "x2_QID2_TEXT")
  )
  expect_equal(
    target_rows$question,
    c("Compare Apples with Red fruit", "Compare Bananas with Yellow fruit")
  )
  expect_equal(target_rows$loop_option, c("Apples", "Bananas"))
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

  target_rows <- dict[grepl("QID2", dict$response_column_id), ]

  expect_equal(
    target_rows$response_column_id,
    c("1_QID2_TEXT", "2_QID2_TEXT")
  )
  expect_equal(
    target_rows$question,
    c("Compare Apples with Red fruit", "Compare Bananas with Yellow fruit")
  )
  expect_equal(target_rows$loop_option, c("Apples", "Bananas"))
})

test_that("looped MC text columns keep loop prefix before QID", {
  raw_metadata <- synthetic_looped_mc_text_raw_metadata()
  normalised_metadata <- normalise_qualtrics_metadata(raw_metadata)

  dict <- variable_dictionary_from_normalised_metadata(
    normalised_metadata,
    use_semantic_name = FALSE,
    block_pattern = NULL,
    block_sep = ".",
    semantic_name_preprocess = NULL
  )

  target_rows <- dict[grepl("QID2", dict$response_column_id), ]

  expect_equal(
    target_rows$response_column_id,
    c("x1_QID2", "x1_QID2", "x1_QID2_2_TEXT",
      "x2_QID2", "x2_QID2", "x2_QID2_2_TEXT")
  )
  expect_equal(target_rows$loop_option, rep(c("Apples", "Bananas"), each = 3))
  expect_equal(unname(target_rows$level), rep(c("1", "2", "2_TEXT"), 2))
})

test_that("numeric loop-prefixed text columns match raw export IDs", {
  raw_metadata <- synthetic_numeric_looped_text_raw_metadata()
  normalised_metadata <- normalise_qualtrics_metadata(raw_metadata)

  dict <- variable_dictionary_from_normalised_metadata(
    normalised_metadata,
    use_semantic_name = FALSE,
    block_pattern = NULL,
    block_sep = ".",
    semantic_name_preprocess = NULL
  )

  target_rows <- dict[grepl("QID3", dict$response_column_id), ]

  expect_equal(
    target_rows$response_column_id,
    paste0(1:12, "_QID3_TEXT")
  )
  expect_equal(target_rows$loop_option, paste("Loop", 1:12))
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

  target_rows <- dict[grepl("QID3", dict$response_column_id), ]

  expect_equal(
    target_rows$response_column_id,
    paste0(1:12, "_QID3_TEXT")
  )
  expect_false(any(grepl("^QID[0-9]+_QID3_TEXT$", target_rows$response_column_id))) # nolint
  expect_equal(target_rows$loop_option, as.character(1:12))
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

  target_rows <- dict[grepl("QID2", dict$response_column_id), ]

  expect_equal(
    target_rows$response_column_id,
    c("1_QID2_TEXT", "2_QID2_TEXT", "3_QID2_TEXT")
  )
  expect_false(any(grepl("^QID1_QID2_TEXT$", target_rows$response_column_id)))
  expect_equal(
    target_rows$loop_option,
    c("Condition 1", "Condition 2", "Condition 3")
  )
})

test_that("Labelled Survey Data matches loop-prefixed MC text columns", {
  raw_metadata <- synthetic_looped_mc_text_raw_metadata()
  normalised_metadata <- normalise_qualtrics_metadata(raw_metadata)
  dict <- variable_dictionary_from_normalised_metadata(
    normalised_metadata,
    use_semantic_name = FALSE,
    block_pattern = NULL,
    block_sep = ".",
    semantic_name_preprocess = NULL
  )

  attr(dict, "class") <- c("qualtdict", class(dict))
  survey <- tibble::tibble(
    externalDataReference = "R_1",
    startDate = "2026-06-01",
    endDate = "2026-06-01",
    x1_QID2 = "2",
    x1_QID2_2_TEXT = "Crisp",
    x2_QID2 = "1",
    x2_QID2_2_TEXT = NA_character_
  )

  labelled_data <- survey_recode(
    dict = dict,
    dat = survey,
    extra_columns = default_extra_columns(),
    unanswer_recode = NULL,
    unanswer_recode_multi = NULL
  )

  expect_named(labelled_data, c(
    "externalDataReference",
    "startDate",
    "endDate",
    "Q2",
    "Q2.1",
    "Q2.2",
    "Q2.3"
  ))
  expect_equal(unname(as.vector(labelled_data$Q2)), "2")
  expect_equal(unname(as.vector(labelled_data[["Q2.1"]])), "Crisp")
  expect_equal(unname(as.vector(labelled_data[["Q2.2"]])), "1")
  expect_equal(
    attr(labelled_data[["Q2.1"]], "label"),
    "Explain your Apples answer"
  )
})

test_that("Labelled Survey Data can match Loop and Merge response columns", {
  raw_metadata <- synthetic_loop_and_merge_raw_metadata()
  normalised_metadata <- normalise_qualtrics_metadata(raw_metadata)
  dict <- variable_dictionary_from_normalised_metadata(
    normalised_metadata,
    use_semantic_name = FALSE,
    block_pattern = NULL,
    block_sep = ".",
    semantic_name_preprocess = NULL
  )

  attr(dict, "class") <- c("qualtdict", class(dict))
  survey <- tibble::tibble(
    externalDataReference = "R_1",
    startDate = "2026-06-01",
    endDate = "2026-06-01",
    x1_QID2_TEXT = "Because they are crisp",
    x2_QID2_TEXT = "Because they are sweet"
  )

  labelled_data <- survey_recode(
    dict = dict,
    dat = survey,
    extra_columns = default_extra_columns(),
    unanswer_recode = NULL,
    unanswer_recode_multi = NULL
  )

  expect_named(labelled_data, c(
    "externalDataReference",
    "startDate",
    "endDate",
    "Q2",
    "Q2.1"
  ))
  expect_equal(
    unname(as.vector(labelled_data[["Q2"]])),
    "Because they are crisp"
  )
  expect_equal(
    unname(as.vector(labelled_data[["Q2.1"]])),
    "Because they are sweet"
  )
  expect_equal(
    attr(labelled_data[["Q2"]], "label"),
    "Why did you choose Apples?"
  )
  expect_equal(
    attr(labelled_data[["Q2.1"]], "label"),
    "Why did you choose Bananas?"
  )
})

test_that("Labelled Survey Data uses variable_name matched by response column", {
  raw_metadata <- synthetic_mc_text_raw_metadata()
  raw_metadata$metadata$questions$QID1$questionName <- "1 Bad Name"
  normalised_metadata <- normalise_qualtrics_metadata(raw_metadata)
  dict <- variable_dictionary_from_normalised_metadata(
    normalised_metadata,
    use_semantic_name = FALSE,
    block_pattern = NULL,
    block_sep = ".",
    semantic_name_preprocess = NULL
  )
  attr(dict, "class") <- c("qualtdict", class(dict))
  survey <- tibble::tibble(
    externalDataReference = "R_1",
    startDate = "2026-06-01",
    endDate = "2026-06-01",
    QID1 = "1",
    QID1_3_TEXT = "Because"
  )

  labelled_data <- survey_recode(
    dict = dict,
    dat = survey,
    extra_columns = default_extra_columns(),
    unanswer_recode = NULL,
    unanswer_recode_multi = NULL
  )

  expect_named(labelled_data, c(
    "externalDataReference",
    "startDate",
    "endDate",
    "X1_Bad_Name",
    "X1_Bad_Name.1"
  ))
  expect_equal(unname(as.vector(labelled_data$X1_Bad_Name)), 1)
  expect_equal(unname(as.vector(labelled_data[["X1_Bad_Name.1"]])), "Because")
  expect_equal(attr(labelled_data[["X1_Bad_Name.1"]], "label"), "Choose one")
})

test_that("known non-question raw columns are explicitly classified", {
  raw_columns <- c(
    "SC_abcdef",
    "Cognitron_ID",
    "GAD7_SCORE",
    "PD_Eligible",
    "test",
    "QID1_TEXT_ANALYSIS_SENTIMENT",
    "QID1_4_TEXT_ANALYSIS_TOPICS",
    "QID126879611_x1"
  )

  classified <- classify_raw_response_columns(raw_columns)

  expect_equal(
    classified$classification,
    c(
      "scoring",
      "embedded_data",
      "scoring",
      "embedded_data",
      "embedded_data",
      "text_analysis",
      "text_analysis",
      NA_character_
    )
  )
  expect_equal(
    classified$details[1],
    "Qualtrics scoring column; not represented as a question dictionary row."
  )
})
