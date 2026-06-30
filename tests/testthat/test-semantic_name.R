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
  expect_identical(
    question_name_dict$variable_name,
    c("Q1", "Q1", "Q1", "Q1.1")
  )
  expect_true("semantic_name" %in% names(semantic_name_dict))
  expect_false(all(is.na(semantic_name_dict$semantic_name)))
  expect_identical(
    semantic_name_dict$semantic_name,
    c("choose_one", "choose_one", "choose_one", "choose_one.txt")
  )
  expect_identical(
    semantic_name_dict$variable_name,
    c("choose_one", "choose_one", "choose_one", "choose_one.txt")
  )
})


test_that("Semantic Names skip Embedded Data Fields after preprocessing", {
  raw_metadata <- synthetic_flat_embedded_data_raw_metadata()
  normalised_metadata <- normalise_qualtrics_metadata(raw_metadata)
  semantic_name_preprocess <- function(dict) {
    dict$row_source <- "question"
    dict$question <- "Renamed by hook"
    dict
  }

  dict <- variable_dictionary_from_normalised_metadata(
    normalised_metadata,
    use_semantic_name = TRUE,
    block_pattern = NULL,
    block_sep = ".",
    semantic_name_preprocess = semantic_name_preprocess
  )
  embedded_rows <- dict[dict$row_source == "embedded_data", ]

  expect_true(all(is.na(embedded_rows$semantic_name)))
  expect_identical(
    embedded_rows$variable_name,
    c("Source_Channel", "Q1")
  )
})


test_that("Semantic Names return early when no question rows are present", {
  dict <- generate_semantic_names(
    tibble::tibble(
      response_column_id = "Source",
      row_source = "embedded_data",
      variable_name = "Source"
    ),
    surveyID = "SV_SYNTHETIC",
    block_pattern = NULL,
    block_sep = ".",
    semantic_name_preprocess = NULL
  )

  expect_identical(dict$semantic_name, NA_character_)
  expect_identical(dict$variable_name, "Source")
})


test_that("Semantic Name generation quiets progress by default", {
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


test_that("semantic_name_preprocess receives full dictionary rows", {
  raw_metadata <- synthetic_loop_and_merge_raw_metadata()
  normalised_metadata <- normalise_qualtrics_metadata(raw_metadata)
  seen <- new.env(parent = emptyenv())
  seen$names <- NULL
  seen$loop_rows <- NULL
  semantic_name_preprocess <- function(dict) {
    seen$names <- names(dict)
    seen$loop_rows <- dict[
      dict$looping,
      c(
        "qid",
        "response_column_id",
        "row_source",
        "question_name",
        "block",
        "question",
        "looping_option",
        "item",
        "label",
        "type",
        "selector",
        "sub_selector",
        "content_type"
      )
    ]
    dict
  }

  variable_dictionary_from_normalised_metadata(
    normalised_metadata,
    use_semantic_name = TRUE,
    block_pattern = NULL,
    block_sep = ".",
    semantic_name_preprocess = semantic_name_preprocess
  )

  expect_true(all(
    c(
      "qid",
      "response_column_id",
      "row_source",
      "question_name",
      "block",
      "question",
      "looping_question",
      "item",
      "level",
      "label",
      "type",
      "selector",
      "content_type",
      "sub_selector",
      "looping_option",
      "looping"
    ) %in%
      seen$names
  ))
  expect_identical(
    seen$loop_rows$response_column_id,
    c("x1_QID2_TEXT", "x2_QID2_TEXT")
  )
  expect_identical(seen$loop_rows$question_name, c("Q2", "Q2"))
  expect_identical(seen$loop_rows$row_source, c("question", "question"))
  expect_identical(seen$loop_rows$looping_option, c("Apples", "Bananas"))
})


test_that("Semantic Name keyword extraction quiets progress bars", {
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

  expect_identical(
    dict$semantic_name,
    c(
      "felt_lot_energy",
      "felt_lot_energy",
      "felt_lot_energy",
      "felt_lot_energy.txt"
    )
  )
})


test_that("Semantic Names fall back from missing multiple-answer labels", {
  dict_rows <- tibble(
    qid = c("QID1", "QID1217"),
    response_column_id = c("QID1", "QID1217_TEXT"),
    question_name = c("Q1", "TOC"),
    block = "Main Block",
    question = c("Choose one", "Missing text entry source should fall back"),
    looping_question = NA_character_,
    item = NA_character_,
    level = c("1", NA_character_),
    label = c("Yes", NA_character_),
    type = "MC",
    selector = c("SAVR", "MACOL"),
    content_type = c("Number", "Text"),
    sub_selector = "TX",
    looping_option = NA_character_,
    looping = FALSE
  )

  expect_no_error(
    dict <- generate_semantic_names(
      dict_rows,
      surveyID = "SV_TEST",
      block_pattern = NULL,
      block_sep = ".",
      semantic_name_preprocess = NULL
    )
  )
  expect_identical(
    dict$semantic_name,
    c("choose_one", "missing_text_entry_source_should_fall_back.txt")
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

    paste0(
      tempdir(),
      "/",
      hash(list(
        algorithm = "semantic-name-source-order-v1",
        unique_texts = unique_texts,
        all_words = all_words
      )),
      ".rds"
    )
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
  capture <- new.env(parent = emptyenv())
  capture$calls <- 0
  semantic_name_preprocess <- function(dict) {
    capture$calls <- capture$calls + 1
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
  expect_identical(capture$calls, 0)

  dict <- variable_dictionary_from_normalised_metadata(
    normalised_metadata,
    use_semantic_name = TRUE,
    block_pattern = NULL,
    block_sep = ".",
    semantic_name_preprocess = semantic_name_preprocess
  )

  expect_identical(capture$calls, 1)
  expect_true(all(grepl("^renamed_by_hook", dict$semantic_name)))
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


test_that("semantic-name dictionaries use final variable_name repair", {
  raw_metadata <- synthetic_mc_text_raw_metadata()
  raw_metadata$metadata$questions$QID1$questionText <- "123 bad name"
  raw_metadata$metadata$questions$QID2 <- raw_metadata$metadata$questions$QID1
  raw_metadata$metadata$questions$QID2$questionName <- "Q2"
  raw_metadata$description$blocks$BL_1$BlockElements <- list(
    list(QuestionID = "QID1"),
    list(QuestionID = "QID2")
  )
  raw_metadata$description$questions$QID2 <-
    raw_metadata$description$questions$QID1
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

  expect_identical(
    dict$semantic_name[first_rows],
    c("123_bad_name", "123_bad_name.txt", "123_bad_name", "123_bad_name.txt")
  )
  expect_identical(
    dict$variable_name[first_rows],
    c(
      "X123_bad_name",
      "X123_bad_name.txt",
      "X123_bad_name.1",
      "X123_bad_name.txt.1"
    )
  )
  expect_identical(
    findings$response_column_id,
    c("QID1", "QID1_3_TEXT", "QID2", "QID2_3_TEXT")
  )
  expect_identical(
    findings$finding,
    rep("repaired_variable_name", 4)
  )
  expect_identical(
    findings$original_candidate,
    dict$semantic_name[first_rows]
  )
  expect_identical(findings$variable_name, dict$variable_name[first_rows])
  expect_identical(
    findings$reason,
    c("unsafe", "unsafe", "unsafe;duplicate", "unsafe;duplicate")
  )
})
