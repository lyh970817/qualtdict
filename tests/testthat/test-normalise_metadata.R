test_that("raw Qualtrics metadata normalises into package-owned metadata", {
  raw_metadata <- synthetic_mc_text_raw_metadata()

  normalised_metadata <- normalise_qualtrics_metadata(raw_metadata)

  expect_s3_class(raw_metadata, "qualtdict_raw_metadata")
  expect_s3_class(normalised_metadata, "qualtdict_normalised_metadata")
  expect_named(
    normalised_metadata,
    c(
      "surveyID",
      "survey_name",
      "questions",
      "embedded_data",
      "scoring",
      "text_analysis"
    )
  )
  expect_identical(normalised_metadata$surveyID, "SV_SYNTHETIC")
  expect_identical(normalised_metadata$survey_name, "Synthetic Survey")
  expect_s3_class(
    normalised_metadata$questions,
    "qualtdict_normalised_questions"
  )
  expect_named(normalised_metadata$questions, "QID1")
  expect_s3_class(
    normalised_metadata$embedded_data,
    "qualtdict_normalised_embedded_data_fields"
  )
  expect_length(normalised_metadata$embedded_data, 0)
  expect_s3_class(
    normalised_metadata$scoring,
    "qualtdict_normalised_scoring_variables"
  )
  expect_length(normalised_metadata$scoring, 0)
  expect_s3_class(
    normalised_metadata$text_analysis,
    "qualtdict_normalised_text_analysis_sidecars"
  )
  expect_length(normalised_metadata$text_analysis, 0)

  question <- normalised_metadata$questions$QID1
  expect_s3_class(question, "qualtdict_normalised_question")
  expect_identical(question$survey_block, "Main Block")
  expect_identical(question$content_type, "Number")
  expect_null(question$looping_qid)
  expect_identical(question$question_name, "Q1")
  expect_identical(question$question_text, "Choose one")
  expect_identical(question$question_type$type, "MC")
  expect_identical(question$question_type$selector, "SAVR")
  expect_identical(question$question_type$sub_selector, "TX")
  expect_named(question$response_choices, c("1", "2", "3"))
  expect_length(question$response_items, 0)
  expect_length(question$column_facts, 0)
})

test_that("Text-analysis Sidecars normalise with parent question context", {
  raw_metadata <- synthetic_text_analysis_raw_metadata()

  text_analysis <- normalise_qualtrics_metadata(raw_metadata)$text_analysis

  expect_s3_class(
    text_analysis,
    "qualtdict_normalised_text_analysis_sidecars"
  )
  expect_named(
    text_analysis,
    c("Q1 Other - Sentiment", "Q1 Other - Parent Topics")
  )
  expect_s3_class(
    text_analysis[["Q1 Other - Sentiment"]],
    "qualtdict_normalised_text_analysis_sidecar"
  )
  expect_identical(
    text_analysis[["Q1 Other - Sentiment"]]$response_column_id,
    "QID1_3_TEXT_SENTIMENT"
  )
  expect_identical(text_analysis[["Q1 Other - Sentiment"]]$parent_qid, "QID1")
  expect_identical(
    text_analysis[["Q1 Other - Sentiment"]]$parent_question_name,
    "Q1"
  )
  expect_identical(
    text_analysis[["Q1 Other - Sentiment"]]$parent_block,
    "Main Block"
  )
  expect_identical(
    text_analysis[["Q1 Other - Parent Topics"]]$response_column_id,
    "QID1_3_e476cefa310845248231594eParTopics"
  )
  expect_identical(
    text_analysis[["Q1 Other - Parent Topics"]]$sidecar_name,
    "Q1 Other - Parent Topics"
  )
})

test_that("Text-analysis Sidecars normalise from response column maps", {
  raw_metadata <- synthetic_mc_text_raw_metadata()
  raw_metadata$response_column_map <- tibble::tibble(
    ImportId = c(
      "QID1_3_TEXT",
      "QID1_3_TEXT_SENTIMENT",
      "QID1_3_e476cefa310845248231594eParTopics"
    ),
    description = c(
      "Choose one - Other",
      "Q1 Other - Sentiment",
      "Q1 Other - Parent Topics"
    ),
    main = c("Choose one", "Q1 Other", "Q1 Other"),
    sub = c("", "Sentiment", "Parent Topics")
  )

  text_analysis <- normalise_qualtrics_metadata(raw_metadata)$text_analysis

  expect_named(
    text_analysis,
    c("Q1 Other - Sentiment", "Q1 Other - Parent Topics")
  )
  expect_identical(
    text_analysis[["Q1 Other - Sentiment"]]$response_column_id,
    "QID1_3_TEXT_SENTIMENT"
  )
  expect_identical(
    text_analysis[["Q1 Other - Parent Topics"]]$response_column_id,
    "QID1_3_e476cefa310845248231594eParTopics"
  )
  expect_identical(text_analysis[["Q1 Other - Sentiment"]]$parent_qid, "QID1")
  expect_identical(
    text_analysis[["Q1 Other - Sentiment"]]$parent_question_name,
    "Q1"
  )
  expect_identical(
    text_analysis[["Q1 Other - Sentiment"]]$parent_block,
    "Main Block"
  )
})

test_that("Text-analysis Sidecars use column-map classification", {
  raw_metadata <- synthetic_column_map_sidecar_raw_metadata()

  text_analysis <- normalise_qualtrics_metadata(raw_metadata)$text_analysis
  text_analysis_ids <- map_chr(text_analysis, "response_column_id")

  expect_setequal(
    text_analysis_ids,
    c(
      glad_sa6_text_analysis_sidecar_ids(),
      edgi_signup_text_analysis_sidecar_ids()
    )
  )
  expect_length(text_analysis_ids, 15)
  expect_false("QID508_TEXT" %in% text_analysis_ids)
  expect_false("QID626_TEXT" %in% text_analysis_ids)
  expect_false("QID429_TEXT" %in% text_analysis_ids)
  expect_false("QID700_1" %in% text_analysis_ids)
  expect_false("QID121_1" %in% text_analysis_ids)
  expect_false("QID700_DO_1" %in% text_analysis_ids)
  expect_false("x27_QID700_1" %in% text_analysis_ids)
  expect_false("8_QID508_TEXT" %in% text_analysis_ids)
  expect_identical(text_analysis[[1]]$parent_qid, "QID694")
  expect_identical(text_analysis[[14]]$parent_qid, "QID121")
})

test_that("Text-analysis Sidecars do not require known metric suffixes", {
  raw_metadata <- synthetic_column_map_sidecar_raw_metadata()
  raw_metadata$response_column_map <- tibble::tibble(
    qname = "QID694_TEXT_9079b4e7_gaygfh795aeaModelLabel",
    ImportId = "QID694_TEXT_9079b4e7_gaygfh795aeaModelLabel",
    description = "CAM.TV.1txt.0 - Model Label",
    main = "CAM.TV.1txt.0",
    sub = "Model Label"
  )

  text_analysis <- normalise_qualtrics_metadata(raw_metadata)$text_analysis

  expect_named(text_analysis, "CAM.TV.1txt.0 - Model Label")
  expect_identical(
    text_analysis[[1]]$response_column_id,
    "QID694_TEXT_9079b4e7_gaygfh795aeaModelLabel"
  )
})

test_that("Text-analysis Sidecar column-map helpers normalise", {
  expect_identical(
    classify_response_column_map(
      tibble::tibble(),
      questions = list(),
      embedded_data = list(),
      scoring = list()
    ),
    empty_response_column_map_classification()
  )
  expect_identical(
    ordinary_question_response_column_ids(list()),
    character()
  )
  expect_identical(
    text_analysis_sidecars_from_response_column_map(NULL),
    list()
  )
  expect_identical(
    response_column_map_ids(tibble::tibble(other = "QID1_TEXT_SENTIMENT")),
    character()
  )
  expect_null(
    normalise_text_analysis_sidecar(
      list(
        sidecar_name = "",
        response_column_id = "QID1_TEXT_SENTIMENT",
        parent_qid = "QID1"
      ),
      questions = list()
    )
  )
  expect_identical(
    text_analysis_sidecar_parent_context(NA_character_, list()),
    list(
      parent_qid = NA_character_,
      parent_question_name = NA_character_,
      parent_block = NA_character_
    )
  )

  classification <- tibble::tibble(
    response_column_id = "QID1_TEXT_SENTIMENT",
    row_source = "text_analysis",
    parent_qid = "QID1",
    display_name = "Q1 Sentiment",
    main = "Q1",
    sub = "Sentiment",
    description = "Q1 Sentiment",
    reason = "derived_question"
  )
  records <- text_analysis_sidecars_from_response_column_map(classification)

  expect_length(records, 1)
  expect_identical(
    records[[1]][c("sidecar_name", "response_column_id", "parent_qid")],
    list(
      sidecar_name = "Q1 Sentiment",
      response_column_id = "QID1_TEXT_SENTIMENT",
      parent_qid = "QID1"
    )
  )
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
    "row_source",
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

  expect_identical(
    dict_subset$response_column_id,
    c("QID1", "QID1", "QID1", "QID1_3_TEXT")
  )
  expect_identical(dict_subset$row_source, rep("question", 4))
  expect_identical(dict_subset$qid, rep("QID1", 4))
  expect_identical(
    dict_subset$question_name,
    c("Q1", "Q1", "Q1", "Q1")
  )
  expect_identical(
    dict_subset$variable_name,
    c("Q1", "Q1", "Q1", "Q1.1")
  )
  expect_identical(dict_subset$block, rep("Main Block", 4))
  expect_identical(dict_subset$question, rep("Choose one", 4))
  expect_true(all(is.na(dict_subset$item)))
  expect_identical(unname(dict_subset$level), c("1", "2", "3", "3_TEXT"))
  expect_identical(
    unname(dict_subset$label),
    c("Yes", "No", "Other", "Other_TEXT")
  )
  expect_identical(dict_subset$type, rep("MC", 4))
  expect_identical(dict_subset$selector, rep("SAVR", 4))
  expect_identical(dict_subset$sub_selector, rep("TX", 4))
  expect_identical(dict_subset$content_type, rep("Number", 4))
  expect_identical(attr(dict, "survey_name"), "Synthetic Survey")
  expect_identical(attr(dict, "surveyID"), "SV_SYNTHETIC")
})

test_that("question-name Variable Dictionaries repair only variable_name", {
  raw_metadata <- synthetic_mc_text_raw_metadata()
  raw_metadata$metadata$questions$QID1$questionName <- "1 Bad Name"
  raw_metadata$metadata$questions$QID2 <- raw_metadata$metadata$questions$QID1
  raw_metadata$metadata$questions$QID2$questionName <- "1 Bad Name"
  raw_metadata$description$blocks$BL_1$BlockElements <- list(
    list(QuestionID = "QID1"),
    list(QuestionID = "QID2")
  )
  raw_metadata$description$questions$QID2 <-
    raw_metadata$description$questions$QID1
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

  expect_identical(
    dict$question_name[first_rows],
    c("1 Bad Name", "1 Bad Name", "1 Bad Name", "1 Bad Name")
  )
  expect_identical(
    dict$variable_name[first_rows],
    c("X1_Bad_Name", "X1_Bad_Name.1", "X1_Bad_Name.2", "X1_Bad_Name.3")
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
  expect_identical(
    findings$finding,
    rep("repaired_variable_name", 4)
  )
  expect_identical(
    findings$response_column_id,
    c("QID1", "QID1_3_TEXT", "QID2", "QID2_3_TEXT")
  )
  expect_identical(
    findings$original_candidate,
    c("1 Bad Name", "1 Bad Name", "1 Bad Name", "1 Bad Name")
  )
  expect_identical(findings$variable_name, dict$variable_name[first_rows])
  expect_identical(
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

test_that("multiple-answer columns use numeric choice recodes", {
  raw_metadata <- synthetic_mc_recode_raw_metadata()
  normalised_metadata <- normalise_qualtrics_metadata(raw_metadata)

  dict <- variable_dictionary_from_normalised_metadata(
    normalised_metadata,
    use_semantic_name = FALSE,
    block_pattern = NULL,
    block_sep = ".",
    semantic_name_preprocess = NULL
  )

  expect_identical(
    dict$response_column_id,
    c(
      "QID1_1",
      "QID1_2",
      "QID1_-88",
      "QID1_-99",
      "QID1_0",
      "QID1_9_TEXT"
    )
  )
  expect_identical(
    unname(dict$level),
    c("1", "2", "-88", "-99", "0", "0_TEXT")
  )
  expect_true(all(lengths(dict) == nrow(dict)))
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

  expect_identical(
    unique(dict$response_column_id),
    c("QID1_x1", "QID1_x2")
  )
  expect_identical(dict$qid, rep("QID1", 4))
  expect_identical(
    unname(dict$item),
    c("Apples", "Apples", "Bananas", "Bananas")
  )
  expect_identical(unname(dict$level), c("1", "2", "1", "2"))
  expect_identical(unname(dict$label), c("Low", "High", "Low", "High"))
})

test_that("multiple-answer response columns use recodes for x choice IDs", {
  raw_metadata <- synthetic_mc_x_choice_raw_metadata()
  normalised_metadata <- normalise_qualtrics_metadata(raw_metadata)

  dict <- variable_dictionary_from_normalised_metadata(
    normalised_metadata,
    use_semantic_name = FALSE,
    block_pattern = NULL,
    block_sep = ".",
    semantic_name_preprocess = NULL
  )

  expect_identical(
    dict$response_column_id,
    paste0("QID126879611_", c("1", "2", "3", "4", "6"))
  )
  expect_identical(unname(dict$level), c("1", "2", "3", "4", "6"))
  expect_identical(
    unname(dict$label),
    paste("Choice", c("1", "2", "3", "4", "6"))
  )
})

test_that("non-analysed multiple-answer choices are not response columns", {
  raw_metadata <- synthetic_mc_recode_raw_metadata()
  raw_metadata$metadata$questions$QID1$choices[["8"]]$analyze <- FALSE

  dict <- variable_dictionary_from_normalised_metadata(
    normalise_qualtrics_metadata(raw_metadata),
    use_semantic_name = FALSE,
    block_pattern = NULL,
    block_sep = ".",
    semantic_name_preprocess = NULL
  )

  expect_false("QID1_-99" %in% dict$response_column_id)
  expect_false("-99" %in% dict$level)
  expect_true(all(lengths(dict) == nrow(dict)))
})

test_that("non-analysed single-answer choices remain value labels", {
  raw_metadata <- synthetic_mc_text_raw_metadata()
  raw_metadata$metadata$questions$QID1$choices[["1"]]$analyze <- FALSE

  dict <- variable_dictionary_from_normalised_metadata(
    normalise_qualtrics_metadata(raw_metadata),
    use_semantic_name = FALSE,
    block_pattern = NULL,
    block_sep = ".",
    semantic_name_preprocess = NULL
  )

  expect_true("QID1" %in% dict$response_column_id)
  expect_true("1" %in% dict$level)
  expect_true("Yes" %in% dict$label)
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

  expect_true(all(
    c(
      "QID2#3_2_1",
      "QID2#3_2_2",
      "QID2#3_4_1",
      "QID2#3_4_2"
    ) %in%
      dict$response_column_id
  ))
  expect_identical(
    grep("^QID2#3_", dict$response_column_id, value = TRUE),
    c("QID2#3_2_1", "QID2#3_2_2", "QID2#3_4_1", "QID2#3_4_2")
  )
  expect_identical(
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

  expect_identical(
    dict$response_column_id,
    c("QID2_x1", "QID2_x2", "QID2_x3")
  )
  expect_identical(unname(dict$item), c("First row", "Second row", "Third row"))
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
  expect_identical(
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
  expect_identical(
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

  expect_identical(dict$response_column_id, c("QID1_1", "QID1_2", "QID1_3"))
  expect_identical(unname(dict$level), c("1", "2", "3"))
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

  target_rows <- dict[grepl("QID2", dict$response_column_id, fixed = TRUE), ]

  expect_identical(
    target_rows$response_column_id,
    c(
      "x1_QID2",
      "x1_QID2",
      "x1_QID2_2_TEXT",
      "x2_QID2",
      "x2_QID2",
      "x2_QID2_2_TEXT"
    )
  )
  expect_identical(
    target_rows$loop_option,
    rep(c("Apples", "Bananas"), each = 3)
  )
  expect_identical(unname(target_rows$level), rep(c("1", "2", "2_TEXT"), 2))
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

  target_rows <- dict[grepl("QID3", dict$response_column_id, fixed = TRUE), ]

  expect_identical(
    target_rows$response_column_id,
    paste0(1:12, "_QID3_TEXT")
  )
  expect_identical(target_rows$loop_option, paste("Loop", 1:12))
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

  expect_named(
    labelled_data,
    c(
      "externalDataReference",
      "startDate",
      "endDate",
      "Q2",
      "Q2.1",
      "Q2.2",
      "Q2.3"
    )
  )
  expect_identical(unname(as.vector(labelled_data$Q2)), "2")
  expect_identical(unname(as.vector(labelled_data[["Q2.1"]])), "Crisp")
  expect_identical(unname(as.vector(labelled_data[["Q2.2"]])), "1")
  expect_identical(
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

  expect_named(
    labelled_data,
    c(
      "externalDataReference",
      "startDate",
      "endDate",
      "Q2",
      "Q2.1"
    )
  )
  expect_identical(
    unname(as.vector(labelled_data[["Q2"]])),
    "Because they are crisp"
  )
  expect_identical(
    unname(as.vector(labelled_data[["Q2.1"]])),
    "Because they are sweet"
  )
  expect_identical(
    attr(labelled_data[["Q2"]], "label"),
    "Why did you choose Apples?"
  )
  expect_identical(
    attr(labelled_data[["Q2.1"]], "label"),
    "Why did you choose Bananas?"
  )
})

test_that("Labelled Survey Data names by response column", {
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

  expect_named(
    labelled_data,
    c(
      "externalDataReference",
      "startDate",
      "endDate",
      "X1_Bad_Name",
      "X1_Bad_Name.1"
    )
  )
  expect_identical(unname(as.vector(labelled_data$X1_Bad_Name)), 1)
  expect_identical(
    unname(as.vector(labelled_data[["X1_Bad_Name.1"]])),
    "Because"
  )
  expect_identical(
    attr(labelled_data[["X1_Bad_Name.1"]], "label"),
    "Choose one"
  )
})
