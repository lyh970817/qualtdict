synthetic_timing_raw_metadata <- function() {
  new_raw_qualtrics_metadata(
    surveyID = "SV_TIMING",
    metadata = list(
      metadata = list(name = "Timing Survey"),
      questions = list(
        QID1 = list(
          questionName = "Q1",
          questionType = list(
            type = "Timing",
            selector = "PageTimer",
            subSelector = NULL
          ),
          questionText = "Timing",
          blocks = list(),
          columns = list(),
          choices = list(),
          subQuestions = list()
        )
      )
    ),
    description = list(
      blocks = list(
        BL_1 = list(
          Description = "Main Block",
          BlockElements = list(
            list(QuestionID = "QID1")
          ),
          Options = list(
            LoopingOptions = list(
              Static = NULL,
              QID = NULL
            )
          )
        )
      ),
      questions = list(
        QID1 = list(
          Validation = list(
            Settings = list(ContentType = NULL)
          )
        )
      )
    )
  )
}

synthetic_file_upload_raw_metadata <- function() {
  new_raw_qualtrics_metadata(
    surveyID = "SV_FILE_UPLOAD",
    metadata = list(
      metadata = list(name = "File Upload Survey"),
      questions = list(
        QID1 = list(
          questionName = "Q1",
          questionType = list(
            type = "FileUpload",
            selector = "FileUpload",
            subSelector = NULL
          ),
          questionText = "Upload a file",
          blocks = list(),
          columns = list(),
          choices = list(),
          subQuestions = list()
        )
      )
    ),
    description = list(
      blocks = list(
        BL_1 = list(
          Description = "Main Block",
          BlockElements = list(
            list(QuestionID = "QID1")
          ),
          Options = list(
            LoopingOptions = list(
              Static = NULL,
              QID = NULL
            )
          )
        )
      ),
      questions = list(
        QID1 = list(
          Validation = list(
            Settings = list(ContentType = NULL)
          )
        )
      )
    )
  )
}

test_that("render_response_columns renders MC rows with aligned facts", {
  raw_metadata <- synthetic_mc_text_raw_metadata()
  question <- normalise_qualtrics_metadata(raw_metadata)$questions$QID1

  rendered <- render_response_columns(question, "QID1")

  expect_identical(
    rendered$response_column_id,
    c("QID1", "QID1", "QID1", "QID1_3_TEXT")
  )
  expect_identical(unname(rendered$level), c("1", "2", "3", "3_TEXT"))
  expect_identical(
    unname(rendered$label),
    c("Yes", "No", "Other", "Other_TEXT")
  )
  expect_true(all(vapply(rendered, length, integer(1)) == nrow(rendered)))
})

test_that(
  paste(
    "render_response_columns uses recode suffixes",
    "for x-named MC choices"
  ),
  {
    question <- normalise_qualtrics_metadata(
      synthetic_mc_x_choice_raw_metadata()
    )$questions$QID126879611

    rendered <- render_response_columns(question, "QID126879611")

    expect_identical(
      rendered$response_column_id,
      paste0("QID126879611_", c("1", "2", "3", "4", "6"))
    )
    expect_identical(unname(rendered$level), c("1", "2", "3", "4", "6"))
    expect_identical(
      unname(rendered$label),
      paste("Choice", c("1", "2", "3", "4", "6"))
    )
  }
)

test_that("render_response_columns skips display block questions", {
  question <- normalise_question_fact(
    qid = "QID1",
    question = list(
      questionName = "Intro",
      questionType = list(type = "DB", selector = "TB", subSelector = NULL),
      questionText = "Intro text",
      choices = list(),
      subQuestions = list(),
      columns = list()
    ),
    block = list(description = "Main Block"),
    content_type = NULL
  )

  rendered <- render_response_columns(question, "QID1")

  expect_identical(nrow(rendered), 0L)
  expect_named(
    rendered,
    c("response_column_id", "question", "item", "level", "label")
  )
})

test_that("render_response_columns accepts package-owned normalised facts", {
  raw_metadata <- synthetic_mc_text_raw_metadata()
  question <- normalise_qualtrics_metadata(raw_metadata)$questions$QID1
  question$questionName <- NULL
  question$questionText <- NULL
  question$questionType <- NULL
  question$block <- NULL
  question$choices <- NULL
  question$subQuestions <- NULL
  question$columns <- NULL

  rendered <- render_response_columns(question, "QID1")

  expect_identical(
    rendered$response_column_id,
    c("QID1", "QID1", "QID1", "QID1_3_TEXT")
  )
  expect_identical(unname(rendered$question), rep("Choose one", 4))
})

test_that("render_response_columns preserves already-prefixed loop qids", {
  raw_metadata <- synthetic_looped_mc_text_raw_metadata()
  question <- normalise_qualtrics_metadata(raw_metadata)$questions$QID2

  rendered <- render_response_columns(question, "x1_QID2")

  expect_identical(
    rendered$response_column_id,
    c("x1_QID2", "x1_QID2", "x1_QID2_2_TEXT")
  )
  expect_identical(unname(rendered$level), c("1", "2", "2_TEXT"))
  expect_true(all(vapply(rendered, length, integer(1)) == nrow(rendered)))
})

test_that("render_response_columns renders matrix rows in stable order", {
  raw_metadata <- synthetic_matrix_raw_metadata()
  question <- normalise_qualtrics_metadata(raw_metadata)$questions$QID1

  rendered <- render_response_columns(question, "QID1")

  expect_identical(
    rendered$response_column_id,
    c("QID1_x1", "QID1_x1", "QID1_x2", "QID1_x2")
  )
  expect_identical(
    unname(rendered$item),
    c("Apples", "Apples", "Bananas", "Bananas")
  )
  expect_identical(unname(rendered$level), c("1", "2", "1", "2"))
  expect_identical(unname(rendered$label), c("Low", "High", "Low", "High"))
  expect_true(all(vapply(rendered, length, integer(1)) == nrow(rendered)))
})

test_that("render_response_columns renders SBS and sidecar columns", {
  sbs_question <- normalise_qualtrics_metadata(
    synthetic_sbs_text_subquestion_raw_metadata()
  )$questions$QID2
  timing_question <- normalise_qualtrics_metadata(
    synthetic_timing_raw_metadata()
  )$questions$QID1
  file_upload_question <- normalise_qualtrics_metadata(
    synthetic_file_upload_raw_metadata()
  )$questions$QID1

  sbs_rendered <- render_response_columns(sbs_question, "QID2")
  timing_rendered <- render_response_columns(timing_question, "QID1")
  file_upload_rendered <- render_response_columns(file_upload_question, "QID1")

  expect_identical(
    sbs_rendered$response_column_id,
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
  expect_true(
    all(vapply(sbs_rendered, length, integer(1)) == nrow(sbs_rendered))
  )

  expect_identical(
    timing_rendered$response_column_id,
    c(
      "QID1_FIRST_CLICK",
      "QID1_LAST_CLICK",
      "QID1_PAGE_SUBMIT",
      "QID1_CLICK_COUNT"
    )
  )
  expect_identical(
    file_upload_rendered$response_column_id,
    c("QID1_FILE_ID", "QID1_FILE_NAME", "QID1_FILE_SIZE", "QID1_FILE_TYPE")
  )
})

test_that("render_response_columns renders carried-forward SBS row IDs", {
  question <- normalise_qualtrics_metadata(
    synthetic_sbs_carried_forward_raw_metadata()
  )$questions$QID2

  rendered <- render_response_columns(question, "QID2")

  expect_identical(
    rendered$response_column_id,
    c("QID2_x1", "QID2_x2", "QID2_x3")
  )
  expect_true(all(vapply(rendered, length, integer(1)) == nrow(rendered)))
})

test_that("render_response_columns renders mixed SBS column cases", {
  question <- normalise_qualtrics_metadata(
    synthetic_sbs_multiple_answer_raw_metadata()
  )$questions$QID2

  rendered <- render_response_columns(question, "QID2")

  expect_identical(
    rendered$response_column_id,
    c(
      "QID2#1_2_1",
      "QID2#1_4_1",
      "QID2#2_2",
      "QID2#2_4",
      "QID2#3_2_1",
      "QID2#3_2_2",
      "QID2#3_4_1",
      "QID2#3_4_2"
    )
  )
  expect_true(all(vapply(rendered, length, integer(1)) == nrow(rendered)))
})

test_that(
  paste(
    "render_response_columns warns and falls back",
    "for unrendered shapes"
  ),
  {
    raw_metadata <- synthetic_mc_text_raw_metadata()
    question <- normalise_qualtrics_metadata(raw_metadata)$questions$QID1
    question$question_type$type <- "Meta"
    question$question_type$selector <- "Browser"
    question$question_type$sub_selector <- NULL

    expect_warning(
      rendered <- render_response_columns(question, "QID1"),
      "without a specific response-column renderer"
    )
    expect_identical(rendered$response_column_id, "QID1")
  }
)

test_that("response column render context separates bare and rendering qids", {
  question_fact <- normalise_qualtrics_metadata(
    synthetic_looped_mc_text_raw_metadata()
  )$questions$QID2

  shape <- response_column_shape(question_fact)
  question_type <- question_fact_question_type(question_fact)

  context <- new_response_column_render_context(
    question_fact = question_fact,
    response_column_qid = "x1_QID2",
    shape = shape,
    question_type = question_type
  )

  expect_identical(context$response_column_qid, "x1_QID2")
  expect_identical(context$question_fact$qid, "QID2")
})

test_that(
  paste(
    "resolve_response_column_qid preserves current precedence",
    "and error"
  ),
  {
    question_fact <- normalise_qualtrics_metadata(
      synthetic_mc_text_raw_metadata()
    )$questions$QID1

    expect_identical(resolve_response_column_qid(question_fact), "QID1")
    expect_identical(
      resolve_response_column_qid(question_fact, "x1_QID1"),
      "x1_QID1"
    )

    question_fact$qid <- NA_character_
    expect_error(
      resolve_response_column_qid(question_fact),
      "`qid` is required to render response columns.",
      fixed = TRUE
    )
  }
)

test_that(
  paste(
    "response column render facts unwrap non-SBS levels",
    "and labels only"
  ),
  {
    mc_question_fact <- normalise_qualtrics_metadata(
      synthetic_mc_text_raw_metadata()
    )$questions$QID1
    sbs_question_fact <- normalise_qualtrics_metadata(
      synthetic_sbs_text_subquestion_raw_metadata()
    )$questions$QID2

    mc_shape <- response_column_shape(mc_question_fact)
    mc_facts <- response_column_render_facts(mc_shape, "MC")
    expect_identical(mc_facts$level, mc_shape$level[[1]])
    expect_identical(mc_facts$label, mc_shape$label[[1]])

    sbs_shape <- response_column_shape(sbs_question_fact)
    sbs_facts <- response_column_render_facts(sbs_shape, "SBS")
    expect_identical(sbs_facts$level, sbs_shape$level)
    expect_identical(sbs_facts$label, sbs_shape$label)
  }
)

test_that("response column choice shape carries text-entry levels and labels", {
  question_fact <- normalise_qualtrics_metadata(
    synthetic_mc_text_raw_metadata()
  )$questions$QID1

  shape <- response_column_choice_shape(question_fact)

  expect_identical(shape$level_len, 3L)
  expect_identical(unname(shape$level[[1]]), c("1", "2", "3", "3_TEXT"))
  expect_identical(
    unname(shape$label[[1]]),
    c("Yes", "No", "Other", "Other_TEXT")
  )
})

test_that("SBS row item shape carries text-entry item rows", {
  question_fact <- normalise_qualtrics_metadata(
    synthetic_sbs_text_subquestion_raw_metadata()
  )$questions$QID2

  item_shape <- response_column_item_shape(question_fact)
  sbs_items <- response_column_sbs_item_shape(
    question_fact,
    item_shape$has_text_sub
  )

  expect_identical(
    unname(sbs_items),
    c("Second row", "Fourth row", "Fourth row_TEXT", "Ninth row")
  )
  expect_named(sbs_items, c("2", "4", "4_TEXT", "9"))
})

test_that(
  paste(
    "response column renderer context preserves unsupported type",
    "fallback"
  ),
  {
    question_fact <- normalise_qualtrics_metadata(
      synthetic_mc_text_raw_metadata()
    )$questions$QID1
    question_fact$question_type <- list(
      type = "UnsupportedType",
      selector = "UnsupportedSelector",
      sub_selector = NULL
    )
    shape <- response_column_shape(question_fact)
    context <- new_response_column_render_context(
      question_fact = question_fact,
      response_column_qid = "QID1",
      shape = shape,
      question_type = question_fact$question_type
    )

    expect_warning(
      rendered <- render_response_column_ids(context),
      "QID1 uses a question type without a specific response-column renderer",
      fixed = TRUE
    )
    expect_identical(rendered, "QID1")
  }
)

test_that("item_or_level_qid preserves choice ids when there is no item", {
  context <- list(
    response_column_qid = "QIDX",
    render_facts = list(
      item = NULL,
      level = c(x1 = "1", x2 = "2")
    )
  )

  expect_identical(
    item_or_level_qid(context),
    c("QIDX_x1", "QIDX_x2")
  )
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
