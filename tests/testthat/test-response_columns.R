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
      block = list(
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
      question = list(
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
      block = list(
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
      question = list(
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

test_that("render_response_columns renders display block response column ids", {
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

  expect_identical(
    rendered$response_column_id,
    "QID1"
  )
  expect_identical(rendered$question, "Intro text")
  expect_true(all(is.na(rendered$item)))
  expect_true(all(is.na(rendered$level)))
  expect_true(all(is.na(rendered$label)))
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
  "render_response_columns warns and falls back for unrendered shapes",
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

test_that("resolve_response_column_qid preserves current precedence and error", {
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
})

test_that("response column render facts unwrap non-SBS levels and labels only", {
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
})

test_that("response column renderer context preserves unsupported type fallback", {
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
})
