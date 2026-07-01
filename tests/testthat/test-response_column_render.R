test_that("render_response_columns preserves row-aligned output shape", {
  rendered <- render_response_column_fixture(
    synthetic_mc_text_raw_metadata(),
    "QID1"
  )

  expect_named(
    rendered,
    c("response_column_id", "question", "item", "level", "label")
  )
  expect_renderer_rows_aligned(rendered)
  expect_snapshot(compact_response_column_render(rendered))
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

test_that("render_response_columns preserves loop-prefixed base IDs", {
  rendered <- render_response_column_fixture(
    synthetic_looped_mc_text_raw_metadata(),
    "QID2",
    base_response_column_id = "x1_QID2"
  )

  expect_identical(
    rendered$response_column_id,
    c("x1_QID2", "x1_QID2", "x1_QID2_2_TEXT")
  )
  expect_snapshot(compact_response_column_render(rendered))
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

test_that("render context separates QID and Base Response Column ID", {
  question_fact <- normalise_qualtrics_metadata(
    synthetic_looped_mc_text_raw_metadata()
  )$questions$QID2

  shape <- response_column_shape(question_fact)
  question_type <- question_fact_question_type(question_fact)

  context <- new_response_column_render_context(
    question_fact = question_fact,
    base_response_column_id = "x1_QID2",
    shape = shape,
    question_type = question_type
  )

  expect_identical(context$base_response_column_id, "x1_QID2")
  expect_identical(context$question_fact$qid, "QID2")
})

test_that("resolve_base_response_column_id preserves precedence and error", {
  question_fact <- normalise_qualtrics_metadata(
    synthetic_mc_text_raw_metadata()
  )$questions$QID1

  expect_identical(resolve_base_response_column_id(question_fact), "QID1")
  expect_identical(
    resolve_base_response_column_id(question_fact, "x1_QID1"),
    "x1_QID1"
  )

  question_fact$qid <- NA_character_
  expect_error(
    resolve_base_response_column_id(question_fact),
    "`qid` is required to render response columns.",
    fixed = TRUE
  )
})

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

test_that("item-or-level rendering preserves choice ids with no item", {
  context <- list(
    base_response_column_id = "QIDX",
    render_facts = list(
      item = NULL,
      level = c(x1 = "1", x2 = "2")
    )
  )

  expect_identical(
    render_response_column_id_with_item_or_level_suffix(context),
    c("QIDX_x1", "QIDX_x2")
  )
})
