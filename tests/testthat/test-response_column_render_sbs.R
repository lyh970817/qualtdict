test_that("SBS mixed columns preserve row metadata alignment", {
  rendered <- render_response_column_fixture(
    synthetic_sbs_text_subquestion_raw_metadata(),
    "QID2"
  )

  expect_renderer_rows_aligned(rendered)
  expect_snapshot(compact_response_column_render(rendered))
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

test_that("SBS carried-forward rows use subquestion response column IDs", {
  rendered <- render_response_column_fixture(
    synthetic_sbs_carried_forward_raw_metadata(),
    "QID2"
  )

  expect_identical(
    rendered$response_column_id,
    c("QID2_x1", "QID2_x2", "QID2_x3")
  )
  expect_snapshot(compact_response_column_render(rendered))
})

test_that("blank SBS row labels fall back to the subquestion recode", {
  question_fact <- normalise_qualtrics_metadata(
    synthetic_sbs_blank_row_raw_metadata()
  )$questions$QID2

  item_shape <- response_column_item_shape(question_fact)
  sbs_items <- response_column_sbs_item_shape(
    question_fact,
    item_shape$has_text_sub
  )

  expect_identical(unname(sbs_items), c("1", "2"))
  expect_named(sbs_items, c("1", "2"))
})

test_that("blank-label medication-form rows render distinct item identity", {
  raw_metadata <- synthetic_sbs_blank_row_raw_metadata()
  normalised_metadata <- normalise_qualtrics_metadata(raw_metadata)

  dict <- variable_dictionary_from_normalised_metadata(
    normalised_metadata,
    use_semantic_name = FALSE,
    block_pattern = NULL,
    block_sep = ".",
    semantic_name_preprocess = NULL
  )

  rows <- grepl("^QID2#1_", dict$response_column_id)
  expect_identical(
    dict$response_column_id[rows],
    c("QID2#1_1_1", "QID2#1_2_1")
  )
  expect_identical(unname(dict$item[rows]), c("1", "2"))
  expect_length(unique(dict$item[rows]), 2L)
})

test_that("duplicated SBS column text is qualified by its adjacent partner", {
  raw_metadata <- synthetic_sbs_duplicate_column_raw_metadata()
  normalised_metadata <- normalise_qualtrics_metadata(raw_metadata)

  dict <- variable_dictionary_from_normalised_metadata(
    normalised_metadata,
    use_semantic_name = FALSE,
    block_pattern = NULL,
    block_sep = ".",
    semantic_name_preprocess = NULL
  )

  question_for <- function(id) {
    unname(dict$question[dict$response_column_id == id][[1]])
  }

  expect_identical(
    question_for("QID2#2_1_1"),
    "Family history Coronary artery disease — Age at diagnosis"
  )
  expect_identical(
    question_for("QID2#4_1_1"),
    "Family history Stroke — Age at diagnosis"
  )
  # The distinct disease Likert columns keep their unqualified question text.
  expect_identical(
    question_for("QID2#1_1"),
    "Family history Coronary artery disease"
  )
  expect_identical(question_for("QID2#3_1"), "Family history Stroke")
})

test_that("duplicated SBS column with no partner falls back to an ordinal", {
  raw_metadata <- synthetic_sbs_ordinal_column_raw_metadata()
  normalised_metadata <- normalise_qualtrics_metadata(raw_metadata)

  dict <- variable_dictionary_from_normalised_metadata(
    normalised_metadata,
    use_semantic_name = FALSE,
    block_pattern = NULL,
    block_sep = ".",
    semantic_name_preprocess = NULL
  )

  question_for <- function(id) {
    unname(dict$question[dict$response_column_id == id][[1]])
  }

  expect_identical(
    question_for("QID2#1_1_1"),
    "Family history Age at diagnosis (column 1)"
  )
  expect_identical(
    question_for("QID2#2_1_1"),
    "Family history Age at diagnosis (column 2)"
  )
})

sbs_grid_question_by_column <- function(raw_metadata) {
  dict <- variable_dictionary_from_normalised_metadata(
    normalise_qualtrics_metadata(raw_metadata),
    use_semantic_name = FALSE,
    block_pattern = NULL,
    block_sep = ".",
    semantic_name_preprocess = NULL
  )

  function(column) {
    prefix <- paste0("^QID2#", column, "_")
    unname(dict$question[grepl(prefix, dict$response_column_id)][[1]])
  }
}

test_that("reversed-order grids pair the age column with its true anchor", {
  question_for <- sbs_grid_question_by_column(
    synthetic_sbs_reversed_column_raw_metadata()
  )

  # The trailing age column (#3) must carry Stroke, not the nearer Coronary.
  expect_identical(
    question_for(1),
    "Family history Coronary artery disease — Age at diagnosis"
  )
  expect_identical(
    question_for(3),
    "Family history Stroke — Age at diagnosis"
  )
})

test_that("duplicated anchor columns fall back to the ordinal", {
  question_for <- sbs_grid_question_by_column(
    synthetic_sbs_duplicate_anchor_raw_metadata()
  )

  expect_identical(
    question_for(2),
    "Family history Age at diagnosis (column 2)"
  )
  expect_identical(
    question_for(4),
    "Family history Age at diagnosis (column 4)"
  )
})

test_that("shared value columns fall back to the ordinal", {
  question_for <- sbs_grid_question_by_column(
    synthetic_sbs_shared_value_column_raw_metadata()
  )

  expect_identical(
    question_for(3),
    "Family history Age at diagnosis (column 3)"
  )
  expect_identical(
    question_for(4),
    "Family history Age at diagnosis (column 4)"
  )
})

test_that("longer repeating units pair on the unambiguous left direction", {
  question_for <- sbs_grid_question_by_column(
    synthetic_sbs_longer_unit_column_raw_metadata()
  )

  expect_identical(
    question_for(2),
    "Family history Diabetes type 1 — Age at diagnosis"
  )
  expect_identical(
    question_for(5),
    "Family history Diabetes type 2 — Age at diagnosis"
  )
})

test_that("ambiguous direction grids fall back to the ordinal", {
  question_for <- sbs_grid_question_by_column(
    synthetic_sbs_ambiguous_column_raw_metadata()
  )

  expect_identical(
    question_for(2),
    "Family history Age at diagnosis (column 2)"
  )
  expect_identical(
    question_for(4),
    "Family history Age at diagnosis (column 4)"
  )
})

test_that("side-by-side qualification helpers handle edge inputs", {
  expect_true(is_blank_sbs_item_label(NULL))
  expect_true(is_blank_sbs_item_label(NA_character_))
  expect_true(is_blank_sbs_item_label("&nbsp;"))
  expect_false(is_blank_sbs_item_label("Mother"))

  expect_null(sbs_fill_blank_item_labels(NULL, list()))
  expect_identical(sbs_fill_blank_item_labels(character(), list()), character())

  expect_identical(sbs_normalise_column_text(NULL), "")
  expect_identical(sbs_normalise_column_text(NA_character_), "")
  expect_identical(sbs_normalise_column_text("&nbsp;"), "")

  # Missing column_position falls back to list order for the ordinal.
  no_position <- list(
    list(question_text = "Age", question_type = list(selector = "TE")),
    list(question_text = "Age", question_type = list(selector = "TE"))
  )
  expect_identical(
    sbs_column_qualified_texts(no_position),
    list("Age (column 1)", "Age (column 2)")
  )

  # A unique neighbour that shares the value column's selector is not an anchor.
  same_selector <- list(
    list(
      question_text = "Notes",
      question_type = list(selector = "TE"),
      column_position = 1L
    ),
    list(
      question_text = "Age",
      question_type = list(selector = "TE"),
      column_position = 2L
    ),
    list(
      question_text = "Age",
      question_type = list(selector = "TE"),
      column_position = 3L
    )
  )
  expect_identical(
    sbs_column_qualified_texts(same_selector),
    list("Notes", "Age (column 2)", "Age (column 3)")
  )
})

test_that("SBS shape preparation carries text-entry item rows", {
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
