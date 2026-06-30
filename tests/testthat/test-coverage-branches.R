coverage_test_dict <- function(
  response_column_id = "QID1",
  variable_name = "q1",
  label = "Yes",
  level = "1",
  item = NA_character_
) {
  dict <- tibble::tibble(
    response_column_id = response_column_id,
    row_source = "question",
    qid = sub("_.*$", "", response_column_id),
    question_name = variable_name,
    variable_name = variable_name,
    block = "Main Block",
    question = "Question text",
    item = item,
    level = level,
    label = label,
    type = "MC",
    selector = "SAVR",
    sub_selector = "TX",
    content_type = NA_character_
  )
  attr(dict, "class") <- c("qualtdict", class(dict))
  dict
}

test_that("dict_generate covers deprecated argument aliases", {
  local_mocked_bindings(
    fetch_dictionary_metadata = function(surveyID) {
      synthetic_mc_text_raw_metadata()
    }
  )

  expect_warning(
    expect_warning(
      semantic_dict <- dict_generate("SV_SYNTHETIC", name = "easy_name"),
      "`name` is deprecated"
    ),
    "`easy_name` is deprecated"
  )
  expect_true("semantic_name" %in% names(semantic_dict))

  expect_warning(
    question_dict <- dict_generate("SV_SYNTHETIC", name = "question_name"),
    "`name` is deprecated"
  )
  expect_false("semantic_name" %in% names(question_dict))

  semantic_preprocess <- function(dict) {
    dict$question <- "semantic branch question"
    dict
  }
  alias_preprocess <- function(dict) {
    dict$question <- "alias branch question"
    dict
  }

  expect_warning(
    dict <- dict_generate(
      "SV_SYNTHETIC",
      variable_name = "semantic_name",
      preprocess = alias_preprocess
    ),
    "`preprocess` is deprecated"
  )
  expect_true(any(grepl("^alias", dict$semantic_name)))

  expect_warning(
    dict <- dict_generate(
      "SV_SYNTHETIC",
      variable_name = "semantic_name",
      semantic_name_preprocess = semantic_preprocess,
      preprocess = alias_preprocess
    ),
    "`preprocess` is deprecated"
  )
  expect_true(any(grepl("^semantic", dict$semantic_name)))
})

test_that("generated dictionary finalisation supplies default findings", {
  dict <- coverage_test_dict()
  attr(dict, "variable_name_findings") <- NULL

  finalised <- finalise_generated_dictionary(dict, use_semantic_name = FALSE)

  expect_s3_class(finalised, "qualtdict")
  expect_s3_class(
    attr(finalised, "variable_name_findings", exact = TRUE),
    "data.frame"
  )
})

test_that("metadata fetching combines Qualtrics metadata endpoints", {
  metadata <- list(metadata = list(name = "Fetched Survey"))
  description <- list(blocks = list(), questions = list())
  local_mocked_bindings(
    metadata2 = function(surveyID, elements) {
      expect_identical(surveyID, "SV_FETCH")
      expect_true("responsecounts" %in% elements)
      metadata
    },
    fetch_description2 = function(surveyID, elements) {
      expect_identical(surveyID, "SV_FETCH")
      expect_false("responsecounts" %in% elements)
      expect_true("scoring" %in% elements)
      description
    },
    fetch_survey2 = function(...) {
      response_schema <- tibble::tibble(QID1 = "1")
      attr(response_schema, "column_map") <- tibble::tibble(
        ImportId = "QID1"
      )
      response_schema
    }
  )

  raw_metadata <- fetch_dictionary_metadata("SV_FETCH")

  expect_s3_class(raw_metadata, "qualtdict_raw_metadata")
  expect_identical(raw_metadata$survey_name, "Fetched Survey")
  expect_identical(raw_metadata$description, description)
  expect_identical(raw_metadata$response_column_map$ImportId, "QID1")
})

test_that("metadata fetching degrades without response column maps", {
  metadata <- list(metadata = list(name = "Fetched Survey"))
  description <- list(blocks = list(), questions = list())
  local_mocked_bindings(
    metadata2 = function(surveyID, elements) {
      metadata
    },
    fetch_description2 = function(surveyID, elements) {
      description
    },
    fetch_survey2 = function(...) {
      stop("response export denied", call. = FALSE)
    }
  )

  expect_warning(
    raw_metadata <- fetch_dictionary_metadata("SV_FETCH"),
    "Failed to fetch the Qualtrics response column map"
  )

  expect_s3_class(raw_metadata, "qualtdict_raw_metadata")
  expect_null(raw_metadata$response_column_map)
})

test_that("dictionary accessors support legacy dictionary columns", {
  dict <- tibble::tibble(qid = "QID1", name = "q1")

  expect_identical(dict_response_column_id(dict), "QID1")
  expect_identical(dict_variable_name(dict), "q1")
})

test_that("dict_validate reports non-quiet finding messages", {
  repaired <- coverage_test_dict(variable_name = "q1_repaired")
  attr(repaired, "variable_name_findings") <- tibble::tibble(
    response_column_id = "QID1",
    original_candidate = "q1 repaired",
    variable_name = "q1_repaired",
    reason = "unsafe"
  )

  capture.output(
    messages <- capture.output(
      invisible(dict_validate(repaired, quiet = FALSE)),
      type = "message"
    )
  )
  expect_match(
    paste(messages, collapse = "\n"),
    "Variable names were repaired"
  )

  level_issue <- coverage_test_dict(
    response_column_id = c("QID1", "QID1", "QID1"),
    variable_name = c("q1", "q1", "q1"),
    label = c("A", "A", "B"),
    level = c("1", "1", "3")
  )
  capture.output(
    messages <- capture.output(
      invisible(dict_validate(level_issue, quiet = FALSE)),
      type = "message"
    )
  )
  expect_match(
    paste(messages, collapse = "\n"),
    "potential incorrect level-label codings"
  )
})

test_that("Labelled Export helpers cover empty and recode branches", {
  expect_identical(
    labelled_export_findings(tibble::tibble(q1 = "1")),
    empty_labelled_export_findings()
  )

  dict <- coverage_test_dict(response_column_id = "QID2")
  dat <- tibble::tibble(externalDataReference = "R_1", QID1 = "1")
  recoded <- survey_recode(
    dict,
    dat,
    extra_columns = "externalDataReference",
    unanswer_recode = NULL,
    unanswer_recode_multi = NULL
  )
  expect_named(recoded, "externalDataReference")

  single_dict <- coverage_test_dict(label = "Selected", level = "1")
  single <- survey_var_recode(
    c("1", "0"),
    single_dict,
    unanswer_recode = NULL,
    unanswer_recode_multi = "0"
  )
  expect_identical(
    attr(single, "labels", exact = TRUE),
    c(Selected = "1", `Not Selected` = "0")
  )

  multi_dict <- coverage_test_dict(
    response_column_id = c("QID1", "QID1"),
    variable_name = c("q1", "q1"),
    label = c("Yes", "No"),
    level = c("1", "2")
  )
  multi <- survey_var_recode(
    c("1", "-99"),
    multi_dict,
    unanswer_recode = "-99",
    unanswer_recode_multi = NULL
  )
  expect_true("Seen but not answered" %in% names(attr(multi, "labels")))
})

test_that("Loop and Merge helpers cover missing and empty branches", {
  question_fact <- list(
    qid = "QID1",
    question_text = "Question ${lm://Field/1}",
    question_type = list(type = "TE", selector = "SL", sub_selector = NULL),
    looping_prefix = character(),
    looping_qid = "QID_MISSING",
    looping_static = NULL
  )
  context <- list(
    question_fact = question_fact,
    looping_qid = "QID_MISSING",
    looping_source_fact = NULL,
    looping_static = NULL,
    static_prefixes = character()
  )

  expect_false(loop_question_fact_should_expand(context))
  expect_false(expand_loop_question_fact(context)[[1]]$looping)

  no_rows_context <- context
  no_rows_context$looping_source_fact <- list(
    question_type = list(type = "MC", selector = "SAVR"),
    response_choices = NULL
  )
  expect_identical(
    expand_loop_question_fact(no_rows_context),
    list(
      no_rows_context$question_fact
    )
  )

  expect_null(loop_options_from_static_fields(NULL, character()))
  expect_identical(
    loop_choice_source(NULL, list(), "x1")$type,
    "missing"
  )
  expect_identical(
    loop_field_values_from_static(NULL, "x1"),
    list(x1 = NULL)
  )
  expect_identical(
    loop_field_values_from_static(list(x1 = list()), "x1"),
    list(x1 = character())
  )
  expect_identical(
    substitute_loop_fields(NA_character_, list(`1` = "A")),
    NA_character_
  )
})

test_that("Response Column ID helpers cover fallback shapes", {
  expect_identical(rep_qid("QID1", NULL, 2), c("QID1", "QID1"))
  expect_identical(
    rep_qid(c("QID1", "QID1"), c(x1 = "A", x2_TEXT = "Other"), 2),
    c("QID1", "QID1", "QID1")
  )
  expect_error(
    response_column_row_vector(c("a", "b"), row_count = 3),
    "not row-aligned"
  )
  local_mocked_bindings(null_na = function(x) list())
  expect_identical(
    response_column_row_vector(list("ignored"), row_count = 1),
    NA_character_
  )

  expect_identical(
    remove_non_exported_choice_columns(list(
      question_type = list(type = "MC", selector = "MAVR"),
      response_choices = list()
    )),
    list(
      question_type = list(type = "MC", selector = "MAVR"),
      response_choices = list()
    )
  )
  expect_identical(
    response_column_sbs_value_shape(list(), NULL, 1, "TE"),
    list(
      level_len = 1,
      level = list(NA_character_),
      label = list(NA_character_)
    )
  )
  expect_type(response_column_renderer_for_context(list(type = "TE")), "list")
  expect_identical(
    add_text_mc("QID1_2_TEXT", stats::setNames("2_TEXT", "x2_TEXT")),
    "QID1_x2_TEXT"
  )
  expect_identical(mc_recode_ids(c("1", "2_TEXT")), c("1", "2_TEXT"))

  empty_context <- list(
    response_column_qid = "QID1",
    render_facts = list(level = character())
  )
  expect_identical(suf_level_qid_macol(empty_context), "QID1")
  expect_identical(suf_level_qid_mavr(empty_context), "QID1")
  expect_identical(suf_choice_level_qid(empty_context), "QID1")

  level_context <- list(
    response_column_qid = "QID1",
    render_facts = list(
      item = c(row1 = "Row 1", row2 = "Row 2"),
      level = c(x1 = "1", x2_TEXT = "2_TEXT")
    )
  )
  expect_identical(suf_nmlabel_qid(level_context), c("QID1_x1", "QID1_x2_TEXT"))
  expect_length(suf_item_suf_level_qid(level_context), 4)
  expect_length(suf_level_suf_item_qid(level_context), 4)
  expect_identical(text(level_context), "QID1_TEXT")
  expect_identical(render_carried_forward_sbs_qids("QID1", NULL), "QID1")
  expect_identical(
    sbs_column_sub_selectors(c("TE", "MC")),
    c(NA_character_, NA_character_)
  )
  expect_identical(sbs_column_id(list(NA), 1), 1)
  expect_identical(sbs_column_choice_ids(character(), "MC", 2), 1:2)
})

test_that("Variable Dictionary assembly covers empty branches", {
  normalised_metadata <- structure(
    list(
      surveyID = "SV_EMPTY",
      survey_name = "Empty Survey",
      questions = list()
    ),
    class = c("qualtdict_normalised_metadata", "list")
  )

  empty_dict <- variable_dictionary_from_normalised_metadata(
    normalised_metadata,
    use_semantic_name = TRUE,
    block_pattern = NULL,
    block_sep = ".",
    semantic_name_preprocess = NULL
  )
  expect_named(
    empty_dict,
    c(
      "qid",
      "response_column_id",
      "row_source",
      "question_name",
      "semantic_name",
      "variable_name",
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
      "looping",
      "loop_option"
    )
  )
  expect_identical(attr(empty_dict, "surveyID", exact = TRUE), "SV_EMPTY")

  no_row_question <- list(qid = "QID1", question_name = "Q1")
  local_mocked_bindings(
    render_response_columns = function(qjson, response_column_qid) {
      tibble::tibble(
        response_column_id = character(),
        question = character(),
        item = character(),
        level = character(),
        label = character()
      )
    }
  )
  expect_null(variable_dictionary_question_row(no_row_question, "QID1"))

  normalised_metadata$questions <- list(QID1 = no_row_question)
  local_mocked_bindings(
    variable_dictionary_question_rows = function(question_meta) {
      list()
    }
  )
  expect_identical(
    nrow(variable_dictionary_from_normalised_metadata(
      normalised_metadata,
      use_semantic_name = FALSE,
      block_pattern = NULL,
      block_sep = ".",
      semantic_name_preprocess = NULL
    )),
    0L
  )

  rows <- tibble::tibble(
    qid = "QID1",
    response_column_id = "QID1",
    row_source = "question",
    question_name = "Q1",
    variable_name = "Q1",
    block = "Block",
    question = "Question",
    looping_question = NA_character_,
    item = NA_character_,
    level = "1",
    label = "Yes",
    type = "MC",
    selector = "SAVR",
    content_type = NA_character_,
    sub_selector = "TX",
    looping_option = NA_character_,
    looping = FALSE
  )
  cleaned <- clean_variable_dictionary_rows(rows, use_semantic_name = TRUE)
  expect_true("semantic_name" %in% names(cleaned))
})

test_that("normalisation and validation helpers cover fallback records", {
  expect_identical(
    default_question_block_metadata(),
    list(
      description = NA_character_,
      looping_prefix = character(),
      looping_qid = NA_character_,
      looping_static = NULL,
      looping_column_names = NULL
    )
  )
  expect_identical(
    question_fact_question_type(list()),
    list(type = NULL, selector = NULL, sub_selector = NULL)
  )
  expect_identical(
    validation_level_label_pairs(list()),
    tibble::tibble(pair = list(), qid = list())
  )
  findings <- inconsistent_response_column_names(tibble::tibble(
    response_column_id = c("QID1", "QID1"),
    variable_name = c("q1_a", "q1_b")
  ))
  expect_identical(
    findings$finding,
    c(
      "inconsistent_variable_name",
      "inconsistent_variable_name"
    )
  )
  expect_identical(
    semantic_block_components(
      tibble::tibble(block = c("Alpha Block", "Beta Block")),
      function(x) substr(x, 1, 1)
    ),
    c("A", "B")
  )
})

test_that("slowrake and retry cover deterministic fallback branches", {
  expect_true(is.na(slowrake_atomic(
    "123",
    stop_words = character(),
    all_words = "words",
    word_min_char = 3,
    stem = FALSE,
    stop_pos = NULL,
    word_token_annotator = NULL,
    pos_annotator = NULL
  )))
  expect_true(is.na(slowrake_atomic(
    "the and",
    stop_words = c("the", "and"),
    all_words = "the and",
    word_min_char = 3,
    stem = FALSE,
    stop_pos = NULL,
    word_token_annotator = NULL,
    pos_annotator = NULL
  )))

  capture.output(
    keywords <- slowrake(
      c("alpha beta", "beta gamma"),
      all_words = "alpha beta gamma",
      stop_pos = NULL,
      stem = FALSE,
      quiet = FALSE
    )
  )
  expect_s3_class(keywords, "rakelist")
  expect_s3_class(
    slowrake(
      "one two three",
      all_words = "one two three",
      stop_pos = "NN",
      stem = FALSE
    ),
    "rakelist"
  )

  attempts <- new.env(parent = emptyenv())
  attempts$count <- 0
  succeeds_second_time <- retry(function() {
    attempts$count <- attempts$count + 1
    if (attempts$count == 1) {
      stop("first failure")
    }
    "ok"
  })
  capture.output(
    messages <- capture.output(
      result <- succeeds_second_time(),
      type = "message"
    )
  )
  expect_identical(result, "ok")
  expect_match(paste(messages, collapse = "\n"), "first failure")

  always_fails <- retry(function() {
    stop("still failing")
  })
  capture.output(
    messages <- capture.output(
      expect_error(always_fails(), "still failing"),
      type = "message"
    )
  )
  expect_match(paste(messages, collapse = "\n"), "still failing")
})

test_that("survey_split_blocks requires Labelled Survey Data", {
  expect_error(
    survey_split_blocks("not data", dict = coverage_test_dict()),
    "Labelled Survey Data"
  )
})
