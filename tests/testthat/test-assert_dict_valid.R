gate_dict <- function(
  response_column_id,
  label,
  level,
  variable_name = "q1",
  row_source = "question",
  type = "MC",
  selector = "SAVR",
  sub_selector = "TX"
) {
  dict <- tibble::tibble(
    response_column_id = response_column_id,
    row_source = row_source,
    qid = sub("^[0-9]+_", "", sub("_[^_]*$", "", response_column_id)),
    question_name = variable_name,
    variable_name = variable_name,
    block = "Main Block",
    question = "Question text",
    item = NA_character_,
    level = level,
    label = label,
    type = type,
    selector = selector,
    sub_selector = sub_selector,
    content_type = NA_character_
  )
  attr(dict, "class") <- c("qualtdict", class(dict))
  attr(dict, "surveyID") <- "SV_TEST"
  dict
}

# Two choices of one multiple-answer question share a Qualtrics RecodeValue,
# so Qualtrics exports ONE column carrying both meanings. This is the shape
# that aborts Labelled Export.
tick_column_collision_dict <- function() {
  gate_dict(
    response_column_id = c("1_QID11_7", "1_QID11_7"),
    variable_name = "ptsd.sertraline",
    label = c(
      "Posttraumatic stress disorder (PTSD)",
      "Body dysmorphic disorder"
    ),
    level = c("1", "1"),
    selector = "MACOL"
  )
}

# A dropdown whose recodes repeat across many choices. It labels without
# aborting, so every response silently takes the first colliding label.
dropdown_collision_dict <- function() {
  gate_dict(
    response_column_id = rep("QID20", 5),
    variable_name = "gp.practice",
    label = c(
      "Aberdeen Medical Practice",
      "Bath Health Centre",
      "Cardiff Surgery",
      "Dundee Practice",
      "Exeter Surgery"
    ),
    level = c("101", "102", "102", "102", "103"),
    selector = "DL"
  )
}

# Levels 1, 8, 9, 10 are a legitimate gapped Qualtrics recode: the level-label
# mapping is one-to-one and every response labels correctly.
gapped_level_dict <- function() {
  gate_dict(
    response_column_id = rep("QID30", 4),
    variable_name = "agreement",
    label = c(
      "Definitely agree",
      "Slightly agree",
      "Slightly disagree",
      "Definitely disagree"
    ),
    level = c("1", "8", "9", "10")
  )
}

clean_dict <- function() {
  gate_dict(
    response_column_id = c("QID1", "QID1"),
    variable_name = "q1",
    label = c("Yes", "No"),
    level = c("1", "2")
  )
}

test_that(
  paste(
    "assert_dict_valid fires on a tick column",
    "with two labels on one level"
  ),
  {
    condition <- expect_error(
      assert_dict_valid(tick_column_collision_dict()),
      class = "qualtdict_export_blocking_findings"
    )

    expect_match(
      conditionMessage(condition),
      paste0(
        "1_QID11_7 \\(ptsd.sertraline\\): level 1 carries 2 labels ",
        "\u2014 \"Posttraumatic stress disorder \\(PTSD\\)\", ",
        "\"Body dysmorphic disorder\""
      )
    )
    expect_identical(
      unique(condition$findings$response_column_id),
      "1_QID11_7"
    )
    expect_identical(unique(condition$findings$mistake), "124")
  }
)

test_that("assert_dict_valid fires on a dropdown with duplicated levels", {
  condition <- expect_error(
    assert_dict_valid(dropdown_collision_dict()),
    class = "qualtdict_export_blocking_findings"
  )

  expect_match(
    conditionMessage(condition),
    "QID20 \\(gp.practice\\): level 102 carries 3 labels"
  )
  expect_identical(unique(condition$findings$response_column_id), "QID20")
})

test_that("assert_dict_valid does not fire on a gapped level sequence", {
  expect_no_error(assert_dict_valid(gapped_level_dict()))
})

test_that("assert_dict_valid does not fire on a clean Variable Dictionary", {
  dict <- clean_dict()

  expect_no_error(assert_dict_valid(dict))
  expect_identical(assert_dict_valid(dict), dict)
})

test_that("assert_dict_valid fires on one label carried by two levels", {
  dict <- gate_dict(
    response_column_id = c("QID40", "QID40"),
    variable_name = "duplicated.label",
    label = c("Prefer not to answer", "Prefer not to answer"),
    level = c("1", "2")
  )

  condition <- expect_error(
    assert_dict_valid(dict),
    class = "qualtdict_export_blocking_findings"
  )
  expect_match(
    conditionMessage(condition),
    paste0(
      "QID40 \\(duplicated.label\\): label \"Prefer not to answer\" ",
      "is carried by 2 levels \u2014 \"1\", \"2\""
    )
  )
})

test_that("assert_dict_valid truncates many collisions in one column", {
  levels <- c(rep("102", 7), as.character(seq(200, 229)))
  dict <- gate_dict(
    response_column_id = rep("QID50", length(levels)),
    variable_name = "nhs.trust",
    label = paste("Practice", seq_along(levels)),
    level = levels,
    selector = "DL"
  )

  condition <- expect_error(
    assert_dict_valid(dict),
    class = "qualtdict_export_blocking_findings"
  )

  message <- conditionMessage(condition)
  expect_match(message, "level 102 carries 7 labels")
  expect_match(
    message,
    "\"Practice 1\", \"Practice 2\", \"Practice 3\" and 4 more"
  )
  expect_false(grepl("Practice 4\"", message, fixed = TRUE))
})

test_that("assert_dict_valid truncates the reported Response Column IDs", {
  dict <- gate_dict(
    response_column_id = rep(paste0("QID", seq_len(12)), each = 2),
    variable_name = rep(paste0("q", seq_len(12)), each = 2),
    label = paste("Label", seq_len(24)),
    level = rep("1", 24)
  )

  condition <- expect_error(
    assert_dict_valid(dict),
    class = "qualtdict_export_blocking_findings"
  )

  expect_match(
    conditionMessage(condition),
    "... and 2 more Response Column IDs.",
    fixed = TRUE
  )
  expect_length(unique(condition$findings$response_column_id), 12L)
})

test_that("assert_dict_valid skips Metadata-defined Export Variable rows", {
  dict <- gate_dict(
    response_column_id = c("ED1", "ED1"),
    row_source = "embedded_data",
    variable_name = "embedded_field",
    label = c("Wave one", "Wave two"),
    level = c("1", "1")
  )

  expect_no_error(assert_dict_valid(dict))
})

test_that("assert_dict_valid rejects what is not a Variable Dictionary", {
  expect_error(
    assert_dict_valid(data.frame(a = 1)),
    "must be a `qualtdict`"
  )
})

test_that("dict_validate stays total for Export-blocking dictionaries", {
  dicts <- list(
    tick_column = tick_column_collision_dict(),
    dropdown = dropdown_collision_dict(),
    gapped = gapped_level_dict(),
    clean = clean_dict()
  )
  mistakes <- lapply(dicts, function(dict) {
    findings <- dict_validate(dict)$validation_findings
    unique(findings$mistake[findings$finding == "level_label_mistake"])
  })

  # Every dictionary validates without erroring, and the `mistake` codes keep
  # their existing encoding: only 1, 3 and 4 are Export-blocking.
  expect_identical(mistakes$tick_column, "124")
  expect_identical(mistakes$dropdown, "124")
  expect_identical(mistakes$gapped, "2")
  expect_identical(mistakes$clean, character())
})

test_that("dict_validate and assert_dict_valid share one predicate", {
  dict <- dropdown_collision_dict()

  findings <- dict_validate(dict)$validation_findings
  level_label_findings <- findings[findings$finding == "level_label_mistake", ]
  blocking_findings <- export_blocking_validation_findings(dict)

  expect_identical(blocking_findings, level_label_findings)
})

# A Loop and Merge text-entry column whose Variable Dictionary carries two
# identical rows, both level and label `NA`. `anyDuplicated()` counts a
# repeated `NA` as a repeat, so it is reported like any other repeated level.
# Seen in `edgi_opt` as `6_QID232_TEXT`.
test_that("assert_dict_valid reports a repeated NA level as <NA>", {
  dict <- gate_dict(
    response_column_id = c("6_QID232_TEXT", "6_QID232_TEXT"),
    variable_name = "family_diagnosed.txt.pain",
    label = c(NA_character_, NA_character_),
    level = c(NA_character_, NA_character_),
    type = "TE"
  )

  condition <- expect_error(
    assert_dict_valid(dict),
    class = "qualtdict_export_blocking_findings"
  )

  expect_match(
    conditionMessage(condition),
    "level <NA> carries 2 labels — <NA>, <NA>",
    fixed = TRUE
  )
})

test_that("assert_dict_valid counts one further repeated level", {
  dict <- gate_dict(
    response_column_id = rep("QID30", 4),
    variable_name = "gp.practice",
    label = c("Alpha", "Beta", "Gamma", "Delta"),
    level = c("1", "1", "2", "2")
  )

  condition <- expect_error(
    assert_dict_valid(dict),
    class = "qualtdict_export_blocking_findings"
  )

  expect_match(
    conditionMessage(condition),
    "(and 1 more repeated level in this column)",
    fixed = TRUE
  )
})

test_that("assert_dict_valid counts several further repeated levels", {
  dict <- gate_dict(
    response_column_id = rep("QID31", 6),
    variable_name = "gp.practice",
    label = c("Alpha", "Beta", "Gamma", "Delta", "Epsilon", "Zeta"),
    level = c("1", "1", "2", "2", "3", "3")
  )

  condition <- expect_error(
    assert_dict_valid(dict),
    class = "qualtdict_export_blocking_findings"
  )

  expect_match(
    conditionMessage(condition),
    "(and 2 more repeated levels in this column)",
    fixed = TRUE
  )
})

test_that("export_blocking_reason falls back when nothing is repeated", {
  # Defensive only: `check_item_export_blocking()` cannot reach this, because
  # a label-level mapping can fail to be one-to-one only when some label or
  # some level is repeated, and both of those return earlier.
  expect_identical(
    export_blocking_reason(
      label = c("Yes", "No"),
      level = c("1", "2"),
      max_examples = 3
    ),
    "label and level are not a one-to-one mapping"
  )
})
