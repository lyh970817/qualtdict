test_that("resolve_exported_response_column_id prefers the declared name", {
  expect_identical(
    resolve_exported_response_column_id(
      "ResponseId",
      c("ResponseId", "Q1")
    ),
    "ResponseId"
  )
})

test_that("resolve_exported_response_column_id falls back to the QSED prefix", {
  expect_identical(
    resolve_exported_response_column_id(
      "ResponseId",
      c("QSEDResponseId", "Q1")
    ),
    "QSEDResponseId"
  )
})

test_that("resolve_exported_response_column_id returns the input unchanged", {
  expect_identical(
    resolve_exported_response_column_id("ResponseId", "Q1"),
    "ResponseId"
  )
})

test_that("dynamic choice recode override uses RecodeValues verbatim", {
  question <- list(choices = list(x1 = list(), x2 = list()))
  description <- list(
    DynamicChoices = list(Locator = "q://QID1/ChoiceGroup/SelectedChoices"),
    RecodeValues = list(`1` = "10", `2` = "20")
  )

  expect_identical(
    resolve_dynamic_choice_recode_override(question, description),
    stats::setNames(c("10", "20"), c("1", "2"))
  )
})

test_that("dynamic choice recode override derives IDs by static-key offset", {
  question <- list(
    choices = list(x1 = list(), x2 = list(), x2_TEXT = list())
  )
  description <- list(
    DynamicChoices = list(Locator = "q://QID1/ChoiceGroup/SelectedChoices"),
    Choices = list(`1` = list(), `2` = list())
  )

  # own static keys max to 2, so carried x1/x2 export as 3/4 and the
  # text-entry choice ID is left untouched.
  expect_identical(
    resolve_dynamic_choice_recode_override(question, description),
    stats::setNames(c("3", "4", "x2_TEXT"), c("x1", "x2", "x2_TEXT"))
  )
})

test_that("dynamic choice recode override offsets from zero without own keys", {
  question <- list(
    choices = list(x1 = list(), x2 = list(), x2_TEXT = list())
  )
  description <- list(
    DynamicChoices = list(Locator = "q://QID1/ChoiceGroup/SelectedChoices")
  )

  expect_identical(
    resolve_dynamic_choice_recode_override(question, description),
    stats::setNames(c("1", "2", "x2_TEXT"), c("x1", "x2", "x2_TEXT"))
  )
})

test_that("dynamic choice recode override is a no-op without carried choices", {
  question <- list(choices = list(x1 = list(), x2 = list()))

  # No description at all, or no DynamicChoices, leaves the question untouched.
  expect_null(resolve_dynamic_choice_recode_override(question, NULL))
  expect_null(resolve_dynamic_choice_recode_override(question, list()))

  # DynamicChoices present but the question carries no choices.
  dynamic <- list(
    DynamicChoices = list(Locator = "q://QID1/ChoiceGroup/SelectedChoices")
  )
  expect_null(
    resolve_dynamic_choice_recode_override(list(choices = list()), dynamic)
  )
})

test_that("normalise_response_choices applies a recode override", {
  choices <- normalise_response_choices(
    list(x1 = list(description = "A"), x2 = list(description = "B")),
    stats::setNames(c("3", "4"), c("x1", "x2"))
  )

  expect_identical(choices$x1$level, "3")
  expect_identical(choices$x2$level, "4")
})

test_that("FORM questions with all choices suppressed export no columns", {
  question <- normalise_question_fact(
    qid = "QID1",
    question = list(
      questionName = "form_q",
      questionType = list(type = "TE", selector = "FORM", subSelector = NULL),
      questionText = "Form",
      choices = list(
        `1` = list(description = "A", analyze = FALSE),
        `2` = list(description = "B", analyze = FALSE)
      ),
      subQuestions = list(),
      columns = list()
    ),
    block = list(description = "Main Block"),
    content_type = NULL
  )

  rendered <- render_response_columns(question, "QID1")

  expect_identical(nrow(rendered), 0L)
})

test_that("FORM questions still export analysed choice columns", {
  question <- normalise_question_fact(
    qid = "QID1",
    question = list(
      questionName = "form_q",
      questionType = list(type = "TE", selector = "FORM", subSelector = NULL),
      questionText = "Form",
      choices = list(
        `1` = list(description = "A", analyze = FALSE),
        `2` = list(description = "B", analyze = TRUE)
      ),
      subQuestions = list(),
      columns = list()
    ),
    block = list(description = "Main Block"),
    content_type = NULL
  )

  rendered <- render_response_columns(question, "QID1")

  expect_identical(rendered$response_column_id, "QID1_2")
})
