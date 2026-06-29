test_that("checkarg_isboolean accepts only scalar booleans", {
  expect_null(checkarg_isboolean(TRUE))
  expect_null(checkarg_isboolean(FALSE))

  expect_error(checkarg_isboolean(NA), "single `TRUE` or `FALSE`")
  expect_error(checkarg_isboolean(c(TRUE, FALSE)), "single `TRUE` or `FALSE`")
  expect_error(checkarg_isboolean("TRUE"), "single `TRUE` or `FALSE`")
})

test_that("checkarg_isstring handles nullable scalar strings", {
  expect_null(checkarg_isstring("value"))
  expect_null(checkarg_isstring(NULL))

  expect_error(checkarg_isstring(NULL, null_okay = FALSE), "single string")
  expect_error(checkarg_isstring(c("a", "b")), "single string")
  expect_error(checkarg_isstring(1), "single string")
})

test_that("checkarg_ischaracter rejects non-character and missing values", {
  expect_null(checkarg_ischaracter(c("a", "b")))
  expect_null(checkarg_ischaracter(NULL))

  expect_error(
    checkarg_ischaracter(NULL, null_okay = FALSE),
    "character vector"
  )
  expect_error(checkarg_ischaracter(1:2), "character vector")
  expect_error(checkarg_ischaracter(c("a", NA)), "must not have missing")
})

test_that("checkarg_isname accepts legacy naming options", {
  expect_null(checkarg_isname("question_name"))
  expect_null(checkarg_isname("semantic_name"))
  expect_null(checkarg_isname("easy_name"))

  expect_error(checkarg_isname("bad_name"), "question_name")
  expect_error(checkarg_isname(NULL), "question_name")
})

test_that("checkarg_isvariable_name accepts current naming options", {
  expect_null(checkarg_isvariable_name("question_name"))
  expect_null(checkarg_isvariable_name("semantic_name"))

  expect_error(checkarg_isvariable_name("easy_name"), "question_name")
  expect_error(checkarg_isvariable_name(NULL), "question_name")
})

test_that("checkarg_isfunction handles nullable function arguments", {
  expect_null(checkarg_isfunction(identity))
  expect_null(checkarg_isfunction(NULL))

  expect_error(checkarg_isfunction(NULL, null_okay = FALSE), "function")
  expect_error(checkarg_isfunction("identity"), "function")
})

test_that("checkarg_isqualtdict requires qualtdict objects", {
  dict <- tibble::tibble(response_column_id = "QID1")
  class(dict) <- c("qualtdict", class(dict))

  expect_null(checkarg_isqualtdict(dict))
  expect_error(checkarg_isqualtdict(tibble::tibble()), "`qualtdict`")
})
