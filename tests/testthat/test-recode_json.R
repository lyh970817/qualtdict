test_that("rep_loop preserves matrix loop positions without level duplication", {
  matrix_loop <- list(
    type = "Matrix",
    qid = c(
      rep("QID_LOOP_1", 4),
      rep("QID_LOOP_2", 4),
      rep("QID_LOOP_17", 4)
    ),
    item = c(
      rep("Loop option A", 4),
      rep("Loop option B", 4),
      rep("Other loop option", 4)
    ),
    looping = FALSE
  )
  looped_question <- list(
    qid = "QID_TARGET",
    name = "MED.DOI",
    question = "After taking ${lm://Field/1}",
    looping_question = NA_character_,
    looping_option = NA_character_,
    looping = TRUE
  )
  question_meta <- list(
    QID_LOOP = list(
      looping_qid = NULL,
      choices = list(
        `1` = list(description = "Not at all"),
        `2` = list(description = "A little"),
        `17` = list(description = "A lot")
      )
    ),
    QID_TARGET = list(
      looping_qid = "QID_LOOP",
      looping_prefix = c("1", "2", "17")
    )
  )

  result <- rep_loop(
    list(QID_LOOP = matrix_loop, QID_TARGET = looped_question),
    question_meta
  )
  looped <- result[vapply(result, function(x) x$looping, logical(1))]
  qids <- unname(vapply(looped, function(x) x$qid, character(1)))
  names <- unname(vapply(looped, function(x) x$name, character(1)))

  expect_length(looped, 3)
  expect_equal(
    qids,
    c("1_QID_TARGET", "2_QID_TARGET", "17_QID_TARGET")
  )
  expect_equal(
    unname(vapply(looped, function(x) x$looping_option, character(1))),
    c("Loop option A", "Loop option B", "Other loop option")
  )
  expect_equal(
    names,
    c("1.MED.DOI", "2.MED.DOI", "17.MED.DOI")
  )
  expect_true(all(vapply(
    looped,
    function(x) is.null(names(x$qid)) && is.null(names(x$name)),
    logical(1)
  )))
})

test_that("rep_loop uses static prefixes for non-matrix loop sources", {
  loop_source <- list(
    type = "MC",
    label = list(c(
      `1` = "Loop option A",
      `6` = "Loop option B",
      `0` = "None"
    )),
    looping = FALSE
  )
  looped_question <- list(
    qid = "QID_TARGET",
    name = "MED.WHY",
    question = "Why were you prescribed ${lm://Field/1}?",
    looping_question = NA_character_,
    looping_option = NA_character_,
    looping = TRUE
  )
  question_meta <- list(
    QID_LOOP = list(
      looping_qid = NULL,
      looping_prefix = NULL,
      choices = list(
        `7` = list(description = "Loop option A"),
        `4` = list(description = "Loop option B"),
        `26` = list(description = "None")
      )
    ),
    QID_TARGET = list(
      looping_qid = "QID_LOOP",
      looping_prefix = c("7", "4", "26")
    )
  )

  result <- rep_loop(
    list(QID_LOOP = loop_source, QID_TARGET = looped_question),
    question_meta
  )
  looped <- result[vapply(result, function(x) x$looping, logical(1))]

  expect_equal(
    unname(vapply(looped, function(x) x$qid, character(1))),
    c("7_QID_TARGET", "4_QID_TARGET", "26_QID_TARGET")
  )
  expect_equal(
    unname(vapply(looped, function(x) x$name, character(1))),
    c("7.MED.WHY", "4.MED.WHY", "26.MED.WHY")
  )
  expect_equal(
    unname(vapply(looped, function(x) x$looping_option, character(1))),
    c("Loop option A", "Loop option B", "None")
  )
})
