synthetic_sbs_carried_forward_raw_metadata <- function() {
  raw_metadata <- synthetic_sbs_multiple_answer_raw_metadata()
  raw_metadata$surveyID <- "SV_SBS_CARRIED_FORWARD"
  raw_metadata$metadata$metadata$name <- "SBS Carried Forward Survey"
  raw_metadata$metadata$questions$QID2$columns <- list()
  raw_metadata$metadata$questions$QID2$subQuestions <- list(
    x1 = list(description = "First row"),
    x2 = list(description = "Second row"),
    x3 = list(description = "Third row")
  )

  raw_metadata
}

synthetic_sbs_text_subquestion_raw_metadata <- function() {
  raw_metadata <- synthetic_sbs_multiple_answer_raw_metadata()
  raw_metadata$metadata$questions$QID2$columns <- list(
    `1` = list(
      questionType = list(
        selector = "TE",
        subSelector = "Medium"
      ),
      questionText = "Text column",
      choices = list(
        `1` = list(recode = "1", description = "Text")
      )
    ),
    `2` = list(
      questionType = list(
        selector = "Likert",
        subSelector = "SingleAnswer"
      ),
      questionText = "Single column A",
      choices = list(
        `1` = list(recode = "1", description = "Yes"),
        `2` = list(recode = "0", description = "No")
      )
    ),
    `3` = list(
      questionType = list(
        selector = "Likert",
        subSelector = "SingleAnswer"
      ),
      questionText = "Single column B",
      choices = list(
        `1` = list(recode = "1", description = "Yes"),
        `2` = list(recode = "0", description = "No")
      )
    )
  )
  raw_metadata$metadata$questions$QID2$subQuestions <- list(
    `2` = list(description = "Second row"),
    `4` = list(description = "Fourth row", textEntry = TRUE),
    `9` = list(description = "Ninth row")
  )

  raw_metadata
}
