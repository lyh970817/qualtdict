synthetic_matrix_raw_metadata <- function() {
  new_raw_qualtrics_metadata(
    surveyID = "SV_MATRIX",
    metadata = list(
      metadata = list(name = "Matrix Survey"),
      questions = list(
        QID1 = list(
          questionName = "Q1",
          questionType = list(
            type = "Matrix",
            selector = "Likert",
            subSelector = "SingleAnswer"
          ),
          questionText = "Rate each item",
          blocks = list(),
          columns = list(),
          choices = list(
            `1` = list(recode = "1", description = "Low"),
            `2` = list(recode = "2", description = "High")
          ),
          subQuestions = list(
            x1 = list(recode = "1", choiceText = "Apples"),
            x2 = list(recode = "2", choiceText = "Bananas")
          )
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
            Settings = list(ContentType = "ValidNumber")
          )
        )
      )
    )
  )
}

synthetic_slider_raw_metadata <- function() {
  new_raw_qualtrics_metadata(
    surveyID = "SV_SLIDER",
    metadata = list(
      metadata = list(name = "Slider Survey"),
      questions = list(
        QID1 = list(
          questionName = "Q1",
          questionType = list(
            type = "Slider",
            selector = "HSLIDER",
            subSelector = NULL
          ),
          questionText = "Rate each item",
          blocks = list(),
          columns = list(),
          choices = list(
            `1` = list(recode = "1", description = "One"),
            `2` = list(recode = "2", description = "Two"),
            `3` = list(recode = "3", description = "Three")
          ),
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
