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

synthetic_multi_statement_slider_raw_metadata <- function() {
  new_raw_qualtrics_metadata(
    surveyID = "SV_SLIDER_MS",
    metadata = list(
      metadata = list(name = "Multi-statement Slider Survey"),
      questions = list(
        QID1 = list(
          questionName = "Q1",
          questionType = list(
            type = "Slider",
            selector = "HSLIDER",
            subSelector = NULL
          ),
          questionText = "Rate each statement",
          blocks = list(),
          columns = list(),
          # Slider scale points as choices (recodes 1..7).
          choices = list(
            `1` = list(recode = "1", description = "1"),
            `2` = list(recode = "2", description = "2"),
            `3` = list(recode = "3", description = "3"),
            `4` = list(recode = "4", description = "4"),
            `5` = list(recode = "5", description = "5"),
            `6` = list(recode = "6", description = "6"),
            `7` = list(recode = "7", description = "7")
          ),
          # Two statements with non-contiguous Qualtrics IDs (1 and 4).
          subQuestions = list(
            `1` = list(recode = "1", choiceText = "Statement A"),
            `4` = list(recode = "4", choiceText = "Statement B")
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
            Settings = list(ContentType = NULL)
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

synthetic_matrix_multiple_answer_raw_metadata <- function() {
  new_raw_qualtrics_metadata(
    surveyID = "SV_MATRIX_MA",
    metadata = list(
      metadata = list(name = "Matrix Multiple Answer Survey"),
      questions = list(
        QID1 = list(
          questionName = "Q1",
          questionType = list(
            type = "Matrix",
            selector = "Likert",
            subSelector = "MultipleAnswer"
          ),
          questionText = "Tick every option that applies",
          blocks = list(),
          columns = list(),
          choices = list(
            `1` = list(recode = "1", description = "Morning"),
            `2` = list(recode = "2", description = "Evening"),
            `3` = list(recode = "-99", description = "Prefer not")
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
            Settings = list(ContentType = NULL)
          )
        )
      )
    )
  )
}
