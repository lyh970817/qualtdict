synthetic_mc_text_raw_metadata <- function() {
  new_raw_qualtrics_metadata(
    surveyID = "SV_SYNTHETIC",
    metadata = list(
      metadata = list(name = "Synthetic Survey"),
      questions = list(
        QID1 = list(
          questionName = "Q1",
          questionType = list(
            type = "MC",
            selector = "SAVR",
            subSelector = "TX"
          ),
          questionText = "Choose one",
          blocks = list(),
          columns = list(),
          choices = list(
            `1` = list(recode = "1", description = "Yes"),
            `2` = list(recode = "2", description = "No"),
            `3` = list(
              recode = "3",
              description = "Other",
              textEntry = TRUE
            )
          ),
          subQuestions = list()
        )
      )
    ),
    description = list(
      block = list(
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
      question = list(
        QID1 = list(
          Validation = list(
            Settings = list(ContentType = "ValidNumber")
          )
        )
      )
    )
  )
}

synthetic_loop_and_merge_raw_metadata <- function(
    question_text = "Why did you choose ${lm://Field/1}?") {
  new_raw_qualtrics_metadata(
    surveyID = "SV_LOOP",
    metadata = list(
      metadata = list(name = "Loop Survey"),
      questions = list(
        QID1 = list(
          questionName = "Q1",
          questionType = list(
            type = "MC",
            selector = "SAVR",
            subSelector = "TX"
          ),
          questionText = "Which options did you consider?",
          blocks = list(),
          columns = list(),
          choices = list(
            x1 = list(recode = "1", description = "Apples"),
            x2 = list(recode = "2", description = "Bananas")
          ),
          subQuestions = list()
        ),
        QID2 = list(
          questionName = "Q2",
          questionType = list(
            type = "TE",
            selector = "SL",
            subSelector = NULL
          ),
          questionText = question_text,
          blocks = list(),
          columns = list(),
          choices = list(),
          subQuestions = list()
        )
      )
    ),
    description = list(
      block = list(
        BL_SOURCE = list(
          Description = "Source Block",
          BlockElements = list(
            list(QuestionID = "QID1")
          ),
          Options = list(
            LoopingOptions = list(
              Static = NULL,
              QID = NULL
            )
          )
        ),
        BL_LOOP = list(
          Description = "Loop Block",
          BlockElements = list(
            list(QuestionID = "QID2")
          ),
          Options = list(
            LoopingOptions = list(
              Static = list(
                x1 = list(`1` = ""),
                x2 = list(`1` = "")
              ),
              QID = "QID1"
            )
          )
        )
      ),
      question = list(
        QID1 = list(
          Validation = list(
            Settings = list(ContentType = "ValidNumber")
          )
        ),
        QID2 = list(
          Validation = list(
            Settings = list(ContentType = NULL)
          )
        )
      )
    )
  )
}
