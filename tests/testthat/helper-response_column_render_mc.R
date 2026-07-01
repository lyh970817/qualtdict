synthetic_mc_x_choice_raw_metadata <- function() {
  new_raw_qualtrics_metadata(
    surveyID = "SV_MC_X",
    metadata = list(
      metadata = list(name = "MC X Choice Survey"),
      questions = list(
        QID126879611 = list(
          questionName = "Q1",
          questionType = list(
            type = "MC",
            selector = "MAVR",
            subSelector = "TX"
          ),
          questionText = "Select all that apply",
          blocks = list(),
          columns = list(),
          choices = list(
            x1 = list(recode = "1", description = "Choice 1"),
            x2 = list(recode = "2", description = "Choice 2"),
            x3 = list(recode = "3", description = "Choice 3"),
            x4 = list(recode = "4", description = "Choice 4"),
            x6 = list(recode = "6", description = "Choice 6")
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
            list(QuestionID = "QID126879611")
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
        QID126879611 = list(
          Validation = list(
            Settings = list(ContentType = NULL)
          )
        )
      )
    )
  )
}

synthetic_mc_recode_raw_metadata <- function(selector = "MACOL") {
  new_raw_qualtrics_metadata(
    surveyID = "SV_MC_RECODE",
    metadata = list(
      metadata = list(name = "MC Recode Survey"),
      questions = list(
        QID1 = list(
          questionName = "Q1",
          questionType = list(
            type = "MC",
            selector = selector,
            subSelector = "TX"
          ),
          questionText = "Select all that apply",
          blocks = list(),
          columns = list(),
          choices = list(
            `10` = list(recode = "1", description = "One"),
            `2` = list(recode = "2", description = "Two"),
            `7` = list(recode = "-88", description = "Prefer not"),
            `8` = list(recode = "-99", description = "Unknown"),
            `9` = list(
              recode = "0",
              description = "Other",
              textEntry = TRUE
            )
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
