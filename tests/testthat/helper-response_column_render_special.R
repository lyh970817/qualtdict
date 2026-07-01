synthetic_timing_raw_metadata <- function() {
  new_raw_qualtrics_metadata(
    surveyID = "SV_TIMING",
    metadata = list(
      metadata = list(name = "Timing Survey"),
      questions = list(
        QID1 = list(
          questionName = "Q1",
          questionType = list(
            type = "Timing",
            selector = "PageTimer",
            subSelector = NULL
          ),
          questionText = "Timing",
          blocks = list(),
          columns = list(),
          choices = list(),
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

synthetic_file_upload_raw_metadata <- function() {
  new_raw_qualtrics_metadata(
    surveyID = "SV_FILE_UPLOAD",
    metadata = list(
      metadata = list(name = "File Upload Survey"),
      questions = list(
        QID1 = list(
          questionName = "Q1",
          questionType = list(
            type = "FileUpload",
            selector = "FileUpload",
            subSelector = NULL
          ),
          questionText = "Upload a file",
          blocks = list(),
          columns = list(),
          choices = list(),
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
