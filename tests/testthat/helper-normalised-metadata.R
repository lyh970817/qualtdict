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
      block = list(
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
      question = list(
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
            Settings = list(ContentType = NULL)
          )
        )
      )
    )
  )
}

synthetic_sbs_multiple_answer_raw_metadata <- function() {
  new_raw_qualtrics_metadata(
    surveyID = "SV_SBS",
    metadata = list(
      metadata = list(name = "SBS Survey"),
      questions = list(
        QID2 = list(
          questionName = "Q2",
          questionType = list(
            type = "SBS",
            selector = "SBSMatrix",
            subSelector = NULL
          ),
          questionText = "Side by side",
          blocks = list(),
          columns = list(
            `1` = list(
              questionType = list(
                selector = "TE",
                subSelector = "Short"
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
              questionText = "Single column",
              choices = list(
                `1` = list(recode = "1", description = "Yes")
              )
            ),
            `3` = list(
              questionType = list(
                selector = "Likert",
                subSelector = "MultipleAnswer"
              ),
              questionText = "Multiple column",
              choices = list(
                `1` = list(recode = "1", description = "Checked A"),
                `2` = list(recode = "2", description = "Checked B")
              )
            )
          ),
          choices = list(),
          subQuestions = list(
            `2` = list(description = "Second row"),
            `4` = list(description = "Fourth row")
          )
        )
      )
    ),
    description = list(
      block = list(
        BL_1 = list(
          Description = "Main Block",
          BlockElements = list(
            list(QuestionID = "QID2")
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
        QID2 = list(
          Validation = list(
            Settings = list(ContentType = NULL)
          )
        )
      )
    )
  )
}

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
            Settings = list(ContentType = NULL)
          )
        )
      )
    )
  )
}

synthetic_numeric_looped_text_raw_metadata <- function() {
  raw_metadata <- synthetic_loop_and_merge_raw_metadata(
    "Why ${lm://Field/1}?"
  )

  raw_metadata$metadata$questions$QID1$choices <- stats::setNames(
    lapply(1:12, function(i) {
      list(recode = as.character(i), description = paste("Loop", i))
    }),
    as.character(1:12)
  )
  raw_metadata$description$block$BL_LOOP$Options$LoopingOptions$Static <-
    stats::setNames(
      lapply(1:12, function(i) list(`1` = "")),
      as.character(1:12)
    )

  raw_metadata$metadata$questions$QID2$questionName <- "Q3"
  names(raw_metadata$metadata$questions)[2] <- "QID3"
  raw_metadata$description$block$BL_LOOP$BlockElements <- list(
    list(QuestionID = "QID3")
  )
  raw_metadata$description$question$QID3 <-
    raw_metadata$description$question$QID2
  raw_metadata$description$question$QID2 <- NULL

  raw_metadata
}

synthetic_static_numeric_looped_text_raw_metadata <- function() {
  raw_metadata <- synthetic_loop_and_merge_raw_metadata(
    "Why ${lm://Field/1}?"
  )

  raw_metadata$metadata$questions$QID1$choices <- list(
    `1` = list(recode = "1", description = "Loop 1"),
    `2` = list(recode = "0", description = "Loop 0"),
    `3` = list(recode = "-88", description = "Loop -88")
  )
  raw_metadata$description$block$BL_LOOP$Options$LoopingOptions$Static <-
    stats::setNames(
      lapply(1:12, function(i) list(`1` = "")),
      as.character(1:12)
    )

  raw_metadata$metadata$questions$QID2$questionName <- "Q3"
  names(raw_metadata$metadata$questions)[2] <- "QID3"
  raw_metadata$description$block$BL_LOOP$BlockElements <- list(
    list(QuestionID = "QID3")
  )
  raw_metadata$description$question$QID3 <-
    raw_metadata$description$question$QID2
  raw_metadata$description$question$QID2 <- NULL

  raw_metadata
}

synthetic_matrix_source_looped_text_raw_metadata <- function() {
  new_raw_qualtrics_metadata(
    surveyID = "SV_MATRIX_LOOP",
    metadata = list(
      metadata = list(name = "Matrix Loop Survey"),
      questions = list(
        QID1 = list(
          questionName = "Q1",
          questionType = list(
            type = "Matrix",
            selector = "Likert",
            subSelector = "SingleAnswer"
          ),
          questionText = "Family conditions",
          blocks = list(),
          columns = list(),
          choices = list(
            `1` = list(recode = "1", description = "Yes"),
            `2` = list(recode = "0", description = "No")
          ),
          subQuestions = list(
            `1` = list(recode = "1", choiceText = "Condition 1"),
            `2` = list(recode = "2", choiceText = "Condition 2"),
            `3` = list(recode = "3", choiceText = "Condition 3")
          )
        ),
        QID2 = list(
          questionName = "Q2",
          questionType = list(
            type = "TE",
            selector = "SL",
            subSelector = NULL
          ),
          questionText = "How many relatives had ${lm://Field/1}?",
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
                `1` = list(`1` = ""),
                `2` = list(`1` = ""),
                `3` = list(`1` = "")
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

synthetic_looped_mc_text_raw_metadata <- function() {
  raw_metadata <- synthetic_loop_and_merge_raw_metadata(
    "Explain your ${lm://Field/1} answer"
  )

  raw_metadata$metadata$questions$QID2 <- list(
    questionName = "Q2",
    questionType = list(
      type = "MC",
      selector = "SAVR",
      subSelector = "TX"
    ),
    questionText = "Explain your ${lm://Field/1} answer",
    blocks = list(),
    columns = list(),
    choices = list(
      `1` = list(recode = "1", description = "Selected"),
      `2` = list(
        recode = "2",
        description = "Other",
        textEntry = TRUE
      )
    ),
    subQuestions = list()
  )

  raw_metadata
}
