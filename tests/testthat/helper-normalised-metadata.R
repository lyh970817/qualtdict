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

synthetic_flat_embedded_data_raw_metadata <- function() {
  raw_metadata <- synthetic_mc_text_raw_metadata()
  raw_metadata$metadata$embedded_data <- list(
    list(name = "Source Channel"),
    list(name = "Q1")
  )
  raw_metadata
}

synthetic_scoring_raw_metadata <- function() {
  raw_metadata <- synthetic_mc_text_raw_metadata()
  raw_metadata$description$scoring <- list(
    list(name = "Total Score"),
    list(name = "Q1")
  )
  raw_metadata
}

synthetic_nested_scoring_raw_metadata <- function() {
  raw_metadata <- synthetic_mc_text_raw_metadata()
  raw_metadata$description$scoring <- list(
    ScoringCategories = list(
      list(ID = "SC_TOTAL", Name = "Total Score"),
      list(ID = "SC_SCREEN")
    )
  )
  raw_metadata
}

synthetic_text_analysis_raw_metadata <- function() {
  raw_metadata <- synthetic_mc_text_raw_metadata()
  raw_metadata$metadata$comments <- list(
    list(
      outputName = "Q1 Sentiment",
      responseColumnId = "QID1_TEXT_SENTIMENT",
      questionId = "QID1"
    ),
    `Q1 Topic` = list(
      responseColumnId = "QID1_TEXT_TOPIC",
      questionId = "QID1"
    ),
    list(
      outputName = "Q1",
      responseColumnId = "Unmatched Topic",
      questionId = "QID99"
    )
  )
  raw_metadata
}

glad_sa6_text_analysis_sidecar_ids <- function() {
  c(
    "QID694_TEXT_9079b4e757e24533be35ff4cTopics",
    "QID694_TEXT_9079b4e7_gaygfh795aeaActionability",
    "QID694_TEXT_9079b4e7_gaygfh795aeaEffort",
    "QID694_TEXT_9079b4e7_gaygfh795aeaEffortNumeric",
    "QID694_TEXT_9079b4e7_gaygfh795aeaEmotIntensity",
    "QID694_TEXT_9079b4e7_gaygfh795aeaEmotion",
    "QID694_TEXT_9079b4e7_gaygfh795aeaParTopics",
    "QID694_TEXT_9079b4e7_gaygfh795aeaSenPol",
    "QID694_TEXT_9079b4e7_gaygfh795aeaSenScore",
    "QID694_TEXT_9079b4e7_gaygfh795aeaSentiment",
    "QID694_TEXT_9079b4e7_gaygfh795aeaTopicSenLabel",
    "QID694_TEXT_9079b4e7_gaygfh795aeaTopicSenScore",
    "QID694_TEXT_9079b4e7_gaygfh795aeaTopicHierarchy1"
  )
}

edgi_signup_text_analysis_sidecar_ids <- function() {
  c(
    "QID121_1_e476cefa310845248231594eParTopics",
    "QID121_1_e476cefa310845248231594eTopics"
  )
}

synthetic_column_map_sidecar_raw_metadata <- function() {
  raw_metadata <- synthetic_mc_text_raw_metadata()
  raw_metadata$metadata$questions$QID508 <- list(
    questionName = "DEM.AGE.1.0",
    questionType = list(type = "TE", selector = "SL", subSelector = NULL),
    questionText = "CurrentAge",
    blocks = list(),
    columns = list(),
    choices = list(),
    subQuestions = list()
  )
  raw_metadata$metadata$questions$QID626 <- list(
    questionName = "DEM.DIS.2txt.0",
    questionType = list(type = "TE", selector = "ML", subSelector = NULL),
    questionText = "DisabilityAdditionalInfo",
    blocks = list(),
    columns = list(),
    choices = list(),
    subQuestions = list()
  )
  raw_metadata$metadata$questions$QID429 <- list(
    questionName = "UXP.INF.1txt.0",
    questionType = list(type = "TE", selector = "ESTB", subSelector = NULL),
    questionText = paste(
      "Is there any other information you would like to share",
      "that relates to this study?"
    ),
    blocks = list(),
    columns = list(),
    choices = list(),
    subQuestions = list()
  )
  raw_metadata$metadata$questions$QID694 <- list(
    questionName = "CAM.TV.1txt.0",
    questionType = list(type = "TE", selector = "SL", subSelector = NULL),
    questionText = "CAM.TV.1txt.0",
    blocks = list(),
    columns = list(),
    choices = list(),
    subQuestions = list()
  )
  raw_metadata$metadata$questions$QID121 <- list(
    questionName = "ED.ANT.6.0",
    questionType = list(type = "TE", selector = "FORM", subSelector = NULL),
    questionText = "ED.ANT.6.0",
    blocks = list(),
    columns = list(),
    choices = list(
      `1` = list(recode = "1", description = "First text field"),
      `2` = list(recode = "2", description = "Second text field")
    ),
    subQuestions = list()
  )
  raw_metadata$metadata$questions$QID700 <- list(
    questionName = "CHECK.ALL",
    questionType = list(type = "MC", selector = "MAVR", subSelector = "TX"),
    questionText = "Select all that apply",
    blocks = list(),
    columns = list(),
    choices = list(
      `1` = list(recode = "1", description = "First choice"),
      `2` = list(recode = "2", description = "Second choice")
    ),
    subQuestions = list()
  )

  extra_qids <- c("QID508", "QID626", "QID429", "QID694", "QID121", "QID700")
  raw_metadata$description$blocks$BL_1$BlockElements <- c(
    raw_metadata$description$blocks$BL_1$BlockElements,
    lapply(extra_qids, function(qid) list(QuestionID = qid))
  )
  for (qid in extra_qids) {
    raw_metadata$description$questions[[qid]] <- list(
      Validation = list(Settings = list(ContentType = NULL))
    )
  }
  raw_metadata$metadata$embedded_data <- list(
    list(name = "Source Channel"),
    list(name = "Cohort")
  )
  raw_metadata$description$scoring <- list(
    ScoringCategories = list(
      list(ID = "SC_TOTAL", Name = "Total Score"),
      list(ID = "SC_HIDDEN", Name = "Hidden Score")
    )
  )

  glad_ids <- glad_sa6_text_analysis_sidecar_ids()
  glad_sub <- c(
    "Topics",
    "Actionability",
    "Effort",
    "Effort Numeric",
    "Emotion Intensity",
    "Emotion",
    "Parent Topics",
    "Sentiment Polarity",
    "Sentiment Score",
    "Sentiment",
    "Topic Sentiment Label",
    "Topic Sentiment Score",
    "Topic Hierarchy Level 1"
  )
  edgi_ids <- edgi_signup_text_analysis_sidecar_ids()
  edgi_sub <- c("Parent Topics", "Topics")
  ordinary_ids <- c(
    "QID1",
    "QID1_3_TEXT",
    "QID508_TEXT",
    "QID626_TEXT",
    "QID429_TEXT",
    "QID700_1",
    "QID121_1",
    "QID700_DO_1",
    "x27_QID700_1",
    "8_QID508_TEXT"
  )
  system_ids <- c("StartTime", "EndDate", "Q_URL")

  raw_metadata$response_column_map <- tibble::tibble(
    qname = c(
      system_ids,
      ordinary_ids,
      glad_ids,
      edgi_ids,
      "Source Channel",
      "SC_TOTAL"
    ),
    ImportId = c(
      system_ids,
      c(
        "QID1",
        "QID1_3_TEXT",
        "QID508_TEXT",
        "QID626_TEXT",
        "QID429_TEXT",
        "QID700",
        "QID121_1",
        "QID700",
        "QID700",
        "QID508_TEXT"
      ),
      glad_ids,
      edgi_ids,
      "Source Channel",
      "SC_TOTAL"
    ),
    description = c(
      "Start Time",
      "End Date",
      "Survey URL",
      "Choose one",
      "Choose one - Other",
      "CurrentAge",
      "DisabilityAdditionalInfo",
      "Other study information",
      "Select all that apply - First choice",
      "ED.ANT.6.0 - First text field",
      "Select all that apply - Display Order - First choice",
      "Select all that apply - First choice",
      "CurrentAge",
      paste("CAM.TV.1txt.0 -", glad_sub),
      paste("ED.ANT.6.0_1 -", edgi_sub),
      "Source Channel",
      "Total Score"
    ),
    main = c(
      "Start Time",
      "End Date",
      "Survey URL",
      "Choose one",
      "Choose one",
      "CurrentAge",
      "DisabilityAdditionalInfo",
      "Other study information",
      "Select all that apply",
      "ED.ANT.6.0",
      "Select all that apply",
      "Select all that apply",
      "CurrentAge",
      rep("CAM.TV.1txt.0", length(glad_ids)),
      rep("ED.ANT.6.0_1", length(edgi_ids)),
      "Source Channel",
      "Total Score"
    ),
    sub = c(
      "",
      "",
      "",
      "",
      "Other",
      "",
      "",
      "",
      "First choice",
      "First text field",
      "Display Order - First choice",
      "First choice",
      "",
      glad_sub,
      edgi_sub,
      "",
      ""
    )
  )

  raw_metadata
}

synthetic_survey_flow_embedded_data_raw_metadata <- function() {
  raw_metadata <- synthetic_mc_text_raw_metadata()
  raw_metadata$metadata$questions$QID2 <- raw_metadata$metadata$questions$QID1
  raw_metadata$metadata$questions$QID2$questionName <- "Q2"
  raw_metadata$metadata$questions$QID2$questionText <- "Follow-up question"
  raw_metadata$description$questions$QID2 <-
    raw_metadata$description$questions$QID1
  raw_metadata$description$blocks$BL_2 <- raw_metadata$description$blocks$BL_1
  raw_metadata$description$blocks$BL_2$Description <- "Follow-up Block"
  raw_metadata$description$blocks$BL_2$BlockElements <- list(
    list(QuestionID = "QID2")
  )
  raw_metadata$metadata$flow <- list(
    Flow = list(
      list(
        Type = "EmbeddedData",
        EmbeddedData = list(list(Field = "Before Main"))
      ),
      list(Type = "Block", ID = "BL_1"),
      list(
        Type = "EmbeddedData",
        EmbeddedData = list(list(Field = "Between Blocks"))
      ),
      list(Type = "Block", ID = "BL_2"),
      list(
        Type = "EmbeddedData",
        EmbeddedData = list(list(Field = "After Follow-up"))
      )
    )
  )

  raw_metadata
}

synthetic_nested_survey_flow_embedded_data_raw_metadata <- function() {
  raw_metadata <- synthetic_survey_flow_embedded_data_raw_metadata()
  raw_metadata$metadata$flow$Flow[[3]] <- list(
    Type = "Branch",
    Flow = list(raw_metadata$metadata$flow$Flow[[3]])
  )

  raw_metadata
}

synthetic_ambiguous_survey_flow_embedded_data_raw_metadata <- function() {
  raw_metadata <- synthetic_mc_text_raw_metadata()
  raw_metadata$metadata$flow <- list(
    Flow = list(
      list(
        Type = "EmbeddedData",
        EmbeddedData = list(list(Field = "Duplicated Field"))
      ),
      list(Type = "Block", ID = "BL_1"),
      list(
        Type = "EmbeddedData",
        EmbeddedData = list(list(Field = "Duplicated Field"))
      )
    )
  )

  raw_metadata
}

synthetic_loop_and_merge_raw_metadata <- function(
  question_text = "Why did you choose ${lm://Field/1}?"
) {
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
      blocks = list(
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
      questions = list(
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

synthetic_multi_field_loop_and_merge_raw_metadata <- function() {
  raw_metadata <- synthetic_loop_and_merge_raw_metadata(
    "Compare ${lm://Field/1} with ${lm://Field/2}"
  )

  raw_metadata$description$blocks$BL_LOOP$Options$LoopingOptions$Static <- list(
    x1 = list(`1` = "", `2` = "Red fruit"),
    x2 = list(`1` = "", `2` = "Yellow fruit")
  )

  raw_metadata
}

synthetic_static_loop_and_merge_raw_metadata <- function() {
  raw_metadata <- synthetic_loop_and_merge_raw_metadata(
    "Compare ${lm://Field/1} with ${lm://Field/2}"
  )

  raw_metadata$metadata$questions$QID1 <- NULL
  raw_metadata$description$questions$QID1 <- NULL
  raw_metadata$description$blocks$BL_SOURCE <- NULL
  raw_metadata$description$blocks$BL_LOOP$Options$LoopingOptions$QID <- NULL
  raw_metadata$description$blocks$BL_LOOP$Options$LoopingOptions$Static <- list(
    `1` = list(`1` = "Apples", `2` = "Red fruit"),
    `2` = list(`1` = "Bananas", `2` = "Yellow fruit")
  )

  raw_metadata
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
      blocks = list(
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
      questions = list(
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
  raw_metadata$description$blocks$BL_LOOP$Options$LoopingOptions$Static <-
    stats::setNames(
      lapply(1:12, function(i) list(`1` = "")),
      as.character(1:12)
    )

  raw_metadata$metadata$questions$QID2$questionName <- "Q3"
  names(raw_metadata$metadata$questions)[2] <- "QID3"
  raw_metadata$description$blocks$BL_LOOP$BlockElements <- list(
    list(QuestionID = "QID3")
  )
  raw_metadata$description$questions$QID3 <-
    raw_metadata$description$questions$QID2
  raw_metadata$description$questions$QID2 <- NULL

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
  raw_metadata$description$blocks$BL_LOOP$Options$LoopingOptions$Static <-
    stats::setNames(
      lapply(1:12, function(i) list(`1` = "")),
      as.character(1:12)
    )

  raw_metadata$metadata$questions$QID2$questionName <- "Q3"
  names(raw_metadata$metadata$questions)[2] <- "QID3"
  raw_metadata$description$blocks$BL_LOOP$BlockElements <- list(
    list(QuestionID = "QID3")
  )
  raw_metadata$description$questions$QID3 <-
    raw_metadata$description$questions$QID2
  raw_metadata$description$questions$QID2 <- NULL

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
      blocks = list(
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
      questions = list(
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
