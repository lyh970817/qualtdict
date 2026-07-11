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

# Long-term-medication-form shape: grid rows carry blank labels, so the
# subQuestion recode is the only distinguishing row fact (glad QID1187 /
# edgi QID525).
synthetic_sbs_blank_row_raw_metadata <- function() {
  raw_metadata <- synthetic_sbs_multiple_answer_raw_metadata()
  raw_metadata$surveyID <- "SV_SBS_BLANK_ROW"
  raw_metadata$metadata$metadata$name <- "SBS Blank Row Survey"
  raw_metadata$metadata$questions$QID2$columns <- list(
    `1` = list(
      questionType = list(selector = "TE", subSelector = "Medium"),
      questionText = " ",
      choices = list(`1` = list(recode = "1", description = "Type"))
    )
  )
  raw_metadata$metadata$questions$QID2$columnOrder <- list("1")
  raw_metadata$metadata$questions$QID2$subQuestions <- list(
    `1` = list(recode = "1", description = "&nbsp;", choiceText = "&nbsp;"),
    `2` = list(recode = "2", description = "", choiceText = "")
  )

  raw_metadata
}

# Family-history shape: two "Age at diagnosis" text-entry columns share
# byte-identical questionText; the disease Likert column that precedes each
# is the only disambiguating adjacency (edgi QID461 / glad QID1215122586).
synthetic_sbs_duplicate_column_raw_metadata <- function() {
  raw_metadata <- synthetic_sbs_multiple_answer_raw_metadata()
  raw_metadata$surveyID <- "SV_SBS_DUP_COL"
  raw_metadata$metadata$metadata$name <- "SBS Duplicate Column Survey"
  raw_metadata$metadata$questions$QID2$questionText <- "Family history"
  raw_metadata$metadata$questions$QID2$columns <- list(
    `1` = list(
      questionType = list(selector = "Likert", subSelector = "SingleAnswer"),
      questionText = "Coronary artery disease",
      choices = list(`1` = list(recode = "1", description = "Yes"))
    ),
    `2` = list(
      questionType = list(selector = "TE", subSelector = "Medium"),
      questionText = "Age at diagnosis",
      choices = list(`1` = list(recode = "1", description = "Age"))
    ),
    `3` = list(
      questionType = list(selector = "Likert", subSelector = "SingleAnswer"),
      questionText = "Stroke",
      choices = list(`1` = list(recode = "1", description = "Yes"))
    ),
    `4` = list(
      questionType = list(selector = "TE", subSelector = "Medium"),
      questionText = "Age at diagnosis",
      choices = list(`1` = list(recode = "1", description = "Age"))
    )
  )
  raw_metadata$metadata$questions$QID2$columnOrder <- list("1", "2", "3", "4")
  raw_metadata$metadata$questions$QID2$subQuestions <- list(
    `1` = list(recode = "1", description = "Mother"),
    `2` = list(recode = "2", description = "Father")
  )

  raw_metadata
}

# Same duplicated "Age at diagnosis" columns with no distinct, differently
# typed partner to the left: the honest ordinal fallback applies.
synthetic_sbs_ordinal_column_raw_metadata <- function() {
  raw_metadata <- synthetic_sbs_duplicate_column_raw_metadata()
  raw_metadata$surveyID <- "SV_SBS_ORDINAL_COL"
  raw_metadata$metadata$metadata$name <- "SBS Ordinal Column Survey"
  raw_metadata$metadata$questions$QID2$columns <- list(
    `1` = list(
      questionType = list(selector = "TE", subSelector = "Medium"),
      questionText = "Age at diagnosis",
      choices = list(`1` = list(recode = "1", description = "Age"))
    ),
    `2` = list(
      questionType = list(selector = "TE", subSelector = "Medium"),
      questionText = "Age at diagnosis",
      choices = list(`1` = list(recode = "1", description = "Age"))
    )
  )
  raw_metadata$metadata$questions$QID2$columnOrder <- list("1", "2")

  raw_metadata
}

# Build a single-row side-by-side grid from an ordered list of column specs so
# the adjacency failure modes can be stated compactly.
sbs_grid_column <- function(selector, question_text) {
  list(
    questionType = list(selector = selector, subSelector = "SingleAnswer"),
    questionText = question_text,
    choices = list(`1` = list(recode = "1", description = "x"))
  )
}

sbs_grid_raw_metadata <- function(survey_id, columns) {
  raw_metadata <- synthetic_sbs_multiple_answer_raw_metadata()
  raw_metadata$surveyID <- survey_id
  raw_metadata$metadata$metadata$name <- survey_id
  raw_metadata$metadata$questions$QID2$questionText <- "Family history"
  named <- stats::setNames(columns, as.character(seq_along(columns)))
  raw_metadata$metadata$questions$QID2$columns <- named
  raw_metadata$metadata$questions$QID2$columnOrder <- as.list(names(named))
  raw_metadata$metadata$questions$QID2$subQuestions <- list(
    `1` = list(recode = "1", description = "Mother")
  )

  raw_metadata
}

# Reversed order: each "Age" precedes its disease. The grid still resolves to a
# single unambiguous direction (right), so the correct pairing survives and the
# trailing age is NOT mislabelled with the preceding disease.
synthetic_sbs_reversed_column_raw_metadata <- function() {
  sbs_grid_raw_metadata(
    "SV_SBS_REVERSED",
    list(
      sbs_grid_column("TE", "Age at diagnosis"),
      sbs_grid_column("Likert", "Coronary artery disease"),
      sbs_grid_column("TE", "Age at diagnosis"),
      sbs_grid_column("Likert", "Stroke")
    )
  )
}

# Duplicated anchor columns: both diseases are identically named, so no anchor
# can distinguish the age columns. The ordinal fallback applies.
synthetic_sbs_duplicate_anchor_raw_metadata <- function() {
  sbs_grid_raw_metadata(
    "SV_SBS_DUP_ANCHOR",
    list(
      sbs_grid_column("Likert", "Coronary artery disease"),
      sbs_grid_column("TE", "Age at diagnosis"),
      sbs_grid_column("Likert", "Coronary artery disease"),
      sbs_grid_column("TE", "Age at diagnosis")
    )
  )
}

# Shared value columns: two age columns trail two diseases with no per-column
# adjacency, so neither age can be attributed to one disease. Ordinal fallback.
synthetic_sbs_shared_value_column_raw_metadata <- function() {
  sbs_grid_raw_metadata(
    "SV_SBS_SHARED_VALUE",
    list(
      sbs_grid_column("Likert", "Coronary artery disease"),
      sbs_grid_column("Likert", "Stroke"),
      sbs_grid_column("TE", "Age at diagnosis"),
      sbs_grid_column("TE", "Age at diagnosis")
    )
  )
}

# Longer repeating unit (the diabetes form): an extra Likert column sits between
# the disease-age units, but each age's immediate-left disease is still its
# unique anchor and only the left direction is valid, so pairing is unambiguous.
synthetic_sbs_longer_unit_column_raw_metadata <- function() {
  sbs_grid_raw_metadata(
    "SV_SBS_LONGER_UNIT",
    list(
      sbs_grid_column("Likert", "Diabetes type 1"),
      sbs_grid_column("TE", "Age at diagnosis"),
      sbs_grid_column("Likert", "Uninterrupted insulin?"),
      sbs_grid_column("Likert", "Diabetes type 2"),
      sbs_grid_column("TE", "Age at diagnosis")
    )
  )
}

# Ambiguous direction: a trailing disease makes both a left and a right pairing
# fully valid, so no direction is unique. The ordinal fallback applies.
synthetic_sbs_ambiguous_column_raw_metadata <- function() {
  sbs_grid_raw_metadata(
    "SV_SBS_AMBIGUOUS",
    list(
      sbs_grid_column("Likert", "Coronary artery disease"),
      sbs_grid_column("TE", "Age at diagnosis"),
      sbs_grid_column("Likert", "Stroke"),
      sbs_grid_column("TE", "Age at diagnosis"),
      sbs_grid_column("Likert", "Diabetes")
    )
  )
}
