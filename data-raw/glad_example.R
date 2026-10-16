# Build `data/glad_example.rda`, the bundled offline example for qualtdict.
#
# Provenance: the survey structure and question wording come from the GLAD
# Study SA8 sign-up questionnaire, read from the maintainer's saved Qualtrics
# API artifacts (the local Finalize Smoke artifacts, see
# `tools/local-finalize-smoke.md`). Running this script therefore needs those
# local artifacts; nobody else can rerun it, and it is not run by
# `R CMD check`. The artifact root is read from `QUALTDICT_SMOKE_ROOT`
# (default `~/.cache/qualtdict/finalize-smoke`).
#
# What is kept from the artifacts: a small subset of Survey Blocks and
# questions (question text, choice labels, matrix items, block names),
# Embedded Data Field names, Scoring Variable definitions, and the response
# column map restricted to the kept Response Column IDs.
#
# What is removed: the survey ID and name, owner, brand, organisation and
# creator identifiers, brand URL, dates, response counts, question
# JavaScript, Embedded Data values, and every block, question, flow node,
# scoring category and column outside the subset.
#
# Participant Response Data: none is read. From `responses_raw_columns.rds`
# only the `column_map` attribute (the response schema) is taken; the
# `responses` component is generated here from the Variable Dictionary with a
# fixed seed and is entirely synthetic.
#
# Run from the package root:
#   Rscript data-raw/glad_example.R

devtools::load_all(".", quiet = TRUE)

artifact_root <- Sys.getenv(
  "QUALTDICT_SMOKE_ROOT",
  "~/.cache/qualtdict/finalize-smoke"
)
artifact_dir <- file.path(artifact_root, "source", "glad_sa8_signup")
stopifnot(dir.exists(artifact_dir))

example_survey_id <- "SV_glad0example00000"
example_survey_name <- "GLAD Signup Example"
example_timestamp <- "2026-01-01T00:00:00Z"

# ---- Selection --------------------------------------------------------------

# Demographics (partial): text entry, drop-down, single-answer and
# multi-answer multiple choice. These four questions are also the ones the
# kept Survey Flow branches test, so the flow logic stays self-contained.
# PHQ9 and GAD7 (whole blocks): a Likert matrix each plus a single-answer
# follow-up whose display logic refers to the matrix in the same block.
kept_qids <- c(
  "QID1125", # DEM.AGE.1.0  How old are you now? (TE)
  "QID1126", # DEM.COR.1.0  Country of residence (MC drop-down)
  "QID1127", # DEM.SEX.1.0  Assigned sex at birth (MC single answer)
  "QID1147", # DEM.EDU.1.0  Qualifications (MC multi-answer)
  "QID127303424", # PHQ.DEP.1.0  PHQ-9 items (Likert matrix)
  "QID127303425", # PHQ.EFT.1.0  PHQ-9 difficulty follow-up
  "QID127303430", # HEAD         GAD-7 items (Likert matrix)
  "QID127303448" # Q5           GAD-7 difficulty follow-up
)
kept_embedded_data <- c(
  "Age",
  "sex",
  "PHQRaw",
  "CurrentDepression",
  "GADRaw",
  "CurrentGAD"
)
kept_scoring_names <- c("PHQ-9", "GAD-7")
kept_system_columns <- c(
  "startDate",
  "endDate",
  "status",
  "progress",
  "duration",
  "finished",
  "recordedDate",
  "_recordId",
  "externalDataReference",
  "distributionChannel",
  "userLanguage"
)

# ---- Read the artifacts -----------------------------------------------------

raw_metadata <- readRDS(file.path(artifact_dir, "metadata.rds"))
raw_description <- readRDS(file.path(artifact_dir, "description.rds"))
# Only the response schema is taken. The response values are dropped
# immediately and never inspected.
raw_column_map <- local({
  responses <- readRDS(file.path(artifact_dir, "responses_raw_columns.rds"))
  attr(responses, "column_map", exact = TRUE)
})
invisible(gc())

# ---- Helpers ----------------------------------------------------------------

`%||%` <- function(x, y) if (is.null(x)) y else x

question_ids_in <- function(x) {
  strings <- unlist(x, use.names = FALSE)
  strings <- strings[is.character(strings)]
  unique(unlist(regmatches(strings, gregexpr("QID[0-9]+", strings))))
}

kept_block_ids <- names(Filter(
  function(block) {
    any(vapply(
      block$BlockElements,
      function(element) isTRUE(element$QuestionID %in% kept_qids),
      logical(1)
    ))
  },
  raw_description$blocks
))
stopifnot(length(kept_block_ids) == 3)

scoring_categories <- raw_description$scoring$ScoringCategories
kept_scoring_ids <- vapply(
  kept_scoring_names,
  function(name) {
    ids <- Filter(function(x) identical(x$Name, name), scoring_categories)
    stopifnot(length(ids) == 1)
    ids[[1]]$ID
  },
  character(1)
)

# Prune a description Survey Flow node and the parallel type-only node from
# `metadata()`. Returns NULL when nothing kept remains under the node.
prune_flow_node <- function(node, meta_node) {
  type <- node$Type
  if (type %in% c("Standard", "Block")) {
    stopifnot(identical(meta_node$id, node$ID))
    if (!node$ID %in% kept_block_ids) {
      return(NULL)
    }
    return(list(node = node, meta_node = meta_node))
  }
  if (type == "EmbeddedData") {
    fields <- Filter(
      function(field) field$Field %in% kept_embedded_data,
      node$EmbeddedData
    )
    if (length(fields) == 0) {
      return(NULL)
    }
    node$EmbeddedData <- lapply(fields, function(field) {
      if (!is.null(field$Value)) {
        field$Value <- ""
      }
      field
    })
    return(list(node = node, meta_node = meta_node))
  }
  if (!is.null(node[["Flow"]])) {
    children <- prune_flow(node[["Flow"]], meta_node[["flow"]])
    if (length(children$nodes) == 0) {
      return(NULL)
    }
    node[["Flow"]] <- children$nodes
    meta_node[["flow"]] <- children$meta_nodes
    return(list(node = node, meta_node = meta_node))
  }

  NULL
}

prune_flow <- function(nodes, meta_nodes) {
  stopifnot(length(nodes) == length(meta_nodes))
  pruned <- Map(prune_flow_node, nodes, meta_nodes)
  pruned <- Filter(Negate(is.null), pruned)
  list(
    nodes = unname(lapply(pruned, `[[`, "node")),
    meta_nodes = unname(lapply(pruned, `[[`, "meta_node"))
  )
}

# Keep the block elements of kept questions. Skip logic that jumps to a
# dropped question is stripped.
keep_block_elements <- function(elements, id_field) {
  elements <- Filter(
    function(element) isTRUE(element[[id_field]] %in% kept_qids),
    elements
  )
  lapply(elements, function(element) {
    skip_logic <- Filter(
      function(skip) {
        destination <- skip$SkipToDestination %||% ""
        !grepl("^QID", destination) || destination %in% kept_qids
      },
      element$SkipLogic
    )
    element$SkipLogic <- if (length(skip_logic) > 0) skip_logic
    element
  })
}

# ---- Trim and scrub metadata() ----------------------------------------------

flow <- prune_flow(raw_description$flow[["Flow"]], raw_metadata$flow)

survey_metadata <- raw_metadata
survey_metadata$metadata <- data.frame(
  surveyID = example_survey_id,
  name = example_survey_name,
  isActive = TRUE,
  creationDate = example_timestamp,
  lastModifiedDate = example_timestamp,
  expiration_startdate = NA,
  expiration_endDate = NA
)
survey_metadata$questions <- raw_metadata$questions[
  names(raw_metadata$questions) %in% kept_qids
]
survey_metadata$responsecounts <- data.frame(
  auditable = 0L,
  generated = 0L,
  deleted = 0L
)
survey_metadata$blocks <- lapply(
  raw_metadata$blocks[kept_block_ids],
  function(block) {
    block$elements <- keep_block_elements(block$elements, "questionId")
    block
  }
)
survey_metadata$flow <- flow$meta_nodes
survey_metadata$embedded_data <- lapply(
  Filter(
    function(field) field$name %in% kept_embedded_data,
    raw_metadata$embedded_data
  ),
  function(field) {
    if (!is.null(field$defaultValue)) {
      field$defaultValue <- ""
    }
    field
  }
)

# ---- Trim and scrub fetch_description() -------------------------------------

survey_description <- raw_description
survey_description$metadata <- tibble::tibble(
  name = c(
    "SurveyName",
    "SurveyStatus",
    "SurveyID",
    "LastModified",
    "LastAccessed",
    "LastActivated",
    "QuestionCount"
  ),
  value = c(
    example_survey_name,
    "Active",
    example_survey_id,
    example_timestamp,
    NA,
    example_timestamp,
    as.character(length(kept_qids))
  )
)
survey_description$flow[["Flow"]] <- flow$nodes
survey_description$blocks <- lapply(
  raw_description$blocks[kept_block_ids],
  function(block) {
    block$BlockElements <- keep_block_elements(
      block$BlockElements,
      "QuestionID"
    )
    block
  }
)
survey_description$questions <- lapply(
  raw_description$questions[
    names(raw_description$questions) %in% kept_qids
  ],
  function(question) {
    question$QuestionJS <- NULL
    # The age question's custom validation compares the answer with
    # Embedded Data Fields that are not kept, and names a message library.
    if (!is.null(question$Validation$Settings$CustomValidation)) {
      question$Validation$Settings$CustomValidation <- NULL
      question$Validation$Settings$Type <- "None"
    }
    if (length(question$GradingData) > 0) {
      question$GradingData <- lapply(question$GradingData, function(grade) {
        grade$Grades <- grade$Grades[names(grade$Grades) %in% kept_scoring_ids]
        grade
      })
    }
    question
  }
)
survey_description$scoring$ScoringCategories <- Filter(
  function(x) x$ID %in% kept_scoring_ids,
  scoring_categories
)
survey_description$scoring$ScoringCategoryGroups <- list()
survey_description$scoring$DefaultScoringCategory <-
  kept_scoring_ids[["PHQ-9"]]
survey_description$scoring$ScoringSummaryCategory <-
  kept_scoring_ids[["PHQ-9"]]

# Every question referenced by kept display logic, branch logic, or text must
# itself be kept.
stopifnot(
  all(question_ids_in(survey_description) %in% kept_qids),
  all(question_ids_in(survey_metadata) %in% kept_qids)
)

# No identifier, address, or piped text may survive the trim.
scrub_patterns <- c(
  "SV_",
  "UR_",
  "BR_",
  "@",
  "http",
  "qualtrics\\.com",
  "kcl\\.ac\\.uk",
  "kcliop",
  "\\$\\{",
  "\\$e\\{"
)
all_strings <- function(x) {
  unlist(rapply(list(x), as.character, how = "unlist"), use.names = FALSE)
}
trimmed_strings <- c(
  all_strings(survey_metadata),
  all_strings(survey_description)
)
trimmed_strings <- setdiff(trimmed_strings, example_survey_id)
for (pattern in scrub_patterns) {
  stopifnot(!any(grepl(pattern, trimmed_strings)))
}

# ---- Response column map ----------------------------------------------------

column_root_qid <- sub("_.*$", "", raw_column_map$qname)
response_column_map <- raw_column_map[
  raw_column_map$qname %in%
    c(kept_system_columns, kept_embedded_data, kept_scoring_ids) |
    column_root_qid %in% kept_qids,
]

# ---- Variable Dictionary ----------------------------------------------------

example_raw_metadata <- new_raw_qualtrics_metadata(
  surveyID = example_survey_id,
  metadata = survey_metadata,
  description = survey_description,
  response_column_map = response_column_map
)

dictionary <- testthat::with_mocked_bindings(
  dict_generate(example_survey_id, variable_name = "question_name"),
  fetch_dictionary_metadata = function(surveyID) example_raw_metadata,
  .package = "qualtdict"
)

# Every non-system column in the map is represented by the dictionary.
stopifnot(setequal(
  setdiff(response_column_map$qname, kept_system_columns),
  unique(dictionary$response_column_id)
))

# ---- Synthetic Participant Response Data -------------------------------------

set.seed(20261016)
n_responses <- 20L

dictionary_levels <- function(response_column_id) {
  as.numeric(dictionary$level[
    dictionary$response_column_id == response_column_id
  ])
}

random_id <- function(n, prefix) {
  alphabet <- c(LETTERS, letters, 0:9)
  vapply(
    seq_len(n),
    function(i) paste0(prefix, paste(sample(alphabet, 15), collapse = "")),
    character(1)
  )
}

sample_levels <- function(response_column_id, n = n_responses) {
  levels <- dictionary_levels(response_column_id)
  levels[sample.int(length(levels), n, replace = TRUE)]
}

likert_items <- function(qid) {
  ids <- unique(dictionary$response_column_id[dictionary$qid %in% qid])
  grep("^QID[0-9]+_[0-9]+$", ids, value = TRUE)
}

start_date <- as.POSIXct("2026-01-05 08:00:00", tz = "UTC") +
  sort(sample(0:(250 * 24 * 3600), n_responses))
duration <- sample(420:2400, n_responses, replace = TRUE)
end_date <- start_date + duration

values <- list(
  startDate = start_date,
  endDate = end_date,
  status = rep(0, n_responses),
  progress = rep(100, n_responses),
  duration = as.numeric(duration),
  finished = rep(1, n_responses),
  recordedDate = end_date + 1,
  `_recordId` = random_id(n_responses, "R_"),
  externalDataReference = sprintf("example-%03d", seq_len(n_responses)),
  distributionChannel = rep("anonymous", n_responses),
  userLanguage = rep("EN", n_responses)
)

# Demographics
age <- sample(18:79, n_responses, replace = TRUE)
values$QID1125_TEXT <- as.numeric(age)
values$QID1126 <- sample_levels("QID1126")
values$QID1127 <- sample_levels("QID1127")
# One tick column per qualification; "Prefer not to answer" is exclusive and
# the "Other" choice carries a free-text column.
qualification_columns <- setdiff(
  unique(dictionary$response_column_id[which(dictionary$qid == "QID1147")]),
  c("QID1147_-999", "QID1147_5_TEXT")
)
prefer_not <- stats::runif(n_responses) < 0.1
for (column in qualification_columns) {
  ticked <- !prefer_not & stats::runif(n_responses) < 0.4
  values[[column]] <- ifelse(ticked, 1, NA_real_)
}
values[["QID1147_-999"]] <- ifelse(prefer_not, 1, NA_real_)
values[["QID1147_5_TEXT"]] <- ifelse(
  is.na(values[["QID1147_5"]]),
  NA_character_,
  paste("example note", seq_len(n_responses))
)

# PHQ-9 and GAD-7 items; the follow-ups are displayed only when at least one
# item is answered above its lowest level, as their display logic says.
add_scale <- function(values, matrix_qid, follow_up_qid) {
  items <- likert_items(matrix_qid)
  for (item in items) {
    values[[item]] <- sample_levels(item)
  }
  total <- Reduce(`+`, values[items])
  shown <- total > 0
  values[[follow_up_qid]] <- ifelse(
    shown,
    sample_levels(follow_up_qid),
    NA_real_
  )
  attr(values, "total") <- total
  values
}
values <- add_scale(values, "QID127303424", "QID127303425")
phq_total <- attr(values, "total")
values <- add_scale(values, "QID127303430", "QID127303448")
gad_total <- attr(values, "total")
attr(values, "total") <- NULL

# Embedded Data Fields and Scoring Variables, derived the way the survey flow
# and scoring derive them.
values$Age <- as.numeric(age)
values$sex <- match(values$QID1127, dictionary_levels("QID1127")) - 1
values$PHQRaw <- phq_total
values$CurrentDepression <- as.numeric(
  cut(phq_total, c(-Inf, 4, 9, 14, 19, Inf), labels = FALSE) - 1
)
values$GADRaw <- gad_total
values$CurrentGAD <- as.numeric(gad_total >= 10)
values[[kept_scoring_ids[["PHQ-9"]]]] <- phq_total
values[[kept_scoring_ids[["GAD-7"]]]] <- gad_total

stopifnot(setequal(names(values), response_column_map$qname))
responses <- tibble::as_tibble(values[response_column_map$qname])
attr(responses, "column_map") <- response_column_map

# ---- Labelled Survey Data ---------------------------------------------------

labelled_survey_data <- testthat::with_mocked_bindings(
  fetch_labelled_survey_data(dictionary),
  fetch_survey2 = function(...) responses,
  .package = "qualtdict"
)

# ---- Save -------------------------------------------------------------------

glad_example <- list(
  survey_metadata = survey_metadata,
  survey_description = survey_description,
  response_column_map = response_column_map,
  responses = responses,
  dictionary = dictionary,
  labelled_survey_data = labelled_survey_data
)

save(
  glad_example,
  file = file.path("data", "glad_example.rda"),
  compress = "xz"
)
