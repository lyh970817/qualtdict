#' Normalise raw Qualtrics metadata
#'
#' The normalised metadata model is internal and question-level for this tracer
#' bullet. It owns the merged question, block, loop, and content-type metadata
#' that dictionary row generation consumes.
#'
#' @keywords internal
#' @noRd
normalise_qualtrics_metadata <- function(raw_metadata) {
  questions <- normalise_qualtrics_questions(
    raw_metadata$metadata,
    raw_metadata$description
  )

  structure(
    list(
      surveyID = raw_metadata$surveyID,
      survey_name = raw_metadata$survey_name,
      questions = questions
    ),
    class = c("qualtdict_normalised_metadata", "list")
  )
}

#' Merge raw question, block, and content-type metadata
#' @keywords internal
#' @noRd
normalise_qualtrics_questions <- function(mt, mt_d) {
  blocks <- mt_d$block
  block_meta <- imap(blocks, function(block, block_id) {
    looping_options <- block$Options$LoopingOptions
    list(
      description = block$Description,
      qid = unlist(map(block$BlockElements, "QuestionID")),
      looping_prefix = names(looping_options$Static),
      looping_qid = looping_options$QID,
      looping_static = looping_options$Static,
      looping_column_names = mt$loopAndMerge[[block_id]]$columnNames
    )
  }) %>%
    map(function(block) {
      map(block$qid, ~ list(
        qid = .x,
        description = block$description,
        looping_prefix = block$looping_prefix,
        looping_qid = block$looping_qid,
        looping_static = block$looping_static,
        looping_column_names = block$looping_column_names
      ))
    }) %>%
    # Use 'c' to combine multiple lists into one list
    # Previously the lists are nested in block and then QID
    do.call(c, .) %>%
    setNames(map_chr(., ~ .x$qid))

  question_meta <- map(
    mt$questions, `[`,
    c(
      "questionName",
      "questionType",
      "questionText",
      "blocks",
      "columns",
      "choices",
      "subQuestions"
    )
  )

  content_type_meta <- mt_d$question %>%
    map("Validation") %>%
    map("Settings") %>%
    map("ContentType") %>%
    map(null_na) %>%
    map(str_remove, "Valid")

  # Order the metadatas by QID name and use only those in question_meta so that
  # the questions match.
  qids <- names(question_meta)
  question_meta <- question_meta[qids] %>%
    order_name()

  block_meta <- block_meta[qids] %>%
    order_name()

  content_type_meta <- content_type_meta[qids] %>%
    order_name()

  question_meta <- imap(question_meta, function(question, qid) {
    block <- block_meta[[qid]]
    if (is.null(block)) {
      block <- list(
        description = NA_character_,
        looping_prefix = character(),
        looping_qid = NA_character_,
        looping_static = NULL,
        looping_column_names = NULL
      )
    }

    normalise_question_fact(
      qid = qid,
      question = question,
      block = block,
      content_type = content_type_meta[[qid]]
    )
  })

  structure(question_meta, class = c("qualtdict_normalised_questions", "list"))
}
