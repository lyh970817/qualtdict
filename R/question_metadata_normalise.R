#' Merge raw question, block, and content-type metadata
#' @keywords internal
#' @noRd
normalise_qualtrics_questions <- function(mt, mt_d) {
  question_meta <- question_metadata(mt)
  qids <- names(question_meta)

  block_meta <- normalise_question_block_metadata(mt, mt_d, qids)
  content_type_meta <- normalise_question_content_types(mt_d, qids)

  question_meta <- question_meta[qids] |>
    order_name()

  question_meta <- imap(question_meta, function(question, qid) {
    normalise_question_fact(
      qid = qid,
      question = question,
      block = block_meta[[qid]] %||% default_question_block_metadata(),
      content_type = content_type_meta[[qid]]
    )
  })

  structure(question_meta, class = c("qualtdict_normalised_questions", "list"))
}

normalise_question_block_metadata <- function(mt, mt_d, qids) {
  block_meta <- block_metadata(mt, mt_d) |>
    map(function(block) {
      map(
        block$qid,
        ~ list(
          qid = .x,
          description = block$description,
          looping_prefix = block$looping_prefix,
          looping_qid = block$looping_qid,
          looping_static = block$looping_static,
          looping_column_names = block$looping_column_names
        )
      )
    }) |>
    # Use 'c' to combine multiple lists into one list
    # Previously the lists are nested in block and then QID
    do.call(c, args = _)

  if (length(block_meta) == 0) {
    block_meta <- setNames(
      rep(list(NULL), length(qids)),
      qids
    )
    block_meta <- order_name(block_meta)
    return(block_meta)
  }

  block_meta <- setNames(block_meta, map_chr(block_meta, ~ .x$qid))

  block_meta[qids] |>
    order_name()
}

block_metadata <- function(mt, mt_d) {
  imap(mt_d[["blocks"]], function(block, block_id) {
    looping_options <- block$Options$LoopingOptions
    list(
      description = block$Description,
      qid = unlist(map(block$BlockElements, "QuestionID")),
      looping_prefix = names(looping_options$Static),
      looping_qid = looping_options$QID,
      looping_static = looping_options$Static,
      looping_column_names = mt$loopAndMerge[[block_id]]$columnNames
    )
  })
}

question_metadata <- function(mt) {
  map(
    mt$questions,
    `[`,
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
}

normalise_question_content_types <- function(mt_d, qids) {
  content_type_meta <- mt_d[["questions"]] |>
    map("Validation") |>
    map("Settings") |>
    map("ContentType") |>
    map(null_na) |>
    map(str_remove, "Valid")

  content_type_meta[qids] |>
    order_name()
}

default_question_block_metadata <- function() {
  list(
    description = NA_character_,
    looping_prefix = character(),
    looping_qid = NA_character_,
    looping_static = NULL,
    looping_column_names = NULL
  )
}
