new_normalised_metadata <- function(
  survey_id,
  survey_name,
  survey_question_facts,
  embedded_data_fields,
  scoring_variables,
  text_analysis_sidecars
) {
  structure(
    list(
      surveyID = survey_id,
      survey_name = survey_name,
      questions = survey_question_facts,
      embedded_data = embedded_data_fields,
      scoring = scoring_variables,
      text_analysis = text_analysis_sidecars
    ),
    class = c("qualtdict_normalised_metadata", "list")
  )
}

new_response_column_map_classification <- function(rows = NULL) {
  if (is.null(rows)) {
    return(tibble::tibble(
      response_column_id = character(),
      row_source = character(),
      parent_qid = character(),
      display_name = character(),
      main = character(),
      sub = character(),
      description = character(),
      reason = character()
    ))
  }

  tibble::as_tibble(rows)
}

new_normalised_text_analysis_sidecars <- function(sidecars = list()) {
  structure(
    sidecars,
    class = c("qualtdict_normalised_text_analysis_sidecars", "list")
  )
}

new_normalised_text_analysis_sidecar <- function(
  sidecar_name,
  response_column_id,
  parent_context
) {
  structure(
    list(
      sidecar_name = sidecar_name,
      response_column_id = response_column_id,
      question_text = paste("Text Analysis:", sidecar_name),
      parent_qid = parent_context$parent_qid,
      parent_question_name = parent_context$parent_question_name,
      parent_block = parent_context$parent_block
    ),
    class = c("qualtdict_normalised_text_analysis_sidecar", "list")
  )
}

#' Normalise raw Qualtrics metadata
#'
#' The normalised metadata model is internal and question-level for this tracer
#' bullet. It owns the merged question, block, loop, and content-type metadata
#' that dictionary row generation consumes.
#'
#' @keywords internal
#' @noRd
normalise_qualtrics_metadata <- function(raw_metadata) {
  survey_question_facts <- normalise_qualtrics_questions(
    raw_metadata$metadata,
    raw_metadata$description
  )
  embedded_data_fields <- normalise_embedded_data_fields(
    raw_metadata$metadata,
    raw_metadata$description
  )
  response_column_map <- raw_metadata$response_column_map
  embedded_data_fields <- filter_exported_embedded_data_fields(
    embedded_data_fields,
    response_column_map
  )
  scoring_variables <- normalise_scoring_variables(
    raw_metadata$description,
    response_column_map = response_column_map
  )
  response_column_classification <- classify_response_column_map(
    response_column_map,
    questions = survey_question_facts,
    embedded_data = embedded_data_fields,
    scoring = scoring_variables
  )
  text_analysis_sidecars <- normalise_text_analysis_sidecars(
    survey_question_facts,
    response_column_classification = response_column_classification
  )

  new_normalised_metadata(
    survey_id = raw_metadata$surveyID,
    survey_name = raw_metadata$survey_name,
    survey_question_facts = survey_question_facts,
    embedded_data_fields = embedded_data_fields,
    scoring_variables = scoring_variables,
    text_analysis_sidecars = text_analysis_sidecars
  )
}

normalise_text_analysis_sidecars <- function(
  questions,
  response_column_classification = NULL
) {
  sidecar_records <- text_analysis_sidecars_from_response_column_map(
    response_column_classification
  )
  if (length(sidecar_records) == 0) {
    return(empty_normalised_text_analysis_sidecars())
  }

  sidecars <- map(
    sidecar_records,
    normalise_text_analysis_sidecar,
    questions
  ) |>
    discard(is.null)

  names(sidecars) <- map_chr(sidecars, "sidecar_name")

  new_normalised_text_analysis_sidecars(sidecars)
}

response_column_map_ids <- function(response_column_map) {
  ids <- response_column_map_row_ids(response_column_map)
  ids[!is.na(ids) & nzchar(ids)]
}

response_column_map_row_ids <- function(response_column_map) {
  if (is.null(response_column_map) || !is.data.frame(response_column_map)) {
    return(character())
  }

  ids <- rep(NA_character_, nrow(response_column_map))
  for (id_column in response_column_map_id_columns()) {
    if (!id_column %in% names(response_column_map)) {
      next
    }

    candidate_ids <- as.character(response_column_map[[id_column]])
    use_candidate <- is.na(ids) | !nzchar(ids)
    valid_candidate <- !is.na(candidate_ids) & nzchar(candidate_ids)
    ids[use_candidate & valid_candidate] <-
      candidate_ids[use_candidate & valid_candidate]
  }

  ids
}

response_column_map_id_columns <- function() {
  c("qname", "ImportId")
}

classify_response_column_map <- function(
  response_column_map,
  questions,
  embedded_data,
  scoring
) {
  if (is.null(response_column_map) || !is.data.frame(response_column_map)) {
    return(empty_response_column_map_classification())
  }
  if (nrow(response_column_map) == 0) {
    return(empty_response_column_map_classification())
  }

  response_column_ids <- response_column_map_row_ids(response_column_map)
  ordinary_question_ids <- ordinary_question_response_column_ids(questions)
  embedded_data_ids <- normalised_response_column_ids(embedded_data)
  scoring_ids <- normalised_response_column_ids(scoring)

  rows <- map2_df(
    seq_len(nrow(response_column_map)),
    response_column_ids,
    classify_response_column_map_row,
    response_column_map = response_column_map,
    questions = questions,
    ordinary_question_ids = ordinary_question_ids,
    embedded_data_ids = embedded_data_ids,
    scoring_ids = scoring_ids
  )

  new_response_column_map_classification(rows)
}

empty_response_column_map_classification <- function() {
  new_response_column_map_classification()
}

classify_response_column_map_row <- function(
  row_index,
  response_column_id,
  response_column_map,
  questions,
  ordinary_question_ids,
  embedded_data_ids,
  scoring_ids
) {
  row <- response_column_map[row_index, , drop = FALSE]
  main <- response_column_map_scalar(row, "main")
  sub <- response_column_map_scalar(row, "sub")
  description <- response_column_map_scalar(row, "description")
  display_name <- response_column_map_display_name(
    row,
    response_column_id
  )
  parent_qid <- response_column_map_parent_qid(response_column_id)
  classification <- response_column_map_row_class(
    response_column_id = response_column_id,
    parent_qid = parent_qid,
    questions = questions,
    ordinary_question_ids = ordinary_question_ids,
    embedded_data_ids = embedded_data_ids,
    scoring_ids = scoring_ids,
    main = main,
    sub = sub,
    description = description
  )

  tibble(
    response_column_id = response_column_id,
    row_source = classification$row_source,
    parent_qid = parent_qid,
    display_name = display_name,
    main = main,
    sub = sub,
    description = description,
    reason = classification$reason
  )
}

response_column_map_row_class <- function(
  response_column_id,
  parent_qid,
  questions,
  ordinary_question_ids,
  embedded_data_ids,
  scoring_ids,
  main,
  sub,
  description
) {
  context <- list(
    response_column_id = response_column_id,
    parent_qid = parent_qid,
    questions = questions,
    ordinary_question_ids = ordinary_question_ids,
    embedded_data_ids = embedded_data_ids,
    scoring_ids = scoring_ids,
    main = main,
    sub = sub,
    description = description
  )

  for (rule in response_column_map_classification_rules()) {
    classification <- rule(context)
    if (!is.null(classification)) {
      return(classification)
    }
  }

  response_column_map_class("unknown", "no_derived_column_map_fields")
}

response_column_map_classification_rules <- function() {
  list(
    response_column_map_missing_id_class,
    response_column_map_embedded_data_class,
    response_column_map_scoring_class,
    response_column_map_question_class,
    response_column_map_system_class,
    response_column_map_display_order_class,
    response_column_map_missing_parent_class,
    response_column_map_loop_prefixed_class,
    response_column_map_ordinary_qid_class,
    response_column_map_text_analysis_class
  )
}

response_column_map_missing_id_class <- function(context) {
  response_column_id <- context$response_column_id
  if (is.na(response_column_id) || !nzchar(response_column_id)) {
    response_column_map_class("unknown", "missing_response_column_id")
  }
}

response_column_map_embedded_data_class <- function(context) {
  if (context$response_column_id %in% context$embedded_data_ids) {
    response_column_map_class("embedded_data", "embedded_data")
  }
}

response_column_map_scoring_class <- function(context) {
  if (context$response_column_id %in% context$scoring_ids) {
    response_column_map_class("scoring", "scoring")
  }
}

response_column_map_question_class <- function(context) {
  if (context$response_column_id %in% context$ordinary_question_ids) {
    response_column_map_class("question", "rendered_question")
  }
}

response_column_map_system_class <- function(context) {
  if (is_system_response_column(context$response_column_id)) {
    response_column_map_class("system", "system_metadata")
  }
}

response_column_map_display_order_class <- function(context) {
  if (is_display_order_response_column(context$response_column_id)) {
    response_column_map_class("question_auxiliary", "display_order")
  }
}

response_column_map_missing_parent_class <- function(context) {
  parent_qid <- context$parent_qid
  if (is.na(parent_qid) || !parent_qid %in% names(context$questions)) {
    response_column_map_class("unknown", "no_known_parent_qid")
  }
}

response_column_map_loop_prefixed_class <- function(context) {
  if (is_loop_prefixed_qid_response_column(context$response_column_id)) {
    response_column_map_class("question_auxiliary", "ordinary_loop_qid_shape")
  }
}

response_column_map_ordinary_qid_class <- function(context) {
  if (is_ordinary_qid_response_column(context$response_column_id)) {
    response_column_map_class("question_auxiliary", "ordinary_qid_shape")
  }
}

response_column_map_text_analysis_class <- function(context) {
  if (
    has_derived_response_column_map_fields(
      context$main,
      context$sub,
      context$description
    )
  ) {
    response_column_map_class("text_analysis", "derived_question")
  }
}

response_column_map_class <- function(row_source, reason) {
  list(row_source = row_source, reason = reason)
}

ordinary_question_response_column_ids <- function(questions) {
  if (is.null(questions) || length(questions) == 0) {
    return(character())
  }

  question_facts <- expand_loop_question_facts(questions)
  ids <- imap(question_facts, function(question_fact, qid) {
    response_column_qid <- question_fact$response_column_qid %||% qid
    render_response_columns(
      question_fact,
      response_column_qid
    )$response_column_id
  })
  unique(unname(unlist(ids, use.names = FALSE)))
}

normalised_response_column_ids <- function(records) {
  if (is.null(records) || length(records) == 0) {
    return(character())
  }

  ids <- map_chr(records, function(record) {
    scalar_character(record$response_column_id)
  })
  ids[!is.na(ids) & nzchar(ids)]
}

is_system_response_column <- function(response_column_id) {
  response_column_id %in%
    system_response_column_ids() ||
    grepl("^Q_", response_column_id)
}

system_response_column_ids <- function() {
  c(
    "StartTime",
    "EndTime",
    "StartDate",
    "EndDate",
    "startDate",
    "endDate",
    "status",
    "ipAddress",
    "progress",
    "duration",
    "finished",
    "recordedDate",
    "_recordId",
    "recipientLastName",
    "recipientFirstName",
    "recipientEmail",
    "externalDataReference",
    "locationLatitude",
    "locationLongitude",
    "distributionChannel",
    "userLanguage",
    "Q_URL"
  )
}

is_ordinary_qid_response_column <- function(response_column_id) {
  grepl("^QID[0-9]+$", response_column_id) ||
    grepl("^QID[0-9]+_[^_]+$", response_column_id) ||
    grepl("^QID[0-9]+_[^_]+_TEXT$", response_column_id) ||
    grepl("^QID[0-9]+#[^_]+_[^_]+(?:_[^_]+)?$", response_column_id)
}

is_display_order_response_column <- function(response_column_id) {
  grepl("^QID[0-9]+_DO_[^_]+$", response_column_id)
}

is_loop_prefixed_qid_response_column <- function(response_column_id) {
  grepl(
    "^[^_]+_QID[0-9]+(?:#[^_]+)?(?:_[^_]+)*(?:_TEXT)?$",
    response_column_id
  )
}

has_derived_response_column_map_fields <- function(main, sub, description) {
  fields <- c(main, sub, description)
  fields <- fields[!is.na(fields)]
  any(nzchar(fields))
}

text_analysis_sidecars_from_response_column_map <- function(
  response_column_classification
) {
  if (is.null(response_column_classification)) {
    return(list())
  }
  if (nrow(response_column_classification) == 0) {
    return(list())
  }

  sidecar_rows <- response_column_classification$row_source == "text_analysis"
  if (!any(sidecar_rows, na.rm = TRUE)) {
    return(list())
  }

  sidecars <- response_column_classification[sidecar_rows, , drop = FALSE]
  map(
    seq_len(nrow(sidecars)),
    function(row_index) {
      sidecar <- sidecars[row_index, , drop = FALSE]
      list(
        sidecar_name = sidecar$display_name,
        response_column_id = sidecar$response_column_id,
        parent_qid = sidecar$parent_qid,
        main = sidecar$main,
        sub = sidecar$sub,
        description = sidecar$description
      )
    }
  )
}

response_column_map_display_name <- function(row, response_column_id) {
  candidates <- c(
    row[["description"]],
    row[["sub"]],
    row[["main"]],
    response_column_id
  )
  candidates <- as.character(candidates)
  candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
  if (length(candidates) == 0) {
    return(NA_character_)
  }

  candidates[[1]]
}

response_column_map_parent_qid <- function(response_column_id) {
  if (is.na(response_column_id) || !nzchar(response_column_id)) {
    return(NA_character_)
  }

  parent_qid <- str_extract(response_column_id, "QID[0-9]+")
  parent_qid %||% NA_character_
}

response_column_map_scalar <- function(row, column) {
  if (!column %in% names(row)) {
    return(NA_character_)
  }

  scalar_character(row[[column]])
}

normalise_text_analysis_sidecar <- function(
  sidecar,
  questions
) {
  sidecar_name <- scalar_character(sidecar$sidecar_name)
  response_column_id <- scalar_character(sidecar$response_column_id)
  if (
    is.na(sidecar_name) ||
      !nzchar(sidecar_name) ||
      is.na(response_column_id) ||
      !nzchar(response_column_id)
  ) {
    return(NULL)
  }

  parent_context <- text_analysis_sidecar_parent_context(
    scalar_character(sidecar$parent_qid),
    questions
  )

  new_normalised_text_analysis_sidecar(
    sidecar_name = sidecar_name,
    response_column_id = response_column_id,
    parent_context = parent_context
  )
}

text_analysis_sidecar_parent_context <- function(parent_qid, questions) {
  if (is.na(parent_qid)) {
    return(empty_text_analysis_sidecar_parent_context())
  }

  parent_question <- questions[[parent_qid]]
  if (is.null(parent_question)) {
    return(empty_text_analysis_sidecar_parent_context())
  }

  list(
    parent_qid = parent_qid,
    parent_question_name = parent_question$question_name,
    parent_block = parent_question$survey_block
  )
}

empty_text_analysis_sidecar_parent_context <- function() {
  list(
    parent_qid = NA_character_,
    parent_question_name = NA_character_,
    parent_block = NA_character_
  )
}

empty_normalised_text_analysis_sidecars <- function() {
  new_normalised_text_analysis_sidecars()
}

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
