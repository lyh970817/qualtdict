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

  tibble::tibble(
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
