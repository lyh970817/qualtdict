#' Return non-empty Response Column IDs for classification rows
#' @noRd
response_column_map_ids <- function(response_column_map) {
  ids <- response_column_map_row_ids(response_column_map)
  ids[!is.na(ids) & nzchar(ids)]
}

#' Resolve row-aligned Response Column IDs for classification rows
#' @noRd
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

#' Fields used by Response Column Map Classification
#' @noRd
response_column_map_id_columns <- function() {
  c("qname", "ImportId")
}

#' Classify rows by Dictionary Row Source
#' @noRd
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

#' Build a Response Column Map Classification table
#' @noRd
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

  rows
}

#' Empty Response Column Map Classification table
#' @noRd
empty_response_column_map_classification <- function() {
  new_response_column_map_classification()
}

#' Classify one Response Column Map Classification row
#' @noRd
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

#' Resolve the row class for one Response Column Map Classification context
#' @noRd
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

#' Response Column Map Classification rules in priority order
#' @noRd
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

#' Classify rows with missing Response Column IDs
#' @noRd
response_column_map_missing_id_class <- function(context) {
  response_column_id <- context$response_column_id
  if (is.na(response_column_id) || !nzchar(response_column_id)) {
    response_column_map_class("unknown", "missing_response_column_id")
  }
}

#' Classify Embedded Data Field rows
#' @noRd
response_column_map_embedded_data_class <- function(context) {
  if (context$response_column_id %in% context$embedded_data_ids) {
    response_column_map_class("embedded_data", "embedded_data")
  }
}

#' Classify Scoring Variable rows
#' @noRd
response_column_map_scoring_class <- function(context) {
  if (context$response_column_id %in% context$scoring_ids) {
    response_column_map_class("scoring", "scoring")
  }
}

#' Classify question-backed rows
#' @noRd
response_column_map_question_class <- function(context) {
  if (context$response_column_id %in% context$ordinary_question_ids) {
    response_column_map_class("question", "rendered_question")
  }
}

#' Classify system metadata rows
#' @noRd
response_column_map_system_class <- function(context) {
  if (is_system_response_column(context$response_column_id)) {
    response_column_map_class("system", "system_metadata")
  }
}

#' Classify display-order auxiliary rows
#' @noRd
response_column_map_display_order_class <- function(context) {
  if (is_display_order_response_column(context$response_column_id)) {
    response_column_map_class("question_auxiliary", "display_order")
  }
}

#' Classify rows that lack a known parent QID
#' @noRd
response_column_map_missing_parent_class <- function(context) {
  parent_qid <- context$parent_qid
  if (is.na(parent_qid) || !parent_qid %in% names(context$questions)) {
    response_column_map_class("unknown", "no_known_parent_qid")
  }
}

#' Classify loop-prefixed QID-shaped auxiliary rows
#' @noRd
response_column_map_loop_prefixed_class <- function(context) {
  if (is_loop_prefixed_qid_response_column(context$response_column_id)) {
    response_column_map_class("question_auxiliary", "ordinary_loop_qid_shape")
  }
}

#' Classify ordinary QID-shaped auxiliary rows
#' @noRd
response_column_map_ordinary_qid_class <- function(context) {
  if (is_ordinary_qid_response_column(context$response_column_id)) {
    response_column_map_class("question_auxiliary", "ordinary_qid_shape")
  }
}

#' Classify Text-analysis Sidecar rows
#' @noRd
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

#' Build a Response Column Map Classification result
#' @noRd
response_column_map_class <- function(row_source, reason) {
  list(row_source = row_source, reason = reason)
}

#' Render ordinary question-backed Response Column IDs
#' @noRd
ordinary_question_response_column_ids <- function(questions) {
  if (is.null(questions) || length(questions) == 0) {
    return(character())
  }

  question_facts <- expand_loop_question_facts(questions)
  ids <- imap(question_facts, function(question_fact, qid) {
    base_response_column_id <- question_fact$base_response_column_id %||% qid
    render_response_columns(
      question_fact,
      base_response_column_id
    )$response_column_id
  })
  unique(unname(unlist(ids, use.names = FALSE)))
}

#' Return non-empty Response Column IDs from normalised records
#' @noRd
normalised_response_column_ids <- function(records) {
  if (is.null(records) || length(records) == 0) {
    return(character())
  }

  ids <- map_chr(records, function(record) {
    scalar_character(record$response_column_id)
  })
  ids[!is.na(ids) & nzchar(ids)]
}

#' Return whether a Response Column ID is system metadata
#' @noRd
is_system_response_column <- function(response_column_id) {
  response_column_id %in%
    system_response_column_ids() ||
    grepl("^Q_", response_column_id)
}

#' Known Qualtrics system metadata Response Column IDs
#' @noRd
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

#' Return whether a Response Column ID has ordinary QID shape
#' @noRd
is_ordinary_qid_response_column <- function(response_column_id) {
  grepl("^QID[0-9]+$", response_column_id) ||
    grepl("^QID[0-9]+_[^_]+$", response_column_id) ||
    grepl("^QID[0-9]+_[^_]+_TEXT$", response_column_id) ||
    grepl("^QID[0-9]+#[^_]+_[^_]+(?:_[^_]+)?$", response_column_id)
}

#' Return whether a Response Column ID is display order
#' @noRd
is_display_order_response_column <- function(response_column_id) {
  grepl("^QID[0-9]+_DO_[^_]+$", response_column_id)
}

#' Return whether a Response Column ID has loop-prefixed QID shape
#' @noRd
is_loop_prefixed_qid_response_column <- function(response_column_id) {
  grepl(
    "^[^_]+_QID[0-9]+(?:#[^_]+)?(?:_[^_]+)*(?:_TEXT)?$",
    response_column_id
  )
}

#' Return whether derived Response Column Map Classification fields are present
#' @noRd
has_derived_response_column_map_fields <- function(main, sub, description) {
  fields <- c(main, sub, description)
  fields <- fields[!is.na(fields)]
  any(nzchar(fields))
}

#' Resolve the display name for a Response Column Map Classification row
#' @noRd
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

#' Resolve the parent QID from a Response Column ID
#' @noRd
response_column_map_parent_qid <- function(response_column_id) {
  if (is.na(response_column_id) || !nzchar(response_column_id)) {
    return(NA_character_)
  }

  parent_qid <- str_extract(response_column_id, "QID[0-9]+")
  parent_qid %||% NA_character_
}

#' Return one scalar Response Column Map Classification field
#' @noRd
response_column_map_scalar <- function(row, column) {
  if (!column %in% names(row)) {
    return(NA_character_)
  }

  scalar_character(row[[column]])
}
