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
  embedded_data <- normalise_flat_embedded_data_fields(raw_metadata$metadata)

  structure(
    list(
      surveyID = raw_metadata$surveyID,
      survey_name = raw_metadata$survey_name,
      questions = questions,
      embedded_data = embedded_data
    ),
    class = c("qualtdict_normalised_metadata", "list")
  )
}

normalise_flat_embedded_data_fields <- function(mt) {
  embedded_data <- mt$embedded_data %||% mt$embeddedData
  field_names <- embedded_data_field_names(embedded_data)

  fields <- map(field_names, function(field_name) {
    structure(
      list(
        field_name = field_name,
        response_column_id = field_name,
        question_text = paste("Embedded Data:", field_name)
      ),
      class = c("qualtdict_normalised_embedded_data_field", "list")
    )
  })
  names(fields) <- field_names

  structure(
    fields,
    class = c("qualtdict_normalised_embedded_data_fields", "list")
  )
}

embedded_data_field_name_columns <- c(
  "name",
  "fieldName",
  "field_name",
  "field",
  "Field",
  "DataField"
)

valid_embedded_data_field_names <- function(field_names) {
  field_names[!is.na(field_names) & nzchar(field_names)]
}

embedded_data_field_names <- function(embedded_data) {
  if (is.null(embedded_data) || length(embedded_data) == 0) {
    return(character())
  }

  if (is.data.frame(embedded_data)) {
    return(embedded_data_field_names_from_data_frame(embedded_data))
  }

  if (is.atomic(embedded_data)) {
    return(embedded_data_field_names_from_atomic(embedded_data))
  }

  field_names <- map2_chr(
    embedded_data,
    names(embedded_data) %||% rep(NA_character_, length(embedded_data)),
    embedded_data_field_name
  )
  unname(valid_embedded_data_field_names(field_names))
}

embedded_data_field_names_from_data_frame <- function(embedded_data) {
  field_columns <- intersect(
    embedded_data_field_name_columns,
    names(embedded_data)
  )
  if (length(field_columns) == 0) {
    return(character())
  }
  field_column <- field_columns[[1]]

  field_names <- as.character(embedded_data[[field_column]])
  valid_embedded_data_field_names(field_names)
}

embedded_data_field_names_from_atomic <- function(embedded_data) {
  field_names <- as.character(embedded_data)
  if (is.null(names(embedded_data))) {
    return(valid_embedded_data_field_names(field_names))
  }

  named_fields <- names(embedded_data)
  use_names <- !is.na(named_fields) & nzchar(named_fields)
  field_names[use_names] <- named_fields[use_names]
  valid_embedded_data_field_names(field_names)
}

embedded_data_field_name <- function(field, fallback_name = NA_character_) {
  if (is.atomic(field) && length(field) == 1) {
    return(scalar_character(field))
  }

  candidates <- map_chr(
    embedded_data_field_name_columns,
    function(candidate_name) {
      if (!candidate_name %in% names(field)) {
        return(NA_character_)
      }

      scalar_character(field[[candidate_name]])
    }
  )
  candidates <- valid_embedded_data_field_names(candidates)
  if (length(candidates) > 0) {
    return(candidates[[1]])
  }

  scalar_character(fallback_name)
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
  imap(mt_d$block, function(block, block_id) {
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
  content_type_meta <- mt_d$question |>
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
