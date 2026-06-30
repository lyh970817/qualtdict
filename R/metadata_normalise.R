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
  embedded_data <- normalise_embedded_data_fields(
    raw_metadata$metadata,
    raw_metadata$description
  )
  scoring <- normalise_scoring_variables(raw_metadata$description)

  structure(
    list(
      surveyID = raw_metadata$surveyID,
      survey_name = raw_metadata$survey_name,
      questions = questions,
      embedded_data = embedded_data,
      scoring = scoring
    ),
    class = c("qualtdict_normalised_metadata", "list")
  )
}

normalise_scoring_variables <- function(mt_d) {
  scoring <- mt_d$scoring %||%
    mt_d$Scoring %||%
    mt_d$scoringData %||%
    mt_d$ScoringData
  scoring <- scoring_variable_definitions(scoring)
  output_names <- scoring_variable_names(scoring)

  variables <- map(output_names, function(output_name) {
    output <- scoring_variable_record(scoring, output_name)
    response_column_id <- scoring_variable_response_column_id(
      output,
      output_name
    )

    structure(
      list(
        output_name = output_name,
        response_column_id = response_column_id,
        question_text = paste("Scoring Variable:", output_name)
      ),
      class = c("qualtdict_normalised_scoring_variable", "list")
    )
  })
  names(variables) <- output_names

  structure(
    variables,
    class = c("qualtdict_normalised_scoring_variables", "list")
  )
}

scoring_variable_name_columns <- c(
  "name",
  "Name",
  "outputName",
  "OutputName",
  "scoreName",
  "ScoreName",
  "fieldName",
  "FieldName",
  "ID",
  "id"
)

scoring_variable_response_column_id_columns <- c(
  "response_column_id",
  "responseColumnId",
  "responseColumnID",
  "columnName",
  "ColumnName",
  "exportTag",
  "ExportTag",
  "importId",
  "ImportId",
  "ID",
  "id"
)

scoring_variable_definition_columns <- c(
  "ScoringCategories",
  "scoringCategories",
  "scoring_categories",
  "categories",
  "Categories"
)

scoring_variable_definitions <- function(scoring) {
  if (is.null(scoring) || !is.list(scoring)) {
    return(scoring)
  }

  definition_columns <- intersect(
    scoring_variable_definition_columns,
    names(scoring)
  )
  if (length(definition_columns) == 0) {
    return(scoring)
  }

  scoring[[definition_columns[[1]]]]
}

valid_scoring_variable_names <- function(output_names) {
  output_names[!is.na(output_names) & nzchar(output_names)]
}

scoring_variable_names <- function(scoring) {
  if (is.null(scoring) || length(scoring) == 0) {
    return(character())
  }

  if (is.data.frame(scoring)) {
    return(scoring_variable_names_from_data_frame(scoring))
  }

  if (is.atomic(scoring)) {
    return(scoring_variable_names_from_atomic(scoring))
  }

  output_names <- map2_chr(
    scoring,
    names(scoring) %||% rep(NA_character_, length(scoring)),
    scoring_variable_name
  )
  unname(valid_scoring_variable_names(output_names))
}

scoring_variable_names_from_data_frame <- function(scoring) {
  output_columns <- intersect(scoring_variable_name_columns, names(scoring))
  if (length(output_columns) == 0) {
    return(character())
  }

  output_names <- as.character(scoring[[output_columns[[1]]]])
  valid_scoring_variable_names(output_names)
}

scoring_variable_names_from_atomic <- function(scoring) {
  output_names <- as.character(scoring)
  if (is.null(names(scoring))) {
    return(valid_scoring_variable_names(output_names))
  }

  named_outputs <- names(scoring)
  use_names <- !is.na(named_outputs) & nzchar(named_outputs)
  output_names[use_names] <- named_outputs[use_names]
  valid_scoring_variable_names(output_names)
}

scoring_variable_record <- function(scoring, output_name) {
  if (is.null(scoring) || is.atomic(scoring) || is.data.frame(scoring)) {
    return(list())
  }

  named_index <- match(output_name, names(scoring))
  if (!is.na(named_index)) {
    return(scoring[[named_index]])
  }

  matches <- map_lgl(scoring, function(output) {
    identical(scoring_variable_name(output), output_name)
  })
  if (any(matches)) {
    return(scoring[[which(matches)[[1]]]])
  }

  list()
}

scoring_variable_name <- function(output, fallback_name = NA_character_) {
  if (is.atomic(output) && length(output) == 1) {
    return(scalar_character(output))
  }

  candidates <- map_chr(
    scoring_variable_name_columns,
    function(candidate_name) {
      if (!candidate_name %in% names(output)) {
        return(NA_character_)
      }

      scalar_character(output[[candidate_name]])
    }
  )
  candidates <- valid_scoring_variable_names(candidates)
  if (length(candidates) > 0) {
    return(candidates[[1]])
  }

  scalar_character(fallback_name)
}

scoring_variable_response_column_id <- function(output, output_name) {
  if (!is.list(output)) {
    return(output_name)
  }

  candidates <- map_chr(
    scoring_variable_response_column_id_columns,
    function(candidate_name) {
      if (!candidate_name %in% names(output)) {
        return(NA_character_)
      }

      scalar_character(output[[candidate_name]])
    }
  )
  candidates <- valid_scoring_variable_names(candidates)
  if (length(candidates) > 0) {
    return(candidates[[1]])
  }

  output_name
}

normalise_embedded_data_fields <- function(mt, mt_d) {
  flat_fields <- normalise_flat_embedded_data_fields(mt)
  flow_fields <- normalise_survey_flow_embedded_data_fields(mt, mt_d)

  merge_embedded_data_fields(flat_fields, flow_fields)
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

normalise_survey_flow_embedded_data_fields <- function(mt, mt_d) {
  flow_items <- survey_flow_items(mt$flow %||% mt_d$flow)
  if (length(flow_items) == 0) {
    return(empty_normalised_embedded_data_fields())
  }

  block_lookup <- survey_flow_block_lookup(mt_d$block)
  block_names <- map_chr(
    flow_items,
    survey_flow_block_name,
    block_lookup = block_lookup
  )

  field_locations <- map2(
    flow_items,
    seq_along(flow_items),
    survey_flow_embedded_data_field_locations,
    block_names = block_names
  ) |>
    do.call(c, args = _)

  if (length(field_locations) == 0) {
    return(empty_normalised_embedded_data_fields())
  }

  location_field_names <- map_chr(field_locations, "field_name")
  field_names <- unique(location_field_names)
  fields <- map(field_names, function(field_name) {
    normalise_survey_flow_embedded_data_field(
      field_locations[location_field_names == field_name]
    )
  })
  names(fields) <- map_chr(fields, "field_name")

  structure(
    fields,
    class = c("qualtdict_normalised_embedded_data_fields", "list")
  )
}

empty_normalised_embedded_data_fields <- function() {
  structure(
    list(),
    class = c("qualtdict_normalised_embedded_data_fields", "list")
  )
}

merge_embedded_data_fields <- function(flat_fields, flow_fields) {
  fields <- flat_fields
  for (field_name in names(flow_fields)) {
    fields[[field_name]] <- utils::modifyList(
      fields[[field_name]] %||% list(),
      flow_fields[[field_name]]
    )
  }

  structure(
    fields,
    class = c("qualtdict_normalised_embedded_data_fields", "list")
  )
}

survey_flow_items <- function(flow) {
  if (is.null(flow) || length(flow) == 0) {
    return(list())
  }

  if (!is.na(survey_flow_item_type(flow))) {
    items <- flow$Flow %||% flow$flow
    if (is.null(items)) {
      return(list(flow))
    }

    return(survey_flow_items(items))
  }

  items <- flow$Flow %||% flow$flow %||% flow
  if (!is.null(names(items))) {
    items <- unname(items)
  }

  unlist(
    map(items, survey_flow_items),
    recursive = FALSE,
    use.names = FALSE
  )
}

survey_flow_block_lookup <- function(blocks) {
  if (is.null(blocks) || length(blocks) == 0) {
    return(character())
  }

  imap_chr(blocks, function(block, block_id) {
    scalar_character(block$Description %||% block$description %||% block_id)
  })
}

survey_flow_block_name <- function(item, block_lookup) {
  block_id <- survey_flow_block_id(item)
  if (is.na(block_id) || !block_id %in% names(block_lookup)) {
    return(NA_character_)
  }

  block_lookup[[block_id]] %||% NA_character_
}

survey_flow_block_id <- function(item) {
  if (!identical(survey_flow_item_type(item), "Block")) {
    return(NA_character_)
  }

  scalar_character(
    item$ID %||%
      item$BlockID %||%
      item$BlockId %||%
      item$id
  )
}

survey_flow_item_type <- function(item) {
  scalar_character(item$Type %||% item$FlowType %||% item$type)
}

survey_flow_embedded_data_field_locations <- function(
  item,
  item_index,
  block_names
) {
  if (!identical(survey_flow_item_type(item), "EmbeddedData")) {
    return(list())
  }

  field_names <- survey_flow_embedded_data_field_names(item)
  if (length(field_names) == 0) {
    return(list())
  }

  previous_block <- previous_survey_flow_block(block_names, item_index)
  next_block <- next_survey_flow_block(block_names, item_index)
  map(field_names, function(field_name) {
    list(
      field_name = field_name,
      previous_block = previous_block,
      next_block = next_block
    )
  })
}

survey_flow_embedded_data_field_names <- function(item) {
  embedded_data <- item$EmbeddedData %||%
    item$EmbeddedDataFields %||%
    item$Fields %||%
    item$embeddedData %||%
    item$embedded_data

  if (is.null(embedded_data)) {
    if (is_embedded_data_field_record(item)) {
      return(valid_embedded_data_field_names(
        embedded_data_field_name(item)
      ))
    }

    return(character())
  }
  if (is_embedded_data_field_record(embedded_data)) {
    return(valid_embedded_data_field_names(
      embedded_data_field_name(embedded_data)
    ))
  }

  embedded_data_field_names(embedded_data)
}

is_embedded_data_field_record <- function(field) {
  is.list(field) &&
    any(embedded_data_field_name_columns %in% names(field))
}

previous_survey_flow_block <- function(block_names, item_index) {
  if (item_index <= 1) {
    return(NA_character_)
  }

  previous_blocks <- block_names[seq_len(item_index - 1)]
  previous_blocks <- previous_blocks[!is.na(previous_blocks)]
  if (length(previous_blocks) == 0) {
    return(NA_character_)
  }

  previous_blocks[[length(previous_blocks)]]
}

next_survey_flow_block <- function(block_names, item_index) {
  if (item_index >= length(block_names)) {
    return(NA_character_)
  }

  next_blocks <- block_names[seq.int(item_index + 1, length(block_names))]
  next_blocks <- next_blocks[!is.na(next_blocks)]
  if (length(next_blocks) == 0) {
    return(NA_character_)
  }

  next_blocks[[1]]
}

normalise_survey_flow_embedded_data_field <- function(locations) {
  field_name <- locations[[1]]$field_name
  if (length(locations) == 1) {
    previous_block <- locations[[1]]$previous_block
    next_block <- locations[[1]]$next_block
  } else {
    previous_block <- NA_character_
    next_block <- NA_character_
  }

  structure(
    list(
      field_name = field_name,
      response_column_id = field_name,
      question_text = paste("Embedded Data:", field_name),
      previous_block = previous_block,
      next_block = next_block
    ),
    class = c("qualtdict_normalised_embedded_data_field", "list")
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
