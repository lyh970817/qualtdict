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
  response_column_map <- raw_metadata$response_column_map
  embedded_data <- filter_exported_embedded_data_fields(
    embedded_data,
    response_column_map
  )
  scoring <- normalise_scoring_variables(
    raw_metadata$description,
    response_column_map = response_column_map
  )
  response_column_classification <- classify_response_column_map(
    response_column_map,
    questions = questions,
    embedded_data = embedded_data,
    scoring = scoring
  )
  text_analysis <- normalise_text_analysis_sidecars(
    raw_metadata$metadata,
    raw_metadata$description,
    questions,
    response_column_map = response_column_map,
    response_column_classification = response_column_classification
  )

  structure(
    list(
      surveyID = raw_metadata$surveyID,
      survey_name = raw_metadata$survey_name,
      questions = questions,
      embedded_data = embedded_data,
      scoring = scoring,
      text_analysis = text_analysis
    ),
    class = c("qualtdict_normalised_metadata", "list")
  )
}

normalise_scoring_variables <- function(mt_d, response_column_map = NULL) {
  scoring <- mt_d$scoring %||%
    mt_d$Scoring %||%
    mt_d$scoringData %||%
    mt_d$ScoringData
  scoring <- scoring_variable_definitions(scoring)
  output_names <- scoring_variable_names(scoring)
  if (length(output_names) == 0) {
    return(empty_normalised_scoring_variables())
  }

  variables <- map(
    output_names,
    normalise_scoring_variable,
    scoring = scoring
  )
  names(variables) <- output_names
  variables <- filter_exported_scoring_variables(
    variables,
    response_column_map
  )

  structure(
    variables,
    class = c("qualtdict_normalised_scoring_variables", "list")
  )
}

empty_normalised_scoring_variables <- function() {
  structure(
    list(),
    class = c("qualtdict_normalised_scoring_variables", "list")
  )
}

filter_exported_scoring_variables <- function(variables, response_column_map) {
  response_column_ids <- response_column_map_ids(response_column_map)
  if (length(response_column_ids) == 0) {
    return(variables)
  }

  keep <- map_lgl(variables, function(variable) {
    variable$response_column_id %in% response_column_ids
  })
  variables[keep]
}

normalise_scoring_variable <- function(output_name, scoring) {
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
  output_column <- output_columns[[1]]

  output_names <- as.character(scoring[[output_column]])
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
  if (is.null(scoring) || is.atomic(scoring)) {
    return(list())
  }

  if (is.data.frame(scoring)) {
    return(scoring_variable_record_from_data_frame(scoring, output_name))
  }

  if (length(scoring) == 0) {
    return(list())
  }

  scoring_names <- names(scoring) %||% rep(NA_character_, length(scoring))
  named_index <- match(output_name, scoring_names)
  if (!is.na(named_index)) {
    return(scoring[[named_index]])
  }

  matches <- map_lgl(scoring, function(output) {
    identical(scoring_variable_name(output), output_name)
  })
  matched_index <- match(TRUE, matches)
  if (!is.na(matched_index)) {
    return(scoring[[matched_index]])
  }

  list()
}

scoring_variable_record_from_data_frame <- function(scoring, output_name) {
  output_columns <- intersect(scoring_variable_name_columns, names(scoring))
  if (length(output_columns) == 0) {
    return(list())
  }
  output_column <- output_columns[[1]]

  output_names <- as.character(scoring[[output_column]])
  matched_index <- match(output_name, output_names)
  if (is.na(matched_index)) {
    return(list())
  }

  as.list(scoring[matched_index, , drop = FALSE])
}

scoring_variable_name <- function(output, fallback_name = NA_character_) {
  if (is.atomic(output) && length(output) == 1) {
    return(scalar_character(output))
  }

  candidate <- scoring_variable_metadata_value(
    output,
    scoring_variable_name_columns
  )
  if (!is.na(candidate)) {
    return(candidate)
  }

  scalar_character(fallback_name)
}

scoring_variable_response_column_id <- function(output, output_name) {
  if (!is.list(output)) {
    return(output_name)
  }

  candidate <- scoring_variable_metadata_value(
    output,
    scoring_variable_response_column_id_columns
  )
  if (!is.na(candidate)) {
    return(candidate)
  }

  output_name
}

scoring_variable_metadata_value <- function(output, candidate_names) {
  candidates <- map_chr(candidate_names, function(candidate_name) {
    if (!candidate_name %in% names(output)) {
      return(NA_character_)
    }

    scalar_character(output[[candidate_name]])
  })
  candidates <- valid_scoring_variable_names(candidates)
  if (length(candidates) > 0) {
    return(candidates[[1]])
  }

  NA_character_
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

  block_lookup <- survey_flow_block_lookup(description_blocks(mt_d))
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

filter_exported_embedded_data_fields <- function(
  fields,
  response_column_map,
  raw_response_columns = NULL
) {
  response_column_ids <- unique(c(
    response_column_map_ids(response_column_map),
    raw_response_columns %||% character()
  ))
  if (length(response_column_ids) == 0) {
    return(fields)
  }

  keep <- map_lgl(fields, function(field) {
    field$response_column_id %in% response_column_ids
  })
  fields <- fields[keep]

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

normalise_text_analysis_sidecars <- function(
  mt,
  mt_d,
  questions,
  response_column_map = NULL,
  response_column_classification = NULL
) {
  sidecar_records <- text_analysis_sidecar_records(
    mt,
    mt_d,
    response_column_map = response_column_map,
    response_column_classification = response_column_classification
  )
  if (length(sidecar_records) == 0) {
    return(empty_normalised_text_analysis_sidecars())
  }

  sidecars <- imap(sidecar_records, function(sidecar, fallback_name) {
    normalise_text_analysis_sidecar(sidecar, fallback_name, questions)
  }) |>
    discard(is.null)

  names(sidecars) <- map_chr(sidecars, "sidecar_name")

  structure(
    sidecars,
    class = c("qualtdict_normalised_text_analysis_sidecars", "list")
  )
}

text_analysis_sidecar_records <- function(
  mt,
  mt_d,
  response_column_map = NULL,
  response_column_classification = NULL
) {
  sources <- list(
    mt$text_analysis,
    mt$textAnalysis,
    mt$textAnalysisSidecars,
    mt$comments,
    mt_d$text_analysis,
    mt_d$textAnalysis,
    mt_d$textAnalysisSidecars,
    mt_d$comments,
    text_analysis_sidecars_from_response_column_map(
      response_column_map,
      response_column_classification = response_column_classification
    )
  )
  sources <- discard(sources, is.null)
  records <- do.call(c, args = map(sources, text_analysis_sidecar_record_list))
  deduplicate_text_analysis_sidecar_records(records %||% list())
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
  c("qname", "ImportId", "importId", "response_column_id")
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

  rows
}

empty_response_column_map_classification <- function() {
  tibble(
    response_column_id = character(),
    row_source = character(),
    parent_qid = character(),
    display_name = character(),
    main = character(),
    sub = character(),
    description = character(),
    reason = character()
  )
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
  response_column_map,
  response_column_classification = NULL
) {
  if (is.null(response_column_classification)) {
    if (is.null(response_column_map) || !is.data.frame(response_column_map)) {
      return(list())
    }
    response_column_classification <- empty_response_column_map_classification()
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
        outputName = sidecar$display_name,
        responseColumnId = sidecar$response_column_id,
        questionId = sidecar$parent_qid,
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

deduplicate_text_analysis_sidecar_records <- function(records) {
  if (length(records) == 0) {
    return(list())
  }

  fallback_names <- names(records) %||% rep(NA_character_, length(records))
  keys <- map2_chr(
    records,
    fallback_names,
    text_analysis_sidecar_record_key
  )
  records[!duplicated(keys)]
}

text_analysis_sidecar_record_key <- function(sidecar, fallback_name) {
  sidecar_name <- text_analysis_sidecar_name(sidecar, fallback_name)
  response_column_id <- text_analysis_sidecar_response_column_id(
    sidecar,
    sidecar_name
  )
  if (!is.na(response_column_id) && nzchar(response_column_id)) {
    return(paste0("response_column_id:", response_column_id))
  }

  paste0("record_index:", fallback_name)
}

text_analysis_sidecar_record_list <- function(sidecars) {
  if (is.null(sidecars) || length(sidecars) == 0) {
    return(list())
  }

  if (is.data.frame(sidecars)) {
    return(map(seq_len(nrow(sidecars)), function(row) {
      as.list(sidecars[row, , drop = FALSE])
    }))
  }

  if (is_text_analysis_sidecar_record(sidecars)) {
    return(list(sidecars))
  }

  if (is.atomic(sidecars)) {
    return(as.list(sidecars))
  }

  sidecars
}

is_text_analysis_sidecar_record <- function(sidecar) {
  is.list(sidecar) &&
    any(
      c(
        text_analysis_sidecar_name_columns,
        text_analysis_sidecar_response_column_id_columns,
        text_analysis_sidecar_parent_qid_columns
      ) %in%
        names(sidecar)
    )
}

normalise_text_analysis_sidecar <- function(
  sidecar,
  fallback_name,
  questions
) {
  sidecar_name <- text_analysis_sidecar_name(sidecar, fallback_name)
  if (is.na(sidecar_name) || !nzchar(sidecar_name)) {
    return(NULL)
  }

  parent_context <- text_analysis_sidecar_parent_context(sidecar, questions)

  structure(
    list(
      sidecar_name = sidecar_name,
      response_column_id = text_analysis_sidecar_response_column_id(
        sidecar,
        sidecar_name
      ),
      question_text = paste("Text Analysis:", sidecar_name),
      parent_qid = parent_context$parent_qid,
      parent_question_name = parent_context$parent_question_name,
      parent_block = parent_context$parent_block
    ),
    class = c("qualtdict_normalised_text_analysis_sidecar", "list")
  )
}

text_analysis_sidecar_parent_context <- function(sidecar, questions) {
  parent_qid <- text_analysis_sidecar_parent_qid(sidecar)
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

text_analysis_sidecar_name_columns <- c(
  "outputName",
  "output_name",
  "name",
  "fieldName",
  "field_name",
  "variableName",
  "variable_name",
  "DataField",
  "id",
  "ID"
)

text_analysis_sidecar_response_column_id_columns <- c(
  "responseColumnId",
  "responseColumnID",
  "response_column_id",
  "columnName",
  "column_name",
  "importId",
  "import_id"
)

text_analysis_sidecar_parent_qid_columns <- c(
  "questionId",
  "questionID",
  "question_id",
  "parentQid",
  "parent_qid",
  "qid",
  "QID"
)

text_analysis_sidecar_name <- function(
  sidecar,
  fallback_name = NA_character_
) {
  text_analysis_sidecar_scalar(
    sidecar,
    text_analysis_sidecar_name_columns,
    fallback_name = fallback_name
  )
}

text_analysis_sidecar_response_column_id <- function(sidecar, sidecar_name) {
  text_analysis_sidecar_scalar(
    sidecar,
    text_analysis_sidecar_response_column_id_columns,
    fallback_name = sidecar_name
  )
}

text_analysis_sidecar_parent_qid <- function(sidecar) {
  text_analysis_sidecar_scalar(
    sidecar,
    text_analysis_sidecar_parent_qid_columns
  )
}

text_analysis_sidecar_scalar <- function(
  sidecar,
  columns,
  fallback_name = NA_character_
) {
  if (is.atomic(sidecar) && length(sidecar) == 1) {
    return(scalar_character(sidecar))
  }

  candidates <- map_chr(columns, function(candidate_name) {
    if (!candidate_name %in% names(sidecar)) {
      return(NA_character_)
    }

    scalar_character(sidecar[[candidate_name]])
  })
  candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
  if (length(candidates) > 0) {
    return(candidates[[1]])
  }

  scalar_character(fallback_name)
}

empty_normalised_text_analysis_sidecars <- function() {
  structure(
    list(),
    class = c("qualtdict_normalised_text_analysis_sidecars", "list")
  )
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
  imap(description_blocks(mt_d), function(block, block_id) {
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

description_blocks <- function(mt_d) {
  mt_d$block %||% mt_d$blocks
}

description_questions <- function(mt_d) {
  mt_d$question %||% mt_d$questions
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
  content_type_meta <- description_questions(mt_d) |>
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
