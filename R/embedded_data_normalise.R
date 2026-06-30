new_normalised_embedded_data_fields <- function(fields = list()) {
  structure(
    fields,
    class = c("qualtdict_normalised_embedded_data_fields", "list")
  )
}

new_normalised_embedded_data_field <- function(
  field_name,
  response_column_id = field_name,
  question_text = paste("Embedded Data:", field_name),
  previous_block = NULL,
  next_block = NULL
) {
  field <- list(
    field_name = field_name,
    response_column_id = response_column_id,
    question_text = question_text
  )
  if (!is.null(previous_block)) {
    field$previous_block <- previous_block
  }
  if (!is.null(next_block)) {
    field$next_block <- next_block
  }

  structure(
    field,
    class = c("qualtdict_normalised_embedded_data_field", "list")
  )
}

normalise_embedded_data_fields <- function(mt, mt_d) {
  flat_fields <- normalise_flat_embedded_data_fields(mt)
  flow_fields <- normalise_survey_flow_embedded_data_fields(mt_d)

  merge_embedded_data_fields(flat_fields, flow_fields)
}

normalise_flat_embedded_data_fields <- function(mt) {
  embedded_data <- mt$embedded_data
  field_names <- embedded_data_field_names(embedded_data)

  fields <- map(field_names, function(field_name) {
    new_normalised_embedded_data_field(
      field_name = field_name
    )
  })
  names(fields) <- field_names

  new_normalised_embedded_data_fields(fields)
}

normalise_survey_flow_embedded_data_fields <- function(mt_d) {
  flow_items <- survey_flow_items(mt_d$flow)
  if (length(flow_items) == 0) {
    return(empty_normalised_embedded_data_fields())
  }

  block_lookup <- survey_flow_block_lookup(mt_d[["blocks"]])
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

  new_normalised_embedded_data_fields(fields)
}

empty_normalised_embedded_data_fields <- function() {
  new_normalised_embedded_data_fields()
}

merge_embedded_data_fields <- function(flat_fields, flow_fields) {
  fields <- flat_fields
  for (field_name in intersect(names(fields), names(flow_fields))) {
    fields[[field_name]] <- utils::modifyList(
      fields[[field_name]],
      flow_fields[[field_name]]
    )
  }

  new_normalised_embedded_data_fields(fields)
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

  new_normalised_embedded_data_fields(fields)
}

survey_flow_items <- function(flow) {
  if (is.null(flow) || length(flow) == 0 || !is.list(flow)) {
    return(list())
  }

  item_type <- survey_flow_item_type(flow)
  if (!is.na(item_type)) {
    child_items <- survey_flow_items(flow[["Flow"]])
    if (item_type %in% c("Block", "Standard", "EmbeddedData")) {
      return(c(list(flow), child_items))
    }

    return(child_items)
  }

  items <- flow[["Flow"]] %||% flow
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
  if (!(survey_flow_item_type(item) %in% c("Block", "Standard"))) {
    return(NA_character_)
  }

  scalar_character(item$ID)
}

survey_flow_item_type <- function(item) {
  if (is.null(item) || !is.list(item)) {
    return(NA_character_)
  }

  scalar_character(item$Type)
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
  embedded_data <- item$EmbeddedData
  if (is.null(embedded_data)) {
    return(character())
  }

  valid_embedded_data_field_names(map_chr(
    embedded_data,
    embedded_data_flow_field_name
  ))
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

  new_normalised_embedded_data_field(
    field_name = field_name,
    previous_block = previous_block,
    next_block = next_block
  )
}

valid_embedded_data_field_names <- function(field_names) {
  field_names[!is.na(field_names) & nzchar(field_names)]
}

embedded_data_field_names <- function(embedded_data) {
  if (is.null(embedded_data) || length(embedded_data) == 0) {
    return(character())
  }

  if (!is.list(embedded_data)) {
    return(character())
  }

  field_names <- map_chr(embedded_data, embedded_data_field_name)
  unname(valid_embedded_data_field_names(field_names))
}

embedded_data_field_name <- function(field) {
  if (is.null(field) || !is.list(field)) {
    return(NA_character_)
  }

  scalar_character(field$name)
}

embedded_data_flow_field_name <- function(field) {
  if (is.null(field) || !is.list(field)) {
    return(NA_character_)
  }

  scalar_character(field$Field)
}
