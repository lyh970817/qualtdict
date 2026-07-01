#' Build SBS-specific row shape
#' @noRd
response_column_sbs_shape <- function(question, shape, has_text_sub) {
  column_shape <- response_column_sbs_column_shape(question)
  item <- response_column_sbs_item_shape(question, has_text_sub)
  value_shape <- response_column_sbs_value_shape(
    column_shape$column_facts,
    item,
    column_shape$level_len,
    column_shape$col_type
  )
  question_text <- shape$question

  if (column_shape$col_len != 0) {
    question_text <- response_column_sbs_questions(
      question,
      column_shape$column_facts,
      item,
      column_shape$level_len
    )
  }

  new_response_column_shape(
    question = question_text,
    item = item,
    level = value_shape$level,
    label = value_shape$label,
    level_len = value_shape$level_len,
    col_len = column_shape$col_len,
    col_type = column_shape$col_type
  )
}

#' Build SBS column facts used by Response Column ID Rendering
#' @noRd
response_column_sbs_column_shape <- function(question) {
  column_facts <- question_fact_column_facts(question)
  col_type <- map_chr(column_facts, ~ .x$question_type$selector)
  attr(col_type, "sub_selector") <-
    map_chr(column_facts, ~ scalar_character(.x$question_type$sub_selector))

  list(
    column_facts = column_facts,
    level_len = map(column_facts, "response_choices") |> map_dbl(length),
    col_len = length(column_facts),
    col_type = col_type
  )
}

#' Build SBS row item facts used by Response Column ID Rendering
#' @noRd
response_column_sbs_item_shape <- function(question, has_text_sub) {
  response_items <- question_fact_response_items(question)
  item <- unlist(map(response_items, "item_label"))
  unlist(add_text(item, has_text_sub))
}

#' Build SBS row-aligned question text facts
#' @noRd
response_column_sbs_questions <- function(
  question,
  column_facts,
  item,
  level_len
) {
  top_question <- question_fact_question_text(question)
  question_text <- map(column_facts, "question_text") |>
    map2(length(item), rep) |>
    map2(level_len, function(question_text, level_len) {
      repeated_items <- rep_item(question_text, item, level_len)
      unlist(repeated_items)
    }) |>
    unlist()

  paste(top_question, question_text, sep = " ")
}

#' Build SBS level and label facts used by Response Column ID Rendering
#' @noRd
response_column_sbs_value_shape <- function(
  column_facts,
  item,
  level_len,
  col_type
) {
  if (length(column_facts) != 0) {
    return(list(
      level_len = level_len,
      level = response_column_sbs_levels(column_facts, col_type),
      label = map(column_facts, "response_choices") |> map(map_chr, "label")
    ))
  }
  if (length(item) > 0) {
    return(list(
      level_len = rep(1, length(item)),
      level = as.list(rep(NA_character_, length(item))),
      label = as.list(rep(NA_character_, length(item)))
    ))
  }

  list(
    level_len = level_len,
    level = list(NA_character_),
    label = list(NA_character_)
  )
}

#' Build SBS response levels from column choices
#' @noRd
response_column_sbs_levels <- function(column_facts, col_type) {
  map(column_facts, "response_choices") |>
    map(map_chr, "level") |>
    map2(col_type, function(level, type) {
      if (type == "TE") {
        level <- paste(level, "TEXT", sep = "_")
      }
      level
    })
}

#' Render side-by-side Response Column IDs
#' @noRd
render_sbs_response_column_ids <- function(context) {
  facts <- context$render_facts
  if (length(facts$col_type) == 0) {
    return(render_carried_forward_sbs_response_column_ids(
      context$base_response_column_id,
      facts$item
    ))
  }

  sbs_rendering_columns(
    base_response_column_id = context$base_response_column_id,
    col_len = facts$col_len,
    item = facts$item,
    level = facts$level,
    choice_len = facts$level_len,
    col_type = facts$col_type
  ) |>
    map(render_sbs_column_response_column_ids) |>
    unlist()
}

#' Render carried-forward side-by-side Response Column IDs
#' @noRd
render_carried_forward_sbs_response_column_ids <- function(
  base_response_column_id,
  item
) {
  if (!is.null(item) && length(item) > 0) {
    return(paste_narm(base_response_column_id, names(item), sep = "_"))
  }

  base_response_column_id
}

#' Build side-by-side rendering column facts
#' @noRd
sbs_rendering_columns <- function(
  base_response_column_id,
  col_len,
  item,
  level,
  choice_len,
  col_type
) {
  col_sub_selector <- sbs_column_sub_selectors(col_type)
  row_names <- names(item)

  lapply(seq_len(col_len), function(col_index) {
    column_id <- sbs_column_id(level, col_index)
    row_ids <- paste(
      paste(base_response_column_id, column_id, sep = "#"),
      row_names,
      sep = "_"
    )
    column_level <- level[[col_index]]

    list(
      column_type = col_type[[col_index]],
      column_sub_selector = col_sub_selector[[col_index]],
      row_ids = row_ids,
      row_names = row_names,
      choice_ids = sbs_column_choice_ids(
        column_level,
        col_type[[col_index]],
        choice_len[[col_index]]
      ),
      choice_count = choice_len[[col_index]]
    )
  })
}

#' Resolve side-by-side column sub-selectors
#' @noRd
sbs_column_sub_selectors <- function(col_type) {
  col_sub_selector <- attr(col_type, "sub_selector", exact = TRUE)
  if (is.null(col_sub_selector)) {
    return(rep(NA_character_, length(col_type)))
  }

  col_sub_selector
}

#' Resolve one side-by-side column ID
#' @noRd
sbs_column_id <- function(level, col_index) {
  column_id <- names(level)[[col_index]]
  if (is.null(column_id) || is.na(column_id) || column_id == "") {
    return(col_index)
  }

  column_id
}

#' Resolve side-by-side column choice IDs
#' @noRd
sbs_column_choice_ids <- function(column_level, column_type, choice_count) {
  choice_ids <- mc_choice_ids(column_level)
  choice_ids <- str_replace(choice_ids, "_TEXT$", "")

  if (column_type == "TE") {
    choice_ids <- seq_along(choice_ids)
  }
  if (is.null(choice_ids) || length(choice_ids) == 0) {
    choice_ids <- seq_len(choice_count)
  }

  choice_ids
}

#' Render Response Column IDs for one side-by-side column
#' @noRd
render_sbs_column_response_column_ids <- function(column) {
  map(seq_along(column$row_ids), function(row_index) {
    render_sbs_row_response_column_ids(sbs_row_context(column, row_index))
  }) |>
    unlist()
}

#' Build side-by-side row rendering context
#' @noRd
sbs_row_context <- function(column, row_index) {
  list(
    row_id = column$row_ids[[row_index]],
    row_name = column$row_names[[row_index]],
    column_type = column$column_type,
    column_sub_selector = column$column_sub_selector,
    choice_ids = column$choice_ids,
    choice_count = column$choice_count
  )
}

#' Render a side-by-side row Response Column ID
#' @noRd
render_sbs_row_response_column_ids <- function(row) {
  if (sbs_row_is_text_entry_column(row)) {
    return(render_sbs_text_entry_column_row(row))
  }

  if (sbs_row_is_text_entry_item(row)) {
    return(render_sbs_text_entry_item_row(row))
  }

  if (sbs_row_is_multiple_answer_column(row)) {
    return(render_sbs_multiple_answer_row(row))
  }

  render_sbs_single_answer_row(row)
}

#' Return whether a side-by-side row belongs to a text-entry column
#' @noRd
sbs_row_is_text_entry_column <- function(row) {
  row$column_type == "TE"
}

#' Return whether a side-by-side row is a text-entry item
#' @noRd
sbs_row_is_text_entry_item <- function(row) {
  grepl("TEXT$", row$row_name)
}

#' Return whether a side-by-side row belongs to a multiple-answer column
#' @noRd
sbs_row_is_multiple_answer_column <- function(row) {
  !is.na(row$column_sub_selector) &&
    row$column_sub_selector == "MultipleAnswer"
}

#' Render a side-by-side text-entry column row
#' @noRd
render_sbs_text_entry_column_row <- function(row) {
  if (sbs_row_is_text_entry_item(row)) {
    return(row$row_id)
  }

  base_row_id <- str_replace(row$row_id, "_TEXT$", "")
  paste(base_row_id, row$choice_ids, sep = "_")
}

#' Render a side-by-side text-entry item row
#' @noRd
render_sbs_text_entry_item_row <- function(row) {
  row$row_id
}

#' Render a side-by-side multiple-answer row
#' @noRd
render_sbs_multiple_answer_row <- function(row) {
  paste(row$row_id, row$choice_ids, sep = "_")
}

#' Render a side-by-side single-answer row
#' @noRd
render_sbs_single_answer_row <- function(row) {
  rep(row$row_id, each = row$choice_count)
}
