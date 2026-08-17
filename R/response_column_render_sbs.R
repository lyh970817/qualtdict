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
  column_facts <- question$column_facts
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
  response_items <- question$response_items
  item <- unlist(map(response_items, "item_label"))
  item <- sbs_fill_blank_item_labels(item, response_items)
  unlist(add_text(item, has_text_sub))
}

#' Fall back to the subQuestion recode/key when a row label is blank
#'
#' Grid (side-by-side) rows can carry blank `choiceText`/`item_label`
#' (`""` or a `&nbsp;`-only string). Left blank, every row of one grid column
#' renders byte-identical `item`, so distinct rows collapse to one identity.
#' The subQuestion recode (its `x<N>` key) is the only distinguishing raw fact,
#' so use it as the row identity when the label is blank.
#' @noRd
sbs_fill_blank_item_labels <- function(item, response_items) {
  if (is.null(item) || length(item) == 0) {
    return(item)
  }
  blank <- vapply(item, is_blank_sbs_item_label, logical(1))
  if (!any(blank)) {
    return(item)
  }
  fallback <- vapply(
    response_items,
    function(response_item) {
      scalar_character(response_item$level %||% response_item$item_id)
    },
    character(1)
  )
  item[blank] <- fallback[blank]
  item
}

#' Return whether a side-by-side row label carries no distinguishing text
#' @noRd
is_blank_sbs_item_label <- function(label) {
  if (is.null(label) || length(label) == 0 || is.na(label)) {
    return(TRUE)
  }
  cleaned <- gsub("&nbsp;", "", label, fixed = TRUE)
  cleaned <- gsub("\u00a0", "", cleaned, fixed = TRUE)
  trimws(cleaned) == ""
}

#' Build SBS row-aligned question text facts
#' @noRd
response_column_sbs_questions <- function(
  question,
  column_facts,
  item,
  level_len
) {
  top_question <- question$question_text
  question_text <- sbs_column_qualified_texts(column_facts) |>
    map2(length(item), rep) |>
    map2(level_len, function(question_text, level_len) {
      repeated_items <- rep_item(question_text, item, level_len)
      unlist(repeated_items)
    }) |>
    unlist()

  paste(top_question, question_text, sep = " ")
}

#' Qualify duplicated side-by-side column question text
#'
#' Two grid columns can share byte-identical `questionText` (e.g. an
#' "Age at diagnosis" text-entry column that sits beside each of several disease
#' Likert columns). Left unqualified, such columns render identical question
#' text and collapse to one content identity even though they mean different
#' things. Qualification prepends an adjacent anchor column's text, but only
#' when the pairing is unambiguous; otherwise it falls back to an honest ordinal
#' ("... (column N)"). Ordinals hash consistently across surveys, so
#' cross-survey Exact Matches survive; a guessed pairing would not. Blank
#' columns are left untouched (their choice labels distinguish them, and the
#' engine's same-QID guard is the backstop).
#'
#' A partner is adopted only when a single grid direction is unambiguous: every
#' duplicated column must have an eligible anchor as its immediate neighbour on
#' the SAME side (all to the left, or all to the right), and only one side may
#' satisfy this. An eligible anchor is a column whose text is non-blank, is not
#' itself duplicated among the siblings, and whose selector differs (a Likert vs
#' TE sanity check). No anchor may be shared between two duplicated columns.
#' This resolves reversed-order grids (age-before-disease) correctly and, where
#' direction cannot be established -- reversed with a trailing anchor,
#' duplicated anchor columns, a shared value column, or a longer repeating
#' unit -- prefers the ordinal over a confident wrong meaning.
#' @noRd
sbs_column_qualified_texts <- function(column_facts) {
  texts <- map_chr(column_facts, ~ scalar_character(.x$question_text))
  selectors <- map_chr(
    column_facts,
    ~ scalar_character(.x$question_type$selector)
  )
  positions <- vapply(
    seq_along(column_facts),
    function(index) {
      position <- column_facts[[index]]$column_position
      if (is.null(position) || is.na(position)) index else as.integer(position)
    },
    integer(1)
  )
  duplicated_text <- sbs_duplicated_column_texts(texts)
  partner <- sbs_column_partner_indices(
    duplicated_text,
    sbs_eligible_anchor_columns(texts),
    selectors,
    positions
  )

  qualified <- vapply(
    seq_along(column_facts),
    function(index) {
      own <- texts[[index]]
      if (!duplicated_text[[index]]) {
        return(own)
      }
      if (!is.na(partner[[index]])) {
        return(paste(texts[[partner[[index]]]], own, sep = " \u2014 "))
      }
      paste0(own, " (column ", positions[[index]], ")")
    },
    character(1)
  )

  as.list(unname(qualified))
}

#' Return whether each side-by-side column text is duplicated among siblings
#'
#' Only non-blank text can trigger qualification; blank columns are excluded so
#' the heuristic stays targeted at genuinely ambiguous meaningful columns.
#' @noRd
sbs_duplicated_column_texts <- function(texts) {
  normalised <- vapply(texts, sbs_normalise_column_text, character(1))
  blank <- normalised == ""
  counts <- table(normalised[!blank])

  vapply(
    seq_along(normalised),
    function(index) {
      !blank[[index]] && counts[[normalised[[index]]]] >= 2L
    },
    logical(1)
  )
}

#' Return whether each column is an eligible anchor: non-blank AND unique text
#'
#' A duplicated column can never anchor another (a duplicated anchor cannot
#' distinguish the columns it would qualify), so eligible anchors carry text
#' that appears exactly once among the siblings.
#' @noRd
sbs_eligible_anchor_columns <- function(texts) {
  normalised <- vapply(texts, sbs_normalise_column_text, character(1))
  blank <- normalised == ""
  counts <- table(normalised[!blank])

  vapply(
    seq_along(normalised),
    function(index) {
      !blank[[index]] && counts[[normalised[[index]]]] == 1L
    },
    logical(1)
  )
}

#' Resolve an unambiguous adjacent anchor for each duplicated column
#'
#' Returns, per column, the index of its qualifying partner, or NA when the
#' pairing is ambiguous (so the caller uses the ordinal fallback). A partner is
#' resolved only when exactly one grid direction (all-left or all-right) yields
#' a distinct eligible anchor as the immediate neighbour of every duplicated
#' column.
#' @noRd
sbs_column_partner_indices <- function(
  duplicated_text,
  eligible_anchor,
  selectors,
  positions
) {
  partner <- rep(NA_integer_, length(duplicated_text))
  duplicated_columns <- which(duplicated_text)
  if (length(duplicated_columns) == 0) {
    return(partner)
  }

  left <- vapply(
    duplicated_columns,
    sbs_adjacent_anchor,
    integer(1),
    direction = -1L,
    eligible_anchor = eligible_anchor,
    selectors = selectors,
    positions = positions
  )
  right <- vapply(
    duplicated_columns,
    sbs_adjacent_anchor,
    integer(1),
    direction = 1L,
    eligible_anchor = eligible_anchor,
    selectors = selectors,
    positions = positions
  )

  left_ok <- sbs_direction_is_unambiguous(left)
  right_ok <- sbs_direction_is_unambiguous(right)

  if (left_ok && !right_ok) {
    partner[duplicated_columns] <- left
  } else if (right_ok && !left_ok) {
    partner[duplicated_columns] <- right
  }

  partner
}

#' Return the immediate neighbour on one side when it is an eligible anchor
#' @noRd
sbs_adjacent_anchor <- function(
  index,
  direction,
  eligible_anchor,
  selectors,
  positions
) {
  neighbour <- which(positions == positions[[index]] + direction)
  if (length(neighbour) != 1) {
    return(NA_integer_)
  }
  if (!eligible_anchor[[neighbour]]) {
    return(NA_integer_)
  }
  if (identical(selectors[[neighbour]], selectors[[index]])) {
    return(NA_integer_)
  }

  neighbour
}

#' Return whether a per-column partner side pairs every column distinctly
#' @noRd
sbs_direction_is_unambiguous <- function(partner) {
  !anyNA(partner) && anyDuplicated(partner) == 0L
}

#' Normalise side-by-side column text for duplicate and distinctness checks
#' @noRd
sbs_normalise_column_text <- function(text) {
  if (is.null(text) || length(text) == 0 || is.na(text)) {
    return("")
  }
  cleaned <- gsub("&nbsp;", "", text, fixed = TRUE)
  cleaned <- gsub("\u00a0", " ", cleaned, fixed = TRUE)

  trimws(cleaned)
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
