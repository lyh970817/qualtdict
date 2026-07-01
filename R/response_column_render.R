#' Resolve the Base Response Column ID used for rendering
#' @noRd
resolve_base_response_column_id <- function(
  question_fact,
  base_response_column_id = NULL
) {
  if (is.null(base_response_column_id)) {
    base_response_column_id <- question_fact$qid
  }
  if (
    is.null(base_response_column_id) ||
      length(base_response_column_id) == 0 ||
      is.na(base_response_column_id[[1]])
  ) {
    stop("`qid` is required to render response columns.", call. = FALSE)
  }

  base_response_column_id
}

#' Build Response Column ID Rendering context
#' @noRd
new_response_column_render_context <- function(
  question_fact,
  base_response_column_id,
  shape,
  question_type
) {
  list(
    question_fact = question_fact,
    base_response_column_id = base_response_column_id,
    shape = shape,
    render_facts = response_column_render_facts(shape, question_type$type),
    type = question_type$type,
    selector = question_type$selector,
    sub_selector = question_type$sub_selector
  )
}

#' Normalise renderer inputs while preserving SBS list-shaped facts
#' @noRd
response_column_render_facts <- function(shape, type) {
  level <- shape$level
  label <- shape$label

  if (type != "SBS") {
    level <- level[[1]]
    label <- label[[1]]
  }

  list(
    question = shape$question,
    item = shape$item,
    level = level,
    label = label,
    level_len = shape$level_len,
    col_len = shape$col_len,
    col_type = shape$col_type
  )
}

#' Render row-aligned Response Column ID facts
#' @noRd
render_response_columns <- function(
  question_fact,
  base_response_column_id = NULL
) {
  base_response_column_id <- resolve_base_response_column_id(
    question_fact,
    base_response_column_id
  )

  question_type <- question_fact_question_type(question_fact)
  shape <- response_column_shape(question_fact)
  context <- new_response_column_render_context(
    question_fact = question_fact,
    base_response_column_id = base_response_column_id,
    shape = shape,
    question_type = question_type
  )

  response_column_id <- response_column_row_vector(
    render_response_column_ids(context)
  )

  response_column_rows(context, response_column_id)
}

#' Build row-aligned Response Column ID facts
#' @noRd
response_column_rows <- function(context, response_column_id) {
  row_count <- length(response_column_id)
  shape <- context$shape

  if (row_count == 0) {
    return(empty_response_columns())
  }

  tibble(
    response_column_id = response_column_id,
    question = response_column_row_vector(shape$question, row_count),
    item = response_column_row_vector(
      render_response_column_items(context),
      row_count
    ),
    level = response_column_row_vector(
      render_response_column_levels(context, response_column_id),
      row_count
    ),
    label = response_column_row_vector(
      render_response_column_labels(context, response_column_id),
      row_count
    )
  )
}

#' Empty Response Column ID fact table
#' @noRd
empty_response_columns <- function() {
  tibble(
    response_column_id = character(),
    question = character(),
    item = character(),
    level = character(),
    label = character()
  )
}

#' Insert text-entry IDs after text-capable choices or items
#' @noRd
add_text <- function(x, has_text, label = FALSE) {
  x <- unlist(x)
  if (!is.null(x)) {
    for (i in seq_along(has_text)) {
      pos <- has_text[i] + (i - 1)
      text <- names(x)[pos]
      text_nm <- x[pos]
      x <- append(x, paste0(text_nm, "_TEXT"), after = pos)

      names(x)[pos + 1] <- paste0(text, "_TEXT")
    }
    return(list(x))
  }
}

#' Repeat Response Column IDs to align with rendered item and choice rows
#' @noRd
repeat_response_column_ids <- function(response_column_id, item, choice_len) {
  if (is.null(item)) {
    return(rep(response_column_id, times = choice_len))
  }
  map2(response_column_id, names(item), function(id, nam) {
    if (grepl("TEXT", nam, fixed = TRUE)) {
      return(id)
    }
    return(rep(id, each = choice_len))
  }) |>
    unlist()
}

#' Repeat item facts to align with rendered choice rows
#' @noRd
rep_item <- function(x, item, choice_len) {
  map(choice_len, function(c) {
    map2(item, x, function(itm, x) {
      if (grepl("TEXT", itm, fixed = TRUE)) {
        return(x)
      }
      return(rep(x, each = c))
    }) |>
      unlist()
  })
}

#' Repeat level facts to align with rendered item rows
#' @noRd
rep_level <- function(level, item) {
  if (is.null(item)) {
    return(unlist(level))
  }

  map(level, function(l) {
    imap(item, function(itm, nam) {
      if (grepl("TEXT", nam, fixed = TRUE)) {
        return("TEXT")
      }
      return(l)
    }) |>
      unlist(recursive = FALSE)
  })
}

#' Flatten a rendered fact and align it to Response Column ID rows
#' @noRd
response_column_row_vector <- function(x, row_count = length(x)) {
  if (length(x) == 0) {
    return(character())
  }

  x <- null_na(x)
  x <- unlist(x, use.names = TRUE)

  if (length(x) == 0) {
    x <- NA_character_
  }
  if (length(x) == 1 && row_count != 1) {
    x <- rep(x, row_count)
  }
  if (length(x) != row_count) {
    if (row_count == 1) {
      x <- x[[1]]
    } else {
      stop(
        "Rendered response-column facts are not row-aligned.",
        call. = FALSE
      )
    }
  }

  x
}

#' Build row facts used by Response Column ID Rendering
#' @noRd
response_column_shape <- function(question) {
  question <- remove_empty_choice_labels(question)
  question <- remove_non_exported_choice_columns(question)

  type <- question_fact_question_type(question)$type
  question_text <- question_fact_question_text(question)
  choice_shape <- response_column_choice_shape(question)
  item_shape <- response_column_item_shape(question)

  shape <- new_response_column_shape(
    question = question_text,
    item = item_shape$item,
    level = choice_shape$level,
    label = choice_shape$label,
    level_len = choice_shape$level_len
  )
  if (type == "SBS") {
    shape <- response_column_sbs_shape(
      question,
      shape,
      item_shape$has_text_sub
    )
  }

  shape
}

#' Build generic choice facts used by Response Column ID Rendering
#' @noRd
response_column_choice_shape <- function(question) {
  response_choices <- question_fact_response_choices(question)
  level_len <- ifelse(length(response_choices) > 0, length(response_choices), 1)

  level <- map(response_choices, "level") |>
    unlist_nm() |>
    list()
  label <- map(response_choices, "label") |>
    unlist_nm() |>
    list()

  has_text <- which(map_lgl(response_choices, "text_entry"))
  if (length(has_text) > 0) {
    level <- add_text(level, has_text)
    label <- add_text(label, has_text)
  }

  list(level = level, label = label, level_len = level_len)
}

#' Build generic item facts used by Response Column ID Rendering
#' @noRd
response_column_item_shape <- function(question) {
  response_items <- question_fact_response_items(question)
  item <- unlist(map(response_items, "item_text"))
  has_text_sub <- which(map_lgl(response_items, "text_entry"))

  if (length(has_text_sub) > 0) {
    item <- unlist(add_text(item, has_text_sub))
  }

  list(item = item, has_text_sub = has_text_sub)
}

#' Build Response Column ID shape from row-aligned facts
#' @noRd
new_response_column_shape <- function(
  question,
  item,
  level,
  label,
  level_len,
  col_len = 0,
  col_type = character()
) {
  list(
    question = question,
    item = item,
    level = level,
    label = label,
    level_len = level_len,
    col_len = col_len,
    col_type = col_type
  )
}

#' Remove empty Qualtrics choice labels before rendering rows
#' @noRd
remove_empty_choice_labels <- function(question) {
  response_choices <- question_fact_response_choices(question)
  nbsps <- map(response_choices, "label") == "&nbsp;"
  if (length(nbsps) != 1) {
    question$response_choices <- response_choices[!nbsps]
    question$choices <- question$response_choices
  }

  question
}

#' Remove non-exported choices only when choices render independent columns
#' @noRd
remove_non_exported_choice_columns <- function(question) {
  if (!question_choices_render_independent_columns(question)) {
    return(question)
  }

  response_choices <- question_fact_response_choices(question)
  if (length(response_choices) == 0) {
    return(question)
  }

  exported <- map_lgl(response_choices, function(choice) {
    isTRUE(choice$analyze %||% TRUE)
  })
  question$response_choices <- response_choices[exported]
  question$choices <- question$response_choices
  question
}

#' Return whether each choice produces a distinct Response Column ID
#' @noRd
question_choices_render_independent_columns <- function(question) {
  question_type <- question_fact_question_type(question)
  type <- question_type$type
  selector <- question_type$selector
  sub_selector <- question_type$sub_selector

  if (identical(type, "MC")) {
    return(selector %in% c("MACOL", "MAVR", "MAHR", "MSB"))
  }
  if (identical(type, "Matrix")) {
    return(identical(sub_selector, "MultipleAnswer"))
  }

  FALSE
}

#' Render row-aligned item facts
#' @noRd
render_response_column_items <- function(context) {
  facts <- context$render_facts
  if (context$type == "SBS" && facts$col_len == 0 && length(facts$item) > 0) {
    return(facts$item)
  }

  rep_item(facts$item, facts$item, facts$level_len) |> null_na()
}

#' Render row-aligned level facts
#' @noRd
render_response_column_levels <- function(context, response_column_id) {
  facts <- context$render_facts
  if (context$type == "SBS" && facts$col_len == 0 && length(facts$item) > 0) {
    return(rep(NA_character_, length(response_column_id)))
  }

  level <- facts$level
  if (context$type != "SBS") {
    level <- list(level)
  }

  rep_level(level, facts$item) |> null_na()
}

#' Render row-aligned label facts
#' @noRd
render_response_column_labels <- function(context, response_column_id) {
  facts <- context$render_facts
  if (context$type == "SBS" && facts$col_len == 0 && length(facts$item) > 0) {
    return(rep(NA_character_, length(response_column_id)))
  }

  label <- facts$label
  if (context$type != "SBS") {
    label <- list(label)
  }

  rep_level(label, facts$item) |> null_na()
}

#' Render Response Column IDs for one context
#' @noRd
render_response_column_ids <- function(context) {
  renderer <- response_column_renderer_for_context(context)
  renderer(context)
}

#' Resolve Response Column ID renderer for one context
#' @noRd
response_column_renderer_for_context <- function(context) {
  renderer <- response_column_renderer_table()

  if (!is.null(context$selector)) {
    if (!is.null(context$sub_selector)) {
      renderer <- renderer[[context$type]][[context$selector]][[
        context$sub_selector
      ]]
    } else {
      renderer <- renderer[[context$type]][[context$selector]]
    }
  } else {
    renderer <- renderer[[context$type]]
  }

  renderer %||% render_unsupported_response_column_ids
}

#' Response Column ID renderer dispatch table
#' @noRd
response_column_renderer_table <- function() {
  list(
    MC = response_column_mc_renderer_table(),
    Matrix = response_column_matrix_renderer_table(),
    Slider = response_column_slider_renderer_table(),
    CS = response_column_cs_renderer_table(),
    TE = response_column_te_renderer_table(),
    SBS = list(SBSMatrix = render_sbs_response_column_ids),
    Timing = list(PageTimer = render_timing_response_column_ids),
    SS = list(TA = render_response_column_id_repeated_by_level),
    FileUpload = list(FileUpload = render_file_upload_response_column_ids),
    PGR = list(
      DragAndDrop = list(NoColumns = render_unsupported_response_column_ids)
    ),
    DD = list(
      DL = render_response_column_id_with_item_suffix_repeated_by_level
    ),
    Draw = list(Signature = render_file_upload_response_column_ids),
    HL = list(Text = render_response_column_id_with_level_and_item_suffixes),
    Meta = list(Browser = render_unsupported_response_column_ids),
    DB = response_column_display_renderer_table()
  )
}

#' Text entry Response Column ID renderers
#' @noRd
response_column_te_renderer_table <- function() {
  list(
    FORM = render_response_column_id_with_named_label_suffix,
    SL = render_response_column_id_with_text_suffix,
    ML = render_response_column_id_with_text_suffix,
    ESTB = render_response_column_id_with_text_suffix
  )
}

#' Render Response Column IDs with choice-level suffixes
#' @noRd
render_response_column_id_with_choice_level_suffix <- function(context) {
  level <- context$render_facts$level
  if (length(level) == 0) {
    return(context$base_response_column_id)
  }

  paste(context$base_response_column_id, mc_choice_ids(level), sep = "_")
}

#' Render Response Column IDs with level suffixes
#' @noRd
render_response_column_id_with_level_suffix <- function(context) {
  level <- context$render_facts$level
  # Add recode values to the end of the Response Column IDs and then add the
  # Qualtrics internal index to text-entry Response Column IDs for multiple
  # choice questions allowing for only one choice.
  add_text_mc(
    paste(context$base_response_column_id, mc_recode_ids(level), sep = "_"),
    level
  )
}

#' Render Response Column IDs with named label suffixes
#' @noRd
render_response_column_id_with_named_label_suffix <- function(context) {
  # Add recode values to the end of the Base Response Column ID.
  paste(
    context$base_response_column_id,
    names(context$render_facts$level),
    sep = "_"
  )
}

#' Render text-entry Response Column IDs
#' @noRd
render_response_column_id_with_text_suffix <- function(context) {
  text(context)
}

#' Repeat Response Column IDs across levels
#' @noRd
render_response_column_id_repeated_by_level <- function(context) {
  level <- context$render_facts$level
  add_text_mc(rep(context$base_response_column_id, length(level)), level)
}

#' Render Response Column IDs with item and level suffixes
#' @noRd
render_response_column_id_with_item_and_level_suffixes <- function(context) {
  facts <- context$render_facts
  level <- mc_choice_ids(facts$level)
  paste_narm(context$base_response_column_id, names(facts$item), sep = "_") |>
    map(paste, level, sep = "_") |>
    unlist()
}

#' Render Response Column IDs with level and item suffixes
#' @noRd
render_response_column_id_with_level_and_item_suffixes <- function(context) {
  facts <- context$render_facts
  level <- mc_choice_ids(facts$level)
  paste_narm(context$base_response_column_id, level, sep = "_") |>
    map(paste, names(facts$item), sep = "_") |>
    unlist()
}

#' Render item-suffixed Response Column IDs repeated across levels
#' @noRd
render_response_column_id_with_item_suffix_repeated_by_level <- function(
  context
) {
  facts <- context$render_facts
  paste_narm(context$base_response_column_id, names(facts$item), sep = "_") |>
    repeat_response_column_ids(facts$item, facts$level_len)
}

#' Render item-based or level-based Response Column IDs
#' @noRd
render_response_column_id_with_item_or_level_suffix <- function(context) {
  if (is.null(context$render_facts$item)) {
    return(render_response_column_id_with_choice_level_suffix(context))
  }

  render_response_column_id_with_item_suffix_repeated_by_level(context)
}

#' Render a bare text-entry Response Column ID
#' @noRd
text <- function(context) {
  paste(context$base_response_column_id, "TEXT", sep = "_")
}
