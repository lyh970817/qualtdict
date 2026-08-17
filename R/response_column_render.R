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

  dplyr::bind_rows(
    response_column_rows(context, response_column_id),
    display_order_response_column_rows(context)
  )
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

#' Render display-order helper Response Column ID rows
#'
#' Qualtrics exports one display-order column per CHOICE, named
#' \code{QID<n>_DO_<choice RecodeValue>}. Every column of the family shares one
#' \code{ImportId} (\code{QID<n>_DO}) in the exportColumnMap and carries the
#' sub-description \code{"Display Order - <choice text>"}, so the RecodeValue
#' names the COLUMN and the cell holds the 1-based POSITION at which that
#' choice was displayed -- blank when the choice was not displayed at all.
#'
#' The choice RecodeValue is therefore not this column's Level universe; the
#' displayed positions are. Those positions are not declared either. Qualtrics
#' supplies no labels for them, and for a carry-forward question the displayed
#' subset varies by respondent, so any static position set would be a superset
#' rather than the universe. An undeclared Level universe reads as
#' "unlabelled", which is honest; declaring the RecodeValue asserts a meaning
#' the cell does not carry.
#'
#' Choice identity stays in \code{label}, which Semantic Name generation reads
#' for multiple-answer selectors, and is additionally carried in \code{item},
#' which is what keeps the per-choice rows distinguishable once the Level is
#' gone.
#' @noRd
display_order_response_column_rows <- function(context) {
  if (!question_renders_display_order(context$question_fact)) {
    return(empty_response_columns())
  }

  choice_order <- question_fact_choice_order(context$question_fact)
  choice_levels <- display_order_choice_levels(
    context$question_fact,
    choice_order
  )
  choice_labels <- display_order_choice_labels(
    context$question_fact,
    choice_order
  )

  tibble(
    response_column_id = paste(
      context$base_response_column_id,
      "DO",
      choice_levels,
      sep = "_"
    ),
    question = question_fact_question_text(context$question_fact),
    item = display_order_items(choice_labels),
    level = NA_character_,
    label = choice_labels
  )
}

#' Build the item text that names the choice a display-order column reports on
#' @noRd
display_order_items <- function(choice_labels) {
  paste("Display order", choice_labels, sep = " - ")
}

#' Return whether a question exports display-order helpers
#' @noRd
question_renders_display_order <- function(question_fact) {
  question_type <- question_fact_question_type(question_fact)
  choice_order <- question_fact_choice_order(question_fact)

  question_type_is_mavr_text(question_type) &&
    length(choice_order) > 0 &&
    question_has_randomization(question_fact)
}

#' Return whether question type facts describe MAVR text
#' @noRd
question_type_is_mavr_text <- function(question_type) {
  identical(question_type$type, "MC") &&
    identical(question_type$selector, "MAVR") &&
    identical(question_type$sub_selector, "TX")
}

#' Return whether a question fact has randomization metadata
#' @noRd
question_has_randomization <- function(question_fact) {
  randomization <- question_fact_randomization(question_fact)
  !is.null(randomization) && length(randomization) > 0
}

#' Resolve display-order helper levels from ordered choices
#' @noRd
display_order_choice_levels <- function(question_fact, choice_order) {
  response_choices <- question_fact_response_choices(question_fact)
  levels <- vapply(
    choice_order,
    function(choice_id) {
      choice <- response_choices[[choice_id]]
      if (is.null(choice)) {
        return(choice_id)
      }
      scalar_character(choice$level %||% choice_id)
    },
    character(1)
  )

  unname(levels)
}

#' Resolve display-order helper labels from ordered choices
#' @noRd
display_order_choice_labels <- function(question_fact, choice_order) {
  response_choices <- question_fact_response_choices(question_fact)
  labels <- vapply(
    choice_order,
    function(choice_id) {
      choice <- response_choices[[choice_id]]
      if (is.null(choice)) {
        return(choice_id)
      }
      scalar_character(choice$label %||% choice_id)
    },
    character(1)
  )

  unname(labels)
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
  }

  question
}

#' Remove non-exported choices only when choices render independent columns
#' @noRd
remove_non_exported_choice_columns <- function(question) {
  if (!question_choices_render_independent_columns(question)) {
    return(question)
  }
  if (question_keeps_non_analysed_choice_columns(question)) {
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
  question
}

#' Return whether non-analysed choices still export response columns
#' @noRd
question_keeps_non_analysed_choice_columns <- function(question) {
  question_type <- question_fact_question_type(question)
  randomization <- question_fact_randomization(question)
  carry_forward <- question_fact_carry_forward(question)

  question_type_is_mavr_text(question_type) &&
    ((!is.null(randomization) && length(randomization) > 0) ||
      (!is.null(carry_forward) && length(carry_forward) > 0))
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
  if (identical(type, "TE")) {
    # Text-entry FORM questions emit one export column per choice, and
    # Qualtrics omits the column for any choice whose `analyze` flag is FALSE.
    return(identical(selector, "FORM"))
  }

  FALSE
}

#' Return whether each choice column stores a selection tick marker
#'
#' Multiple-answer questions export one column per choice. The Qualtrics
#' response schema types those questions as `array` (a selected set) rather
#' than `number` (a recode), and the flat export flattens the set to one column
#' per member named `<schema entry>_<choice RecodeValue>`. The RecodeValue
#' therefore identifies the *column*; the cell holds a membership indicator.
#' Natively that indicator is `1` when the box is ticked and blank otherwise,
#' with `unanswer_recode_multi` / `unanswer_recode` filling the unticked and
#' unseen cases at export time.
#' @noRd
question_type_renders_choice_tick_columns <- function(question_type) {
  type <- question_type$type
  selector <- question_type$selector
  sub_selector <- question_type$sub_selector

  if (identical(type, "MC")) {
    return(
      !is.null(selector) &&
        length(selector) == 1 &&
        selector %in% c("MACOL", "MAVR", "MAHR", "MSB")
    )
  }
  if (identical(type, "Matrix")) {
    return(identical(sub_selector, "MultipleAnswer"))
  }

  FALSE
}

#' Return whether a Rendering context renders per-choice tick columns
#' @noRd
context_renders_choice_tick_columns <- function(context) {
  question_type_renders_choice_tick_columns(
    context[c("type", "selector", "sub_selector")]
  )
}

#' Return the Level a ticked per-choice column stores
#' @noRd
choice_tick_level <- function() {
  "1"
}

#' Replace per-choice recodes with the tick marker they actually store
#'
#' Text-entry Levels are preserved verbatim: those columns hold free text, and
#' `survey_var_recode_context()` keys `is_text_var` off the `_TEXT` marker.
#' @noRd
apply_choice_tick_levels <- function(level) {
  level <- unlist(level, use.names = TRUE)
  if (length(level) == 0) {
    return(character())
  }

  keep <- is.na(level) | grepl("TEXT", level, fixed = TRUE)
  level[!keep] <- choice_tick_level()
  level
}

#' Return whether a context renders exactly one column per item
#'
#' Carried-forward side-by-side questions (SBS with no columns) and
#' multi-statement sliders both export one column per statement item with no
#' choice-level or choice-label facts.
#' @noRd
context_renders_one_column_per_item <- function(context) {
  facts <- context$render_facts
  if (length(facts$item) == 0) {
    return(FALSE)
  }

  is_carried_forward_sbs <- context$type == "SBS" && facts$col_len == 0
  is_multi_statement_slider <- context$type == "Slider"

  is_carried_forward_sbs || is_multi_statement_slider
}

#' Render row-aligned item facts
#' @noRd
render_response_column_items <- function(context) {
  facts <- context$render_facts
  if (context_renders_one_column_per_item(context)) {
    return(facts$item)
  }

  rep_item(facts$item, facts$item, facts$level_len) |> null_na()
}

#' Render row-aligned level facts
#' @noRd
render_response_column_levels <- function(context, response_column_id) {
  facts <- context$render_facts
  if (context_renders_one_column_per_item(context)) {
    return(rep(NA_character_, length(response_column_id)))
  }

  level <- facts$level
  if (context$type != "SBS") {
    level <- list(level)
  }

  rendered <- rep_level(level, facts$item) |> null_na()
  if (context_renders_choice_tick_columns(context)) {
    rendered <- apply_choice_tick_levels(rendered)
  }

  rendered
}

#' Render row-aligned label facts
#' @noRd
render_response_column_labels <- function(context, response_column_id) {
  facts <- context$render_facts
  if (context_renders_one_column_per_item(context)) {
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
    Captcha = list(V2 = render_no_response_column_ids),
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
  choice_ids <- names(context$render_facts$level)
  if (length(choice_ids) == 0) {
    # Every choice was suppressed (e.g. all `analyze == FALSE`), so the
    # question exports no columns.
    return(character(0))
  }
  paste(
    context$base_response_column_id,
    choice_ids,
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
