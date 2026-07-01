#' Resolve the Response Column ID QID used for rendering
#' @noRd
resolve_response_column_qid <- function(
  question_fact,
  response_column_qid = NULL
) {
  if (is.null(response_column_qid)) {
    response_column_qid <- question_fact$qid
  }
  if (
    is.null(response_column_qid) ||
      length(response_column_qid) == 0 ||
      is.na(response_column_qid[[1]])
  ) {
    stop("`qid` is required to render response columns.", call. = FALSE)
  }

  response_column_qid
}

#' Build Response Column ID Rendering context
#' @noRd
new_response_column_render_context <- function(
  question_fact,
  response_column_qid,
  shape,
  question_type
) {
  list(
    question_fact = question_fact,
    response_column_qid = response_column_qid,
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
render_response_columns <- function(question_fact, response_column_qid = NULL) {
  response_column_qid <- resolve_response_column_qid(
    question_fact,
    response_column_qid
  )

  question_type <- question_fact_question_type(question_fact)
  shape <- response_column_shape(question_fact)
  context <- new_response_column_render_context(
    question_fact = question_fact,
    response_column_qid = response_column_qid,
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

#' Repeat QIDs to align with rendered item and choice rows
#' @noRd
rep_qid <- function(qid, item, choice_len) {
  if (is.null(item)) {
    return(rep(qid, times = choice_len))
  }
  map2(qid, names(item), function(id, nam) {
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

  renderer %||% not_applicable_qid
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
    SBS = list(SBSMatrix = sbs_qid),
    Timing = list(PageTimer = timing_qid),
    SS = list(TA = rep_level_qid),
    FileUpload = list(FileUpload = file_upload_qid),
    PGR = list(DragAndDrop = list(NoColumns = not_applicable_qid)),
    DD = list(DL = suf_item_rep_level_qid),
    Draw = list(Signature = file_upload_qid),
    HL = list(Text = suf_level_suf_item_qid),
    Meta = list(Browser = not_applicable_qid),
    DB = response_column_display_renderer_table()
  )
}

#' Multiple choice Response Column ID renderers
#' @noRd
response_column_mc_renderer_table <- function() {
  list(
    MACOL = list(TX = suf_level_qid_macol),
    MAVR = list(TX = suf_level_qid_mavr),
    MAHR = list(TX = suf_level_qid),
    MSB = suf_level_qid,
    SAVR = list(TX = rep_level_qid),
    SACOL = list(TX = rep_level_qid),
    DL = rep_level_qid,
    SAHR = list(TX = rep_level_qid),
    SB = rep_level_qid,
    NPS = rep_level_qid
  )
}

#' Matrix Response Column ID renderers
#' @noRd
response_column_matrix_renderer_table <- function() {
  list(
    Likert = response_column_likert_renderer_table(),
    TE = list(
      Short = suf_item_suf_level_qid,
      Medium = suf_item_suf_level_qid,
      Long = suf_item_suf_level_qid
    ),
    Profile = list(
      SingleAnswer = suf_item_rep_level_qid,
      DL = suf_item_rep_level_qid
    ),
    Bipolar = suf_item_rep_level_qid,
    RO = suf_item_suf_level_qid,
    MaxDiff = suf_item_rep_level_qid,
    CS = list(WTB = suf_item_suf_level_qid)
  )
}

#' Likert matrix Response Column ID renderers
#' @noRd
response_column_likert_renderer_table <- function() {
  list(
    MultipleAnswer = suf_item_suf_level_qid,
    DL = suf_item_rep_level_qid,
    SingleAnswer = item_or_level_qid,
    DND = item_or_level_qid,
    SACV = item_or_level_qid,
    SACH = item_or_level_qid,
    SACCOL = item_or_level_qid
  )
}

#' Slider Response Column ID renderers
#' @noRd
response_column_slider_renderer_table <- function() {
  list(
    HSLIDER = suf_level_qid,
    HBAR = suf_level_qid,
    STAR = suf_level_qid
  )
}

#' Constant sum Response Column ID renderers
#' @noRd
response_column_cs_renderer_table <- function() {
  list(
    HR = list(TX = suf_nmlabel_qid),
    VRTL = list(TX = item_or_level_qid),
    HBAR = item_or_level_qid,
    HSLIDER = item_or_level_qid
  )
}

#' Text entry Response Column ID renderers
#' @noRd
response_column_te_renderer_table <- function() {
  list(
    FORM = suf_nmlabel_qid,
    SL = suf_text_qid,
    ML = suf_text_qid,
    ESTB = suf_text_qid
  )
}

#' Display block Response Column ID renderers
#' @noRd
response_column_display_renderer_table <- function() {
  list(
    TB = questiontext_qid,
    PTB = questiontext_qid,
    FLB = questiontext_qid,
    GRB = list(
      WTXB = questiontext_qid,
      WOTXB = questiontext_qid
    )
  )
}

#' Render no Response Column IDs for display text questions
#' @noRd
questiontext_qid <- function(context) {
  character()
}

#' Add Qualtrics text-entry IDs to multiple choice Response Column IDs
#' @noRd
add_text_mc <- function(new_qid, level) {
  # For computed QIDs for multiple choice questions allowing for only one
  # choice with text options, add Qualtrics internal index to the end of
  # the QID for text options
  text_pos <- grep("TEXT", level, fixed = TRUE)
  if (length(text_pos) > 0 && !is.null(names(level))) {
    for (pos in text_pos) {
      level_suffix <- paste0("_", level[[pos]])
      if (endsWith(new_qid[[pos]], level_suffix)) {
        prefix <- substr(
          new_qid[[pos]],
          1,
          nchar(new_qid[[pos]]) - nchar(level_suffix)
        )
      } else {
        prefix <- new_qid[[pos]]
      }
      new_qid[[pos]] <- paste(prefix, names(level)[[pos]], sep = "_")
    }
  }
  new_qid
}

#' Resolve multiple choice IDs for Response Column ID rendering
#' @noRd
mc_choice_ids <- function(level) {
  choice_ids <- names(level)
  if (is.null(choice_ids)) {
    return(level)
  }

  if (all(grepl("^x[0-9]+(_TEXT)?$", choice_ids))) {
    return(choice_ids)
  }

  response_choice_ids <- unname(level)
  text_choice_ids <- grepl("TEXT$", level)
  response_choice_ids[text_choice_ids] <- choice_ids[text_choice_ids]

  response_choice_ids
}

#' Resolve multiple choice recodes for Response Column ID rendering
#' @noRd
mc_recode_ids <- function(level) {
  choice_ids <- names(level)
  if (is.null(choice_ids)) {
    return(level)
  }

  response_choice_ids <- unname(level)
  text_choice_ids <- grepl("TEXT$", level)
  response_choice_ids[text_choice_ids] <- choice_ids[text_choice_ids]

  response_choice_ids
}

#' Render Response Column IDs with level suffixes
#' @noRd
suf_level_qid <- function(context) {
  level <- context$render_facts$level
  # Add recode values to the end of the QIDs and then add Qualtrics internal
  # index to the end of QIDs with text options belonging to multiple choice
  # questions allowing for only one choice
  add_text_mc(
    paste(context$response_column_qid, mc_recode_ids(level), sep = "_"),
    level
  )
}

#' Render MACOL Response Column IDs with level suffixes
#' @noRd
suf_level_qid_macol <- function(context) {
  level <- context$render_facts$level
  if (length(level) == 0) {
    return(context$response_column_qid)
  }

  paste(context$response_column_qid, mc_recode_ids(level), sep = "_")
}

#' Render MAVR Response Column IDs with level suffixes
#' @noRd
suf_level_qid_mavr <- function(context) {
  level <- context$render_facts$level
  if (length(level) == 0) {
    return(context$response_column_qid)
  }

  paste(context$response_column_qid, mc_recode_ids(level), sep = "_")
}

#' Render Response Column IDs with choice-level suffixes
#' @noRd
suf_choice_level_qid <- function(context) {
  level <- context$render_facts$level
  if (length(level) == 0) {
    return(context$response_column_qid)
  }

  paste(context$response_column_qid, mc_choice_ids(level), sep = "_")
}

#' Render Response Column IDs with named level suffixes
#' @noRd
suf_nmlabel_qid <- function(context) {
  # Add recode values to the end of the QID
  paste(
    context$response_column_qid,
    names(context$render_facts$level),
    sep = "_"
  )
}

#' Render text-entry Response Column IDs
#' @noRd
suf_text_qid <- function(context) {
  paste(context$response_column_qid, "TEXT", sep = "_")
}

#' Repeat Response Column IDs across levels
#' @noRd
rep_level_qid <- function(context) {
  level <- context$render_facts$level
  add_text_mc(rep(context$response_column_qid, length(level)), level)
}

#' Render Response Column IDs with item and level suffixes
#' @noRd
suf_item_suf_level_qid <- function(context) {
  facts <- context$render_facts
  level <- mc_choice_ids(facts$level)
  paste_narm(context$response_column_qid, names(facts$item), sep = "_") |>
    map(paste, level, sep = "_") |>
    unlist()
}

#' Render Response Column IDs with level and item suffixes
#' @noRd
suf_level_suf_item_qid <- function(context) {
  facts <- context$render_facts
  level <- mc_choice_ids(facts$level)
  paste_narm(context$response_column_qid, level, sep = "_") |>
    map(paste, names(facts$item), sep = "_") |>
    unlist()
}

#' Render item-suffixed Response Column IDs repeated across levels
#' @noRd
suf_item_rep_level_qid <- function(context) {
  facts <- context$render_facts
  paste_narm(context$response_column_qid, names(facts$item), sep = "_") |>
    rep_qid(facts$item, facts$level_len)
}

#' Render item-based or level-based Response Column IDs
#' @noRd
item_or_level_qid <- function(context) {
  if (is.null(context$render_facts$item)) {
    return(suf_choice_level_qid(context))
  }

  suf_item_rep_level_qid(context)
}

#' Render a bare text-entry Response Column ID
#' @noRd
text <- function(context) {
  paste(context$response_column_qid, "TEXT", sep = "_")
}

#' Render side-by-side Response Column IDs
#' @noRd
sbs_qid <- function(context) {
  facts <- context$render_facts
  if (length(facts$col_type) == 0) {
    return(render_carried_forward_sbs_qids(
      context$response_column_qid,
      facts$item
    ))
  }

  sbs_rendering_columns(
    qid = context$response_column_qid,
    col_len = facts$col_len,
    item = facts$item,
    level = facts$level,
    choice_len = facts$level_len,
    col_type = facts$col_type
  ) |>
    map(render_sbs_column_qids) |>
    unlist()
}

#' Render carried-forward side-by-side Response Column IDs
#' @noRd
render_carried_forward_sbs_qids <- function(qid, item) {
  if (!is.null(item) && length(item) > 0) {
    return(paste_narm(qid, names(item), sep = "_"))
  }

  qid
}

#' Build side-by-side rendering column facts
#' @noRd
sbs_rendering_columns <- function(
  qid,
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
    row_ids <- paste(paste(qid, column_id, sep = "#"), row_names, sep = "_")
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
render_sbs_column_qids <- function(column) {
  map(seq_along(column$row_ids), function(row_index) {
    render_sbs_row_qid(sbs_row_context(column, row_index))
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
render_sbs_row_qid <- function(row) {
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

#' Render timing question Response Column IDs
#' @noRd
timing_qid <- function(context) {
  paste0(
    context$response_column_qid,
    c(
      "_FIRST_CLICK",
      "_LAST_CLICK",
      "_PAGE_SUBMIT",
      "_CLICK_COUNT"
    )
  )
}

#' Render file-upload Response Column IDs
#' @noRd
file_upload_qid <- function(context) {
  paste0(
    context$response_column_qid,
    c(
      "_FILE_ID",
      "_FILE_NAME",
      "_FILE_SIZE",
      "_FILE_TYPE"
    )
  )
}

#' Render fallback Response Column IDs for unsupported question shapes
#' @noRd
not_applicable_qid <- function(context) {
  warn_msg <- paste0(
    context$response_column_qid,
    " uses a question type without a specific response-column renderer; ",
    "falling back to the bare QID."
  )
  warning(warn_msg)
  context$response_column_qid
}
