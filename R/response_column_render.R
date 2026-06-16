#' Render Qualtrics Response Column ID rows
#'
#' The renderer accepts one normalised question fact and returns row-aligned
#' response-column facts. Loop and Merge expansion stays upstream; callers may
#' pass an already-prefixed response-column `qid`, but this function does not
#' choose loop options or substitute loop text.
#'
#' @keywords internal
#' @noRd
render_response_columns <- function(question, qid = NULL) {
  if (is.null(qid)) {
    qid <- question$qid
  }
  if (is.null(qid) || length(qid) == 0 || is.na(qid[[1]])) {
    stop("`qid` is required to render response columns.", call. = FALSE)
  }

  shape <- response_column_shape(question)
  question_type <- question_fact_question_type(question)
  type <- question_type$type
  selector <- question_type$selector
  sub_selector <- question_type$sub_selector

  response_column_id <- qid_recode(qid,
    col_len = shape$col_len,
    col_type = shape$col_type,
    item = shape$item,
    level = shape$level,
    label = shape$label,
    choice_len = shape$level_len,
    type = type,
    selector = selector,
    sub_selector = sub_selector,
    is_qid = TRUE
  )
  response_column_id <- response_column_row_vector(response_column_id)
  row_count <- length(response_column_id)

  tibble(
    response_column_id = response_column_id,
    question = response_column_row_vector(shape$question, row_count),
    item = response_column_row_vector(
      response_column_item(
        shape$item,
        shape$level_len,
        type,
        shape$col_len
      ),
      row_count
    ),
    level = response_column_row_vector(
      response_column_level(
        shape$level,
        shape$item,
        response_column_id,
        type,
        shape$col_len
      ),
      row_count
    ),
    label = response_column_row_vector(
      response_column_label(
        shape$label,
        shape$item,
        response_column_id,
        type,
        shape$col_len
      ),
      row_count
    )
  )
}

#' Insert text-entry IDs after text-capable choices or items
#' @keywords internal
#' @noRd
add_text <- function(x, has_text, label = FALSE) {
  x <- unlist(x)
  if (!is.null(x)) {
    for (i in seq_along(has_text)) {
      pos <- has_text[i] + (i - 1)
      text <- names(x)[pos]
      text_nm <- x[pos]
      x <- append(x,
        paste(text_nm, sep = "_", "TEXT"),
        after = pos
      )

      names(x)[pos + 1] <- paste(text, sep = "_", "TEXT")
    }
    return(list(x))
  }
}

#' Repeat QIDs to align with rendered item and choice rows
#' @keywords internal
#' @noRd
rep_qid <- function(qid, item, choice_len) {
  if (is.null(item)) {
    return(rep(qid, times = choice_len))
  }
  map2(qid, names(item), function(id, nam) {
    if (!grepl("TEXT", nam)) {
      return(rep(id, each = choice_len))
    } else {
      return(id)
    }
  }) %>%
    unlist()
}

#' Repeat item facts to align with rendered choice rows
#' @keywords internal
#' @noRd
rep_item <- function(x, item, choice_len) {
  map(choice_len, function(c) {
    map2(item, x, function(itm, x) {
      if (!grepl("TEXT", itm)) {
        return(rep(x, each = c))
      } else {
        return(x)
      }
    }) %>%
      unlist()
  })
}

#' Repeat level facts to align with rendered item rows
#' @keywords internal
#' @noRd
rep_level <- function(level, item) {
  if (is.null(item)) {
    return(unlist(level))
  }

  map(level, function(l) {
    imap(item, function(itm, nam) {
      if (!grepl("TEXT", nam)) {
        return(l)
      } else {
        return("TEXT")
      }
    }) %>%
      unlist(recursive = FALSE)
  })
}

#' Flatten a rendered fact and align it to response-column rows
#' @keywords internal
#' @noRd
response_column_row_vector <- function(x, row_count = length(x)) {
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

#' Build row facts used by response-column rendering
#' @keywords internal
#' @noRd
response_column_shape <- function(question) {
  question <- remove_empty_choice_labels(question)

  type <- question_fact_question_type(question)$type
  question_text <- question_fact_question_text(question)
  response_choices <- question_fact_response_choices(question)
  response_items <- question_fact_response_items(question)

  level_len <- length(response_choices) %>% ifelse(. > 0, ., 1)

  level <- map(response_choices, "level") %>%
    unlist_nm() %>%
    list()

  label <- map(response_choices, "label") %>%
    unlist_nm() %>%
    list()

  has_text <- which(map_lgl(response_choices, "text_entry"))
  if (length(has_text) > 0) {
    level <- add_text(level, has_text)
    label <- add_text(label, has_text)
  }

  item <- unlist(map(response_items, "item_text"))
  has_text_sub <- which(map_lgl(
    response_items,
    "text_entry"
  ))
  if (length(has_text_sub) > 0) {
    item <- unlist(add_text(item, has_text_sub))
  }

  col_len <- 0
  col_type <- character()

  if (type == "SBS") {
    sbs_shape <- response_column_sbs_shape(
      question,
      question_text,
      item,
      has_text_sub
    )
    question_text <- sbs_shape$question
    level_len <- sbs_shape$level_len
    level <- sbs_shape$level
    label <- sbs_shape$label
    item <- sbs_shape$item
    col_len <- sbs_shape$col_len
    col_type <- sbs_shape$col_type
  }

  list(
    question = question_text,
    item = item,
    level = level,
    label = label,
    level_len = level_len,
    col_len = col_len,
    col_type = col_type
  )
}

#' Remove empty Qualtrics choice labels before rendering rows
#' @keywords internal
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

#' Build SBS-specific row shape
#' @keywords internal
#' @noRd
response_column_sbs_shape <- function(question,
                                      question_text,
                                      item,
                                      has_text_sub) {
  column_facts <- question_fact_column_facts(question)
  response_items <- question_fact_response_items(question)

  level_len <- map(column_facts, "response_choices") %>% map_dbl(length)
  col_len <- length(column_facts)
  col_type <- map_chr(column_facts, ~ .x$question_type$selector)
  attr(col_type, "sub_selector") <-
    map_chr(column_facts, ~ scalar_character(.x$question_type$sub_selector))

  item <- unlist(map(response_items, "item_label"))
  item <- unlist(add_text(item, has_text_sub))

  if (col_len != 0) {
    top_question <- question_fact_question_text(question)
    question_text <- map(column_facts, "question_text") %>%
      map2(length(item), rep) %>%
      map2(level_len, ~ rep_item(.x, item, .y) %>% unlist) %>%
      unlist() %>%
      paste(top_question, ., sep = " ")

    level <- map(column_facts, "response_choices") %>%
      map(~ map_chr(.x, "level")) %>%
      map2(col_type, function(level, type) {
        if (type == "TE") {
          level <- paste(level, "TEXT", sep = "_")
        }
        level
      })

    label <- map(column_facts, "response_choices") %>%
      map(~ map_chr(.x, "label"))
  } else if (length(item) > 0) {
    level_len <- rep(1, length(item))
    level <- as.list(rep(NA_character_, length(item)))
    label <- as.list(rep(NA_character_, length(item)))
  } else {
    level <- list(NA_character_)
    label <- list(NA_character_)
  }

  list(
    question = question_text,
    item = item,
    level = level,
    label = label,
    level_len = level_len,
    col_len = col_len,
    col_type = col_type
  )
}

#' Render row-aligned item facts
#' @keywords internal
#' @noRd
response_column_item <- function(item,
                                 level_len,
                                 type,
                                 col_len) {
  if (type == "SBS" && col_len == 0 && length(item) > 0) {
    return(item)
  }

  rep_item(item, item, level_len) %>% null_na()
}

#' Render row-aligned level facts
#' @keywords internal
#' @noRd
response_column_level <- function(level,
                                  item,
                                  response_column_id,
                                  type,
                                  col_len) {
  if (type == "SBS" && col_len == 0 && length(item) > 0) {
    return(rep(NA_character_, length(response_column_id)))
  }

  rep_level(level, item) %>% null_na()
}

#' Render row-aligned label facts
#' @keywords internal
#' @noRd
response_column_label <- function(label,
                                  item,
                                  response_column_id,
                                  type,
                                  col_len) {
  if (type == "SBS" && col_len == 0 && length(item) > 0) {
    return(rep(NA_character_, length(response_column_id)))
  }

  rep_level(label, item) %>% null_na()
}

questiontext_qid <- function(qid,
                             col_len,
                             item,
                             level,
                             label,
                             choice_len,
                             col_type) {
  # For texts without choices
  # Return the original QID
  qid
}

add_text_mc <- function(new_qid, level) {
  # For computed QIDs for multiple choice questions allowing for only one
  # choice with text options, add Qualtrics internal index to the end of
  # the QID for text options
  text_pos <- grep("TEXT", level)
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

sub_text_mc <- function(new_qid, qid, level) {
  text_pos <- grep("TEXT", level)
  if (!is.null(text_pos)) {
    new_qid[text_pos] <- paste(qid, names(level), sep = "_")[text_pos]
  }
  new_qid
}

suf_level_qid <- function(qid,
                          col_len,
                          item,
                          level,
                          label,
                          choice_len,
                          col_type) {
  # Add recode values to the end of the QIDs and then add Qualtrics internal
  # index to the end of QIDs with text options belonging to multiple choice
  # questions allowing for only one choice
  add_text_mc(paste(qid, mc_choice_ids(level), sep = "_"), level)
}

suf_level_qid_macol <- function(qid,
                          col_len,
                          item,
                          level,
                          label,
                          choice_len,
                          col_type) {
  paste(qid, mc_choice_ids(level), sep = "_")
}

suf_level_qid_mavr <- function(qid,
                          col_len,
                          item,
                          level,
                          label,
                          choice_len,
                          col_type) {
  paste(qid, mc_choice_ids(level), sep = "_")
}

suf_nmlabel_qid <- function(qid,
                            col_len,
                            item,
                            level,
                            label,
                            choice_len,
                            col_type) {
  # Add recode values to the end of the QID
  paste(qid, names(level), sep = "_")
}

suf_text_qid <- function(qid,
                         col_len,
                         item,
                         level,
                         label,
                         choice_len,
                         col_type) {
  paste(qid, "TEXT", sep = "_")
}

rep_level_qid <- function(qid,
                          col_len,
                          item,
                          level,
                          label,
                          choice_len,
                          col_type) {
  add_text_mc(rep(qid, length(level)), level)
}

suf_item_suf_level_qid <- function(qid,
                                   col_len,
                                   item,
                                   level,
                                   label,
                                   choice_len,
                                   col_type) {
  level <- mc_choice_ids(level)
  paste_narm(qid, names(item), sep = "_") %>%
    map(paste, level, sep = "_") %>%
    unlist()
}

suf_level_suf_item_qid <- function(qid,
                                   col_len,
                                   item,
                                   level,
                                   label,
                                   choice_len,
                                   col_type) {
  level <- mc_choice_ids(level)
  paste_narm(qid, level, sep = "_") %>%
    map(paste, names(item), sep = "_") %>%
    unlist()
}

suf_item_rep_level_qid <- function(qid,
                                   col_len,
                                   item,
                                   level,
                                   label,
                                   choice_len,
                                   col_type) {
  paste_narm(qid, names(item), sep = "_") %>%
    rep_qid(item, choice_len)
}

item_or_level_qid <- function(qid,
                              col_len,
                              item,
                              level,
                              label,
                              choice_len,
                              col_type) {
  if (is.null(item)) {
    suf_level_qid(
      qid,
      col_len,
      item,
      level,
      label,
      choice_len,
      col_type
    )
  } else {
    suf_item_rep_level_qid(
      qid,
      col_len,
      item,
      level,
      label,
      choice_len,
      col_type
    )
  }
}

text <- function(qid,
                 col_len,
                 item,
                 level,
                 label,
                 choice_len) {
  paste(qid, "TEXT", sep = "_")
}

sbs_qid <- function(qid,
                    col_len,
                    item,
                    level,
                    label,
                    choice_len,
                    col_type) {
  if (length(col_type) == 0) {
    # Carried forward question
    if (!is.null(item) && length(item) > 0) {
      return(paste_narm(qid, names(item), sep = "_"))
    }
    return(qid)
  }

  col_sub_selector <- attr(col_type, "sub_selector", exact = TRUE)
  if (is.null(col_sub_selector)) {
    col_sub_selector <- rep(NA_character_, length(col_type))
  }

  seq_len(col_len) %>%
    map(function(col_index) {
      column_id <- names(level)[[col_index]]
      if (is.null(column_id) || is.na(column_id) || column_id == "") {
        column_id <- col_index
      }
      row_names <- names(item)
      row_ids <- paste(paste(qid, column_id, sep = "#"), row_names, sep = "_")
      column_level <- level[[col_index]]
      choice_ids <- mc_choice_ids(column_level)
      choice_ids <- str_replace(choice_ids, "_TEXT$", "")
      if (col_type[[col_index]] == "TE") {
        choice_ids <- seq_along(choice_ids)
      }
      if (is.null(choice_ids) || length(choice_ids) == 0) {
        choice_ids <- seq_len(choice_len[[col_index]])
      }

      map(seq_along(row_ids), function(row_index) {
        if (col_type[[col_index]] == "TE") {
          if (grepl("TEXT$", row_names[[row_index]])) {
            return(row_ids[[row_index]])
          }

          base_row_id <- str_replace(row_ids[[row_index]], "_TEXT$", "")
          return(paste(base_row_id, choice_ids, sep = "_"))
        }

        if (grepl("TEXT$", row_names[[row_index]])) {
          return(row_ids[[row_index]])
        }

        if (!is.na(col_sub_selector[[col_index]]) &&
          col_sub_selector[[col_index]] == "MultipleAnswer") {
          return(paste(row_ids[[row_index]], choice_ids, sep = "_"))
        }

        rep(row_ids[[row_index]], each = choice_len[[col_index]])
      }) %>%
        unlist()
    }) %>%
    unlist()
}

timing_qid <- function(qid,
                       col_len,
                       item,
                       level,
                       label,
                       choice_len,
                       col_type) {
  paste0(qid, c(
    "_FIRST_CLICK", "_LAST_CLICK", "_PAGE_SUBMIT",
    "_CLICK_COUNT"
  ))
}

file_upload_qid <- function(qid,
                            col_len,
                            item,
                            level,
                            label,
                            choice_len,
                            col_type) {
  paste0(qid, c(
    "_FILE_ID", "_FILE_NAME", "_FILE_SIZE", "_FILE_TYPE"
  ))
}

not_applicable_qid <- function(qid,
                               col_len,
                               item,
                               level,
                               label,
                               choice_len,
                               col_type) {
  warn_msg <- paste0(
    qid, " is an unsupported type of question."
  )
  warning(warn_msg)
  qid
}

qid_recode <- function(qid,
                       col_len,
                       item,
                       level,
                       label,
                       choice_len,
                       col_type,
                       type,
                       selector,
                       sub_selector,
                       is_qid) {
  recode_list <- list(
    MC =
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
      ),
    Matrix =
      list(
        Likert = list(
          MultipleAnswer = suf_item_suf_level_qid,
          DL = suf_item_rep_level_qid,
          SingleAnswer = item_or_level_qid,
          DND = item_or_level_qid,
          SACV = item_or_level_qid,
          SACH = item_or_level_qid,
          SACCOL = item_or_level_qid
        ),
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
      ),
    Slider = list(
      HSLIDER = suf_level_qid,
      HBAR = suf_level_qid,
      STAR = suf_level_qid
    ),
    CS = list(
      HR = list(TX = suf_nmlabel_qid),
      VRTL = list(TX = item_or_level_qid),
      HBAR = item_or_level_qid,
      HSLIDER = item_or_level_qid
    ),
    TE = list(
      FORM = suf_nmlabel_qid,
      SL = suf_text_qid,
      ML = suf_text_qid,
      ESTB = suf_text_qid
    ),
    SBS = list(SBSMatrix = sbs_qid),
    Timing = list(PageTimer = timing_qid),
    SS = list(TA = rep_level_qid),
    FileUpload = list(FileUpload = file_upload_qid),
    PGR = list(DragAndDrop = list(NoColumns = not_applicable_qid)),
    DD = list(DL = suf_item_rep_level_qid),
    Draw = list(Signature = file_upload_qid),
    HL = list(Text = suf_level_suf_item_qid),
    Meta = list(Browser = not_applicable_qid),
    DB = list(
      TB = questiontext_qid,
      PTB = questiontext_qid,
      FLB = questiontext_qid,
      GRB = list(
        WTXB = questiontext_qid,
        WOTXB = questiontext_qid
      )
    )
  )

  if (type != "SBS") {
    level <- level[[1]]
    label <- label[[1]]
  }

  if (!is.null(selector)) {
    if (!is.null(sub_selector)) {
      recode_func <- recode_list[[type]][[selector]][[sub_selector]]
    } else {
      recode_func <- recode_list[[type]][[selector]]
    }
  } else {
    recode_func <- recode_list[[type]]
  }

  if (is.null(recode_func)) {
    recode_func <- not_applicable_qid
  }

  new_qid <- recode_func(
    qid,
    col_len,
    item,
    level,
    label,
    choice_len,
    col_type
  )

  return(new_qid)
}
