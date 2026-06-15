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
