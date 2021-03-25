recode_json <- function(surveyID,
                        easyname_gen,
                        block_pattern,
                        block_sep) {
  suppressMessages(
    suppressWarnings(
      invisible(capture.output(
        survey <- # Hides the progress bar
          fetch_survey(surveyID,
            import_id = TRUE, convert = FALSE,
            label = FALSE, force_request = TRUE,
            limit = 1,
          )
      ))
    )
  )

  qids_data <- str_extract(colnames(survey), "QID[0-9]+") %>%
    discard(is.na)

  mt <- metadata(
    surveyID,
    c(
      "questions",
      "metadata",
      "blocks",
      "responsecounts",
      "flow",
      "comments"
    )
  )


  mt_d <- fetch_description(
    surveyID,
    c(
      "questions",
      "metadata",
      "blocks",
      "flow"
    )
  )

  blocks <- mt_d$block
  block_meta <- map(blocks, function(block) {
    list(
      description = block$Description,
      qid = unlist(map(block$BlockElements, "QuestionID")),
      looping_prefix = names(block$Options$LoopingOptions$Static),
      looping_qid = block$Options$LoopingOptions$QID
    )
  }) %>%
    map(function(block) {
      map(block$qid, ~ list(
        qid = .x,
        description = block$description,
        looping_prefix = block$looping_prefix,
        looping_qid = block$looping_qid
      ))
    }) %>%
    do.call(c, .) %>%
    setNames(map_chr(., ~ .x$qid))

  question_meta <- map(
    mt$questions, `[`,
    c(
      "questionName",
      "questionType", "questionText",
      "blocks", "columns",
      "choices", "subQuestions"
    )
  )

  question_meta <- question_meta[unique(qids_data)] %>%
    order_name()

  block_meta <- block_meta[names(block_meta) %in% unique(qids_data)] %>%
    order_name()

  question_meta <- map2(question_meta, block_meta, function(x, y) {
    x["block"] <- y["description"]
    x["looping_prefix"] <- y["looping_prefix"]
    x["looping_qid"] <- y["looping_qid"]
    return(x)
  })


  json <- imap(question_meta, function(qjson, qid) {
    sub_q_len <- length(qjson$subQuestions) %>% ifelse(. > 0, ., 1)
    choice_len <- length(qjson$choices) %>% ifelse(. > 0, ., 1)

    question_name <- qjson$questionName
    type <- qjson$questionType$type
    question <- qjson$questionText
    selector <- qjson$questionType$selector
    block <- qjson$block

    level <- unlist(map(qjson$choices, "recode"))
    label <- unlist(map(qjson$choices, "description"))

    has_text <- which(map_lgl(qjson$choices, ~ "textEntry" %in% names(.x)))

    if (length(has_text) > 0) {
      # Add text level and labels directly after the non-text level
      level <- add_text(level, has_text)
      label <- add_text(label, has_text)
    }

    has_text_sub <- which(map_lgl(qjson$subQuestions, ~ "textEntry" %in% names(.x)))
    item <- unlist(map(qjson$subQuestions, "choiceText"))
    sub_selector <- qjson$questionType$subSelector
    if (length(has_text_sub) > 0) {
      item <- add_text(item, has_text_sub)
      sub_q_len <- sub_q_len + 1
    }

    level_len_col <- map(qjson$columns, "choices") %>% map_dbl(length)
    col_len <- length(qjson$columns)
    col_type <- map_chr(qjson$columns, ~ .x$questionType$selector)

    # Zero length columns means it's a carried forward question
    if (type == "SBS" & col_len != 0) {
      if (qid == "QID791") {
        browser()
      }
      level_lens <- map(qjson$columns, "choices") %>% map_dbl(length)
      choice_len <- sum(level_len_col)

      question <- map(qjson$columns, "questionText") %>%
        map2(level_len_col, ~ rep(.x, each = .y)) %>%
        unlist() %>%
        rep(times = sub_q_len)

      level <- map(qjson$columns, "choices") %>%
        map(~ map_chr(.x, "recode")) %>%
        unlist()

      label <- map(qjson$columns, "choices") %>%
        map(~ map_chr(.x, "description")) %>%
        unlist()

      item <- unlist(map(qjson$subQuestions, "description"))
    }

    new_qid <- qid_recode(qid,
      col_len = col_len, col_type = col_type, item = item, level = level, label = label,
      choice_len = choice_len, level_len_col = level_len_col,
      type = type, selector = selector, sub_selector = sub_selector, is_qid = TRUE
    )

    question_name <- qid_recode(question_name,
      col_len = col_len, col_type = col_type, item = item, level = level, label = label,
      choice_len = choice_len, level_len_col = level_len_col,
      type = type, selector = selector, sub_selector = sub_selector, is_qid = FALSE
    )

    tab_qid <- data.frame(
      qid = new_qid,
      name = question_name,
      block = block,
      question,
      item = rep_item(item, choice_len) %>% null_na(),
      level = rep_level(level, item) %>% null_na(),
      label = rep_level(label, item) %>% null_na(),
      type, selector,
      sub_selector = null_na(sub_selector),
      looping = all(!is.null(qjson$looping_qid))
    ) %>%
      as_tibble()

    return(tab_qid)
  }) %>%
    discard(is.null) %>%
    rep_loop(question_meta) %>%
    bind_rows() %>%
    # Don't get rid of bracekts in labels
    remove_format(skip = "label")

  if (easyname_gen) {
    json <- easyname_gen(json, surveyID, block_pattern, block_sep)
  }

  # Remove duplicated question text in item
  json$item[json$item == json$question] <- NA

  attr(json, "survey_name") <- as.character(mt$metadata$name)
  attr(json, "surveyID") <- surveyID

  return(json)
}

add_text <- function(x, has_text, label = F) {
  if (!is.null(x)) {
    for (i in seq_along(has_text)) {
      pos <- has_text[i] + (i - 1)
      text <- names(x)[pos]
      text_nm <- x[pos]
      x <- append(x,
        paste(text_nm, sep = "_", "TEXT"),
        after = pos
      )

      # Required for sub
      names(x)[pos + 1] <- paste(text, sep = "_", "TEXT")
    }
    return(x)
  }
}

rep_qid <- function(qid, item, choice_len) {
  if (is.null(item)) {
    return(rep(qid, times = choice_len))
  }
  map2(qid, names(item), function(id, nam) {
    if (!grepl("TEXT", nam)) {
      return(rep(id, each = choice_len))
    }
    else {
      return(id)
    }
  }) %>%
    unlist()
}

rep_item <- function(item, choice_len) {
  imap(item, function(itm, nam) {
    if (!grepl("TEXT", nam)) {
      return(rep(itm, each = choice_len))
    }
    else {
      return(itm)
    }
  }) %>%
    unlist()
}

rep_level <- function(level, item) {
  if (is.null(item)) {
    return(level)
  }

  imap(item, function(itm, nam) {
    if (!grepl("TEXT", nam)) {
      return(level)
    }
    else {
      return("TEXT")
    }
  }) %>%
    unlist()
}

rep_loop <- function(x, question_meta) {
  looping_prefix <- map(question_meta, "looping_prefix")
  looping_qid <- unlist(map(question_meta, "looping_qid"))
  imap(x, function(qmeta, name) {
    if (all(qmeta$looping)) {
      prefixes <- looping_prefix[[name]]
      # This excludes all the "_TEXT" choices!!!
      labels <- x[[looping_qid[name]]][["label"]][prefixes]
      map2(prefixes, labels, function(prefix, label) {
        qmeta["qid"] <- paste(prefix, qmeta[["qid"]], sep = "_")
        qmeta["question"] <- paste(qmeta[["question"]], label, sep = "-")
        qmeta["name"] <- paste(prefix, qmeta[["name"]], sep = "_")
        return(qmeta)
      })
    }
    else {
      return(list(qmeta))
    }
  }) %>%
    unlist(recursive = FALSE)
}

qid_recode <- function(qid,
                       col_len,
                       item,
                       level,
                       label = label,
                       choice_len,
                       level_len_col,
                       col_type,
                       type,
                       selector,
                       sub_selector,
                       is_qid) {
  if (type == "MC") {
    if (selector == "MACOL" || selector == "MAVR" || selector == "MAHR") {
      new_qid <- paste(qid, level, sep = "_")
    }
    else if (selector == "SAVR" || selector == "SACOL" || selector == "DL" || selector == "SAHR") {
      new_qid <- rep(qid, length(level))
    }

    text_pos <- grep("TEXT", level)
    if (!is.null(text_pos)) {
      new_qid[text_pos] <- paste(qid, names(level), sep = "_")[text_pos]
    }
  }
  else if (type == "Matrix") {
    if (selector == "Likert") {
      if (sub_selector == "MultipleAnswer") {
        new_qid <- paste_narm(qid, names(item), sep = "_") %>%
          map(paste, level, sep = "_") %>%
          unlist()
      }
      else if (sub_selector == "DL") {
        new_qid <- paste_narm(qid, names(item), sep = "_") %>%
          rep_qid(item, choice_len)
      }
      else if (sub_selector == "SingleAnswer") {
        if (is.null(item)) {
          new_qid <- paste(qid, names(level), sep = "_")
        }
        else {
          new_qid <- paste(qid, names(item), sep = "_") %>%
            rep_qid(item, choice_len)
        }
      }
    }
    else if (selector == "TE") {
      new_qid <- paste(qid, names(item), sep = "_") %>%
        rep(each = choice_len) %>%
        paste(level, sep = "_")
    }
    else if (selector == "Profile" || selector == "Bipolar") {
      new_qid <- paste(qid, names(item), sep = "_") %>%
        rep_qid(item, choice_len)
    }
  }
  else if (type == "Slider" && selector == "HSLIDER") {
    if (is.null(item)) {
      new_qid <- paste(qid, names(level), sep = "_")
    }
    else {
      new_qid <- paste(qid, names(item), sep = "_") %>%
        rep_item(choice_len)
    }
  }
  else if (type == "Slider" && selector == "HBAR") {
    new_qid <- paste(qid, level, sep = "_")
  }
  else if (type == "Slider" && selector == "STAR") {
    new_qid <- paste(qid, level, sep = "_")
  }
  else if (type == "TE" && selector == "FORM") {
    new_qid <- paste(qid, names(label), sep = "_")
  }
  else if (type == "TE" &&
    (selector == "SL" ||
      selector == "ML" ||
      selector == "ESTB")) {
    new_qid <- paste(qid, "TEXT", sep = "_")
  }
  else if (type == "SBS") {
    new_qid <- paste(qid, sep = "#", seq(col_len)) %>%
      map2(length(item), rep) %>%
      map(paste, names(item), sep = "_") %>%
      map2(level_len_col, rep) %>%
      map2(col_type, function(qids, type) {
        if (type == "TE") {
          return(paste(qids, "1", sep = "_"))
        }
        else {
          return(qids)
        }
      }) %>%
      unlist()
  }
  else if (type == "CS") {
    if (selector == "HR") {
      if (sub_selector == "TX") {
        # Is it possible to have item?
        if (is.null(item)) {
          new_qid <- paste(qid, names(level), sep = "_")
        }
        else {
          new_qid <- paste(qid, names(item), sep = "_") %>%
            rep_item(choice_len)
        }
      }
    }
  }
  else {
    warn_msg <- glue::glue(
      "{qid} is an unsupported type of question with type = {type}, selector = {selector} and sub_selector = {sub_selector}.
    Please report this to the issue page of the qualtdict package."
    )

    warning(warn_msg, call. = FALSE)
    new_qid <- qid
  }
  # Some questions might have no choices which produces NULL
  if (is.null(new_qid)) {
    return(qid)
  }

  # This needs to be refactored so that with supported 'type' but
  # unsupported 'selector' or 'sub_selector' there is still a warning
  # msg

  return(new_qid)
}
