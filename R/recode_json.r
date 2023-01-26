recode_json <- function(surveyID,
                        easyname_gen,
                        block_pattern,
                        block_sep,
                        preprocess) {
  # Fetch metadata
  # Wrapper functions foo2 to retry when timeout
  mt <- metadata2(
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


  mt_d <- fetch_description2(
    surveyID,
    c(
      "questions",
      "metadata",
      "blocks",
      "flow"
    )
  )


  # Extract useful block metadata
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
    # Use 'c' to combine multiple lists into one list
    # Previously the lists are nested in block and then QID
    do.call(c, .) %>%
    setNames(map_chr(., ~ .x$qid))

  # Extract question metadata
  question_meta <- map(
    mt$questions, `[`,
    c(
      "questionName",
      "questionType", "questionText",
      "blocks", "columns",
      "choices", "subQuestions"
    )
  )

  content_type_meta <- mt_d$question %>%
    map("Validation") %>%
    map("Settings") %>%
    map("ContentType") %>%
    map(null_na) %>%
    map(str_remove, "Valid")

  # Order the metadatas by QID name and use only those
  # in question_meta so that the questions match
  qids <- names(question_meta)
  question_meta <- question_meta[qids] %>%
    order_name()


  block_meta <- block_meta[qids] %>%
    order_name()

  content_type_meta <- content_type_meta[qids] %>%
    order_name()

  # Combine two metadata
  question_meta <- map2(question_meta, block_meta, function(x, y) {
    x["block"] <- y["description"]
    x["looping_prefix"] <- y["looping_prefix"]
    x["looping_qid"] <- y["looping_qid"]
    return(x)
  }) %>%
    map2(content_type_meta, function(x, y) {
      x["content_type"] <- y
      return(x)
    })

  json <- imap(question_meta, function(qjson, qid) {
    # If no subquestion or choice, treat the number as 0
    sub_q_len <- length(qjson$subQuestions) %>% ifelse(. > 0, ., 1)

    # Clean the &nbsp; level/label fields (empty on Qualtrics)
    nbsps <- map(qjson$choices, "description") == "&nbsp;"

    # If there is only one nbsq, the question is a title
    if (length(nbsps) != 1) {
      qjson$choices <- qjson$choices[!nbsps]
    }

    choice_len <- length(qjson$choices) %>% ifelse(. > 0, ., 1)

    question_name <- qjson$questionName
    type <- qjson$questionType$type
    question <- qjson$questionText
    selector <- qjson$questionType$selector
    block <- qjson$block
    content_type <- qjson$content_type

    # The rep_level function works on lists for dealing with SBS questions
    # For consistency we convert to lists for non-SBS questions

    unlist_n <- function(list) {
      # unlist that preserves names
      names <- names(list)
      v <- unlist(map(list, null_na)) %>%
        setNames(names)
      return(v)
    }

    level <- map(qjson$choices, "recode") %>%
      unlist_n() %>%
      list()

    label <- map(qjson$choices, "description") %>%
      unlist_n() %>%
      list()


    # Recode QIDs for  text entry choices
    has_text <- which(map_lgl(qjson$choices, ~ "textEntry" %in% names(.x)))

    if (length(has_text) > 0) {
      # Add text level and labels directly after the non-text level
      level <- add_text(level, has_text)
      label <- add_text(label, has_text)
    }

    # Recode QIDs for  text entry item
    has_text_sub <- which(map_lgl(qjson$subQuestions, ~ "textEntry" %in% names(.x)))
    item <- unlist(map(qjson$subQuestions, "choiceText"))
    sub_selector <- qjson$questionType$subSelector
    if (length(has_text_sub) > 0) {
      item <- unlist(add_text(item, has_text_sub))
      sub_q_len <- sub_q_len + length(has_text_sub)
    }

    # Get number of levels in each column
    level_len_col <- map(qjson$columns, "choices") %>% map_dbl(length)
    # Calculate column length
    col_len <- length(qjson$columns)
    # Get column types
    col_type <- map_chr(qjson$columns, ~ .x$questionType$selector)

    # Zero length columns means it's a carried forward question
    if (type == "SBS" & col_len != 0) {
      level_lens <- map(qjson$columns, "choices") %>% map_dbl(length)
      choice_len <- level_len_col

      # Get overacching question
      top_question <- qjson$questionText
      # Get questions in each column
      question <- map(qjson$columns, "questionText") %>%
        # Repeat the question for the number of levels, separately for each
        # column
        map2(level_len_col, ~ rep(.x, each = .y)) %>%
        unlist() %>%
        # Repeat for the number of items
        rep(each = sub_q_len) %>%
        # Prepend the overarching question
        paste(top_question, ., sep = " ")

      level <- map(qjson$columns, "choices") %>%
        map(~ map_chr(.x, "recode"))

      label <- map(qjson$columns, "choices") %>%
        map(~ map_chr(.x, "description"))

      item <- unlist(map(qjson$subQuestions, "description"))
    }

    new_qid <- qid_recode(qid,
      col_len = col_len, col_type = col_type, 
      item = item, level = level, label = label,
      choice_len = choice_len, level_len_col = level_len_col,
      type = type, selector = selector, 
      sub_selector = sub_selector, is_qid = TRUE
    )

    question_name <- qid_recode(question_name,
      col_len = col_len, col_type = col_type, 
      item = item, level = level, label = label,
      choice_len = choice_len, level_len_col = level_len_col,
      type = type, selector = selector, 
      sub_selector = sub_selector, is_qid = FALSE
    )
    # Use a list instead so cols can be named vectors (for `rep_loop`)?
    list_qid <- list(
      qid = new_qid,
      name = null_na(question_name),
      block = block,
      question = question,
      looping_question = NA,
      item = rep_item(item, choice_len) %>% null_na(),
      level = rep_level(level, item) %>% null_na(),
      label = rep_level(label, item) %>% null_na(),
      type = type, selector = selector, content_type = content_type,
      sub_selector = null_na(sub_selector),
      # To use in rep_loop
      looping_option = NA,
      looping = all(!is.null(qjson$looping_qid))
    )

    return(list_qid)
  }) %>%
    discard(is.null) %>%
    rep_loop(question_meta) %>%
    to_dataframe() %>%
    convert_html()

  if (easyname_gen) {
    json <- easyname_gen(json, surveyID, block_pattern, block_sep, preprocess)
  }

  # Remove duplicated question text in item
  # This was useful in generating easy names
  json$item[json$item == json$question] <- NA

  # Add questions with loop and merge placeholders replaced with labels
  looping_questions <- json$looping_question
  json$question[!is.na(looping_questions)] <-
    looping_questions[!is.na(looping_questions)]

  attr(json, "survey_name") <- as.character(mt$metadata$name)
  attr(json, "surveyID") <- surveyID

  return(json)
}

add_text <- function(x, has_text, label = F) {
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

      # Required for sub
      names(x)[pos + 1] <- paste(text, sep = "_", "TEXT")
    }
    return(list(x))
  }
}

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

rep_item <- function(item, choice_len) {
  map(choice_len, function(c) {
    imap(item, function(itm, nam) {
      if (!grepl("TEXT", nam)) {
        return(rep(itm, each = c))
      } else {
        return(itm)
      }
    })
  })
}

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
    })
  })
}

rep_loop <- function(x, question_meta) {
  looping_qids_meta <- unlist(map(question_meta, "looping_qid"))
  looping_prefixes_meta <- map(question_meta, "looping_prefix")
  imap(x, function(qmeta, name) {
    if (qmeta$looping) {
      looping_qmeta <- x[[looping_qids_meta[name]]]

      # Get loop option and prefixes (names) generated
      # in `recode_json` (remove _TEXT)
      if (looping_qmeta[["type"]] == "Matrix") {
        loop_options <- map(looping_qmeta[["item"]], ~ map_chr(.x, 1))
      } else {
        loop_options <- map(looping_qmeta[["label"]], ~ map_chr(.x, 1))
      }

      loop_options <- loop_options %>%
        unlist() %>%
        discard(grepl("_TEXT", names(.)))

      imap(loop_options, function(option, prefix) {
        qmeta[["qid"]] <- paste(prefix, qmeta[["qid"]], sep = "_")
        # What about second loop (field 2))?
        qmeta[["looping_question"]] <-
          gsub("\\$\\{lm://Field/1\\}", option, qmeta[["question"]])
        qmeta[["question"]] <-
          gsub("\\$\\{lm://Field/1\\}", "{}", qmeta[["question"]])
        qmeta[["name"]] <- paste(prefix, qmeta[["name"]], sep = ".")
        # To use in easyname_gen
        qmeta[["looping_option"]] <- option
        return(qmeta)
      })
    } else {
      return(list(qmeta))
    }
  }) %>%
    unlist(recursive = FALSE)
}

to_dataframe <- function(json) {
  map_df(json, function(qmeta) {
    map_df(qmeta, unlist) 
  })
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
  if (type != "SBS") {
    level <- level[[1]]
    label <- label[[1]]
  }

  if (type == "MC") {
    if (selector == "MACOL" || selector == "MAVR" || selector == "MAHR") {
      new_qid <- paste(qid, level, sep = "_")
    } else if (selector == "SAVR" || selector == "SACOL" || selector == "DL" || selector == "SAHR") {
      new_qid <- rep(qid, length(level))
    }

    text_pos <- grep("TEXT", level)
    if (!is.null(text_pos)) {
      new_qid[text_pos] <- paste(qid, names(level), sep = "_")[text_pos]
    }
  } else if (type == "Matrix") {
    if (selector == "Likert") {
      if (sub_selector == "MultipleAnswer") {
        new_qid <- paste_narm(qid, names(item), sep = "_") %>%
          map(paste, level, sep = "_") %>%
          unlist()
      } else if (sub_selector == "DL") {
        new_qid <- paste_narm(qid, names(item), sep = "_") %>%
          rep_qid(item, choice_len)
      } else if (sub_selector == "SingleAnswer") {
        if (is.null(item)) {
          new_qid <- paste(qid, names(level), sep = "_")
        } else {
          new_qid <- paste(qid, names(item), sep = "_") %>%
            rep_qid(item, choice_len)
        }
      }
    } else if (selector == "TE") {
      new_qid <- paste(qid, names(item), sep = "_") %>%
        rep(each = choice_len) %>%
        paste(level, sep = "_")
    } else if (selector == "Profile" || selector == "Bipolar") {
      new_qid <- paste(qid, names(item), sep = "_") %>%
        rep_qid(item, choice_len)
    }
  } else if (type == "Slider" && selector == "HSLIDER") {
    if (is.null(item)) {
      new_qid <- paste(qid, names(level), sep = "_")
    } else {
      new_qid <- paste(qid, names(item), sep = "_") %>%
        rep_item(choice_len)
    }
  } else if (type == "Slider" && selector == "HBAR") {
    new_qid <- paste(qid, level, sep = "_")
  } else if (type == "Slider" && selector == "STAR") {
    new_qid <- paste(qid, level, sep = "_")
  } else if (type == "TE" && selector == "FORM") {
    new_qid <- paste(qid, names(label), sep = "_")
  } else if (type == "TE" &&
    (selector == "SL" ||
      selector == "ML" ||
      selector == "ESTB")) {
    new_qid <- paste(qid, "TEXT", sep = "_")
  } else if (type == "SBS") {
    new_qid <- paste(qid, sep = "#", seq(col_len)) %>%
      map2(length(item), rep) %>%
      map(paste, names(item), sep = "_") %>%
      map2(level_len_col, ~ rep(.x, each = .y)) %>%
      map2(col_type, function(qids, type) {
        if (type == "TE") {
          return(paste(qids, "1", sep = "_"))
        } else {
          return(qids)
        }
      }) %>%
      unlist()
  } else if (type == "CS") {
    if (selector == "HR") {
      if (sub_selector == "TX") {
        # Is it possible to have item?
        if (is.null(item)) {
          new_qid <- paste(qid, names(level), sep = "_")
        } else {
          new_qid <- paste(qid, names(item), sep = "_") %>%
            rep_item(choice_len)
        }
      }
    }
  } else {
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
