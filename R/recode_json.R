#' Download metadata in json format and recode into dictionary format
#' @keywords internal
#' @noRd
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
    # Clean the &nbsp; level/label fields (empty on Qualtrics)
    nbsps <- map(qjson$choices, "description") == "&nbsp;"
    # If there is only one nbsq, the question is a title
    # No need to clean
    if (length(nbsps) != 1) {
      qjson$choices <- qjson$choices[!nbsps]
    }

    question_name <- qjson$questionName
    type <- qjson$questionType$type
    question <- qjson$questionText
    selector <- qjson$questionType$selector
    block <- qjson$block
    content_type <- qjson$content_type

    # If no subquestion or choice, treat subquestion length as 1
    sub_q_len <- length(qjson$subQuestions) %>% ifelse(. > 0, ., 1)

    # If no levels, treat level length as 1
    level_len <- length(qjson$choices) %>% ifelse(. > 0, ., 1)

    # The rep_level function works on lists for dealing with SBS questions
    # For consistency we convert to lists for non-SBS questions
    level <- map(qjson$choices, "recode") %>%
      unlist_nm() %>%
      list()

    label <- map(qjson$choices, "description") %>%
      unlist_nm() %>%
      list()

    # Recode for text entry choices
    has_text <- which(map_lgl(qjson$choices, ~ "textEntry" %in% names(.x)))
    if (length(has_text) > 0) {
      # Add text level and labels directly after the non-text level
      level <- add_text(level, has_text)
      label <- add_text(label, has_text)
    }

    item <- unlist(map(qjson$subQuestions, "choiceText"))
    sub_selector <- qjson$questionType$subSelector

    # Recode for text entry item
    has_text_sub <- which(map_lgl(
      qjson$subQuestions, ~ "textEntry" %in% names(.x)
    ))
    if (length(has_text_sub) > 0) {
      item <- unlist(add_text(item, has_text_sub))
      sub_q_len <- sub_q_len + length(has_text_sub)
    }

    if (type == "SBS") {
      # Get number of levels in each column
      level_len <- map(qjson$columns, "choices") %>% map_dbl(length)
      # Calculate column length
      col_len <- length(qjson$columns)
      # Get column types
      col_type <- map_chr(qjson$columns, ~ .x$questionType$selector)
      if (col_len != 0) {
        # Zero length columns means it's a carried forward question

        # Get overacching question
        top_question <- qjson$questionText
        # Get questions in each column
        question <- map(qjson$columns, "questionText") %>%
          map2(length(item), rep) %>%
          map2(level_len, ~ rep_item(.x, item, .y) %>% unlist) %>%
          unlist() %>%
          # Prepend the overarching question
          paste(top_question, ., sep = " ")

        level <- map(qjson$columns, "choices") %>%
          map(~ map_chr(.x, "recode")) %>%
          map2(col_type, function(level, type) {
            if (type == "TE") {
              level <- paste(level, "TEXT", sep = "_")
            }
            level
          })

        label <- map(qjson$columns, "choices") %>%
          map(~ map_chr(.x, "description"))

        item <- unlist(map(qjson$subQuestions, "description"))
        item <- unlist(add_text(item, has_text_sub))
      }
    }

    new_qid <- qid_recode(qid,
      col_len = col_len, col_type = col_type,
      item = item, level = level, label = label,
      choice_len = level_len,
      type = type, selector = selector,
      sub_selector = sub_selector, is_qid = TRUE
    )

    question_name <- qid_recode(question_name,
      col_len = col_len, col_type = col_type,
      item = item, level = level, label = label,
      choice_len = level_len,
      type = type, selector = selector,
      sub_selector = sub_selector, is_qid = FALSE
    )

    list_qid <- list(
      qid = new_qid,
      name = null_na(question_name),
      block = block,
      question = question,
      looping_question = NA,
      item = rep_item(item, item, level_len) %>% null_na(),
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
  # This is useful in generating easy names
  json$item[json$item == json$question] <- NA

  # Add questions with loop and merge placeholders replaced with labels
  looping_questions <- json$looping_question
  json$question[!is.na(looping_questions)] <-
    looping_questions[!is.na(looping_questions)]

  attr(json, "survey_name") <- as.character(mt$metadata$name)
  attr(json, "surveyID") <- surveyID

  return(json)
}

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

rep_item <- function(x, item, choice_len) {
  map(choice_len, function(c) {
    map2(item, x, function(itm, x) {
      if (!grepl("TEXT", itm)) {
        return(rep(x, each = c))
      } else {
        return(x)
      }
    })%>%
      unlist()
  }) }

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

rep_loop <- function(x, question_meta) {
  looping_qids_meta <- unlist(map(question_meta, "looping_qid"))
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
