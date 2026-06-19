#' Expand Loop and Merge question facts
#' @keywords internal
#' @noRd
expand_loop_question_facts <- function(questions) {
  imap(questions, function(question, name) {
    looping_qid <- scalar_character(question_fact_looping_qid(question))
    looping_static <- question_fact_looping_static(question)
    has_static_loop <- !is.null(looping_static) && length(looping_static) > 0

    if (is.na(looping_qid) && !has_static_loop) {
      question[["looping"]] <- FALSE
      return(list(question))
    }

    looping_source <- NULL
    if (!is.na(looping_qid)) {
      looping_source <- questions[[looping_qid]]
    }
    if (!is.na(looping_qid) && is.null(looping_source)) {
      question[["looping"]] <- FALSE
      return(list(question))
    }

    loop_rows <- loop_rows_for_question(
      question = question,
      looping_source = looping_source,
      looping_qid = looping_qid
    )
    if (is.null(loop_rows) || length(loop_rows) == 0) {
      return(list(question))
    }

    map(loop_rows, function(loop_row) {
      looped_question <- question
      looped_question[["looping_question"]] <-
        substitute_loop_fields(
          question[["question_text"]],
          loop_row$fields
        )
      looped_question[["question_text"]] <-
        loop_question_text_template(question[["question_text"]])
      looped_question[["looping_option"]] <- loop_row$option
      looped_question[["looping_prefix"]] <- loop_row$prefix
      looped_question[["looping_qid"]] <- looping_qid
      looped_question[["qid"]] <- question[["qid"]]
      looped_question[["response_column_qid"]] <-
        loop_response_column_id(
          paste(loop_row$prefix, question[["qid"]], sep = "_")
        )
      looped_question[["looping"]] <- TRUE
      looped_question
    })
  }) %>%
    unlist(recursive = FALSE)
}

#' Build Loop and Merge row facts for one question fact
#' @keywords internal
#' @noRd
loop_rows_for_question <- function(question, looping_source, looping_qid) {
  loop_options <- loop_options_for_question(
    question = question,
    looping_source = looping_source,
    looping_qid = looping_qid
  )

  if (is.null(loop_options) || length(loop_options) == 0) {
    return(NULL)
  }

  field_values <- loop_field_values_for_question(
    question = question,
    prefixes = names(loop_options)
  )

  imap(loop_options, function(option, prefix) {
    fields <- field_values[[prefix]] %||% character()
    fields[["1"]] <- option
    fields <- fields[!is.na(fields)]

    list(
      prefix = prefix,
      option = option,
      fields = fields
    )
  })
}

#' Build Loop and Merge options for one question fact
#' @keywords internal
#' @noRd
loop_options_for_question <- function(question, looping_source, looping_qid) {
  static_prefixes <- unlist(
    question_fact_looping_prefix(question),
    use.names = FALSE
  )

  if (!is.null(looping_source) &&
    scalar_character(question_fact_question_type(looping_source)$type) == "Matrix") { # nolint
    loop_items <- question_fact_response_items(looping_source)
    loop_options <- setNames(
      map_chr(loop_items, "item_text"),
      map_chr(loop_items, "item_id")
    )
    if (length(static_prefixes) > 0) {
      ordered_prefixes <- static_prefixes[
        static_prefixes %in% names(loop_options)
      ]
      if (length(ordered_prefixes) > 0) {
        loop_options <- loop_options[ordered_prefixes]
      }
    }
    loop_options <- loop_options[!is.na(loop_options)]
    return(loop_options)
  }

  if (is.null(looping_source)) {
    return(loop_options_from_static_fields(
      question_fact_looping_static(question),
      static_prefixes
    ))
  }

  loop_options_from_static_choices(
    question_fact_looping_prefix(question),
    question_fact_response_choices(looping_source),
    static_prefixes
  )
}

#' Resolve Loop and Merge options from static field values
#' @keywords internal
#' @noRd
loop_options_from_static_fields <- function(looping_static, static_prefixes) {
  if (is.null(looping_static) || length(static_prefixes) == 0) {
    return(NULL)
  }

  loop_options <- vapply(static_prefixes, function(prefix) {
    fields <- looping_static[[prefix]]
    option <- scalar_character(fields[["1"]])
    if (is.na(option) || option == "") {
      option <- prefix
    }
    option
  }, character(1))
  names(loop_options) <- static_prefixes
  loop_options
}

#' Return a Loop and Merge-specific response column identifier
#' @keywords internal
#' @noRd
loop_response_column_id <- function(response_column_id) {
  str_replace(
    response_column_id,
    "^([^_]+_QID[0-9]+)_[^_]+_TEXT_TEXT$",
    "\\1_TEXT"
  )
}

#' Resolve Loop and Merge options from static choices
#' @keywords internal
#' @noRd
loop_options_from_static_choices <- function(looping_prefixes,
                                             choices,
                                             static_prefixes) {
  if (is.null(choices) || length(static_prefixes) == 0) {
    return(NULL)
  }

  if (!is.null(looping_prefixes) && length(looping_prefixes) > 0) {
    choice_recodes <- map_chr(choices, ~ scalar_character(.x$recode))
    choice_by_recode <- setNames(choices, choice_recodes)
    static_choices <- map(static_prefixes, function(prefix) {
      if (prefix %in% names(choices)) {
        return(choices[[prefix]])
      }
      if (prefix %in% names(choice_by_recode)) {
        return(choice_by_recode[[prefix]])
      }
      NULL
    })
    if (all(map_lgl(static_choices, ~ !is.null(.x)))) {
      choices <- setNames(static_choices, static_prefixes)
    } else {
      choices <- setNames(
        lapply(static_prefixes, function(prefix) {
          list(description = prefix, choiceText = prefix)
        }),
        static_prefixes
      )
    }
  } else if (!all(static_prefixes %in% names(choices))) {
    return(NULL)
  }

  loop_options <- vapply(static_prefixes, function(prefix) {
    choice <- choices[[prefix]]
    option <- choice[["description"]]
    if (is.null(option) || is.na(option) || option == "") {
      option <- choice[["choiceText"]]
    }
    as.character(option)
  }, character(1))
  names(loop_options) <- static_prefixes
  loop_options
}

#' Resolve Loop and Merge field values beyond the primary option
#' @keywords internal
#' @noRd
loop_field_values_for_question <- function(question, prefixes) {
  field_values <- loop_field_values_from_static(
    question_fact_looping_static(question),
    prefixes
  )

  column_field_values <- loop_field_values_from_column_names(
    question_fact_looping_column_names(question),
    prefixes
  )

  utils::modifyList(column_field_values, field_values)
}

#' Resolve Loop and Merge fields from block Static rows
#' @keywords internal
#' @noRd
loop_field_values_from_static <- function(looping_static, prefixes) {
  if (is.null(looping_static) || length(prefixes) == 0) {
    return(setNames(vector("list", length(prefixes)), prefixes))
  }

  setNames(lapply(prefixes, function(prefix) {
    fields <- looping_static[[prefix]]
    if (is.null(fields) || length(fields) == 0) {
      return(character())
    }

    field_values <- map_chr(fields, scalar_character)
    field_values <- field_values[!is.na(field_values) & field_values != ""]
    field_values
  }), prefixes)
}

#' Resolve Loop and Merge fields from metadata column names
#' @keywords internal
#' @noRd
loop_field_values_from_column_names <- function(column_names, prefixes) {
  if (is.null(column_names) || length(prefixes) == 0) {
    return(setNames(vector("list", length(prefixes)), prefixes))
  }

  values_by_prefix <- setNames(vector("list", length(prefixes)), prefixes)
  for (field_name in names(column_names)) {
    field_number <- str_match(field_name, "^field([0-9]+)$")[, 2]
    if (is.na(field_number)) {
      next
    }

    values <- unlist(column_names[[field_name]], use.names = FALSE)
    if (length(values) != length(prefixes)) {
      next
    }

    for (index in seq_along(prefixes)) {
      value <- scalar_character(values[[index]])
      if (!is.na(value) && value != "") {
        values_by_prefix[[prefixes[[index]]]][[field_number]] <- value
      }
    }
  }

  values_by_prefix
}

#' Substitute Loop and Merge placeholders in question text
#' @keywords internal
#' @noRd
substitute_loop_fields <- function(question_text, fields) {
  if (is.null(question_text) || is.na(question_text)) {
    return(question_text)
  }

  for (field_number in names(fields)) {
    question_text <- str_replace_all(
      question_text,
      fixed(paste0("${lm://Field/", field_number, "}")),
      fields[[field_number]]
    )
  }

  question_text
}

#' Replace Loop and Merge placeholders with Semantic Name placeholders
#' @keywords internal
#' @noRd
loop_question_text_template <- function(question_text) {
  str_replace_all(
    question_text,
    "\\$\\{lm://Field/[0-9]+\\}",
    "{}"
  )
}

#' Extract Loop and Merge field numbers from question text
#' @keywords internal
#' @noRd
loop_field_numbers <- function(question_text) {
  if (is.null(question_text) || is.na(question_text)) {
    return(character())
  }

  matches <- str_match_all(
    question_text,
    "\\$\\{lm://Field/([0-9]+)\\}"
  )[[1]]

  if (nrow(matches) == 0) {
    return(character())
  }

  matches[, 2]
}
