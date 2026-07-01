#' Expand Loop and Merge Normalised Question Facts
#' @noRd
expand_loop_question_facts <- function(survey_question_facts) {
  imap(survey_question_facts, function(question_fact, bare_qid) {
    context <- new_loop_expansion_context(
      question_fact = question_fact,
      survey_question_facts = survey_question_facts
    )

    expand_loop_question_fact(context)
  }) |>
    unlist(recursive = FALSE)
}

#' Build Loop and Merge expansion context for one Normalised Question Fact
#' @noRd
new_loop_expansion_context <- function(question_fact, survey_question_facts) {
  looping_qid <- scalar_character(question_fact_looping_qid(question_fact))
  looping_static <- question_fact_looping_static(question_fact)

  list(
    question_fact = question_fact,
    looping_qid = looping_qid,
    looping_source_fact = if (!is.na(looping_qid)) {
      survey_question_facts[[looping_qid]]
    } else {
      NULL
    },
    looping_static = looping_static,
    static_prefixes = unlist(
      question_fact_looping_prefix(question_fact),
      use.names = FALSE
    )
  )
}

#' Return whether one Normalised Question Fact should be loop-expanded
#' @noRd
loop_question_fact_should_expand <- function(context) {
  has_static_loop <- !is.null(context$looping_static) &&
    length(context$looping_static) > 0

  if (is.na(context$looping_qid) && !has_static_loop) {
    return(FALSE)
  }
  if (
    !is.na(context$looping_qid) &&
      is.null(context$looping_source_fact) &&
      !has_static_loop
  ) {
    return(FALSE)
  }

  TRUE
}

#' Expand one Normalised Question Fact or mark it as not looping
#' @noRd
expand_loop_question_fact <- function(context) {
  if (!loop_question_fact_should_expand(context)) {
    return(list(mark_question_fact_not_looping(context$question_fact)))
  }

  loop_rows <- loop_rows_for_context(context)
  if (is.null(loop_rows) || length(loop_rows) == 0) {
    return(list(context$question_fact))
  }

  map(loop_rows, loop_expanded_question_fact, context = context)
}

#' Mark a Normalised Question Fact as not loop-expanded
#' @noRd
mark_question_fact_not_looping <- function(question_fact) {
  question_fact[["looping"]] <- FALSE
  question_fact
}

#' Build Loop and Merge row facts for one Normalised Question Fact
#' @noRd
loop_rows_for_context <- function(context) {
  loop_options <- loop_options_for_context(context)

  if (is.null(loop_options) || length(loop_options) == 0) {
    return(NULL)
  }

  field_values <- loop_field_values_for_question(
    question_fact = context$question_fact,
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

#' Build Loop and Merge options for one Normalised Question Fact
#' @noRd
loop_options_for_context <- function(context) {
  static_prefixes <- context$static_prefixes
  looping_source_fact <- context$looping_source_fact

  source_type <- if (!is.null(looping_source_fact)) {
    scalar_character(question_fact_question_type(looping_source_fact)$type)
  } else {
    NA_character_
  }

  if (!is.null(looping_source_fact) && source_type == "Matrix") {
    loop_items <- question_fact_response_items(looping_source_fact)
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

  if (is.null(looping_source_fact)) {
    return(loop_options_from_static_fields(
      context$looping_static,
      static_prefixes
    ))
  }

  loop_options_from_static_choices(
    question_fact_looping_prefix(context$question_fact),
    question_fact_response_choices(looping_source_fact),
    static_prefixes
  )
}

#' Build one Loop-expanded Question Fact
#' @noRd
loop_expanded_question_fact <- function(loop_row, context) {
  question_fact <- context$question_fact
  looped_question_fact <- question_fact
  looped_question_fact[["looping_question"]] <-
    substitute_loop_fields(
      question_fact[["question_text"]],
      loop_row$fields
    )
  looped_question_fact[["question_text"]] <-
    loop_question_text_template(question_fact[["question_text"]])
  looped_question_fact[["looping_option"]] <- loop_row$option
  looped_question_fact[["looping_prefix"]] <- loop_row$prefix
  looped_question_fact[["looping_qid"]] <- context$looping_qid
  looped_question_fact[["qid"]] <- question_fact[["qid"]]
  looped_question_fact[["base_response_column_id"]] <-
    loop_response_column_id(
      paste(loop_row$prefix, question_fact[["qid"]], sep = "_")
    )
  looped_question_fact[["looping"]] <- TRUE
  looped_question_fact
}

#' Resolve Loop and Merge options from static field values
#' @noRd
loop_options_from_static_fields <- function(looping_static, static_prefixes) {
  if (is.null(looping_static) || length(static_prefixes) == 0) {
    return(NULL)
  }

  loop_options <- vapply(
    static_prefixes,
    function(prefix) {
      fields <- looping_static[[prefix]]
      option <- scalar_character(fields[["1"]])
      if (is.na(option) || option == "") {
        option <- prefix
      }
      option
    },
    character(1)
  )
  names(loop_options) <- static_prefixes
  loop_options
}

#' Return a Loop and Merge-specific Response Column ID
#' @noRd
loop_response_column_id <- function(response_column_id) {
  str_replace(
    response_column_id,
    "^([^_]+_QID[0-9]+)_[^_]+_TEXT_TEXT$",
    "\\1_TEXT"
  )
}

#' Resolve Loop and Merge options from static choices
#' @noRd
loop_options_from_static_choices <- function(
  looping_prefixes,
  choices,
  static_prefixes
) {
  source <- loop_choice_source(looping_prefixes, choices, static_prefixes)
  if (loop_choice_source_is_missing(source)) {
    return(NULL)
  }

  loop_options_from_choice_source(source, static_prefixes)
}

#' Resolve the source of Loop Option choices
#' @noRd
loop_choice_source <- function(looping_prefixes, choices, static_prefixes) {
  if (is.null(choices) || length(static_prefixes) == 0) {
    return(new_loop_choice_source("missing"))
  }

  if (has_looping_prefixes(looping_prefixes)) {
    return(loop_choice_source_from_prefixes(choices, static_prefixes))
  }

  loop_choice_source_from_direct_ids(choices, static_prefixes)
}

#' Build a Loop Option choice source
#' @noRd
new_loop_choice_source <- function(type, choices = NULL) {
  list(type = type, choices = choices)
}

#' Return whether a Loop Option choice source is missing
#' @noRd
loop_choice_source_is_missing <- function(source) {
  identical(source$type, "missing")
}

#' Return whether looping prefixes are present
#' @noRd
has_looping_prefixes <- function(looping_prefixes) {
  !is.null(looping_prefixes) && length(looping_prefixes) > 0
}

#' Resolve Loop Option choices from static prefixes
#' @noRd
loop_choice_source_from_prefixes <- function(choices, static_prefixes) {
  resolved_choices <- static_choices_by_id_or_recode(choices, static_prefixes)
  resolved <- map_lgl(resolved_choices, Negate(is.null))
  if (!any(resolved) || mean(resolved) < 0.5) {
    non_exported_choices <- map_lgl(resolved_choices, function(choice) {
      !is.null(choice) && isFALSE(choice$analyze)
    })
    static_prefixes <- static_prefixes[!non_exported_choices]
    return(new_loop_choice_source(
      "fallback",
      fallback_static_choices(static_prefixes)
    ))
  }

  source_choices <- map2(
    resolved_choices,
    static_prefixes,
    function(choice, prefix) {
      if (is.null(choice)) {
        return(NULL)
      }
      if (isFALSE(choice$analyze)) {
        return(NULL)
      }

      choice
    }
  )

  keep <- map_lgl(source_choices, Negate(is.null))
  new_loop_choice_source(
    "resolved",
    setNames(source_choices[keep], static_prefixes[keep])
  )
}

#' Resolve Loop Option choices from direct choice IDs
#' @noRd
loop_choice_source_from_direct_ids <- function(choices, static_prefixes) {
  if (!all(static_prefixes %in% names(choices))) {
    return(new_loop_choice_source("missing"))
  }

  new_loop_choice_source("direct", choices)
}

#' Resolve static choices by ID or recode
#' @noRd
static_choices_by_id_or_recode <- function(choices, static_prefixes) {
  choice_recodes <- map_chr(choices, ~ scalar_character(.x$recode))
  choice_by_recode <- setNames(choices, choice_recodes)

  map(static_prefixes, function(prefix) {
    if (prefix %in% names(choices)) {
      return(choices[[prefix]])
    }
    if (prefix %in% names(choice_by_recode)) {
      return(choice_by_recode[[prefix]])
    }
    NULL
  })
}

#' Build fallback choices from static prefixes
#' @noRd
fallback_static_choices <- function(static_prefixes) {
  setNames(
    lapply(static_prefixes, fallback_static_choice),
    static_prefixes
  )
}

#' Build one fallback static choice
#' @noRd
fallback_static_choice <- function(prefix) {
  list(description = prefix, choiceText = prefix)
}

#' Resolve Loop Options from a choice source
#' @noRd
loop_options_from_choice_source <- function(source, static_prefixes) {
  static_prefixes <- static_prefixes[static_prefixes %in% names(source$choices)]
  loop_options <- vapply(
    static_prefixes,
    function(prefix) {
      loop_option_label(source$choices[[prefix]])
    },
    character(1)
  )
  names(loop_options) <- static_prefixes
  loop_options
}

#' Resolve a Loop Option label from a choice
#' @noRd
loop_option_label <- function(choice) {
  option <- choice[["description"]]
  if (is.null(option) || is.na(option) || option == "") {
    option <- choice[["choiceText"]]
  }

  as.character(option)
}

#' Resolve Loop and Merge field values beyond the primary option
#' @noRd
loop_field_values_for_question <- function(question_fact, prefixes) {
  field_values <- loop_field_values_from_static(
    question_fact_looping_static(question_fact),
    prefixes
  )

  column_field_values <- loop_field_values_from_column_names(
    question_fact_looping_column_names(question_fact),
    prefixes
  )

  utils::modifyList(column_field_values, field_values)
}

#' Resolve Loop and Merge fields from block Static rows
#' @noRd
loop_field_values_from_static <- function(looping_static, prefixes) {
  if (is.null(looping_static) || length(prefixes) == 0) {
    return(setNames(vector("list", length(prefixes)), prefixes))
  }

  setNames(
    lapply(prefixes, function(prefix) {
      fields <- looping_static[[prefix]]
      if (is.null(fields) || length(fields) == 0) {
        return(character())
      }

      field_values <- map_chr(fields, scalar_character)
      field_values <- field_values[!is.na(field_values) & field_values != ""]
      field_values
    }),
    prefixes
  )
}

#' Resolve Loop and Merge fields from metadata column fields
#' @noRd
loop_field_values_from_column_names <- function(column_names, prefixes) {
  if (is.null(column_names) || length(prefixes) == 0) {
    return(empty_loop_field_values(prefixes))
  }

  loop_field_values_from_records(
    records = loop_column_field_records(column_names, prefixes),
    prefixes = prefixes
  )
}

#' Empty Loop and Merge field values for prefixes
#' @noRd
empty_loop_field_values <- function(prefixes) {
  setNames(vector("list", length(prefixes)), prefixes)
}

#' Parse Loop and Merge field records from metadata column fields
#' @noRd
loop_column_field_records <- function(column_names, prefixes) {
  records <- lapply(names(column_names), function(field_name) {
    loop_column_field_record(field_name, column_names[[field_name]])
  })

  Filter(
    function(record) valid_loop_column_field_record(record, prefixes),
    records
  )
}

#' Build one Loop and Merge column field record
#' @noRd
loop_column_field_record <- function(field_name, values) {
  list(
    field_number = loop_column_field_number(field_name),
    values = unlist(values, use.names = FALSE)
  )
}

#' Resolve the Loop and Merge field number
#' @noRd
loop_column_field_number <- function(field_name) {
  str_match(field_name, "^field([0-9]+)$")[, 2]
}

#' Return whether one Loop and Merge field record is valid
#' @noRd
valid_loop_column_field_record <- function(record, prefixes) {
  !is.na(record$field_number) && length(record$values) == length(prefixes)
}

#' Combine Loop and Merge field records by prefix
#' @noRd
loop_field_values_from_records <- function(records, prefixes) {
  values_by_prefix <- empty_loop_field_values(prefixes)

  for (record in records) {
    values_by_prefix <- add_loop_field_record_values(
      values_by_prefix,
      prefixes,
      record
    )
  }

  values_by_prefix
}

#' Add one Loop and Merge field record to prefix values
#' @noRd
add_loop_field_record_values <- function(values_by_prefix, prefixes, record) {
  for (index in seq_along(prefixes)) {
    value <- scalar_character(record$values[[index]])
    if (!is.na(value) && value != "") {
      values_by_prefix[[prefixes[[index]]]][[record$field_number]] <- value
    }
  }

  values_by_prefix
}

#' Substitute Loop and Merge placeholders in question text
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
#' @noRd
loop_question_text_template <- function(question_text) {
  str_replace_all(
    question_text,
    "\\$\\{lm://Field/[0-9]+\\}",
    "{}"
  )
}
