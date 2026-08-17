#' Expand Loop and Merge Normalised Question Facts
#' @noRd
expand_loop_question_facts <- function(survey_question_facts) {
  outcomes <- imap(survey_question_facts, function(question_fact, bare_qid) {
    context <- new_loop_expansion_context(
      question_fact = question_fact,
      survey_question_facts = survey_question_facts
    )

    expand_loop_question_fact(context)
  })

  question_facts <- unlist(
    map(outcomes, loop_expansion_outcome_question_facts),
    recursive = FALSE
  )
  diagnostics <- unlist(
    map(outcomes, loop_expansion_outcome_diagnostics),
    recursive = FALSE
  )

  attr(question_facts, "unsupported_loop_diagnostics") <- diagnostics
  question_facts
}

#' Expand Loop and Merge question facts and render ordinary response columns
#' @noRd
expand_then_render_question_response_columns <- function(question_facts) {
  expanded_question_facts <- expand_loop_question_facts(question_facts)

  rendered <- imap(expanded_question_facts, function(question_fact, qid) {
    base_response_column_id <-
      question_fact$base_response_column_id %||% qid
    response_columns <- render_response_columns(
      question_fact,
      base_response_column_id
    )
    list(
      question_fact = question_fact,
      response_column_id = unique(response_columns$response_column_id),
      response_columns = response_columns
    )
  })
  attr(rendered, "unsupported_loop_diagnostics") <-
    attr(expanded_question_facts, "unsupported_loop_diagnostics")

  rendered
}

#' Build Loop and Merge expansion context for one Normalised Question Fact
#' @noRd
new_loop_expansion_context <- function(question_fact, survey_question_facts) {
  looping_qid <- scalar_character(question_fact$looping_qid)
  looping_static <- question_fact$looping_static

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
      question_fact$looping_prefix,
      use.names = FALSE
    )
  )
}

#' Build one Loop and Merge expansion outcome
#' @noRd
new_loop_expansion_outcome <- function(
  type,
  question_facts = list(),
  diagnostics = list()
) {
  list(
    type = type,
    question_facts = question_facts,
    diagnostics = diagnostics
  )
}

#' Build a non-looping expansion outcome
#' @noRd
loop_expansion_outcome_not_looping <- function(question_fact) {
  new_loop_expansion_outcome(
    type = "not_looping",
    question_facts = list(mark_question_fact_not_looping(question_fact))
  )
}

#' Build an expanded Loop and Merge outcome
#' @noRd
loop_expansion_outcome_expanded <- function(question_facts) {
  new_loop_expansion_outcome(
    type = "expanded_loop",
    question_facts = question_facts
  )
}

#' Build an unsupported Loop and Merge expansion outcome
#' @noRd
loop_expansion_outcome_unsupported <- function(context, reason) {
  new_loop_expansion_outcome(
    type = "unsupported_loop",
    diagnostics = list(new_unsupported_loop_diagnostic(context, reason))
  )
}

#' Return question facts from one Loop and Merge expansion outcome
#' @noRd
loop_expansion_outcome_question_facts <- function(outcome) {
  outcome$question_facts %||% list()
}

#' Return diagnostics from one Loop and Merge expansion outcome
#' @noRd
loop_expansion_outcome_diagnostics <- function(outcome) {
  outcome$diagnostics %||% list()
}

#' Build one internal unsupported Loop and Merge diagnostic
#' @noRd
new_unsupported_loop_diagnostic <- function(context, reason) {
  list(
    qid = scalar_character(context$question_fact$qid),
    question_name = scalar_character(
      context$question_fact$question_name
    ),
    looping_qid = scalar_character(context$looping_qid),
    reason = reason
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

  TRUE
}

#' Resolve an internal unsupported Loop and Merge reason
#' @noRd
unsupported_loop_reason <- function(context) {
  if (
    !is.na(context$looping_qid) &&
      is.null(context$looping_source_fact)
  ) {
    return("source QID is absent from Normalised Question Facts")
  }

  "static prefixes cannot be resolved against source choices"
}

#' Expand one Normalised Question Fact or mark it as not looping
#' @noRd
expand_loop_question_fact <- function(context) {
  if (!loop_question_fact_should_expand(context)) {
    return(loop_expansion_outcome_not_looping(context$question_fact))
  }

  loop_rows <- loop_rows_for_context(context)
  if (is.null(loop_rows) || length(loop_rows) == 0) {
    reason <- unsupported_loop_reason(context)
    return(loop_expansion_outcome_unsupported(context, reason))
  }

  loop_expansion_outcome_expanded(
    map(loop_rows, loop_expanded_question_fact, context = context)
  )
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
  looping_source_fact <- context$looping_source_fact

  if (is.null(looping_source_fact)) {
    return(loop_options_from_static_only_source(context))
  }

  source_type <- scalar_character(
    looping_source_fact$question_type$type
  )

  if (identical(source_type, "Matrix")) {
    return(loop_options_from_matrix_source(context))
  }

  loop_options_from_choice_source_context(context)
}

#' Resolve Loop Options from a Matrix source
#' @noRd
loop_options_from_matrix_source <- function(context) {
  loop_items <- context$looping_source_fact$response_items
  if (is.null(loop_items) || length(loop_items) == 0) {
    return(NULL)
  }

  loop_options <- setNames(
    map_chr(loop_items, "item_text"),
    map_chr(loop_items, "item_id")
  )
  loop_options <- loop_options[!is.na(loop_options)]
  if (length(loop_options) == 0) {
    return(NULL)
  }

  static_prefixes <- context$static_prefixes
  if (length(static_prefixes) > 0) {
    ordered_prefixes <- reconcile_matrix_source_order(
      static_prefixes,
      names(loop_options)
    )
    ordered_prefixes <- ordered_prefixes[
      ordered_prefixes %in% names(loop_options)
    ]
    loop_options <- loop_options[ordered_prefixes]
  }

  loop_options
}

#' Resolve Loop Options from a choice source
#' @noRd
loop_options_from_choice_source_context <- function(context) {
  loop_options_from_static_choices(
    context$question_fact$looping_prefix,
    context$looping_source_fact$response_choices,
    context$static_prefixes
  )
}

#' Resolve Loop Options from static Loop and Merge rows
#'
#' Covers both source-less cases: no Loop and Merge source QID at all, and a
#' source QID whose Question Facts are absent from the survey.
#' @noRd
loop_options_from_static_only_source <- function(context) {
  loop_options_from_static_fields(
    context$looping_static,
    context$static_prefixes
  )
}

#' Build one Loop-expanded Question Fact
#' @noRd
loop_expanded_question_fact <- function(loop_row, context) {
  new_loop_expanded_question_fact(
    question_fact = context$question_fact,
    loop_row = loop_row,
    context = context
  )
}

#' Build one Loop-expanded Question Fact from resolved Loop and Merge fields
#' @noRd
new_loop_expanded_question_fact <- function(
  question_fact,
  loop_row,
  context
) {
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
    normalise_loop_base_response_column_id(
      paste(
        looped_question_fact[["looping_prefix"]],
        question_fact[["qid"]],
        sep = "_"
      )
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

#' Normalise a Loop and Merge Base Response Column ID
#' @noRd
normalise_loop_base_response_column_id <- function(response_column_id) {
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

  loop_options_from_choice_source(
    source,
    source$static_prefixes %||% static_prefixes
  )
}

#' Resolve the source of Loop Option choices
#' @noRd
loop_choice_source <- function(looping_prefixes, choices, static_prefixes) {
  if (is.null(choices) || length(static_prefixes) == 0) {
    return(new_loop_choice_source("missing"))
  }

  if (has_looping_prefixes(looping_prefixes)) {
    return(choice_source_from_static_prefixes(choices, static_prefixes))
  }

  loop_choice_source_from_direct_ids(choices, static_prefixes)
}

#' Build a Loop Option choice source
#' @noRd
new_loop_choice_source <- function(
  type,
  choices = NULL,
  static_prefixes = NULL
) {
  list(type = type, choices = choices, static_prefixes = static_prefixes)
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
choice_source_from_static_prefixes <- function(choices, static_prefixes) {
  static_prefixes <- reconcile_choice_source_omitted_ids(
    choices,
    static_prefixes
  )
  resolved_choices <- static_choices_by_id_or_level(choices, static_prefixes)
  resolved <- map_lgl(resolved_choices, Negate(is.null))
  if (!any(resolved) || !all(resolved)) {
    return(new_loop_choice_source("missing"))
  }

  new_loop_choice_source(
    "resolved",
    setNames(resolved_choices, static_prefixes),
    static_prefixes = static_prefixes
  )
}

#' Reconcile Matrix source order against static Loop and Merge prefixes
#' @noRd
reconcile_matrix_source_order <- function(static_prefixes, source_ids) {
  if (length(static_prefixes) == 0 || length(source_ids) == 0) {
    return(static_prefixes)
  }

  replace_count <- min(length(static_prefixes), length(source_ids))
  for (pos in seq_len(replace_count)) {
    if (!static_prefixes[[pos]] %in% source_ids) {
      static_prefixes[[pos]] <- source_ids[[pos]]
    }
  }

  static_prefixes
}

#' Insert omitted source choice IDs into static Loop and Merge prefixes
#' @noRd
reconcile_choice_source_omitted_ids <- function(choices, static_prefixes) {
  resolved_choices <- static_choices_by_id_or_level(choices, static_prefixes)
  unresolved <- map_lgl(resolved_choices, is.null)
  source_ids <- names(choices)
  unlist(
    map(
      seq_along(static_prefixes),
      reconcile_choice_source_position,
      static_prefixes = static_prefixes,
      source_ids = source_ids,
      unresolved = unresolved
    ),
    use.names = FALSE
  )
}

#' Reconcile one static choice prefix against one source choice position
#' @noRd
reconcile_choice_source_position <- function(
  pos,
  static_prefixes,
  source_ids,
  unresolved
) {
  prefix <- static_prefixes[[pos]]
  if (pos > length(source_ids)) {
    return(prefix)
  }

  source_id <- source_ids[[pos]]
  if (
    unresolved[[pos]] &&
      is_supported_choice_source_prefix(prefix, source_id)
  ) {
    return(source_id)
  }

  if (is_stale_choice_source_prefix(prefix, source_id, static_prefixes)) {
    return(c(source_id, prefix))
  }

  prefix
}

#' Return whether one unresolved static prefix is supported for source insertion
#' @noRd
is_supported_choice_source_prefix <- function(prefix, source_id) {
  grepl("^x[0-9]+$", prefix) &&
    grepl("^x[0-9]+$", source_id)
}

#' Return whether one static prefix is stale against the source choice ID
#' @noRd
is_stale_choice_source_prefix <- function(prefix, source_id, static_prefixes) {
  grepl("^x[0-9]+$", prefix) &&
    grepl("^x[0-9]+$", source_id) &&
    !identical(prefix, source_id) &&
    !source_id %in% static_prefixes
}

#' Resolve Loop Option choices from direct choice IDs
#' @noRd
loop_choice_source_from_direct_ids <- function(choices, static_prefixes) {
  if (!all(static_prefixes %in% names(choices))) {
    return(new_loop_choice_source("missing"))
  }

  new_loop_choice_source("direct", choices, static_prefixes = static_prefixes)
}

#' Resolve static choices by ID or Level
#' @noRd
static_choices_by_id_or_level <- function(choices, static_prefixes) {
  choice_levels <- map_chr(choices, ~ scalar_character(.x$level))
  choice_by_level <- setNames(choices, choice_levels)

  map(static_prefixes, function(prefix) {
    if (prefix %in% names(choices)) {
      return(choices[[prefix]])
    }
    if (prefix %in% names(choice_by_level)) {
      return(choice_by_level[[prefix]])
    }
    NULL
  })
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
  option <- choice[["label"]]
  if (is.null(option) || is.na(option) || option == "") {
    option <- choice[["choice_id"]]
  }

  as.character(option)
}

#' Resolve Loop and Merge field values with static fields taking precedence
#' @noRd
loop_field_values_static_overrides_column_names <- function(
  question_fact,
  prefixes
) {
  field_values <- loop_field_values_from_static(
    question_fact$looping_static,
    prefixes
  )

  column_field_values <- loop_field_values_from_column_names(
    question_fact$looping_column_names,
    prefixes
  )

  utils::modifyList(column_field_values, field_values)
}

#' Resolve Loop and Merge field values beyond the primary option
#' @noRd
loop_field_values_for_question <- function(question_fact, prefixes) {
  loop_field_values_static_overrides_column_names(question_fact, prefixes)
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
