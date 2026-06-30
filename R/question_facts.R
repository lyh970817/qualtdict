#' Coerce a possibly missing scalar to character
#' @noRd
scalar_character <- function(x) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) {
    return(NA_character_)
  }

  as.character(x[[1]])
}

#' Build one package-owned normalised question fact
#' @importFrom rlang %||%
#' @noRd
normalise_question_fact <- function(qid, question, block, content_type) {
  question_name <- scalar_character(question$questionName)
  question_text <- scalar_character(question$questionText)
  question_type <- question_fact_question_type(question)
  survey_block <- scalar_character(block$description)
  response_choices <- normalise_response_choices(question$choices)
  response_items <- normalise_response_items(question$subQuestions)
  column_facts <- normalise_column_facts(question$columns)
  looping_prefix <- block$looping_prefix
  looping_qid <- block$looping_qid
  looping_static <- block$looping_static
  looping_column_names <- block$looping_column_names

  structure(
    list(
      qid = qid,
      question_name = question_name,
      question_text = question_text,
      question_type = question_type,
      survey_block = survey_block,
      content_type = content_type,
      response_choices = response_choices,
      response_items = response_items,
      column_facts = column_facts,
      looping_prefix = looping_prefix,
      looping_qid = looping_qid,
      looping_static = looping_static,
      looping_column_names = looping_column_names
    ),
    class = c("qualtdict_normalised_question", "list")
  )
}

#' Build package-owned response choice facts
#' @noRd
normalise_response_choices <- function(choices) {
  imap(choices, function(choice, choice_id) {
    label <- scalar_character(choice$label %||% choice$description)
    text_entry <- "text_entry" %in%
      names(choice) ||
      "textEntry" %in% names(choice)
    analyze <- choice$analyze
    if (is.null(analyze) || length(analyze) == 0 || is.na(analyze[[1]])) {
      analyze <- TRUE
    }

    list(
      choice_id = choice_id,
      level = scalar_character(choice$level %||% choice$recode),
      label = label,
      text_entry = text_entry,
      recode = scalar_character(choice$level %||% choice$recode),
      description = label,
      analyze = isTRUE(analyze),
      textEntry = if (text_entry) TRUE else NULL
    )
  })
}

#' Build package-owned response item facts
#' @noRd
normalise_response_items <- function(items) {
  imap(items, function(item, item_id) {
    item_text <- scalar_character(item$item_text %||% item$choiceText)
    item_label <- scalar_character(item$item_label %||% item$description)
    text_entry <- "text_entry" %in% names(item) || "textEntry" %in% names(item)

    list(
      item_id = item_id,
      item_text = item_text,
      item_label = item_label,
      text_entry = text_entry,
      recode = scalar_character(item$level %||% item$recode),
      choiceText = item_text,
      description = item_label,
      textEntry = if (text_entry) TRUE else NULL
    )
  })
}

#' Build package-owned SBS column facts
#' @noRd
normalise_column_facts <- function(columns) {
  imap(columns, function(column, column_id) {
    question_type <- question_fact_question_type(column)

    list(
      column_id = column_id,
      question_text = scalar_character(
        column$question_text %||%
          column$questionText
      ),
      question_type = question_type,
      response_choices = normalise_response_choices(column$choices)
    )
  })
}

#' Return a question fact field with optional legacy fallback
#' @noRd
question_fact_value <- function(question, owned_name, legacy_name = NULL) {
  value <- question[[owned_name]]
  if (is.null(value) && !is.null(legacy_name)) {
    value <- question[[legacy_name]]
  }

  value
}

#' Return package-owned question type facts
#' @noRd
question_fact_question_type <- function(question) {
  question_type <- question_fact_value(
    question,
    "question_type",
    "questionType"
  )
  if (is.null(question_type)) {
    return(list(type = NULL, selector = NULL, sub_selector = NULL))
  }

  sub_selector <- question_type$sub_selector
  if (is.null(sub_selector)) {
    sub_selector <- question_type$subSelector
  }

  list(
    type = scalar_character(question_type$type),
    selector = scalar_character(question_type$selector),
    sub_selector = if (
      is.null(sub_selector) ||
        (length(sub_selector) == 1 && is.na(sub_selector))
    ) {
      NULL
    } else {
      scalar_character(sub_selector)
    }
  )
}

#' Return a package-owned question name
#' @noRd
question_fact_question_name <- function(question) {
  question_fact_value(question, "question_name", "questionName")
}

#' Return a package-owned question text
#' @noRd
question_fact_question_text <- function(question) {
  question_fact_value(question, "question_text", "questionText")
}

#' Return a package-owned survey block
#' @noRd
question_fact_survey_block <- function(question) {
  question_fact_value(question, "survey_block", "block")
}

#' Return package-owned response choices
#' @noRd
question_fact_response_choices <- function(question) {
  question_fact_value(question, "response_choices", "choices")
}

#' Return package-owned response items
#' @noRd
question_fact_response_items <- function(question) {
  question_fact_value(question, "response_items", "subQuestions")
}

#' Return package-owned column facts
#' @noRd
question_fact_column_facts <- function(question) {
  question_fact_value(question, "column_facts", "columns")
}

#' Return package-owned Loop and Merge prefix facts
#' @noRd
question_fact_looping_prefix <- function(question) {
  question_fact_value(question, "looping_prefix")
}

#' Return package-owned Loop and Merge source facts
#' @noRd
question_fact_looping_qid <- function(question) {
  question_fact_value(question, "looping_qid")
}

#' Return package-owned Loop and Merge static row facts
#' @noRd
question_fact_looping_static <- function(question) {
  question_fact_value(question, "looping_static")
}

#' Return package-owned Loop and Merge column-name facts
#' @noRd
question_fact_looping_column_names <- function(question) {
  question_fact_value(question, "looping_column_names")
}
