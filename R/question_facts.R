#' Coerce a possibly missing scalar to character
#' @noRd
scalar_character <- function(x) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) {
    return(NA_character_)
  }

  as.character(x[[1]])
}

#' Build one package-owned Normalised Question Fact
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
  choice_order <- as.character(question$choiceOrder %||% character())
  carry_forward <- question$carryForward
  randomization <- question$randomization
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
      choice_order = choice_order,
      carry_forward = carry_forward,
      randomization = randomization,
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

#' Return a Normalised Question Fact field with optional legacy fallback
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

#' Return a package-owned Survey Block
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

#' Return package-owned display-order choice facts
#' @noRd
question_fact_choice_order <- function(question) {
  question_fact_value(question, "choice_order", "choiceOrder")
}

#' Return package-owned carry-forward facts
#' @noRd
question_fact_carry_forward <- function(question) {
  question_fact_value(question, "carry_forward", "carryForward")
}

#' Return package-owned randomization facts
#' @noRd
question_fact_randomization <- function(question) {
  question_fact_value(question, "randomization")
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

#' Return package-owned Loop and Merge metadata field facts
#' @noRd
question_fact_looping_column_names <- function(question) {
  question_fact_value(question, "looping_column_names")
}

#' Return package-owned Base Response Column ID facts
#' @noRd
question_fact_base_response_column_id <- function(question) {
  question_fact_value(question, "base_response_column_id")
}

#' Return substituted Loop and Merge question text
#' @noRd
question_fact_looping_question <- function(question) {
  question_fact_value(question, "looping_question")
}

#' Return the resolved Loop Option
#' @noRd
question_fact_looping_option <- function(question) {
  question_fact_value(question, "looping_option")
}

#' Return whether a question fact is Loop-expanded
#' @noRd
question_fact_looping_status <- function(question) {
  isTRUE(question_fact_value(question, "looping"))
}

#' Return the resolved Loop and Merge prefix for a Loop-expanded Question Fact
#' @noRd
question_fact_looping_prefix_value <- function(question) {
  question_fact_value(question, "looping_prefix")
}
