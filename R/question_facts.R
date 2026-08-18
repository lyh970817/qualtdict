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
normalise_question_fact <- function(
  qid,
  question,
  block,
  content_type,
  description = NULL
) {
  question_name <- scalar_character(question$questionName)
  question_text <- scalar_character(question$questionText)
  question_type <- raw_question_type(question)
  survey_block <- scalar_character(block$description)
  recode_override <- resolve_dynamic_choice_recode_override(
    question,
    description
  )
  response_choices <- normalise_response_choices(
    question$choices,
    recode_override
  )
  response_items <- normalise_response_items(question$subQuestions)
  column_facts <- normalise_column_facts(question$columns, question$columnOrder)
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

#' Resolve export choice IDs for dynamic / carry-forward multiple choice
#'
#' Qualtrics renumbers the choices of carry-forward (dynamic-choice) questions
#' sequentially in the question metadata (`mt`), but the real export column
#' suffixes are the original Qualtrics choice IDs. These true IDs are only
#' recoverable from the survey *description* metadata (`mt_d`): either verbatim
#' from `RecodeValues`, or, when that is absent, by offsetting each
#' carried-forward choice's source ID (`x<N>` key) by the maximum of the
#' question's own static choice IDs. Non-dynamic questions are left untouched.
#' @noRd
resolve_dynamic_choice_recode_override <- function(question, description) {
  if (is.null(description)) {
    return(NULL)
  }
  dynamic <- description$DynamicChoices
  if (is.null(dynamic) || is.null(dynamic$Locator)) {
    return(NULL)
  }

  choice_ids <- names(question$choices)
  if (is.null(choice_ids) || length(choice_ids) == 0) {
    return(NULL)
  }

  # Tier 1: RecodeValues carries the export choice IDs verbatim.
  recode_values <- description$RecodeValues
  if (!is.null(recode_values) && length(recode_values) > 0) {
    return(vapply(recode_values, scalar_character, character(1)))
  }

  # Tier 2: derive the export choice ID for each carried-forward choice.
  derive_carryforward_choice_ids(choice_ids, description$Choices)
}

#' Derive export choice IDs for carried-forward choices
#'
#' Offsets each carried-forward choice's source ID (`x<N>` key) by the maximum
#' of the question's own static choice IDs; own static choices keep their key.
#' @noRd
derive_carryforward_choice_ids <- function(choice_ids, own_choices) {
  own_keys <- suppressWarnings(as.integer(names(own_choices)))
  own_keys <- own_keys[!is.na(own_keys)]
  offset <- if (length(own_keys) > 0) max(own_keys) else 0L

  carried <- regmatches(choice_ids, regexec("^x([0-9]+)$", choice_ids))
  override <- vapply(
    seq_along(choice_ids),
    function(i) {
      match <- carried[[i]]
      if (length(match) == 2L) {
        as.character(offset + as.integer(match[[2]]))
      } else {
        choice_ids[[i]]
      }
    },
    character(1)
  )
  setNames(override, choice_ids)
}

#' Build package-owned response choice facts
#' @noRd
normalise_response_choices <- function(choices, recode_override = NULL) {
  imap(choices, function(choice, choice_id) {
    label <- scalar_character(choice$label %||% choice$description)
    text_entry <- "text_entry" %in%
      names(choice) ||
      "textEntry" %in% names(choice)
    analyze <- choice$analyze
    if (is.null(analyze) || length(analyze) == 0 || is.na(analyze[[1]])) {
      analyze <- TRUE
    }

    recode <- choice$level %||% choice$recode
    if (
      !is.null(recode_override) &&
        choice_id %in% names(recode_override) &&
        !is.na(recode_override[[choice_id]])
    ) {
      recode <- recode_override[[choice_id]]
    }

    list(
      choice_id = choice_id,
      level = scalar_character(recode),
      label = label,
      text_entry = text_entry,
      analyze = isTRUE(analyze)
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
      level = scalar_character(item$level %||% item$recode)
    )
  })
}

#' Build package-owned SBS column facts
#'
#' `column_position` records the 1-based place of each column in `columnOrder`
#' (falling back to list order when `columnOrder` is absent). Response Column
#' ID Rendering uses it to walk left through the grid columns when qualifying
#' duplicated column question text and to render honest ordinal fallbacks.
#' @noRd
normalise_column_facts <- function(columns, column_order = NULL) {
  ordered_ids <- normalise_column_order(columns, column_order)

  imap(columns, function(column, column_id) {
    question_type <- raw_question_type(column)

    list(
      column_id = column_id,
      column_position = match(column_id, ordered_ids),
      question_text = scalar_character(
        column$question_text %||%
          column$questionText
      ),
      question_type = question_type,
      response_choices = normalise_response_choices(column$choices)
    )
  })
}

#' Resolve the SBS column order used to place each column
#' @noRd
normalise_column_order <- function(columns, column_order) {
  ids <- names(columns)
  if (is.null(column_order) || length(column_order) == 0) {
    return(ids)
  }
  ordered <- as.character(unlist(column_order))
  ordered <- ordered[ordered %in% ids]

  c(ordered, setdiff(ids, ordered))
}

#' Read the raw Qualtrics question type triple at the normalisation boundary
#'
#' Private to the normaliser: only raw metadata may still spell the triple in
#' camelCase. `normalise_question_fact()` stores the clean triple once, and
#' downstream code reads `question$question_type$type` (and friends) directly.
#' @noRd
raw_question_type <- function(raw) {
  question_type <- raw[["question_type"]] %||% raw[["questionType"]]
  if (is.null(question_type)) {
    return(list(type = NULL, selector = NULL, sub_selector = NULL))
  }

  sub_selector <- question_type$sub_selector %||% question_type$subSelector
  list(
    type = scalar_character(question_type$type),
    selector = scalar_character(question_type$selector),
    sub_selector = normalise_sub_selector(sub_selector)
  )
}

#' Normalise a raw sub-selector to a scalar character or NULL
#' @noRd
normalise_sub_selector <- function(sub_selector) {
  if (
    is.null(sub_selector) ||
      (length(sub_selector) == 1 && is.na(sub_selector))
  ) {
    return(NULL)
  }

  scalar_character(sub_selector)
}
