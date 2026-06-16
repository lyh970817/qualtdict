#' Empty Unsupported Structure Findings table
#'
#' Unsupported Structure Findings are produced during normalisation and travel
#' with generated Variable Dictionaries as an attribute.
#'
#' @keywords internal
#' @noRd
empty_unsupported_structure_findings <- function() {
  tibble(
    qid = character(),
    type = character(),
    selector = character(),
    sub_selector = character(),
    finding = character(),
    details = character()
  )
}

#' Return Unsupported Structure Findings
#'
#' Unsupported Structure Findings describe Qualtrics structures that qualtdict
#' could not fully represent while normalising survey metadata. They are
#' attached to generated Variable Dictionaries and can also be read from the
#' internal normalised metadata object.
#'
#' @param x A Variable Dictionary from [dict_generate()] or an internal
#'   normalised metadata object.
#'
#' @return A data frame of Unsupported Structure Findings.
#' @export
unsupported_structure_findings <- function(x) {
  findings <- attr(x, "unsupported_structure_findings", exact = TRUE)

  if (is.null(findings) &&
    is.list(x) &&
    "unsupported_structure_findings" %in% names(x)) {
    findings <- x$unsupported_structure_findings
  }

  if (is.null(findings)) {
    return(empty_unsupported_structure_findings())
  }

  findings
}

#' Classify raw response columns that are not question dictionary rows
#' @keywords internal
#' @noRd
classify_raw_response_columns <- function(raw_columns) {
  classification <- rep(NA_character_, length(raw_columns))
  details <- rep(NA_character_, length(raw_columns))

  scoring <- grepl("^SC_", raw_columns) | grepl("_SCORE$", raw_columns)
  classification[scoring] <- "scoring"
  details[scoring] <-
    "Qualtrics scoring column; not represented as a question dictionary row."

  text_analysis <- is.na(classification) &
    grepl("^(.+_)?QID[0-9]+(_[0-9]+)?_TEXT_.+", raw_columns)
  classification[text_analysis] <- "text_analysis"
  details[text_analysis] <-
    "Qualtrics text-analysis sidecar column; not represented as a question dictionary row."

  embedded_data <- is.na(classification) &
    grepl("^[A-Za-z][A-Za-z0-9_]*$", raw_columns) &
    !grepl("^QID[0-9]+", raw_columns)
  classification[embedded_data] <- "embedded_data"
  details[embedded_data] <-
    "Embedded-data or user-defined metadata column; not represented as a question dictionary row."

  tibble(
    raw_column = raw_columns,
    classification = classification,
    details = details
  )
}

#' Detect unsupported Loop and Merge shapes
#' @keywords internal
#' @noRd
unsupported_loop_structure_findings <- function(questions) {
  findings <- imap(questions, function(question, qid) {
    looping_qid <- scalar_character(question_fact_looping_qid(question))
    looping_static <- question_fact_looping_static(question)
    has_static_loop <- !is.null(looping_static) && length(looping_static) > 0

    if (is.na(looping_qid) && !has_static_loop) {
      return(NULL)
    }

    unsupported <- list()

    missing_loop_source <- !is.na(looping_qid) &&
      !looping_qid %in% names(questions)
    if (missing_loop_source) {
      unsupported <- c(unsupported, list(unsupported_structure_finding(
        qid = qid,
        question = question,
        finding = "missing_loop_source",
        details = paste0(
          "Loop and Merge source question `",
          looping_qid,
          "` was not found in survey metadata."
        )
      )))
    }

    if (!missing_loop_source) {
      loop_fields <- loop_field_numbers(question_fact_question_text(question))
      unsupported_fields <- unsupported_loop_field_numbers(
        question = question,
        looping_source = questions[[looping_qid]],
        looping_qid = looping_qid,
        loop_fields = loop_fields
      )
    } else {
      unsupported_fields <- character()
    }

    if (length(unsupported_fields) > 0) {
      unsupported <- c(unsupported, list(unsupported_structure_finding(
        qid = qid,
        question = question,
        finding = "unsupported_loop_field",
        details = paste0(
          "Loop and Merge placeholder field(s) could not be resolved: ",
          paste(unique(unsupported_fields), collapse = ", "),
          "."
        )
      )))
    }

    unsupported
  }) %>%
    unlist(recursive = FALSE) %>%
    bind_rows()

  if (nrow(findings) == 0) {
    return(empty_unsupported_structure_findings())
  }

  findings
}

#' Detect unresolved Loop and Merge field numbers
#' @keywords internal
#' @noRd
unsupported_loop_field_numbers <- function(question,
                                           looping_source,
                                           looping_qid,
                                           loop_fields) {
  if (length(loop_fields) == 0) {
    return(character())
  }

  loop_rows <- loop_rows_for_question(
    question = question,
    looping_source = looping_source,
    looping_qid = looping_qid
  )
  if (is.null(loop_rows) || length(loop_rows) == 0) {
    return(loop_fields)
  }

  loop_fields[!vapply(loop_fields, function(field_number) {
    all(vapply(loop_rows, function(loop_row) {
      field_value <- loop_row$fields[field_number]
      length(field_value) == 1 && !is.na(field_value) && field_value != ""
    }, logical(1)))
  }, logical(1))]
}

#' Build one Unsupported Structure Finding row
#' @keywords internal
#' @noRd
unsupported_structure_finding <- function(qid, question, finding, details) {
  question_type <- question_fact_question_type(question)

  tibble(
    qid = qid,
    type = scalar_character(question_type$type),
    selector = scalar_character(question_type$selector),
    sub_selector = scalar_character(question_type$sub_selector),
    finding = finding,
    details = details
  )
}
