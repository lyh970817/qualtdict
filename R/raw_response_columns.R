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
  details[text_analysis] <- paste0(
    "Qualtrics text-analysis sidecar column; ",
    "not represented as a question dictionary row."
  )

  embedded_data <- is.na(classification) &
    grepl("^[A-Za-z][A-Za-z0-9_]*$", raw_columns) &
    !grepl("^QID[0-9]+", raw_columns)
  classification[embedded_data] <- "embedded_data"
  details[embedded_data] <- paste0(
    "Embedded-data or user-defined metadata column; ",
    "not represented as a question dictionary row."
  )

  tibble(
    raw_column = raw_columns,
    classification = classification,
    details = details
  )
}
