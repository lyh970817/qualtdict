#' Display block Response Column ID renderers
#' @noRd
response_column_display_renderer_table <- function() {
  list(
    TB = render_no_response_column_ids,
    PTB = render_no_response_column_ids,
    FLB = render_no_response_column_ids,
    GRB = list(
      WTXB = render_no_response_column_ids,
      WOTXB = render_no_response_column_ids
    )
  )
}

#' Render no Response Column IDs for display text questions
#' @noRd
render_no_response_column_ids <- function(context) {
  character()
}

#' Render timing question Response Column IDs
#' @noRd
render_timing_response_column_ids <- function(context) {
  paste0(
    context$base_response_column_id,
    c(
      "_FIRST_CLICK",
      "_LAST_CLICK",
      "_PAGE_SUBMIT",
      "_CLICK_COUNT"
    )
  )
}

#' Render file-upload Response Column IDs
#' @noRd
render_file_upload_response_column_ids <- function(context) {
  paste0(
    context$base_response_column_id,
    c(
      "_FILE_ID",
      "_FILE_NAME",
      "_FILE_SIZE",
      "_FILE_TYPE"
    )
  )
}

#' Render fallback Response Column IDs for unsupported question shapes
#' @noRd
render_unsupported_response_column_ids <- function(context) {
  warn_msg <- paste0(
    context$base_response_column_id,
    " uses a question type without a specific response-column renderer; ",
    "falling back to the Base Response Column ID."
  )
  warning(warn_msg)
  context$base_response_column_id
}
