#' Multiple choice Response Column ID renderers
#' @noRd
response_column_mc_renderer_table <- function() {
  list(
    MACOL = list(TX = render_macol_response_column_id_with_level_suffix),
    MAVR = list(TX = render_mavr_response_column_id_with_level_suffix),
    MAHR = list(TX = render_response_column_id_with_level_suffix),
    MSB = render_response_column_id_with_level_suffix,
    SAVR = list(TX = render_response_column_id_repeated_by_level),
    SACOL = list(TX = render_response_column_id_repeated_by_level),
    DL = render_response_column_id_repeated_by_level,
    SAHR = list(TX = render_response_column_id_repeated_by_level),
    SB = render_response_column_id_repeated_by_level,
    NPS = render_response_column_id_repeated_by_level
  )
}

#' Add Qualtrics text-entry IDs to multiple choice Response Column IDs
#' @noRd
add_text_mc <- function(response_column_id, level) {
  # For computed Response Column IDs for multiple choice questions allowing
  # for only one choice with text options, add the Qualtrics internal index to
  # the end of the text-entry Response Column IDs.
  text_pos <- grep("TEXT", level, fixed = TRUE)
  if (length(text_pos) > 0 && !is.null(names(level))) {
    for (pos in text_pos) {
      level_suffix <- paste0("_", level[[pos]])
      if (endsWith(response_column_id[[pos]], level_suffix)) {
        prefix <- substr(
          response_column_id[[pos]],
          1,
          nchar(response_column_id[[pos]]) - nchar(level_suffix)
        )
      } else {
        prefix <- response_column_id[[pos]]
      }
      response_column_id[[pos]] <- paste(prefix, names(level)[[pos]], sep = "_")
    }
  }
  response_column_id
}

#' Resolve multiple choice IDs for Response Column ID rendering
#' @noRd
mc_choice_ids <- function(level) {
  choice_ids <- names(level)
  if (is.null(choice_ids)) {
    return(level)
  }

  if (all(grepl("^x[0-9]+(_TEXT)?$", choice_ids))) {
    return(choice_ids)
  }

  response_choice_ids <- unname(level)
  text_choice_ids <- grepl("TEXT$", level)
  response_choice_ids[text_choice_ids] <- choice_ids[text_choice_ids]

  response_choice_ids
}

#' Resolve multiple choice recodes for Response Column ID rendering
#' @noRd
mc_recode_ids <- function(level) {
  choice_ids <- names(level)
  if (is.null(choice_ids)) {
    return(level)
  }

  response_choice_ids <- unname(level)
  text_choice_ids <- grepl("TEXT$", level)
  response_choice_ids[text_choice_ids] <- choice_ids[text_choice_ids]

  response_choice_ids
}

#' Render Response Column IDs with level suffixes
#' @noRd
render_response_column_id_with_level_suffix <- function(context) {
  level <- context$render_facts$level
  # Add recode values to the end of the Response Column IDs and then add the
  # Qualtrics internal index to text-entry Response Column IDs for multiple
  # choice questions allowing for only one choice.
  add_text_mc(
    paste(context$base_response_column_id, mc_recode_ids(level), sep = "_"),
    level
  )
}

#' Render MACOL Response Column IDs with level suffixes
#' @noRd
render_macol_response_column_id_with_level_suffix <- function(context) {
  level <- context$render_facts$level
  if (length(level) == 0) {
    return(context$base_response_column_id)
  }

  paste(context$base_response_column_id, mc_recode_ids(level), sep = "_")
}

#' Render MAVR Response Column IDs with level suffixes
#' @noRd
render_mavr_response_column_id_with_level_suffix <- function(context) {
  level <- context$render_facts$level
  if (length(level) == 0) {
    return(context$base_response_column_id)
  }

  paste(context$base_response_column_id, mc_recode_ids(level), sep = "_")
}
