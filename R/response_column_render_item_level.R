#' Matrix Response Column ID renderers
#' @noRd
response_column_matrix_renderer_table <- function() {
  item_and_level <- render_response_column_id_with_item_and_level_suffixes
  item_suffix_repeated <-
    render_response_column_id_with_item_suffix_repeated_by_level

  list(
    Likert = response_column_likert_renderer_table(),
    TE = list(
      Short = item_and_level,
      Medium = item_and_level,
      Long = item_and_level
    ),
    Profile = list(
      SingleAnswer = item_suffix_repeated,
      DL = item_suffix_repeated
    ),
    Bipolar = item_suffix_repeated,
    RO = item_and_level,
    MaxDiff = item_suffix_repeated,
    CS = list(WTB = item_and_level)
  )
}

#' Likert matrix Response Column ID renderers
#' @noRd
response_column_likert_renderer_table <- function() {
  list(
    MultipleAnswer = render_response_column_id_with_item_and_level_suffixes,
    DL = render_response_column_id_with_item_suffix_repeated_by_level,
    SingleAnswer = render_response_column_id_with_item_or_level_suffix,
    DND = render_response_column_id_with_item_or_level_suffix,
    SACV = render_response_column_id_with_item_or_level_suffix,
    SACH = render_response_column_id_with_item_or_level_suffix,
    SACCOL = render_response_column_id_with_item_or_level_suffix
  )
}

#' Slider Response Column ID renderers
#' @noRd
response_column_slider_renderer_table <- function() {
  list(
    HSLIDER = render_response_column_id_with_level_suffix,
    HBAR = render_response_column_id_with_level_suffix,
    STAR = render_response_column_id_with_level_suffix
  )
}

#' Constant sum Response Column ID renderers
#' @noRd
response_column_cs_renderer_table <- function() {
  list(
    HR = list(TX = render_response_column_id_with_named_label_suffix),
    VRTL = list(TX = render_response_column_id_with_item_or_level_suffix),
    HBAR = render_response_column_id_with_item_or_level_suffix,
    HSLIDER = render_response_column_id_with_item_or_level_suffix
  )
}
