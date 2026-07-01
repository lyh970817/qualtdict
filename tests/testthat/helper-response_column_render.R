if (!exists("normalise_qualtrics_metadata", mode = "function")) {
  load_all <- getExportedValue("pkgload", "load_all")
  load_all(".", export_all = TRUE, helpers = FALSE, quiet = TRUE)
}

if (identical(Sys.getenv("NOT_CRAN"), "")) {
  Sys.setenv(NOT_CRAN = "true")
}

render_response_column_fixture <- function(
  raw_metadata,
  qid,
  base_response_column_id = qid
) {
  question <- normalise_qualtrics_metadata(raw_metadata)$questions[[qid]]
  render_response_columns(question, base_response_column_id)
}

compact_response_column_render <- function(rendered) {
  rendered |>
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        ~ unname(as.character(.x))
      )
    )
}

expect_renderer_rows_aligned <- function(rendered) {
  expect_true(all(vapply(rendered, length, integer(1)) == nrow(rendered)))
}
