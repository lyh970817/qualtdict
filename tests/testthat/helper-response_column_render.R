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

synthetic_timing_raw_metadata <- function() {
  new_raw_qualtrics_metadata(
    surveyID = "SV_TIMING",
    metadata = list(
      metadata = list(name = "Timing Survey"),
      questions = list(
        QID1 = list(
          questionName = "Q1",
          questionType = list(
            type = "Timing",
            selector = "PageTimer",
            subSelector = NULL
          ),
          questionText = "Timing",
          blocks = list(),
          columns = list(),
          choices = list(),
          subQuestions = list()
        )
      )
    ),
    description = list(
      blocks = list(
        BL_1 = list(
          Description = "Main Block",
          BlockElements = list(
            list(QuestionID = "QID1")
          ),
          Options = list(
            LoopingOptions = list(
              Static = NULL,
              QID = NULL
            )
          )
        )
      ),
      questions = list(
        QID1 = list(
          Validation = list(
            Settings = list(ContentType = NULL)
          )
        )
      )
    )
  )
}

synthetic_file_upload_raw_metadata <- function() {
  new_raw_qualtrics_metadata(
    surveyID = "SV_FILE_UPLOAD",
    metadata = list(
      metadata = list(name = "File Upload Survey"),
      questions = list(
        QID1 = list(
          questionName = "Q1",
          questionType = list(
            type = "FileUpload",
            selector = "FileUpload",
            subSelector = NULL
          ),
          questionText = "Upload a file",
          blocks = list(),
          columns = list(),
          choices = list(),
          subQuestions = list()
        )
      )
    ),
    description = list(
      blocks = list(
        BL_1 = list(
          Description = "Main Block",
          BlockElements = list(
            list(QuestionID = "QID1")
          ),
          Options = list(
            LoopingOptions = list(
              Static = NULL,
              QID = NULL
            )
          )
        )
      ),
      questions = list(
        QID1 = list(
          Validation = list(
            Settings = list(ContentType = NULL)
          )
        )
      )
    )
  )
}
