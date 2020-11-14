#' Upload dictionary to a googlesheet with \code{surveyID} and
#' \code{survey_name} metadata kept
#'
#' Upload dictionary to a googlesheet. \code{surveyID} and
#' \code{survey_name} are added to the sheet.
#' @param dict A variable dictionary returned by
#' \code{\link[qualtdict]{dict_generate}}.
#' @param url String. An url for a googlesheet page.
#' @param surveyID Logical. Whether to write \code{surveyID} to the
#' googlesheet.
#'
#' @export
dict_upload <- function(dict, url, surveyID = FALSE) {

  dict <- bind_rows(tibble_row(), dict)
  dict[1, "qid"] <- "surveyName"
  dict[1, "question"] <- attr(dict, "surveyName")

  if (surveyID) {
    dict <- bind_rows(tibble_row(), dict)
    dict[2, "qid"] <- "surveyID"
    dict[2, "question"] <- attr(dict, "surveyID")
  }
  write_sheet(dict, url)
  invisible(dict)
}

#' Download dictionary from a googlesheet with \code{surveyID} and
#' \code{survey_name} metadata
#'
#' Download dictionary to a googlesheet. \code{surveyID} and
#' \code{survey_name} are added to the dictionary as attirbutes.
#' @param url String. An url for a googlesheet page.
#'
#' @export
dict_download <- function(url) {
  dict <- read_sheet(url)
  attr(dict, "surveyName") <- dict[1, "question"]
  if (dict[2, "qid"] == "surveyID") {
    attr(dict, "surveyID") <- dict[2, "question"]
  }
  dict
}
