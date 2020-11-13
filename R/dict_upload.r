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

#' Add a derived variable to the dictionary
#'
#' Add a derived variable to the dictionary. An additional \code{formula}
#' column is created to store a function that deriives this variable.
#' @param dict A variable dictionary returned by
#' \code{\link[qualtdict]{dict_generate}}.
#' @param var String. Name of the derived variable to be added.
#' @param fun Function. A function that takes a survey data frame as input
#' and returns the value of the derived variable.
#'
#' @export
add_derived <- function(dict, var, fun) {
  var_name <- get_varname(dict)

  dict <- bind_rows(dict, tibble_row())
  dict[nrows(dict), var_name] <- var
  dict[nrows(dict), "formula"] <- deparse1(fun)

  dict
}

#' Update a survey data frame by adding derived variables from the
#' dictionary
#'
#' Add values of derived variables from a dictionary to the survey data
#' frame.
#' @param data Data frame. A survey data frame returned by
#' \code{\link[qualtdict]{get_survey_data}}
#' @param dict A variable dictionary returned by
#' \code{\link[qualtdict]{dict_generate}}.
#'
#' @export
data_update <- function(data, dict) {
  formulae <- dict[["formula"]]

  derived_vars <- map2_df(
    formulae[!is.na(formulae)],
    dict[["name"]][!is.na(formulae)],
    function(formula, name) {
      fun <- eval(str2lang(tl))
      data[name] <- tryCatch(,
        fun(data),
        error = function(cond) {
          message("An error has occurred when deriving variable ", paste0("'", dv, "'"), "in ", questionnaire)
          message(paste0(cond))
          return(NULL)
        }
      )
    }
  )

  bind_cols(data, derived_vars)
}
