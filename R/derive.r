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
data_derive <- function(data, dict) {
  formulae <- dict[["formula"]]

  derived_vars <- map2_df(
    formulae[!is.na(formulae)],
    dict[["name"]][!is.na(formulae)],
    function(formula, name) {
      fun <- eval(str2lang(formula))
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
