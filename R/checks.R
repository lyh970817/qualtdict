#' Check that an argument is a single non-missing logical value
#' @importFrom rlang abort
#' @importFrom glue glue
#' @noRd
checkarg_isboolean <-
  function(arg) {
    test <-
      is.logical(arg) && length(arg) == 1 && !is.na(arg)

    if (!test) {
      rlang::abort(
        c(
          glue::glue("Error in argument '{deparse(substitute(arg))}':"),
          "Argument must be a single `TRUE` or `FALSE`."
        )
      )
    }
  }

#' Check that an argument is a single character string
#' @importFrom rlang abort
#' @importFrom glue glue
#' @noRd
checkarg_isstring <-
  function(arg, null_okay = TRUE) {
    if (null_okay && is.null(arg)) {
      return()
    }

    test <-
      is.character(arg) && length(arg) == 1

    if (!test) {
      rlang::abort(
        c(
          glue::glue("Error in argument '{deparse(substitute(arg))}':"),
          "Argument must be a single string."
        )
      )
    }
  }

#' Check that an argument is a character vector with no missing values
#' @importFrom rlang abort
#' @importFrom glue glue
#' @noRd
checkarg_ischaracter <-
  function(arg, null_okay = TRUE) {
    if (null_okay && is.null(arg)) {
      return()
    }

    test_char <-
      is.character(arg)

    if (!test_char) {
      rlang::abort(
        c(
          glue::glue("Error in argument '{deparse(substitute(arg))}':"),
          "Argument must be a character vector."
        )
      )
    }

    test_missing <-
      !anyNA(arg)

    if (!test_missing) {
      rlang::abort(
        c(
          glue::glue("Error in argument '{deparse(substitute(arg))}':"),
          "Argument must not have missing values."
        )
      )
    }
  }

#' Check the deprecated variable name selector argument
#' @importFrom rlang abort
#' @importFrom glue glue
#' @noRd
checkarg_isname <- function(arg) {
  test <-
    length(arg) == 1 &&
    !is.null(arg) &&
    arg %in% c("question_name", "semantic_name", "easy_name")

  if (!test) {
    rlang::abort(
      c(
        glue::glue("Error in argument '{deparse(substitute(arg))}':"),
        paste0(
          "Argument must be one of ",
          "`c(\"question_name\", \"semantic_name\")`."
        )
      )
    )
  }
}

#' Check the variable name selector argument
#' @importFrom rlang abort
#' @importFrom glue glue
#' @noRd
checkarg_isvariable_name <- function(arg) {
  test <-
    length(arg) == 1 &&
    !is.null(arg) &&
    arg %in% c("question_name", "semantic_name")

  if (!test) {
    rlang::abort(
      c(
        glue::glue("Error in argument '{deparse(substitute(arg))}':"),
        paste0(
          "Argument must be one of ",
          "`c(\"question_name\", \"semantic_name\")`."
        )
      )
    )
  }
}

#' Check that an argument is a function
#' @importFrom rlang abort
#' @importFrom glue glue
#' @noRd
checkarg_isfunction <- function(arg, null_okay = TRUE) {
  if (null_okay && is.null(arg)) {
    return()
  }

  test <-
    is.function(arg)

  if (!test) {
    rlang::abort(
      c(
        glue::glue("Error in argument '{deparse(substitute(arg))}':"),
        "Argument must be a function."
      )
    )
  }
}


#' Check that an argument is a qualtdict object
#' @importFrom rlang abort
#' @importFrom glue glue
#' @noRd
checkarg_isqualtdict <- function(arg) {
  test <- inherits(arg, "qualtdict")

  if (!test) {
    rlang::abort(
      c(
        glue::glue("Error in argument '{deparse(substitute(arg))}':"),
        "Argument must be a `qualtdict` from `dict_generate`."
      )
    )
  }
}
