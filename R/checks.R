#' Is boolean (length-1 logical)
#' @importFrom rlang abort
#' @importFrom glue glue
#' @keywords internal
checkarg_isboolean <-
  function(arg) {
    test <-
      is.logical(arg) && !is.na(arg) && length(arg) == 1

    if (!test) {
      rlang::abort(
        c(
          glue::glue("Error in argument '{deparse(substitute(arg))}':"),
          "Argument must be a single `TRUE` or `FALSE`."
        )
      )
    }
  }

#' Is string (length-1 character)
#' @importFrom rlang abort
#' @importFrom glue glue
#' @keywords internal
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

#' Is character vector with no missing values:
#' @importFrom rlang abort
#' @importFrom glue glue
#' @keywords internal
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
      all(!is.na(arg))

    if (!test_missing) {
      rlang::abort(
        c(
          glue::glue("Error in argument '{deparse(substitute(arg))}':"),
          "Argument must not have missing values."
        )
      )
    }
  }

#' Is one of `c("question_name", "easy_name")`
#' @importFrom rlang abort
#' @importFrom glue glue
#' @keywords internal
checkarg_isname <- function(arg) {
  test <-
    length(arg) == 1 && !is.null(arg) &&
      arg %in% c("question_name", "easy_name")

  if (!test) {
    rlang::abort(
      c(
        glue::glue("Error in argument '{deparse(substitute(arg))}':"),
        "Argument must be one of `c(\"question_name\", \"easy_name\")`."
      )
    )
  }
}

#' Is one of `c("question_name", "easy_name")`
#' @importFrom rlang abort
#' @importFrom glue glue
#' @keywords internal
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


#' Is a `qualtdict`
#' @importFrom rlang abort
#' @importFrom glue glue
#' @keywords internal
checkarg_isqualtdict <- function(arg) {
  test <- any(class(arg) == "qualtdict")

  if (!test) {
    rlang::abort(
      c(
        glue::glue("Error in argument '{deparse(substitute(arg))}':"),
        "Argument must be a `qualtdict` from `dict_generate`."
      )
    )
  }
}
