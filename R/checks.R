#' Is boolean (length-1 logical)
#' @importFrom rlang abort
#' @importFrom glue glue
#' @keywords internal
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

#' Is string (length-1 character)
#' @importFrom rlang abort
#' @importFrom glue glue
#' @keywords internal
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

#' Is character vector with no missing values:
#' @importFrom rlang abort
#' @importFrom glue glue
#' @keywords internal
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

#' Is deprecated variable_name alias
#' @importFrom rlang abort
#' @importFrom glue glue
#' @keywords internal
#' @noRd
checkarg_isname <- function(arg) {
  test <-
    length(arg) == 1 && !is.null(arg) &&
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

#' Is one of `c("question_name", "semantic_name")`
#' @importFrom rlang abort
#' @importFrom glue glue
#' @keywords internal
#' @noRd
checkarg_isvariable_name <- function(arg) {
  test <-
    length(arg) == 1 && !is.null(arg) &&
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

#' Is function
#' @importFrom rlang abort
#' @importFrom glue glue
#' @keywords internal
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


#' Is a `qualtdict`
#' @importFrom rlang abort
#' @importFrom glue glue
#' @keywords internal
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
