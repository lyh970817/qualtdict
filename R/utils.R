#' @importFrom stats setNames
#' @import dplyr
#' @import purrr
#' @import stringr
#' @importFrom tibble tibble as_tibble enframe
#' @importFrom magrittr %>%
#' @importFrom crul Async
#' @importFrom utils globalVariables

globalVariables(c(
  ".", "label", "level", "pair",
  "name", "easyname", "easyblock", "easyquestion",
  "get_pos_tags", "handle_pos_error", "stop_pos_tags"
))

#' Given a two-column dataframe find which row is not one-to-one
#' @param cols A dataframe with two columns
#' @importFrom rlang :=
#' @keywords internal
#' @noRd
which_not_onetoone <- function(cols) {
  which_not_oneto <- function(cols, from, to) {
    cols %>%
      group_by(.data[[from]]) %>%
      filter(length(unique(.data[[to]])) != 1) %>%
      summarize(!!to := unique(.data[[to]]), .groups = "keep")
  }
  names_cols <- colnames(cols)
  map(
    names_cols,
    ~ which_not_oneto(cols, from = .x, to = setdiff(names_cols, .x))
  )
}

#' Given a two-column dataframe determine which row is one-to-one (logical)
#' @param cols A dataframe with two columns
#' @keywords internal
#' @noRd
is_onetoone <- function(cols) {
  !any(map_dbl(which_not_onetoone(cols), nrow) > 0)
}

#' Convert NULL or a list of NULLs to NA
#' @param x NULL or a list of NULLs
#' @keywords internal
#' @noRd
null_na <- function(x) {
  # A list created with NULL values will have lengths of all 0
  if (is.null(x) ||
    all(map_dbl(x, length) == 0)) {
    NA
  } else {
    x
  }
}

#' Convert html special characeters
#' @param data A dataframe
#' @importFrom xml2 xml_text read_html
#' @keywords internal
#' @noRd
convert_html <- function(data) {
  unescape_html <- function(x) {
    map_chr(
      x,
      function(x) {
        if (is.na(x)) {
          NA
        } else {
          xml_text(
            suppressMessages(
              read_html(paste0("<x>", x, "</x>"))
            )
          )
        }
      }
    )
  }

  data %>%
    mutate_all(
      unescape_html
    )
}

#' Suppose x = f(unique(y)), find f(y)
#' @param x A character vector
#' @param y A character vector
#' @keywords internal
#' @noRd
unique_expand <- function(x, y) {
  if (all(is.na(x))) {
    return(x)
  }

  y[y == ""] <- " "
  recode(y, !!!setNames(x, unique(y)))
}

#' `paste` but with seperator associated with NA removed
#' @keywords internal
#' @noRd
paste_narm <- function(...) {
  args <- list(...)
  is_null_na <- map_lgl(args, ~ is.null(.x) | all(is.na(.x)))
  args[is_null_na] <- NULL

  sep <- list(...)$sep %>%
    ifelse(is.null(.), " ", .)
  do.call(paste, args)
}

#' `unlist` that preserve names
#' @keywords internal
#' @noRd
unlist_nm <- function(list) {
  names <- names(list)
  v <- unlist(map(list, null_na)) %>%
    setNames(names)
  return(v)
}

#' Order a list by name
#' @keywords internal
#' @noRd
order_name <- function(list) {
  list[order(as.numeric(str_extract(names(list), "[0-9]+")))]
}

#' Function factory for maximum five times of retries when failed
#' @param f A function
#' @keywords internal
#' @noRd
retry <- function(f) {
  function(...) {
    r <- NULL
    attempt <- 1
    while (is.null(r) && attempt <= 5) {
      # Don't cummulate count with endless attempts
      attempt <- attempt + 1
      try(
        r <- f(...)
      )
    }

    if (is.null(r)) {
      f(...)
    } else {
      return(r)
    }
  }
}

#' Retry version of `fetch_survey`
#' @importFrom qualtRics fetch_survey
#' @keywords internal
#' @noRd
fetch_survey2 <- retry(fetch_survey)

#' Retry version of `metadata`
#' @importFrom qualtRics metadata
#' @keywords internal
#' @noRd
metadata2 <- retry(metadata)

#' Retry version of `fetch_description`
#' @importFrom qualtRics fetch_description
#' @keywords internal
#' @noRd
fetch_description2 <- retry(fetch_description)

#' @importFrom qualtRics qualtrics_api_credentials
#' @export
qualtRics::qualtrics_api_credentials
