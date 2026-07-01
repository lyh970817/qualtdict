#' @importFrom stats setNames
#' @importFrom dplyr bind_cols bind_rows distinct everything filter group_by
#' @importFrom dplyr mutate mutate_all n_distinct recode reframe rename select
#' @importFrom dplyr summarize ungroup
#' @importFrom purrr discard imap imap_chr map map_chr map_df map_dbl map_lgl
#' @importFrom purrr map2 map2_chr map2_df modify
#' @importFrom stringr fixed str_extract str_match str_match_all str_remove
#' @importFrom stringr str_remove_all str_replace str_replace_all str_split
#' @importFrom tibble tibble as_tibble enframe
#' @importFrom crul Async
#' @importFrom utils globalVariables

globalVariables(c(
  ".",
  "label",
  "level",
  "pair",
  "name",
  "variable_name",
  "semantic_name",
  "semantic_block",
  "semantic_question",
  "response_column_id",
  "original_candidate",
  "repaired_candidate",
  "reason"
))

#' Create a progress bar when output is not quiet
#' @noRd
new_progress_bar <- function(total, quiet = TRUE) {
  if (quiet || total == 0) {
    return(NULL)
  }

  utils::txtProgressBar(min = 0, max = total, style = 3)
}

#' Advance a progress bar when present
#' @noRd
tick_progress_bar <- function(progress_bar, value) {
  if (!is.null(progress_bar)) {
    utils::setTxtProgressBar(progress_bar, value)
  }
}

#' Close a progress bar when present
#' @noRd
close_progress_bar <- function(progress_bar) {
  if (!is.null(progress_bar)) {
    close(progress_bar)
  }
}

#' Find rows that violate a one-to-one mapping
#' @param cols A dataframe with two columns
#' @importFrom rlang := .data
#' @noRd
which_not_onetoone <- function(cols) {
  which_not_oneto <- function(cols, from, to) {
    cols |>
      group_by(.data[[from]]) |>
      filter(length(unique(.data[[to]])) != 1) |>
      reframe(!!to := unique(.data[[to]]))
  }
  names_cols <- colnames(cols)
  map(
    names_cols,
    ~ which_not_oneto(cols, from = .x, to = setdiff(names_cols, .x))
  )
}

#' Return whether two columns form a one-to-one mapping
#' @param cols A dataframe with two columns
#' @noRd
is_onetoone <- function(cols) {
  !any(map_dbl(which_not_onetoone(cols), nrow) > 0)
}

#' Convert NULL or a list of NULLs to NA
#' @param x NULL or a list of NULLs
#' @noRd
null_na <- function(x) {
  # A list created with NULL values will have lengths of all 0
  if (
    is.null(x) ||
      all(map_dbl(x, length) == 0)
  ) {
    NA
  } else {
    x
  }
}

#' Convert HTML entities to text
#' @param data A dataframe
#' @importFrom xml2 xml_text read_html
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

  data |>
    mutate_all(
      unescape_html
    )
}

#' Expand values aligned with unique keys back to the original keys
#' @param x A character vector
#' @param y A character vector
#' @noRd
unique_expand <- function(x, y) {
  if (all(is.na(x))) {
    return(x)
  }

  y[y == ""] <- " "
  recode(y, !!!setNames(x, unique(y)))
}

#' Paste values after removing NULL or all-NA arguments
#' @noRd
paste_narm <- function(...) {
  args <- list(...)
  is_null_na <- map_lgl(args, ~ is.null(.x) | all(is.na(.x)))
  args[is_null_na] <- NULL

  sep <- list(...)$sep
  sep <- ifelse(is.null(sep), " ", sep)
  do.call(paste, args)
}

#' Unlist values while preserving names
#' @noRd
unlist_nm <- function(list) {
  if (length(list) == 0) {
    return(character())
  }

  names <- names(list)
  v <- unlist(map(list, null_na)) |>
    setNames(names)
  return(v)
}

#' Order a list by name
#' @noRd
order_name <- function(list) {
  list[order(as.numeric(str_extract(names(list), "[0-9]+")))]
}

#' Wrap a function with up to five retry attempts
#' @param f A function
#' @noRd
retry <- function(f) {
  function(...) {
    r <- NULL
    attempt <- 1
    while (is.null(r) && attempt <= 5) {
      # Don't cummulate count with endless attempts
      attempt <- attempt + 1
      try_result <- try(f(...))
      if (!inherits(try_result, "try-error")) {
        r <- try_result
      }
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
#' @noRd
fetch_survey2 <- retry(fetch_survey)

#' Retry version of `metadata`
#' @importFrom qualtRics metadata
#' @noRd
metadata2 <- retry(metadata)

#' Retry version of `fetch_description`
#' @importFrom qualtRics fetch_description
#' @noRd
fetch_description2 <- retry(fetch_description)

#' @importFrom qualtRics qualtrics_api_credentials
#' @export
qualtRics::qualtrics_api_credentials
