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
is_onetoone <- function(cols) {
  !any(map_dbl(which_not_onetoone(cols), nrow) > 0)
}

#' Convert NULL or a list of NULLs to NA
#' @param x NULL or a list of NULLs
#' @keywords internal
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
unique_expand <- function(x, y) {
  if (all(is.na(x))) {
    return(x)
  }

  y[y == ""] <- " "
  recode(y, !!!setNames(x, unique(y)))
}

survey_rename <- function(survey) {
  qid_cols_nosfx <- str_replace(colnames(survey), "(#[0-9])?_[0-9_]+", "")
  qid_cols_all <- make.unique(qid_cols_nosfx)
  colnames(survey) <- qid_cols_all

  return(survey)
}
#' `paste` but with seperator associated with NA removed
#' @keywords internal
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
unlist_nm <- function(list) {
  names <- names(list)
  v <- unlist(map(list, null_na)) %>%
    setNames(names)
  return(v)
}

#' Order a list by name
#' @keywords internal
order_name <- function(list) {
  list[order(as.numeric(str_extract(names(list), "[0-9]+")))]
}

#' Function factory for maximum five times of retries when failed
#' @param f A function
#' @keywords internal
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
fetch_survey2 <- retry(fetch_survey)

#' Retry version of `metadata`
#' @importFrom qualtRics metadata 
#' @keywords internal
metadata2 <- retry(metadata)

#' Retry version of `fetch_description`
#' @importFrom qualtRics fetch_description
#' @keywords internal
fetch_description2 <- retry(fetch_description)


#' Install Qualtrics credentials in your \code{.Renviron} file for repeated use
#'
#' @description This function adds your Qualtrics API key and base URL to your
#' \code{.Renviron} file so it can be called securely without being stored in
#' your code. After you have installed these two credentials, they can be
#' called any time with \code{Sys.getenv("QUALTRICS_API_KEY")} or
#' \code{Sys.getenv("QUALTRICS_BASE_URL")}. If you do not have an
#' \code{.Renviron} file, the function will create one for you. If you already
#' have an \code{.Renviron} file, the function will append the key to your
#' existing file, while making a backup of your original file for disaster
#' recovery purposes.
#' @param api_key The API key provided to you from Qualtrics formatted in quotes.
#' Learn more about Qualtrics API keys at \url{https://api.qualtrics.com/docs/}
#' @param base_url The institution-specific base URL for your Qualtrics account,
#' formatted in quotes. Find your base URL at \url{https://api.qualtrics.com/docs/}
#' @param install If TRUE, will install the key in your \code{.Renviron} file
#' for use in future sessions.  Defaults to FALSE (single session use).
#' @param overwrite If TRUE, will overwrite existing Qualtrics
#' credentials that you already have in your \code{.Renviron} file.
#' @examples
#' \dontrun{
#' qualtrics_api_credentials(
#'   api_key = "<YOUR-QUALTRICS_API_KEY>",
#'   base_url = "<YOUR-QUALTRICS_BASE_URL>",
#'   install = TRUE
#' )
#' # Reload your environment so you can use the credentials without restarting R
#' readRenviron("~/.Renviron")
#' # You can check it with:
#' Sys.getenv("QUALTRICS_API_KEY")
#'
#' # If you need to overwrite existing credentials:
#' qualtrics_api_credentials(
#'   api_key = "<YOUR-QUALTRICS_API_KEY>",
#'   base_url = "<YOUR-QUALTRICS_BASE_URL>",
#'   overwrite = TRUE,
#'   install = TRUE
#' )
#' # Reload your environment to use the credentials
#' }
#' @importFrom qualtRics qualtrics_api_credentials
#' @export qualtrics_api_credentials
#' @name qualtrics_api_credentials
NULL
