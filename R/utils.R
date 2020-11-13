#' @import dplyr
#' @import tibble
#' @import tidyr
#' @import purrr
#' @import stringr
#' @import stringi
#' @import stringdist
#' @import slowraker
#' @import sjlabelled
#' @import googlesheets4
#' @importFrom magrittr %>%

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

is_onetoone <- function(cols) {
  !any(map_dbl(which_not_onetoone(cols), nrow) > 0)
}

null_na <- function(x) {
  if (is.null(x)) {
    NA
  } else {
    x
  }
}

remove_format <- function(data, skip) {
  chr_cols <- discard(colnames(data), ~ . %in% c(skip))
  mutate_at(
    data, vars(-question_name),
    ~ str_remove_all(., "(<[^>]+>|\\n)+") %>%
      str_remove_all("Selected Choice - ") %>%
      str_squish()
  ) %>%
    mutate_at(
      vars(all_of(chr_cols)),
      ~ str_remove_all(., "\\(.*\\)") %>%
        str_squish()
    )
}

unique_expand <- function(x, ...) {
  # Suppose x = unique(x') and a one-to-one mapping between x and
  # unique(paste(...)),
  # expand x to the same length of ...
  # and preserve the mapping
  if (all(is.na(x))) {
    return(x)
  }
  y <- paste(...)
  y[y == ""] <- " "
  recode(y, !!!setNames(x, unique(y)))
}

survey_rename <- function(survey) {
  qid_cols_nosfx <- str_replace(colnames(survey), "(#[0-9])?_[0-9_]+", "")
  qid_cols_all <- make.unique(qid_cols_nosfx)
  colnames(survey) <- qid_cols_all

  return(survey)
}

# get_varname <- function(dict) {
#   if ("easyname" %in% colnames(dict)) {
#     return("easyname")
#   }
#   else if ("question_name" %in% colnames(dict)) {
#     return("easyname")
#   }
# }

match_all <- function(x, y) {
  unlist(map(y, ~ which(x == .x)))
}


paste_narm <- function(...) {
  args <- list(...)
  is_null_na <- map_lgl(args, ~ is.null(.x) || is.na(.x))
  args[is_null_na] <- NULL

  sep <- list(...)$sep %>%
    ifelse(is.null(.), " ", .)
  do.call(paste, args)
}

or <- function(x) {
  if (length(x) > 1) {
    lgl <- do.call(`|`, x)
  }
  else {
    lgl <- unlist(x)
  }

  lgl[is.na(lgl)] <- FALSE
  return(lgl)
}

split_orderd <- function(x, f) {
  split(x, f = factor(f, level = unique(f)))
}

order_name <- function(list) {
  list[order(as.numeric(str_extract(names(list), "[0-9]+")))]
}

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
#'
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
