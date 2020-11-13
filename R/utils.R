#' @import dplyr
#' @import tibble
#' @import tidyr
#' @import purrr
#' @import stringr
#' @import stringi
#' @import stringdist
#' @import slowraker
#' @import qualtRics
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
