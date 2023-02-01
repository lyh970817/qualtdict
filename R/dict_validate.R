#' Check for potential mistakes in the dictionary
#'
#' Check for potential mistakes in the dictionary: 1) Whether the variable
#' names are unique; 2) For each variable, whether there is a unique
#' mapping between level and label and whether the levels are consecutive;
#' and extract unique level-label pairings.
#'
#' @param dict A variable dictionary returned by
#' \code{\link[qualtdict]{dict_generate}}.
#'
#' @return
#' If no mistake found in the dictionary, a dataframe containing the
#' unique level-label pairings. If mistakes found in the dictionary,
#' a list containing the above dataframe and a list with components
#' \itemize{
#'   \item non_unique_names  - A dataframe containing qids with non-unique
#' names and their names
#'   \item mistake - A dataframe containing qids with one of the following
#'   mistakes and mistake code: not one-to-one mapping between labels and
#'   levels (1), levels not increasing in step of 1 (2), duplicated levels
#'   (3) and duplicated labels (4).
#' }
#'
#' @examples
#' \dontrun{
#'
#' # Create a dictionary
#' mydict <- dict_generate("SV_4YyAHbAxpdbzacl",
#'   name = "easy_name",
#'   block_pattern = block_pattern,
#'   block_sep = ".",
#'   split_by_block = FALSE
#' )
#'
#'
#' # Validate the dictionary
#' validation <- dict_validate(mydict)
#' }
#'
#' @export
dict_validate <- function(dict) {
  checkarg_isqualtdict(dict)
  message("Validating dictionary...")

  split_dict <- split(dict, factor(dict$qid))

  level_label_pairs <- split_dict %>%
    map(select, label, level) %>%
    enframe(value = "pair") %>%
    group_by(pair) %>%
    summarize(qid = list(name), .groups = "drop")

  error_list <- list()
  non_unique_names <- check_names(dict)
  if (any(map_dbl(non_unique_names, nrow) > 0)) {
    message("Variables don't have unique names.")
    error_list$non_unique_names <- non_unique_names
  }

  mistake <- check_json(split_dict)

  if (nrow(mistake) > 0) {
    message("There are variables with potential incorrect level-label codings.")
    error_list$mistake <- mistake
  }

  if (length(error_list) == 0) {
    return(level_label_pairs)
  } else {
    return(list(
      errors = error_list,
      level_label_pairs = level_label_pairs
    ))
  }
}

check_names <- function(dict) {
  cols <- dict[c("qid", "name")]
  which_not_onetoone(cols)
}

check_item <- function(dat, qid) {
  item_name <- dat[dat$qid == qid, "name"]
  cols <- dat[c("label", "level")]

  # Here recode is sometimes "none" and will cause a warning
  col2_pos <- suppressWarnings(
    as.numeric(cols[[2]]) %>%
      subset(. >= 0)
  )

  has_mistake <- c(
    # Check correspondence
    !is_onetoone(cols),
    # Check constant step == 1
    !(all(diff(sort(col2_pos)) == 1) | length(diff(col2_pos)) == 0),
    # Check duplication
    any(duplicated(cols[[1]])),
    any(duplicated(cols[[2]]))
  )

  if (any(has_mistake)) {
    bind_cols(
      tibble(
        qid = qid,
        item_name,
        mistake = paste(which(has_mistake), collapse = "")
      ),
      cols
    )
  }
}

check_json <- function(split_jsons) {
  mistakes <- imap(split_jsons, check_item) %>%
    bind_rows()
  if (nrow(mistakes) > 0) {
    return(mistakes)
  } else {
    return(tibble())
  }
}
