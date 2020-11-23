#' Compare two dictionaries and suggest potential matching variables
#'
#' Compare variables in two dictionaries based on either their question or
#' item text and labels and suggest potential (fuzzy) matches by comparing
#' each question or item text in \code{dict} to all the ones in
#' \code{reference_dict} and obtain the best match.
#' The results can be
#' used for
#' \code{\link[qualtdict]{dict_rename}} to ensure the same variables in
#' two different dictionaries have the same names.
#' @param dict A variable dictionary returned by
#' \code{\link[qualtdict]{dict_generate}}.
#' @param reference_dict Variable dictionary returned by
#' \code{\link[qualtdict]{dict_generate}}. The variable names in this
#' dictionary wil be used when the object returned by the function is used
#' for \code{\link[qualtdict]{dict_merge}} or \code{\link[qualtdict]{dict_rename}}
#' @param field String. Which field is used when comparing variables.
#'
#' @export
dict_compare <- function(dict,
                         reference_dict,
                         field = c("question", "item", "both")) {
  field <- match.arg(field)

  if (field == "both") {
    field <- c("question", "item")
  }

  texts <- do.call(paste_narm, as.list(dict[field]))
  texts_ref <- do.call(paste_narm, as.list(reference_dict[field]))

  # When field is "item", some texts could be empty due to no content in
  # item. Fill those texts with "question".
  if (all(field == "item")) {
    texts[texts == ""] <- dict[["question"]][texts == ""]
    texts_ref[texts_ref == ""] <- reference_dict[["question"]][texts_ref == ""]
  }

  # Get matching indices for identical matches
  match_is <- match(texts, texts_ref)

  # Select non-identical texts for fuzzyz matching
  texts_tofuzzy <- ifelse(texts %in% texts_ref, NA, texts) %>%
    discard(is.na)
  texts_ref_tofuzzy <- ifelse(texts_ref %in% texts, NA, texts_ref) %>%
    discard(is.na)

  # Get matching indices for fuzzy matches
  amatch_is <- amatch(texts_tofuzzy, texts_ref_tofuzzy, maxDist = 1000)

  # Get matching results
  texts_fuzzy_is <- get_match(amatch_is)[[1]]
  texts_ref_fuzzy_is <- get_match(amatch_is)[[2]]

  texts_is <- get_match(match_is)[[1]]
  texts_ref_is <- get_match(match_is)[[2]]

  texts_match <- c(
    rep(FALSE, times = length(texts_fuzzy_is)),
    rep(TRUE, times = length(texts_is))
  )

  labels <- get_labels(
    dict,
    c(texts_fuzzy_is, texts_is)
  )

  labels_ref <- get_labels(
    reference_dict,
    c(texts_ref_fuzzy_is, texts_ref_is)
  )

  label_match <- map2_lgl(labels, labels_ref, ~ identical(.x, .y))

  if (all(is.na(amatch_is)) && all(is.na(match_is))) {
    return(tibble())
  }
  else {
    tibble(
      name = dict[["name"]][c(texts_fuzzy_is, texts_is)],
      question = texts[c(texts_fuzzy_is, texts_is)],
      n_levels = map_dbl(labels, length),
      name_reference = reference_dict[["name"]][
        c(texts_ref_fuzzy_is, texts_ref_is)
      ],
      question_reference = texts_ref[c(
        texts_ref_fuzzy_is,
        texts_ref_is
      )],
      n_levels_ref = map_dbl(labels_ref, length),
      texts_match = texts_match,
      label_match = label_match
    ) %>%
      .[!duplicated(.), ]
    #     na.omit()
  }
}

get_match <- function(matches) {
  list(
    which(!is.na(matches)),
    discard(matches, is.na)
  )
}

get_labels <- function(dict, matches) {
  nms <- dict[["name"]][matches]
  map(nms, ~ dict %>%
    filter(.data[["name"]] == .x) %>%
    select(label) %>%
    unlist())
}
