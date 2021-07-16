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
#' @param ignore_punctuation Logical Whether to ignore punctuations in
#' comparison
#' @param ... Other arguments passed to
#' \code{\link[stringdist]{amatch}} to configure fuzzy matching.
#'
#' @export
dict_compare <- function(dict,
                         reference_dict,
                         field = c("all", "question", "item"),
                         ignore_punctuation = TRUE,
                         ...) {
  field <- match.arg(field)

  get_texts <- function(dict, field) {
    apply(dict, 1, function(row) {
      item <- row[["item"]]
      type <- row[["type"]]
      selector <- row[["selector"]]
      sub_selector <- row[["sub_selector"]]
      if (field == "all") {
        fields <- get_fields(item, type, selector, sub_selector)
      }
      do.call(paste_narm, as.list(row[fields]))
    }) %>%
      setNames(dict[["name"]])
  }

  texts <- get_texts(dict, field)
  texts_ref <- get_texts(reference_dict, field)

  # When field is "item", some texts could be empty due to no content in
  # item. Fill those texts with "question".
  texts[texts == ""] <- dict[["question"]][texts == ""]
  texts_ref[texts_ref == ""] <- reference_dict[["question"]][texts_ref == ""]

  # Remove the repeated rows referring to the same variable
  texts <- texts[!duplicated(names(texts))]
  texts_ref <- texts_ref[!duplicated(names(texts_ref))]

  if (ignore_punctuation) {
    # Remove punctuations
    texts <- str_remove_all("texts", "[[:punct:]]")
    texts_ref <- str_remove_all("texts_ref", "[[:punct:]]")
  }

  # Get matching indices for identical matches
  match_is <- match(texts, texts_ref)
  # Get matching results
  texts_is <- get_match(match_is)[[1]]

  # Get matching indices for fuzzy matches
  amatch_is <- amatch(texts, texts_ref, ...)

  texts_fuzzy_is <- get_match(amatch_is)[[1]]
  texts_ref_fuzzy_is <- get_match(amatch_is)[[2]]

  texts_match <- ifelse(texts_fuzzy_is %in% texts_is, TRUE, FALSE)

  matched_texts <- texts[texts_fuzzy_is]
  matched_texts_ref <- texts_ref[texts_ref_fuzzy_is]

  matched_names <- names(matched_texts)
  matched_names_ref <- names(matched_texts_ref)

  labels <- get_labels(
    dict, matched_names
  )

  labels_ref <- get_labels(
    reference_dict,
    matched_names_ref
  )

  label_match <- map2_lgl(labels, labels_ref, ~ identical(.x, .y))

  if (all(is.na(amatch_is)) && all(is.na(match_is))) {
    tibble(
      name = character(),
      text = character(),
      n_levels = numeric(),
      name_reference = character(),
      text_reference = character(),
      n_levels_ref = numeric(),
      texts_match = character(),
      label_match = character()
   )
  }
  else {
    tibble(
      name = matched_names,
      text = matched_texts,
      n_levels = map_dbl(labels, length),
      name_reference = matched_names_ref,
      text_reference = matched_texts_ref,
      n_levels_ref = map_dbl(labels_ref, length),
      texts_match = texts_match,
      label_match = label_match
    ) %>%
      arrange(texts_match, label_match)
  }
}

get_match <- function(matches) {
  list(
    which(!is.na(matches)),
    discard(matches, is.na)
  )
}

get_labels <- function(dict, matches) {
  map(matches, ~ dict %>%
    filter(.data[["name"]] == .x) %>%
    select(label) %>%
    unlist())
}

get_fields <- function(item,
                       type,
                       selector,
                       sub_selector) {
  if (type == "MC") {
    if (selector == "MACOL" || selector == "MAVR" || selector == "MAHR") {
      fields <- c("question", "label")
    }
    else if (selector == "SAVR" || selector == "SACOL" || selector == "DL" || selector == "SAHR") {
      fields <- "question"
    }
  }
  else if (type == "Matrix") {
    if (selector == "Likert") {
      if (sub_selector == "MultipleAnswer") {
        fields <- c("question", "item", "label")
      }
      else if (sub_selector == "DL") {
        fields <- c("question", "item")
      }
      else if (sub_selector == "SingleAnswer") {
        if (is.null(item)) {
          fields <- c("question", "label")
        }
        else {
          fields <- c("question", "item")
        }
      }
    }
    else if (selector == "TE") {
      fields <- c("question", "item", "label")
    }
    else if (selector == "Profile" || selector == "Bipolar") {
      fields <- c("question", "item")
    }
  }
  else if (type == "Slider" && selector == "HSLIDER") {
    if (is.null(item)) {
      fields <- c("question", "label")
    }
    else {
      fields <- c("question", "item")
    }
  }
  else if (type == "Slider" && selector == "HBAR") {
    fields <- c("question", "label")
  }
  else if (type == "Slider" && selector == "STAR") {
    fields <- c("question", "label")
  }
  else if (type == "TE" && selector == "FORM") {
    fields <- c("question", "label")
  }
  else if (type == "TE" &&
    (selector == "SL" ||
      selector == "ML" ||
      selector == "ESTB")) {
    fields <- c("question")
  }
  else if (type == "SBS") {
    fields <- c("question", "item")
  }
  else if (type == "CS") {
    if (selector == "HR") {
      if (sub_selector == "TX") {
        if (is.null(item)) {
          fields <- c("question", "label")
        }
        else {
          fields <- c("question", "item")
        }
      }
    }
  }

  return(fields)
}
