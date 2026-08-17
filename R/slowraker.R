#' Calculate keyword scores for candidate word groups
#' @noRd
calc_keyword_scores <- function(cand_words, all_words) {
  # Get a list of unique words in each keyword so we don't double count (e.g.,
  # don't double count "vector" in "vector times vector").
  unq_wrds <- unlist(lapply(cand_words, unique))

  wrd_cnts <- as.matrix(table(unq_wrds))
  all_wrd_cnts <- as.matrix(table(all_words)[rownames(wrd_cnts)])

  temp_score1 <- vapply(
    rownames(wrd_cnts),
    function(x) {
      sum(
        vapply(
          cand_words,
          function(q) ifelse(x %in% q, length(q) - 1, 0),
          numeric(1)
        )
      )
    },
    numeric(1)
  )

  degree <- temp_score1 + wrd_cnts[, 1]

  word_scores <- structure(degree / all_wrd_cnts, names = rownames(wrd_cnts))
  unlist(lapply(cand_words, function(x) sum(word_scores[x])))
}

#' Run RAKE keyword extraction for one text value
#' @noRd
slowrake_atomic <- function(
  txt,
  stop_words,
  all_words,
  word_min_char,
  stem,
  stop_pos,
  word_token_annotator,
  pos_annotator
) {
  txt <- paste0(txt, ".")

  if (!grepl("[[:alpha:]]", txt)) {
    return(NA)
  }

  txt <- slowrake_remove_pos_tags(
    txt,
    stop_pos,
    word_token_annotator,
    pos_annotator
  )
  cand_words <- slowrake_candidate_words(txt, stop_words, word_min_char)
  if (length(cand_words) == 0) {
    return(NA)
  }

  keyword_df <- slowrake_keyword_df(cand_words, all_words, stem)
  slowraker_internal("process_keyword_df")(keyword_df)
}

#' Remove stopped parts of speech before keyword extraction
#' @noRd
slowrake_remove_pos_tags <- function(
  txt,
  stop_pos,
  word_token_annotator,
  pos_annotator
) {
  if (!is.null(stop_pos)) {
    pos_word_df <- tryCatch(
      slowraker_internal("get_pos_tags")(
        txt,
        word_token_annotator,
        pos_annotator
      ),
      error = slowraker_internal("handle_pos_error")
    )
    txt <- slowraker_internal("stop_pos_tags")(pos_word_df, stop_pos)
  }

  txt
}

#' Build candidate word groups for keyword extraction
#' @noRd
slowrake_candidate_words <- function(txt, stop_words, word_min_char) {
  txt <- tolower(txt)
  cand_words <- slowraker_internal("get_cand_words")(txt, stop_words)
  slowraker_internal("filter_words")(cand_words, word_min_char)
}

#' Build a scored keyword data frame
#' @noRd
slowrake_keyword_df <- function(cand_words, all_words, stem) {
  collapse <- function(x) paste(x, collapse = " ")
  keyword <- vapply(cand_words, collapse, character(1))

  if (stem) {
    cand_words <- lapply(cand_words, SnowballC::wordStem)
  }

  score <- calc_keyword_scores(cand_words, all_words)

  keyword_df <- data.frame(
    keyword = keyword,
    score = score
  )

  if (stem) {
    keyword_df$stem <- vapply(cand_words, collapse, character(1))
  }

  keyword_df
}

#' Run RAKE keyword extraction for text values
#' @noRd
slowrake <- function(
  txt,
  all_words,
  stop_words = NULL,
  stop_pos = c("VB", "VBD", "VBG", "VBN", "VBP", "VBZ"),
  word_min_char = 3,
  stem = TRUE,
  quiet = TRUE
) {
  if (is.null(stop_words)) {
    stop_words <- slowraker::smart_words
  }

  num_docs <- length(txt)
  one_doc <- num_docs == 1

  annotators <- slowrake_pos_annotators(stop_pos)

  if (!one_doc && !quiet) {
    prog_bar <- utils::txtProgressBar(min = 0, max = num_docs, style = 3)
    on.exit(close(prog_bar), add = TRUE)
  }

  all_out <- vector(mode = "list", length = num_docs)

  all_words <- tolower(all_words)
  all_words <- slowraker_internal("get_cand_words")(all_words, stop_words)
  all_words <- slowraker_internal("filter_words")(all_words, word_min_char)

  collapse <- function(x) paste(x, collapse = " ")
  all_words <- vapply(all_words, collapse, character(1))

  if (stem) {
    all_words <- lapply(all_words, SnowballC::wordStem)
  }

  for (i in seq_along(txt)) {
    all_out[[i]] <- slowrake_atomic(
      txt = txt[i],
      all_words = unlist(lapply(all_words, str_split, " ")),
      stop_words = stop_words,
      word_min_char = word_min_char,
      stem = stem,
      stop_pos = stop_pos,
      pos_annotator = annotators$pos,
      word_token_annotator = annotators$word_token
    )
    if (!one_doc && !quiet) {
      utils::setTxtProgressBar(prog_bar, i)
    }
  }

  all_out
}

#' Build the openNLP annotators used for POS-tag filtering
#'
#' POS-tag filtering is the only branch that needs \pkg{openNLP} (and so
#' Java via \pkg{rJava}); \code{stop_pos = NULL} never touches it.
#' @noRd
slowrake_pos_annotators <- function(stop_pos) {
  if (is.null(stop_pos)) {
    return(NULL)
  }
  if (!semantic_name_package_available("openNLP")) {
    abort(c(
      "POS-tag filtering (`stop_pos`) needs the optional package openNLP.",
      x = "Install openNLP (it needs Java via rJava).",
      i = "Or use `stop_pos = NULL` to skip POS-tag filtering."
    ))
  }

  list(
    pos = openNLP::Maxent_POS_Tag_Annotator(),
    word_token = openNLP::Maxent_Word_Token_Annotator()
  )
}

# resolves on first semantic-name use, memoised for the session
slowraker_internal <- local({
  cache <- new.env(parent = emptyenv())
  function(name) {
    if (is.null(cache[[name]])) {
      cache[[name]] <- utils::getFromNamespace(name, "slowraker")
    }
    cache[[name]]
  }
})
