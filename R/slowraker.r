calc_keyword_scores <- function(cand_words, all_words) {
  # Get a list of unique words in each keyword so we don't double count (e.g.,
  # don't double count "vector" in "vector times vector").
  unq_wrds <- unlist(lapply(cand_words, unique))

  wrd_cnts <- as.matrix(table(unq_wrds))
  all_wrd_cnts <- as.matrix(table(all_words)[rownames(wrd_cnts)])

  temp_score1 <- vapply(
    rownames(wrd_cnts), function(x) {
      sum(
        vapply(
          cand_words, function(q) ifelse(x %in% q, length(q) - 1, 0), numeric(1)
        )
      )
    },
    numeric(1)
  )

  degree <- temp_score1 + wrd_cnts[, 1]

  word_scores <- structure(degree / all_wrd_cnts, names = rownames(wrd_cnts))
  unlist(lapply(cand_words, function(x) sum(word_scores[x])))
}

slowrake_atomic <- function(txt, stop_words, all_words, word_min_char, stem, stop_pos,
                            word_token_annotator, pos_annotator) {
  txt <- paste0(txt, ".")

  if (!grepl("[[:alpha:]]", txt)) {
    return(NA)
  }

  if (!is.null(stop_pos)) {
    tryCatch(
      pos_word_df <- get_pos_tags(txt, word_token_annotator, pos_annotator),
      error = handle_pos_error
    )
    txt <- stop_pos_tags(pos_word_df, stop_pos)
  }

  txt <- tolower(txt)

  cand_words <- slowraker2$get_cand_words(txt, stop_words)
  cand_words <- slowraker2$filter_words(cand_words, word_min_char)

  # drop dashes. have to do this at this point instead of sooner b/c we want to
  # apply min word length filter on the complete hyphenated word, not on each
  # component word. note: we are still limited by fact that single letters are
  # in list of stopwords and r thinks that - is a word boundary, so the "k" in
  # "k-means" will be dropped.

  if (length(cand_words) == 0) {
    return(NA)
  }

  # Convert word vectors into keywords (a word vector contains the words in a
  # keyword)
  collapse <- function(x) paste0(x, collapse = " ")
  keyword <- vapply(cand_words, collapse, character(1))

  if (stem) cand_words <- lapply(cand_words, SnowballC::wordStem)

  score <- calc_keyword_scores(cand_words, all_words)

  keyword_df <- data.frame(
    keyword = keyword,
    score = score,
    stringsAsFactors = FALSE
  )

  if (stem) {
    keyword_df$stem <- vapply(cand_words, collapse, character(1))
  }

  slowraker2$process_keyword_df(keyword_df)
}

slowrake <- function(txt,
                     all_words,
                     stop_words = smart_words,
                     stop_pos = c("VB", "VBD", "VBG", "VBN", "VBP", "VBZ"),
                     word_min_char = 3,
                     stem = TRUE) {
  num_docs <- length(txt)
  one_doc <- num_docs == 1

  if (!is.null(stop_pos)) {
    pos_annotator <- openNLP::Maxent_POS_Tag_Annotator()
    word_token_annotator <- openNLP::Maxent_Word_Token_Annotator()
  }

  if (!one_doc) {
    prog_bar <- utils::txtProgressBar(min = 0, max = num_docs, style = 3)
  }

  all_out <- vector(mode = "list", length = num_docs)

  all_words <- tolower(all_words)
  all_words <- slowraker2$get_cand_words(all_words, stop_words)
  all_words <- slowraker2$filter_words(all_words, word_min_char)

  collapse <- function(x) paste0(x, collapse = " ")
  all_words <- vapply(all_words, collapse, character(1))

  if (stem) all_words <- lapply(all_words, SnowballC::wordStem)

  for (i in seq_along(txt)) {
    all_out[[i]] <- slowrake_atomic(
      txt = txt[i],
      all_words = unlist(lapply(all_words, str_split, " ")),
      stop_words = stop_words,
      word_min_char = word_min_char,
      stem = stem,
      stop_pos = stop_pos,
      pos_annotator = pos_annotator,
      word_token_annotator = word_token_annotator
    )
    if (!one_doc) utils::setTxtProgressBar(prog_bar, i)
  }

  structure(all_out, class = c(class(all_out), "rakelist"))
}

.internals <- c("get_cand_words", "filter_words", "process_keyword_df")
# load from the slowraker namespace
slowraker2 <- structure(
  mapply(function(.internals, i) getFromNamespace(i, "slowraker"), .internals, .internals),
  class = c("internal")
)
