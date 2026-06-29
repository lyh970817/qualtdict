#' Generate Semantic Names from dictionary
#' @importFrom tidyr unite
#' @importFrom stringi stri_count_words
#' @importFrom rlang hash
#' @importFrom utils head
#' @keywords internal
#' @noRd
generate_semantic_names <- function(
  json,
  surveyID,
  block_pattern,
  block_sep,
  semantic_name_preprocess,
  quiet = TRUE
) {
  if (!"variable_name" %in% names(json)) {
    json$variable_name <- NA_character_
  }
  json_makename <- json

  if (!is.null(semantic_name_preprocess)) {
    json_makename <- semantic_name_preprocess(json_makename)
  }

  question_rows <- semantic_name_question_rows(json_makename)
  json$semantic_name <- NA_character_
  if (!any(question_rows)) {
    return(json)
  }

  question_json <- json_makename[question_rows, ]
  response_column_id <- dict_response_column_id(question_json)
  texts <- semantic_name_texts(question_json)
  keywords <- semantic_name_keywords(texts, quiet = quiet)
  question_json <- add_semantic_name_components(
    question_json,
    texts = texts,
    keywords = keywords,
    block_pattern = block_pattern,
    block_sep = block_sep
  )
  question_json <- add_semantic_name_suffixes(
    question_json,
    response_column_id
  )

  json$semantic_name[question_rows] <- question_json$semantic_name
  json$variable_name[question_rows] <- question_json$semantic_name

  json
}

semantic_name_question_rows <- function(json_makename) {
  if (!"row_source" %in% names(json_makename)) {
    return(rep(TRUE, nrow(json_makename)))
  }

  !is.na(json_makename$row_source) & json_makename$row_source == "question"
}

semantic_name_texts <- function(json_makename) {
  texts <- json_makename$item
  missing_text <- is.na(texts) | !nzchar(texts)
  texts[missing_text] <- json_makename$question[missing_text]

  ma_lgl <- json_makename$selector %in% c("MACOL", "MAVR", "MAHR", "MSB")
  ma_label_lgl <-
    ma_lgl & !is.na(json_makename$label) & nzchar(json_makename$label)
  texts[ma_label_lgl] <- json_makename$label[ma_label_lgl]

  sbs_matrix <- json_makename$type == "SBS"
  texts[sbs_matrix] <- json_makename$question[sbs_matrix]

  missing_text <- is.na(texts) | !nzchar(texts)
  texts[missing_text] <- json_makename$question[missing_text]

  texts
}

semantic_name_keywords <- function(texts, quiet = TRUE) {
  unique_texts <- unique(texts)
  cleaned_unique_texts <- clean_semantic_name_text(unique_texts)
  all_words <- paste(texts, collapse = " ")
  tmpfile_path <- semantic_name_cache_path(cleaned_unique_texts, all_words)

  if (file.exists(tmpfile_path)) {
    return(readRDS(tmpfile_path))
  }
  if (!quiet) {
    message("Generating Semantic Names...")
  }

  keywords <- slowrake(
    cleaned_unique_texts,
    all_words = all_words,
    stop_pos = NULL,
    quiet = quiet
  )
  saveRDS(keywords, file = tmpfile_path)

  keywords
}

semantic_name_cache_path <- function(cleaned_unique_texts, all_words) {
  paste0(
    tempdir(),
    "/",
    hash(list(
      algorithm = "semantic-name-source-order-v1",
      unique_texts = cleaned_unique_texts,
      all_words = all_words
    )),
    ".rds"
  )
}

add_semantic_name_components <- function(
  json_makename,
  texts,
  keywords,
  block_pattern,
  block_sep
) {
  json_makename$semantic_question <-
    unique_expand(semantic_question_components(texts, keywords), texts)
  json_makename$semantic_block <-
    unique_expand(
      tolower(semantic_block_components(json_makename, block_pattern)),
      json_makename$block
    )

  json_makename |>
    unite(
      semantic_name,
      semantic_block,
      semantic_question,
      sep = block_sep,
      na.rm = TRUE
    ) |>
    mutate(semantic_name = semantic_name) |>
    select(semantic_name, everything())
}

semantic_question_components <- function(texts, keywords) {
  unique_texts <- unique(texts)
  cleaned_unique_texts <- clean_semantic_name_text(unique_texts)

  imap_chr(keywords, function(x, i) {
    if (all(is.na(x)) || stri_count_words(unique_texts[i]) < 8) {
      nm <- cleaned_unique_texts[i]
    } else {
      nm <- semantic_name_source_order_component(x, cleaned_unique_texts[i])
    }

    tolower(str_replace_all(nm, "\\s", "_"))
  })
}

semantic_block_components <- function(json_makename, block_pattern) {
  if (!is.null(block_pattern)) {
    block_components <- map_chr(unique(json_makename$block), block_pattern)
    return(make.unique(block_components))
  }

  NA
}

add_semantic_name_suffixes <- function(json_makename, response_column_id) {
  txt_qs <- grep("_TEXT", response_column_id, fixed = TRUE)
  json_makename$semantic_name[txt_qs] <-
    paste(json_makename$semantic_name[txt_qs], ".txt")

  json_makename <- add_matrix_semantic_name_suffixes(json_makename)
  json_makename <- add_sbs_semantic_name_suffixes(json_makename)
  json_makename <- add_loop_semantic_name_suffixes(json_makename)
  json_makename$semantic_name <-
    str_remove_all(json_makename$semantic_name, "[^0-9A-Za-z_\\.]")

  json_makename
}

add_matrix_semantic_name_suffixes <- function(json_makename) {
  add_label_qs <-
    json_makename$type == "Matrix" &
    json_makename$selector == "Likert" &
    json_makename$sub_selector == "MultipleAnswer"

  json_makename$semantic_name[add_label_qs] <-
    paste(
      json_makename$semantic_name[add_label_qs],
      semantic_name_label_suffix(json_makename$label[add_label_qs]),
      sep = "."
    )

  json_makename
}

add_sbs_semantic_name_suffixes <- function(json_makename) {
  sbs_matrix <- json_makename$type == "SBS"
  json_makename$semantic_name[sbs_matrix] <-
    paste(
      json_makename$semantic_name[sbs_matrix],
      semantic_name_label_suffix(json_makename$item[sbs_matrix]),
      sep = "."
    )

  json_makename
}

add_loop_semantic_name_suffixes <- function(json_makename) {
  add_loop_option_qs <- as.logical(json_makename$looping)
  loop_json <- json_makename[add_loop_option_qs, ]
  loop_options <-
    semantic_name_label_suffix(json_makename$looping_option[add_loop_option_qs])
  json_makename$semantic_name[add_loop_option_qs] <-
    paste(loop_json$semantic_name, loop_options, sep = ".")

  json_makename
}

semantic_name_label_suffix <- function(x) {
  str_remove_all(
    str_replace_all(
      tolower(x),
      "\\s",
      "_"
    ),
    "[^0-9A-Za-z_\\.]"
  )
}

clean_semantic_name_text <- function(text) {
  str_remove_all(text, "\\(.+\\)") |>
    str_remove_all("[[:punct:]]")
}

semantic_name_source_order_component <- function(
  keywords,
  source_text,
  max_words = 4
) {
  keyword_words <- character()
  keyword_values <- keywords$keyword %||% keywords[[1]]

  for (keyword in keyword_values) {
    keyword_words <- c(keyword_words, unlist(str_split(keyword, "\\s+")))
    keyword_words <- unique(discard(keyword_words, ~ .x == ""))

    if (length(keyword_words) >= max_words) {
      break
    }
  }

  source_words <- unlist(str_split(tolower(source_text), "\\s+"))
  keyword_words <- tolower(keyword_words)
  source_position <- match(keyword_words, source_words)
  source_position[is.na(source_position)] <- Inf

  keyword_words <-
    keyword_words[order(source_position, seq_along(keyword_words))]
  paste(head(keyword_words, max_words), collapse = "_")
}
