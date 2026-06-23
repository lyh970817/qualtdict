#' Generate Semantic Names from dictionary
#' @importFrom tidyr unite
#' @importFrom stringi stri_count_words
#' @importFrom rlang hash
#' @importFrom utils head
#' @keywords internal
#' @noRd
generate_semantic_names <- function(json,
                              surveyID,
                              block_pattern,
                              block_sep,
                              semantic_name_preprocess,
                              quiet = TRUE) {
  json_makename <- json
  response_column_id <- dict_response_column_id(json)

  if (!is.null(semantic_name_preprocess)) {
    json_makename <- semantic_name_preprocess(json_makename)
  }

  # Extract relevant text
  texts <- json_makename$item

  texts[is.na(texts)] <- json_makename$question[is.na(texts)]

  # For these questions each chioce (with a label) is exported as variable,
  # thus the Semantic Name should depend on the label
  ma_lgl <- json_makename$selector %in% c("MACOL", "MAVR", "MAHR", "MSB")
  texts[ma_lgl] <- json_makename$label[ma_lgl]

  # For SBS matrix
  sbs_matrix <- json_makename$type == "SBS"
  texts[sbs_matrix] <- json_makename$question[sbs_matrix]

  # Extract unique text
  unique_texts <- unique(texts)
  cleaned_unique_texts <- clean_semantic_name_text(unique_texts)
  all_words <- paste(texts, collapse = " ")

  # Generate temp file path
  tmpfile_path <- paste0(tempdir(), "/", hash(list(
    algorithm = "semantic-name-source-order-v1",
    unique_texts = cleaned_unique_texts,
    all_words = all_words
  )), ".rds")

  # Check if the same keywords have been saved in temp file path,
  # if not, generate them
  if (file.exists(tmpfile_path)) {
    keywords <- readRDS(tmpfile_path)
  }
  if (!file.exists(tmpfile_path)) {
    if (!quiet) {
      message("Generating Semantic Names...")
    }

    keywords <- slowrake(
      cleaned_unique_texts,
      all_words = all_words,
      stop_pos = NULL,
      quiet = quiet
    )

    # Save in temp folder
    saveRDS(keywords, file = tmpfile_path)
  }

  # Generate Semantic Name component for each unique question text.
  semantic_question_single <- imap_chr(keywords, function(x, i) {
    if (all(is.na(x))) {
      # If no keywords generated, use original text
      nm <- cleaned_unique_texts[i]
    } else if (stri_count_words(unique(texts)[i]) < 8) {
      # If original text shorter than 8 words, use original text
      nm <- cleaned_unique_texts[i]
    } else {
      # Else use the first four selected keyword words in source order.
      nm <- semantic_name_source_order_component(x, cleaned_unique_texts[i])
    }

    tolower(str_replace_all(nm, "\\s", "_"))
  })

  # Generate block prefix for each unique block name
  block_single <-
    if (!is.null(block_pattern)) {
      map_chr(unique(json_makename$block), block_pattern) %>%
        make.unique()
    } else {
      NA
    }

  # Expand Semantic Name components and block prefixes to repeat the right
  # number of times
  json_makename$semantic_question <-
    unique_expand(semantic_question_single, texts)
  json_makename$semantic_block <-
    unique_expand(tolower(block_single), json_makename$block)

  json_makename <- json_makename %>%
    unite(semantic_name, semantic_block, semantic_question,
      sep = block_sep, na.rm = TRUE
    ) %>%
    mutate(semantic_name = semantic_name) %>%
    select(semantic_name, everything())

  # Add txt to text questions
  txt_qs <- grep("_TEXT", response_column_id, fixed = TRUE)
  json_makename$semantic_name[txt_qs] <-
    paste(json_makename$semantic_name[txt_qs], ".txt")

  label_to_sfx <- function(x) {
    str_remove_all(str_replace_all(
      tolower(x),
      "\\s", "_"
    ), "[^0-9A-Za-z_\\.]")
  }

  # Add label to matrix with multiple answers
  add_label_qs <-
    json_makename$type == "Matrix" &
      json_makename$selector == "Likert" &
      json_makename$sub_selector == "MultipleAnswer"

  json_makename$semantic_name[add_label_qs] <-
    paste(json_makename$semantic_name[add_label_qs],
      label_to_sfx(json_makename$label[add_label_qs]),
      sep = "."
    )

  # Add item to sbs matrix with single answers
  json_makename$semantic_name[sbs_matrix] <-
    paste(json_makename$semantic_name[sbs_matrix],
      label_to_sfx(json_makename$item[sbs_matrix]),
      sep = "."
    )

  # Add label to loop and merge
  add_loop_option_qs <- as.logical(json_makename$looping)
  loop_json <- json_makename[add_loop_option_qs, ]
  loop_options <- label_to_sfx(json_makename$looping_option[add_loop_option_qs])
  json_makename$semantic_name[add_loop_option_qs] <-
    paste(loop_json$semantic_name, loop_options, sep = ".")

  # Remove symbols
  json_makename$semantic_name <-
    str_remove_all(json_makename$semantic_name, "[^0-9A-Za-z_\\.]")

  json$semantic_name <- json_makename$semantic_name
  json$variable_name <- json$semantic_name

  json
}

clean_semantic_name_text <- function(text) {
  str_remove_all(text, "\\(.+\\)") %>%
    str_remove_all("[[:punct:]]")
}

semantic_name_source_order_component <- function(keywords, source_text,
                                                 max_words = 4) {
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
