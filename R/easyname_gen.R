#' Generate easy names from dictionary
#' @importFrom tidyr unite
#' @importFrom stringi stri_count_words
#' @keywords internal
easyname_gen <- function(json,
                         surveyID,
                         block_pattern,
                         block_sep,
                         preprocess) {
  json_makename <- json

  if (!is.null(preprocess)) {
    json_makename <- preprocess(json_makename)
  }

  # Extract relevant text
  texts <- json_makename$item

  texts[is.na(texts)] <- json_makename$question[is.na(texts)]

  # For these questions each chioce (with a label) is exported as variable,
  # thus the easy name should depend on the label
  ma_lgl <- json_makename$selector %in% c("MACOL", "MAVR", "MAHR", "MSB")
  texts[ma_lgl] <- json_makename$label[ma_lgl]

  # For SBS matrix
  sbs_matrix <- json_makename$type == "SBS"
  texts[sbs_matrix] <- json_makename$question[sbs_matrix]

  # Extract unique text
  unique_texts <- unique(texts)

  # Generate temp file path
  tmpfile_path <- paste0(tempdir(), "/", surveyID, "K.rds")

  # Check if the same keywords have been saved in temp file path,
  # if not, generate them
  if (file.exists(tmpfile_path)) {
    keywords <- readRDS(tmpfile_path)
  }
  if (!file.exists(tmpfile_path) || length(unique_texts) != length(keywords)) {
    message("Generating easy names...")

    # Remove brackets and punctuations
    unique_texts <- str_remove_all(unique_texts, "\\(.+\\)")
    unique_texts <- str_remove_all(unique_texts, "[[:punct:]]")

    keywords <- slowrake(unique_texts,
      all_words = paste(texts, collapse = ""), stop_pos = NULL
    )

    # Save in temp folder
    saveRDS(keywords, file = tmpfile_path)
  }

  # Generate easy variable name for each unique question text
  easyquestion_single <- imap_chr(keywords, function(x, i) {
    if (all(is.na(x))) {
      # If no keywords generated, use original text
      nm <- unique_texts[i]
    } else if (stri_count_words(unique(texts)[i]) < 8) {
      # If original text shorter than 8 words, use original text
      nm <- unique_texts[i]
    } else {
      # Else use the firt four keywords
      nm <- paste(x[[1]], collapse = " ") %>%
        str_split(" ") %>%
        unlist() %>%
        .[1:4] %>%
        discard(is.na) %>%
        paste(collapse = "_")
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

  # Expand easy variable names and block prefixes to repeat the right
  # number of times
  json_makename$easyquestion <- unique_expand(easyquestion_single, texts)
  json_makename$easyblock <-
    unique_expand(tolower(block_single), json_makename$block)

  json_makename <- json_makename %>%
    unite(easyname, easyblock, easyquestion,
      sep = block_sep, na.rm = TRUE
    ) %>%
    mutate(easyname = easyname) %>%
    select(easyname, everything())

  # Add txt to text questions
  txt_qs <- grep("_TEXT", json_makename$qid)
  json_makename$easyname[txt_qs] <-
    paste(json_makename$easyname[txt_qs], ".txt")

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

  json_makename$easyname[add_label_qs] <-
    paste(json_makename$easyname[add_label_qs],
      label_to_sfx(json_makename$label[add_label_qs]),
      sep = "."
    )

  # Add item to sbs matrix with single answers
  json_makename$easyname[sbs_matrix] <-
    paste(json_makename$easyname[sbs_matrix],
      label_to_sfx(json_makename$item[sbs_matrix]),
      sep = "."
    )

  # Add label to loop and merge
  add_loop_option_qs <- as.logical(json_makename$looping)
  loop_json <- json_makename[add_loop_option_qs, ]
  loop_options <- label_to_sfx(json_makename$looping_option[add_loop_option_qs])
  json_makename$easyname[add_loop_option_qs] <-
    paste(loop_json$easyname, loop_options, sep = ".")

  # Remove symbols
  json_makename$easyname <-
    str_remove_all(json_makename$easyname, "[^0-9A-Za-z_\\.]")

  # Make unique duplicated easynames
  duplicated_easynames <-
    which_not_onetoone(json_makename[c("easyname", "qid")])[[1]]
  duplicated_easynames["easyname"] <- make.unique(duplicated_easynames[["easyname"]]) # nolint # nolint

  not_duplicated_easynames <-
    json_makename[!json_makename$qid %in%
      duplicated_easynames$qid, c("easyname", "qid")]

  all_easynames <- bind_rows(
    duplicated_easynames,
    not_duplicated_easynames
  )

  json$name <-
    recode(
      json_makename$qid,
      !!!setNames(all_easynames$easyname, all_easynames$qid)
    )

  json
}
