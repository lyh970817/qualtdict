#' Generate a variable dictionary for a survey
#'
#' Generate a variable dictionary for a Qualtrics survey survey
#'
#' @param surveyID String. Unique ID for the survey you want to download.
#' @param var_name String. Which variable name to use in the dictionary,
#' \code{question_name} for question names on Qualtrics, \code{easy_name}
#' for easy names generated based on question text.
#' @param block_pattern Function. A function that given the name of a
#' survey block, returns a string to be appended to variable names in that block.
#' Defaults to \code{NULL}.
#' @param block_sep String. Seperator between variable names and block
#' prefixes returned by \code{block_pattern}. Defaults to "_".
#' @param split_by_block Logical. If \code{TRUE}, the function returns a
#' list with each element being the dictionary for a single survey block.
#'
#' @export
#' @examples
#' \dontrun{
#'
#' # Create a function for \code{block_pattern} that returns the first three letters of a string
#' block_pattern <- function(x) {
#'   substring(x, 1, 3)
#' }
#'
#'
#' mydict <- dict_generate("SV_4YyAHbAxpdbzacl",
#'   var_name = "easy_name",
#'   block_pattern = block_pattern,
#'   block_sep = ".",
#'   split_by_block = FALSE
#' )
#' }
#'
dict_generate <- function(surveyID,
                          var_name = c("question_name", "easy_name"),
                          block_pattern = NULL,
                          block_sep = ".",
                          split_by_block = FALSE,
                          survey_name = NULL) {
  var_name <- match.arg(var_name)

  easyname_gen <- ifelse(
    var_name == "easy_name",
    TRUE, FALSE
  )

  dict <- recode_json(surveyID,
    easyname_gen = easyname_gen,
    block_pattern = block_pattern,
    block_sep = block_sep
  )

  dict <- dict[c(
    "qid", "name", "block", "question",
    "item", "level", "label", "type", "selector", "sub_selector", "content_type"
  )]

  # Add survey attributes
  if (!is.null(survey_name)) {
    attr(dict, "survey_name") <- survey_name
  }


  if (split_by_block) {
    dict <- split(dict, dict$block)
  }

  dict
}

easyname_gen <- function(json, surveyID, block_pattern, block_sep) {
  if (!requireNamespace("slowraker", quietly = TRUE)) {
    stop("Package \"slowraker\" needed for `easyname_gen = TRUE` to work. Please install it.",
      call. = FALSE
    )
  }

  json_copy <- json

  # Extract relevant text
  texts <- json$item

  texts[is.na(texts)] <- json$question[is.na(texts)]
  # Temporary
  # For these questions each chioce (with a label) is exported as variable,
  # thus the easy name should depend on the label
  ma_lgl <- json$selector == "MACOL" | json$selector == "MAVR" | json$selector == "MAHR"
  texts[ma_lgl] <- json$label[ma_lgl]

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
      map_chr(unique(json$block), block_pattern) %>%
        make.unique()
    } else {
      NA
    }

  # Expand easy variable names and block prefixes to repeat the right
  # number of times
  json$easyyquestion <- unique_expand(easyquestion_single, texts)
  json$easyblock <- unique_expand(tolower(block_single), json$block)

  json <- json %>%
    unite(easyname, easyblock, easyyquestion,
      sep = block_sep, na.rm = T
    ) %>%
    mutate(easyname = easyname) %>%
    select(easyname, everything())

  # Add txt to text questions
  txt_qs <- grep("_TEXT", json$qid)
  json$easyname[txt_qs] <- paste(json$easyname[txt_qs], ".txt")

  label_to_sfx <- function(x) {
    str_remove_all(str_replace_all(
      tolower(x),
      "\\s", "_"
    ), "[^0-9A-Za-z_\\.]")
  }

  # Add label to matrix with multiple answers
  add_label_qs <- json$type == "Matrix" & json$selector == "Likert" & json$sub_selector == "MultipleAnswer"
  json$easyname[add_label_qs] <- paste(json$easyname[add_label_qs], label_to_sfx(json$label[add_label_qs]), sep = ".")

  # Add label to loop and merge
  add_loop_label_qs <- as.logical(json$looping)
  loop_json <- json[add_loop_label_qs, ]
  loop_labels <- label_to_sfx(json$looping_label[add_loop_label_qs])
  json$easyname[add_loop_label_qs] <- paste(loop_json$easyname, loop_labels, sep = ".")

  json$easyname <- str_remove_all(json$easyname, "[^0-9A-Za-z_\\.]")

  # Make unique duplicated easynames
  duplicated_easynames <- which_not_onetoone(json[c("easyname", "qid")])[[1]]
  duplicated_easynames["easyname"] <- make.unique(duplicated_easynames[["easyname"]])
  not_duplicated_easynames <-
    json[!json$qid %in% duplicated_easynames$qid, c("easyname", "qid")]

  all_easynames <- bind_rows(
    duplicated_easynames,
    not_duplicated_easynames
  )

  json_copy$name <- recode(json$qid, !!!setNames(all_easynames$easyname, all_easynames$qid))
  # json_copy$easyname <- NULL

  json_copy
}
