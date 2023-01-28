#' Generate a variable dictionary for a survey
#'
#' Generate a variable dictionary for a Qualtrics survey survey
#'
#' @param surveyID String. Unique ID for the survey you want to download.
#' @param name String. Which type of variable names to use in the dictionary,
#' \code{question_name} for question names on Qualtrics, \code{easy_name}
#' for easy names generated based on question text.
#' @param block_pattern Function. A function that given the name of a
#' survey block, returns a string to be appended to variable names
#' in that block.
#' Defaults to \code{NULL}.
#' @param preprocess Function. A function that given a dataframe with the
#' column names as in a dictionary, does formatting for each column.
#' Defaults to \code{NULL}.
#' @param block_sep String. Seperator between variable names and block
#' prefixes returned by \code{block_pattern}. Defaults to ".".
#' @param split_by_block Logical. If \code{TRUE}, the function returns a
#' list with each element being the dictionary for a single survey block.
#'
#'
#' @return
#' A 'qualtdict' dataframe or a list of 'qualtdict' dataframes (depending on
#' \code{split_by_block}.
#'
#' @export
#' @examples
#' \dontrun{
#'
#' # Create a function for \code{block_pattern}
#' # that returns the first three letters of a string
#' block_pattern <- function(x) {
#'   substring(x, 1, 3)
#' }
#'
#'
#' mydict <- dict_generate("SV_4YyAHbAxpdbzacl",
#'   name = "easy_name",
#'   block_pattern = block_pattern,
#'   block_sep = ".",
#'   split_by_block = FALSE
#' )
#' }
#'
dict_generate <- function(surveyID,
                          name = c("question_name", "easy_name"),
                          block_pattern = NULL,
                          block_sep = ".",
                          preprocess = NULL,
                          split_by_block = FALSE) {
  name <- match.arg(name)

  easyname_gen <- ifelse(
    name == "easy_name",
    TRUE, FALSE
  )

  dict <- recode_json(surveyID,
    easyname_gen = easyname_gen,
    block_pattern = block_pattern,
    block_sep = block_sep,
    preprocess = preprocess
  )

  dict <- dict[c(
    "qid", "name", "block", "question",
    "item", "level", "label", "type", "selector", "sub_selector", "content_type"
  )]

  attr(dict, "class") <- c("qualtdict", class(dict))

  if (split_by_block) {
    dict <- split(dict, dict$block)
  }

  dict
}
