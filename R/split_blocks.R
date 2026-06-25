#' Split a Variable Dictionary by Survey Block
#'
#' Create block-specific views of a Variable Dictionary without changing the
#' return type of \code{\link[qualtdict]{dict_generate}}.
#'
#' @inheritParams dict_validate
#'
#' @return A named list of Variable Dictionaries, one per Survey Block.
#' @export
#' @examples
#' dict <- data.frame(
#'   response_column_id = c("QID1", "QID2"),
#'   variable_name = c("q1", "q2"),
#'   block = c("Block A", "Block B")
#' )
#' class(dict) <- c("qualtdict", class(dict))
#'
#' dict_split_blocks(dict)
dict_split_blocks <- function(dict) {
  checkarg_isqualtdict(dict)

  map(
    split(dict, dict$block),
    copy_qualtdict_attrs,
    source = dict
  )
}

#' Split Labelled Survey Data by Survey Block
#'
#' Create block-specific views of Labelled Survey Data without changing the
#' return type of \code{\link[qualtdict]{get_survey_data}}.
#'
#' @param dat Labelled Survey Data returned by
#' \code{\link[qualtdict]{get_survey_data}}.
#' @inheritParams dict_validate
#' @param extra_columns A character vector of raw Labelled Survey Data columns
#' to retain in each block data set. Defaults to
#' \code{c("externalDataReference", "startDate", "endDate")}. Missing
#' user-specified columns error; missing default columns warn and are skipped.
#' Use \code{NULL} to retain no extra columns.
#' @details
#' When \code{dict} is not supplied, \code{survey_split_blocks()} uses the
#' \code{dict} attribute attached by \code{\link[qualtdict]{get_survey_data}}.
#'
#' @return A named list of data frames, one per Survey Block.
#' @export
#' @examples
#' dict <- data.frame(
#'   response_column_id = c("QID1", "QID2"),
#'   variable_name = c("q1", "q2"),
#'   block = c("Block A", "Block B")
#' )
#' class(dict) <- c("qualtdict", class(dict))
#'
#' dat <- data.frame(
#'   externalDataReference = "R_1",
#'   startDate = "2026-06-01",
#'   endDate = "2026-06-01",
#'   q1 = "1",
#'   q2 = "2"
#' )
#' attr(dat, "dict") <- dict
#'
#' survey_split_blocks(dat)
survey_split_blocks <- function(dat,
                                dict = attr(dat, "dict", exact = TRUE),
                                extra_columns = c(
                                  "externalDataReference",
                                  "startDate",
                                  "endDate"
                                )) {
  if (!is.data.frame(dat)) {
    rlang::abort("`dat` must be Labelled Survey Data.")
  }
  checkarg_isqualtdict(dict)

  extra_columns_user_supplied <- !missing(extra_columns)
  checkarg_ischaracter(extra_columns, null_okay = TRUE)
  extra_columns <- resolve_extra_columns(
    dat = dat,
    extra_columns = extra_columns,
    extra_columns_user_supplied = extra_columns_user_supplied
  )

  map(dict_split_blocks(dict), function(block_dict) {
    block_columns <- unique(dict_variable_name(block_dict))
    block_columns <- block_columns[block_columns %in% colnames(dat)]
    block_dat <- dat[unique(c(extra_columns, block_columns))]
    attr(block_dat, "dict") <- block_dict
    block_dat
  })
}
