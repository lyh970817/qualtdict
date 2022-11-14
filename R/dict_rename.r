#' Rename variables in a dictionary with names of matching variables in a
#' reference dictionary
#'
#' Based on matching suggestions returned by \code{dict_compare}, rename
#' variables in a \code{dict} with the names of matching variables from the
#' reference dictionary in \code{dict_matches}.
#' item text and labels and suggest potential (fuzzy) matches by comparing
#' each question or item text in \code{dict} to all the ones in
#' \code{reference_dict} and obtain the best match.
#' The results can be
#' used for
#' \code{\link[qualtdict]{dict_rename}} to ensure the same variables in
#' two different dictionaries have the same names.
#' @param dict Variable dictionary returned by
#' \code{\link[qualtdict]{dict_generate}}.
#' @param dict_matches A data frame returned by \code{\link[qualtdict]{dict_compare}}
#' the indicate variable matching between \code{dict} and a reference
#' dictionary.
#'
#' @export
dict_rename <- function(dict,
                        dict_matches) {
  if (length(dict_matches[["name_reference"]]) == 0) {
    stop("No matches found")
  }

  renames <- make.unique(dict_matches[["name_reference"]])

  dict[["name"]] <- recode(
    dict[["name"]],
    !!!setNames(
      renames,
      dict_matches[["name"]]
    )
  )

  dict
}
