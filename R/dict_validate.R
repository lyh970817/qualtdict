#' Return Validation Findings for a Variable Dictionary
#'
#' Validate Variable Dictionary consistency and return a stable validation
#' object. Validation Findings report repaired \code{variable_name} values,
#' non-unique or unsafe Dictionary Variable Names, and level-label coding
#' issues. The validation result is a consistency screen, not proof that the
#' source Qualtrics metadata is correct.
#'
#' @param dict A Variable Dictionary returned by
#' \code{\link[qualtdict]{dict_generate}}.
#' @param quiet Boolean. If \code{TRUE}, suppress routine validation messages.
#' Defaults to \code{TRUE}.
#'
#' @return
#' A `qualtdict_validation` object. This is a list with stable components:
#' \itemize{
#'   \item `validation_findings` - A data frame of Validation Findings for
#'   Variable Dictionary consistency issues, including repaired variable names,
#'   non-unique or unsafe final `variable_name` values, and level-label coding
#'   findings.
#'   \item `level_label_pairs` - A data frame containing the unique level-label
#'   pairings.
#' }
#'
#' @examples
#' dict <- data.frame(
#'   response_column_id = c("QID1", "QID2"),
#'   variable_name = c("q1", "q2"),
#'   label = c("Yes", "No"),
#'   level = c("1", "2")
#' )
#' class(dict) <- c("qualtdict", class(dict))
#'
#' validation <- dict_validate(dict)
#'
#' @export
dict_validate <- function(dict, quiet = TRUE) {
  checkarg_isqualtdict(dict)
  checkarg_isboolean(quiet)
  if (!quiet) {
    message("Validating dictionary...")
  }

  level_label_dict <- question_level_label_validation_dict(dict)
  split_dict <- split(
    level_label_dict,
    factor(dict_response_column_id(level_label_dict))
  )

  if (!quiet) {
    message("Checking level-label pairs...")
  }
  level_label_pairs <- validation_level_label_pairs(split_dict, quiet = quiet)

  repaired_names <- repaired_name_validation_findings(dict)
  if (!quiet && nrow(repaired_names) > 0) {
    message("Variable names were repaired for export.")
  }

  variable_name_findings <- variable_name_validation_findings(dict)
  if (!quiet && nrow(variable_name_findings) > 0) {
    message("Variable names are not export-safe and unique.")
  }

  if (!quiet) {
    message("Checking level-label consistency...")
  }
  mistake <- check_json(split_dict, quiet = quiet)
  level_label_findings <- level_label_validation_findings(mistake)

  if (!quiet && nrow(level_label_findings) > 0) {
    message("There are variables with potential incorrect level-label codings.")
  }

  validation_findings <- bind_rows(
    repaired_names,
    variable_name_findings,
    level_label_findings
  )

  new_qualtdict_validation(
    validation_findings = validation_findings,
    level_label_pairs = level_label_pairs
  )
}

question_level_label_validation_dict <- function(dict) {
  dict[dict_question_rows(dict), ]
}
