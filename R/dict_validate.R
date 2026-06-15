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
#' Validation Findings are separate from Unsupported Structure Findings.
#' Unsupported Structure Findings describe Qualtrics structures that
#' \code{qualtdict} could not fully represent while normalising metadata; use
#' [unsupported_structure_findings()] to inspect them on the Variable
#' Dictionary.
#'
#' @examples
#' \dontrun{
#' survey_id <- "SV_XXXXXXXXXXXXXXXX"
#'
#' # Create a dictionary
#' mydict <- dict_generate(survey_id, variable_name = "question_name")
#'
#'
#' # Validate the dictionary
#' validation <- dict_validate(mydict)
#' }
#'
#' @export
dict_validate <- function(dict) {
  checkarg_isqualtdict(dict)
  message("Validating dictionary...")

  split_dict <- split(
    dict,
    factor(dict_response_column_id(dict))
  )

  level_label_pairs <- validation_level_label_pairs(split_dict)

  repaired_names <- repaired_name_validation_findings(dict)
  if (nrow(repaired_names) > 0) {
    message("Variable names were repaired for export.")
  }

  variable_name_findings <- variable_name_validation_findings(dict)
  if (nrow(variable_name_findings) > 0) {
    message("Variable names are not export-safe and unique.")
  }

  mistake <- check_json(split_dict)
  level_label_findings <- level_label_validation_findings(mistake)

  if (nrow(level_label_findings) > 0) {
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

new_qualtdict_validation <- function(validation_findings, level_label_pairs) {
  structure(
    list(
      validation_findings = normalize_validation_findings(
        validation_findings
      ),
      level_label_pairs = level_label_pairs
    ),
    class = c("qualtdict_validation", "list")
  )
}

validation_finding_columns <- function() {
  c(
    "finding",
    "response_column_id",
    "variable_name",
    "original_candidate",
    "reason",
    "item_name",
    "mistake",
    "label",
    "level"
  )
}

empty_validation_findings <- function() {
  tibble(
    finding = character(),
    response_column_id = character(),
    variable_name = character(),
    original_candidate = character(),
    reason = character(),
    item_name = character(),
    mistake = character(),
    label = character(),
    level = character()
  )
}

normalize_validation_findings <- function(findings) {
  if (is.null(findings) || nrow(findings) == 0) {
    return(empty_validation_findings())
  }

  findings <- as_tibble(findings)
  missing_columns <- setdiff(validation_finding_columns(), names(findings))
  for (column in missing_columns) {
    findings[[column]] <- NA_character_
  }
  findings <- findings[validation_finding_columns()]
  findings[] <- lapply(findings, as.character)
  findings
}

validation_level_label_pairs <- function(split_dict) {
  if (length(split_dict) == 0) {
    return(tibble(pair = list(), qid = list()))
  }

  split_dict %>%
    modify(function(x) {
      # Remove names so they don't interfere with grouping
      x$level <- setNames(x$level, NULL)
      x$label <- setNames(x$label, NULL)
      x
    }) %>%
    map(select, label, level) %>%
    enframe(value = "pair") %>%
    group_by(pair) %>%
    summarize(qid = list(name), .groups = "drop")
}

repaired_name_validation_findings <- function(dict) {
  repaired_names <- attr(dict, "variable_name_findings", exact = TRUE)
  if (is.null(repaired_names)) {
    repaired_names <- empty_variable_name_findings()
  }
  if (nrow(repaired_names) == 0) {
    return(empty_validation_findings())
  }

  repaired_names$finding <- "repaired_variable_name"
  normalize_validation_findings(repaired_names)
}

variable_name_validation_findings <- function(dict) {
  names_by_response_column <- tibble(
    response_column_id = as.character(dict_response_column_id(dict)),
    variable_name = as.character(dict_variable_name(dict))
  ) %>%
    distinct()

  bind_rows(
    inconsistent_response_column_names(names_by_response_column),
    duplicate_variable_names(names_by_response_column),
    unsafe_variable_names(names_by_response_column)
  )
}

inconsistent_response_column_names <- function(names_by_response_column) {
  findings <- names_by_response_column %>%
    group_by(.data$response_column_id) %>%
    filter(n_distinct(.data$variable_name) > 1) %>%
    ungroup()

  if (nrow(findings) == 0) {
    return(empty_validation_findings())
  }

  findings$finding <- "inconsistent_variable_name"
  findings$reason <- "response_column_id_not_one_to_one"
  normalize_validation_findings(findings)
}

duplicate_variable_names <- function(names_by_response_column) {
  findings <- names_by_response_column %>%
    group_by(.data$variable_name) %>%
    filter(n_distinct(.data$response_column_id) > 1) %>%
    ungroup()

  if (nrow(findings) == 0) {
    return(empty_validation_findings())
  }

  findings$finding <- "duplicate_variable_name"
  findings$reason <- "variable_name_not_unique"
  normalize_validation_findings(findings)
}

unsafe_variable_names <- function(names_by_response_column) {
  repaired_variable_name <- repair_variable_name_base(
    names_by_response_column$variable_name
  )
  unsafe <- is.na(names_by_response_column$variable_name) |
    names_by_response_column$variable_name != repaired_variable_name

  findings <- names_by_response_column[unsafe, ]
  if (nrow(findings) == 0) {
    return(empty_validation_findings())
  }

  findings$finding <- "unsafe_variable_name"
  findings$original_candidate <- findings$variable_name
  findings$reason <- "unsafe"
  normalize_validation_findings(findings)
}

level_label_validation_findings <- function(mistake) {
  if (nrow(mistake) == 0) {
    return(empty_validation_findings())
  }

  mistake$finding <- "level_label_mistake"
  mistake$variable_name <- mistake$item_name
  normalize_validation_findings(mistake)
}

check_names <- function(dict) {
  cols <- tibble(
    response_column_id = dict_response_column_id(dict),
    variable_name = dict_variable_name(dict)
  )
  which_not_onetoone(cols)
}

check_item <- function(dat, response_column_id) {
  item_name <- dict_variable_name(dat)[
    dict_response_column_id(dat) == response_column_id
  ]
  cols <- dat[c("label", "level")]

  # Here recode is sometimes "none" and will cause a warning
  col2_pos <- suppressWarnings(
    as.numeric(cols[[2]]) %>%
      subset(. >= 0)
  )

  has_mistake <- c(
    # Check correspondence
    !is_onetoone(cols),
    # Check constant step == 1
    !(all(diff(sort(col2_pos)) == 1) | length(diff(col2_pos)) == 0),
    # Check duplication
    any(duplicated(cols[[1]])),
    any(duplicated(cols[[2]]))
  )

  if (any(has_mistake)) {
    bind_cols(
      tibble(
        qid = response_column_id,
        response_column_id = response_column_id,
        item_name,
        mistake = paste(which(has_mistake), collapse = "")
      ),
      cols
    )
  }
}

check_json <- function(split_jsons) {
  mistakes <- imap(split_jsons, check_item) %>%
    bind_rows()
  if (nrow(mistakes) > 0) {
    return(mistakes)
  } else {
    return(tibble())
  }
}
