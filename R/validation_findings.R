#' Build a qualtdict validation result
#' @noRd
new_qualtdict_validation <- function(validation_findings, level_label_pairs) {
  list(
    validation_findings = normalize_validation_findings(
      validation_findings
    ),
    level_label_pairs = level_label_pairs
  )
}

#' Validation Finding schema columns
#' @noRd
validation_finding_columns <- function() {
  c(
    "finding",
    "severity",
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

#' Empty Validation Findings table
#' @noRd
empty_validation_findings <- function() {
  tibble(
    finding = character(),
    severity = character(),
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

#' Classify Validation Finding severity
#'
#' Every Validation Finding is either a Definite Validation Finding
#' (`"definite"`: the affected export column is uninterpretable or its
#' identity is unreliable) or a Suggestive Validation Finding
#' (`"suggestive"`: worth review, but the column's data and identity are
#' sound). `"suggestive"` covers exactly two shapes:
#' `repaired_variable_name` (the name was repaired successfully; the data and
#' labels are untouched) and a `level_label_mistake` none of whose tripped
#' tests is Export-blocking (a gapped level run is ordinary Qualtrics survey
#' design). Everything else is `"definite"`: the Export-blocking level-label
#' codings (`export_blocking_mistake_tests()`), and the variable-name findings
#' that break the rename identity of Labelled Export
#' (`inconsistent_variable_name`, `duplicate_variable_name`,
#' `unsafe_variable_name`). An unrecognised finding class, or a
#' `level_label_mistake` with no recorded test code, classifies as
#' `"definite"`, so a new class fails closed until it is classified here.
#'
#' Severity is derived from `finding` and `mistake` on every normalisation and
#' never stored independently, so it cannot drift from the classes it
#' describes.
#' @noRd
validation_finding_severity <- function(finding, mistake) {
  blocking_pattern <- paste0(
    "[",
    paste(export_blocking_mistake_tests(), collapse = ""),
    "]"
  )
  is_class <- function(x, class) {
    !is.na(x) & x == class
  }
  suggestive <- is_class(finding, "repaired_variable_name") |
    (is_class(finding, "level_label_mistake") &
      !is.na(mistake) &
      nzchar(mistake) &
      !grepl(blocking_pattern, mistake))

  ifelse(suggestive, "suggestive", "definite")
}

#' Normalise Validation Findings to the package schema
#' @noRd
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
  findings$severity <- validation_finding_severity(
    findings$finding,
    findings$mistake
  )
  findings
}

#' Build level-label pairs used for validation
#' @noRd
validation_level_label_pairs <- function(split_dict, quiet = TRUE) {
  if (length(split_dict) == 0) {
    return(tibble(pair = list(), qid = list()))
  }

  progress_bar <- new_progress_bar(length(split_dict), quiet = quiet)
  on.exit(close_progress_bar(progress_bar), add = TRUE)
  for (i in seq_along(split_dict)) {
    x <- split_dict[[i]]
    # Remove names so they don't interfere with grouping
    x$level <- setNames(x$level, NULL)
    x$label <- setNames(x$label, NULL)
    split_dict[[i]] <- x
    tick_progress_bar(progress_bar, i)
  }

  split_dict |>
    map(select, label, level) |>
    enframe(value = "pair") |>
    group_by(pair) |>
    summarize(qid = list(name), .groups = "drop")
}

#' Build Validation Findings for repaired Dictionary Variable Names
#' @noRd
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

#' Build Validation Findings for Dictionary Variable Names
#' @noRd
variable_name_validation_findings <- function(dict) {
  names_by_response_column <- tibble(
    response_column_id = as.character(dict_response_column_id(dict)),
    variable_name = as.character(dict_variable_name(dict))
  ) |>
    distinct()

  bind_rows(
    inconsistent_response_column_names(names_by_response_column),
    duplicate_variable_names(names_by_response_column),
    unsafe_variable_names(names_by_response_column)
  )
}

#' Find inconsistent Dictionary Variable Names by Response Column ID
#' @noRd
inconsistent_response_column_names <- function(names_by_response_column) {
  findings <- names_by_response_column |>
    group_by(.data$response_column_id) |>
    filter(n_distinct(.data$variable_name) > 1) |>
    ungroup()

  if (nrow(findings) == 0) {
    return(empty_validation_findings())
  }

  findings$finding <- "inconsistent_variable_name"
  findings$reason <- "response_column_id_not_one_to_one"
  normalize_validation_findings(findings)
}

#' Find duplicate Dictionary Variable Names
#' @noRd
duplicate_variable_names <- function(names_by_response_column) {
  findings <- names_by_response_column |>
    group_by(.data$variable_name) |>
    filter(n_distinct(.data$response_column_id) > 1) |>
    ungroup()

  if (nrow(findings) == 0) {
    return(empty_validation_findings())
  }

  findings$finding <- "duplicate_variable_name"
  findings$reason <- "variable_name_not_unique"
  normalize_validation_findings(findings)
}

#' Find unsafe Dictionary Variable Names
#' @noRd
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

#' Build Validation Findings for level-label issues
#' @noRd
level_label_validation_findings <- function(mistake) {
  if (nrow(mistake) == 0) {
    return(empty_validation_findings())
  }

  mistake$finding <- "level_label_mistake"
  mistake$variable_name <- mistake$item_name
  normalize_validation_findings(mistake)
}

#' Run the level-label tests for one Response Column ID
#'
#' Returns the four level-label tests as a logical vector, in the order whose
#' tripped 1-based positions are concatenated into the `mistake` code (so
#' `"124"` means tests 1, 2 and 4 fired):
#' 1. label and level are not a one-to-one mapping,
#' 2. the non-negative levels are not a contiguous step-1 run,
#' 3. one label is carried by more than one row,
#' 4. one level is carried by more than one row.
#'
#' This is the one predicate behind both `dict_validate()` and
#' `assert_dict_valid()`, so the reported findings and the Labelled Export gate
#' cannot drift apart.
#'
#' @param cols A data frame whose first column holds labels and whose second
#' column holds levels.
#' @noRd
level_label_mistake_tests <- function(cols) {
  # Here recode is sometimes "none" and will cause a warning
  col2_numeric <- suppressWarnings(as.numeric(cols[[2]]))
  col2_pos <- subset(col2_numeric, col2_numeric >= 0)

  c(
    # Check correspondence
    !is_onetoone(cols),
    # Check constant step == 1
    !(all(diff(sort(col2_pos)) == 1) | length(diff(col2_pos)) == 0),
    # Check duplication
    anyDuplicated(cols[[1]]) > 0,
    anyDuplicated(cols[[2]]) > 0
  )
}

#' Build the level-label mistake rows for one Response Column ID
#' @noRd
level_label_mistake_rows <- function(
  dat,
  response_column_id,
  cols,
  has_mistake
) {
  item_name <- dict_variable_name(dat)[
    dict_response_column_id(dat) == response_column_id
  ]

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

#' Check one Response Column ID for level-label issues
#' @noRd
check_item <- function(dat, response_column_id) {
  cols <- dat[c("label", "level")]
  has_mistake <- level_label_mistake_tests(cols)

  if (any(has_mistake)) {
    level_label_mistake_rows(dat, response_column_id, cols, has_mistake)
  }
}

#' Check one Response Column ID for Export-blocking level-label issues
#'
#' Runs the same tests on the same rows as `check_item()` but keeps only the
#' Response Column IDs whose tripped tests are Export-blocking, and never runs
#' the level-label pairing summary, so the Labelled Export gate stays cheap.
#' @noRd
check_item_export_blocking <- function(dat, response_column_id) {
  cols <- dat[c("label", "level")]

  # A label-level mapping can only fail to be one-to-one when some label or
  # some level is repeated, so tests 3 and 4 alone decide whether a Response
  # Column ID can be Export-blocking. Testing them first is what keeps the gate
  # off the one-to-one scan for the Response Column IDs that pass, which is
  # nearly all of them.
  if (anyDuplicated(cols[[1]]) == 0 && anyDuplicated(cols[[2]]) == 0) {
    return(NULL)
  }

  has_mistake <- level_label_mistake_tests(cols)

  if (any(has_mistake[export_blocking_mistake_tests()])) {
    level_label_mistake_rows(dat, response_column_id, cols, has_mistake)
  }
}

#' Check split Variable Dictionary rows for level-label issues
#' @noRd
check_json <- function(split_jsons, quiet = TRUE) {
  progress_bar <- new_progress_bar(length(split_jsons), quiet = quiet)
  on.exit(close_progress_bar(progress_bar), add = TRUE)

  mistakes <- vector("list", length(split_jsons))
  for (i in seq_along(split_jsons)) {
    mistakes[i] <- list(check_item(split_jsons[[i]], names(split_jsons)[[i]]))
    tick_progress_bar(progress_bar, i)
  }

  mistakes <- bind_rows(mistakes)
  if (nrow(mistakes) > 0) {
    return(mistakes)
  } else {
    return(tibble())
  }
}
