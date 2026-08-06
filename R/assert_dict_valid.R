#' Test positions whose level-label mistake blocks Labelled Export
#'
#' Positions in `level_label_mistake_tests()` whose finding makes Labelled
#' Export wrong or impossible: a level-label mapping that is not one-to-one
#' (1), one label carried by several rows (3), and one level carried by
#' several rows (4). A level carried by several rows is the crashing shape:
#' Qualtrics keys an export column on the choice recode, so two choices
#' sharing a recode become one export column carrying both meanings, and the
#' value-label arithmetic in `survey_var_label_facts()` then has more labels
#' than levels.
#'
#' Test 2 (the non-negative levels are not a contiguous step-1 run) is
#' deliberately not Export-blocking. Gapped recodes are ordinary Qualtrics
#' survey design and label correctly.
#' @noRd
export_blocking_mistake_tests <- function() {
  c(1L, 3L, 4L)
}

#' Return Export-blocking Validation Findings for a Variable Dictionary
#'
#' Runs `level_label_mistake_tests()` over the same rows and the same grouping
#' `dict_validate()` uses, and keeps the Response Column IDs whose tripped
#' tests are Export-blocking. The level-label pairing summary is never built,
#' so this stays cheap enough to gate every Labelled Export.
#' @noRd
export_blocking_validation_findings <- function(dict) {
  split_dict <- level_label_validation_groups(dict)

  mistakes <- vector("list", length(split_dict))
  for (i in seq_along(split_dict)) {
    mistakes[i] <- list(
      check_item_export_blocking(split_dict[[i]], names(split_dict)[[i]])
    )
  }

  level_label_validation_findings(bind_rows(mistakes))
}

#' Return positions in `x` holding `value`, matching `NA` to `NA`
#' @noRd
value_positions <- function(x, value) {
  if (is.na(value)) {
    return(which(is.na(x)))
  }

  which(!is.na(x) & x == value)
}

#' Group the values sharing each repeated key
#'
#' Returns one entry per key value carried by more than one row, holding that
#' key's values in row order. Keys are compared the way `anyDuplicated()`
#' counts them, so a repeated `NA` key is a group.
#' @noRd
repeated_key_groups <- function(keys, values) {
  repeated <- unique(keys[duplicated(keys)])
  groups <- map(repeated, ~ values[value_positions(keys, .x)])
  setNames(groups, as.character(repeated))
}

#' Format a level for an Export-blocking Validation Finding message
#' @noRd
format_finding_level <- function(level) {
  if (is.na(level)) {
    return("<NA>")
  }

  level
}

#' Format labels for an Export-blocking Validation Finding message
#' @noRd
format_finding_examples <- function(values, max_examples) {
  shown <- head(values, max_examples)
  shown <- ifelse(
    is.na(shown),
    "<NA>",
    paste0("\"", truncate_finding_value(shown), "\"")
  )
  formatted <- toString(shown)

  if (length(values) > length(shown)) {
    formatted <- paste0(
      formatted,
      " and ",
      length(values) - length(shown),
      " more"
    )
  }

  formatted
}

#' Shorten a long label for an Export-blocking Validation Finding message
#' @noRd
truncate_finding_value <- function(value, max_characters = 60) {
  ifelse(
    nchar(value) > max_characters,
    paste0(substr(value, 1, max_characters - 3), "..."),
    value
  )
}

#' Describe why one Response Column ID blocks Labelled Export
#' @noRd
export_blocking_reason <- function(label, level, max_examples) {
  repeated_levels <- repeated_key_groups(level, label)
  if (length(repeated_levels) > 0) {
    return(paste0(
      "level ",
      format_finding_level(names(repeated_levels)[[1]]),
      " carries ",
      length(repeated_levels[[1]]),
      " labels \u2014 ",
      format_finding_examples(repeated_levels[[1]], max_examples),
      collision_suffix(length(repeated_levels) - 1, "level")
    ))
  }

  repeated_labels <- repeated_key_groups(label, level)
  if (length(repeated_labels) > 0) {
    return(paste0(
      "label ",
      format_finding_examples(names(repeated_labels)[[1]], 1),
      " is carried by ",
      length(repeated_labels[[1]]),
      " levels \u2014 ",
      format_finding_examples(repeated_labels[[1]], max_examples),
      collision_suffix(length(repeated_labels) - 1, "label")
    ))
  }

  "label and level are not a one-to-one mapping"
}

#' Report how many further collisions one Response Column ID carries
#' @noRd
collision_suffix <- function(remaining, unit) {
  if (remaining <= 0) {
    return("")
  }

  paste0(
    " (and ",
    remaining,
    " more repeated ",
    unit,
    if (remaining == 1) "" else "s",
    " in this column)"
  )
}

#' Build the error bullets for Export-blocking Validation Findings
#' @noRd
export_blocking_bullets <- function(
  findings,
  max_columns = 10,
  max_examples = 3
) {
  response_column_ids <- unique(findings$response_column_id)
  shown <- head(response_column_ids, max_columns)

  bullets <- map_chr(shown, function(response_column_id) {
    rows <- findings[findings$response_column_id == response_column_id, ]
    paste0(
      response_column_id,
      " (",
      rows$variable_name[[1]],
      "): ",
      export_blocking_reason(rows$label, rows$level, max_examples)
    )
  })

  if (length(response_column_ids) > length(shown)) {
    bullets <- c(
      bullets,
      paste0(
        "... and ",
        length(response_column_ids) - length(shown),
        " more Response Column IDs."
      )
    )
  }

  bullets
}

#' Build the Export-blocking Validation Findings error condition message
#' @noRd
export_blocking_message <- function(findings) {
  bullets <- export_blocking_bullets(findings)

  hints <- c(
    paste(
      "Levels and labels are reported as the Qualtrics survey defines them;",
      "qualtdict does not repair them."
    ),
    paste(
      "Use `dict_validate()` to list every Validation Finding without",
      "erroring."
    ),
    paste(
      "To download anyway, use `exclude_findings = \"validation\"` to drop",
      "the affected Export Variables, or",
      "`require_valid_dict = FALSE` to keep them with unreliable value",
      "labels."
    )
  )

  c(
    paste(
      "Variable Dictionary has level-label codings that break Labelled",
      "Export."
    ),
    setNames(bullets, rep("x", length(bullets))),
    setNames(hints, rep("i", length(hints)))
  )
}

#' Require a Variable Dictionary that can be exported
#'
#' Error when a Variable Dictionary carries Export-blocking Validation
#' Findings: level-label codings that make Labelled Export wrong or
#' impossible. `assert_dict_valid()` is the gate
#' [fetch_labelled_survey_data()] applies before downloading responses, so a
#' defective Variable Dictionary is reported by Response Column ID instead of
#' aborting deep inside labelling after the download is paid for.
#'
#' Export-blocking Validation Findings are the level-label findings where
#' label and level are not a one-to-one mapping, one label is carried by
#' several rows, or one level is carried by several rows. A level carried by
#' several rows is the shape that aborts Labelled Export: Qualtrics keys an
#' export column on the choice recode, so two choices sharing a recode are
#' exported as one column carrying both meanings.
#'
#' A gapped level sequence is not Export-blocking. Gapped recodes are ordinary
#' Qualtrics survey design and label correctly.
#'
#' Levels and labels are reported as the Qualtrics survey defines them.
#' qualtdict neither merges the rows nor repairs the coding, so the finding
#' stays visible as the survey-authoring error it is. [dict_validate()] stays
#' total: it reports every Validation Finding and never errors, so it can be
#' used to survey the damage.
#'
#' @inheritParams dict_validate
#'
#' @return `dict`, invisibly, when it carries no Export-blocking Validation
#' Findings. Otherwise an error of class
#' `qualtdict_export_blocking_findings`, whose `findings` field holds the
#' Export-blocking Validation Findings.
#'
#' @seealso [dict_validate()] for the total, never-erroring report.
#'
#' @export
#' @examples
#' dict <- data.frame(
#'   response_column_id = c("QID1", "QID1"),
#'   variable_name = "q1",
#'   label = c("Yes", "No"),
#'   level = c("1", "2")
#' )
#' class(dict) <- c("qualtdict", class(dict))
#'
#' assert_dict_valid(dict)
#'
#' # Two choices sharing one Qualtrics recode are exported as one column.
#' broken <- dict
#' broken$level <- c("1", "1")
#'
#' try(assert_dict_valid(broken))
assert_dict_valid <- function(dict) {
  checkarg_isqualtdict(dict)

  findings <- export_blocking_validation_findings(dict)
  if (nrow(findings) == 0) {
    return(invisible(dict))
  }

  rlang::abort(
    export_blocking_message(findings),
    class = "qualtdict_export_blocking_findings",
    findings = findings
  )
}
