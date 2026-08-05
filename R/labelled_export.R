#' Default extra columns retained in Labelled Survey Data
#' @noRd
default_extra_columns <- function() {
  c("externalDataReference", "startDate", "endDate")
}

#' Response metadata columns retained in Labelled Survey Data
#'
#' Maps each canonical response-metadata column name emitted in Labelled Survey
#' Data to the Qualtrics export column spellings it can arrive as. The first
#' candidate present in the downloaded data wins. \code{import_id = TRUE} (the
#' owned fetch setting) yields camelCase import ids such as \code{progress} and
#' \code{_recordId}; the label-based export uses PascalCase such as
#' \code{Progress} and \code{ResponseId}. Both are normalised to the canonical
#' name.
#' @noRd
response_meta_columns <- function() {
  list(
    startDate = c("startDate", "StartDate"),
    endDate = c("endDate", "EndDate"),
    recordedDate = c("recordedDate", "RecordedDate"),
    ResponseId = c("_recordId", "ResponseId", "ResponseID", "responseId"),
    Progress = c("progress", "Progress")
  )
}

#' Resolve present response metadata columns, normalised to canonical names
#'
#' Returns a named list of columns pulled from downloaded survey data, one entry
#' per canonical response-metadata name that is present. Best effort: canonical
#' names already among \code{exclude} (already emitted) and names with no
#' present candidate are skipped silently, so fixtures lacking response
#' metadata degrade gracefully rather than erroring.
#' @noRd
resolve_response_meta_columns <- function(dat, exclude = character()) {
  spec <- response_meta_columns()
  resolved <- list()
  for (canonical in names(spec)) {
    if (canonical %in% exclude) {
      next
    }
    present <- intersect(spec[[canonical]], colnames(dat))
    if (length(present) > 0) {
      resolved[[canonical]] <- dat[[present[[1]]]]
    }
  }

  resolved
}

#' Append normalised response metadata columns to Labelled Survey Data
#' @noRd
add_response_meta_columns <- function(out, dat, include = TRUE) {
  if (!include) {
    return(out)
  }

  resolved <- resolve_response_meta_columns(dat, exclude = colnames(out))
  if (length(resolved) == 0) {
    return(out)
  }

  bind_cols(out, as_tibble(resolved))
}

#' Arguments owned by qualtdict when fetching survey data
#' @noRd
owned_fetch_survey_args <- function() {
  c(
    "surveyID",
    "import_id",
    "importId",
    "convert",
    "label",
    "breakout_sets",
    "breakoutSets",
    "include_qids"
  )
}

#' Prepare arguments for the owned fetch_survey call
#' @noRd
prepare_fetch_survey_args <- function(args, dict) {
  blocked_args <- intersect(names(args), owned_fetch_survey_args())
  if (length(blocked_args) > 0) {
    blocked_arg_names <- paste0("`", blocked_args, "`")
    blocked_message <- toString(blocked_arg_names)
    rlang::abort(c(
      "These `fetch_survey()` arguments are owned by qualtdict.",
      i = paste0("Remove: ", blocked_message)
    ))
  }

  surveyID <- attr(dict, "surveyID", exact = TRUE)
  if (is.null(surveyID)) {
    rlang::abort(c(
      "`dict` is missing its `surveyID` attribute.",
      i = "Regenerate it with `dict_generate()`."
    ))
  }

  args$force_request <- TRUE
  args$surveyID <- surveyID
  args$import_id <- TRUE
  args$convert <- FALSE
  args$label <- FALSE
  args$breakout_sets <- TRUE
  args
}

#' Exclude Variable Dictionary rows with selected findings
#' @noRd
exclude_dict_findings <- function(
  dict,
  exclude_findings = c("none", "validation"),
  quiet = TRUE
) {
  exclude_findings <- match.arg(exclude_findings)
  if (exclude_findings == "none") {
    return(dict)
  }

  excluded_response_column_ids <- character()

  if (exclude_findings == "validation") {
    validation <- dict_validate(dict, quiet = quiet)
    validation_findings <- validation$validation_findings
    excluded_response_column_ids <- c(
      excluded_response_column_ids,
      validation_findings$response_column_id
    )
  }

  excluded_response_column_ids <- unique(
    excluded_response_column_ids[!is.na(excluded_response_column_ids)]
  )

  exclude_rows <- dict_response_column_id(dict) %in%
    excluded_response_column_ids

  copy_qualtdict_attrs(dict[!exclude_rows, ], dict)
}

#' Copy qualtdict object attributes after row filtering
#' @noRd
copy_qualtdict_attrs <- function(dict, source) {
  attr(dict, "class") <- attr(source, "class")
  attr(dict, "surveyID") <- attr(source, "surveyID", exact = TRUE)
  attr(dict, "survey_name") <- attr(source, "survey_name", exact = TRUE)
  attr(dict, "variable_name_findings") <-
    filter_variable_findings(
      attr(source, "variable_name_findings", exact = TRUE),
      dict
    )
  dict
}

#' Filter Variable Name repair findings to retained rows
#' @noRd
filter_variable_findings <- function(findings, dict) {
  if (is.null(findings) || nrow(findings) == 0) {
    return(empty_variable_name_findings())
  }

  findings[
    findings$response_column_id %in% unique(dict_response_column_id(dict)),
  ]
}

#' Empty Labelled Export Findings table
#' @noRd
empty_labelled_export_findings <- function() {
  tibble(
    finding = character(),
    response_column_id = character(),
    qid = character(),
    variable_name = character(),
    reason = character()
  )
}

#' Return Labelled Export Findings
#'
#' Labelled Export Findings describe issues detected while matching a Variable
#' Dictionary to downloaded survey data.
#'
#' @param x Labelled Survey Data returned by [fetch_labelled_survey_data()].
#'
#' @return A data frame of Labelled Export Findings.
#' @export
#' @examples
#' dat <- data.frame(q1 = "1")
#' attr(dat, "labelled_export_findings") <- data.frame(
#'   finding = "missing_response_column_id",
#'   response_column_id = "QID2",
#'   qid = "QID2",
#'   variable_name = "q2",
#'   reason = "not_found_in_downloaded_survey_data"
#' )
#'
#' labelled_export_findings(dat)
labelled_export_findings <- function(x) {
  findings <- attr(x, "labelled_export_findings", exact = TRUE)
  if (is.null(findings)) {
    return(empty_labelled_export_findings())
  }

  findings
}

#' Find Variable Dictionary rows missing from downloaded survey data
#' @noRd
missing_response_column_findings <- function(dict, dat) {
  response_column_id <- dict_response_column_id(dict)
  missing <- !response_column_id %in% colnames(dat)
  if (!any(missing)) {
    return(empty_labelled_export_findings())
  }

  qid <- response_column_id
  if ("qid" %in% names(dict)) {
    qid <- dict$qid
  }

  tibble(
    finding = "missing_response_column_id",
    response_column_id = as.character(response_column_id[missing]),
    qid = as.character(qid[missing]),
    variable_name = as.character(dict_variable_name(dict)[missing]),
    reason = "not_found_in_downloaded_survey_data"
  ) |>
    distinct(.data$response_column_id, .keep_all = TRUE)
}

#' Resolve retained extra columns from downloaded survey data
#' @noRd
resolve_extra_columns <- function(
  dat,
  extra_columns,
  extra_columns_user_supplied
) {
  if (is.null(extra_columns)) {
    return(character())
  }

  missing_extra_columns <- setdiff(extra_columns, colnames(dat))
  if (length(missing_extra_columns) > 0) {
    missing_message <- paste0(
      "`",
      missing_extra_columns,
      "`",
      collapse = ", "
    )
    source_label <- if (extra_columns_user_supplied) {
      "user-specified"
    } else {
      "default"
    }

    # Absent columns are omitted rather than fatal, so callers can pass several
    # candidate names (for example ordered id-column fallbacks) where only one
    # is present in a given export.
    warning(
      "Missing ",
      source_label,
      " `extra_columns` in downloaded survey data: ",
      missing_message,
      ". Absent columns are omitted.",
      call. = FALSE
    )
  }

  extra_columns[extra_columns %in% colnames(dat)]
}

#' Rename and label downloaded survey data from a Variable Dictionary
#' @noRd
survey_recode <- function(
  dict,
  dat,
  extra_columns = default_extra_columns(),
  extra_columns_user_supplied = FALSE,
  unanswer_recode,
  unanswer_recode_multi
) {
  # Response metadata is included best-effort unless the caller opts out of all
  # extra columns with `extra_columns = NULL`.
  include_response_meta <- !is.null(extra_columns)

  response_column_id <- dict_response_column_id(dict)
  in_dat <- response_column_id %in% colnames(dat)
  dict <- dict[in_dat, ]
  response_column_id <- response_column_id[in_dat]
  unique_response_column_ids <- unique(response_column_id)
  unique_varnames <- unique(dict_variable_name(dict))

  extra_columns <- resolve_extra_columns(
    dat = dat,
    extra_columns = extra_columns,
    extra_columns_user_supplied = extra_columns_user_supplied
  )
  extra_columns <- setdiff(extra_columns, unique_response_column_ids)
  dat_cols <- unique(c(extra_columns, unique_response_column_ids))

  if (length(unique_response_column_ids) == 0) {
    return(add_response_meta_columns(dat[dat_cols], dat, include_response_meta))
  }

  varnames <- setNames(unique_response_column_ids, unique_varnames)
  renamed <- rename(dat[dat_cols], !!!varnames)

  # level = unique to preserve ordering
  split_dict <- split(
    dict,
    factor(response_column_id, levels = unique_response_column_ids)
  )
  dat_vars <- map2_df(
    renamed[unique_varnames],
    split_dict,
    ~ survey_var_recode(
      .x,
      .y,
      unanswer_recode = unanswer_recode,
      unanswer_recode_multi = unanswer_recode_multi
    )
  )

  out <- bind_cols(renamed[extra_columns], dat_vars)
  add_response_meta_columns(out, dat, include_response_meta)
}


#' Apply variable and value labels to one Export Variable
#' @importFrom sjlabelled set_label set_labels
#' @importFrom haven read_xpt
#' @noRd
survey_var_recode <- function(
  var,
  var_dict,
  unanswer_recode,
  unanswer_recode_multi
) {
  context <- survey_var_recode_context(var_dict)

  if (survey_var_should_coerce_numeric(context)) {
    # Check for content_type numeric,
    # vector with numbers such as "06" passes validation on Qualtrics but
    # will be character when read by readr

    var <- as.numeric(var)
  }

  label_facts <- survey_var_label_facts(
    var_dict,
    context,
    unanswer_recode = unanswer_recode,
    unanswer_recode_multi = unanswer_recode_multi
  )

  # TE variables dont have levels or labels
  if (all(is.na(label_facts$levels))) {
    text_label <- unique(paste_narm(var_dict[["question"]], var_dict[["item"]]))
    var <- set_label(var, label = text_label)
  } else {
    var <- set_labels(
      var,
      labels = setNames(label_facts$levels, label_facts$labels)
    )
  }

  return(var)
}

#' Build labelling context for one Export Variable
#' @noRd
survey_var_recode_context <- function(var_dict) {
  # Multiple rows for a question, only first one chosen.
  type <- var_dict[["type"]][1]
  levels <- var_dict[["level"]]

  list(
    type = type,
    content_type = var_dict[["content_type"]][[1]],
    levels = levels,
    labels = var_dict[["label"]],
    is_metadata_defined_row = dict_metadata_defined_rows(var_dict)[[1]],
    is_text_var = isTRUE(type == "TE") ||
      any(grepl("_TEXT", levels, fixed = TRUE), na.rm = TRUE),
    is_choice_tick_column = dict_choice_tick_column(var_dict),
    is_display_order_column = dict_display_order_column(var_dict),
    has_value_labels = !all(is.na(levels))
  )
}

#' Return whether Variable Dictionary rows describe a per-choice tick column
#'
#' Display-order columns ride a multiple-answer question's type/selector but
#' are not tick columns: the cell holds the position at which one choice was
#' displayed, not whether it was selected. They are excluded here so the
#' unticked/unseen recodes are never declared against them.
#' @noRd
dict_choice_tick_column <- function(var_dict) {
  if (dict_display_order_column(var_dict)) {
    return(FALSE)
  }

  question_type_renders_choice_tick_columns(list(
    type = dict_first_or_na(var_dict[["type"]]),
    selector = dict_first_or_na(var_dict[["selector"]]),
    sub_selector = dict_first_or_na(var_dict[["sub_selector"]])
  ))
}

#' Return whether Variable Dictionary rows describe a display-order column
#'
#' A Loop and Merge expansion prefixes the Response Column ID with its loop
#' option, which the package predicate does not expect, so the prefix is
#' stripped before testing.
#' @noRd
dict_display_order_column <- function(var_dict) {
  response_column_id <- dict_first_or_na(dict_response_column_id(var_dict))
  if (is.na(response_column_id)) {
    return(FALSE)
  }

  is_display_order_response_column(
    sub("^[^_]+_(QID[0-9]+_)", "\\1", response_column_id)
  )
}

#' Return the first value of a Variable Dictionary column, or NA
#' @noRd
dict_first_or_na <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(NA_character_)
  }
  as.character(x)[[1]]
}

#' Return whether one Export Variable should be coerced to numeric
#' @noRd
survey_var_should_coerce_numeric <- function(context) {
  !context$is_text_var &&
    !is.na(context$content_type) &&
    context$content_type == "Number"
}

#' Build value-label facts for one Export Variable
#' @noRd
survey_var_label_facts <- function(
  var_dict,
  context,
  unanswer_recode,
  unanswer_recode_multi
) {
  levels <- context$levels
  labels <- context$labels

  if (survey_var_carries_no_value_labels(context)) {
    return(list(levels = NA_character_, labels = labels))
  }

  if (context$is_choice_tick_column) {
    # One export column per choice. The choice RecodeValue names the column;
    # the cell holds a membership indicator, so the Level universe is the tick
    # domain plus whatever the export recodes unticked and unseen cells to.
    if (!is.null(unanswer_recode_multi)) {
      levels <- c(levels, unanswer_recode_multi)
      labels <- c(labels, paste("Not", labels))
    }
    if (!is.null(unanswer_recode)) {
      levels <- c(levels, unanswer_recode)
      labels <- c(labels, "Seen but not answered")
    }
  } else if (nrow(var_dict) == 1) {
    # Single row means allowing for multiple answer
    if (!is.null(unanswer_recode_multi)) {
      levels <- c(levels, unanswer_recode_multi)
      labels <- c(labels, paste("Not", labels))
    }
  } else if (nrow(var_dict) > 1) {
    # If multiple rows it's ordinal
    labels <- grep("TEXT", labels, invert = TRUE, value = TRUE, fixed = TRUE)
    levels <- grep("TEXT", levels, invert = TRUE, value = TRUE, fixed = TRUE)
    if (!is.null(unanswer_recode)) {
      levels <- c(levels, unanswer_recode)
      labels <- c(labels, "Seen but not answered")
    }
  }

  list(levels = levels, labels = labels)
}

#' Return whether an Export Variable carries no value labels
#'
#' Free text has no value universe. Neither does a display-order column: it
#' stores the position at which one choice was displayed, and the
#' unticked/unseen recodes describe a tick it never holds. A Metadata-defined
#' row with no declared Level has none either.
#' @noRd
survey_var_carries_no_value_labels <- function(context) {
  context$is_text_var ||
    context$is_display_order_column ||
    (context$is_metadata_defined_row && !context$has_value_labels)
}
