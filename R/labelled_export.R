default_extra_columns <- function() {
  c("externalDataReference", "startDate", "endDate")
}

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

prepare_fetch_survey_args <- function(args, dict) {
  blocked_args <- intersect(names(args), owned_fetch_survey_args())
  if (length(blocked_args) > 0) {
    rlang::abort(c(
      "These `fetch_survey()` arguments are owned by qualtdict.",
      i = paste(
        "Remove:",
        paste(sprintf("`%s`", blocked_args), collapse = ", ")
      )
    ))
  }

  args$force_request <- TRUE
  args$surveyID <- attr(dict, "surveyID")
  args$import_id <- TRUE
  args$convert <- FALSE
  args$label <- FALSE
  args$breakout_sets <- TRUE
  args
}

exclude_dict_findings <- function(dict,
                                  exclude_findings = c("none", "validation")) {
  exclude_findings <- match.arg(exclude_findings)
  if (exclude_findings == "none") {
    return(dict)
  }

  excluded_response_column_ids <- character()

  if (exclude_findings == "validation") {
    validation_findings <- dict_validate(dict)$validation_findings
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

filter_variable_findings <- function(findings, dict) {
  if (is.null(findings) || nrow(findings) == 0) {
    return(empty_variable_name_findings())
  }

  findings[
    findings$response_column_id %in% unique(dict_response_column_id(dict)),
  ]
}

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
#' @param x Labelled Survey Data returned by [get_survey_data()].
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
  ) %>%
    distinct(.data$response_column_id, .keep_all = TRUE)
}

resolve_extra_columns <- function(dat,
                                  extra_columns,
                                  extra_columns_user_supplied) {
  if (is.null(extra_columns)) {
    return(character())
  }

  missing_extra_columns <- setdiff(extra_columns, colnames(dat))
  if (length(missing_extra_columns) > 0) {
    missing_message <- paste(
      sprintf("`%s`", missing_extra_columns),
      collapse = ", "
    )
    if (extra_columns_user_supplied) {
      rlang::abort(c(
        "Missing user-specified `extra_columns` in downloaded survey data.",
        i = paste("Missing:", missing_message)
      ))
    }

    warning(
      paste(
        "Missing default `extra_columns` in downloaded survey data:",
        missing_message
      ),
      call. = FALSE
    )
  }

  extra_columns[extra_columns %in% colnames(dat)]
}

#' Add labels to survey
#' @keywords internal
#' @noRd
survey_recode <- function(dict,
                          dat,
                          extra_columns = default_extra_columns(),
                          extra_columns_user_supplied = FALSE,
                          unanswer_recode,
                          unanswer_recode_multi) {
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
    return(dat[dat_cols])
  }

  varnames <- setNames(unique_response_column_ids, unique_varnames)
  dat <- rename(dat[dat_cols], !!!varnames)

  # level = unique to preserve ordering
  split_dict <- split(
    dict,
    factor(response_column_id, levels = unique_response_column_ids)
  )
  dat_vars <- map2_df(
    dat[unique_varnames], split_dict,
    ~ survey_var_recode(.x, .y,
      unanswer_recode = unanswer_recode,
      unanswer_recode_multi = unanswer_recode_multi
    )
  )

  bind_cols(dat[extra_columns], dat_vars)
}


#' Add labels to each variable in survey (`sjlabelled` uses `haven`)
#' @importFrom sjlabelled set_label set_labels
#' @importFrom haven read_xpt
#' @keywords internal
#' @noRd
survey_var_recode <- function(var,
                              var_dict,
                              unanswer_recode,
                              unanswer_recode_multi) {
  # Multiple rows for a question, only first one chosen
  type <- var_dict[["type"]][1]
  content_type <- var_dict[["content_type"]][[1]]
  levels <- var_dict[["level"]]
  labels <- var_dict[["label"]]
  is_text_var <- type == "TE" || any(grepl("_TEXT", levels))

  if (!is_text_var && !is.na(content_type) && content_type == "Number") {
    # Check for content_type numeric,
    # vector with numbers such as "06" passes validation on Qualtrics but
    # will be character when read by readr

    var <- as.numeric(var)
  }

  if (is_text_var) {
    levels <- NA

  } else if (nrow(var_dict) == 1) {
    # Single row means allowing for multiple answer
    if (!is.null(unanswer_recode_multi)) {
      levels <- c(levels, unanswer_recode_multi)
      labels <- c(labels, paste("Not", labels))
    }
  } else if (nrow(var_dict) > 1) {
    # If multiple rows it's ordinal
    labels <- grep("TEXT", labels, invert = TRUE, value = TRUE)
    levels <- grep("TEXT", levels, invert = TRUE, value = TRUE)
    if (!is.null(unanswer_recode)) {
      levels <- c(levels, unanswer_recode)
      labels <- c(labels, "Seen but not answered")
    }
  }

  # TE variables dont have levels or labels
  if (any(!is.na(levels))) {
    var <- set_labels(var, labels = setNames(levels, labels))
  } else {
    text_label <- unique(paste_narm(var_dict[["question"]], var_dict[["item"]]))
    var <- set_label(var, label = text_label)
  }


  return(var)
}
