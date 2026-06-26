smoke_functions <- function() {
  c(
    "dict_generate",
    "dict_validate",
    "get_survey_data",
    "labelled_export_findings",
    "dict_split_blocks",
    "survey_split_blocks"
  )
}

parse_smoke_functions <- function(value, supported = smoke_functions()) {
  if (is.null(value)) {
    return(supported)
  }

  selected <- trimws(strsplit(value, ",", fixed = TRUE)[[1]])
  selected <- selected[nzchar(selected)]

  if (length(selected) == 0) {
    stop(
      "Select at least one smoke-covered exported function with `--functions`.",
      call. = FALSE
    )
  }

  unknown <- setdiff(selected, supported)
  if (length(unknown) > 0) {
    stop(
      "Unknown smoke-covered exported function",
      if (length(unknown) == 1) "" else "s",
      ": ",
      paste(unknown, collapse = ", "),
      ". Supported values: ",
      paste(supported, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  unique(selected)
}

smoke_scenario_requirements <- function(selected_functions) {
  needs_labelled <- any(c(
    "get_survey_data",
    "labelled_export_findings",
    "survey_split_blocks"
  ) %in% selected_functions)

  list(
    needs_dict = length(selected_functions) > 0,
    needs_validation = "dict_validate" %in% selected_functions,
    needs_labelled = needs_labelled,
    needs_export_findings = "labelled_export_findings" %in% selected_functions,
    needs_dict_blocks = "dict_split_blocks" %in% selected_functions,
    needs_survey_blocks = "survey_split_blocks" %in% selected_functions
  )
}

smoke_summary_names <- function(functions) {
  output_map <- c(
    dict_generate = "dict",
    dict_validate = "validation",
    get_survey_data = "labelled",
    labelled_export_findings = "labelled_export_findings",
    dict_split_blocks = "dict_blocks",
    survey_split_blocks = "survey_blocks"
  )

  summaries <- unname(output_map[functions])
  if ("get_survey_data" %in% functions) {
    summaries <- c(summaries, "labelled_excluding_validation")
  }
  unique(summaries)
}

smoke_result_line <- function(result, status) {
  dict_summary <- result$summaries[["dict"]]
  labelled_summary <- result$summaries[["labelled"]]
  message <- paste0(
    result$alias,
    " / ",
    result$variable_name,
    ": ",
    status,
    " hash=",
    result$scenario_hash
  )

  if (!is.null(dict_summary) && !is.null(labelled_summary)) {
    return(paste0(
      message,
      " dict_rows=",
      dict_summary$rows,
      " labelled_rows=",
      labelled_summary$rows,
      " labelled_cols=",
      labelled_summary$columns
    ))
  }

  paste0(
    message,
    " outputs=",
    paste(names(result$summaries), collapse = ",")
  )
}

smoke_mismatch_lines <- function(current, baseline) {
  current_dict <- current$summaries[["dict"]]
  baseline_dict <- baseline$summaries[["dict"]]
  current_labelled <- current$summaries[["labelled"]]
  baseline_labelled <- baseline$summaries[["labelled"]]
  current_validation <- current$summaries[["validation"]]
  baseline_validation <- baseline$summaries[["validation"]]

  lines <- c(
    paste0("Mismatch: ", current$alias, " / ", current$variable_name),
    paste0("  baseline hash: ", baseline$scenario_hash),
    paste0("  current hash:  ", current$scenario_hash),
    paste0("  outputs: ", paste(names(current$summaries), collapse = ", "))
  )

  if (!is.null(current_dict) && !is.null(baseline_dict)) {
    lines <- c(
      lines,
      paste0(
        "  dict rows: ",
        baseline_dict$rows,
        " -> ",
        current_dict$rows
      )
    )
  }
  if (!is.null(current_labelled) && !is.null(baseline_labelled)) {
    lines <- c(
      lines,
      paste0(
        "  labelled dims: ",
        baseline_labelled$rows,
        "x",
        baseline_labelled$columns,
        " -> ",
        current_labelled$rows,
        "x",
        current_labelled$columns
      )
    )
  }
  if (!is.null(current_validation) && !is.null(baseline_validation)) {
    lines <- c(
      lines,
      paste0(
        "  validation findings rows: ",
        baseline_validation$validation_findings_rows,
        " -> ",
        current_validation$validation_findings_rows
      )
    )
  }

  lines
}

hash_smoke_list <- function(x) {
  temp <- tempfile(fileext = ".rds")
  on.exit(unlink(temp), add = TRUE)
  saveRDS(x, temp, version = 2)
  unname(tools::md5sum(temp))
}

project_smoke_record <- function(record, selected_functions) {
  selected_summary_names <- smoke_summary_names(selected_functions)

  if (all(names(record$summaries) %in% selected_summary_names)) {
    return(record)
  }

  selected_summary_names <- selected_summary_names[
    selected_summary_names %in% names(record$summaries)
  ]

  record$summaries <- record$summaries[selected_summary_names]
  record$object_hashes <- record$object_hashes[selected_summary_names]
  record$scenario_hash <- hash_smoke_list(record$object_hashes)
  record
}

merge_smoke_baseline <- function(existing, selected) {
  merged <- existing
  merged$generated_at <- selected$generated_at

  for (name in names(selected$summaries)) {
    merged$summaries[[name]] <- selected$summaries[[name]]
  }
  for (name in names(selected$object_hashes)) {
    merged$object_hashes[[name]] <- selected$object_hashes[[name]]
  }

  merged$scenario_hash <- hash_smoke_list(merged$object_hashes)
  merged
}
