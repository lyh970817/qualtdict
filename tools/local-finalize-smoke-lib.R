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

hash_smoke_list <- function(x) {
  temp <- tempfile(fileext = ".rds")
  on.exit(unlink(temp), add = TRUE)
  saveRDS(x, temp, version = 2)
  unname(tools::md5sum(temp))
}

project_smoke_record <- function(record, selected_functions) {
  selected_summary_names <- smoke_summary_names(selected_functions)
  selected_summary_names <- names(record$summaries)[
    names(record$summaries) %in% selected_summary_names
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
