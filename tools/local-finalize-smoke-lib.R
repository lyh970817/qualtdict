smoke_functions <- function() {
  c(
    "dict_generate",
    "dict_validate",
    "fetch_labelled_survey_data",
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

smoke_variable_names <- function() {
  "question_name"
}

parse_smoke_variable_names <- function(
  value,
  supported = smoke_variable_names()
) {
  if (is.null(value)) {
    return("question_name")
  }

  selected <- trimws(strsplit(value, ",", fixed = TRUE)[[1]])
  selected <- selected[nzchar(selected)]

  if (length(selected) == 0) {
    stop(
      "Select at least one Variable Dictionary route with `--variable-name`.",
      call. = FALSE
    )
  }
  disabled <- intersect(selected, c("semantic_name", "all"))
  if (length(disabled) > 0) {
    stop(
      "Semantic Name smoke route is disabled; use `--variable-name ",
      "question_name` or omit `--variable-name`.",
      call. = FALSE
    )
  }

  unknown <- setdiff(selected, supported)
  if (length(unknown) > 0) {
    stop(
      "Unknown Variable Dictionary route",
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
  needs_labelled <- any(
    c(
      "fetch_labelled_survey_data",
      "labelled_export_findings",
      "survey_split_blocks"
    ) %in%
      selected_functions
  )

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
  output_map <- list(
    dict_generate = c("dict", "level_universe"),
    dict_validate = "validation",
    fetch_labelled_survey_data = "labelled",
    labelled_export_findings = "labelled_export_findings",
    dict_split_blocks = "dict_blocks",
    survey_split_blocks = "survey_blocks"
  )

  summaries <- unlist(unname(output_map[functions]), use.names = FALSE)
  if ("fetch_labelled_survey_data" %in% functions) {
    summaries <- c(summaries, "labelled_excluding_validation")
  }
  unique(summaries)
}

smoke_full_summary_names <- function() {
  c(
    "dict",
    "level_universe",
    "validation",
    "labelled",
    "labelled_export_findings",
    "dict_blocks",
    "survey_blocks",
    "labelled_excluding_validation"
  )
}

# ---------------------------------------------------------------------------
# Level universe observation
#
# A Variable Dictionary row declares a `level` universe for its Response
# Column ID. Nothing in qualtdict ever checks that universe against the values
# the column actually stores, and the local smoke artifacts cannot supply the
# check: `sanitize_allowed_levels()` rewrites every non-missing value with
# values cycled from that same universe, so the sanitized responses are
# in-universe by construction.
#
# The fetch script therefore records a privacy-filtered observation of the RAW
# value universe in `manifest.json` (`level_universe`), before sanitization.
# The check recomputes the declared universe from the CURRENT dictionary and
# compares it with that recorded observation. Only aggregate value histograms
# are persisted, and only for columns whose values are short numeric codes.
# ---------------------------------------------------------------------------

level_universe_schema_version <- function() {
  1L
}

read_smoke_manifest <- function(path) {
  jsonlite::fromJSON(path, simplifyVector = FALSE)
}

level_universe_minimum_response_rows <- function() {
  100L
}

level_universe_max_distinct_codes <- function() {
  12L
}

level_universe_max_code_width <- function() {
  6L
}

level_universe_chr <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(character())
  }
  values <- as.character(unlist(x, use.names = FALSE))
  values[!is.na(values)]
}

#' Return whether observed values are safe to persist as codes
#'
#' Only short, numeric, low-cardinality value sets are retained. The rule
#' cannot admit free text, identifiers, dates, or high-cardinality numerics.
level_universe_code_safe <- function(values) {
  values <- level_universe_chr(values)
  if (length(values) == 0) {
    return(FALSE)
  }
  distinct <- unique(values)

  length(distinct) <= level_universe_max_distinct_codes() &&
    all(nchar(distinct) <= level_universe_max_code_width()) &&
    all(grepl("^-?[0-9]+(\\.[0-9]+)?$", distinct))
}

#' Return the Response Column IDs Qualtrics renders one-per-choice
#'
#' Keyed on the question type recorded in the Variable Dictionary, using the
#' package's own predicate, so the classification does not depend on what the
#' dictionary happens to declare as that column's Level universe.
level_universe_per_choice_columns <- function(dict) {
  renders_tick_columns <- getFromNamespace(
    "question_type_renders_choice_tick_columns",
    "qualtdict"
  )
  first_or_na <- function(x) {
    if (is.null(x)) {
      return(rep(NA_character_, nrow(dict)))
    }
    as.character(x)
  }

  type <- first_or_na(dict[["type"]])
  selector <- first_or_na(dict[["selector"]])
  sub_selector <- first_or_na(dict[["sub_selector"]])
  per_choice <- vapply(
    seq_len(nrow(dict)),
    function(index) {
      isTRUE(renders_tick_columns(list(
        type = type[[index]],
        selector = selector[[index]],
        sub_selector = sub_selector[[index]]
      )))
    },
    logical(1)
  )

  unique(as.character(dict[["response_column_id"]])[per_choice])
}

#' Classify a Response Column ID by its export shape
level_universe_shape <- function(response_column_id, per_choice_columns) {
  if (response_column_id %in% per_choice_columns) {
    return("one_column_per_choice")
  }

  "ordinary"
}

#' Compare one recorded observation with the current declared universe
#'
#' `declared_universe_drift` separates a dictionary that changed since the
#' fetch from a column whose stored values were never in its universe.
level_universe_compare <- function(
  observed,
  current_levels,
  per_choice_columns = character()
) {
  current_levels <- level_universe_chr(current_levels)
  recorded_levels <- level_universe_chr(observed$declared_levels)
  codes <- level_universe_chr(observed$codes)
  counts <- as.integer(unlist(observed$counts, use.names = FALSE))
  redacted <- isTRUE(observed$redacted)

  out_codes <- character()
  values_out_of_universe <- as.integer(
    observed$n_out_of_universe_at_fetch %||% 0L
  )
  if (!redacted) {
    outside <- !codes %in% current_levels
    out_codes <- codes[outside]
    values_out_of_universe <- sum(counts[outside])
  }

  # A `_TEXT` Level is a marker, not a value universe: `survey_var_recode()`
  # reads it as "this column holds free text" and attaches no value labels.
  # Free text has no universe to violate.
  is_text_universe <- length(current_levels) > 0 &&
    all(grepl("TEXT", current_levels, fixed = TRUE))
  if (is_text_universe) {
    out_codes <- character()
    values_out_of_universe <- 0L
  }

  status <- if (is_text_universe) {
    "text_column"
  } else if (!setequal(recorded_levels, current_levels)) {
    "declared_universe_drift"
  } else if (redacted) {
    "redacted_carry_forward"
  } else if (length(out_codes) > 0) {
    "data_violation"
  } else {
    "clean"
  }

  list(
    response_column_id = observed$response_column_id,
    shape = level_universe_shape(
      observed$response_column_id,
      per_choice_columns
    ),
    status = status,
    redacted = redacted,
    declared_levels_at_fetch = recorded_levels,
    declared_levels = current_levels,
    out_codes = out_codes,
    n_non_missing = as.integer(observed$n_non_missing %||% 0L),
    values_out_of_universe = as.integer(values_out_of_universe)
  )
}

#' Summarise a Level universe comparison for the smoke record
summarise_level_universe <- function(result) {
  statuses <- vapply(result$comparisons, `[[`, character(1), "status")
  shapes <- vapply(result$comparisons, `[[`, character(1), "shape")
  out_of_universe <- vapply(
    result$comparisons,
    `[[`,
    integer(1),
    "values_out_of_universe"
  )
  violating <- statuses == "data_violation"

  summary <- list(
    available = isTRUE(result$available),
    schema = as.integer(result$schema %||% NA_integer_),
    response_rows = as.integer(result$response_rows %||% NA_integer_),
    columns_with_universe = as.integer(result$columns_with_universe %||% 0L),
    columns_observed = length(result$comparisons),
    columns_by_status = as.list(table_counts(statuses)),
    columns_by_shape = as.list(table_counts(shapes)),
    violating_columns = as.integer(sum(violating)),
    values_observed = as.integer(result$values_observed %||% 0L),
    values_out_of_universe = as.integer(sum(out_of_universe)),
    one_column_per_choice_violations = as.integer(
      sum(violating & shapes == "one_column_per_choice")
    ),
    violation_fingerprint = level_universe_fingerprint(result$comparisons)
  )
  summary$object_hash <- hash_smoke_list(summary)
  summary
}

#' Build the per-column violation fingerprint carried in the blessed summary
level_universe_fingerprint <- function(comparisons) {
  violations <- Filter(
    function(comparison) identical(comparison$status, "data_violation"),
    comparisons
  )
  if (length(violations) == 0) {
    return(list())
  }

  fingerprint <- vapply(
    violations,
    function(comparison) {
      paste0(
        comparison$response_column_id,
        ":",
        paste(sort(comparison$declared_levels), collapse = "|"),
        "->",
        paste(sort(comparison$out_codes), collapse = "|")
      )
    },
    character(1)
  )

  as.list(sort(fingerprint))
}

#' Observe the raw value universe of every column with a declared universe
#'
#' Called on the RAW responses, before sanitization. Persists aggregate value
#' histograms only, and only for columns that pass `level_universe_code_safe()`.
level_universe_observation <- function(
  responses,
  allowed_levels,
  per_choice_columns = character()
) {
  observed <- list()
  columns_with_universe <- 0L
  values_observed <- 0L
  values_out_of_universe <- 0L
  values_out_of_universe_redacted <- 0L
  redacted_columns <- 0L

  for (name in names(responses)) {
    declared <- level_universe_chr(allowed_levels[[name]])
    if (length(declared) == 0) {
      next
    }
    columns_with_universe <- columns_with_universe + 1L

    values <- responses[[name]]
    values <- as.character(values[!is.na(values)])
    if (length(values) == 0) {
      next
    }

    counts <- table(values)
    codes <- names(counts)
    counts <- as.integer(counts)
    outside <- !codes %in% declared
    out_of_universe <- sum(counts[outside])
    safe <- level_universe_code_safe(codes)

    values_observed <- values_observed + length(values)
    values_out_of_universe <- values_out_of_universe + out_of_universe
    if (!safe) {
      redacted_columns <- redacted_columns + 1L
      values_out_of_universe_redacted <-
        values_out_of_universe_redacted + out_of_universe
    }

    record <- list(
      response_column_id = name,
      declared_levels = as.list(declared),
      shape = level_universe_shape(name, per_choice_columns),
      redacted = !safe,
      n_non_missing = length(values),
      n_out_of_universe_at_fetch = as.integer(out_of_universe)
    )
    if (safe) {
      record$codes <- as.list(codes)
      record$counts <- as.list(counts)
    }
    observed[[length(observed) + 1L]] <- record
  }

  list(
    schema = level_universe_schema_version(),
    response_rows = nrow(responses),
    totals = list(
      columns_with_universe = columns_with_universe,
      columns_observed = length(observed),
      values_observed = as.integer(values_observed),
      values_out_of_universe = as.integer(values_out_of_universe),
      redacted_columns = redacted_columns,
      values_out_of_universe_redacted = as.integer(
        values_out_of_universe_redacted
      )
    ),
    observed = observed
  )
}

table_counts <- function(x) {
  if (length(x) == 0) {
    return(list())
  }
  counts <- table(x)
  stats::setNames(as.integer(counts), names(counts))
}

`%||%` <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
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
  current_levels <- current$summaries[["level_universe"]]
  baseline_levels <- baseline$summaries[["level_universe"]]
  if (!is.null(current_levels) && !is.null(baseline_levels)) {
    lines <- c(
      lines,
      paste0(
        "  level universe violating columns: ",
        baseline_levels$violating_columns,
        " -> ",
        current_levels$violating_columns
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

order_smoke_record_summaries <- function(record) {
  known_names <- smoke_full_summary_names()
  summary_names <- names(record$summaries)
  ordered_summary_names <- c(
    known_names[known_names %in% summary_names],
    summary_names[!summary_names %in% known_names]
  )

  hash_names <- names(record$object_hashes)
  ordered_hash_names <- c(
    known_names[known_names %in% hash_names],
    hash_names[!hash_names %in% known_names]
  )

  record$summaries <- record$summaries[ordered_summary_names]
  record$object_hashes <- record$object_hashes[ordered_hash_names]
  record$scenario_hash <- hash_smoke_list(record$object_hashes)
  record
}

project_smoke_record <- function(record, selected_functions) {
  if (
    length(selected_functions) == length(smoke_functions()) &&
      setequal(selected_functions, smoke_functions())
  ) {
    return(order_smoke_record_summaries(record))
  }

  selected_summary_names <- smoke_summary_names(selected_functions)

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

  order_smoke_record_summaries(merged)
}

bless_smoke_record <- function(existing, selected, selective = FALSE) {
  if (!isTRUE(selective) || is.null(existing)) {
    return(selected)
  }

  merge_smoke_baseline(existing, selected)
}
