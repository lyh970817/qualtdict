#!/usr/bin/env Rscript

usage <- function() {
  cat(
    paste(
      "Usage:",
      "  Rscript tools/local-finalize-smoke.R check",
      "  Rscript tools/local-finalize-smoke.R bless",
      "  Rscript tools/local-finalize-smoke.R check --survey survey_a",
      "  Rscript tools/local-finalize-smoke.R check --survey-count all",
      "  Rscript tools/local-finalize-smoke.R check --functions fetch_labelled_survey_data",
      "  Rscript tools/local-finalize-smoke.R check --variable-name semantic_name",
      "",
      "Options:",
      "  --config PATH    Survey config JSON.",
      "  --root DIR       Local smoke artifact directory.",
      "  --survey ALIAS   Run one configured survey alias.",
      "  --survey-count N Randomly sample N configured surveys; default 2.",
      "                    Use `all` to run every configured survey.",
      "  --survey-seed N  Integer seed for reproducible survey sampling.",
      "  --functions NAMES  Comma-separated smoke-covered exported functions.",
      "  --variable-name NAMES  Comma-separated Variable Dictionary routes:",
      "                    question_name, semantic_name, or all. Default question_name.",
      "",
      "`check` compares current exported-function output hashes with local",
      "baselines. It also verifies Response Column ID parity against local",
      "raw fetched response columns. Full `bless` replaces local baselines",
      "with current output hashes. `bless --functions` updates selected",
      "output summaries inside existing baselines. Parity mismatches remain",
      "hard failures.",
      sep = "\n"
    ),
    "\n"
  )
}

args <- commandArgs(trailingOnly = TRUE)
options(error = function() quit(status = 2))

if ("--help" %in% args || "-h" %in% args || length(args) == 0) {
  usage()
  quit(status = if (length(args) == 0) 2 else 0)
}

command <- args[[1]]
args <- args[-1]

if (!command %in% c("check", "bless")) {
  usage()
  stop("Command must be `check` or `bless`.", call. = FALSE)
}

arg_value <- function(flag, default = NULL) {
  pos <- match(flag, args)
  if (is.na(pos)) {
    return(default)
  }
  if (pos == length(args)) {
    stop("Missing value for ", flag, ".", call. = FALSE)
  }
  args[[pos + 1]]
}

require_namespace <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop(
      "Install local tooling dependency `",
      package,
      "` in the development environment.",
      call. = FALSE
    )
  }
}

require_namespace("devtools")
require_namespace("jsonlite")

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_path <- if (length(script_arg) > 0) {
  normalizePath(
    sub("^--file=", "", script_arg[[1]]),
    winslash = "/",
    mustWork = TRUE
  )
} else {
  NA_character_
}
project_root <- if (!is.na(script_path)) {
  normalizePath(file.path(dirname(script_path), ".."), winslash = "/")
} else {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}
if (!file.exists(file.path(project_root, "DESCRIPTION"))) {
  stop("Run this script from the qualtdict repository.", call. = FALSE)
}

source(file.path(project_root, "tools", "local-finalize-smoke-lib.R"))

config_path <- arg_value(
  "--config",
  file.path(project_root, "tools", "local-finalize-smoke-surveys.json")
)
smoke_root <- arg_value(
  "--root",
  file.path(project_root, ".local", "finalize-smoke")
)
survey_filter <- arg_value("--survey")
functions_filter <- arg_value("--functions")
variable_name_filter <- arg_value("--variable-name")
survey_count <- parse_smoke_survey_count(arg_value("--survey-count"))
survey_seed <- parse_smoke_survey_seed(arg_value("--survey-seed"))
selected_functions <- parse_smoke_functions(functions_filter)
selected_variable_names <- parse_smoke_variable_names(variable_name_filter)
selective_functions <- !is.null(functions_filter)

read_config <- function(path) {
  if (!file.exists(path)) {
    stop("Survey config not found: ", path, call. = FALSE)
  }
  config <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  surveys <- config$surveys
  if (!is.list(surveys) || length(surveys) == 0) {
    stop(
      "Survey config must contain a non-empty `surveys` array.",
      call. = FALSE
    )
  }
  surveys
}

is_placeholder_survey_id <- function(survey_id) {
  !is.character(survey_id) ||
    length(survey_id) != 1 ||
    !grepl("^SV_", survey_id) ||
    grepl("REPLACE|TODO|PLACEHOLDER", survey_id, ignore.case = TRUE)
}

validate_survey <- function(survey) {
  if (is.null(survey$alias) || !grepl("^[A-Za-z0-9_-]+$", survey$alias)) {
    stop("Each survey needs an alphanumeric `alias`.", call. = FALSE)
  }
  if (is_placeholder_survey_id(survey$survey_id)) {
    stop(
      "Survey `",
      survey$alias,
      "` still has a placeholder `survey_id` in ",
      config_path,
      ".",
      call. = FALSE
    )
  }
  survey
}

artifact_path <- function(survey, file) {
  file.path(smoke_root, "source", survey$alias, file)
}

artifact_md5 <- function(path) {
  unname(tools::md5sum(path))
}

validate_manifest <- function(survey, paths) {
  manifest <- read_json(paths[["manifest"]])
  if (!identical(manifest$alias, survey$alias)) {
    stop(
      "Manifest alias does not match config for `",
      survey$alias,
      "`.",
      call. = FALSE
    )
  }
  if (!identical(manifest$survey_id, survey$survey_id)) {
    stop(
      "Manifest survey_id does not match config for `",
      survey$alias,
      "`.",
      call. = FALSE
    )
  }

  expected_hashes <- c(
    metadata = manifest$artifacts$metadata$md5,
    description = manifest$artifacts$description$md5,
    responses = manifest$artifacts$responses_raw_columns$md5
  )
  actual_hashes <- vapply(
    paths[names(expected_hashes)],
    artifact_md5,
    character(1)
  )
  mismatch <- names(expected_hashes)[actual_hashes != expected_hashes]
  if (length(mismatch) > 0) {
    stop(
      "Manifest artifact hash mismatch for `",
      survey$alias,
      "`: ",
      paste(mismatch, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(manifest)
}

load_artifacts <- function(survey) {
  paths <- c(
    metadata = artifact_path(survey, "metadata.rds"),
    description = artifact_path(survey, "description.rds"),
    responses = artifact_path(survey, "responses_raw_columns.rds"),
    manifest = artifact_path(survey, "manifest.json")
  )
  missing <- paths[!file.exists(paths)]
  if (length(missing) > 0) {
    stop(
      "Missing local smoke artifacts for `",
      survey$alias,
      "`. Run `Rscript tools/fetch-local-finalize-smoke.R` first. Missing: ",
      paste(basename(missing), collapse = ", "),
      call. = FALSE
    )
  }
  validate_manifest(survey, paths)
  list(
    metadata = readRDS(paths[["metadata"]]),
    description = readRDS(paths[["description"]]),
    responses = readRDS(paths[["responses"]])
  )
}

replace_namespace_binding <- function(name, value) {
  namespace <- asNamespace("qualtdict")
  original <- get(name, envir = namespace)
  unlockBinding(name, namespace)
  assign(name, value, envir = namespace)
  lockBinding(name, namespace)
  original
}

with_artifact_fetches <- function(survey, artifacts, expr) {
  raw_metadata_constructor <- getFromNamespace(
    "new_raw_qualtrics_metadata",
    "qualtdict"
  )

  original_metadata <- replace_namespace_binding(
    "fetch_dictionary_metadata",
    function(surveyID) {
      if (!identical(surveyID, survey$survey_id)) {
        stop(
          "Unexpected surveyID requested from local artifacts.",
          call. = FALSE
        )
      }
      raw_metadata_constructor(
        surveyID = surveyID,
        metadata = artifacts$metadata,
        description = artifacts$description
      )
    }
  )
  original_survey <- replace_namespace_binding(
    "fetch_survey2",
    function(...) {
      fetch_args <- list(...)
      if (!identical(fetch_args$surveyID, survey$survey_id)) {
        stop(
          "Unexpected surveyID requested from local responses.",
          call. = FALSE
        )
      }
      artifacts$responses
    }
  )

  on.exit(
    {
      replace_namespace_binding("fetch_dictionary_metadata", original_metadata)
      replace_namespace_binding("fetch_survey2", original_survey)
    },
    add = TRUE
  )

  force(expr)
}

hash_object <- function(x) {
  temp <- tempfile(fileext = ".rds")
  on.exit(unlink(temp), add = TRUE)
  saveRDS(x, temp, version = 2)
  unname(tools::md5sum(temp))
}

hash_list <- function(x) {
  hash_object(x)
}

run_step <- function(label, expr) {
  cat("START ", label, "\n", sep = "")
  flush.console()
  timing <- system.time(value <- force(expr))
  cat(
    "DONE ",
    label,
    " elapsed=",
    unname(timing[["elapsed"]]),
    "s\n",
    sep = ""
  )
  flush.console()
  value
}

class_summary <- function(x) {
  vapply(x, function(column) paste(class(column), collapse = "/"), character(1))
}

label_summary <- function(x) {
  labels <- lapply(x, attr, which = "label", exact = TRUE)
  labels[!vapply(labels, is.null, logical(1))]
}

value_label_summary <- function(x) {
  labels <- lapply(x, attr, which = "labels", exact = TRUE)
  labels[!vapply(labels, is.null, logical(1))]
}

data_frame_summary <- function(x) {
  list(
    rows = nrow(x),
    columns = ncol(x),
    names = names(x),
    classes = as.list(class_summary(x)),
    missing = as.list(vapply(
      x,
      function(column) sum(is.na(column)),
      integer(1)
    )),
    labels = label_summary(x),
    value_labels = value_label_summary(x),
    object_hash = hash_object(x)
  )
}

validation_summary <- function(x) {
  list(
    validation_findings_rows = nrow(x$validation_findings),
    level_label_pairs_rows = nrow(x$level_label_pairs),
    validation_findings_hash = hash_object(x$validation_findings),
    level_label_pairs_hash = hash_object(x$level_label_pairs),
    object_hash = hash_object(x)
  )
}

block_list_summary <- function(x) {
  list(
    blocks = names(x),
    block_count = length(x),
    rows = as.list(vapply(x, nrow, integer(1))),
    columns = as.list(vapply(x, ncol, integer(1))),
    hashes = as.list(vapply(x, hash_object, character(1))),
    object_hash = hash_object(x)
  )
}

scenario_baseline_path <- function(survey_alias, variable_name) {
  file.path(
    smoke_root,
    "baselines",
    paste0(survey_alias, "-", variable_name, ".json")
  )
}

write_json <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(
    x,
    path,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null",
    digits = NA
  )
}

read_json <- function(path) {
  jsonlite::fromJSON(path, simplifyVector = FALSE)
}

column_sample <- function(x, limit = 20) {
  if (length(x) <= limit) {
    return(paste(x, collapse = ", "))
  }
  paste(
    paste(head(x, limit), collapse = ", "),
    "... (",
    length(x) - limit,
    " more)",
    sep = ""
  )
}

response_column_id_parity <- function(
  dict_response_column_ids,
  raw_response_columns
) {
  dict_response_column_ids <- unique(as.character(dict_response_column_ids))
  dict_response_column_ids <- dict_response_column_ids[
    !is.na(dict_response_column_ids)
  ]

  raw_response_columns <- unique(as.character(raw_response_columns))
  raw_response_columns <- raw_response_columns[!is.na(raw_response_columns)]

  missing_from_raw_response <- setdiff(
    dict_response_column_ids,
    raw_response_columns
  )

  # ADR 0005 records the planned Metadata-defined Export Variable contract, but
  # that ADR has not been implemented yet. Until Embedded Data Fields, Scoring
  # Variables, and Text-analysis Sidecars are represented in Variable
  # Dictionaries, keep the stricter raw-to-dictionary parity check disabled.
  #
  # nolint start: commented_code_linter
  # checked_raw_response_columns <- raw_response_columns[
  #   grepl("(^|_)QID[0-9]+", raw_response_columns) |
  #     grepl("^SC_", raw_response_columns) |
  #     grepl("_SCORE$", raw_response_columns)
  # ]
  # missing_from_dict <- setdiff(
  #   checked_raw_response_columns,
  #   dict_response_column_ids
  # )
  # nolint end
  checked_raw_response_columns <- character()
  missing_from_dict <- character()

  list(
    ok = length(missing_from_raw_response) == 0,
    dict_response_column_ids = dict_response_column_ids,
    raw_response_columns = raw_response_columns,
    checked_raw_response_columns = checked_raw_response_columns,
    missing_from_raw_response = missing_from_raw_response,
    missing_from_dict = missing_from_dict
  )
}

response_column_id_parity_record <- function(survey, parity) {
  c(
    list(
      alias = survey$alias,
      survey_id = survey$survey_id,
      generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    ),
    parity
  )
}

write_response_column_id_parity <- function(survey, parity, run_dir) {
  write_json(
    response_column_id_parity_record(survey, parity),
    file.path(run_dir, paste0(survey$alias, "-response-column-id-parity.json"))
  )
}

stop_response_column_id_parity <- function(survey, parity) {
  messages <- c(
    paste0("Response Column ID parity failed for `", survey$alias, "`.")
  )
  if (length(parity$missing_from_raw_response) > 0) {
    messages <- c(
      messages,
      paste0(
        "Variable Dictionary Response Column IDs missing from raw fetched ",
        "response data: ",
        column_sample(parity$missing_from_raw_response)
      )
    )
  }
  if (length(parity$missing_from_dict) > 0) {
    messages <- c(
      messages,
      paste0(
        "QID/scoring raw response columns missing from the Variable ",
        "Dictionary: ",
        column_sample(parity$missing_from_dict)
      )
    )
  }

  stop(structure(
    list(message = paste(messages, collapse = "\n"), call = NULL),
    class = c("response_column_id_parity_error", "error", "condition")
  ))
}

assert_response_column_id_parity <- function(survey, dict, responses, run_dir) {
  parity <- response_column_id_parity(
    dict_response_column_ids = dict[["response_column_id"]],
    raw_response_columns = names(responses)
  )
  write_response_column_id_parity(survey, parity, run_dir)

  if (!isTRUE(parity$ok)) {
    stop_response_column_id_parity(survey, parity)
  }

  cat(
    survey$alias,
    ": Response Column ID parity matched",
    " dict_ids=",
    length(parity$dict_response_column_ids),
    " raw_cols=",
    length(parity$raw_response_columns),
    "\n",
    sep = ""
  )
  invisible(parity)
}

run_scenario <- function(
  survey,
  variable_name,
  selected_functions,
  dict = NULL
) {
  scenario_label <- paste(survey$alias, variable_name, sep = " / ")
  requirements <- smoke_scenario_requirements(selected_functions)
  objects <- list()
  summaries <- list()

  if (is.null(dict)) {
    dict <- run_step(
      paste(scenario_label, "dict_generate"),
      qualtdict::dict_generate(
        survey$survey_id,
        variable_name = variable_name,
        quiet = FALSE
      )
    )
  }
  objects$dict <- dict

  if (requirements$needs_validation) {
    validation <- run_step(
      paste(scenario_label, "dict_validate"),
      qualtdict::dict_validate(dict, quiet = FALSE)
    )
    objects$validation <- validation
  }

  if (requirements$needs_labelled) {
    labelled <- run_step(
      paste(scenario_label, "fetch_labelled_survey_data"),
      qualtdict::fetch_labelled_survey_data(dict, quiet = FALSE)
    )
    objects$labelled <- labelled
  }

  if (requirements$needs_export_findings) {
    export_findings <- run_step(
      paste(scenario_label, "labelled_export_findings"),
      qualtdict::labelled_export_findings(objects$labelled)
    )
    objects$labelled_export_findings <- export_findings
  }

  if (requirements$needs_dict_blocks) {
    dict_blocks <- run_step(
      paste(scenario_label, "dict_split_blocks"),
      qualtdict::dict_split_blocks(dict)
    )
    objects$dict_blocks <- dict_blocks
  }

  if (requirements$needs_survey_blocks) {
    survey_blocks <- run_step(
      paste(scenario_label, "survey_split_blocks"),
      qualtdict::survey_split_blocks(objects$labelled)
    )
    objects$survey_blocks <- survey_blocks
  }

  if (
    identical(variable_name, "question_name") &&
      "fetch_labelled_survey_data" %in% selected_functions
  ) {
    labelled_excluding_validation <- run_step(
      paste(scenario_label, "fetch_labelled_survey_data exclude validation"),
      qualtdict::fetch_labelled_survey_data(
        dict,
        exclude_findings = "validation",
        quiet = FALSE
      )
    )
    objects$labelled_excluding_validation <- labelled_excluding_validation
  }

  if ("dict_generate" %in% selected_functions) {
    summaries$dict <- data_frame_summary(objects$dict)
  }
  if ("dict_validate" %in% selected_functions) {
    summaries$validation <- validation_summary(objects$validation)
  }
  if ("fetch_labelled_survey_data" %in% selected_functions) {
    summaries$labelled <- data_frame_summary(objects$labelled)
  }
  if ("labelled_export_findings" %in% selected_functions) {
    summaries$labelled_export_findings <-
      data_frame_summary(objects$labelled_export_findings)
  }
  if ("dict_split_blocks" %in% selected_functions) {
    summaries$dict_blocks <- block_list_summary(objects$dict_blocks)
  }
  if ("survey_split_blocks" %in% selected_functions) {
    summaries$survey_blocks <- block_list_summary(objects$survey_blocks)
  }
  if (!is.null(objects$labelled_excluding_validation)) {
    summaries$labelled_excluding_validation <-
      data_frame_summary(objects$labelled_excluding_validation)
  }

  object_hashes <- lapply(summaries, `[[`, "object_hash")
  list(
    alias = survey$alias,
    survey_id = survey$survey_id,
    variable_name = variable_name,
    generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    summaries = summaries,
    object_hashes = object_hashes,
    scenario_hash = hash_list(object_hashes),
    objects = objects
  )
}

baseline_record <- function(result) {
  result$objects <- NULL
  result
}

write_run_artifacts <- function(result, run_dir) {
  prefix <- paste(result$alias, result$variable_name, sep = "-")
  write_json(
    baseline_record(result),
    file.path(run_dir, paste0(prefix, "-summary.json"))
  )
  saveRDS(
    result$objects,
    file.path(run_dir, paste0(prefix, "-objects.rds")),
    version = 2
  )
}

print_result_line <- function(result, status) {
  cat(smoke_result_line(result, status), "\n", sep = "")
}

compare_records <- function(current, baseline) {
  identical(current$scenario_hash, baseline$scenario_hash) &&
    identical(current$object_hashes, baseline$object_hashes)
}

print_mismatch <- function(current, baseline) {
  cat(
    paste(smoke_mismatch_lines(current, baseline), collapse = "\n"),
    "\n",
    sep = ""
  )
}

run_survey <- function(
  survey,
  run_dir,
  selected_functions,
  selected_variable_names
) {
  cat("SURVEY ", survey$alias, "\n", sep = "")
  flush.console()
  artifacts <- run_step(
    paste(survey$alias, "load_artifacts"),
    load_artifacts(survey)
  )
  with_artifact_fetches(survey, artifacts, {
    results <- lapply(
      selected_variable_names,
      function(variable_name) {
        run_scenario(
          survey,
          variable_name,
          selected_functions = selected_functions
        )
      }
    )
    names(results) <- selected_variable_names

    run_step(
      paste(survey$alias, "Response Column ID parity"),
      assert_response_column_id_parity(
        survey = survey,
        dict = results[[1]]$objects$dict,
        responses = artifacts$responses,
        run_dir = run_dir
      )
    )
    lapply(results, function(result) {
      write_run_artifacts(result, run_dir)
      result
    })
  })
}

surveys <- lapply(read_config(config_path), validate_survey)
if (!is.null(survey_filter)) {
  surveys <- Filter(
    function(survey) identical(survey$alias, survey_filter),
    surveys
  )
  if (length(surveys) == 0) {
    stop("No configured survey has alias `", survey_filter, "`.", call. = FALSE)
  }
} else {
  surveys <- sample_smoke_surveys(
    surveys,
    count = survey_count,
    seed = survey_seed
  )
}

setwd(project_root)
devtools::load_all(".", quiet = TRUE)

run_id <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
run_dir <- file.path(smoke_root, "runs", run_id)
dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)

cat(
  "Selected surveys: ",
  paste(vapply(surveys, `[[`, character(1), "alias"), collapse = ", "),
  "\n",
  sep = ""
)
cat(
  "Selected Variable Dictionary routes: ",
  paste(selected_variable_names, collapse = ", "),
  "\n",
  sep = ""
)

results <- tryCatch(
  unlist(
    lapply(
      surveys,
      run_survey,
      run_dir = run_dir,
      selected_functions = selected_functions,
      selected_variable_names = selected_variable_names
    ),
    recursive = FALSE
  ),
  response_column_id_parity_error = function(error) {
    cat(conditionMessage(error), "\n", sep = "")
    cat("Wrote current run artifacts to ", run_dir, "\n", sep = "")
    quit(status = 1)
  }
)

if (identical(command, "bless")) {
  for (result in results) {
    path <- scenario_baseline_path(result$alias, result$variable_name)
    selected_record <- project_smoke_record(
      baseline_record(result),
      selected_functions
    )
    existing_record <- if (file.exists(path)) read_json(path) else NULL
    selected_record <- bless_smoke_record(
      existing = existing_record,
      selected = selected_record,
      selective = selective_functions
    )
    write_json(selected_record, path)
    print_result_line(result, "blessed")
  }
  cat(
    "Wrote baselines under ",
    file.path(smoke_root, "baselines"),
    "\n",
    sep = ""
  )
  quit(status = 0)
}

status <- 0L
for (result in results) {
  path <- scenario_baseline_path(result$alias, result$variable_name)
  if (!file.exists(path)) {
    cat(
      "Missing baseline: ",
      result$alias,
      " / ",
      result$variable_name,
      "\n",
      sep = ""
    )
    print_result_line(result, "current")
    status <- 1L
    next
  }

  baseline <- read_json(path)
  current <- project_smoke_record(
    baseline_record(result),
    selected_functions
  )
  baseline <- project_smoke_record(baseline, selected_functions)
  if (compare_records(current, baseline)) {
    print_result_line(result, "matched")
  } else {
    print_mismatch(current, baseline)
    status <- 1L
  }
}

cat("Wrote current run artifacts to ", run_dir, "\n", sep = "")
if (status != 0L) {
  cat(
    "Inspect the current run artifacts, then run `bless` if changes are intended.\n"
  )
}
quit(status = status)
