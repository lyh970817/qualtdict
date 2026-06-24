#!/usr/bin/env Rscript

usage <- function() {
  cat(
    paste(
      "Usage:",
      "  Rscript tools/local-finalize-smoke.R check",
      "  Rscript tools/local-finalize-smoke.R bless",
      "  Rscript tools/local-finalize-smoke.R check --survey survey_a",
      "",
      "Options:",
      "  --config PATH    Survey config JSON.",
      "  --root DIR       Local smoke artifact directory.",
      "",
      "`check` compares current exported-function output hashes with local",
      "baselines. `bless` replaces local baselines with current output hashes.",
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

config_path <- arg_value(
  "--config",
  file.path(project_root, "tools", "local-finalize-smoke-surveys.json")
)
smoke_root <- arg_value(
  "--root",
  file.path(project_root, ".local", "finalize-smoke")
)
survey_filter <- arg_value("--survey")

read_config <- function(path) {
  if (!file.exists(path)) {
    stop("Survey config not found: ", path, call. = FALSE)
  }
  config <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  surveys <- config$surveys
  if (!is.list(surveys) || length(surveys) == 0) {
    stop("Survey config must contain a non-empty `surveys` array.", call. = FALSE)
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
    stop("Manifest alias does not match config for `", survey$alias, "`.", call. = FALSE)
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
  actual_hashes <- vapply(paths[names(expected_hashes)], artifact_md5, character(1))
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
        stop("Unexpected surveyID requested from local artifacts.", call. = FALSE)
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
        stop("Unexpected surveyID requested from local responses.", call. = FALSE)
      }
      artifacts$responses
    }
  )

  on.exit({
    replace_namespace_binding("fetch_dictionary_metadata", original_metadata)
    replace_namespace_binding("fetch_survey2", original_survey)
  }, add = TRUE)

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

run_scenario <- function(survey, variable_name) {
  dict <- qualtdict::dict_generate(
    survey$survey_id,
    variable_name = variable_name,
    quiet = TRUE
  )
  validation <- qualtdict::dict_validate(dict)
  labelled <- qualtdict::get_survey_data(dict)
  export_findings <- qualtdict::labelled_export_findings(labelled)
  dict_blocks <- qualtdict::dict_split_blocks(dict)
  survey_blocks <- qualtdict::survey_split_blocks(labelled)

  objects <- list(
    dict = dict,
    validation = validation,
    labelled = labelled,
    labelled_export_findings = export_findings,
    dict_blocks = dict_blocks,
    survey_blocks = survey_blocks
  )

  if (identical(variable_name, "question_name")) {
    labelled_excluding_validation <- qualtdict::get_survey_data(
      dict,
      exclude_findings = "validation"
    )
    objects$labelled_excluding_validation <- labelled_excluding_validation
  }

  summaries <- list(
    dict = data_frame_summary(dict),
    validation = validation_summary(validation),
    labelled = data_frame_summary(labelled),
    labelled_export_findings = data_frame_summary(export_findings),
    dict_blocks = block_list_summary(dict_blocks),
    survey_blocks = block_list_summary(survey_blocks)
  )

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
  cat(
    result$alias,
    " / ",
    result$variable_name,
    ": ",
    status,
    " hash=",
    result$scenario_hash,
    " dict_rows=",
    result$summaries$dict$rows,
    " labelled_rows=",
    result$summaries$labelled$rows,
    " labelled_cols=",
    result$summaries$labelled$columns,
    "\n",
    sep = ""
  )
}

compare_records <- function(current, baseline) {
  identical(current$scenario_hash, baseline$scenario_hash) &&
    identical(current$object_hashes, baseline$object_hashes)
}

print_mismatch <- function(current, baseline) {
  cat(
    "Mismatch: ",
    current$alias,
    " / ",
    current$variable_name,
    "\n",
    sep = ""
  )
  cat("  baseline hash: ", baseline$scenario_hash, "\n", sep = "")
  cat("  current hash:  ", current$scenario_hash, "\n", sep = "")
  cat(
    "  dict rows: ",
    baseline$summaries$dict$rows,
    " -> ",
    current$summaries$dict$rows,
    "\n",
    sep = ""
  )
  cat(
    "  labelled dims: ",
    baseline$summaries$labelled$rows,
    "x",
    baseline$summaries$labelled$columns,
    " -> ",
    current$summaries$labelled$rows,
    "x",
    current$summaries$labelled$columns,
    "\n",
    sep = ""
  )
  cat(
    "  validation findings rows: ",
    baseline$summaries$validation$validation_findings_rows,
    " -> ",
    current$summaries$validation$validation_findings_rows,
    "\n",
    sep = ""
  )
}

run_survey <- function(survey, run_dir) {
  artifacts <- load_artifacts(survey)
  with_artifact_fetches(survey, artifacts, {
    lapply(c("question_name", "semantic_name"), function(variable_name) {
      result <- run_scenario(survey, variable_name)
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
}

setwd(project_root)
devtools::load_all(".", quiet = TRUE)

run_id <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
run_dir <- file.path(smoke_root, "runs", run_id)
dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)

results <- unlist(
  lapply(surveys, run_survey, run_dir = run_dir),
  recursive = FALSE
)

if (identical(command, "bless")) {
  for (result in results) {
    path <- scenario_baseline_path(result$alias, result$variable_name)
    write_json(baseline_record(result), path)
    print_result_line(result, "blessed")
  }
  cat("Wrote baselines under ", file.path(smoke_root, "baselines"), "\n", sep = "")
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
  current <- baseline_record(result)
  if (compare_records(current, baseline)) {
    print_result_line(result, "matched")
  } else {
    print_mismatch(current, baseline)
    status <- 1L
  }
}

cat("Wrote current run artifacts to ", run_dir, "\n", sep = "")
if (status != 0L) {
  cat("Inspect the current run artifacts, then run `bless` if changes are intended.\n")
}
quit(status = status)
