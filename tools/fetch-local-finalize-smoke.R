#!/usr/bin/env Rscript

usage <- function() {
  cat(
    paste(
      "Usage:",
      "  Rscript tools/fetch-local-finalize-smoke.R",
      "  Rscript tools/fetch-local-finalize-smoke.R --survey survey_a",
      "",
      "Options:",
      "  --config PATH    Survey config JSON.",
      "  --output-dir DIR Local smoke artifact directory.",
      "  --credentials PATH",
      "                   Optional .Renviron file to read before fetching.",
      "  --response-limit N",
      "                   Optional maximum number of response rows to fetch.",
      "",
      "This trusted-human script downloads fixed Qualtrics surveys and writes",
      "unsanitized metadata/description plus sanitized response data. It never",
      "persists raw response data.",
      sep = "\n"
    ),
    "\n"
  )
}

args <- commandArgs(trailingOnly = TRUE)

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

if ("--help" %in% args || "-h" %in% args) {
  usage()
  quit(status = 0)
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

require_namespace("jsonlite")
require_namespace("qualtRics")
require_namespace("devtools")

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
project_root <- normalizePath(
  if (!is.na(script_path)) file.path(dirname(script_path), "..") else getwd(),
  winslash = "/",
  mustWork = FALSE
)
if (!file.exists(file.path(project_root, "DESCRIPTION"))) {
  project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}
setwd(project_root)
devtools::load_all(".", quiet = TRUE)

config_path <- arg_value(
  "--config",
  file.path(project_root, "tools", "local-finalize-smoke-surveys.json")
)
output_dir <- arg_value(
  "--output-dir",
  file.path(project_root, ".local", "finalize-smoke")
)
survey_filter <- arg_value("--survey")
credentials <- arg_value("--credentials")
response_limit <- arg_value("--response-limit")

if (!is.null(response_limit)) {
  response_limit <- suppressWarnings(as.integer(response_limit))
  if (is.na(response_limit) || response_limit < 1) {
    stop(
      "`--response-limit` must be an integer greater than zero.",
      call. = FALSE
    )
  }
}

if (!is.null(credentials)) {
  readRenviron(credentials)
}

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

metadata_elements <- function() {
  c(
    "questions",
    "metadata",
    "blocks",
    "responsecounts",
    "flow",
    "embedded_data"
  )
}

description_elements <- function() {
  c("questions", "metadata", "blocks", "flow", "scoring")
}

identifying_column <- function(name) {
  name_lower <- tolower(name)
  grepl(
    paste(
      c(
        "responseid",
        "externaldatareference",
        "ipaddress",
        "recipient",
        "email",
        "firstname",
        "lastname",
        "phone",
        "latitude",
        "longitude",
        "location",
        "contact",
        "distribution"
      ),
      collapse = "|"
    ),
    name_lower
  )
}

date_column <- function(name, x) {
  name_lower <- tolower(name)
  inherits(x, c("Date", "POSIXct", "POSIXlt")) ||
    grepl("date|time|timestamp", name_lower)
}

code_like_character <- function(x) {
  values <- unique(x[!is.na(x)])
  if (length(values) == 0 || length(values) > 50) {
    return(FALSE)
  }
  all(nchar(values) <= 30) &&
    all(grepl("^[A-Za-z0-9_.:#/-]+$", values))
}

synthetic_timestamps <- function(n) {
  format(
    as.POSIXct("2026-01-01 00:00:00", tz = "UTC") + seq_len(n) * 60,
    "%Y-%m-%d %H:%M:%S"
  )
}

sanitize_identifier <- function(x, name) {
  non_missing <- which(!is.na(x))
  y <- as.character(x)
  prefix <- "ID"
  if (grepl("email", tolower(name))) {
    y[non_missing] <- sprintf(
      "person_%06d@example.invalid",
      seq_along(non_missing)
    )
  } else if (grepl("ip", tolower(name))) {
    y[non_missing] <- sprintf(
      "192.0.2.%d",
      ((seq_along(non_missing) - 1) %% 254) + 1
    )
  } else {
    y[non_missing] <- sprintf("%s_%06d", prefix, seq_along(non_missing))
  }
  y
}

sanitize_date <- function(x) {
  non_missing <- which(!is.na(x))
  y <- x
  values <- synthetic_timestamps(length(non_missing))
  if (inherits(x, "Date")) {
    y[non_missing] <- as.Date(values)
  } else if (inherits(x, "hms")) {
    y[non_missing] <- seq_along(non_missing) * 60
  } else if (inherits(x, c("POSIXct", "POSIXlt"))) {
    y[non_missing] <- as.POSIXct(values, tz = "UTC")
  } else {
    y[non_missing] <- values
  }
  y
}

sanitize_allowed_levels <- function(x, allowed_levels) {
  allowed_levels <- allowed_levels[
    !is.na(allowed_levels) & nzchar(allowed_levels)
  ]
  if (length(allowed_levels) == 0) {
    return(NULL)
  }

  non_missing <- which(!is.na(x))
  y <- if (is.numeric(x) || is.integer(x)) x else as.character(x)
  replacement <- allowed_levels[
    ((seq_along(non_missing) - 1) %% length(allowed_levels)) + 1
  ]

  if (is.numeric(x) || is.integer(x)) {
    numeric_replacement <- suppressWarnings(as.numeric(replacement))
    if (all(!is.na(numeric_replacement))) {
      y[non_missing] <- numeric_replacement
      if (is.integer(x)) {
        y <- as.integer(y)
      }
      return(y)
    }
  }

  y <- as.character(x)
  y[non_missing] <- replacement
  y
}

sanitize_numeric <- function(x) {
  non_missing <- which(!is.na(x))
  y <- x
  y[non_missing] <- seq_along(non_missing)
  if (is.integer(x)) {
    y <- as.integer(y)
  }
  y
}

sanitize_logical <- function(x) {
  non_missing <- which(!is.na(x))
  y <- x
  y[non_missing] <- rep(c(TRUE, FALSE), length.out = length(non_missing))
  y
}

sanitize_code_like <- function(x) {
  non_missing <- which(!is.na(x))
  y <- as.character(x)
  y[non_missing] <- sprintf("CODE_%06d", seq_along(non_missing))
  y
}

sanitize_text <- function(x) {
  non_missing <- which(!is.na(x))
  y <- as.character(x)
  y[non_missing] <- sprintf("TEXT_%06d", seq_along(non_missing))
  y
}

sanitize_column <- function(x, name, allowed_levels = character()) {
  if (identifying_column(name)) {
    return(list(values = sanitize_identifier(x, name), strategy = "identifier"))
  }
  if (date_column(name, x)) {
    return(list(values = sanitize_date(x), strategy = "date"))
  }
  sanitized_allowed <- sanitize_allowed_levels(x, allowed_levels)
  if (!is.null(sanitized_allowed)) {
    return(list(
      values = sanitized_allowed,
      strategy = "metadata_allowed_level"
    ))
  }
  if (is.numeric(x) || is.integer(x)) {
    return(list(values = sanitize_numeric(x), strategy = "numeric"))
  }
  if (is.logical(x)) {
    return(list(values = sanitize_logical(x), strategy = "logical"))
  }
  if (is.character(x) && code_like_character(x)) {
    return(list(
      values = sanitize_code_like(x),
      strategy = "code_like_character"
    ))
  }
  list(values = sanitize_text(x), strategy = "text")
}

sanitize_responses <- function(responses, allowed_levels) {
  sanitized <- responses
  report <- vector("list", length(responses))
  names(report) <- names(responses)
  for (name in names(responses)) {
    result <- sanitize_column(
      responses[[name]],
      name,
      allowed_levels = allowed_levels[[name]] %||% character()
    )
    sanitized[[name]] <- result$values
    report[[name]] <- list(
      strategy = result$strategy,
      non_missing = sum(!is.na(responses[[name]])),
      class = class(responses[[name]])
    )
  }
  list(data = sanitized, report = report)
}

`%||%` <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}

build_allowed_levels <- function(survey_id, metadata, description) {
  raw_metadata <- getFromNamespace(
    "new_raw_qualtrics_metadata",
    "qualtdict"
  )(
    surveyID = survey_id,
    metadata = metadata,
    description = description
  )
  normalised_metadata <- getFromNamespace(
    "normalise_qualtrics_metadata",
    "qualtdict"
  )(raw_metadata)
  dict <- getFromNamespace(
    "variable_dictionary_from_normalised_metadata",
    "qualtdict"
  )(
    normalised_metadata,
    use_semantic_name = FALSE,
    block_pattern = NULL,
    block_sep = ".",
    semantic_name_preprocess = NULL,
    quiet = TRUE
  )

  levels <- split(dict$level, dict$response_column_id)
  lapply(levels, function(x) unique(as.character(x[!is.na(x)])))
}

artifact_md5 <- function(path) {
  unname(tools::md5sum(path))
}

package_version_or_na <- function(package) {
  if (requireNamespace(package, quietly = TRUE)) {
    return(as.character(utils::packageVersion(package)))
  }
  NA_character_
}

write_artifacts <- function(survey) {
  alias_dir <- file.path(output_dir, "source", survey$alias)
  dir.create(alias_dir, recursive = TRUE, showWarnings = FALSE)

  message("Fetching metadata for ", survey$alias, "...")
  metadata <- qualtRics::metadata(survey$survey_id, metadata_elements())
  description <- qualtRics::fetch_description(
    survey$survey_id,
    description_elements()
  )

  message("Fetching and sanitizing responses for ", survey$alias, "...")
  responses <- qualtRics::fetch_survey(
    surveyID = survey$survey_id,
    limit = response_limit,
    force_request = TRUE,
    import_id = TRUE,
    convert = FALSE,
    label = FALSE,
    breakout_sets = TRUE
  )
  allowed_levels <- build_allowed_levels(
    survey_id = survey$survey_id,
    metadata = metadata,
    description = description
  )
  response_column_map <- attr(responses, "column_map", exact = TRUE)
  sanitized <- sanitize_responses(responses, allowed_levels)
  attr(sanitized$data, "column_map") <- response_column_map
  rm(responses)
  gc(verbose = FALSE)

  metadata_path <- file.path(alias_dir, "metadata.rds")
  description_path <- file.path(alias_dir, "description.rds")
  responses_path <- file.path(alias_dir, "responses_raw_columns.rds")
  manifest_path <- file.path(alias_dir, "manifest.json")

  saveRDS(metadata, metadata_path, version = 2)
  saveRDS(description, description_path, version = 2)
  saveRDS(sanitized$data, responses_path, version = 2)

  manifest <- list(
    alias = survey$alias,
    survey_id = survey$survey_id,
    fetched_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    response_limit = response_limit,
    package_versions = list(
      R = as.character(getRversion()),
      qualtRics = package_version_or_na("qualtRics"),
      jsonlite = package_version_or_na("jsonlite")
    ),
    artifacts = list(
      metadata = list(
        path = basename(metadata_path),
        md5 = artifact_md5(metadata_path)
      ),
      description = list(
        path = basename(description_path),
        md5 = artifact_md5(description_path)
      ),
      responses_raw_columns = list(
        path = basename(responses_path),
        md5 = artifact_md5(responses_path),
        rows = nrow(sanitized$data),
        columns = ncol(sanitized$data),
        column_names = names(sanitized$data)
      )
    ),
    sanitization = list(columns = sanitized$report)
  )

  jsonlite::write_json(
    manifest,
    manifest_path,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )

  message("Wrote sanitized artifacts for ", survey$alias, " to ", alias_dir)
  invisible(manifest)
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

invisible(lapply(surveys, write_artifacts))
