helper_path <- testthat::test_path(
  "..",
  "..",
  "tools",
  "local-finalize-smoke-lib.R"
)
skip_if_not(
  file.exists(helper_path),
  "local smoke tooling is excluded from package builds"
)
source(helper_path)

source_smoke_parity_helpers <- function(envir = parent.frame()) {
  smoke_script <- testthat::test_path(
    "..",
    "..",
    "tools",
    "local-finalize-smoke.R"
  )
  smoke_lines <- readLines(smoke_script, warn = FALSE)
  start <- grep("^column_sample <- function", smoke_lines)
  end <- grep("^assert_response_column_id_parity <- function", smoke_lines) - 1L
  eval(
    parse(text = paste(smoke_lines[start:end], collapse = "\n")),
    envir = envir
  )
}

source_smoke_parity_helpers()

test_that("parse_smoke_functions defaults to all smoke-covered functions", {
  expect_identical(
    parse_smoke_functions(NULL),
    c(
      "dict_generate",
      "dict_validate",
      "fetch_labelled_survey_data",
      "labelled_export_findings",
      "dict_split_blocks",
      "survey_split_blocks"
    )
  )
})

test_that("local smoke tests use a build-safe helper bootstrap", {
  test_file <- testthat::test_path("test-local_finalize_smoke.R")
  test_lines <- readLines(test_file, warn = FALSE)
  test_text <- gsub("\\s+", " ", paste(test_lines, collapse = "\n"))

  expect_match(
    test_text,
    paste0(
      "helper_path <- testthat::test_path\\(",
      " *\"..\", \"..\", \"tools\", ",
      "\"local-finalize-smoke-lib.R\" *\\)"
    )
  )
  expect_match(
    test_text,
    paste0(
      "skip_if_not\\( file.exists\\(helper_path\\), ",
      "\"local smoke tooling is excluded from package builds\" \\)"
    )
  )
  expect_match(test_text, "source\\(helper_path\\)")
})

test_that("local smoke artifact refresh requests represented metadata", {
  fetch_script <- testthat::test_path(
    "..",
    "..",
    "tools",
    "fetch-local-finalize-smoke.R"
  )
  fetch_text <- paste(readLines(fetch_script, warn = FALSE), collapse = "\n")

  expect_match(fetch_text, '"embedded_data"', fixed = TRUE)
  expect_match(fetch_text, '"scoring"', fixed = TRUE)
  expect_match(fetch_text, 'attr(responses, "column_map"', fixed = TRUE)
  expect_match(fetch_text, 'attr(sanitized$data, "column_map"', fixed = TRUE)
})

test_that("local smoke parity uses package response-column classification", {
  smoke_script <- testthat::test_path(
    "..",
    "..",
    "tools",
    "local-finalize-smoke.R"
  )
  smoke_text <- paste(readLines(smoke_script, warn = FALSE), collapse = "\n")

  expect_match(smoke_text, "classify_response_column_map", fixed = TRUE)
  expect_match(smoke_text, "normalise_embedded_data_fields", fixed = TRUE)
  expect_match(smoke_text, "normalise_scoring_variables", fixed = TRUE)
  expect_no_match(smoke_text, "raw_text_analysis_sidecar", fixed = TRUE)
  expect_no_match(
    smoke_text,
    "smoke_text_analysis_metric_suffix_pattern",
    fixed = TRUE
  )
  expect_match(smoke_text, "metadata = artifacts$metadata", fixed = TRUE)
  expect_match(smoke_text, "description = artifacts$description", fixed = TRUE)
  expect_no_match(
    smoke_text,
    "raw-to-dictionary parity is not a hard check",
    fixed = TRUE
  )
})

test_that("response column parity checks all non-system raw columns", {
  raw_metadata <- synthetic_column_map_sidecar_raw_metadata()
  metadata <- raw_metadata$metadata
  description <- raw_metadata$description
  response_column_map <- raw_metadata$response_column_map
  sidecar_ids <- c(
    glad_sa6_text_analysis_sidecar_ids(),
    edgi_signup_text_analysis_sidecar_ids()
  )
  raw_response_columns <- c(
    "StartTime",
    "EndDate",
    "Q_URL",
    "QID1",
    "QID1_3_TEXT",
    "QID508_TEXT",
    "QID700_1",
    "QID121_1",
    "QID1_DO_1",
    "x27_QID700_1",
    "8_QID508_TEXT",
    sidecar_ids,
    "SC_TOTAL",
    "SC_HIDDEN",
    "Source Channel",
    "Cohort"
  )

  parity <- response_column_id_parity(
    dict_response_column_ids = c(
      "QID1",
      "QID1_3_TEXT",
      "QID508_TEXT",
      "QID700_1",
      "QID121_1",
      sidecar_ids,
      "SC_TOTAL",
      "Source Channel"
    ),
    raw_response_columns = raw_response_columns,
    metadata = metadata,
    description = description,
    response_column_map = response_column_map
  )

  expect_false(parity$ok)
  expect_identical(parity$missing_from_raw_response, character())
  expect_setequal(
    parity$non_system_raw_response_columns,
    c(
      "QID1",
      "QID1_3_TEXT",
      "QID508_TEXT",
      "QID700_1",
      "QID121_1",
      "QID1_DO_1",
      "x27_QID700_1",
      "8_QID508_TEXT",
      sidecar_ids,
      "SC_TOTAL",
      "SC_HIDDEN",
      "Source Channel",
      "Cohort"
    )
  )
  expect_setequal(
    parity$missing_from_dict,
    c(
      "QID1_DO_1",
      "x27_QID700_1",
      "8_QID508_TEXT",
      "SC_HIDDEN",
      "Cohort"
    )
  )
  expect_setequal(
    parity$question_auxiliary_exported_columns,
    c("x27_QID700_1", "8_QID508_TEXT")
  )
  expect_setequal(
    parity$system_raw_response_columns,
    c("StartTime", "EndDate", "Q_URL")
  )
  expect_false("StartTime" %in% parity$non_system_raw_response_columns)
  expect_false("EndDate" %in% parity$non_system_raw_response_columns)
  expect_false("Q_URL" %in% parity$non_system_raw_response_columns)
  expect_true(all(
    c(
      "response_column_id",
      "row_source",
      "reason",
      "parent_type",
      "parent_selector",
      "parent_sub_selector",
      "present_in_dict",
      "system_excluded"
    ) %in%
      names(parity$raw_response_column_classification)
  ))
  row_source <- stats::setNames(
    parity$raw_response_column_classification$row_source,
    parity$raw_response_column_classification$response_column_id
  )
  expect_identical(row_source[["QID1_DO_1"]], "unclassified")
  expect_identical(row_source[["x27_QID700_1"]], "question_auxiliary")
  expect_identical(row_source[["StartTime"]], "system")
})

test_that("response column parity reports question auxiliary diagnostics", {
  raw_metadata <- synthetic_sbs_multiple_answer_raw_metadata()
  raw_metadata$response_column_map <- tibble::tibble(
    qname = c("QID2#3_2_1", "QID2#3_4_2", "QID2#1_4_TEXT"),
    ImportId = c("QID2#3_2_1", "QID2#3_4_2", "QID2#1_4_TEXT"),
    description = c(
      "Side by side - Multiple column - Second row - Checked A",
      "Side by side - Multiple column - Fourth row - Checked B",
      "Side by side - Text column - Fourth row"
    ),
    main = rep("Side by side", 3),
    sub = c(
      "Multiple column - Second row - Checked A",
      "Multiple column - Fourth row - Checked B",
      "Text column - Fourth row"
    )
  )

  parity <- response_column_id_parity(
    dict_response_column_ids = c("QID2#3_2_1", "QID2#3_4_2"),
    raw_response_columns = raw_metadata$response_column_map$ImportId,
    metadata = raw_metadata$metadata,
    description = raw_metadata$description,
    response_column_map = raw_metadata$response_column_map
  )

  expect_false(parity$ok)
  expect_identical(parity$missing_from_dict, "QID2#1_4_TEXT")
  expect_identical(
    parity$question_auxiliary_exported_columns,
    "QID2#1_4_TEXT"
  )
})

test_that("response column parity reports dict columns missing from raw", {
  parity <- response_column_id_parity(
    dict_response_column_ids = c("QID1", "SC_TOTAL"),
    raw_response_columns = "QID1",
    response_column_map = tibble::tibble(ImportId = "QID1")
  )

  expect_false(parity$ok)
  expect_identical(parity$missing_from_raw_response, "SC_TOTAL")
  expect_identical(parity$missing_from_dict, character())
})

test_that("response column parity checks raw questions without column maps", {
  parity <- response_column_id_parity(
    dict_response_column_ids = character(),
    raw_response_columns = c("QID1", "QID1_DO_1")
  )

  expect_false(parity$ok)
  expect_identical(
    parity$non_system_raw_response_columns,
    c("QID1", "QID1_DO_1")
  )
  expect_identical(
    parity$missing_from_dict,
    c("QID1", "QID1_DO_1")
  )
})

test_that("parse_smoke_functions trims whitespace and removes duplicates", {
  expect_identical(
    parse_smoke_functions(paste0(
      " fetch_labelled_survey_data, dict_split_blocks,",
      "fetch_labelled_survey_data "
    )),
    c("fetch_labelled_survey_data", "dict_split_blocks")
  )
})

test_that("parse_smoke_functions rejects empty selections", {
  expect_error(
    parse_smoke_functions(" , "),
    "Select at least one smoke-covered exported function"
  )
})

test_that("parse_smoke_functions rejects unknown selections", {
  expect_error(
    parse_smoke_functions("fetch_labelled_survey_data,unknown_function"),
    "Unknown smoke-covered exported function"
  )
})

test_that("parse_smoke_variable_names defaults to question_name", {
  expect_identical(parse_smoke_variable_names(NULL), "question_name")
})

test_that("parse_smoke_variable_names accepts the question_name route", {
  expect_identical(
    parse_smoke_variable_names(" question_name, question_name "),
    "question_name"
  )
})

test_that("parse_smoke_variable_names rejects disabled and unknown routes", {
  expect_error(
    parse_smoke_variable_names(" , "),
    "Select at least one Variable Dictionary route"
  )
  expect_error(
    parse_smoke_variable_names("semantic_name"),
    "Semantic Name smoke route is disabled"
  )
  expect_error(
    parse_smoke_variable_names("all"),
    "Semantic Name smoke route is disabled"
  )
  expect_error(
    parse_smoke_variable_names("question_name,bad_route"),
    "Unknown Variable Dictionary route"
  )
})

test_that("smoke_summary_names maps functions to summary object names", {
  expect_identical(
    smoke_summary_names(c("dict_generate", "fetch_labelled_survey_data")),
    c("dict", "level_universe", "labelled", "labelled_excluding_validation")
  )
  expect_identical(
    smoke_summary_names(c("dict_split_blocks", "survey_split_blocks")),
    c("dict_blocks", "survey_blocks")
  )
})

test_that("project_smoke_record keeps only selected summaries and hashes", {
  record <- list(
    alias = "survey_a",
    survey_id = "SV_123",
    variable_name = "question_name",
    generated_at = "2026-06-26T00:00:00Z",
    summaries = list(
      labelled = list(object_hash = "labelled-hash"),
      dict_blocks = list(object_hash = "dict-blocks-hash"),
      dict = list(object_hash = "dict-hash"),
      validation = list(object_hash = "validation-hash"),
      labelled_excluding_validation = list(object_hash = "labelled-ex-hash")
    ),
    object_hashes = list(
      labelled = "labelled-hash",
      dict_blocks = "dict-blocks-hash",
      dict = "dict-hash",
      validation = "validation-hash",
      labelled_excluding_validation = "labelled-ex-hash"
    ),
    scenario_hash = "full-hash"
  )

  projected <- project_smoke_record(
    record,
    c("dict_split_blocks", "fetch_labelled_survey_data")
  )

  expect_named(
    projected$summaries,
    c("dict_blocks", "labelled", "labelled_excluding_validation")
  )
  expect_identical(
    projected$object_hashes,
    list(
      dict_blocks = "dict-blocks-hash",
      labelled = "labelled-hash",
      labelled_excluding_validation = "labelled-ex-hash"
    )
  )
  expect_match(projected$scenario_hash, "^[0-9a-f]{32}$")
  expect_false(identical(projected$scenario_hash, "full-hash"))
})

test_that("project_smoke_record omits unavailable optional summaries", {
  record <- list(
    alias = "survey_a",
    survey_id = "SV_123",
    variable_name = "semantic_name",
    generated_at = "2026-06-26T00:00:00Z",
    summaries = list(labelled = list(object_hash = "labelled-hash")),
    object_hashes = list(labelled = "labelled-hash"),
    scenario_hash = "full-hash"
  )

  projected <- project_smoke_record(record, "fetch_labelled_survey_data")

  expect_named(projected$summaries, "labelled")
  expect_identical(projected$object_hashes, list(labelled = "labelled-hash"))
})

test_that("project_smoke_record keeps full-run order", {
  record <- list(
    alias = "survey_a",
    survey_id = "SV_123",
    variable_name = "question_name",
    generated_at = "2026-06-26T00:00:00Z",
    summaries = list(
      dict = list(object_hash = "dict-hash"),
      validation = list(object_hash = "validation-hash"),
      labelled = list(object_hash = "labelled-hash"),
      labelled_excluding_validation = list(
        object_hash = "labelled-ex-hash"
      ),
      labelled_export_findings = list(
        object_hash = "export-findings-hash"
      ),
      level_universe = list(object_hash = "level-universe-hash"),
      dict_blocks = list(object_hash = "dict-blocks-hash"),
      survey_blocks = list(object_hash = "survey-blocks-hash")
    ),
    object_hashes = list(
      dict = "dict-hash",
      validation = "validation-hash",
      labelled = "labelled-hash",
      labelled_excluding_validation = "labelled-ex-hash",
      labelled_export_findings = "export-findings-hash",
      level_universe = "level-universe-hash",
      dict_blocks = "dict-blocks-hash",
      survey_blocks = "survey-blocks-hash"
    ),
    scenario_hash = "full-hash"
  )

  projected <- project_smoke_record(record, parse_smoke_functions(NULL))

  expect_named(
    projected$summaries,
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
  )
  expect_identical(
    projected$object_hashes,
    list(
      dict = "dict-hash",
      level_universe = "level-universe-hash",
      validation = "validation-hash",
      labelled = "labelled-hash",
      labelled_export_findings = "export-findings-hash",
      dict_blocks = "dict-blocks-hash",
      survey_blocks = "survey-blocks-hash",
      labelled_excluding_validation = "labelled-ex-hash"
    )
  )
  expect_match(projected$scenario_hash, "^[0-9a-f]{32}$")
  expect_false(identical(projected$scenario_hash, "full-hash"))
})

test_that("bless_smoke_record replaces stale baseline entries in full mode", {
  existing <- list(
    alias = "survey_a",
    survey_id = "SV_123",
    variable_name = "question_name",
    generated_at = "old-time",
    summaries = list(
      dict = list(object_hash = "old-dict"),
      validation = list(object_hash = "old-validation"),
      stale_summary = list(object_hash = "old-stale")
    ),
    object_hashes = list(
      dict = "old-dict",
      validation = "old-validation",
      stale_summary = "old-stale"
    ),
    scenario_hash = "old-scenario"
  )
  selected <- list(
    alias = "survey_a",
    survey_id = "SV_123",
    variable_name = "question_name",
    generated_at = "new-time",
    summaries = list(
      dict = list(object_hash = "new-dict"),
      labelled = list(object_hash = "new-labelled")
    ),
    object_hashes = list(
      dict = "new-dict",
      labelled = "new-labelled"
    ),
    scenario_hash = "selected-scenario"
  )

  blessed <- bless_smoke_record(existing, selected, selective = FALSE)

  expect_identical(blessed, selected)
  expect_false("stale_summary" %in% names(blessed$summaries))
  expect_false("stale_summary" %in% names(blessed$object_hashes))
})

test_that("bless_smoke_record merges selected summaries in selective mode", {
  existing <- list(
    alias = "survey_a",
    survey_id = "SV_123",
    variable_name = "question_name",
    generated_at = "old-time",
    summaries = list(
      dict = list(object_hash = "old-dict"),
      validation = list(object_hash = "old-validation"),
      survey_blocks = list(object_hash = "old-survey-blocks")
    ),
    object_hashes = list(
      dict = "old-dict",
      validation = "old-validation",
      survey_blocks = "old-survey-blocks"
    ),
    scenario_hash = "old-scenario"
  )
  selected <- list(
    alias = "survey_a",
    survey_id = "SV_123",
    variable_name = "question_name",
    generated_at = "new-time",
    summaries = list(labelled = list(object_hash = "new-labelled")),
    object_hashes = list(labelled = "new-labelled"),
    scenario_hash = "selected-scenario"
  )

  blessed <- bless_smoke_record(existing, selected, selective = TRUE)

  expect_named(
    blessed$summaries,
    c("dict", "validation", "labelled", "survey_blocks")
  )
  expect_identical(blessed$summaries$dict, list(object_hash = "old-dict"))
  expect_identical(
    blessed$summaries$labelled,
    list(object_hash = "new-labelled")
  )
  expect_identical(
    blessed$summaries$survey_blocks,
    list(object_hash = "old-survey-blocks")
  )
  expect_identical(blessed$generated_at, "new-time")
})

test_that("merge_smoke_baseline updates only selected summaries and hashes", {
  existing <- list(
    alias = "survey_a",
    survey_id = "SV_123",
    variable_name = "question_name",
    generated_at = "old-time",
    summaries = list(
      dict = list(object_hash = "old-dict"),
      labelled = list(object_hash = "old-labelled")
    ),
    object_hashes = list(
      dict = "old-dict",
      labelled = "old-labelled"
    ),
    scenario_hash = "old-scenario"
  )
  selected <- list(
    alias = "survey_a",
    survey_id = "SV_123",
    variable_name = "question_name",
    generated_at = "new-time",
    summaries = list(labelled = list(object_hash = "new-labelled")),
    object_hashes = list(labelled = "new-labelled"),
    scenario_hash = "selected-scenario"
  )

  merged <- merge_smoke_baseline(existing, selected)

  expect_identical(merged$summaries$dict, list(object_hash = "old-dict"))
  expect_identical(
    merged$summaries$labelled,
    list(object_hash = "new-labelled")
  )
  expect_identical(
    merged$object_hashes,
    list(
      dict = "old-dict",
      labelled = "new-labelled"
    )
  )
  expect_match(merged$scenario_hash, "^[0-9a-f]{32}$")
  expect_false(identical(merged$scenario_hash, "old-scenario"))
  expect_identical(merged$generated_at, "new-time")
})

test_that("merge_smoke_baseline restores canonical order for known summaries", {
  existing <- list(
    alias = "survey_a",
    survey_id = "SV_123",
    variable_name = "question_name",
    generated_at = "old-time",
    summaries = list(
      dict = list(object_hash = "old-dict"),
      survey_blocks = list(object_hash = "old-survey-blocks")
    ),
    object_hashes = list(
      dict = "old-dict",
      survey_blocks = "old-survey-blocks"
    ),
    scenario_hash = "old-scenario"
  )
  selected <- list(
    alias = "survey_a",
    survey_id = "SV_123",
    variable_name = "question_name",
    generated_at = "new-time",
    summaries = list(labelled = list(object_hash = "new-labelled")),
    object_hashes = list(labelled = "new-labelled"),
    scenario_hash = "selected-scenario"
  )

  merged <- merge_smoke_baseline(existing, selected)

  expect_named(
    merged$summaries,
    c("dict", "labelled", "survey_blocks")
  )
  expect_identical(
    merged$object_hashes,
    list(
      dict = "old-dict",
      labelled = "new-labelled",
      survey_blocks = "old-survey-blocks"
    )
  )
  expect_match(merged$scenario_hash, "^[0-9a-f]{32}$")
})

test_that("smoke_scenario_requirements marks needed setup", {
  expect_identical(
    smoke_scenario_requirements("dict_generate"),
    list(
      needs_dict = TRUE,
      needs_validation = FALSE,
      needs_labelled = FALSE,
      needs_export_findings = FALSE,
      needs_dict_blocks = FALSE,
      needs_survey_blocks = FALSE
    )
  )

  expect_identical(
    smoke_scenario_requirements("labelled_export_findings"),
    list(
      needs_dict = TRUE,
      needs_validation = FALSE,
      needs_labelled = TRUE,
      needs_export_findings = TRUE,
      needs_dict_blocks = FALSE,
      needs_survey_blocks = FALSE
    )
  )

  expect_identical(
    smoke_scenario_requirements("survey_split_blocks"),
    list(
      needs_dict = TRUE,
      needs_validation = FALSE,
      needs_labelled = TRUE,
      needs_export_findings = FALSE,
      needs_dict_blocks = FALSE,
      needs_survey_blocks = TRUE
    )
  )
})

test_that("local finalize smoke help distinguishes full and selective bless", {
  script_path <- testthat::test_path(
    "..",
    "..",
    "tools",
    "local-finalize-smoke.R"
  )
  skip_if_not(
    file.exists(script_path),
    "local smoke tooling is excluded from package builds"
  )

  help_output <- system2(
    command = file.path(R.home("bin"), "Rscript"),
    args = c(script_path, "--help"),
    stdout = TRUE,
    stderr = TRUE
  )
  help_text <- gsub("\\s+", " ", paste(help_output, collapse = "\n"))

  expect_match(help_text, "--functions", fixed = TRUE)
  expect_no_match(help_text, "--survey-count", fixed = TRUE)
  expect_no_match(help_text, "--survey-seed", fixed = TRUE)
  expect_match(help_text, "--variable-name", fixed = TRUE)
  expect_match(
    help_text,
    "Full `bless` replaces local baselines with current output hashes",
    fixed = TRUE
  )
  expect_match(
    help_text,
    paste0(
      "`bless --functions` updates selected output summaries inside ",
      "existing baselines"
    ),
    fixed = TRUE
  )
})

test_that("local finalize smoke rejects legacy survey selection flags", {
  script_path <- testthat::test_path(
    "..",
    "..",
    "tools",
    "local-finalize-smoke.R"
  )
  skip_if_not(
    file.exists(script_path),
    "local smoke tooling is excluded from package builds"
  )

  survey_count_output <- suppressWarnings(system2(
    command = file.path(R.home("bin"), "Rscript"),
    args = c(script_path, "check", "--survey-count", "2"),
    stdout = TRUE,
    stderr = TRUE
  ))
  survey_count_status <- attr(survey_count_output, "status")

  survey_output <- suppressWarnings(system2(
    command = file.path(R.home("bin"), "Rscript"),
    args = c(script_path, "check", "--survey", "survey_a"),
    stdout = TRUE,
    stderr = TRUE
  ))
  survey_status <- attr(survey_output, "status")

  expect_identical(survey_count_status, 2L)
  expect_match(
    paste(survey_count_output, collapse = "\n"),
    "Unknown option: --survey-count.",
    fixed = TRUE
  )
  expect_identical(survey_status, 2L)
  expect_match(
    paste(survey_output, collapse = "\n"),
    "Unknown option: --survey.",
    fixed = TRUE
  )
})

test_that("local finalize smoke config keeps the thirteen-survey surface", {
  config_path <- testthat::test_path(
    "..",
    "..",
    "tools",
    "local-finalize-smoke-surveys.json"
  )
  skip_if_not(
    file.exists(config_path),
    "local smoke tooling is excluded from package builds"
  )

  config_lines <- readLines(config_path, warn = FALSE)
  aliases <- sub(
    '^\\s*"alias": "([^"]+)",?\\s*$',
    "\\1",
    grep('"alias":', config_lines, value = TRUE, fixed = TRUE)
  )

  expect_length(aliases, 12L)
  expect_identical(
    aliases,
    c(
      "glad_sa8_signup",
      "edgi_signup_official",
      "glad_sa7_optional",
      "edgi_optional_all",
      "glad_sa6_signup",
      "glad_medications",
      "ramp",
      "coping_glad",
      "coping_edgi",
      "coping_nbr",
      "coping_fupb_ongoing",
      "ramp_fupb"
    )
  )
})

test_that("smoke_result_line includes detailed row counts for full summaries", {
  result <- list(
    alias = "survey_a",
    variable_name = "question_name",
    scenario_hash = "abc123",
    summaries = list(
      dict = list(rows = 12L),
      labelled = list(rows = 34L, columns = 5L)
    )
  )

  expect_identical(
    smoke_result_line(result, "matched"),
    paste0(
      "survey_a / question_name: matched hash=abc123 dict_rows=12 ",
      "labelled_rows=34 labelled_cols=5"
    )
  )
})

test_that("smoke_result_line falls back to outputs for selective summaries", {
  result <- list(
    alias = "survey_a",
    variable_name = "semantic_name",
    scenario_hash = "def456",
    summaries = list(
      validation = list(validation_findings_rows = 2L),
      dict_blocks = list(block_count = 3L)
    )
  )

  expect_identical(
    smoke_result_line(result, "current"),
    paste0(
      "survey_a / semantic_name: current hash=def456 ",
      "outputs=validation,dict_blocks"
    )
  )
})

test_that("smoke_mismatch_lines includes summary deltas", {
  baseline <- list(
    alias = "survey_a",
    variable_name = "question_name",
    scenario_hash = "oldhash",
    summaries = list(
      dict = list(rows = 12L),
      labelled = list(rows = 34L, columns = 5L),
      validation = list(validation_findings_rows = 2L)
    )
  )
  current <- list(
    alias = "survey_a",
    variable_name = "question_name",
    scenario_hash = "newhash",
    summaries = list(
      dict = list(rows = 13L),
      labelled = list(rows = 35L, columns = 6L),
      validation = list(validation_findings_rows = 4L)
    )
  )

  expect_identical(
    smoke_mismatch_lines(current, baseline),
    c(
      "Mismatch: survey_a / question_name",
      "  baseline hash: oldhash",
      "  current hash:  newhash",
      "  outputs: dict, labelled, validation",
      "  dict rows: 12 -> 13",
      "  labelled dims: 34x5 -> 35x6",
      "  validation findings rows: 2 -> 4"
    )
  )
})

test_that("smoke_mismatch_lines reports selective outputs", {
  baseline <- list(
    alias = "survey_a",
    variable_name = "semantic_name",
    scenario_hash = "oldhash",
    summaries = list(dict_blocks = list(block_count = 2L))
  )
  current <- list(
    alias = "survey_a",
    variable_name = "semantic_name",
    scenario_hash = "newhash",
    summaries = list(dict_blocks = list(block_count = 3L))
  )

  expect_identical(
    smoke_mismatch_lines(current, baseline),
    c(
      "Mismatch: survey_a / semantic_name",
      "  baseline hash: oldhash",
      "  current hash:  newhash",
      "  outputs: dict_blocks"
    )
  )
})

test_that("level_universe_code_safe keeps only short numeric code sets", {
  expect_true(level_universe_code_safe(c("0", "1")))
  expect_true(level_universe_code_safe(c("-77", "1", "0")))
  expect_false(level_universe_code_safe(as.character(1:13)))
  expect_false(level_universe_code_safe("TEXT_000001"))
  expect_false(level_universe_code_safe("1234567"))
  expect_false(level_universe_code_safe(character()))
})

test_that("per-choice columns are classified from the question type", {
  dict <- tibble::tibble(
    response_column_id = c(
      "QID1215196547_0",
      "QID1215196512",
      "QID1_x1_1",
      "QID2_1"
    ),
    level = c("1", "1", "1", "1"),
    type = c("MC", "MC", "Matrix", "Matrix"),
    selector = c("MAVR", "SAVR", "Likert", "Likert"),
    sub_selector = c("TX", "TX", "MultipleAnswer", "SingleAnswer")
  )

  per_choice_columns <- level_universe_per_choice_columns(dict)

  expect_setequal(per_choice_columns, c("QID1215196547_0", "QID1_x1_1"))
  expect_identical(
    level_universe_shape("QID1215196547_0", per_choice_columns),
    "one_column_per_choice"
  )
  expect_identical(
    level_universe_shape("QID1215196512", per_choice_columns),
    "ordinary"
  )
})

test_that("level_universe_compare reports per-choice data violations", {
  # The worked example: `QID1215196547_0` declared the choice recode `0`, but
  # the raw export stores the tick marker `1` for the persons who ticked it.
  comparison <- level_universe_compare(
    list(
      response_column_id = "QID1215196547_0",
      declared_levels = list("0"),
      codes = list("1"),
      counts = list(34L),
      redacted = FALSE,
      n_non_missing = 34L,
      n_out_of_universe_at_fetch = 34L
    ),
    current_levels = "0",
    per_choice_columns = "QID1215196547_0"
  )

  expect_identical(comparison$status, "data_violation")
  expect_identical(comparison$out_codes, "1")
  expect_identical(comparison$shape, "one_column_per_choice")
  expect_identical(comparison$values_out_of_universe, 34L)
})

test_that("level_universe_compare accepts the tick Level universe", {
  comparison <- level_universe_compare(
    list(
      response_column_id = "QID1215196547_0",
      declared_levels = list("1"),
      codes = list("1"),
      counts = list(34L),
      redacted = FALSE,
      n_non_missing = 34L,
      n_out_of_universe_at_fetch = 0L
    ),
    current_levels = "1"
  )

  expect_identical(comparison$status, "clean")
  expect_identical(comparison$out_codes, character())
  expect_identical(comparison$values_out_of_universe, 0L)
})

test_that("drift annotates a violation instead of masking it", {
  # The real `11_QID1212948321_12` case: no universe was declared at fetch
  # time, the dictionary now declares {12}, and the raw data holds "1". The
  # declaration drifted AND "1" is outside the CURRENT universe. Raw codes do
  # not change when a dictionary does, so this is a genuine violation and must
  # be reported as one -- ranking drift first would report zero violations for
  # precisely the regression this check exists to catch.
  comparison <- level_universe_compare(
    list(
      response_column_id = "11_QID1212948321_12",
      declared_levels = list(),
      codes = list("1"),
      counts = list(3L),
      redacted = FALSE,
      n_non_missing = 3L,
      n_out_of_universe_at_fetch = 0L
    ),
    current_levels = "12"
  )

  expect_identical(comparison$status, "data_violation")
  expect_true(comparison$drifted)
  expect_identical(comparison$out_codes, "1")
  expect_identical(comparison$values_out_of_universe, 3L)
})

test_that("drift alone, with no violation, still reports as drift", {
  comparison <- level_universe_compare(
    list(
      response_column_id = "QID1215196547_0",
      declared_levels = list("1"),
      codes = list("1"),
      counts = list(8L),
      redacted = FALSE,
      n_non_missing = 8L,
      n_out_of_universe_at_fetch = 0L
    ),
    current_levels = c("1", "2")
  )

  expect_identical(comparison$status, "declared_universe_drift")
  expect_true(comparison$drifted)
  expect_identical(comparison$out_codes, character())
})

test_that("a clean column is not marked as drifted", {
  comparison <- level_universe_compare(
    list(
      response_column_id = "QID1215196547_0",
      declared_levels = list("1"),
      codes = list("1"),
      counts = list(8L),
      redacted = FALSE,
      n_non_missing = 8L,
      n_out_of_universe_at_fetch = 0L
    ),
    current_levels = "1"
  )

  expect_identical(comparison$status, "clean")
  expect_false(comparison$drifted)
})

test_that("level_universe_compare treats text markers as free text", {
  comparison <- level_universe_compare(
    list(
      response_column_id = "QID1215196531_2_TEXT",
      declared_levels = list("2_TEXT"),
      codes = list("0", "3"),
      counts = list(4L, 11L),
      redacted = FALSE,
      n_non_missing = 15L,
      n_out_of_universe_at_fetch = 15L
    ),
    current_levels = "2_TEXT"
  )

  expect_identical(comparison$status, "text_column")
  expect_identical(comparison$out_codes, character())
  expect_identical(comparison$values_out_of_universe, 0L)
})

test_that("level_universe_compare carries redacted columns forward", {
  comparison <- level_universe_compare(
    list(
      response_column_id = "QID508_TEXT",
      declared_levels = list("1"),
      redacted = TRUE,
      n_non_missing = 12L,
      n_out_of_universe_at_fetch = 12L
    ),
    current_levels = "1"
  )

  expect_identical(comparison$status, "redacted_carry_forward")
  expect_identical(comparison$values_out_of_universe, 12L)
  expect_identical(comparison$out_codes, character())
})

test_that("level_universe_observation records raw codes before sanitization", {
  responses <- tibble::tibble(
    QID1_0 = c("1", NA, "1"),
    QID1_1 = c(NA, "1", NA),
    QID508_TEXT = c("a free text answer", NA, NA),
    Cohort = c("x", "y", "z")
  )

  observation <- level_universe_observation(
    responses,
    allowed_levels = list(
      QID1_0 = "0",
      QID1_1 = "1",
      QID508_TEXT = "1"
    ),
    per_choice_columns = c("QID1_0", "QID1_1")
  )

  expect_identical(observation$schema, level_universe_schema_version())
  expect_identical(observation$response_rows, 3L)
  expect_identical(observation$totals$columns_with_universe, 3L)
  expect_identical(observation$totals$columns_observed, 3L)
  expect_identical(observation$totals$values_out_of_universe, 3L)
  expect_identical(observation$totals$redacted_columns, 1L)

  observed <- stats::setNames(
    observation$observed,
    vapply(observation$observed, `[[`, character(1), "response_column_id")
  )
  expect_identical(observed$QID1_0$codes, list("1"))
  expect_identical(observed$QID1_0$shape, "one_column_per_choice")
  expect_false(observed$QID1_0$redacted)
  expect_true(observed$QID508_TEXT$redacted)
  expect_null(observed$QID508_TEXT$codes)
})

test_that("summarise_level_universe counts violations and fingerprints them", {
  result <- list(
    available = TRUE,
    schema = 1L,
    response_rows = 400L,
    columns_with_universe = 3L,
    values_observed = 70L,
    comparisons = list(
      level_universe_compare(
        list(
          response_column_id = "QID1_0",
          declared_levels = list("0"),
          codes = list("1"),
          counts = list(34L),
          redacted = FALSE,
          n_non_missing = 34L,
          n_out_of_universe_at_fetch = 34L
        ),
        current_levels = "0",
        per_choice_columns = c("QID1_0", "QID1_1")
      ),
      level_universe_compare(
        list(
          response_column_id = "QID1_1",
          declared_levels = list("1"),
          codes = list("1"),
          counts = list(36L),
          redacted = FALSE,
          n_non_missing = 36L,
          n_out_of_universe_at_fetch = 0L
        ),
        current_levels = "1",
        per_choice_columns = c("QID1_0", "QID1_1")
      )
    )
  )

  summary <- summarise_level_universe(result)

  expect_true(summary$available)
  expect_identical(summary$columns_observed, 2L)
  expect_identical(summary$violating_columns, 1L)
  expect_identical(summary$values_out_of_universe, 34L)
  expect_identical(summary$one_column_per_choice_violations, 1L)
  expect_identical(summary$violation_fingerprint, list("QID1_0:0->1"))
  expect_match(summary$object_hash, "^[0-9a-f]{32}$")
})

test_that("local smoke artifact refresh records the Level universe", {
  fetch_script <- testthat::test_path(
    "..",
    "..",
    "tools",
    "fetch-local-finalize-smoke.R"
  )
  fetch_lines <- readLines(fetch_script, warn = FALSE)
  fetch_text <- paste(fetch_lines, collapse = "\n")

  expect_match(fetch_text, "level_universe = level_universe", fixed = TRUE)
  expect_match(fetch_text, "--allow-small-sample", fixed = TRUE)

  observation_line <- grep(
    "level_universe <- level_universe_observation(",
    fetch_lines,
    fixed = TRUE
  )
  sanitize_line <- grep(
    "sanitized <- sanitize_responses(",
    fetch_lines,
    fixed = TRUE
  )
  remove_line <- grep("^  rm\\(responses\\)$", fetch_lines)

  expect_length(observation_line, 1L)
  expect_length(sanitize_line, 1L)
  expect_length(remove_line, 1L)
  # Observing after sanitization would pass vacuously: sanitization rewrites
  # every non-missing value with values cycled from the declared universe.
  expect_lt(observation_line, sanitize_line)
  expect_lt(observation_line, remove_line)
})

test_that("local smoke check gates missing and vacuous Level observations", {
  smoke_script <- testthat::test_path(
    "..",
    "..",
    "tools",
    "local-finalize-smoke.R"
  )
  smoke_text <- paste(readLines(smoke_script, warn = FALSE), collapse = "\n")

  expect_match(smoke_text, "assert_level_universe", fixed = TRUE)
  expect_match(smoke_text, "level_universe_artifact_error", fixed = TRUE)
  expect_match(smoke_text, "level_universe_error", fixed = TRUE)
  expect_match(
    smoke_text,
    "level_universe_minimum_response_rows()",
    fixed = TRUE
  )
})

test_that("level universe artifacts are checked when they exist", {
  manifest_path <- testthat::test_path(
    "..",
    "..",
    ".local",
    "finalize-smoke",
    "source",
    "glad_sa8_signup",
    "manifest.json"
  )
  skip_if_not(
    file.exists(manifest_path),
    "local smoke artifacts are not available"
  )
  manifest <- read_smoke_manifest(manifest_path)
  skip_if_not(
    !is.null(manifest$level_universe),
    "artifacts predate the Level universe observation; refetch"
  )

  observation <- manifest$level_universe
  expect_identical(
    as.integer(observation$schema),
    level_universe_schema_version()
  )
  expect_gte(
    as.integer(observation$response_rows),
    level_universe_minimum_response_rows()
  )
  expect_gt(as.integer(observation$totals$values_observed), 0L)
  expect_false(any(vapply(
    observation$observed,
    function(record) {
      isTRUE(record$redacted) && !is.null(record$codes)
    },
    logical(1)
  )))
})
