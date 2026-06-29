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

test_that("parse_smoke_variable_names accepts routes and all", {
  expect_identical(
    parse_smoke_variable_names(" semantic_name, question_name,semantic_name "),
    c("semantic_name", "question_name")
  )
  expect_identical(
    parse_smoke_variable_names("all"),
    c("question_name", "semantic_name")
  )
})

test_that("parse_smoke_variable_names rejects empty and unknown selections", {
  expect_error(
    parse_smoke_variable_names(" , "),
    "Select at least one Variable Dictionary route"
  )
  expect_error(
    parse_smoke_variable_names("question_name,bad_route"),
    "Unknown Variable Dictionary route"
  )
  expect_error(
    parse_smoke_variable_names("all,question_name"),
    "`all` cannot be combined"
  )
})

test_that("smoke_summary_names maps functions to summary object names", {
  expect_identical(
    smoke_summary_names(c("dict_generate", "fetch_labelled_survey_data")),
    c("dict", "labelled", "labelled_excluding_validation")
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
      dict_blocks = list(object_hash = "dict-blocks-hash"),
      survey_blocks = list(object_hash = "survey-blocks-hash")
    ),
    object_hashes = list(
      dict = "dict-hash",
      validation = "validation-hash",
      labelled = "labelled-hash",
      labelled_excluding_validation = "labelled-ex-hash",
      labelled_export_findings = "export-findings-hash",
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

test_that("local finalize smoke config keeps the seven-survey surface", {
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

  expect_length(aliases, 7L)
  expect_identical(
    aliases,
    c(
      "glad_sa8_signup",
      "edgi_signup_official",
      "glad_sa7_optional",
      "edgi_optional_all",
      "glad_sa6_signup",
      "edgi_medications",
      "glad_medications"
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
