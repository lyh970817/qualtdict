source(testthat::test_path(
  "..",
  "..",
  "tools",
  "local-finalize-smoke-lib.R"
))

test_that("parse_smoke_functions defaults to all smoke-covered functions", {
  expect_identical(
    parse_smoke_functions(NULL),
    c(
      "dict_generate",
      "dict_validate",
      "get_survey_data",
      "labelled_export_findings",
      "dict_split_blocks",
      "survey_split_blocks"
    )
  )
})

test_that("parse_smoke_functions trims whitespace and removes duplicates", {
  expect_identical(
    parse_smoke_functions(" get_survey_data, dict_split_blocks,get_survey_data "),
    c("get_survey_data", "dict_split_blocks")
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
    parse_smoke_functions("get_survey_data,unknown_function"),
    "Unknown smoke-covered exported function"
  )
})

test_that("smoke_summary_names maps functions to summary object names", {
  expect_identical(
    smoke_summary_names(c("dict_generate", "get_survey_data")),
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
    c("dict_split_blocks", "get_survey_data")
  )

  expect_identical(
    names(projected$summaries),
    c("dict_blocks", "labelled", "labelled_excluding_validation")
  )
  expect_identical(projected$object_hashes, list(
    dict_blocks = "dict-blocks-hash",
    labelled = "labelled-hash",
    labelled_excluding_validation = "labelled-ex-hash"
  ))
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

  projected <- project_smoke_record(record, "get_survey_data")

  expect_identical(names(projected$summaries), "labelled")
  expect_identical(projected$object_hashes, list(labelled = "labelled-hash"))
})

test_that(
  "project_smoke_record preserves full record order and hash for full selection",
  {
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

    expect_identical(names(projected$summaries), names(record$summaries))
    expect_identical(names(projected$object_hashes), names(record$object_hashes))
    expect_identical(projected$scenario_hash, "full-hash")
  }
)

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
  expect_identical(merged$summaries$labelled, list(object_hash = "new-labelled"))
  expect_identical(merged$object_hashes, list(
    dict = "old-dict",
    labelled = "new-labelled"
  ))
  expect_match(merged$scenario_hash, "^[0-9a-f]{32}$")
  expect_false(identical(merged$scenario_hash, "old-scenario"))
  expect_identical(merged$generated_at, "new-time")
})

test_that("smoke_scenario_requirements marks setup needed by selected functions", {
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
    "survey_a / question_name: matched hash=abc123 dict_rows=12 labelled_rows=34 labelled_cols=5"
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
    "survey_a / semantic_name: current hash=def456 outputs=validation,dict_blocks"
  )
})

test_that("smoke_mismatch_lines includes detailed summary deltas when available", {
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

test_that("smoke_mismatch_lines reports selective outputs without missing-summary assumptions", {
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
