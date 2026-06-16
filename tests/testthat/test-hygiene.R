test_that("recorded Qualtrics fixtures are scrubbed for review", {
  fixture_files <- list.files(
    test_path("../fixtures"),
    pattern = "[.]yml$",
    full.names = TRUE
  )

  expect_gt(length(fixture_files), 0)

  fixture_text <- unlist(
    lapply(fixture_files, readLines, warn = FALSE),
    use.names = FALSE
  )

  api_token_headers <- grep(
    "^\\s*X-API-TOKEN:",
    fixture_text,
    value = TRUE
  )
  expect_true(
    all(grepl("X-API-TOKEN: QUALTRICS_API_KEY", api_token_headers)),
    info = "VCR fixtures must retain only the QUALTRICS_API_KEY placeholder."
  )

  forbidden_patterns <- c(
    "qualtrics[.]kcl[.]ac[.]uk",
    "UR_cYCFQWCwckGHpf7",
    "DV_bxf8drkuO8NR2sZ",
    "RS_8eabOkW2G0HpIJE",
    "BL_eJoO9gHZSsFvXBI",
    "BL_bJjQ45bkyQSKQXs",
    "BL_06dlEES4T6Zch1Q",
    "\\bkcliop\\b"
  )

  fixture_blob <- paste(fixture_text, collapse = "\n")
  expect_false(
    any(grepl(paste(forbidden_patterns, collapse = "|"), fixture_blob)),
    info = "VCR fixtures contain account-specific Qualtrics identifiers."
  )
})

root_text <- function(...) {
  path <- file.path(test_path("../.."), ...)
  skip_if_not(
    file.exists(path),
    paste("Source document is not available in installed package:", path)
  )
  paste(
    readLines(path, warn = FALSE),
    collapse = "\n"
  )
}

test_that("README documents the qualtRics boundary and main workflow", {
  readme <- root_text("README.Rmd")

  expect_match(readme, "`qualtRics` owns API access", fixed = TRUE)
  expect_match(readme, "Variable Dictionary To Labelled Survey Data",
    fixed = TRUE
  )
  expect_match(readme, "response_column_id", fixed = TRUE)
  expect_match(readme, "qid", fixed = TRUE)
  expect_match(readme, "question_name", fixed = TRUE)
  expect_match(readme, "semantic_name", fixed = TRUE)
  expect_match(readme, "variable_name", fixed = TRUE)
  expect_match(readme, "Validation Findings", fixed = TRUE)
  expect_match(readme, "Labelled Survey Data", fixed = TRUE)
  expect_match(readme, "not stable", fixed = TRUE)
  expect_match(readme, "tested offline", fixed = TRUE)
  expect_match(readme, "require credentials", fixed = TRUE)
})

test_that("README and vignette avoid stale API names and private IDs", {
  public_docs <- paste(
    root_text("README.Rmd"),
    root_text("vignettes", "qualtdict.Rmd"),
    collapse = "\n"
  )

  expect_false(grepl("var_name", public_docs, fixed = TRUE))
  expect_false(grepl("easy_name", public_docs, fixed = TRUE))
  expect_false(grepl("easy names", public_docs, fixed = TRUE))
  expect_false(grepl("SV_4YyAHbAxpdbzacl", public_docs, fixed = TRUE))
  expect_false(grepl("qualtrics.kcl.ac.uk", public_docs, fixed = TRUE))
  expect_true(grepl("SV_XXXXXXXXXXXXXXXX", public_docs, fixed = TRUE))
})

test_that("exported function docs use canonical public terminology", {
  function_docs <- paste(
    root_text("R", "dict_generate.R"),
    root_text("R", "dict_validate.R"),
    root_text("R", "get_survey_data.R"),
    root_text("R", "metadata_normalise.R"),
    root_text("R", "split_blocks.R"),
    collapse = "\n"
  )

  canonical_terms <- c(
    "Variable Dictionary",
    "Labelled Survey Data",
    "response_column_id",
    "qid",
    "question_name",
    "semantic_name",
    "variable_name",
    "Validation Findings"
  )

  for (term in canonical_terms) {
    expect_true(
      grepl(term, function_docs, fixed = TRUE),
      info = paste("Missing canonical term:", term)
    )
  }

  expect_false(grepl("Download a labeled", function_docs, fixed = TRUE))
  expect_false(grepl("potential mistakes in the dictionary", function_docs,
    fixed = TRUE
  ))
})
