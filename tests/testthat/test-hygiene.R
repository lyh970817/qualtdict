test_that("recorded Qualtrics cassettes are not committed", {
  fixture_files <- list.files(
    test_path("../fixtures"),
    pattern = "[.]yml$",
    full.names = TRUE
  )

  expect_equal(
    length(fixture_files),
    0L,
    info = paste(
      "Do not commit VCR cassettes for Qualtrics API calls;",
      "they can contain Participant Response Data."
    )
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
  expect_match(
    readme,
    "Variable Dictionary To Labelled Survey Data",
    fixed = TRUE
  )
  expect_match(readme, "response_column_id", fixed = TRUE)
  expect_match(readme, "row_source", fixed = TRUE)
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
    root_text("R", "fetch_labelled_survey_data.R"),
    root_text("R", "metadata_normalise.R"),
    root_text("R", "split_blocks.R"),
    collapse = "\n"
  )

  canonical_terms <- c(
    "Variable Dictionary",
    "Labelled Survey Data",
    "response_column_id",
    "row_source",
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
  expect_false(grepl(
    "potential mistakes in the dictionary",
    function_docs,
    fixed = TRUE
  ))
})

function_line_count <- function(path, function_name) {
  parsed <- getParseData(parse(text = root_text(path), keep.source = TRUE))
  assignment <- parsed[
    parsed$token == "SYMBOL" & parsed$text == function_name,
  ]
  assignment <- assignment[which.min(assignment$line1), ]
  function_call <- parsed[
    parsed$token == "FUNCTION" & parsed$line1 >= assignment$line1,
  ]
  function_call <- function_call[which.min(function_call$line1), ]
  function_expr <- parsed[parsed$id == function_call$parent, ]

  function_expr$line2 - function_expr$line1 + 1L
}

test_that("pkgcheck-targeted functions stay under the line limit", {
  flagged_functions <- list(
    list(path = "R/dict_generate.R", name = "dict_generate"),
    list(
      path = "R/metadata_normalise.R",
      name = "normalise_qualtrics_questions"
    ),
    list(path = "R/semantic_name.R", name = "generate_semantic_names"),
    list(
      path = "R/variable_dictionary.R",
      name = "variable_dictionary_from_normalised_metadata"
    )
  )

  line_counts <- vapply(
    flagged_functions,
    function(flagged_function) {
      function_line_count(flagged_function$path, flagged_function$name)
    },
    integer(1)
  )
  names(line_counts) <- vapply(
    flagged_functions,
    function(flagged_function) {
      paste(flagged_function$path, flagged_function$name, sep = ":")
    },
    character(1)
  )

  over_limit <- line_counts[line_counts > 50L]
  expect_equal(
    unname(over_limit),
    integer(),
    info = paste(
      paste(names(over_limit), over_limit, sep = " = "),
      collapse = "; "
    )
  )
})
