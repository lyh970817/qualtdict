# `glad_example` is loaded through utils::data() so the tests work whether or
# not the package uses LazyData.
glad_example_data <- function() {
  env <- new.env(parent = emptyenv())
  utils::data("glad_example", package = "qualtdict", envir = env)
  env$glad_example
}

glad_example_survey_id <- "SV_glad0example00000"

glad_example_raw_metadata <- function(example) {
  new_raw_qualtrics_metadata(
    surveyID = example$survey_metadata$metadata$surveyID,
    metadata = example$survey_metadata,
    description = example$survey_description,
    response_column_map = example$response_column_map
  )
}

# Every string in an object: values, names, and attributes, recursively.
glad_example_strings <- function(x) {
  values <- if (is.list(x)) {
    unlist(lapply(unclass(x), glad_example_strings), use.names = FALSE)
  } else if (is.character(x) || is.factor(x)) {
    as.character(x)
  }
  attrs <- attributes(x)
  attrs <- attrs[setdiff(names(attrs), "row.names")]
  c(
    values,
    unlist(lapply(attrs, glad_example_strings), use.names = FALSE)
  )
}

# Embedded Data `Value` entries in a description Survey Flow.
glad_example_flow_values <- function(flow) {
  if (!is.list(flow)) {
    return(character())
  }
  values <- if (identical(flow$Type, "EmbeddedData")) {
    unlist(lapply(flow$EmbeddedData, `[[`, "Value"))
  }
  c(values, unlist(lapply(unname(flow), glad_example_flow_values)))
}

test_that("glad_example has the documented components", {
  example <- glad_example_data()

  expect_type(example, "list")
  expect_named(
    example,
    c(
      "survey_metadata",
      "survey_description",
      "response_column_map",
      "responses",
      "dictionary",
      "labelled_survey_data"
    )
  )
  expect_named(
    example$survey_metadata,
    c(
      "metadata",
      "questions",
      "responsecounts",
      "blocks",
      "flow",
      "embedded_data"
    )
  )
  expect_named(
    example$survey_description,
    c("metadata", "flow", "blocks", "questions", "scoring")
  )
  expect_s3_class(example$response_column_map, "data.frame")
  expect_s3_class(example$responses, "tbl_df")
  expect_identical(dim(example$responses), c(20L, 48L))
  expect_named(example$responses, example$response_column_map$qname)
  expect_identical(
    attr(example$responses, "column_map", exact = TRUE),
    example$response_column_map
  )
  expect_s3_class(example$dictionary, "qualtdict")
  expect_identical(nrow(example$dictionary), 97L)
  expect_length(unique(example$dictionary$response_column_id), 37L)
  expect_setequal(
    example$dictionary$row_source,
    c("question", "embedded_data", "scoring")
  )
  expect_setequal(
    example$dictionary$block[!is.na(example$dictionary$block)],
    c("Demographics", "PHQ9", "GAD7")
  )
  expect_identical(
    attr(example$dictionary, "surveyID", exact = TRUE),
    glad_example_survey_id
  )
  expect_s3_class(example$labelled_survey_data, "tbl_df")
  expect_identical(nrow(example$labelled_survey_data), 20L)
  expect_s3_class(
    attr(example$labelled_survey_data, "dict", exact = TRUE),
    "qualtdict"
  )
})

test_that("glad_example carries no Qualtrics identifiers or addresses", {
  example <- glad_example_data()
  strings <- glad_example_strings(example)

  expect_identical(
    unique(grep("SV_", strings, fixed = TRUE, value = TRUE)),
    glad_example_survey_id
  )
  for (pattern in c(
    "UR_",
    "BR_",
    "@",
    "http",
    "qualtrics.com",
    "kcl.ac.uk",
    "kcliop",
    "${",
    "$e{"
  )) {
    expect_identical(
      grep(pattern, strings, fixed = TRUE, value = TRUE),
      character(),
      label = paste("strings containing", pattern)
    )
  }

  survey_metadata <- example$survey_metadata
  expect_identical(survey_metadata$metadata$surveyID, glad_example_survey_id)
  expect_identical(survey_metadata$metadata$name, "GLAD Signup Example")
  expect_identical(
    intersect(names(survey_metadata$metadata), c("ownerId", "organizationId")),
    character()
  )
  expect_identical(
    unlist(survey_metadata$responsecounts),
    c(auditable = 0L, generated = 0L, deleted = 0L)
  )
  expect_identical(
    unique(unlist(lapply(survey_metadata$embedded_data, `[[`, "defaultValue"))),
    ""
  )

  survey_description <- example$survey_description
  expect_identical(
    intersect(
      survey_description$metadata$name,
      c("BrandID", "BrandBaseURL", "OwnerID", "CreatorID")
    ),
    character()
  )
  expect_identical(
    unique(survey_description$metadata$value[
      survey_description$metadata$name == "SurveyID"
    ]),
    glad_example_survey_id
  )
  expect_identical(
    unique(glad_example_flow_values(survey_description$flow)),
    ""
  )
  expect_false(
    "QuestionJS" %in% unlist(lapply(survey_description$questions, names))
  )
})

test_that("glad_example carries none of the real survey IDs", {
  survey_list <- test_path(
    "..",
    "..",
    "tools",
    "local-finalize-smoke-surveys-all.json"
  )
  skip_if_not(file.exists(survey_list), "Survey list is not available.")
  lines <- readLines(survey_list, warn = FALSE)
  real_ids <- unlist(regmatches(lines, gregexpr("SV_[A-Za-z0-9]+", lines)))

  expect_gt(length(real_ids), 0)
  expect_identical(
    intersect(real_ids, glad_example_strings(glad_example_data())),
    character()
  )
})

test_that("glad_example responses stay within the dictionary levels", {
  example <- glad_example_data()
  dict <- example$dictionary
  coded <- dict$row_source == "question" &
    !is.na(dict$level) &
    !grepl("_TEXT$", dict$response_column_id)

  for (response_column_id in unique(dict$response_column_id[coded])) {
    levels <- as.numeric(dict$level[
      coded & dict$response_column_id == response_column_id
    ])
    values <- example$responses[[response_column_id]]
    expect_in(values[!is.na(values)], levels)
  }
})

test_that("glad_example dictionary regenerates from the stored metadata", {
  example <- glad_example_data()
  raw_metadata <- glad_example_raw_metadata(example)
  local_mocked_bindings(
    fetch_dictionary_metadata = function(surveyID) {
      expect_identical(surveyID, glad_example_survey_id)
      raw_metadata
    }
  )

  expect_no_warning(
    dict <- dict_generate(
      glad_example_survey_id,
      variable_name = "question_name"
    )
  )
  expect_identical(dict, example$dictionary)
})

test_that("glad_example Labelled Survey Data regenerates from responses", {
  example <- glad_example_data()
  local_mocked_bindings(
    fetch_survey2 = function(...) {
      expect_identical(list(...)$surveyID, glad_example_survey_id)
      example$responses
    }
  )

  expect_no_warning(
    dat <- survey_fetch_labelled(example$dictionary)
  )
  expect_identical(dat, example$labelled_survey_data)
})

test_that("glad_example dictionary has no Export-blocking findings", {
  example <- glad_example_data()
  findings <- dict_validate(example$dictionary)$validation_findings

  expect_gt(nrow(findings), 0)
  expect_identical(unique(findings$severity), "suggestive")
  expect_no_error(dict_assert_valid(example$dictionary))
})

test_that("glad_example.rda is small and xz-compressed", {
  path <- test_path("..", "..", "data", "glad_example.rda")
  skip_if_not(file.exists(path), "Source data directory is not available.")

  expect_lt(file.size(path), 200 * 1024)
  expect_identical(as.character(tools::checkRdaFiles(path)$compress), "xz")
})
