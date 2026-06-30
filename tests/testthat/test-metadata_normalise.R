test_that("metadata normalisation summary stays stable", {
  raw_metadata <- synthetic_column_map_sidecar_raw_metadata()
  normalised_metadata <- normalise_qualtrics_metadata(raw_metadata)

  expect_snapshot_value(
    metadata_normalise_summary(normalised_metadata),
    style = "json2"
  )
})

test_that("metadata normalisation preserves representative dictionary rows", {
  raw_metadata <- synthetic_loop_and_merge_raw_metadata()
  normalised_metadata <- normalise_qualtrics_metadata(raw_metadata)

  dict <- variable_dictionary_from_normalised_metadata(
    normalised_metadata,
    use_semantic_name = FALSE,
    block_pattern = NULL,
    block_sep = ".",
    semantic_name_preprocess = NULL
  )

  expect_snapshot_value(metadata_dictionary_summary(dict), style = "json2")
})
