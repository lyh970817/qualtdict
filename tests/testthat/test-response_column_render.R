test_that("render_response_columns preserves row-aligned output shape", {
  rendered <- render_response_column_fixture(
    synthetic_mc_text_raw_metadata(),
    "QID1"
  )

  expect_named(
    rendered,
    c("response_column_id", "question", "item", "level", "label")
  )
  expect_renderer_rows_aligned(rendered)
  expect_snapshot(compact_response_column_render(rendered))
})

test_that("render_response_columns preserves loop-prefixed base IDs", {
  rendered <- render_response_column_fixture(
    synthetic_looped_mc_text_raw_metadata(),
    "QID2",
    base_response_column_id = "x1_QID2"
  )

  expect_identical(
    rendered$response_column_id,
    c("x1_QID2", "x1_QID2", "x1_QID2_2_TEXT")
  )
  expect_snapshot(compact_response_column_render(rendered))
})
