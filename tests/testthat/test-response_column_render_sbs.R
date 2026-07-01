test_that("SBS mixed columns preserve row metadata alignment", {
  rendered <- render_response_column_fixture(
    synthetic_sbs_text_subquestion_raw_metadata(),
    "QID2"
  )

  expect_renderer_rows_aligned(rendered)
  expect_snapshot(compact_response_column_render(rendered))
})

test_that("SBS carried-forward rows use subquestion response column IDs", {
  rendered <- render_response_column_fixture(
    synthetic_sbs_carried_forward_raw_metadata(),
    "QID2"
  )

  expect_identical(
    rendered$response_column_id,
    c("QID2_x1", "QID2_x2", "QID2_x3")
  )
  expect_snapshot(compact_response_column_render(rendered))
})
