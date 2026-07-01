test_that("matrix item-level rows render in stable order", {
  rendered <- render_response_column_fixture(
    synthetic_matrix_raw_metadata(),
    "QID1"
  )

  expect_identical(
    rendered$response_column_id,
    c("QID1_x1", "QID1_x1", "QID1_x2", "QID1_x2")
  )
  expect_snapshot(compact_response_column_render(rendered))
})

test_that("slider items render as one response column per item", {
  rendered <- render_response_column_fixture(
    synthetic_slider_raw_metadata(),
    "QID1"
  )

  expect_identical(
    rendered$response_column_id,
    c("QID1_1", "QID1_2", "QID1_3")
  )
  expect_snapshot(compact_response_column_render(rendered))
})
