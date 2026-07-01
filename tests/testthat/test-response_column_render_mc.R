test_that("MC text-entry choices render current response column IDs", {
  rendered <- render_response_column_fixture(
    synthetic_mc_text_raw_metadata(),
    "QID1"
  )

  expect_identical(
    rendered$response_column_id,
    c("QID1", "QID1", "QID1", "QID1_3_TEXT")
  )
  expect_snapshot(compact_response_column_render(rendered))
})

test_that("MC independent columns preserve recode suffix behavior", {
  rendered <- render_response_column_fixture(
    synthetic_mc_x_choice_raw_metadata(),
    "QID126879611"
  )

  expect_identical(
    rendered$response_column_id,
    paste0("QID126879611_", c("1", "2", "3", "4", "6"))
  )
  expect_snapshot(compact_response_column_render(rendered))
})
