vcr::use_cassette("dict_generate", {
  suppressWarnings(
    x <- dict_generate("SV_0AQg1pFepA0V2d0", name = "question_name")
  )
})

test_that("dict_validate", {
  x_validate <- dict_validate(x)
  expect_snapshot_value(x_validate, style = "json2")
})
