test_that("dict_generate", {
  vcr::use_cassette("dict_generate", {
    suppressWarnings(
      x <- dict_generate("SV_0AQg1pFepA0V2d0", name = "easy_name")
    )
  })

  expect_snapshot_value(x, style = "json2")
})
