vcr::use_cassette("dict_generate", {
  suppressWarnings(
    x <- dict_generate("SV_0AQg1pFepA0V2d0", name = "question_name")
  )
})

test_that("get_survey_data", {
  vcr::use_cassette("get_survey_data", {
    expect_warning(x_dat <- get_survey_data(x), regexp = NA)
  })
  expect_s3_class(x_dat, c("spec_tbl_df", "tbl_df", "tbl", "data.frame"))
})
