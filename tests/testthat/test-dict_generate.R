test_that("dict_generate", {
  vcr::use_cassette("dict_generate", {
    suppressWarnings(
      x <- dict_generate("SV_0AQg1pFepA0V2d0", name = "easy_name")
    )
  })
  expect_s3_class(
    x,
    c("spec_tbl_df", "tbl_df", "tbl", "data.frame", "qualtdict")
  )
  expect_equal(
    unique(x$qid),
    c(
      "QID1", "QID1_3_TEXT", "QID2_TEXT", "QID3_1", "QID3_2", "QID3_3",
      "QID4_1", "QID4_2", "QID4_3", "QID6", "QID6_x3_TEXT", "QID7_1_1",
      "QID7_1_2", "QID7_1_3", "QID7_1_4", "QID7_1_5", "QID7_2_1",
      "QID7_2_2", "QID7_2_3", "QID7_2_4", "QID7_2_5", "QID7_3_1",
      "QID7_3_2", "QID7_3_3", "QID7_3_4", "QID7_3_5", "QID8_1", "QID8_2",
      "QID8_3", "QID9_1_1", "QID9_1_2", "QID9_1_3", "QID9_2_1",
      "QID9_2_2", "QID9_2_3", "QID9_3_1", "QID9_3_2", "QID9_3_3",
      "QID10_1_1", "QID10_1_2", "QID10_1_3", "QID10_2_1", "QID10_2_2",
      "QID10_2_3", "QID10_3_1", "QID10_3_2", "QID10_3_3", "QID11_1_1",
      "QID11_1_2", "QID11_1_3", "QID11_2_1", "QID11_2_2", "QID11_2_3",
      "QID11_3_1", "QID11_3_2", "QID11_3_3", "QID12_1", "QID12_2",
      "QID12_3", "QID13_1", "QID13_2", "QID13_3", "QID14_1", "QID14_2",
      "QID14_3", "QID15_1", "QID15_2", "QID15_3", "QID16#1_1_1",
      "QID16#1_1_2", "QID16#1_2_1", "QID16#1_2_2", "QID16#1_3_1",
      "QID16#1_3_2", "QID16#2_1", "QID16#2_2", "QID16#2_3", "QID17",
      "QID18_FIRST_CLICK", "QID18_LAST_CLICK", "QID18_PAGE_SUBMIT",
      "QID18_CLICK_COUNT", "QID19", "QID20_1", "QID20_2", "QID20_3",
      "QID21_1", "QID21_2", "QID21_3", "QID22_1", "QID22_2", "QID22_3",
      "QID23_FILE_ID", "QID23_FILE_NAME", "QID23_FILE_SIZE",
      "QID23_FILE_TYPE", "QID24", "QID25_1", "QID25_2", "QID25_3",
      "QID26_FILE_ID", "QID26_FILE_NAME", "QID26_FILE_SIZE",
      "QID26_FILE_TYPE", "QID28_1_4", "QID28_1_5", "QID28_1_6",
      "QID28_1_7", "QID28_1_8", "QID28_1_9", "QID28_1_10", "QID28_1_11",
      "QID28_1_12", "QID28_1_13", "QID28_1_14", "QID28_1_15",
      "QID28_1_16", "QID28_1_17", "QID28_1_18", "QID28_1_19",
      "QID28_1_20", "QID28_1_21", "QID28_1_22", "QID28_1_23",
      "QID28_1_24", "QID28_1_25", "QID28_1_26", "QID28_1_27",
      "QID28_1_28", "QID28_1_29", "QID28_1_30", "QID28_1_31",
      "QID28_1_32", "QID28_1_33", "QID28_1_34", "QID28_1_35",
      "QID28_1_36", "QID28_1_37", "QID28_1_38", "QID28_1_39",
      "QID28_1_40", "QID28_1_41", "QID28_1_42", "QID28_1_43",
      "QID28_1_44", "QID28_1_45", "QID28_2_4", "QID28_2_5", "QID28_2_6",
      "QID28_2_7", "QID28_2_8", "QID28_2_9", "QID28_2_10", "QID28_2_11",
      "QID28_2_12", "QID28_2_13", "QID28_2_14", "QID28_2_15",
      "QID28_2_16", "QID28_2_17", "QID28_2_18", "QID28_2_19",
      "QID28_2_20", "QID28_2_21", "QID28_2_22", "QID28_2_23",
      "QID28_2_24", "QID28_2_25", "QID28_2_26", "QID28_2_27",
      "QID28_2_28", "QID28_2_29", "QID28_2_30", "QID28_2_31",
      "QID28_2_32", "QID28_2_33", "QID28_2_34", "QID28_2_35",
      "QID28_2_36", "QID28_2_37", "QID28_2_38", "QID28_2_39",
      "QID28_2_40", "QID28_2_41", "QID28_2_42", "QID28_2_43",
      "QID28_2_44", "QID28_2_45", "QID28_3_4", "QID28_3_5", "QID28_3_6",
      "QID28_3_7", "QID28_3_8", "QID28_3_9", "QID28_3_10", "QID28_3_11",
      "QID28_3_12", "QID28_3_13", "QID28_3_14", "QID28_3_15",
      "QID28_3_16", "QID28_3_17", "QID28_3_18", "QID28_3_19",
      "QID28_3_20", "QID28_3_21", "QID28_3_22", "QID28_3_23",
      "QID28_3_24", "QID28_3_25", "QID28_3_26", "QID28_3_27",
      "QID28_3_28", "QID28_3_29", "QID28_3_30", "QID28_3_31",
      "QID28_3_32", "QID28_3_33", "QID28_3_34", "QID28_3_35",
      "QID28_3_36", "QID28_3_37", "QID28_3_38", "QID28_3_39",
      "QID28_3_40", "QID28_3_41", "QID28_3_42", "QID28_3_43",
      "QID28_3_44", "QID28_3_45", "QID29", "x1_QID31", "x1_QID31_1_TEXT",
      "x2_QID31", "x2_QID31_1_TEXT", "x3_QID31", "x3_QID31_1_TEXT",
      "QID32", "QID33_1", "QID33_2", "QID33_3", "QID34_1", "QID34_2",
      "QID34_3", "QID35_1", "QID35_2", "QID35_3", "QID36_1", "QID36_2",
      "QID36_3", "QID37", "QID38_1", "QID38_2", "QID38_3", "QID39",
      "QID40_1", "QID40_2", "QID40_3", "QID41", "QID42", "QID43"
    )
  )
})
