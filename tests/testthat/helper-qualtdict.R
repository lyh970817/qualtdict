library(vcr) # *Required* as vcr is set up on loading
if (!nzchar(Sys.getenv("APIKEY"))) {
  Sys.setenv("APIKEY" = "foobar")
}

invisible(vcr::vcr_configure(
  filter_sensitive_data = list(
    "QUALTRICS_API_KEY" = Sys.getenv("QUALTRICS_API_KEY"),
    "QUALTRICS_BASE_URL" = Sys.getenv("QUALTRICS_BASE_URL")
  ),
  dir = "../fixtures"
))
