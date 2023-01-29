library(vcr) # *Required* as vcr is set up on loading
if (!nzchar(Sys.getenv("QUALTRICS_API_KEY"))) {
  Sys.setenv("QUALTRICS_API_KEY" = "foobar")
  Sys.setenv("QUALTRICS_BASE_URL" = "www.qualtrics.com")
}

invisible(vcr::vcr_configure(
  filter_sensitive_data = list(
    "QUALTRICS_API_KEY" = Sys.getenv("QUALTRICS_API_KEY"),
    "QUALTRICS_BASE_URL" = Sys.getenv("QUALTRICS_BASE_URL")
  ),
  dir = "../fixtures"
))
