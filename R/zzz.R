# On unload
.onUnload <-
  function(libname = find.package("qualtdict"), pkgname = "qualtdict") {
    # If user unloads/detaches package make sure that these values are erased
    Sys.setenv("QUALTRICS_BASE_URL" = "")
    Sys.setenv("QUALTRICS_API_KEY" = "")
  }
