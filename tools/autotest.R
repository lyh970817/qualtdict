#!/usr/bin/env Rscript

# Release gate: run rOpenSci's autotest on qualtdict and fail on any
# diagnostic that tools/autotest-exclusions.yaml does not explain.
#
# Run from the package root:
#
#   Rscript tools/autotest.R
#
# See tools/autotest.md.

if (!requireNamespace("autotest", quietly = TRUE)) {
  stop(
    paste0(
      "autotest is not installed. Install it with:\n",
      "  install.packages(\"autotest\", repos = c(",
      "\"https://ropensci.r-universe.dev\", ",
      "\"https://cloud.r-project.org\"))"
    ),
    call. = FALSE
  )
}
if (!requireNamespace("yaml", quietly = TRUE)) {
  stop("yaml is needed to read tools/autotest-exclusions.yaml.", call. = FALSE)
}

pkg_dir <- normalizePath(".")
desc_path <- file.path(pkg_dir, "DESCRIPTION")
if (
  !file.exists(desc_path) ||
    !identical(unname(read.dcf(desc_path, "Package")[1, 1]), "qualtdict")
) {
  stop("Run tools/autotest.R from the qualtdict package root.", call. = FALSE)
}

# autotest (through typetracer) installs the package into the first library
# on .libPaths() and traces examples and tests in a subprocess that finds its
# libraries through R_LIBS. A throwaway library keeps that install out of
# every library you use.
tmp_lib <- tempfile("qualtdict-autotest-lib-")
dir.create(tmp_lib)
.libPaths(c(tmp_lib, .libPaths()))
Sys.setenv(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep))

read_exclusions <- function(path) {
  entries <- yaml::read_yaml(path)$exclusions
  for (entry in entries) {
    if (
      length(entry$functions) == 0 ||
        (is.null(entry$test_name) && is.null(entry$content))
    ) {
      stop(
        "Every exclusion in ",
        path,
        " needs `functions` and a ",
        "`test_name` or `content`.",
        call. = FALSE
      )
    }
    if (is.null(entry$note) || !nzchar(trimws(entry$note))) {
      stop(
        "An exclusion for ",
        toString(unlist(entry$functions)),
        " in ",
        path,
        " has no `note` saying why.",
        call. = FALSE
      )
    }
  }
  entries
}

is_excluded <- function(res, entries) {
  excluded <- rep(FALSE, nrow(res))
  for (entry in entries) {
    match <- res$fn_name %in% unlist(entry$functions)
    if (!is.null(entry$test_name)) {
      match <- match & res$test_name %in% entry$test_name
    }
    if (!is.null(entry$parameter)) {
      match <- match & res$parameter %in% entry$parameter
    }
    if (!is.null(entry$content)) {
      content_match <- rep(FALSE, nrow(res))
      for (text in unlist(entry$content)) {
        content_match <- content_match |
          grepl(text, res$content, fixed = TRUE)
      }
      match <- match & content_match
    }
    excluded <- excluded | match
  }
  excluded
}

exclusions <- read_exclusions(
  file.path(pkg_dir, "tools", "autotest-exclusions.yaml")
)

res <- autotest::autotest_package(
  package = pkg_dir,
  test = TRUE,
  progress = "none"
)
res <- as.data.frame(res)
if (nrow(res) == 0) {
  res <- data.frame(
    type = character(),
    test_name = character(),
    fn_name = character(),
    parameter = character(),
    content = character()
  )
}

excluded <- is_excluded(res, exclusions)
remaining <- res[!excluded, , drop = FALSE]

options(width = 200)
cols <- c("type", "test_name", "fn_name", "parameter", "content")
cat(
  "autotest: ",
  nrow(res),
  " diagnostic(s), ",
  sum(excluded),
  " excluded by tools/autotest-exclusions.yaml, ",
  nrow(remaining),
  " remaining.\n",
  sep = ""
)
if (nrow(remaining) > 0) {
  shown <- remaining[cols]
  shown$content <- substr(gsub("\\s+", " ", shown$content), 1, 100)
  print(shown, right = FALSE, row.names = FALSE)
}
unlink(tmp_lib, recursive = TRUE)
if (nrow(remaining) > 0) {
  quit(status = 1)
}
cat("autotest release gate passed.\n")
