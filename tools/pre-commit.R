#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
if (!length(args)) {
  stop("Expected a pre-commit command.", call. = FALSE)
}

command <- args[[1]]
files <- args[-1]

quit_with <- function(ok) {
  quit(save = "no", status = if (isTRUE(ok)) 0 else 1)
}

check_namespace <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop(
      "Package '", package, "' is required for this hook. ",
      "Enter nix-shell before running pre-commit.",
      call. = FALSE
    )
  }
}

parse_r_files <- function(files) {
  if (!length(files)) {
    quit_with(TRUE)
  }

  invisible(lapply(files, parse))
}

lint_package <- function() {
  check_namespace("lintr")
  lints <- lintr::lint_package()
  print(lints)
  quit_with(length(lints) == 0)
}

spell_check <- function() {
  check_namespace("spelling")
  findings <- spelling::spell_check_package()

  if (NROW(findings) > 0) {
    print(findings)
  }

  quit_with(NROW(findings) == 0)
}

deps_in_desc <- function() {
  package_fields <- c("Depends", "Imports", "Suggests")
  description <- read.dcf("DESCRIPTION", all = TRUE)[1, ]
  declared <- paste(description[intersect(package_fields, names(description))],
    collapse = ","
  )
  declared <- gsub("\\([^)]*\\)", "", declared)
  declared <- trimws(unlist(strsplit(declared, ",")))
  declared <- declared[nzchar(declared) & declared != "R"]

  code_files <- c(
    list.files("R", pattern = "\\.[Rr]$", recursive = TRUE, full.names = TRUE),
    list.files("tests", pattern = "\\.[Rr]$", recursive = TRUE, full.names = TRUE),
    list.files("vignettes", pattern = "\\.[Rr]md$", recursive = TRUE, full.names = TRUE)
  )
  code_files <- code_files[file.exists(code_files)]
  lines <- unlist(lapply(code_files, readLines, warn = FALSE), use.names = FALSE)
  matches <- regmatches(
    lines,
    gregexpr("\\b[A-Za-z][A-Za-z0-9.]*::", lines, perl = TRUE)
  )
  used <- sort(unique(sub("::$", "", unlist(matches, use.names = FALSE))))
  base_packages <- c(
    "base", "compiler", "datasets", "grDevices", "graphics", "grid",
    "methods", "parallel", "splines", "stats", "stats4", "tools", "utils"
  )
  missing <- setdiff(used, c(declared, base_packages))

  if (length(missing)) {
    message(
      "Packages used with :: but missing from DESCRIPTION: ",
      paste(missing, collapse = ", ")
    )
  }

  quit_with(!length(missing))
}

switch(command,
  roxygenize = {
    check_namespace("roxygen2")
    roxygen2::roxygenise()
  },
  tidy_description = {
    check_namespace("desc")
    desc::desc_normalize("DESCRIPTION")
  },
  spell_check = spell_check(),
  lintr = lint_package(),
  readme_rendered = {
    check_namespace("rmarkdown")
    rmarkdown::render(
      "README.Rmd",
      output_format = rmarkdown::github_document(html_preview = FALSE),
      output_file = "README.md",
      quiet = TRUE
    )
  },
  parsable_R = parse_r_files(files),
  deps_in_desc = deps_in_desc(),
  testthat = {
    check_namespace("devtools")
    devtools::test()
  },
  pkgcheck = {
    check_namespace("pkgcheck")
    checks <- pkgcheck::pkgcheck()
    print(checks)
    summary <- pkgcheck:::summarise_all_checks(checks)
    quit_with(isTRUE(attr(summary, "checks_okay")))
  },
  r_cmd_check = {
    check_namespace("devtools")
    devtools::check(args = "--no-manual")
  },
  stop("Unknown pre-commit command: ", command, call. = FALSE)
)
