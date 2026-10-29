#!/usr/bin/env Rscript

# Manual release/resubmission gate. See tools/mutator.md.
# Source this file to exercise the runner on a synthetic package locally.
run_mutator <- function(
  pkg_dir,
  output_dir,
  cores = 12L,
  scope = c("dict_validate.R", "dict_generate.R", "labelled_export.R")
) {
  if (!requireNamespace("mutator", quietly = TRUE)) {
    stop("Install mutator as described in tools/mutator.md.", call. = FALSE)
  }
  tool <- utils::packageDescription("mutator")
  if (is.null(tool$RemoteSha) || !nzchar(tool$RemoteSha)) {
    stop("Install mutator from a Git commit; see tools/mutator.md.")
  }
  pkg_dir <- normalizePath(pkg_dir, mustWork = TRUE)
  stopifnot(
    length(cores) == 1L,
    is.finite(cores),
    cores >= 1L,
    cores == as.integer(cores),
    length(scope) > 0L,
    all(file.exists(file.path(pkg_dir, "R", scope)))
  )
  if (file.exists(output_dir)) {
    stop("Use a new output directory for each run.", call. = FALSE)
  }
  stopifnot(dir.create(output_dir, recursive = TRUE))
  output_dir <- normalizePath(output_dir, mustWork = TRUE)
  review_path <- file.path(output_dir, "review.md")
  writeLines(
    c(
      "# Mutation release gate review",
      "",
      "Run status: INCOMPLETE",
      "Gate status: PENDING HUMAN REVIEW",
      "",
      "Do not mark the gate passed until the run and review are complete."
    ),
    review_path
  )

  log <- file(file.path(output_dir, "run.log"), open = "wt")
  sink(log, split = TRUE)
  sink(log, type = "message")
  on.exit(
    {
      sink(type = "message")
      sink()
      close(log)
    },
    add = TRUE
  )

  git <- function(...) {
    system2("git", c("-C", shQuote(pkg_dir), ...), stdout = TRUE)
  }
  head <- git("rev-parse", "HEAD")
  stopifnot(is.null(attr(head, "status")))
  writeLines(git("status", "--short"), file.path(output_dir, "git-status.txt"))
  writeLines(
    git("diff", "--no-ext-diff", "--no-color", "HEAD"),
    file.path(output_dir, "checkout.diff")
  )
  writeLines(capture.output(print(tool)), file.path(output_dir, "mutator.txt"))
  writeLines(
    capture.output(sessionInfo()),
    file.path(output_dir, "session.txt")
  )

  # Preserve the code/tests actually used, including any uncommitted changes.
  source_dir <- file.path(output_dir, "source")
  dir.create(source_dir)
  stopifnot(all(file.copy(
    file.path(pkg_dir, c("R", "tests", "DESCRIPTION", "NAMESPACE")),
    source_dir,
    recursive = TRUE
  )))
  inputs <- c(
    "DESCRIPTION",
    "NAMESPACE",
    unlist(lapply(
      c("R", "tests", "data", "inst"),
      function(dir) {
        file.path(
          dir,
          list.files(
            file.path(pkg_dir, dir),
            recursive = TRUE,
            all.files = TRUE
          )
        )
      }
    ))
  )
  hashes <- tools::md5sum(file.path(pkg_dir, inputs))
  names(hashes) <- inputs
  write.csv(
    data.frame(file = inputs, md5 = unname(hashes)),
    file.path(output_dir, "inputs.csv"),
    row.names = FALSE
  )

  settings <- list(
    pkg_dir = pkg_dir,
    cores = as.integer(cores),
    mutation_dir = file.path(output_dir, "mutants"),
    exclude_files = setdiff(
      list.files(file.path(pkg_dir, "R"), "\\.R$"),
      scope
    ),
    max_mutants = NULL,
    coverage_guided = FALSE,
    timeout_seconds = 180,
    max_show = Inf,
    operators = "default",
    strategy = "testthat",
    cran = TRUE,
    fail_fast = TRUE,
    isolate = FALSE,
    detectEqMutants = FALSE,
    max_line_deletions = 0,
    isFullLog = TRUE
  )
  started <- Sys.time()
  metadata <- list(
    head = head,
    scope = scope,
    seed = 20261025L,
    mutator_version = tool$Version,
    mutator_commit = tool$RemoteSha,
    started = started,
    settings = settings
  )
  saveRDS(metadata, file.path(output_dir, "metadata.rds"))
  writeLines(
    capture.output(dput(metadata)),
    file.path(output_dir, "metadata.txt")
  )
  # Subprocesses find this session's optional local library through R_LIBS.
  old_libs <- Sys.getenv("R_LIBS", unset = NA_character_)
  on.exit(
    {
      if (is.na(old_libs)) {
        Sys.unsetenv("R_LIBS")
      } else {
        Sys.setenv(R_LIBS = old_libs)
      }
    },
    add = TRUE
  )
  Sys.setenv(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep))
  set.seed(metadata$seed)
  result <- do.call(mutator::mutate_package, settings)
  saveRDS(result, file.path(output_dir, "result.rds"))
  writeLines(
    capture.output(str(result[c("summary", "timing")])),
    file.path(output_dir, "summary.txt")
  )
  stopifnot(result$summary$generated > 0L)
  stopifnot(result$summary$tested == result$summary$generated)

  dir.create(file.path(output_dir, "diffs"))
  records <- lapply(names(result$package_mutants), function(id) {
    mutant <- result$package_mutants[[id]]
    original <- file.path(source_dir, "R", basename(mutant$src))
    diff_path <- file.path("diffs", paste0(id, ".diff"))
    status <- system2(
      "git",
      c(
        "diff",
        "--no-index",
        "--no-ext-diff",
        "--no-color",
        "--",
        shQuote(original),
        shQuote(mutant$mutant_file)
      ),
      stdout = file.path(output_dir, diff_path)
    )
    stopifnot(status %in% c(0L, 1L))
    data.frame(
      id = id,
      file = file.path("R", basename(mutant$src)),
      start_line = mutant$mutation_loc$start_line,
      end_line = mutant$mutation_loc$end_line,
      status = mutant$status,
      mutation = mutant$mutation_info,
      diff = diff_path
    )
  })
  records <- do.call(rbind, records)
  write.csv(records, file.path(output_dir, "mutants.csv"), row.names = FALSE)
  stopifnot(all(scope %in% basename(records$file)))
  current_hashes <- tools::md5sum(file.path(pkg_dir, inputs))
  stopifnot(identical(unname(hashes), unname(current_hashes)))

  review <- c(
    "# Mutation release gate review",
    "",
    "Run status: COMPLETE",
    "Gate status: PENDING HUMAN REVIEW",
    "",
    paste("Checkout commit:", head),
    paste("mutator version:", tool$Version),
    paste("mutator source commit:", tool$RemoteSha),
    paste("Scope:", paste(file.path("R", scope), collapse = ", ")),
    paste("Started:", format(started, tz = "UTC", usetz = TRUE)),
    paste("Finished:", format(Sys.time(), tz = "UTC", usetz = TRUE)),
    "",
    paste("Generated:", result$summary$generated),
    paste("Tested:", result$summary$tested),
    paste("Killed:", result$summary$killed),
    paste("Survived:", result$summary$survived),
    paste("Timed out (HANG):", result$summary$hanged),
    "",
    "Settings, seed and tool provenance: metadata.txt and mutator.txt.",
    "Checkout changes: git-status.txt and checkout.diff.",
    "Results: result.rds, summary.txt, mutants.csv, run.log and diffs/.",
    "Original code/tests: source/; input checksums: inputs.csv.",
    "",
    "## Human decision",
    "",
    "Reviewer: [pending]",
    "Review date: [pending]",
    "Final source/test commit: [pending]",
    "Follow-up run and review evidence after fixes: [pending or not needed]",
    "Decision: [pending; passed only under tools/mutator.md]",
    "",
    "Review every SURVIVED entry below and resolve every HANG.",
    "For a real gap, record the behavioural assertion/fix and follow-up run.",
    "For an accepted equivalent or intentional mutant, explain why that",
    "specific source change preserves the contract or changes only",
    "deliberately unasserted behaviour. A category alone is not a rationale.",
    "Inspect suspicious KILLED results in run.log for infrastructure failures.",
    "A successful script exit does not pass this manual gate."
  )
  pending <- records[records$status %in% c("SURVIVED", "HANG"), ]
  for (i in seq_len(nrow(pending))) {
    mutant <- pending[i, ]
    review <- c(
      review,
      "",
      paste("##", mutant$id),
      "",
      paste("Status:", mutant$status),
      paste0(
        "Source: ",
        mutant$file,
        ":",
        mutant$start_line,
        "-",
        mutant$end_line
      ),
      paste0("Diff: [inspect mutation](", mutant$diff, ")"),
      "",
      "Disposition: [pending]",
      "Specific rationale or consequential gap: [pending]",
      "Fix/test commit and follow-up evidence: [pending or not needed]"
    )
  }
  writeLines(review, review_path)
  cat("\nMutation run complete. Human review pending:\n", review_path, "\n")
  invisible(result)
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) > 2L || "--help" %in% args) {
    cat("Usage: Rscript tools/mutator.R [NEW_OUTPUT_DIRECTORY] [CORES]\n")
    quit(status = if ("--help" %in% args) 0L else 1L)
  }
  stopifnot(identical(
    unname(read.dcf("DESCRIPTION", "Package")[1, 1]),
    "qualtdict"
  ))
  output_dir <- if (length(args) >= 1L) {
    args[1]
  } else {
    file.path(
      ".local",
      "mutator",
      format(Sys.time(), "%Y%m%d-%H%M%S")
    )
  }
  cores <- if (length(args) == 2L) as.numeric(args[2]) else 12L
  run_mutator(".", output_dir, cores = cores)
}
