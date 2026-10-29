# Manual mutator release gate

Before each release or review resubmission, run `tools/mutator.R` and complete
its human review. This is a local maintainer gate alongside autotest, outside
the ordinary test suite and CI. It needs no Qualtrics credentials. mutator is
an optional development tool, not an Imports or Suggests dependency.

## Install and run

Use the package's development environment, with its ordinary test dependencies
available. Install mutator in a local library; the runner requires a Git source
commit recorded as `RemoteSha`. The version used to develop this runner was
0.2.2.9000 at the following commit:

```r
remotes::install_github(
  "PRL-PRG/mutator@e55f95fdd81a17d039e6880ba03562126b1bb78b",
  upgrade = "never"
)
```

From the qualtdict root, run:

```sh
Rscript tools/mutator.R
```

The default evidence directory is `.local/mutator/YYYYMMDD-HHMMSS/`. To choose
a new directory and worker count:

```sh
Rscript tools/mutator.R .local/mutator/release-review 12
```

The directory must not already exist: each run retains its own evidence and
review. Keep outputs under ignored `.local/`, or outside the repository.
`.local/` and `tools/` are excluded from the package build. Do not commit run
artifacts. Keep the checkout and test inputs unchanged while the run executes.

## Scope and settings

The runner mutates the current checkout of `R/dict_validate.R`,
`R/dict_generate.R` and `R/labelled_export.R`, using all generated mutants in
those files. This is the audited core scope, not a package-wide mutation claim.
Other source files are excluded from mutation generation; their functions can
still execute in the tests. There is no sampling mode in the release command.

Settings are fixed to seed 20261025, the default mutation operators, no extra
line-deletion mutants, full-suite execution for each mutant
(`coverage_guided = FALSE`), the testthat strategy, and a 180-second timeout per
mutant. The default is 12 workers. Tests use mutator's CRAN mode (`cran = TRUE`)
and stop after the first failure (`fail_fast = TRUE`). Automated equivalence
classification is disabled; every survivor needs human judgment. Test trees
use mutator's normal snapshot isolation (`isolate = FALSE`); unchanged test
files must not be written by tests. Review the saved settings when comparing
runs. A full run can take tens of minutes, depending on the checkout and machine.

The upstream [configuration guide](https://prl-prg.github.io/mutator/articles/configuration.html)
and `?mutator::mutate_package` describe these settings. If the scope or
operators need changing for a future release, change and review the runner
explicitly, and record the resulting scope in the release review.

## Evidence and review

A successful exit means the full mutation run and evidence generation finished.
It leaves `review.md` marked **PENDING HUMAN REVIEW**. It never signs off the
manual gate. If the baseline, mutation run or evidence generation fails, the
script exits nonzero and leaves the review marked **INCOMPLETE**; inspect
`run.log`, fix the cause and rerun into a new directory.

Each completed run saves:

- `metadata.txt` and `metadata.rds`: checkout commit, mutator version and source
  commit, scope, seed and settings; `mutator.txt` and `session.txt`: installed
  tool metadata and R environment.
- `git-status.txt` and `checkout.diff`: local changes relative to HEAD;
  `inputs.csv`: checksums for R code, tests, bundled data, installed resources,
  DESCRIPTION and NAMESPACE; `source/`: original R code, tests and package
  metadata used for review.
- `result.rds`, `summary.txt`, `mutants.csv` and `run.log`: raw results, counts,
  timing, mutation IDs, source locations and execution diagnostics.
- `mutants/` and `diffs/`: generated mutated source files and a diff for every
  result. Temporary mutant package paths in the raw RDS can expire; these saved
  source files and diffs are the durable evidence.
- `review.md`: a decision template with an entry for every survivor and timeout.
  IDs are local to that run and tool version, not a permanent baseline.

To pass the manual gate, a maintainer must:

1. Confirm the baseline and full scoped run completed, inspect diagnostics for
   infrastructure failures, and resolve timeouts. A `HANG` is unresolved
   evidence, not an assertion that killed a mutant. mutator can also classify
   load or execution errors as `KILLED`, so suspicious kills need investigation.
2. Review each survivor's saved diff against the intended contract. Fix
   consequential missing assertions or package defects. For an accepted
   equivalent or intentional mutant, record a specific explanation of why
   that change preserves the contract or affects only deliberately unasserted
   behaviour. “Equivalent” or “message wording” alone is not sufficient.
3. After code or test fixes, rerun the complete gate on the intended final
   checkout and review its results. Link earlier findings and fixes from the
   final review; do not carry IDs or old counts across runs as a baseline.
4. Fill in the reviewer, review date, final source/test commit and follow-up
   evidence in `review.md`. Change its gate status and decision to **PASSED**
   only when no consequential gaps, unexplained survivors or timeouts remain.
   Retain that completed evidence with the local release records.

There is no zero-survivor rule and no global mutation-score threshold.
Survivors can be valid when their specific rationale is recorded and reviewed.
A numeric score or an unedited report is not evidence that this gate passed.

## Checking changes to the runner

This file defines `run_mutator()` and executes the release command only when
run directly with Rscript. For a bounded local runner check, source it and call
`run_mutator(pkg_dir, output_dir, cores = 1L, scope = "example.R")` against a tiny
temporary Git package with synthetic testthat tests. This exercises the real
installed mutator and evidence generation without adding mutation runs to the
ordinary tests. Such a check is not a qualtdict release-gate run or sign-off.
