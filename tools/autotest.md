# autotest Release Gate

`tools/autotest.R` runs rOpenSci's
[autotest](https://docs.ropensci.org/autotest/) on qualtdict and fails when
it reports a diagnostic that `tools/autotest-exclusions.yaml` does not
explain. It needs no Qualtrics credentials and is not part of CI.

## What it checks

autotest traces the calls that the package's examples make to exported
functions, then calls each function again with mutated inputs: rectangular
inputs converted to other classes, logical parameters negated, vectors where
single values are expected, and so on. It reports errors, warnings and
messages from those calls, return values that do not match their
documentation, parameter documentation that does not name the class of the
values passed in examples, and parameters that no example demonstrates.

## When to run it

Run it before a release and before resubmitting the package for review,
from the package root:

```sh
Rscript tools/autotest.R
```

Install autotest first if needed:

```r
install.packages(
  "autotest",
  repos = c("https://ropensci.r-universe.dev", "https://cloud.r-project.org")
)
```

The script installs qualtdict into a temporary library for the run, prints
the diagnostics that remain after exclusions, and exits with status 1 if any
remain, 0 otherwise.

## Adding an exclusion

autotest reads no exclusion file of its own, and its `test_data` argument
cannot switch a test off for a single function, so exclusions live in
`tools/autotest-exclusions.yaml` and the script drops the matching
diagnostics after the run. Exclude a diagnostic only when it reports
deliberate behaviour, such as a class check that rejects plain data frames.
Fix everything else in the code or documentation.

Each entry lists the `functions` it applies to and narrows the match with
any of `test_name`, `parameter` and `content` (fixed substrings of the
diagnostic's content). It needs a `test_name` or `content`, and a `note`
saying why; the script stops on an entry without a note. Add a comment above
the entry with the longer reasoning, for example:

```yaml
  - test_name: rect_as_other
    functions:
      - dict_validate
    parameter: dict
    note: >-
      `dict` must be a qualtdict object; the class check is deliberate.
```
