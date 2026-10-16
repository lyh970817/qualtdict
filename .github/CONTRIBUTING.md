# Contributing to qualtdict

This outlines how to propose a change to qualtdict.

## Fixing typos

You can fix typos, spelling mistakes, or grammatical errors in the
documentation directly using the GitHub web interface, as long as the changes
are made in the _source_ file. This generally means you'll need to edit
[roxygen2 comments](https://roxygen2.r-lib.org/articles/roxygen2.html) in an
`.R` file, not a `.Rd` file. You can find the `.R` file that generates the
`.Rd` by reading the comment in the first line. `README.md` is generated from
`README.Rmd`, so edit the `.Rmd` file.

The spelling hook checks documentation against `inst/WORDLIST`. If it flags a
correctly spelled word, such as a package name or a technical term, add the
word to `inst/WORDLIST`.

## Bigger changes

If you want to make a bigger change, first file an issue and make sure the
maintainer agrees that it is needed. If you have found a bug, file an issue
that illustrates the bug with a minimal
[reprex](https://www.tidyverse.org/help/#reprex) (this will also help you
write a unit test, if needed).

### Pull request process

*   Fork the package and clone it onto your computer, for example with
    `usethis::create_from_github("lyh970817/qualtdict", fork = TRUE)`.

*   Install the development dependencies with
    `devtools::install_dev_deps()`.

*   Install the [pre-commit](https://pre-commit.com) hooks with
    `pre-commit install`. The hooks run formatting, roxygen, lintr, spelling
    and dependency checks on each commit.

*   Make sure the package passes R CMD check by running `devtools::check()`.
    If it doesn't pass cleanly, ask for help before continuing.

*   Create a Git branch for your pull request (PR), for example with
    `usethis::pr_init("brief-description-of-change")`.

*   Make your changes, commit them, and create a PR with `usethis::pr_push()`,
    following the prompts in your browser. The title of your PR should briefly
    describe the change. The body of your PR should contain
    `Fixes #issue-number`.

*   For user-facing changes, add a bullet to the top of `NEWS.md` (just below
    the first header). Follow the style described in
    <https://style.tidyverse.org/news.html>.

### Code style

*   New code follows the tidyverse [style guide](https://style.tidyverse.org).
    Formatting is done by [Air](https://posit-dev.github.io/air/): run
    `air format .`, which the pre-commit hooks also run. Don't reformat code
    unrelated to your change.

*   Source lines are limited to 80 columns (see
    `docs/adr/0007-use-80-column-source-line-limit.md`).

*   Documentation uses [roxygen2](https://roxygen2.r-lib.org/) with
    [Markdown syntax](https://roxygen2.r-lib.org/articles/markdown.html).

*   Unit tests use [testthat](https://testthat.r-lib.org/) edition 3.
    Contributions with test cases included are easier to accept.

### Testing without credentials

Tests and documentation examples never use private survey data. The fetch of
Qualtrics metadata is isolated in one internal function, and tests replace it
with `testthat::local_mocked_bindings()`, feeding it synthetic
Qualtrics-shaped metadata built by the helpers in
`tests/testthat/helper-*.R`. The test suite and R CMD check therefore run
without a Qualtrics account.

Only live metadata or response downloads need credentials. Register them with
`qualtRics::qualtrics_api_credentials()`.

The maintainer-only smoke check in `tools/` replays saved artifacts from real
surveys. It is not part of CI.

## Repository layout

`CONTEXT.md` is the vocabulary that code, documentation and tests follow. Each
term has an "Avoid" line listing wordings not to use. Use its terms in your
changes. The
[Glossary](https://lyh970817.github.io/qualtdict/articles/glossary.html) on
the package website mirrors its user-facing terms, so a change to one of
those definitions also updates `vignettes/articles/glossary.Rmd`. The
vocabulary of the maintainer smoke check is in
`tools/local-finalize-smoke.md`.

`docs/adr/` holds dated architecture decision records. A change that reverses
a recorded decision adds a new ADR rather than editing the old one.

`docs/design/` holds longer design notes.

`tools/` holds maintainer scripts that need Qualtrics credentials, following
the unofficial convention described at
<https://r-pkgs.org/misc.html#sec-misc-tools>. See
`tools/local-finalize-smoke.md` and `tools/fetch-local-finalize-smoke.md`.

## Code of Conduct

Please note that the qualtdict project is released with a
[Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this
project you agree to abide by its terms.
