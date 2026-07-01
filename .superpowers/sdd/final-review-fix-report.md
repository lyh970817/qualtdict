## 2026-07-01 final review fix

- Added focused synthetic coverage in `tests/testthat/test-response_column_render_special.R` for the `Draw / Signature` dispatch path, asserting it renders the same four fixed file-upload Response Column ID suffixes as `FileUpload`.
- Added focused synthetic coverage in `tests/testthat/test-response_column_render_special.R` for the `PGR / DragAndDrop / NoColumns` dispatch path, asserting it warns with Base Response Column ID wording and falls back to the Base Response Column ID.
- Updated `tests/testthat/_snaps/response_column_render_special.md` with the two new snapshot entries.
- Verified with:
  - `Rscript -e 'testthat::test_file("tests/testthat/test-response_column_render_special.R")'`
  - `Rscript -e 'testthat::test_file("tests/testthat/test-response_column_render.R")'`
  - `Rscript -e 'testthat::test_file("tests/testthat/test-response_column_render_mc.R")'`
  - `Rscript -e 'testthat::test_file("tests/testthat/test-response_column_render_item_level.R")'`
  - `Rscript -e 'testthat::test_file("tests/testthat/test-response_column_render_sbs.R")'`
