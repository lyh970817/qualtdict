# qualtdict 0.0.0.9000

- `dict_generate()` now represents flat Embedded Data Fields from Qualtrics
  metadata as Metadata-defined Export Variable rows with
  `row_source = "embedded_data"`.

- `dict_split_blocks()` and `survey_split_blocks()` now preserve unassigned
  Variable Dictionary rows in a `"..unassigned"` split.

- Improved rOpenSci package-check readiness by narrowing package imports,
  cleaning examples, reducing duplicated parameter documentation, and removing
  unused internal helpers.
