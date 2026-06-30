# Changelog

## qualtdict 0.0.0.9000

- [`dict_generate()`](https://lyh970817.github.io/qualtdict/reference/dict_generate.md)
  now represents Scoring Variables from Qualtrics survey description
  metadata as Metadata-defined Export Variable rows with
  `row_source = "scoring"`.

- [`dict_generate()`](https://lyh970817.github.io/qualtdict/reference/dict_generate.md)
  now accepts `embedded_data_block_assignment` to optionally assign
  Survey Flow Embedded Data Fields to the nearest previous or next
  Survey Block while leaving them unassigned by default.

- [`dict_generate()`](https://lyh970817.github.io/qualtdict/reference/dict_generate.md)
  now represents flat Embedded Data Fields from Qualtrics metadata as
  Metadata-defined Export Variable rows with
  `row_source = "embedded_data"`.

- [`dict_split_blocks()`](https://lyh970817.github.io/qualtdict/reference/dict_split_blocks.md)
  and
  [`survey_split_blocks()`](https://lyh970817.github.io/qualtdict/reference/survey_split_blocks.md)
  now preserve unassigned Variable Dictionary rows in a `"..unassigned"`
  split.

- Improved rOpenSci package-check readiness by narrowing package
  imports, cleaning examples, reducing duplicated parameter
  documentation, and removing unused internal helpers.
