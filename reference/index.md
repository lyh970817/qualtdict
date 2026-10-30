# Package index

## Generate and validate dictionaries

Build Variable Dictionaries from Qualtrics metadata and inspect their
findings.

- [`dict_generate()`](https://lyh970817.github.io/qualtdict/reference/dict_generate.md)
  : Generate a Variable Dictionary for a survey
- [`dict_validate()`](https://lyh970817.github.io/qualtdict/reference/dict_validate.md)
  : Return Validation Findings for a Variable Dictionary
- [`dict_assert_valid()`](https://lyh970817.github.io/qualtdict/reference/dict_assert_valid.md)
  : Require a Variable Dictionary that can be exported

## Create labelled survey data

Apply a Variable Dictionary to downloaded responses and inspect export
findings.

- [`survey_fetch_labelled()`](https://lyh970817.github.io/qualtdict/reference/survey_fetch_labelled.md)
  : Fetch Labelled Survey Data
- [`survey_export_findings()`](https://lyh970817.github.io/qualtdict/reference/survey_export_findings.md)
  : Return Labelled Export Findings

## Work with survey blocks

Create block-specific views of dictionaries and labelled survey data.

- [`dict_split_blocks()`](https://lyh970817.github.io/qualtdict/reference/dict_split_blocks.md)
  : Split a Variable Dictionary by Survey Block
- [`survey_split_blocks()`](https://lyh970817.github.io/qualtdict/reference/survey_split_blocks.md)
  : Split Labelled Survey Data by Survey Block

## Credentials

Configure Qualtrics API credentials through the re-exported qualtRics
helper.

- [`reexports`](https://lyh970817.github.io/qualtdict/reference/reexports.md)
  [`qualtrics_api_credentials`](https://lyh970817.github.io/qualtdict/reference/reexports.md)
  : Objects exported from other packages

## Example data

An offline example survey with its metadata, Variable Dictionary, and
Labelled Survey Data.

- [`glad_example`](https://lyh970817.github.io/qualtdict/reference/glad_example.md)
  : Example survey metadata, Variable Dictionary, and Labelled Survey
  Data
