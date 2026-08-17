# qualtdict 0.0.0.9000

- The Variable Dictionary returned by `dict_generate()` now names its Loop
  Option column `looping_option` instead of `loop_option`. The two names
  carried the same value and the internal duplicate is removed, so the
  dictionary keeps one name per concept (ADR 0010).

- `dict_generate()` no longer accepts the pre-release compatibility arguments
  `name` and `preprocess`, nor the `easy_name` value. Use `variable_name`
  (with `"question_name"` or `"semantic_name"`) and
  `semantic_name_preprocess` instead. The aliases warned on every use and are
  removed before the first release, so the package keeps one name per concept.

- `dict_generate()` no longer fails on a Loop and Merge source choice whose
  label is blank or missing. Such a Loop Option now takes the source choice
  ID as its label instead of aborting Loop expansion for the whole survey.

- `dict_validate()` output now classifies every Validation Finding by
  `severity`: `"definite"` when the finding makes the affected export column
  uninterpretable or its identity unreliable (the Export-blocking
  level-label codings, and inconsistent, duplicate, or unsafe final
  `variable_name` values), `"suggestive"` when the finding only reports
  something worth review while the column's data and identity stay sound (a
  repaired `variable_name`; a gapped level run). Severity is derived from the
  finding class on every normalisation, so it cannot drift, and an
  unrecognised class classifies as `"definite"` (fails closed).
  `fetch_labelled_survey_data()` gains `exclude_findings = "definite"`, which
  drops only the rows carrying Definite Validation Findings and keeps every
  row whose findings are merely suggestive; `"validation"` keeps its existing
  drop-everything meaning. The pre-download `assert_dict_valid()` gate is
  skipped for both `"definite"` and `"validation"` (every Export-blocking
  Response Column ID is dropped after download under either), and it never
  aborts on Suggestive Validation Findings under any setting.

- `assert_dict_valid()` is a new exported gate that errors when a Variable
  Dictionary carries Export-blocking Validation Findings: level-label codings
  where label and level are not one-to-one, one label is carried by several
  rows, or one level is carried by several rows. The error names every
  offending Response Column ID, its Dictionary Variable Name, and the colliding
  labels. A gapped level sequence is not Export-blocking.
  `fetch_labelled_survey_data()` now applies the gate before downloading
  responses, so a defective Variable Dictionary no longer aborts a whole survey
  deep inside labelling after the download is paid for; pass
  `require_valid_dict = FALSE` to download from a survey known to carry such
  findings. `dict_validate()` is unchanged and stays total: it reports every
  Validation Finding and never errors.

- `dict_generate()` no longer declares a choice recode as the Level universe
  of a `_DO_` display-order column. Qualtrics exports one such column per
  choice: the recode names the column and the cell holds the position at which
  that choice was displayed. These columns now declare no Level universe,
  carry the choice in `item`, and take a variable label but no value labels in
  `fetch_labelled_survey_data()`.

- `dict_generate()` now resolves response-column-id parity with Qualtrics
  exports for embedded, Captcha, Text-entry/FORM, and carry-forward
  multiple-choice questions: colliding embedded fields honour the Qualtrics
  QSED export-rename, Captcha and `analyze == FALSE` text-entry fields are
  suppressed (no export column), and carry-forward multiple-choice export
  choiceIds are derived from survey-definition `RecodeValues`/`DynamicChoices`
  rather than sequential `/surveys` recodes.

- `dict_generate()` now preserves question context while representing
  Embedded Data Fields when Qualtrics description metadata uses the
  `blocks`/`questions` structure returned by `qualtRics::fetch_description()`.

- Local finalization smoke artifact refresh now captures Qualtrics
  `embedded_data`, scoring metadata, and response column-map metadata for
  Metadata-defined Export Variable inspection.

- `dict_generate()` now represents Scoring Variables from Qualtrics survey
  description metadata as Metadata-defined Export Variable rows with
  `row_source = "scoring"`.

- `dict_generate()` now represents Text-analysis Sidecars from Qualtrics
  metadata as Metadata-defined Export Variable rows with
  `row_source = "text_analysis"` and parent question context when a clear
  parent QID is available.

- `dict_generate()` now accepts `embedded_data_block_assignment` to optionally
  assign Survey Flow Embedded Data Fields to the nearest previous or next
  Survey Block while leaving them unassigned by default.

- `dict_generate()` now represents flat Embedded Data Fields from Qualtrics
  metadata as Metadata-defined Export Variable rows with
  `row_source = "embedded_data"`.

- `dict_split_blocks()` and `survey_split_blocks()` now preserve unassigned
  Variable Dictionary rows in a `"..unassigned"` split.

- Improved rOpenSci package-check readiness by narrowing package imports,
  cleaning examples, reducing duplicated parameter documentation, and removing
  unused internal helpers.
