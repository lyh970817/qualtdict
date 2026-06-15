# qualtdict

qualtdict is a metadata and labelled-export companion to qualtRics for Qualtrics
surveys. qualtRics owns API access; qualtdict owns the package-specific language
around survey metadata dictionaries, validation, naming, and labelled exports.
It promises to preserve response-column identity, expose validation and
unsupported-structure findings, and support Qualtrics Loop and Merge structures;
it does not promise to be a general Qualtrics downloader, survey cleaning
framework, or stable semantic-name generator.

## Language

**Metadata dictionary companion**:
The role of qualtdict relative to qualtRics: qualtdict turns Qualtrics metadata
retrieved through qualtRics into dictionary, validation, naming, and labelled
export artifacts.
_Avoid_: Downloader replacement, preprocessing framework

**Variable Dictionary**:
A tabular representation of a Qualtrics survey's variable metadata, including
variable identifiers, question text, item text, levels, labels, blocks, and
question type information.
_Avoid_: Metadata, survey dictionary, codebook

**qualtdict object**:
The R representation of a Variable Dictionary: a data frame with class
`qualtdict`.
_Avoid_: qualtdict dictionary, metadata object

**QID**:
The bare Qualtrics question identifier for a survey question, such as `QID1` or
`QID16`. One QID can produce multiple response columns.
_Avoid_: Question ID, question name, response column ID

**Response Column ID**:
The downloaded response column identifier used to match Labelled Survey Data
columns to Variable Dictionary rows. It is the provenance key for a dictionary
row. In current qualtdict exports this comes from
`qualtRics::fetch_survey(import_id = TRUE)`, and may include choice, item, text,
timing, column, or loop information.
_Avoid_: QID, question ID, column name

**Dictionary Variable Name**:
The analyst-facing variable name assigned by qualtdict. Stored in the Variable
Dictionary as `variable_name`; it may be selected from a Qualtrics Question Name
or from a generated Semantic Name, and is made suitable for labelled export
columns.
_Avoid_: QID, response column ID, column name

**Question Name**:
The Qualtrics API `questionName`, corresponding to the editable Qualtrics
question number or name such as `Q1`, `Q2`, `CSAT`, or `Intro`. A Question Name
is an always-present raw Qualtrics naming reference and remains distinct from
the selected Dictionary Variable Name.
_Avoid_: Question label, question text, question ID

**Semantic Name**:
A best-effort Dictionary Variable Name generated from survey text, block
information, and response metadata when Qualtrics question names are not
suitable for analysis. A Semantic Name is a readable convenience when present,
not a stability guarantee.
_Avoid_: Easy name, stable name, canonical name

**Export Variable**:
A column in labelled survey data after qualtdict has matched a Response Column
ID, renamed it to its Dictionary Variable Name, and attached variable metadata.
_Avoid_: Dictionary row, QID

**Validation Finding**:
A potential structural inconsistency detected in a Variable Dictionary, such as
non-unique or repaired names, duplicated levels or labels, non-consecutive
levels, or non-one-to-one level-label mappings. A clean validation result is a
consistency screen, not proof that the survey metadata is correct.
_Avoid_: Mistake, error, invalid dictionary

**Unsupported Structure Finding**:
A finding that qualtdict could not fully represent a Qualtrics question
structure while normalising Qualtrics metadata. Unsupported Structure Findings
are separate from Validation Findings, which concern the generated Variable
Dictionary.
_Avoid_: Unsupported warning, not applicable question

**Labelled Survey Data**:
Survey response data whose Export Variables have been renamed and annotated
with labels from a Variable Dictionary.
_Avoid_: Labeled survey data, raw survey data

**Labelled Export**:
The workflow or product category in which qualtdict applies a Variable
Dictionary to Qualtrics responses to produce Labelled Survey Data.
_Avoid_: Labeled export, raw export

**Survey Block**:
A Qualtrics grouping of questions, represented in the Variable Dictionary by
the `block` column.
_Avoid_: Block

**Block Prefix**:
A short string derived from a Survey Block and prepended to a Semantic Name to
help distinguish variables from different blocks.
_Avoid_: Block name, block pattern

**Loop and Merge Support**:
The target package capability of representing Qualtrics Loop and Merge
structures in Variable Dictionaries, Response Column IDs, question text,
Semantic Names, and Labelled Survey Data. This is a package promise to satisfy
before rOpenSci submission, not a claim that every Loop and Merge structure is
already covered.
_Avoid_: Partial loop handling

**Loop Option**:
A value in a Qualtrics Loop and Merge structure that can expand one survey
question into distinct Variable Dictionary rows and Export Variables.
_Avoid_: Loop label, loop prefix
