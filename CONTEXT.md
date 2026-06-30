# qualtdict

qualtdict is a metadata and labelled-export companion to qualtRics for Qualtrics
surveys. qualtRics owns API access; qualtdict owns the package-specific language
around survey metadata dictionaries, validation, naming, and labelled exports.
It promises to preserve response-column identity, expose validation findings,
and support Qualtrics Loop and Merge structures; it does not promise to be a
general Qualtrics downloader, survey cleaning framework, or stable semantic-name
generator.

## Language

**Metadata dictionary companion**:
The role of qualtdict relative to qualtRics: qualtdict turns Qualtrics metadata
retrieved through qualtRics into dictionary, validation, naming, and labelled
export artifacts.
_Avoid_: Downloader replacement, preprocessing framework

**Variable Dictionary**:
A tabular representation of a Qualtrics survey's variable metadata, including
variable identifiers, question text, item text, levels, labels, blocks, and
question type information. A Variable Dictionary can include rows for
question-backed export variables and for metadata-defined export variables such
as Embedded Data Fields, Scoring Variables, and Text-analysis Sidecars.
_Avoid_: Metadata, survey dictionary, codebook

**qualtdict object**:
The R representation of a Variable Dictionary: a data frame with class
`qualtdict`.
_Avoid_: qualtdict dictionary, metadata object

**Dictionary Row Source**:
The kind of metadata fact that produced a Variable Dictionary row. It separates
ordinary question-backed rows from metadata-defined export variables while
preserving Response Column ID as the row provenance key. Expected sources
include `question`, `embedded_data`, `scoring`, and `text_analysis`.
_Avoid_: Row type, column type, source column

**QID**:
The bare Qualtrics question identifier for a survey question, such as `QID1` or
`QID16`. One QID can produce multiple response columns.
_Avoid_: Question ID, question name, response column ID

**Metadata-defined Export Variable**:
An export variable represented by Qualtrics survey metadata but not produced by
ordinary question response rendering. Metadata-defined Export Variables belong
in the Variable Dictionary when Qualtrics metadata defines them. They still use
Response Column ID as their exported-column identifier and Dictionary Row Source
to describe their origin.
_Avoid_: Extra column, raw response column, unmatched column

**Non-question Export Variable**:
A Metadata-defined Export Variable that is not an ordinary question-backed
response variable. Embedded Data Fields and Scoring Variables are Non-question
Export Variables. Text-analysis Sidecars are question-adjacent but should still
be distinguished from primary question response rows by Dictionary Row Source.
_Avoid_: Raw response column, ignored column, unknown sidecar

**Normalised Question Fact**:
The package-owned representation of one Qualtrics question after raw metadata
normalisation and before Variable Dictionary row rendering. It carries stable
question-level facts such as the QID, Question Name, question text, Survey
Block, question type, response choices, response items, column facts, and Loop
and Merge source references. It is the input shape for Response Column ID
Rendering and the intended upstream shape for Loop and Merge expansion.
_Avoid_: Raw Qualtrics question, dictionary row, response column

**Response Column ID**:
The downloaded response column identifier used to match Labelled Survey Data
columns to Variable Dictionary rows. It is the provenance key for a dictionary
row. In current qualtdict question-backed exports this corresponds to
`qualtRics::fetch_survey(import_id = TRUE)` and may include choice, item, text,
timing, column, or loop information. Metadata-defined Export Variables also use
Response Column ID for their exported-column identifier.
_Avoid_: QID, question ID, column name

**Response Column ID Rendering**:
The package-owned capability that turns already-normalised question facts into
the concrete Response Column IDs used in Variable Dictionaries and Labelled
Survey Data matching. It owns Qualtrics response-column string grammar for
question shapes, choices, items, text entries, timing fields, and file-upload
fields. Loop and Merge expansion sits upstream: rendered question facts may
already include loop-derived prefixes or Loop Option context, but this
capability does not choose Loop Options or substitute Loop and Merge question
text.
_Avoid_: QID recoding, column-name generation, loop expansion

**Response Column Map Classification**:
The package-owned interpretation of Qualtrics response column map rows as
question-backed rows, Metadata-defined Export Variables, system metadata,
question auxiliary columns, or unknown rows. It links response column map
entries to Dictionary Row Source decisions without changing Response Column
IDs.
_Avoid_: Sidecar detection, raw column parsing

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
not a stability guarantee; when it derives words from naming text, it preserves
their source order.
_Avoid_: Easy name, stable name, canonical name

**Export Variable**:
A column in labelled survey data after qualtdict has matched a Response Column
ID, renamed it to its Dictionary Variable Name, and attached variable metadata.
_Avoid_: Dictionary row, QID

**Embedded Data Field**:
A Qualtrics embedded data field defined by survey metadata or survey flow and
represented as a Metadata-defined Export Variable in the Variable Dictionary.
Embedded Data Fields are not QIDs and should not be treated as question-backed
rows, but they can still be matched to Labelled Survey Data by Response Column
ID.
_Avoid_: Embedded variable, extra column, user metadata column

**Scoring Variable**:
A Qualtrics scoring output defined by scoring metadata and represented as a
Metadata-defined Export Variable in the Variable Dictionary. Scoring Variables
are not ordinary question responses even when their values are derived from
question answers.
_Avoid_: Score column, scoring sidecar, calculated raw column

**Text-analysis Sidecar**:
A metadata-defined export variable derived from Qualtrics text-analysis
settings for a text response. A Text-analysis Sidecar is question-adjacent and
may refer to a parent QID, but it is distinct from the primary text response
row.
_Avoid_: Text column, raw sidecar, analysis column

**Validation Finding**:
A potential structural inconsistency detected in a Variable Dictionary, such as
non-unique or repaired names, duplicated levels or labels, non-consecutive
levels, or non-one-to-one level-label mappings. A clean validation result is a
consistency screen, not proof that the survey metadata is correct.
_Avoid_: Mistake, error, invalid dictionary

**Labelled Export Finding**:
A finding detected while matching a Variable Dictionary to downloaded survey
data during Labelled Export, such as a Response Column ID represented by the
Variable Dictionary but absent from the downloaded data. Labelled Export
Findings are separate from Validation Findings because they are detected after
metadata normalisation, while assembling Labelled Survey Data.
_Avoid_: Export error

**Labelled Survey Data**:
Participant Response Data whose Export Variables have been renamed and
annotated with labels from a Variable Dictionary.
_Avoid_: Labeled survey data, raw survey data

**Participant Response Data**:
Record-level data submitted by or associated with survey participants, whether
downloaded directly from Qualtrics or transformed into Labelled Survey Data.
Variable Dictionaries, survey metadata, question text, response choices,
Response Column IDs, Validation Findings, and Labelled Export Findings are not
Participant Response Data unless they contain row-level participant values.
_Avoid_: Responses, raw data

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

**Loop-expanded Question Fact**:
A normalised question fact after the Loop and Merge expansion adapter has
resolved one Loop Option for a looped question. It preserves the bare QID,
records the loop source QID, loop prefix, and Loop Option, and carries question
text with supported Loop and Merge placeholders already substituted. Response
Column ID Rendering consumes Loop-expanded Question Facts as ordinary question
facts; it does not choose Loop Options or substitute Loop and Merge text.
_Avoid_: looped dictionary row, patched row
