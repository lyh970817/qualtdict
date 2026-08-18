# qualtdict

qualtdict is a metadata and labelled-export companion to qualtRics for
Qualtrics surveys. qualtRics owns API access; qualtdict owns the
package-specific language around survey metadata dictionaries,
validation, naming, and labelled exports. It promises to preserve
response-column identity, expose validation findings, and support
Qualtrics Loop and Merge structures; it does not promise to be a general
Qualtrics downloader, survey cleaning framework, or stable semantic-name
generator.

## Language

**Metadata dictionary companion**: The role of qualtdict relative to
qualtRics: qualtdict turns Qualtrics metadata retrieved through
qualtRics into dictionary, validation, naming, and labelled export
artifacts. *Avoid*: Downloader replacement, preprocessing framework

**Variable Dictionary**: A tabular representation of a Qualtrics
survey’s variable metadata, including variable identifiers, question
text, item text, levels, labels, blocks, and question type information.
A Variable Dictionary can include rows for question-backed export
variables and for metadata-defined export variables such as Embedded
Data Fields, Scoring Variables, and Text-analysis Sidecars. *Avoid*:
Metadata, survey dictionary, codebook

**qualtdict object**: The R representation of a Variable Dictionary: a
data frame with class `qualtdict`. *Avoid*: qualtdict dictionary,
metadata object

**Dictionary Row Source**: The kind of metadata fact that produced a
Variable Dictionary row. It separates ordinary question-backed rows from
metadata-defined export variables while preserving Response Column ID as
the row provenance key. Expected sources include `question`,
`embedded_data`, `scoring`, and `text_analysis`. *Avoid*: Row type,
column type, source column

**QID**: The bare Qualtrics question identifier for a survey question,
such as `QID1` or `QID16`. One QID can produce multiple response
columns. *Avoid*: Question ID, question name, response column ID

**Metadata-defined Export Variable**: An export variable represented by
Qualtrics survey metadata but not produced by ordinary question response
rendering. Metadata-defined Export Variables belong in the Variable
Dictionary when Qualtrics metadata defines them. They still use Response
Column ID as their exported-column identifier and Dictionary Row Source
to describe their origin. *Avoid*: Extra column, raw response column,
unmatched column

**Non-question Export Variable**: A Metadata-defined Export Variable
that is not an ordinary question-backed response variable. Embedded Data
Fields and Scoring Variables are Non-question Export Variables.
Text-analysis Sidecars are question-adjacent but should still be
distinguished from primary question response rows by Dictionary Row
Source. *Avoid*: Raw response column, ignored column, unknown sidecar

**Normalised Question Fact**: The package-owned representation of one
Qualtrics question after raw metadata normalisation and before Variable
Dictionary row rendering. It carries stable question-level facts such as
the QID, Question Name, question text, Survey Block, question type,
response choices, response items, column facts, and Loop and Merge
source references. It is the input shape for Response Column ID
Rendering and the intended upstream shape for Loop and Merge expansion.
*Avoid*: Raw Qualtrics question, dictionary row, response column

**Response Column ID**: The downloaded response column identifier used
to match Labelled Survey Data columns to Variable Dictionary rows. It is
the provenance key for a dictionary row. In current qualtdict
question-backed exports this corresponds to
`qualtRics::fetch_survey(import_id = TRUE)` and may include choice,
item, text, timing, column, or loop information. Metadata-defined Export
Variables also use Response Column ID for their exported-column
identifier. *Avoid*: QID, question ID, column name

**Response Column ID Rendering**: The package-owned capability that
turns already-normalised question facts into the concrete Response
Column IDs used in Variable Dictionaries and Labelled Survey Data
matching. It owns Qualtrics response-column string grammar for question
shapes, choices, items, text entries, timing fields, and file-upload
fields. Loop and Merge expansion sits upstream: rendered question facts
may already include loop-derived prefixes or Loop Option context, but
this capability does not choose Loop Options or substitute Loop and
Merge question text. *Avoid*: QID recoding, column-name generation, loop
expansion

**Base Response Column ID**: The Response Column ID stem supplied to
Response Column ID Rendering before question-shape-specific suffixes are
added. It may be a bare QID or may already include a Loop and Merge
prefix; it is not necessarily a QID. *Avoid*: response column QID,
rendered QID

**Response Column Map Classification**: The package-owned interpretation
of Qualtrics response column map rows as question-backed rows,
Metadata-defined Export Variables, system metadata, question auxiliary
columns, or unknown rows. It links response column map entries to
Dictionary Row Source decisions without changing Response Column IDs.
*Avoid*: Sidecar detection, raw column parsing

**Dictionary Variable Name**: The analyst-facing variable name assigned
by qualtdict. Stored in the Variable Dictionary as `variable_name`; it
may be selected from a Qualtrics Question Name or from a generated
Semantic Name, and is made suitable for labelled export columns.
*Avoid*: QID, response column ID, column name

**Question Name**: The Qualtrics API `questionName`, corresponding to
the editable Qualtrics question number or name such as `Q1`, `Q2`,
`CSAT`, or `Intro`. A Question Name is an always-present raw Qualtrics
naming reference and remains distinct from the selected Dictionary
Variable Name. *Avoid*: Question label, question text, question ID

**Semantic Name**: A best-effort Dictionary Variable Name generated from
survey text, block information, and response metadata when Qualtrics
question names are not suitable for analysis. A Semantic Name is a
readable convenience when present, not a stability guarantee; when it
derives words from naming text, it preserves their source order.
*Avoid*: Easy name, stable name, canonical name

**Export Variable**: A column in labelled survey data after qualtdict
has matched a Response Column ID, renamed it to its Dictionary Variable
Name, and attached variable metadata. *Avoid*: Dictionary row, QID

**Rendered Response-column Coverage**: The package capability of
representing non-system exported response columns in a Variable
Dictionary from Qualtrics metadata and Response Column ID Rendering,
with response-schema metadata used only for export filtering or
discovery where ordinary survey metadata does not carry the needed fact.
*Avoid*: Raw-header coverage, schema fallback, extra-column mode

**Response-schema Filter**: The use of Qualtrics response-schema
metadata to decide whether a metadata-defined export variable is present
in the export schema. In current scope, this applies to Embedded Data
Fields, Scoring Variables, and Text-analysis Sidecars, not to ordinary
question-backed Response Column ID Rendering. *Avoid*: Response-column
source of truth, raw-header filter

**Embedded Data Field**: A Qualtrics embedded data field defined by
survey metadata or survey flow and represented as a Metadata-defined
Export Variable in the Variable Dictionary. Embedded Data Fields are not
QIDs and should not be treated as question-backed rows, but they can
still be matched to Labelled Survey Data by Response Column ID. *Avoid*:
Embedded variable, extra column, user metadata column

**Scoring Variable**: A Qualtrics scoring output defined by scoring
metadata and represented as a Metadata-defined Export Variable in the
Variable Dictionary. Scoring Variables are not ordinary question
responses even when their values are derived from question answers.
*Avoid*: Score column, scoring sidecar, calculated raw column

**Text-analysis Sidecar**: A metadata-defined export variable derived
from Qualtrics text-analysis settings for a text response. A
Text-analysis Sidecar is question-adjacent and may refer to a parent
QID, but it is distinct from the primary text response row. *Avoid*:
Text column, raw sidecar, analysis column

**Validation Finding**: A potential structural inconsistency detected in
a Variable Dictionary, such as non-unique or repaired names, duplicated
levels or labels, non-consecutive levels, or non-one-to-one level-label
mappings. A clean validation result is a consistency screen, not proof
that the survey metadata is correct. *Avoid*: Mistake, error, invalid
dictionary

**Export-blocking Validation Finding**: A Validation Finding whose
level-label coding makes Labelled Export wrong or impossible: label and
level are not one-to-one, one label is carried by several rows, or one
level is carried by several rows. A level carried by several rows is the
aborting shape, because Qualtrics keys an export column on the choice
recode and exports one column for two choices that share it. A gapped
level sequence is not Export-blocking. Export-blocking Validation
Findings are reported, never repaired: the Variable Dictionary keeps
every row exactly as the Qualtrics survey defines it. *Avoid*: Fatal
finding, invalid dictionary, broken level

**Definite Validation Finding**: A Validation Finding whose row is wrong
in a way that makes its export column uninterpretable or its identity
unreliable: the Export-blocking level-label codings, and the
variable-name findings that break the rename identity of Labelled Export
(inconsistent, duplicate, or unsafe final `variable_name` values).
Reported with `severity == "definite"` by
[`dict_validate()`](https://lyh970817.github.io/qualtdict/reference/dict_validate.md);
the set `exclude_findings = "definite"` drops. Every Export-blocking
Validation Finding is a Definite Validation Finding. An unrecognised
finding class classifies as definite until it is classified
deliberately. *Avoid*: fatal finding, hard finding, error-class finding

**Suggestive Validation Finding**: A Validation Finding that reports
something worth review while the column’s data and identity stay sound:
a repaired `variable_name` (the repair succeeded; data and labels are
untouched), or a level-label finding none of whose tripped tests is
Export-blocking (a gapped or non-step-1 level run is ordinary Qualtrics
survey design). Reported with `severity == "suggestive"` by
[`dict_validate()`](https://lyh970817.github.io/qualtdict/reference/dict_validate.md);
never dropped by `exclude_findings = "definite"` and never aborts the
pre-download gate. *Avoid*: soft finding, warning-class finding,
cosmetic finding

**Labelled Export Finding**: A finding detected while matching a
Variable Dictionary to downloaded survey data during Labelled Export,
such as a Response Column ID represented by the Variable Dictionary but
absent from the downloaded data. Labelled Export Findings are separate
from Validation Findings because they are detected after metadata
normalisation, while assembling Labelled Survey Data. *Avoid*: Export
error

**Labelled Survey Data**: Participant Response Data whose Export
Variables have been renamed and annotated with labels from a Variable
Dictionary. *Avoid*: Labeled survey data, raw survey data

**Participant Response Data**: Record-level data submitted by or
associated with survey participants, whether downloaded directly from
Qualtrics or transformed into Labelled Survey Data. Variable
Dictionaries, survey metadata, question text, response choices, Response
Column IDs, Validation Findings, and Labelled Export Findings are not
Participant Response Data unless they contain row-level participant
values. *Avoid*: Responses, raw data

**Labelled Export**: The workflow or product category in which qualtdict
applies a Variable Dictionary to Qualtrics responses to produce Labelled
Survey Data. *Avoid*: Labeled export, raw export

**Survey Block**: A Qualtrics grouping of questions, represented in the
Variable Dictionary by the `block` column. *Avoid*: Block

**Block Prefix**: A short string derived from a Survey Block and
prepended to a Semantic Name to help distinguish variables from
different blocks. *Avoid*: Block name, block pattern

**Loop and Merge Support**: The target package capability of
representing Qualtrics Loop and Merge structures in Variable
Dictionaries, Response Column IDs, question text, Semantic Names, and
Labelled Survey Data. This is a package promise to satisfy before
rOpenSci submission, not a claim that every Loop and Merge structure is
already covered. *Avoid*: Partial loop handling

**Loop Option**: A value in a Qualtrics Loop and Merge structure that
can expand one survey question into distinct Variable Dictionary rows
and Export Variables. *Avoid*: Loop label, loop prefix

**Loop-expanded Question Fact**: A normalised question fact after the
Loop and Merge expansion adapter has resolved one Loop Option for a
looped question. It preserves the bare QID, records the loop source QID,
loop prefix, and Loop Option, and carries question text with supported
Loop and Merge placeholders already substituted. Response Column ID
Rendering consumes Loop-expanded Question Facts as ordinary question
facts; it does not choose Loop Options or substitute Loop and Merge
text. *Avoid*: looped dictionary row, patched row

### Finalize Smoke

**Finalize Smoke**: The local-only workflow that runs qualtdict’s
exported functions over fixed local survey artifacts and compares the
results against Smoke Baselines. It is a finalization gate run by a
human or agent on one machine, never CI. *Avoid*: smoke test suite,
integration tests

**Smoke Scenario**: One exported-function execution against one survey
within a Finalize Smoke run, such as `dict_generate` on one survey.
Bless and Check runs execute the same Smoke Scenarios; they differ only
in what is done with the results. *Avoid*: test case, smoke step

**Scenario Time**: The summed execution time spent inside Smoke
Scenarios during one Finalize Smoke run. It excludes harness overhead
such as artifact loading, object hashing, and baseline reading and
writing, so a run’s wall-clock time is a multiple of its Scenario Time.
It is a property of any run, whether Bless or Check. *Avoid*: run time,
wall time, smoke duration

**Bless**: A Finalize Smoke run that records its Smoke Scenario results
as the Smoke Baseline. Blessing asserts that the current behaviour is
the intended reference, so it follows verification, never substitutes
for it. *Avoid*: update baselines, snapshot

**Check**: A Finalize Smoke run that executes the Smoke Scenarios and
compares their results against the Smoke Baseline, failing loudly on any
mismatch or missing baseline entry. *Avoid*: verify run, smoke
comparison

**Smoke Baseline**: The recorded per-survey Smoke Scenario results that
Check compares against, keyed by survey and scenario. Smoke Baselines
are local to the machine that Blessed them and are never committed.
*Avoid*: snapshot, expected output, golden file
