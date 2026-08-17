# Canonicalise the Normalised Record Schema

Status: accepted

Normalised records carry exactly one canonical name per concept. The
normalisation boundary (`normalise_question_fact()` and its choice, item and
column helpers) is the only place that may read raw Qualtrics spellings
(`questionType`, `recode`, `description`, `choiceText`, `textEntry`);
everything downstream of it reads the canonical field directly, with no
fallback.

## Problem

Every Normalised Question Fact used to carry each fact twice: the canonical
field (`level`, `label`, `item_text`, `text_entry`) plus a raw-spelled mirror
(`recode`, `description`, `choiceText`, `textEntry`), and two render-time
helpers manufactured a legacy `choices` field nothing read. On the reading
side, seventeen `question_fact_*()` accessors wrapped one-line field reads
behind a getter with a legacy-name fallback that no code path could trigger
any more. The duality made every record's shape ambiguous: a reader could not
tell which name was authoritative, and a writer could silently update one copy
but not the other.

## Decision

- **One name per concept.** The choice and item normalisers emit only
  canonical fields. The recode fact is named `level` on both record kinds,
  matching the Variable Dictionary's `level` column; items previously called
  it `recode` while choices called it `level`.
- **Normalise the question type once.** A private `raw_question_type()` reads
  the raw triple (including camelCase spellings) only inside the normaliser,
  which stores the clean `type` / `selector` / `sub_selector` list once.
  Downstream code reads `question$question_type$type` directly.
- **No fallback accessors.** The `question_fact_*()` shims and their
  `question_fact_value()` legacy-name primitive are deleted; readers use plain
  `$` field access. The normaliser's output shape is the contract.
- **No legacy dictionary columns.** `dict_response_column_id()` and
  `dict_variable_name()` are plain column reads; the pre-rename `qid` / `name`
  fallbacks are gone because no supported code path produces that shape.
- **One Loop Option column.** The Variable Dictionary carried the resolved
  Loop Option twice, as `looping_option` and a duplicate `loop_option`. The
  duplicate is dropped; `looping_option` is the one column, including in the
  dictionary `dict_generate()` returns (which previously exposed the
  `loop_option` spelling). No consumer in this repo or in the ilovedata
  engine read `loop_option`.

## Consequences

A record field either exists under its canonical name or the record did not
pass through normalisation — there is no third state. Raw-spelling reads
outside the normalisation boundary are defects. Externally, the generated
Variable Dictionary is unchanged except that its Loop Option column is now
named `looping_option`.
