# Distinguish Collapsed Side-by-side Grid Cell Identity

Status: accepted

Response Column ID Rendering will make two grid (side-by-side) cell identities
survive into the Variable Dictionary that Qualtrics metadata otherwise erases:
the row identity of blank-labelled subquestions, and the column identity of
sibling columns that carry byte-identical question text. Both are rendered from
Normalised Question Facts; neither changes a Response Column ID.

Without these, distinct grid cells render byte-identical question text, item
text, and Label Maps. Downstream, content hashing (which excludes grid position
by design, so that the same cell matches across surveys) then pools genuinely
different cells under one identity. This ADR fixes the identity at the source in
qualtdict so the rendered facts already distinguish the cells.

## Problem 1 — blank row labels collapse grid rows

The long-term-medication form (glad_sa8_signup `QID1187`, edgi `QID525`) is an
SBS/SBSMatrix question with six subquestions whose labels are blank. The only
distinguishing raw fact is the subquestion recode key `"1"`…`"6"`, which was
read to build the Response Column ID but never surfaced into `item`:

```r
# metadata.rds -> questions$QID1187$subQuestions
$`1`$recode      : "1"
$`1`$description  : "&nbsp;"
$`1`$choiceText   : "&nbsp;"
$`3`$recode      : "3"
$`3`$description  : ""      # later rows are the empty string, not "&nbsp;"
$`3`$choiceText   : ""
```

Before the fix, all six `QID1187#1_*_1` dictionary entries carried an empty
`item` and identical question text. Verified against responses, fixture person
`157588` held Midodrine / Low dose naltrexone / Salbutamol / Cetirizine /
Epipens across five cells that had collapsed to one identity.

### Decision

`response_column_sbs_item_shape()` (via `sbs_fill_blank_item_labels()`) falls
back to the subquestion recode (its `x<N>` key) whenever the row label is blank
— an empty string, a `&nbsp;`-only string, or whitespace/non-breaking-space
only. Non-blank labels are untouched. The recode is a stable per-row fact shared
across surveys, so glad row *N* and edgi row *N* still render matching `item`
and their cross-survey Exact Match survives.

The numeric fallback does not collide with `clean_variable_dictionary_rows()`,
which NAs `item` only when it equals the (much longer) question text.

## Problem 2 — duplicated column text collapses grid columns

The family-history side-by-sides (edgi `QID461`, glad_sa8_signup
`QID1215122586`, and the diabetes form `QID1215122591`) place 23 family-member
rows against columns where each disease Likert column is immediately followed by
an "Age at diagnosis" text-entry column. The two age columns share byte-
identical `questionText`; the disease pairing exists only as adjacency in
`columnOrder`:

```r
# edgi metadata.rds -> questions$QID461, columnOrder = 1 2 3 4
col[1]: Matrix/Likert  "Coronary artery disease"
col[2]: Matrix/TE      "Age at diagnosis"
col[3]: Matrix/Likert  "Stroke"
col[4]: Matrix/TE      "Age at diagnosis"    # identical text to col[2]
```

With 23 rows × 2 identical age columns, edgi `QID461#2_x_1` and `QID461#4_x_1`
collapsed to one identity per row, and the same collapse recurred across the
three surveys. glad states the same columns with `<strong>` markup
(`"<strong>Age at diagnosis</strong>"`), which the dictionary's HTML unescaping
later strips, so the collapse is identical in both surveys.

### Decision

`response_column_sbs_questions()` (via `sbs_column_qualified_texts()`) qualifies
a column's question text **only** when that text is duplicated among its sibling
columns. It uses `columnOrder` (`column_position`, captured by
`normalise_column_facts()`) to resolve a single grid **direction** rather than
walking greedily to the first candidate. A partner is adopted only when that
direction is unambiguous: for every duplicated column, its immediate neighbour
on the SAME side (all to the left, or all to the right) must be an **eligible
anchor** — a column whose text is non-blank, is *not itself duplicated* among
the siblings, and whose selector differs (a Likert vs TE sanity check) — no
anchor may be shared between two duplicated columns, and only one side may
satisfy this. The chosen partner is prepended:

```
Coronary artery disease — Age at diagnosis
Stroke — Age at diagnosis
```

When no single direction resolves cleanly, it falls back to an honest ordinal:

```
Age at diagnosis (column 2)
```

Blank column text is never qualified: those columns are distinguished by their
choice Label Maps, and the engine's same-QID guard is the backstop.

The partner text is prepended raw; HTML markup is stripped later by the
dictionary's shared unescaping, so edgi (`"Coronary artery disease"`) and glad
(`"<strong>Coronary artery disease</strong>"`) both normalise to the same
qualified text and their cross-survey Exact Match survives.

### Why direction, not a nearest-partner walk

An unconditional walk to the nearest distinct, differently-typed column produces
confident wrong meanings. In a reversed grid `[Age, Coronary, Age, Stroke]` the
second age column's nearest preceding distinct column is `Coronary`, but that
age is Stroke's — and a wrong "Coronary artery disease — Age at diagnosis" would
then cross-survey Exact-Match a real coronary-age column (a merge the engine's
same-`(survey, qid)` guard cannot catch, because it spans surveys). Resolving a
whole-grid direction instead — and requiring the anchor itself to be unique —
keeps every adopted pairing unambiguous, and prefers the ordinal in every
ambiguous case (a wrong meaning is worse than a vague one). Behaviour on the
named failure modes:

- **Reversed order** — `[Age, Coronary, Age, Stroke]`: the left side is invalid
  (the first age has no left neighbour), so only the right direction resolves;
  each age pairs with the disease that actually follows it. The second age
  correctly reads `Stroke — Age at diagnosis`, never `Coronary`.
- **A shared value column** — `[Coronary, Stroke, Age, Age]`: neither age has a
  per-column anchor adjacency (one age's neighbour is the other age), so neither
  direction resolves and both fall back to the ordinal.
- **Duplicated anchor columns** — `[Coronary, Age, Coronary, Age]`: the repeated
  `Coronary` is not an eligible anchor (it cannot distinguish the ages it would
  qualify), so no direction resolves and the ordinal applies.
- **Longer repeating units** — `[Diabetes type 1, Age, Insulin?, Diabetes type
  2, Age]` (the real diabetes form): the extra Likert column sits between units,
  but each age's immediate-left disease is still its unique anchor and only the
  left direction is valid, so the correct pairing survives. Where a longer unit
  instead makes both directions valid — e.g. a trailing disease in
  `[Coronary, Age, Stroke, Age, Diabetes]` — no direction is unique and the
  ordinal applies.

## Considered Options

- Encode grid position into the content hash. Rejected: position must stay
  excluded so the same cell matches across surveys (ADR 0001's normalised model
  is the layer that owns cross-survey-stable facts).
- Qualify every SBS column unconditionally. Rejected: it perturbs columns that
  are already distinct and invents pairings where none is warranted.
- Fix only in the engine. Rejected: the missing identity is a rendering fact;
  fixing it here means a distinguishing `item`/`question` flows into the store's
  `content_hash` and the false Exact Matches dissolve with no engine change.

## Consequences

`normalise_column_facts()` now records `column_position` from `columnOrder`
(falling back to list order), so Response Column ID Rendering can resolve a grid
direction across the columns. Only duplicated non-blank column text and blank
row labels change;
distinct columns and labelled rows render exactly as before, so existing SBS
snapshots are unaffected. The qualification is a best-effort disambiguation, not
a guarantee: where adjacency is genuinely ambiguous the rendered text is an
honest ordinal, and the engine's same-QID guard remains the backstop against any
residual same-survey, same-QID collapse.
