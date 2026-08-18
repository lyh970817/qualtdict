# Design: Pluggable Semantic Name Strategies (RAKE + LLM)

Status: draft for review — written 2026-08-17, before implementation.
Builds on the canonical-schema-refactor branch (post ADR 0010, post
Suggests move of the Semantic Name stack). Implementation is expected
to happen in a fresh session using this document as the specification.

## Purpose

Allow the Semantic Name capability to source its name *proposals* from
an LLM instead of (or in addition to) the built-in RAKE keyword
algorithm, without qualtdict learning anything about LLM providers,
keys, or networking — and without weakening any existing identity,
uniqueness, or reproducibility invariant.

## Glossary alignment (CONTEXT.md)

A **Semantic Name** remains what CONTEXT.md says it is: a best-effort
Dictionary Variable Name generated from survey text, block information,
and response metadata; a readable convenience, not a stability
guarantee. This design does not change that contract. New term
introduced here: **Naming Strategy** — the pluggable component that
turns naming texts into name *stems*; everything after the stem
(suffixes, sanitisation, uniqueness) is not the strategy's business.

## Current state (verified on the branch)

The seam already exists and is the product of the Phase 3 refactor:

- One call site enters the subsystem: `variable_dictionary.R` calls
  `generate_semantic_names()` only when
  `variable_name = "semantic_name"` (off by default).
- `R/semantic_name.R` pipeline (current line refs):
  `generate_semantic_names` (:27) → `semantic_name_texts` (:73, picks
  the per-row naming text: item → question → MA label → SBS fallback)
  → `semantic_name_keywords` (:94, RAKE via `slowrake`, cached) →
  `add_semantic_name_components` (:165) →
  `semantic_question_components` (:194) + `semantic_block_components`
  (:211) → `add_semantic_name_suffixes` (:222; matrix `.label`, SBS
  `.item`, loop `.loop_option`, `.txt`) →
  `semantic_name_label_suffix` (:283, charset sanitisation).
- Content-addressed cache: `semantic_name_cache_path` (:149) keys an
  RDS on `rlang::hash(list(<algorithm version>, cleaned texts,
  corpus))` under `semantic_name_cache_dir()` (:129; option →
  `QUALTDICT_SEMANTIC_CACHE_DIR` → `tempdir()`).
- Optional-capability guards: `check_semantic_name_available()` (:9)
  with the mockable `semantic_name_package_available()` (:3);
  slowraker/SnowballC/stringi/tidyr are Suggests; `slowraker_internal`
  in `R/slowraker.R` resolves lazily.
- Uniqueness is enforced unconditionally *downstream* by
  `repair_variable_dictionary_names()` (`R/variable_name.R`), which
  sanitises the charset, applies `make.unique`, and records
  `variable_name_findings`. Nothing in this design may bypass it.

## Goals

1. `dict_generate()` accepts a Naming Strategy; RAKE remains the
   default and the behaviour of existing code is byte-identical when
   no strategy is supplied.
2. An LLM strategy exists behind `Suggests: ellmer`, provider-agnostic
   (the user passes a constructed `ellmer` chat object — Anthropic,
   OpenAI, Gemini, Bedrock, or local Ollama — so qualtdict never
   touches keys, endpoints, or retries).
3. Reproducibility via the existing content-addressed cache: an LLM is
   consulted at most once per (strategy fingerprint, texts) — later
   runs are offline and byte-stable.
4. Uniqueness stays mechanical and deterministic.

## Non-goals

- No new hard dependency; nothing moves out of Suggests.
- No change to structural suffixes, `variable_name` repair,
  Validation Findings, or any non-semantic code path.
- No promise of cross-model or cross-version name stability (the
  documented contract already disclaims this).
- The "committed mapping as a Decision-style file" idea is deferred to
  a possible Phase C (see end); it is attractive but separable.

## Design

### The strategy contract

A Naming Strategy is an object created by a constructor, carrying a
fingerprint for caching and a propose function:

```r
new_semantic_name_strategy <- function(id, fingerprint, propose) {
  structure(
    list(id = id, fingerprint = fingerprint, propose = propose),
    class = "qualtdict_semantic_name_strategy"
  )
}
```

- `propose(texts, context)` receives the **unique cleaned naming
  texts** (exactly what `semantic_name_keywords()` receives today) and
  a `context` tibble with one row per unique text: `text`,
  `question_name`, `survey_block`, `n_rows` (how many dictionary rows
  share the text). It returns a character vector of snake_case name
  **stems**, same length and order as `texts`. `NA` means "no
  proposal" and falls back to the cleaned text, mirroring today's
  RAKE fallback for short/keyword-less texts.
- The stem is the *only* thing a strategy produces. Block components
  (`block_pattern`/`block_sep`), structural suffixes, charset
  sanitisation, and uniqueness all remain in the existing pipeline,
  applied after the stems come back.
- A plain function supplied by the user is auto-wrapped
  (`id = "custom"`, fingerprint = `rlang::hash(deparse(fn))`; document
  that closures over external state defeat the cache key).

### API change

```r
dict_generate(
  surveyID,
  variable_name = c("question_name", "semantic_name"),
  semantic_name_strategy = rake_naming_strategy(),
  block_pattern = NULL,
  block_sep = ".",
  semantic_name_preprocess = NULL,
  embedded_data_block_assignment = c("none", "previous", "next"),
  quiet = TRUE
)
```

- `semantic_name_strategy` is validated only when
  `variable_name = "semantic_name"` (like the other semantic-only
  arguments; roxygen documents it as semantic-only).
- Lazy default: constructing `rake_naming_strategy()` must not touch
  slowraker (the propose closure resolves lazily, matching the
  existing `slowraker_internal` pattern), so the default argument is
  safe to evaluate on the question_name path.
- `check_semantic_name_available()` becomes strategy-aware: the RAKE
  strategy requires slowraker/SnowballC/stringi/tidyr as today; the
  LLM strategy requires ellmer (plus stringi/tidyr, still used by the
  shared component/suffix pipeline).
- `semantic_name_preprocess` is untouched: it runs on the dictionary
  rows before text selection, for every strategy.

### `rake_naming_strategy()`

A thin re-packaging of the current code path: `propose` calls
`semantic_name_keywords()` + `semantic_question_components()`.
Fingerprint = the existing algorithm-version string already used in
the cache key. With this strategy the produced names — and the cache
keys — must be identical to today's output (regression-tested against
the existing test-semantic_name.R expectations).

### `llm_naming_strategy(chat, prompt_version = "v1", style = NULL)`

- `chat` is a user-constructed ellmer chat object. Its model identity
  (`chat$get_model()`) joins `prompt_version` and `style` in the
  fingerprint.
- One **batched structured call** per survey (chunked if very large):
  the prompt lists every unique text with its `question_name` and
  `survey_block`, and requests, via ellmer structured output
  (`type_array` of `type_object(text_id, stem)`), snake_case stems
  that are: lowercase `[a-z0-9_]`, at most ~4 words, informative, and
  **unique across the batch** — with the survey-wide sibling view
  encouraging consistent schemes (e.g. `phq9_interest`,
  `phq9_sleep`), which is the actual advantage over RAKE.
- Determinism knobs: request temperature 0 where the backend supports
  it; but correctness never depends on it — the cache is what makes
  reruns stable.
- Post-validation in package code (never trust the model): coerce
  through the same sanitisation used today; empty/invalid stems
  become `NA` (→ cleaned-text fallback).

### Uniqueness and collision policy (decided)

Propose → validate → one bounded repair round → mechanical fallback:

1. The batch prompt asks for unique stems (cheap, usually sufficient).
2. If validated stems collide, at most **one** follow-up structured
   call: only the colliding texts, shown alongside their neighbours'
   accepted stems, asking for distinct alternatives.
3. Anything still colliding proceeds unchanged; the existing
   downstream machinery (structural suffixes, then
   `repair_variable_dictionary_names()` / `make.unique`) guarantees
   final uniqueness exactly as it does for RAKE today, and records the
   repair in `variable_name_findings`.

Explicitly rejected: recompute-until-unique loops (unbounded,
nondeterministic) and any LLM involvement in structural suffixes
(suffixes encode survey structure, not semantics).

### Caching (decided)

Extend `semantic_name_cache_path()` to key on
`hash(list(strategy$fingerprint, cleaned_unique_texts, corpus))`,
where `corpus` participates only for strategies that use it (RAKE
does; the LLM strategy keys on texts + fingerprint alone). Behavioural
consequences:

- First semantic run with an LLM strategy needs the backend; every
  later run with the same fingerprint and texts is offline and
  byte-stable. The cache RDS is the reproducibility artifact; the
  existing cache-dir option/env-var lets a project pin it somewhere
  durable (documented in the vignette).
- RAKE cache keys change shape (fingerprint replaces the bare version
  string): a one-time cold cache on upgrade, no behaviour change.
  Verify the "cache key includes scoring corpus" test still holds.

## Testing plan

- The strategy contract makes core tests LLM-free: inject a
  deterministic fake strategy (`new_semantic_name_strategy` with a
  closure) to cover wiring, ordering, NA fallback, collision repair
  round-trip, and cache keying — no ellmer needed.
- `llm_naming_strategy()` unit tests use a stub chat object (an object
  mimicking the two or three ellmer methods actually touched), plus
  `skip_if_not_installed("ellmer")` guards, mirroring the existing
  `semantic_name_package_available()` mocking pattern.
- Regression: `rake_naming_strategy()` output identical to current
  semantic snapshots; the default question_name path never evaluates
  any Suggests package.
- No test, example, or vignette may call a live LLM API (CRAN/rOpenSci
  constraint). Vignette LLM chunks use `eval = FALSE`.

## Documentation and privacy

- roxygen for `dict_generate` and both constructors; vignette section
  showing `chat_ollama()` (offline, keyless) first and a hosted
  backend second.
- Privacy note (required): with a hosted backend, survey **metadata**
  (question texts) leaves the machine. This does not touch the
  participant-data boundary (responses are never involved in naming),
  but the docs must say so explicitly and point at the Ollama route
  for anyone whose surveys are themselves sensitive.
- README "not a stable Semantic Name generator" framing stays; add one
  sentence that stability within a project comes from the cache.

## Repo constraints checklist (for the implementing session)

- 80-column source limit (ADR 0007); run air on changed files.
- Hooks run lintr, testthat, goodpractice (function length and
  cyclomatic-complexity caps bite — keep `generate_semantic_names`
  within the test-hygiene.R line cap; that test also hard-codes the
  literal path `R/semantic_name.R` and the name
  `generate_semantic_names`, so neither may be renamed or moved).
- Spelling hook: add ellmer/Ollama/LLM etc. to inst/WORDLIST as
  needed.
- New strategy code lives in `R/semantic_name.R` (or a sibling
  `R/semantic_name_strategy.R` — new file names are fine, existing
  ones are pinned). ellmer goes in Suggests, alphabetised.
- Stage only changed files; never git add -A (pre-existing deletions
  and untracked local files must stay out of commits).

## Implementation phases

- **Phase A (pure refactor, no new deps):** introduce
  `new_semantic_name_strategy()` + `rake_naming_strategy()`, thread
  `semantic_name_strategy` through `dict_generate()` →
  `variable_dictionary_from_normalised_metadata()` →
  `generate_semantic_names()`, extend the cache key, full suite green
  with identical semantic snapshots. Record as an ADR (strategy
  interface + cache-key change).
- **Phase B:** `llm_naming_strategy()` + stub-chat tests + docs +
  privacy note + WORDLIST. `Suggests: ellmer`.
- **Phase C (deferred, separate decision):** graduate the cached
  text→name mapping into a committed, human-editable Decision-style
  file that, when present, overrides any strategy. Decide only after
  Phase B usage shows whether the cache alone suffices.

## Open questions (defaults chosen; overridable at implementation)

1. Chunk size for the batched call on very large surveys — default:
   single call up to ~200 unique texts, then chunk with sibling-name
   context carried between chunks.
2. Should `style` accept a free-text instruction (e.g. "prefix by
   instrument abbreviation")? Default: yes, a single optional string
   folded into the prompt and the fingerprint.
3. Fingerprint for user-supplied bare functions — default:
   `hash(deparse(fn))` with a documented caveat, rather than refusing
   to cache.
