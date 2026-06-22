# Sandcastle

This repo uses Sandcastle to run local Codex agents against GitHub issues
created by the project issue workflow.

The Sandcastle sandbox provider is `noSandbox()`. Sandcastle creates git
worktrees for named branches, while Codex handles edit permissions through its
own sandbox flags.

## Issue Contract

Sandcastle reads the full GitHub issue body directly. Issues produced by
`$to-issues` are expected to contain:

- `## What to build`
- `## Acceptance criteria`
- `## Blocked by`

The runners do not require or extract an `## Agent Brief`.

By default, candidate issues must be open and labelled `ready-for-agent`.
Use `--label <label>` to select a different lane.

## Commands

Run the oldest unblocked ready issue, or a specific issue:

```sh
npm run sandcastle
npm run sandcastle -- --issue 42
```

Run one issue and then review the same branch:

```sh
npm run sandcastle:sequential-review -- --issue 42
```

Plan one safe parallel wave from the ready queue:

```sh
npm run sandcastle:parallel
```

Plan from an explicit candidate set:

```sh
npm run sandcastle:parallel -- --issues 42,43,44
```

Complete all explicitly listed issues in dependency-safe waves:

```sh
npm run sandcastle:parallel -- --issues 42,43,44 --schedule all
```

Use the review variant for per-branch review after implementation:

```sh
npm run sandcastle:parallel-review -- --issues 42,43,44
npm run sandcastle:parallel-review -- --issues 42,43,44 --schedule all
```

## Blockers

The runners parse the `## Blocked by` section. `None - can start immediately`
means unblocked. References like `#42` are treated as blockers while the
referenced issue is open.

Single issue modes refuse blocked issues by default. Use `--ignore-blockers`
only for deliberate single-issue overrides:

```sh
npm run sandcastle -- --issue 42 --ignore-blockers
```

Parallel modes do not support `--ignore-blockers`.

## Planning

Parallel modes first remove blocked issues deterministically, then ask a
read-only Codex planner to choose the next safe wave. Planner output is parsed
from `<plan>` tags and validated with Zod.

Use `--dry-run` to print prompt inputs without running agents. Use `--plan-only`
to run the planner and stop before implementation.

## Branches

Ordinary modes use deterministic issue branches:

```text
sandcastle/issue-42
```

Scheduled batch mode uses a local integration branch plus batch-scoped issue
branches:

```text
sandcastle/batch/<run-id>
sandcastle/batch/<run-id>/issue-42
```

Later waves start from the updated integration branch, so dependent issue
branches include earlier wave changes in their history.

## Side Effects

Default behavior:

- create local branches/worktrees;
- create local commits;
- write `.sandcastle/runs/<run-id>/summary.json` and logs;
- do not push;
- do not open PRs;
- do not close, comment on, or relabel GitHub issues.

Generated run artifacts are ignored by git.
