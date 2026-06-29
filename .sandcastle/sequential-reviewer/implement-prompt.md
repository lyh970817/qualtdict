# Context

## Open issues

!`node_modules/.bin/tsx .sandcastle/list-ready-issues.mts`

The list above has already been filtered to issues with the `ready-for-agent`
label and is the sole source of truth for what work exists. Do not run your
own unfiltered query to find more issues — if the list is empty, there is
nothing to do.

Each issue includes:

- `hasSubIssues`: true when GitHub reports this issue has sub-issues.
- `parentIssueId`: the GitHub parent issue number when this issue is a
  sub-issue.
- `linkedImplementationIssueIds`: other ready issues whose body or comments
  link to this issue as their parent PRD.

## Recent commits (last 10)

!`git log --oneline -10`

# Task

You are an autonomous coding agent working through issues one at a time.

## Priority order

Work on issues in this order:

1. **Bug fixes** — broken behaviour affecting users
2. **Tracer bullets** — thin end-to-end slices that prove an approach works
3. **Polish** — improving existing functionality (error messages, UX, docs)
4. **Refactors** — internal cleanups with no user-visible change

Pick the highest-priority open issue that is directly actionable and not
blocked by another open issue.

Treat a PRD container as context, not implementation work. Do not select a PRD
container, even if it has `ready-for-agent` and appears unblocked.

An issue is a PRD container if either condition is true:

- `hasSubIssues` is true.
- The issue appears to be a PRD or parent/tracking issue and
  `linkedImplementationIssueIds` is not empty.

Child implementation issues remain valid candidates. Use their parent PRD only
as context.

After selecting the issue, write the selected issue number and title to
`.sandcastle/selected-issue.json` for the later publisher phase. Do not commit
that file.

## Workflow

1. **Explore** — read the issue carefully. Pull in the parent PRD if referenced. Read the relevant source files and tests before writing any code.
2. **Plan** — decide what to change and why. Keep the change as small as possible.
3. **Execute** — use RGR (Red → Green → Repeat → Refactor): write a failing test first, then write the implementation to pass it.
4. **Verify** — run `Rscript -e 'devtools::test()'` and the strongest practical R package checks for the change before committing. Fix any failures before proceeding.
5. **Commit** — make a single git commit. The message MUST:
   - Use a short imperative subject consistent with the repository's recent commits
   - Include the task completed and any PRD reference
   - List key decisions made
   - List files changed
   - Note any blockers for the next iteration
6. **Hand off** — leave the issue open. A later publisher phase will push the
   reviewed branch, open a PR, and merge that PR with a closing keyword.

## Rules

- Work on **one issue per iteration**. Do not attempt multiple issues in a single iteration.
- Do not close an issue until you have committed the fix and verified tests pass.
- Do not leave commented-out code or TODO comments in committed code.
- Never implement or close a PRD container. If all remaining issues are PRD
  containers or blocked, leave them open and output the completion signal.
- Do not push branches, open PRs, merge PRs, or close issues in this phase.
- If you are blocked (missing context, failing tests you cannot fix, external dependency), leave a comment on the issue and move on — do not close it.

# Done

When all actionable issues are complete (or you are blocked on all remaining ones), or the open-issues block at the top of this prompt is empty, output the completion signal:

<promise>COMPLETE</promise>
