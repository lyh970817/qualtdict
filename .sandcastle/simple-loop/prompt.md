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

## Base branch policy

The GitHub base branch is always `main`. This workflow may run from a dedicated
runner branch such as `sandcastle/runner`; do not treat the current checkout
branch as the PR base, and never open PRs against the runner branch.

Before creating or updating an issue branch, run `git fetch origin main`. Start
new issue branches from `origin/main`, and use `origin/main` for diffs, rebases,
and mergeability checks. Open PRs with
`gh pr create --base main --head sandcastle/issue-<ID>`.

After a PR merge, update local base state without checking out local `main`. If
this checkout has a `sandcastle/runner` branch, switch back to it and
fast-forward it from `origin/main`:

`git switch sandcastle/runner && git fetch origin main && git merge --ff-only origin/main`

If this checkout is already on local `main` and no runner branch is being used,
`git pull --ff-only` is acceptable. Do not merge issue branches into any local
base branch; the GitHub PR merge is the merge.

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

## Workflow

1. **Explore** — read the issue carefully. Pull in the parent PRD if referenced. Read the relevant source files and tests before writing any code.
2. **Plan** — decide what to change and why. Keep the change as small as possible.
3. **Execute** — use RGR (Red → Green → Repeat → Refactor): write a failing test first, then write the implementation to pass it.
4. **Verify** — run `Rscript -e 'devtools::test()'` before committing. Fix any failures before proceeding. Do not run `devtools::check()` manually; R CMD check belongs to the configured pre-push hook.
5. **Commit** — make a single git commit. The message MUST:
   - Use a short imperative subject consistent with the repository's recent commits
   - Include the task completed and any PRD reference
   - List key decisions made
   - List files changed
   - Note any blockers for the next iteration
6. **Publish** — push the issue branch, open a pull request, and merge that
   pull request. Use a PR body with a closing keyword such as `Fixes #<ID>` so
   GitHub closes the issue when the PR merges. Do not close issues directly.

## Rules

- Work on **one issue per iteration**. Do not attempt multiple issues in a single iteration.
- Do not close an issue until you have committed the fix and verified tests pass.
- Do not leave commented-out code or TODO comments in committed code.
- Never implement or close a PRD container. If all remaining issues are PRD
  containers or blocked, leave them open and output the completion signal.
- Before editing files, create or check out a deterministic issue branch named
  `sandcastle/issue-<ID>`. Do not work directly on the runner or base branch.
- After verification succeeds, run `git push -u origin sandcastle/issue-<ID>`.
- Open a PR against `main` with `gh pr create --base main --head
  sandcastle/issue-<ID>`. If a PR already exists for the branch, update/reuse it
  instead of creating a duplicate.
- If merging this PR completes a parent PRD, add a closing keyword for that
  parent PRD to the PR body before merging. Only add that parent PRD closing
  keyword after checking that all linked implementation issues for the PRD are
  closed or will be closed by this PR.
- Merge the PR with `gh pr merge --merge --delete-branch`. If GitHub refuses
  the merge because required checks or permissions are missing, leave the PR
  open, comment on the issue with the PR URL and blocker, and do not close the
  issue directly.
- If you are blocked (missing context, failing tests you cannot fix, external dependency), leave a comment on the issue and move on — do not close it.

# Done

When all actionable issues are complete (or you are blocked on all remaining ones), or the open-issues block at the top of this prompt is empty, output the completion signal:

<promise>COMPLETE</promise>
