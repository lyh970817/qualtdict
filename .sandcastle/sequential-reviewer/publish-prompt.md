# TASK

Publish reviewed branch `{{BRANCH}}` as a pull request, then merge that pull
request through GitHub.

# ISSUE CONTEXT

Read `.sandcastle/selected-issue.json` to identify the issue this branch
implements. If that file is missing, inspect the branch commits and issue list
to infer the issue number. If you cannot identify exactly one issue, push the
branch and open a PR without a closing keyword, report the blocker, and do not
merge the PR.

# BASE BRANCH POLICY

The GitHub base branch is always `main`. The publisher may be running from a
dedicated runner branch such as `sandcastle/runner`; do not treat the current
checkout branch as the PR base, and never open PRs against the runner branch.

Use `origin/main` as the current base for diffs, rebases, and mergeability
checks. Before publishing or re-checking `{{BRANCH}}`, run
`git fetch origin main`. Open the PR with
`gh pr create --base main --head {{BRANCH}}`.

After the PR merge, update local base state without checking out local `main`.
If this checkout has a `sandcastle/runner` branch, switch back to it and
fast-forward it from `origin/main`:

`git switch sandcastle/runner && git fetch origin main && git merge --ff-only origin/main`

If this checkout is already on local `main` and no runner branch is being used,
`git pull --ff-only` is acceptable. Do not merge `{{BRANCH}}` into any local
base branch; the GitHub PR merge is the merge.

# WORKFLOW

1. Confirm you are on branch `{{BRANCH}}`.
2. Inspect the branch diff against `origin/main`.
3. Run `Rscript -e 'devtools::test()'` for the reviewed branch. Do not run
   `devtools::check()` manually; R CMD check belongs to the configured pre-push
   hook and should run during `git push`.
4. If tests or the pre-push hook fail, fix the issue on the branch, commit the
   fix, and rerun the relevant check before proceeding.
5. Push the branch with `git push -u origin {{BRANCH}}`.
6. Open a pull request with `gh pr create --base main --head {{BRANCH}}`.
   Include `Fixes #<ID>` in the PR body when the issue number is known so
   GitHub closes the issue when the PR merges.
7. If a PR already exists for the branch, update/reuse it instead of creating a
   duplicate.
8. If merging this PR completes a parent PRD, add a closing keyword for that
   parent PRD to the PR body before merging. Only add that parent PRD closing
   keyword after checking that all linked implementation issues for the PRD are
   closed or will be closed by this PR.
9. Before merging, verify `gh pr view <PR> --json closingIssuesReferences`
   includes the intended issue. If not, fix the PR body before merging.
10. Merge the PR with `gh pr merge --merge --delete-branch`.
11. After the PR merge, update the runner/base state as described in the base
    branch policy above.

# RULES

- Do not merge `{{BRANCH}}` locally into the base branch. The PR merge is the
  merge.
- Do not close issues directly with `gh issue close`. Issues should close
  through the merged PR's closing keyword.
- Do not close parent PRDs directly.
- If GitHub refuses to merge the PR because required checks or permissions are
  missing, leave the PR open and report the PR URL and blocker. Do not merge the
  branch locally as a fallback.

Once complete, output <promise>COMPLETE</promise>.
