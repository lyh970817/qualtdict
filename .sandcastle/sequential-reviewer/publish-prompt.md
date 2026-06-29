# TASK

Publish reviewed branch `{{BRANCH}}` as a pull request, then merge that pull
request through GitHub.

# ISSUE CONTEXT

Read `.sandcastle/selected-issue.json` to identify the issue this branch
implements. If that file is missing, inspect the branch commits and issue list
to infer the issue number. If you cannot identify exactly one issue, push the
branch and open a PR without a closing keyword, report the blocker, and do not
merge the PR.

# WORKFLOW

1. Confirm you are on branch `{{BRANCH}}`.
2. Inspect the branch diff against the current base branch.
3. Run `Rscript -e 'devtools::test()'` and the strongest practical R package
   checks for the reviewed branch.
4. If tests fail, fix the issue on the branch, commit the fix, and rerun the
   relevant checks before proceeding.
5. Push the branch with `git push -u origin {{BRANCH}}`.
6. Open a pull request with `gh pr create`, targeting the current base branch.
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
11. After the PR merge, update the local base branch with `git pull --ff-only`.

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
