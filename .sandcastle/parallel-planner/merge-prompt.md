# TASK

Publish the following completed branches as pull requests, then merge the pull
requests through GitHub:

{{BRANCHES}}

Branch-to-issue mapping:

{{ISSUE_BRANCHES}}

For each branch:

1. Inspect the branch diff against the current base branch.
2. Run `Rscript -e 'devtools::test()'` for that branch before publishing it.
   Do not run `devtools::check()` manually; R CMD check belongs to the
   configured pre-push hook and should run during `git push`.
3. If tests or the pre-push hook fail, fix the issue on the branch, commit the
   fix, and rerun the relevant check before proceeding.
4. Push the branch with `git push -u origin <branch>`.
5. Open a pull request with `gh pr create`, targeting the current base branch.
   Include a closing keyword such as `Fixes #<ID>` in the PR body so GitHub
   closes the issue when the PR merges.
6. If a PR already exists for the branch, update/reuse it instead of creating a
   duplicate.
7. Before merging, verify `gh pr view <PR> --json closingIssuesReferences`
   includes the intended issue. If not, fix the PR body before merging.
8. Merge the PR with `gh pr merge --merge --delete-branch`.

If local finalization smoke artifacts are available and the branch changes
exported behavior, run the smoke script as one self-contained invocation for the
relevant finalization surface. Choose `--functions` from the smoke-covered
exported outputs whose behavior could change. The script runs required
prerequisites internally, so do not broaden `--functions` merely because a
prerequisite such as `dict_generate()` runs to produce a downstream output.

Use the default `question_name` Variable Dictionary route. The local
finalization smoke script disables the Semantic Name route because it is too
expensive for the publication workflow. Do not pass `--variable-name
semantic_name` or `--variable-name all`; changes that affect Semantic Name
behavior should be covered by ordinary tests and package checks instead.
For example:

`Rscript tools/local-finalize-smoke.R check --functions dict_generate --variable-name question_name`

Inspect the terminal output and saved RDS object artifacts under
`.local/finalize-smoke/runs/<timestamp>/`; temporary uncommitted R code is
acceptable for local inspection. Smoke runs can take several minutes. Wait with
a longer timeout, do not repeatedly poll the process, and inspect output once
the smoke command exits before treating the agent as idle. Missing artifacts
are not a failure; report that the smoke check could not be run.

Do not merge branches locally into the base branch. The PR merge is the merge.
After each PR merge, update the local base branch with `git pull --ff-only`.

# CLOSE ISSUES

Do not close issues directly with `gh issue close`. Issues should close through
the merged PR's closing keyword.

If merging a child PR completes a parent PRD, add a closing keyword for that
parent PRD to the child PR body before merging it. Only add that parent PRD
closing keyword after checking that all linked implementation issues for that
PRD are closed or were merged by PR in this run. Do not close an unfinished
parent PRD merely because one child issue completed, and do not close parent
PRDs directly.

Here are all the issues:

{{ISSUES}}

If GitHub refuses to merge a PR because required checks or permissions are
missing, leave the PR open and report the PR URL and blocker. Do not merge the
branch locally as a fallback.

Once you've published and merged everything you can, output
<promise>COMPLETE</promise>.
