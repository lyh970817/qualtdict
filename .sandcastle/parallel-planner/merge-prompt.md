# TASK

Publish the following completed branches as pull requests, then merge the pull
requests through GitHub:

{{BRANCHES}}

Branch-to-issue mapping:

{{ISSUE_BRANCHES}}

For each branch:

1. Inspect the branch diff against the current base branch.
2. Run `Rscript -e 'devtools::test()'` and the strongest practical R package
   checks for that branch before publishing it.
3. If tests fail, fix the issue on the branch, commit the fix, and rerun the
   relevant checks before proceeding.
4. Push the branch with `git push -u origin <branch>`.
5. Open a pull request with `gh pr create`, targeting the current base branch.
   Include a closing keyword such as `Fixes #<ID>` in the PR body so GitHub
   closes the issue when the PR merges.
6. If a PR already exists for the branch, update/reuse it instead of creating a
   duplicate.
7. Merge the PR with `gh pr merge --merge --delete-branch`.

If local finalization smoke artifacts are available and the branch changes
exported behavior, run the smoke script as one self-contained invocation for the
relevant finalization surface. Choose `--functions` from the smoke-covered
exported outputs whose behavior could change. The script runs required
prerequisites internally, so do not broaden `--functions` merely because a
prerequisite such as `dict_generate()` runs to produce a downstream output.

Choose `--variable-name` from the naming-route dependency of the changed code,
not from the selected downstream output alone. Use the default `question_name`
route for changes that do not affect Semantic Name generation, shared naming
inputs, or route-specific Dictionary Variable Name behavior. For example,
changes to validation, Labelled Export, or block splitting should not use
`--variable-name all` unless the changed code actually depends on both naming
routes. Prefer reproducible two-survey sampling while iterating, for example:

`Rscript tools/local-finalize-smoke.R check --survey-seed 123 --functions dict_generate --variable-name question_name`

Select the Semantic Name route only when Semantic Name behavior or shared naming
behavior is relevant:

`Rscript tools/local-finalize-smoke.R check --survey-seed 123 --functions dict_generate --variable-name semantic_name`

Use `--variable-name all` only when both naming routes are relevant. Use
`--survey-count all` only when the code change really needs every configured
survey, and explain why. Inspect the terminal output and saved RDS object
artifacts under `.local/finalize-smoke/runs/<timestamp>/`; temporary
uncommitted R code is acceptable for local inspection.
Smoke runs can take several minutes, especially with Semantic Name generation
or many surveys selected. Wait with a longer timeout, do not repeatedly poll the
process, and inspect output once the smoke command exits before treating the
agent as idle.

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
