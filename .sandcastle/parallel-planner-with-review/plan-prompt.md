# ISSUES

Here are the open issues in the repo:

<issues-json>

!`node_modules/.bin/tsx .sandcastle/list-ready-issues.mts`

</issues-json>

The list above has already been filtered to issues with the `ready-for-agent`
label. Each issue also includes issue-shape fields:

- `hasSubIssues`: true when GitHub reports this issue has sub-issues.
- `parentIssueId`: the GitHub parent issue number when this issue is a
  sub-issue.
- `linkedImplementationIssueIds`: other ready issues whose body or comments
  link to this issue as their parent PRD.

# TASK

Analyze the open issues and build a dependency graph. For each issue, determine whether it **blocks** or **is blocked by** any other open issue.

An issue B is **blocked by** issue A if:

- B requires code or infrastructure that A introduces
- B and A modify overlapping files or modules, making concurrent work likely to produce merge conflicts
- B's requirements depend on a decision or API shape that A will establish

An issue is **unblocked** if it has zero blocking dependencies on other open issues.

For each unblocked issue, assign a branch name using the exact format `sandcastle/issue-{id}` (no slug or other suffix). This must be deterministic so that re-planning the same issue always produces the same branch name and accumulated progress is preserved.

## PRD containers

Treat a PRD container as context, not implementation work. Do not include a PRD
container in the plan, even if it has `ready-for-agent` and appears unblocked.

An issue is a PRD container if either condition is true:

- `hasSubIssues` is true.
- The issue appears to be a PRD or parent/tracking issue and
  `linkedImplementationIssueIds` is not empty.

Child implementation issues remain valid candidates. Use their parent PRD only
as context.

# OUTPUT

Output your plan as a JSON object wrapped in `<plan>` tags:

<plan>
{"issues": [{"id": "42", "title": "Fix auth bug", "branch": "sandcastle/issue-42"}]}
</plan>

Include only unblocked directly actionable issues. If every directly actionable
issue is blocked, include the single highest-priority directly actionable
candidate (the one with the fewest or weakest dependencies). Never use this
fallback to include a PRD container.

Always emit the `<plan>` tags, even when there is nothing to do. If there are no issues to work on at all, output `<plan>{"issues": []}</plan>` so the run can exit cleanly.
