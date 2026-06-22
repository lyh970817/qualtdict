import type { Issue } from "./github-issues.mts";

export const implementationPrompt = (issue: Issue): string => `You are running as a Sandcastle Codex implementation agent for GitHub issue #${issue.number}.

Follow the repository instructions in AGENTS.md. Before editing, read CONTEXT.md,
docs/agents/*.md, and any ADRs relevant to the work.

Operational contract:
- The GitHub issue body below is authoritative.
- Keep the implementation narrowly scoped to this one issue.
- Use the repo's existing R package style and helper patterns.
- Run the strongest practical verification for the change.
- Do not require live Qualtrics credentials for verification.
- Prefer fixture-backed, snapshot, and local package tests.
- API-dependent tests must use existing vcr cassettes or recorded fixtures.
- If live credentials are required to verify fully, run the strongest local non-live verification and report the gap.
- When implementation and verification are complete, create local git commits.
- Use multiple commits when there are distinct user-facing changes.
- Do not push, open a PR, close the issue, comment on the issue, change labels, or edit unrelated local planning files.
- Do not commit secrets, private Qualtrics data, .Renviron, .Rhistory, .RData, .Rds, node_modules, or Sandcastle run artifacts.
- End your final response with the exact completion signal: <promise>COMPLETE</promise>

Issue:
- Number: #${issue.number}
- Title: ${issue.title}
- URL: ${issue.url}

## Issue Body

${issue.body || "(empty)"}
`;

export const reviewPrompt = (issue: Issue, branch: string, baseBranch: string): string => `You are reviewing Sandcastle implementation work for GitHub issue #${issue.number}.

Follow AGENTS.md and this repo's code-review standards. Review the branch diff and make small corrective commits directly on branch ${branch} only when needed.

Do not push, open a PR, close the issue, comment on the issue, change labels, or edit unrelated local planning files.

Issue:
- Number: #${issue.number}
- Title: ${issue.title}
- URL: ${issue.url}

Base branch: ${baseBranch}
Implementation branch: ${branch}

Inspect:
- git diff ${baseBranch}...${branch}
- git log ${baseBranch}..${branch} --oneline

Review focus:
- Correctness against the issue body and acceptance criteria.
- Missing or weak tests for changed behavior.
- Regressions, unsafe assumptions, or accidental scope creep.
- Consistency with the R package's existing style and helper patterns.
- No verification path requiring live Qualtrics credentials.

If the branch is already good, make no changes. If you make changes, run focused non-live verification and commit them.

End your final response with the exact completion signal: <promise>COMPLETE</promise>
`;

export const plannerPrompt = (issues: Issue[], branchFor: (issue: Issue) => string): string => `You are planning a Sandcastle batch for this repository.

The issues below have already been filtered to open ready-for-agent issues with no open blockers from their ## Blocked by sections.

<issues-json>
${JSON.stringify(
  issues.map((issue) => ({
    number: issue.number,
    title: issue.title,
    url: issue.url,
    branch: branchFor(issue),
    body: issue.body,
  })),
  null,
  2,
)}
</issues-json>

Choose the largest conservative set of issues that can safely run concurrently now.

Do not include two issues in the same wave if:
- they likely edit the same files, tests, snapshots, fixtures, generated docs, package metadata, NAMESPACE, shared helpers, or CI/config;
- one issue depends on API shape, code, tests, fixtures, or generated artifacts the other will establish;
- the issue bodies imply ordering uncertainty.

Be conservative. If overlap is unclear, include only the earlier or simpler issue and leave the other for a later wave.
Use the exact branch value supplied for each issue.

Output a JSON object wrapped in <plan> tags:
<plan>
{"issues":[{"number":42,"title":"Fix auth bug","branch":"sandcastle/issue-42"}]}
</plan>

Always emit <plan> tags. If there are no safe issues, output <plan>{"issues":[]}</plan>.
`;
