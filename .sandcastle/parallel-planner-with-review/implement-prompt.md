# TASK

Fix issue {{TASK_ID}}: {{ISSUE_TITLE}}

Pull in the issue using `gh issue view {{TASK_ID}} --comments`. If it has a parent PRD, pull that in too.

Only work on the issue specified.

Before writing code, run `node_modules/.bin/tsx .sandcastle/list-ready-issues.mts`
and find issue {{TASK_ID}} in the JSON output. If it is a PRD container, do not
implement it, do not commit, and do not close it. Leave a comment explaining
that the issue appears to be a parent PRD/tracking issue with implementation
issues, then output <promise>COMPLETE</promise>.

An issue is a PRD container if either condition is true:

- `hasSubIssues` is true.
- The issue appears to be a PRD or parent/tracking issue and
  `linkedImplementationIssueIds` is not empty.

Work on branch {{BRANCH}}. Make commits and run tests.

# CONTEXT

Here are the last 10 commits:

<recent-commits>

!`git log -n 10 --format="%H%n%ad%n%B---" --date=short`

</recent-commits>

# EXPLORATION

Explore the repo and fill your context window with relevant information that will allow you to complete the task.

Pay extra attention to test files that touch the relevant parts of the code.

# EXECUTION

If applicable, use RGR to complete the task.

1. RED: write one test
2. GREEN: write the implementation to pass that test
3. REPEAT until done
4. REFACTOR the code

# FEEDBACK LOOPS

Before committing, run `Rscript -e 'devtools::test()'` and the strongest practical R package checks for the change. For exported behavior, prefer `Rscript -e 'devtools::check()'`; for narrow changes, focused `testthat` files are acceptable if you explain the smaller verification scope.

# COMMIT

Make a git commit. The commit message must:

1. Use a short imperative subject consistent with the repository's recent commits
2. Include task completed + PRD reference
3. Key decisions made
4. Files changed
5. Blockers or notes for next iteration

Keep it concise.

# THE ISSUE

If the task is not complete, leave a comment on the issue with what was done.

Do not close the issue directly. A later publisher phase will open and merge a
PR with a closing keyword.

Once complete, output <promise>COMPLETE</promise>.

# FINAL RULES

ONLY WORK ON A SINGLE TASK.
