import { parseArgs } from "./lib/args.mts";
import { issueBranch } from "./lib/branches.mts";
import { resolveBlockers, unblockedIssues } from "./lib/blockers.mts";
import { currentBranch, assertIssuesReady, listIssues } from "./lib/github-issues.mts";
import { implementationPrompt, reviewPrompt } from "./lib/prompts.mts";
import { createRunId, createSummary, logPath, writeSummary } from "./lib/run-summary.mts";
import { runImplementation, runReview } from "./lib/sandcastle-runs.mts";

const options = parseArgs(process.argv.slice(2), "sequential-review");
const runId = createRunId("sequential-review", options.runId);
const summary = createSummary("sequential-review", runId);
writeSummary(summary);

try {
  const issues = listIssues(options);
  assertIssuesReady(issues, options.label);

  const issue =
    options.issues.length > 0
      ? issues[0]
      : unblockedIssues(issues).unblocked.sort((a, b) =>
          (a.createdAt ?? "").localeCompare(b.createdAt ?? ""),
        )[0];

  if (!issue) {
    summary.status = "blocked";
    summary.notes.push("No unblocked ready-for-agent issue found.");
    writeSummary(summary);
    console.log(`No unblocked issue found. Summary: ${summary.runDir}/summary.json`);
    process.exit(0);
  }

  const blockerStatus = resolveBlockers(issue);
  if (blockerStatus.blocked && !options.ignoreBlockers) {
    summary.status = "blocked";
    summary.issues.push({
      number: issue.number,
      title: issue.title,
      branch: issueBranch(issue.number),
      status: "blocked",
      commits: [],
      blockers: blockerStatus.openBlockers,
    });
    summary.notes.push("Issue has open or ambiguous blockers. Pass --ignore-blockers to override.");
    writeSummary(summary);
    throw new Error(`Issue #${issue.number} is blocked by ${blockerStatus.openBlockers.join(", ") || "ambiguous Blocked by text"}.`);
  }

  const branch = issueBranch(issue.number);
  summary.baseBranch = options.baseBranch ?? currentBranch();
  summary.issues.push({
    number: issue.number,
    title: issue.title,
    branch,
    status: "planned",
    commits: [],
    log: logPath(summary, `issue-${issue.number}`),
    reviewLog: logPath(summary, `review-${issue.number}`),
  });
  writeSummary(summary);

  if (options.dryRun) {
    summary.status = "dry-run";
    summary.notes.push("Dry run printed implementation and review prompts without running Codex.");
    writeSummary(summary);
    console.log(implementationPrompt(issue));
    console.log("\n--- Review Prompt ---\n");
    console.log(reviewPrompt(issue, branch, summary.baseBranch));
    process.exit(0);
  }

  const implementation = await runImplementation(issue, branch, summary.baseBranch, options, summary);
  const review = implementation.commits.length > 0
    ? await runReview(issue, branch, summary.baseBranch, options, summary)
    : undefined;

  summary.issues[0]!.status = implementation.commits.length > 0 ? "completed" : "skipped";
  summary.issues[0]!.commits = [
    ...implementation.commits.map((commit) => commit.sha),
    ...(review?.commits.map((commit) => commit.sha) ?? []),
  ];
  summary.status = "completed";
  writeSummary(summary);

  console.log(`Sandcastle sequential review run complete. Summary: ${summary.runDir}/summary.json`);
} catch (error) {
  summary.status = "failed";
  summary.notes.push(error instanceof Error ? error.message : String(error));
  writeSummary(summary);
  throw error;
}
