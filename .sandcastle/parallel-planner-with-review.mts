import { parseArgs } from "./lib/args.mts";
import { batchBranch, createIntegrationWorktree, issueBranch, mergeIntoIntegration } from "./lib/branches.mts";
import { unblockedIssues } from "./lib/blockers.mts";
import { assertIssuesReady, currentBranch, listIssues, type Issue } from "./lib/github-issues.mts";
import { createRunId, createSummary, logPath, writeSummary } from "./lib/run-summary.mts";
import { runImplementation, runReview } from "./lib/sandcastle-runs.mts";
import { planWave, runLimited, type PlannedIssue } from "./lib/scheduler.mts";

const options = parseArgs(process.argv.slice(2), "parallel-review");
const runId = createRunId("parallel-review", options.runId);
const summary = createSummary("parallel-review", runId);
writeSummary(summary);

const plannedIssue = (planned: PlannedIssue, issues: Issue[]): Issue => {
  const issue = issues.find((candidate) => candidate.number === planned.number);
  if (!issue) throw new Error(`Planner selected unknown issue #${planned.number}.`);
  return issue;
};

const implementAndReview = async (
  issue: Issue,
  branch: string,
  baseBranch: string,
) => {
  const implementation = await runImplementation(issue, branch, baseBranch, options, summary);
  const review = implementation.commits.length > 0
    ? await runReview(issue, branch, baseBranch, options, summary)
    : undefined;

  return {
    commits: [
      ...implementation.commits.map((commit) => commit.sha),
      ...(review?.commits.map((commit) => commit.sha) ?? []),
    ],
  };
};

try {
  const allIssues = listIssues(options);
  assertIssuesReady(allIssues, options.label);
  summary.baseBranch = options.baseBranch ?? currentBranch();

  if (options.schedule === "all") {
    const integrationBranch = batchBranch(runId);
    summary.integrationBranch = integrationBranch;
    const integrationWorktree =
      options.dryRun || options.planOnly
        ? undefined
        : createIntegrationWorktree(summary.runDir, integrationBranch, summary.baseBranch);
    const completed = new Set<number>();
    let remaining = [...allIssues];

    for (let iteration = 1; iteration <= options.maxIterations! && remaining.length > 0; iteration += 1) {
      const { unblocked, statuses } = unblockedIssues(remaining, completed);
      summary.issues.push(
        ...statuses
          .filter(
            (status) =>
              status.blocked && !summary.issues.some((entry) => entry.number === status.issue.number),
          )
          .map((status) => ({
            number: status.issue.number,
            title: status.issue.title,
            branch: "",
            status: "blocked" as const,
            commits: [],
            blockers: status.openBlockers,
          })),
      );

      if (unblocked.length === 0) {
        summary.status = completed.size > 0 ? "blocked" : "failed";
        summary.notes.push("No unblocked issues remain for the scheduled batch.");
        writeSummary(summary);
        break;
      }

      const planned = await planWave(unblocked, options, summary, iteration, runId);
      if (options.dryRun || options.planOnly) {
        summary.status = options.dryRun ? "dry-run" : "plan-only";
        summary.notes.push(`Planned wave ${iteration}; implementation was not run.`);
        summary.issues.push(
          ...planned.map((issue) => ({
            number: issue.number,
            title: issue.title,
            branch: issue.branch,
            status: "planned" as const,
            commits: [],
          })),
        );
        writeSummary(summary);
        break;
      }

      if (planned.length === 0) {
        summary.status = "blocked";
        summary.notes.push(`Planner returned no safe issues for wave ${iteration}.`);
        writeSummary(summary);
        break;
      }

      const settled = await runLimited(planned, options.maxParallel, async (plannedIssueInfo) => {
        const issue = plannedIssue(plannedIssueInfo, remaining);
        return implementAndReview(issue, plannedIssueInfo.branch, integrationBranch);
      });

      let waveFailed = false;
      for (const [index, outcome] of settled.entries()) {
        const selected = planned[index]!;
        const issue = plannedIssue(selected, remaining);
        if (outcome.status === "rejected") {
          waveFailed = true;
          summary.issues.push({
            number: issue.number,
            title: issue.title,
            branch: selected.branch,
            status: "failed",
            commits: [],
            log: logPath(summary, `issue-${issue.number}`),
            reviewLog: logPath(summary, `review-${issue.number}`),
            error: String(outcome.reason),
          });
        } else {
          summary.issues.push({
            number: issue.number,
            title: issue.title,
            branch: selected.branch,
            status: outcome.value.commits.length > 0 ? "completed" : "skipped",
            commits: outcome.value.commits,
            log: logPath(summary, `issue-${issue.number}`),
            reviewLog: logPath(summary, `review-${issue.number}`),
          });
          if (outcome.value.commits.length > 0) {
            mergeIntoIntegration(integrationWorktree!, selected.branch);
          }
          completed.add(issue.number);
        }
      }

      writeSummary(summary);
      if (waveFailed) {
        summary.status = "failed";
        summary.notes.push(`Wave ${iteration} failed. Stopped scheduled batch.`);
        writeSummary(summary);
        break;
      }

      remaining = remaining.filter((issue) => !completed.has(issue.number));
    }

    if (remaining.length === 0 && summary.status === "running") {
      summary.status = "completed";
      writeSummary(summary);
    } else if (remaining.length > 0 && summary.status === "running") {
      summary.status = "blocked";
      summary.notes.push(`Stopped after ${options.maxIterations} wave(s) with ${remaining.length} issue(s) remaining.`);
      writeSummary(summary);
    }

    console.log(`Sandcastle scheduled parallel review run complete. Summary: ${summary.runDir}/summary.json`);
    process.exit(summary.status === "failed" ? 1 : 0);
  }

  const { unblocked, statuses } = unblockedIssues(allIssues);
  summary.issues.push(
    ...statuses
      .filter((status) => status.blocked)
      .map((status) => ({
        number: status.issue.number,
        title: status.issue.title,
        branch: issueBranch(status.issue.number),
        status: "blocked" as const,
        commits: [],
        blockers: status.openBlockers,
      })),
  );

  const planned = await planWave(unblocked, options, summary, 1);
  if (options.dryRun || options.planOnly) {
    summary.status = options.dryRun ? "dry-run" : "plan-only";
    summary.issues.push(
      ...planned.map((issue) => ({
        number: issue.number,
        title: issue.title,
        branch: issue.branch,
        status: "planned" as const,
        commits: [],
      })),
    );
    writeSummary(summary);
    console.log(`Sandcastle parallel review planning complete. Summary: ${summary.runDir}/summary.json`);
    process.exit(0);
  }

  if (planned.length === 0) {
    summary.status = "blocked";
    summary.notes.push("Planner returned no safe issues.");
    writeSummary(summary);
    console.log(`No safe parallel review issues found. Summary: ${summary.runDir}/summary.json`);
    process.exit(0);
  }

  const settled = await runLimited(planned, options.maxParallel, async (plannedIssueInfo) => {
    const issue = plannedIssue(plannedIssueInfo, unblocked);
    return implementAndReview(issue, plannedIssueInfo.branch, summary.baseBranch!);
  });

  let failed = false;
  for (const [index, outcome] of settled.entries()) {
    const selected = planned[index]!;
    const issue = plannedIssue(selected, unblocked);
    if (outcome.status === "rejected") {
      failed = true;
      summary.issues.push({
        number: issue.number,
        title: issue.title,
        branch: selected.branch,
        status: "failed",
        commits: [],
        log: logPath(summary, `issue-${issue.number}`),
        reviewLog: logPath(summary, `review-${issue.number}`),
        error: String(outcome.reason),
      });
    } else {
      summary.issues.push({
        number: issue.number,
        title: issue.title,
        branch: selected.branch,
        status: outcome.value.commits.length > 0 ? "completed" : "skipped",
        commits: outcome.value.commits,
        log: logPath(summary, `issue-${issue.number}`),
        reviewLog: logPath(summary, `review-${issue.number}`),
      });
    }
  }

  summary.status = failed ? "failed" : "completed";
  writeSummary(summary);
  console.log(`Sandcastle parallel review run complete. Summary: ${summary.runDir}/summary.json`);
  process.exit(failed ? 1 : 0);
} catch (error) {
  summary.status = "failed";
  summary.notes.push(error instanceof Error ? error.message : String(error));
  writeSummary(summary);
  throw error;
}
