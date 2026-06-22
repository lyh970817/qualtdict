import { Output, run } from "@ai-hero/sandcastle";
import { noSandbox } from "@ai-hero/sandcastle/sandboxes/no-sandbox";
import { z } from "zod";
import type { Options } from "./args.mts";
import { batchIssueBranch, issueBranch } from "./branches.mts";
import { codexWithSandbox, plannerCodexOptions } from "./codex-agent.mts";
import type { Issue } from "./github-issues.mts";
import { plannerPrompt } from "./prompts.mts";
import { logPath, type Summary } from "./run-summary.mts";

const planSchema = z.object({
  issues: z.array(
    z.object({
      number: z.number().int().positive(),
      title: z.string(),
      branch: z.string(),
    }),
  ),
});

export type PlannedIssue = z.infer<typeof planSchema>["issues"][number];

export const planWave = async (
  issues: Issue[],
  options: Options,
  summary: Summary,
  iteration: number,
  runIdForBatch?: string,
): Promise<PlannedIssue[]> => {
  const branchFor = (issue: Issue) =>
    runIdForBatch ? batchIssueBranch(runIdForBatch, issue.number) : issueBranch(issue.number);
  const prompt = plannerPrompt(issues, branchFor);

  if (options.dryRun) {
    console.log(prompt);
    summary.status = "dry-run";
    summary.notes.push("Dry run printed planner prompt without running agents.");
    return [];
  }

  const plan = await run({
    name: `planner-${iteration}`,
    sandbox: noSandbox(),
    agent: codexWithSandbox(plannerCodexOptions(options)),
    prompt,
    maxIterations: 1,
    output: Output.object({ tag: "plan", schema: planSchema }),
    logging: {
      type: "file",
      path: logPath(summary, `planner-${iteration}`),
      verbose: true,
    },
    idleTimeoutSeconds: 1200,
    completionTimeoutSeconds: 120,
  });

  return plan.output.issues;
};

export const runLimited = async <T, R>(
  items: T[],
  limit: number,
  worker: (item: T) => Promise<R>,
): Promise<Array<PromiseSettledResult<R>>> => {
  const results: Array<PromiseSettledResult<R>> = new Array(items.length);
  let nextIndex = 0;

  const workers = Array.from({ length: Math.min(limit, items.length) }, async () => {
    while (nextIndex < items.length) {
      const index = nextIndex;
      nextIndex += 1;
      try {
        results[index] = { status: "fulfilled", value: await worker(items[index]!) };
      } catch (error) {
        results[index] = { status: "rejected", reason: error };
      }
    }
  });

  await Promise.all(workers);
  return results;
};
