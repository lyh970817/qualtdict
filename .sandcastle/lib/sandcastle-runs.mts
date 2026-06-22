import { run, type RunResult } from "@ai-hero/sandcastle";
import { noSandbox } from "@ai-hero/sandcastle/sandboxes/no-sandbox";
import type { Options } from "./args.mts";
import { codexWithSandbox } from "./codex-agent.mts";
import type { Issue } from "./github-issues.mts";
import { implementationPrompt, reviewPrompt } from "./prompts.mts";
import { logPath, type Summary } from "./run-summary.mts";

export const runImplementation = async (
  issue: Issue,
  branch: string,
  baseBranch: string,
  options: Options,
  summary: Summary,
): Promise<RunResult> =>
  run({
    name: `issue-${issue.number}`,
    sandbox: noSandbox(),
    agent: codexWithSandbox(options),
    prompt: implementationPrompt(issue),
    maxIterations: 1,
    branchStrategy: { type: "branch", branch, baseBranch },
    logging: {
      type: "file",
      path: logPath(summary, `issue-${issue.number}`),
      verbose: true,
    },
    idleTimeoutSeconds: 1200,
    completionTimeoutSeconds: 120,
  });

export const runReview = async (
  issue: Issue,
  branch: string,
  baseBranch: string,
  options: Options,
  summary: Summary,
): Promise<RunResult> =>
  run({
    name: `review-${issue.number}`,
    sandbox: noSandbox(),
    agent: codexWithSandbox(options),
    prompt: reviewPrompt(issue, branch, baseBranch),
    maxIterations: 1,
    branchStrategy: { type: "branch", branch, baseBranch },
    logging: {
      type: "file",
      path: logPath(summary, `review-${issue.number}`),
      verbose: true,
    },
    idleTimeoutSeconds: 1200,
    completionTimeoutSeconds: 120,
  });
