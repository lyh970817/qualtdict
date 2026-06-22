import { mkdirSync } from "node:fs";
import { join } from "node:path";
import { git } from "./github-issues.mts";

export const issueBranch = (issueNumber: number): string => `sandcastle/issue-${issueNumber}`;

export const batchBranch = (runId: string): string => `sandcastle/batch/${runId}`;

export const batchIssueBranch = (runId: string, issueNumber: number): string =>
  `sandcastle/batch/${runId}/issue-${issueNumber}`;

export const createIntegrationWorktree = (
  runDir: string,
  branch: string,
  baseBranch: string,
): string => {
  const worktreePath = join(runDir, "integration-worktree");
  mkdirSync(runDir, { recursive: true });
  git(["worktree", "add", "-b", branch, worktreePath, baseBranch]);
  return worktreePath;
};

export const mergeIntoIntegration = (worktreePath: string, branch: string): void => {
  git(["merge", branch, "--no-edit"], worktreePath);
};
