import { execFileSync } from "node:child_process";
import type { Options } from "./args.mts";

export type Issue = {
  number: number;
  title: string;
  body: string;
  url: string;
  state: "OPEN" | "CLOSED";
  createdAt?: string;
  labels: Array<{ name: string }>;
};

export const ghJson = <T,>(args: string[]): T => {
  const output = execFileSync("gh", args, {
    encoding: "utf8",
    stdio: ["ignore", "pipe", "pipe"],
  });
  return JSON.parse(output) as T;
};

export const gitOutput = (args: string[], cwd = process.cwd()): string =>
  execFileSync("git", args, {
    cwd,
    encoding: "utf8",
    stdio: ["ignore", "pipe", "pipe"],
  }).trim();

export const git = (args: string[], cwd = process.cwd()): void => {
  execFileSync("git", args, {
    cwd,
    stdio: "inherit",
  });
};

export const currentBranch = (): string => gitOutput(["branch", "--show-current"]) || "HEAD";

export const loadIssue = (issueNumber: number): Issue =>
  ghJson<Issue>([
    "issue",
    "view",
    String(issueNumber),
    "--json",
    "number,title,body,url,state,labels,createdAt",
  ]);

export const listIssues = (options: Options): Issue[] => {
  if (options.issues.length > 0) {
    return options.issues.map(loadIssue);
  }

  return ghJson<Issue[]>([
    "issue",
    "list",
    "--state",
    "open",
    "--label",
    options.label,
    "--limit",
    String(options.maxIssues),
    "--json",
    "number,title,body,url,state,labels,createdAt",
  ]).sort((a, b) => (a.createdAt ?? "").localeCompare(b.createdAt ?? ""));
};

export const assertIssueReady = (issue: Issue, label: string): void => {
  if (issue.state !== "OPEN") {
    throw new Error(`Issue #${issue.number} is not open.`);
  }

  const labels = issue.labels.map((issueLabel) => issueLabel.name);
  if (!labels.includes(label)) {
    throw new Error(`Issue #${issue.number} does not have required label "${label}".`);
  }
};

export const assertIssuesReady = (issues: Issue[], label: string): void => {
  for (const issue of issues) {
    assertIssueReady(issue, label);
  }
};
