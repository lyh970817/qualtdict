import { mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import type { Mode } from "./args.mts";

export type IssueSummary = {
  number: number;
  title: string;
  branch: string;
  status: "pending" | "planned" | "completed" | "failed" | "blocked" | "skipped";
  commits: string[];
  log?: string;
  reviewLog?: string;
  error?: string;
  blockers?: number[];
};

export type Summary = {
  runId: string;
  mode: Mode;
  status: "running" | "completed" | "failed" | "blocked" | "plan-only" | "dry-run";
  runDir: string;
  integrationBranch?: string;
  baseBranch?: string;
  issues: IssueSummary[];
  notes: string[];
};

export const createRunId = (mode: Mode, override?: string): string => {
  if (override) return override;
  const timestamp = new Date().toISOString().replace(/[-:]/g, "").replace(/\..+$/, "");
  return `${timestamp}-${mode}`;
};

export const createSummary = (mode: Mode, runId: string): Summary => {
  const runDir = join(".sandcastle", "runs", runId);
  mkdirSync(join(runDir, "logs"), { recursive: true });
  return {
    runId,
    mode,
    status: "running",
    runDir,
    issues: [],
    notes: [],
  };
};

export const logPath = (summary: Summary, name: string): string =>
  join(summary.runDir, "logs", `${name}.log`);

export const writeSummary = (summary: Summary): void => {
  mkdirSync(summary.runDir, { recursive: true });
  writeFileSync(join(summary.runDir, "summary.json"), `${JSON.stringify(summary, null, 2)}\n`);
};

export const readSummary = (summary: Summary): Summary =>
  JSON.parse(readFileSync(join(summary.runDir, "summary.json"), "utf8")) as Summary;
