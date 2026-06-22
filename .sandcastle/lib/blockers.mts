import { loadIssue, type Issue } from "./github-issues.mts";

export type BlockerStatus = {
  issue: Issue;
  blockerRefs: number[];
  openBlockers: number[];
  ambiguous: boolean;
  blocked: boolean;
};

export const section = (markdown: string, heading: string): string | undefined => {
  const lines = markdown.replace(/\r\n/g, "\n").split("\n");
  const headingPattern = new RegExp(`^##\\s+${escapeRegExp(heading)}\\s*$`, "i");
  const start = lines.findIndex((line) => headingPattern.test(line.trim()));
  if (start === -1) return undefined;

  const end = lines.findIndex((line, index) => index > start && /^##\s+\S/.test(line));
  const body = lines.slice(start + 1, end === -1 ? undefined : end).join("\n").trim();
  return body.length > 0 ? body : undefined;
};

export const blockerRefs = (issue: Issue): { refs: number[]; ambiguous: boolean } => {
  const blockedBy = section(issue.body, "Blocked by");
  if (!blockedBy) return { refs: [], ambiguous: true };

  if (/^\s*(?:[-*]\s*)?none\b/i.test(blockedBy)) {
    return { refs: [], ambiguous: false };
  }

  const refs = [...blockedBy.matchAll(/#(\d+)/g)].map((match) => Number(match[1]));
  return { refs: [...new Set(refs)], ambiguous: refs.length === 0 };
};

export const resolveBlockers = (
  issue: Issue,
  completedIssues: Set<number> = new Set(),
): BlockerStatus => {
  const parsed = blockerRefs(issue);
  const openBlockers = parsed.refs.filter((ref) => {
    if (completedIssues.has(ref)) return false;
    const blocker = loadIssue(ref);
    return blocker.state === "OPEN";
  });

  return {
    issue,
    blockerRefs: parsed.refs,
    openBlockers,
    ambiguous: parsed.ambiguous,
    blocked: parsed.ambiguous || openBlockers.length > 0,
  };
};

export const unblockedIssues = (
  issues: Issue[],
  completedIssues: Set<number> = new Set(),
): { unblocked: Issue[]; statuses: BlockerStatus[] } => {
  const statuses = issues.map((issue) => resolveBlockers(issue, completedIssues));
  return {
    statuses,
    unblocked: statuses.filter((status) => !status.blocked).map((status) => status.issue),
  };
};

const escapeRegExp = (value: string): string =>
  value.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
