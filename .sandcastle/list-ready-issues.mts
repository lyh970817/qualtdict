import { execFileSync } from "node:child_process";

type GhLabel = {
  readonly name: string;
};

type GhComment = {
  readonly body: string;
};

type GhIssue = {
  readonly number: number;
  readonly title: string;
  readonly body?: string;
  readonly labels?: readonly GhLabel[];
  readonly comments?: readonly GhComment[];
};

type ReadyIssue = {
  readonly id: string;
  readonly title: string;
  readonly body: string;
  readonly labels: readonly string[];
  readonly comments: readonly string[];
  readonly hasSubIssues: boolean;
  readonly parentIssueId: string | null;
  readonly linkedImplementationIssueIds: readonly string[];
};

const gh = (args: readonly string[]) =>
  execFileSync("gh", args, {
    encoding: "utf8",
    stdio: ["ignore", "pipe", "pipe"],
  }).trim();

const safeGh = (args: readonly string[]) => {
  try {
    return gh(args);
  } catch {
    return "";
  }
};

const repo = gh([
  "repo",
  "view",
  "--json",
  "nameWithOwner",
  "-q",
  ".nameWithOwner",
]);
const [owner, name] = repo.split("/");

if (!owner || !name) {
  throw new Error(`Could not parse GitHub repo name: ${repo}`);
}

const rawIssues = gh([
  "issue",
  "list",
  "--state",
  "open",
  "--label",
  "ready-for-agent",
  "--limit",
  "100",
  "--json",
  "number,title,body,labels,comments",
]);

const issues = JSON.parse(rawIssues) as readonly GhIssue[];

const getSubIssueCount = (issueNumber: number) => {
  const count = safeGh([
    "api",
    `repos/${repo}/issues/${issueNumber}/sub_issues`,
    "--jq",
    "length",
  ]);

  return Number.parseInt(count || "0", 10) || 0;
};

const getParentIssueId = (issueNumber: number) => {
  const parentNumber = safeGh([
    "api",
    "graphql",
    "-f",
    [
      "query=query($owner: String!, $name: String!, $number: Int!) {",
      "repository(owner: $owner, name: $name) {",
      "issue(number: $number) { parent { number } }",
      "}",
      "}",
    ].join(" "),
    "-f",
    `owner=${owner}`,
    "-f",
    `name=${name}`,
    "-F",
    `number=${issueNumber}`,
    "--jq",
    ".data.repository.issue.parent.number // empty",
  ]);

  return parentNumber || null;
};

const markdownSection = (body: string, heading: string) => {
  const lines = body.split(/\r?\n/);
  const start = lines.findIndex((line) =>
    new RegExp(`^##\\s+${heading}\\b`, "i").test(line),
  );

  if (start === -1) return "";

  const sectionLines: string[] = [];

  for (const line of lines.slice(start + 1)) {
    if (/^##\s+/.test(line)) break;
    sectionLines.push(line);
  }

  return sectionLines.join("\n");
};

const referencesParentIssue = (body: string, parentNumber: number) => {
  const escapedNumber = String(parentNumber).replace(
    /[.*+?^${}()|[\]\\]/g,
    "\\$&",
  );
  const issueRef = `#${escapedNumber}\\b`;
  const issueUrlRef = `/issues/${escapedNumber}\\b`;
  const parentSection = markdownSection(body, "Parent");

  return new RegExp(issueRef, "i").test(parentSection) ||
    new RegExp(issueUrlRef, "i").test(parentSection) ||
    new RegExp(
      `Parent PRD[^\\n]*(?:${issueRef}|${issueUrlRef})`,
      "im",
    ).test(body) ||
    new RegExp(
      `Parent[\\s:]+(?:${issueRef}|${issueUrlRef})`,
      "im",
    ).test(body);
};

const readyIssues: ReadyIssue[] = issues.map((issue) => ({
  id: String(issue.number),
  title: issue.title,
  body: issue.body ?? "",
  labels: (issue.labels ?? []).map((label) => label.name),
  comments: (issue.comments ?? []).map((comment) => comment.body),
  hasSubIssues: getSubIssueCount(issue.number) > 0,
  parentIssueId: getParentIssueId(issue.number),
  linkedImplementationIssueIds: [],
}));

const readyIssuesWithLinks = readyIssues.map((issue) => ({
  ...issue,
  linkedImplementationIssueIds: readyIssues
    .filter((candidate) => candidate.id !== issue.id)
    .filter((candidate) =>
      referencesParentIssue(
        [candidate.body, ...candidate.comments].join("\n\n"),
        Number.parseInt(issue.id, 10),
      ),
    )
    .map((candidate) => candidate.id),
}));

console.log(JSON.stringify(readyIssuesWithLinks));
