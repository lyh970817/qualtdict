// Parallel Planner with Review — four-phase orchestration loop
//
// This template drives a multi-phase workflow:
//   Phase 1 (Plan):             A Codex agent analyzes open issues, builds a
//                               dependency graph, and outputs a <plan> JSON
//                               listing unblocked issues with branch names.
//   Phase 2 (Execute + Review): For each issue, a sandbox is created via
//                               createSandbox(). The implementer runs first
//                               (100 iterations). If it produces commits, a
//                               reviewer runs in the same sandbox on the same
//                               branch (1 iteration). All issue pipelines run
//                               concurrently via Promise.allSettled().
//   Phase 3 (Publish):          A single agent pushes completed branches,
//                               opens PRs, and merges those PRs.
//
// The outer loop repeats up to MAX_ITERATIONS times so that newly unblocked
// issues are picked up after each round of PR merges.
//
// Usage:
//   npx tsx .sandcastle/parallel-planner-with-review/main.mts [--max-iterations 10]

import * as sandcastle from "@ai-hero/sandcastle";
import { noSandbox } from "@ai-hero/sandcastle/sandboxes/no-sandbox";
import { z } from "zod";
import { codex } from "../codex.mts";
import {
  formatFlowUsage,
  isDirectRun,
  parseSandcastleArgs,
  resolveMaxIterations,
  type SandcastleOptions,
} from "../options.mts";

// The planner emits its plan as JSON inside <plan> tags; Output.object extracts
// and validates it against this schema. We use Zod here, but any Standard
// Schema validator works just as well — Valibot, ArkType, etc. See
// https://standardschema.dev.
const planSchema = z.object({
  issues: z.array(
    z.object({ id: z.string(), title: z.string(), branch: z.string() }),
  ),
});

// ---------------------------------------------------------------------------
// Configuration
// ---------------------------------------------------------------------------

// Maximum number of plan→execute→publish cycles before stopping.
// Raise this if your backlog is large; lower it for a quick smoke-test run.
const MAX_ITERATIONS = 10;

// ---------------------------------------------------------------------------
// Main loop
// ---------------------------------------------------------------------------

export default async function main(options: SandcastleOptions = {}) {
  const maxIterations = resolveMaxIterations(MAX_ITERATIONS, options);

  for (let iteration = 1; iteration <= maxIterations; iteration++) {
    console.log(`\n=== Iteration ${iteration}/${maxIterations} ===\n`);

    // -----------------------------------------------------------------------
    // Phase 1: Plan
    //
    // The planning agent reads the open issue list,
    // builds a dependency graph, and selects the issues that can be worked in
    // parallel right now (i.e., no blocking dependencies on other open issues).
    //
    // It outputs a <plan> JSON block — Output.object parses and validates it.
    // -----------------------------------------------------------------------
    const plan = await sandcastle.run({
      sandbox: noSandbox(),
      name: "planner",
      // One iteration is enough: the planner just needs to read and reason,
      // not write code. (Structured output requires maxIterations: 1.)
      maxIterations: 1,
      agent: codex(),
      promptFile: "./.sandcastle/parallel-planner-with-review/plan-prompt.md",
      // Extract and validate the <plan> JSON into a typed object. Throws
      // StructuredOutputError if the tag is missing, the JSON is malformed, or
      // validation fails — which aborts the loop.
      output: sandcastle.Output.object({ tag: "plan", schema: planSchema }),
    });

    const issues = plan.output.issues;

    if (issues.length === 0) {
      // No unblocked work — either everything is done or everything is blocked.
      console.log("No unblocked issues to work on. Exiting.");
      break;
    }

    console.log(
      `Planning complete. ${issues.length} issue(s) to work in parallel:`,
    );
    for (const issue of issues) {
      console.log(`  ${issue.id}: ${issue.title} → ${issue.branch}`);
    }

    // -----------------------------------------------------------------------
    // Phase 2: Execute + Review
    //
    // For each issue, create a sandbox via createSandbox() so the implementer
    // and reviewer share the same sandbox instance per branch. The implementer
    // runs first; if it produces commits, the reviewer runs in the same sandbox.
    //
    // Promise.allSettled means one failing pipeline doesn't cancel the others.
    // -----------------------------------------------------------------------

    const settled = await Promise.allSettled(
      issues.map(async (issue) => {
        const sandbox = await sandcastle.createSandbox({
          branch: issue.branch,
          sandbox: noSandbox(),
        });

        try {
          // Run the implementer
          const implement = await sandbox.run({
            name: "implementer",
            maxIterations: 100,
            agent: codex(),
            promptFile:
              "./.sandcastle/parallel-planner-with-review/implement-prompt.md",
            promptArgs: {
              TASK_ID: issue.id,
              ISSUE_TITLE: issue.title,
              BRANCH: issue.branch,
            },
          });

          // Only review if the implementer produced commits
          if (implement.commits.length > 0) {
            const review = await sandbox.run({
              name: "reviewer",
              maxIterations: 1,
              agent: codex(),
              promptFile:
                "./.sandcastle/parallel-planner-with-review/review-prompt.md",
              promptArgs: {
                BRANCH: issue.branch,
              },
            });

            // Combine commits from both runs so the publish phase sees all of
            // them.
            // Each sandbox.run() only returns commits from its own run.
            return {
              ...review,
              commits: [...implement.commits, ...review.commits],
            };
          }

          return implement;
        } finally {
          await sandbox.close();
        }
      }),
    );

    // Log any agents that threw (network error, sandbox crash, etc.).
    for (const [i, outcome] of settled.entries()) {
      if (outcome.status === "rejected") {
        console.error(
          `  ✗ ${issues[i]!.id} (${issues[i]!.branch}) failed: ${outcome.reason}`,
        );
      }
    }

    // Only pass branches that actually produced commits to the publish phase.
    // An agent that ran successfully but made no commits has nothing to publish.
    const completedIssues = settled
      .map((outcome, i) => ({ outcome, issue: issues[i]! }))
      .filter(
        (entry) =>
          entry.outcome.status === "fulfilled" &&
          entry.outcome.value.commits.length > 0,
      )
      .map((entry) => entry.issue);

    const completedBranches = completedIssues.map((i) => i.branch);

    console.log(
      `\nExecution complete. ${completedBranches.length} branch(es) with commits:`,
    );
    for (const branch of completedBranches) {
      console.log(`  ${branch}`);
    }

    if (completedBranches.length === 0) {
      // All agents ran but none made commits — nothing to publish this cycle.
      console.log("No commits produced. Nothing to publish.");
      continue;
    }

    // -----------------------------------------------------------------------
    // Phase 3: Publish
    //
    // One agent pushes completed branches, opens PRs, and merges those PRs
    // through GitHub. It must not merge branches locally into the base branch.
    //
    // The {{BRANCHES}} and {{ISSUES}} prompt arguments are lists that the agent
    // uses to know which branches to publish and which issues each PR should
    // close via GitHub closing keywords.
    // -----------------------------------------------------------------------
    await sandcastle.run({
      sandbox: noSandbox(),
      name: "publisher",
      maxIterations: 1,
      agent: codex(),
      promptFile: "./.sandcastle/parallel-planner-with-review/merge-prompt.md",
      promptArgs: {
        // A markdown list of branch names, one per line.
        BRANCHES: completedBranches.map((b) => `- ${b}`).join("\n"),
        // A markdown list of issue IDs and titles, one per line.
        ISSUES: completedIssues.map((i) => `- ${i.id}: ${i.title}`).join("\n"),
        // A markdown list pairing branch names with the issue each PR closes.
        ISSUE_BRANCHES: completedIssues
          .map((i) => `- ${i.branch}: #${i.id} ${i.title}`)
          .join("\n"),
      },
    });

    console.log("\nPull requests published and merged.");
  }

  console.log("\nAll done.");
}

if (isDirectRun(import.meta.url)) {
  try {
    await main(
      parseSandcastleArgs(process.argv.slice(2), {
        allowFlowName: false,
      }).options,
    );
  } catch (error) {
    console.error(error instanceof Error ? error.message : String(error));
    console.error("");
    console.error(
      formatFlowUsage(".sandcastle/parallel-planner-with-review/main.mts"),
    );
    process.exit(1);
  }
}
