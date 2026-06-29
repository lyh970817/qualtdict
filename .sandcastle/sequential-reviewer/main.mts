// Sequential Reviewer — implement-review-publish loop
//
// This template drives a three-phase workflow per issue:
//   Phase 1 (Implement): A Codex agent picks an open issue, works on it
//                        on a dedicated branch, commits the changes, and signals
//                        completion.
//   Phase 2 (Review):    A second Codex agent reviews the branch diff and either
//                        approves it or makes corrections directly on the branch.
//   Phase 3 (Publish):   A third Codex agent pushes the branch, opens a PR, and
//                        merges that PR through GitHub.
//
// All phases share a single sandbox created via createSandbox(), so the agents
// work on the same explicit branch.
//
// The outer loop repeats up to MAX_ITERATIONS times, processing one issue per
// iteration and stopping early once the backlog is exhausted (an implement
// phase that produces no commits). This is a middle-complexity option between
// the simple-loop (no review gate) and the parallel-planner (concurrent
// execution with a planning phase).
//
// Usage:
//   npx tsx .sandcastle/sequential-reviewer/main.mts [--max-iterations 10]

import * as sandcastle from "@ai-hero/sandcastle";
import { noSandbox } from "@ai-hero/sandcastle/sandboxes/no-sandbox";
import { codex } from "../codex.mts";
import {
  formatFlowUsage,
  isDirectRun,
  parseSandcastleArgs,
  resolveMaxIterations,
  type SandcastleOptions,
} from "../options.mts";

// ---------------------------------------------------------------------------
// Configuration
// ---------------------------------------------------------------------------

// Maximum number of implement→review→publish cycles to run before stopping.
// Each cycle works on one issue. Raise this to process more issues per run.
const MAX_ITERATIONS = 10;

// ---------------------------------------------------------------------------
// Main loop
// ---------------------------------------------------------------------------

export default async function main(options: SandcastleOptions = {}) {
  const maxIterations = resolveMaxIterations(MAX_ITERATIONS, options);

  for (let iteration = 1; iteration <= maxIterations; iteration++) {
    console.log(`\n=== Iteration ${iteration}/${maxIterations} ===\n`);

    // Generate a unique branch name for this iteration.
    const branch = `sandcastle/sequential-reviewer/${Date.now()}`;

    // Create a single sandbox that both the implementer and reviewer share.
    // This gives both agents a real, named branch that persists across phases.
    const sandbox = await sandcastle.createSandbox({
      branch,
      sandbox: noSandbox(),
    });

    try {
      // ---------------------------------------------------------------------
      // Phase 1: Implement
      //
      // A Codex agent picks the next open issue, writes the
      // implementation (using RGR: Red → Green → Repeat → Refactor), and
      // commits the result.
      //
      // The agent signals completion via <promise>COMPLETE</promise> when done.
      // ---------------------------------------------------------------------
      // One iteration so each outer pass implements a single issue on its own
      // branch, then hands it to the reviewer. A higher value lets the agent
      // drain the whole backlog onto this one branch in a single pass, which
      // defeats the per-issue review.
      const implement = await sandbox.run({
        name: "implementer",
        maxIterations: 1,
        agent: codex(),
        promptFile: "./.sandcastle/sequential-reviewer/implement-prompt.md",
      });

      if (!implement.commits.length) {
        // No commits means the backlog is empty or every remaining issue is
        // blocked — there is nothing left to implement or review, so stop.
        console.log("Implementation agent made no commits. Stopping.");
        break;
      }

      console.log(`\nImplementation complete on branch: ${branch}`);
      console.log(`Commits: ${implement.commits.length}`);

      // ---------------------------------------------------------------------
      // Phase 2: Review
      //
      // A second Codex agent reviews the diff of the branch produced by
      // Phase 1. It uses the {{BRANCH}} prompt argument to inspect the right
      // branch, and either approves or makes corrections directly on the
      // branch.
      // ---------------------------------------------------------------------
      await sandbox.run({
        name: "reviewer",
        maxIterations: 1,
        agent: codex(),
        promptFile: "./.sandcastle/sequential-reviewer/review-prompt.md",
        promptArgs: {
          BRANCH: branch,
        },
      });

      console.log("\nReview complete.");

      // ---------------------------------------------------------------------
      // Phase 3: Publish
      //
      // A third Codex agent pushes the reviewed branch, creates or updates a
      // PR, and merges that PR through GitHub. It must not merge the branch
      // locally.
      // ---------------------------------------------------------------------
      await sandbox.run({
        name: "publisher",
        maxIterations: 1,
        agent: codex(),
        promptFile: "./.sandcastle/sequential-reviewer/publish-prompt.md",
        promptArgs: {
          BRANCH: branch,
        },
      });

      console.log("\nPull request published and merged.");
    } finally {
      await sandbox.close();
    }
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
    console.error(formatFlowUsage(".sandcastle/sequential-reviewer/main.mts"));
    process.exit(1);
  }
}
