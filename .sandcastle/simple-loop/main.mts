import { run } from "@ai-hero/sandcastle";
import { noSandbox } from "@ai-hero/sandcastle/sandboxes/no-sandbox";
import { codex } from "../codex.mts";
import {
  formatFlowUsage,
  isDirectRun,
  parseSandcastleArgs,
  resolveMaxIterations,
  type SandcastleOptions,
} from "../options.mts";

// Simple loop: an agent that picks open issues one by one and lands them by PR.
// Run this with: npx tsx .sandcastle/simple-loop/main.mts [--max-iterations 3]

export default async function main(options: SandcastleOptions = {}) {
  await run({
    // A name for this run, shown as a prefix in log output.
    name: "worker",

    // Sandbox provider — runs the agent in the local repository environment.
    sandbox: noSandbox(),

    // The shared agent provider default lives in .sandcastle/codex.mts.
    agent: codex(),

    // Path to the prompt file. Shell expressions inside are evaluated inside
    // the sandbox at the start of each iteration, so the agent always sees
    // fresh data.
    promptFile: "./.sandcastle/simple-loop/prompt.md",

    // Maximum number of iterations (agent invocations) to run in a session.
    // Each iteration works on a single issue. Increase this to process more
    // issues per run, or set it to 1 for a single-shot mode.
    maxIterations: resolveMaxIterations(3, options),

    // The prompt owns branch creation, PR creation, and PR merge. Do not use
    // merge-to-head here; that would bypass the repository's PR workflow.
  });
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
    console.error(formatFlowUsage(".sandcastle/simple-loop/main.mts"));
    process.exit(1);
  }
}
