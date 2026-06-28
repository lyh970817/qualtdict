import { run, codex } from "@ai-hero/sandcastle";
import { noSandbox } from "@ai-hero/sandcastle/sandboxes/no-sandbox";

// Simple loop: an agent that picks open issues one by one and closes them.
// Run this with: npx tsx .sandcastle/simple-loop/main.mts

await run({
  // A name for this run, shown as a prefix in log output.
  name: "worker",

  // Sandbox provider — runs the agent in the local repository environment.
  sandbox: noSandbox(),

  // The agent provider. Pass a model string to codex().
  agent: codex("gpt-5.4"),

  // Path to the prompt file. Shell expressions inside are evaluated inside the
  // sandbox at the start of each iteration, so the agent always sees fresh data.
  promptFile: "./.sandcastle/simple-loop/prompt.md",

  // Maximum number of iterations (agent invocations) to run in a session.
  // Each iteration works on a single issue. Increase this to process more issues
  // per run, or set it to 1 for a single-shot mode.
  maxIterations: 3,

  // Branch strategy — merge-to-head creates a temporary branch for the agent
  // to work on, then merges the result back to HEAD when the run completes.
  branchStrategy: { type: "merge-to-head" },
});
