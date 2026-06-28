import { run, codex } from "@ai-hero/sandcastle";
import { noSandbox } from "@ai-hero/sandcastle/sandboxes/no-sandbox";

// Blank template: customize this to build your own orchestration.
// Run this with: npx tsx .sandcastle/blank/main.mts

await run({
  agent: codex("gpt-5.4"),
  sandbox: noSandbox(),
  promptFile: "./.sandcastle/blank/prompt.md",
});
