import { run } from "@ai-hero/sandcastle";
import { noSandbox } from "@ai-hero/sandcastle/sandboxes/no-sandbox";
import { codex } from "../codex.mts";

// Blank template: customize this to build your own orchestration.
// Run this with: npx tsx .sandcastle/blank/main.mts

await run({
  agent: codex(),
  sandbox: noSandbox(),
  promptFile: "./.sandcastle/blank/prompt.md",
});
