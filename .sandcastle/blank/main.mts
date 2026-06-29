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

// Blank template: customize this to build your own orchestration.
// Run this with: npx tsx .sandcastle/blank/main.mts [--max-iterations 1]

export default async function main(options: SandcastleOptions = {}) {
  await run({
    agent: codex(),
    sandbox: noSandbox(),
    promptFile: "./.sandcastle/blank/prompt.md",
    maxIterations: resolveMaxIterations(1, options),
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
    console.error(formatFlowUsage(".sandcastle/blank/main.mts"));
    process.exit(1);
  }
}
