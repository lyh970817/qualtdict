import { pathToFileURL } from "node:url";

export interface SandcastleOptions {
  maxIterations?: number;
}

interface ParseOptions {
  allowFlowName: boolean;
}

export interface ParsedSandcastleArgs {
  flowName?: string;
  options: SandcastleOptions;
}

const maxIterationsFlags = new Set([
  "--max-iterations",
  "--max-iteration",
  "--maxIterations",
]);

export function isDirectRun(importMetaUrl: string) {
  const scriptPath = process.argv[1];

  return Boolean(scriptPath && importMetaUrl === pathToFileURL(scriptPath).href);
}

export function parseSandcastleArgs(
  args: string[],
  { allowFlowName }: ParseOptions,
): ParsedSandcastleArgs {
  let flowName: string | undefined;
  const options: SandcastleOptions = {};

  for (let i = 0; i < args.length; i++) {
    const arg = args[i]!;

    if (arg.startsWith("--")) {
      const [flag, inlineValue] = arg.split("=", 2);

      if (!maxIterationsFlags.has(flag)) {
        throw new Error(`Unknown option: ${flag}`);
      }

      const value = inlineValue ?? args[++i];
      options.maxIterations = parsePositiveInteger(flag, value);
      continue;
    }

    if (!allowFlowName) {
      throw new Error(`Unexpected argument: ${arg}`);
    }

    if (flowName) {
      throw new Error(`Unexpected argument: ${arg}`);
    }

    flowName = arg;
  }

  return { flowName, options };
}

export function resolveMaxIterations(
  defaultValue: number,
  options: SandcastleOptions,
) {
  return options.maxIterations ?? defaultValue;
}

export function formatSandcastleUsage(availableFlows: readonly string[]) {
  const flowList = availableFlows.map((name) => `  - ${name}`).join("\n");

  return `Usage: npm run sandcastle -- <flow> [options]

Available flows:
${flowList}

Options:
  --max-iterations <n>  Override the workflow's maximum iteration count.`;
}

export function formatFlowUsage(flowPath: string) {
  return `Usage: npx tsx ${flowPath} [options]

Options:
  --max-iterations <n>  Override the workflow's maximum iteration count.`;
}

function parsePositiveInteger(flag: string, value: string | undefined) {
  if (!value) {
    throw new Error(`Missing value for ${flag}`);
  }

  const parsed = Number(value);

  if (!Number.isSafeInteger(parsed) || parsed < 1) {
    throw new Error(`${flag} must be a positive integer`);
  }

  return parsed;
}
