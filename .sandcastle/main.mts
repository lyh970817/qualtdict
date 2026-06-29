import {
  formatSandcastleUsage,
  parseSandcastleArgs,
  type SandcastleOptions,
} from "./options.mts";

const flows = {
  blank: () => import("./blank/main.mts"),
  "simple-loop": () => import("./simple-loop/main.mts"),
  "sequential-reviewer": () => import("./sequential-reviewer/main.mts"),
  "parallel-planner": () => import("./parallel-planner/main.mts"),
  "parallel-planner-with-review": () =>
    import("./parallel-planner-with-review/main.mts"),
} as const;

type FlowName = keyof typeof flows;
type FlowModule = {
  default: (options: SandcastleOptions) => Promise<void> | void;
};

let parsedArgs: ReturnType<typeof parseSandcastleArgs>;

try {
  parsedArgs = parseSandcastleArgs(process.argv.slice(2), {
    allowFlowName: true,
  });
} catch (error) {
  console.error(error instanceof Error ? error.message : String(error));
  console.error("");
  console.error(formatSandcastleUsage(Object.keys(flows)));
  process.exit(1);
}

const flowName = parsedArgs.flowName;

if (!flowName || !(flowName in flows)) {
  if (flowName) {
    console.error(`Unknown flow: ${flowName}`);
    console.error("");
  }

  console.error(formatSandcastleUsage(Object.keys(flows)));
  process.exit(1);
}

const flow = (await flows[flowName as FlowName]()) as FlowModule;

await flow.default(parsedArgs.options);
