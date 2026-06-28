const flows = {
  blank: () => import("./blank/main.mts"),
  "simple-loop": () => import("./simple-loop/main.mts"),
  "sequential-reviewer": () => import("./sequential-reviewer/main.mts"),
  "parallel-planner": () => import("./parallel-planner/main.mts"),
  "parallel-planner-with-review": () =>
    import("./parallel-planner-with-review/main.mts"),
} as const;

type FlowName = keyof typeof flows;

const flowName = process.argv[2];

if (!flowName || !(flowName in flows)) {
  const availableFlows = Object.keys(flows)
    .map((name) => `  - ${name}`)
    .join("\n");

  console.error(`Usage: npm run sandcastle -- <flow>

Available flows:
${availableFlows}`);
  process.exit(1);
}

await flows[flowName as FlowName]();
