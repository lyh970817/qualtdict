export type Mode = "single" | "sequential-review" | "parallel" | "parallel-review";
export type ScheduleMode = "wave" | "all";

export type Options = {
  issues: number[];
  label: string;
  dryRun: boolean;
  planOnly: boolean;
  ignoreBlockers: boolean;
  schedule: ScheduleMode;
  maxIssues: number;
  maxParallel: number;
  maxIterations?: number;
  model?: string;
  effort?: "low" | "medium" | "high" | "xhigh";
  networkAccess: "enabled" | "disabled";
  baseBranch?: string;
  runId?: string;
};

const parseArgsUnsafe = (argv: string[], mode: Mode): Options => {
  const options: Options = {
    issues: [],
    label: "ready-for-agent",
    dryRun: false,
    planOnly: false,
    ignoreBlockers: false,
    schedule: "wave",
    maxIssues: 100,
    maxParallel: 3,
    networkAccess: "enabled",
  };

  for (let i = 0; i < argv.length; i += 1) {
    const arg = argv[i];
    const next = () => {
      const value = argv[i + 1];
      if (!value || value.startsWith("--")) {
        throw new Error(`${arg} requires a value`);
      }
      i += 1;
      return value;
    };

    switch (arg) {
      case "--issue":
        options.issues.push(parsePositiveInt(next(), "--issue"));
        break;
      case "--issues":
        options.issues.push(
          ...next()
            .split(",")
            .map((value) => parsePositiveInt(value.trim(), "--issues")),
        );
        break;
      case "--label":
        options.label = next();
        break;
      case "--dry-run":
        options.dryRun = true;
        break;
      case "--plan-only":
        options.planOnly = true;
        break;
      case "--ignore-blockers":
        options.ignoreBlockers = true;
        break;
      case "--schedule": {
        const schedule = next();
        if (!["wave", "all"].includes(schedule)) {
          throw new Error("--schedule must be one of: wave, all");
        }
        options.schedule = schedule as ScheduleMode;
        break;
      }
      case "--max-issues":
        options.maxIssues = parsePositiveInt(next(), "--max-issues");
        break;
      case "--max-parallel":
        options.maxParallel = parsePositiveInt(next(), "--max-parallel");
        break;
      case "--max-iterations":
        options.maxIterations = parsePositiveInt(next(), "--max-iterations");
        break;
      case "--model":
        options.model = next();
        break;
      case "--effort": {
        const effort = next();
        if (!["low", "medium", "high", "xhigh"].includes(effort)) {
          throw new Error("--effort must be one of: low, medium, high, xhigh");
        }
        options.effort = effort as Options["effort"];
        break;
      }
      case "--network-access": {
        const networkAccess = next();
        if (!["enabled", "disabled"].includes(networkAccess)) {
          throw new Error("--network-access must be enabled or disabled");
        }
        options.networkAccess = networkAccess as Options["networkAccess"];
        break;
      }
      case "--base-branch":
        options.baseBranch = next();
        break;
      case "--run-id":
        options.runId = next();
        break;
      case "--help":
      case "-h":
        printUsage(mode);
        process.exit(0);
      default:
        throw new Error(`Unknown argument: ${arg}`);
    }
  }

  options.issues = [...new Set(options.issues)];

  if (["single", "sequential-review"].includes(mode) && options.issues.length > 1) {
    throw new Error(`${mode} mode accepts at most one issue.`);
  }

  if (!["parallel", "parallel-review"].includes(mode) && options.planOnly) {
    throw new Error("--plan-only is only supported for parallel modes.");
  }

  if (!["single", "sequential-review"].includes(mode) && options.ignoreBlockers) {
    throw new Error("--ignore-blockers is only supported for single issue modes.");
  }

  if (options.schedule === "all") {
    if (!["parallel", "parallel-review"].includes(mode)) {
      throw new Error("--schedule all is only supported for parallel modes.");
    }
    if (options.issues.length === 0) {
      throw new Error("--schedule all requires explicit --issues.");
    }
    options.maxIterations ??= options.issues.length;
  }

  options.maxIterations ??= 1;
  return options;
};

export const parseArgs = (argv: string[], mode: Mode): Options => {
  try {
    return parseArgsUnsafe(argv, mode);
  } catch (error) {
    console.error(error instanceof Error ? error.message : String(error));
    process.exit(1);
  }
};

export const printUsage = (mode: Mode) => {
  const parallelOnly = ["parallel", "parallel-review"].includes(mode)
    ? `
  --schedule <mode>        wave or all. Default: wave.
  --max-parallel <number>  Maximum implementation agents in flight. Default: 3.
  --max-iterations <n>     Planner cycles. For --schedule all, waves.`
    : "";

  const singleOnly = ["single", "sequential-review"].includes(mode)
    ? `
  --ignore-blockers        Run the selected single issue even with open blockers.`
    : "";

  console.log(`Usage:
  npm run sandcastle${mode === "single" ? "" : `:${mode}`} -- [options]

Options:
  --issue <number>         GitHub issue number. Can be repeated.
  --issues <numbers>       Comma-separated GitHub issue numbers.
  --label <label>          Required issue label. Default: ready-for-agent.
  --dry-run                Print resolved prompts/plan inputs and exit without agents.
  --plan-only              Run planner only, then exit before implementation.
  --model <model>          Override local Codex default model.
  --effort <level>         Set reasoning effort: low, medium, high, or xhigh.
  --network-access <mode>  Codex network_access value. Default: enabled.
  --base-branch <name>     Base branch for review diffs and scheduled batches.
  --run-id <id>            Override generated run artifact id.${singleOnly}${parallelOnly}
  --help                   Show this help.
`);
};

const parsePositiveInt = (value: string, flag: string): number => {
  const parsed = Number(value);
  if (!Number.isInteger(parsed) || parsed <= 0) {
    throw new Error(`${flag} must be a positive integer`);
  }
  return parsed;
};
