import { codex, type AgentProvider } from "@ai-hero/sandcastle";
import type { Options } from "./args.mts";

export type CodexOptions = Pick<Options, "model" | "effort" | "networkAccess"> & {
  yolo?: boolean;
  sandbox?: "read-only" | "workspace-write" | "danger-full-access";
};

const shellArg = (value: string): string => `'${value.replace(/'/g, "'\\''")}'`;

export const codexWithSandbox = (options: CodexOptions): AgentProvider => {
  const base = codex(options.model ?? "local-default");

  return {
    ...base,
    buildPrintCommand({ prompt, resumeSession }) {
      if (resumeSession) {
        throw new Error("This Sandcastle runner does not support Codex session resume.");
      }

      const modelFlag = options.model ? ` --model ${shellArg(options.model)}` : "";
      const effortFlag = options.effort
        ? ` -c ${shellArg(`model_reasoning_effort="${options.effort}"`)}`
        : "";
      const networkFlag = ` -c ${shellArg(`network_access="${options.networkAccess}"`)}`;
      const permissionFlags =
        options.yolo === false
          ? ` --ask-for-approval never --sandbox ${shellArg(options.sandbox ?? "read-only")}`
          : " --yolo";

      return {
        command:
          `codex exec --json${permissionFlags}` +
          `${networkFlag}${modelFlag}${effortFlag} -`,
        stdin: prompt,
      };
    },
  };
};

export const plannerCodexOptions = (options: Options): CodexOptions => ({
  model: options.model,
  effort: options.effort,
  networkAccess: options.networkAccess,
  yolo: false,
  sandbox: "read-only",
});
