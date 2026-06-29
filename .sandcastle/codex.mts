import { codex as sandcastleCodex } from "@ai-hero/sandcastle";

const CODEX_PROFILE = "mattpocock";
const CODEX_MODEL = "gpt-5.5";
const CODEX_OPTIONS = { effort: "high" } satisfies Parameters<
  typeof sandcastleCodex
>[1];

export const codex = (
  model: Parameters<typeof sandcastleCodex>[0] = CODEX_MODEL,
  options: Parameters<typeof sandcastleCodex>[1] = CODEX_OPTIONS,
) => {
  const agent = sandcastleCodex(model, options);

  const profiledAgent: typeof agent = {
    ...agent,
    buildPrintCommand(args) {
      const command = agent.buildPrintCommand(args);

      return {
        ...command,
        command: command.command.replace(
          /^codex\b/,
          `codex --profile ${CODEX_PROFILE}`,
        ),
      };
    },
    buildInteractiveArgs(args) {
      const [command, ...argsWithoutCommand] = agent.buildInteractiveArgs(args);

      return [
        command,
        "--profile",
        CODEX_PROFILE,
        ...argsWithoutCommand,
      ].filter((arg): arg is string => Boolean(arg));
    },
  };

  return profiledAgent;
};
