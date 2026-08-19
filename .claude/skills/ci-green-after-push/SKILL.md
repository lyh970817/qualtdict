---
name: ci-green-after-push
description: Wait for GitHub Actions to finish, and fix red runs, after pushing to origin or opening a PR in this repo. Use immediately after any `git push` that reaches origin, and after `gh pr create`. Not for local commits, local pre-commit or pre-push hook runs, or edits to files under `.github/workflows/`.
---

# Wait for CI after pushing

A push or a PR is not finished until every workflow run it triggered has
reached a terminal state.

## Wait

Poll `gh run list --branch <branch>` until every run triggered by the pushed
head SHA is terminal, then report each workflow and its conclusion.

The check matrix is slow: 10-30 minutes is normal. Keep polling. Do not
report the push as done, and do not move on to unrelated work, while a run is
queued or in progress.

## Fix

If a run fails, read the failing log with `gh run view <id> --log-failed`,
diagnose the cause, fix it, push again, and wait again. Repeat until green.

Fix the cause, never the check. Skipping a test, disabling a workflow,
loosening a hook, or passing `--no-verify` is not a fix. If the check itself
is wrong, stop and say so rather than editing it green.

Reproduce failures locally through the Nix flake (`nix develop -c ...`); the
ambient `Rscript` cannot load the package's dependencies.

A red `pkgdown` run means the published site did not deploy.

## When not to wait

Only when the user has explicitly said not to.
