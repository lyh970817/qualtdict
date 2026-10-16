---
name: dual-commit
description: Use when publishing qualtdict work to GitHub, when bringing `personal` back up to date with `main`, or when one piece of work touches both package files and this machine's personal-only files (agent instructions, skills, local rOpenSci reference docs, the Nix environment). Covers the branch and remote map — `main` is the public branch under rOpenSci review and `personal` is `main` plus the personal-only paths — where work is cut and merged, how to split mixed changes, the exact push commands, the post-merge restore step, and the checks that prove it landed.
---

qualtdict has one remote and two long-lived branches. Nothing is filtered at
push time: the split between what reviewers and users see and what is
machine-local is a split between two branches, `main` and `personal`. Get
the direction of every merge right and the rest follows. Get it wrong and the
agent instructions, skills and local reference docs land on the branch that
rOpenSci reviewers are reading.

## The branch and remote map

- **`origin`** is `git@github.com:lyh970817/qualtdict.git`, and it is the
  only remote.
- **`main` is the public package.** It tracks `origin/main`, and it is what
  rOpenSci reviewers (ropensci/software-review#807), CI, pkgdown and users
  see. Every package change ends up here first.
- **`personal` is `main` plus this machine's own files**, published to
  `origin` as `personal` and tracking `origin/personal`. It is where the owner
  works day to day. It adds exactly these paths, absent from `main`:
  - `AGENTS.md`
  - `.claude/skills/ci-green-after-push/SKILL.md`
  - `.claude/skills/dual-commit/SKILL.md` (this file)
  - `docs/agents/` (agent domain notes)
  - `docs/ropensci/` (snapshots of the rOpenSci dev guide and statistical
    software review book, with their upstream LICENSE)
  - `flake.nix`, `flake.lock` and `.envrc`

The three Nix files are one environment, not three: `flake.nix` defines it,
`flake.lock` pins it, and `.envrc` is the single line `use nix` that makes
direnv load it. They move together; restoring one without the others leaves
an environment that does not evaluate or does not load. **At the time of
writing all three are still tracked on `main`.** They are scheduled to be
removed from `main` as part of the rOpenSci editor-check work. When that
lands, merging `main` into `personal` deletes them here too, and they must be
restored on `personal` in a follow-up commit (see below). Until then they
show up in neither direction of the diff.

`docs/agents/` and `docs/ropensci/` are caught by the `docs/*` rule in
`.gitignore`. They were added with `git add -f` and stay tracked; new files
under them need `git add -f` again. Do not edit `.gitignore` or
`.Rbuildignore` to accommodate personal files: those are package files and
belong to `main`.

`personal` is a branch on the public GitHub repository, so anyone can read
it. It must never contain credentials (`.Renviron` and `tools/.Renviron` stay
ignored), Qualtrics API cassettes, or downloaded survey data. `tmp/`,
`pkgdown/`, `scratch.R`, `.R-lib/`, `.direnv/` and `.nix/` stay untracked on
both branches.

## Where work happens

- Cut feature branches from **`main`**, in worktrees under `.worktrees/` or
  `.claude/worktrees/` (both ignored). A worktree cut from `main` has no
  `AGENTS.md` or skills; read them from the main checkout.
- Merge the finished feature branch into `main`, then push `main`.
- Refresh `personal` by merging `main` into it.

The flow is one-way: `main` into `personal`. Never merge `personal` into
`main`, and never cut a public feature branch from `personal`. If a branch was
cut from `personal` by mistake, do not merge it into `main`. Check what it
would publish:

```sh
git diff --name-only main...BRANCH
```

If that lists no personal-only paths, cherry-pick the branch's genuinely new
commits onto a fresh branch from `main` (the merge-base comparison above
names what is new; `personal`'s own commits are not). If it lists personal
paths, split first.

## Mixed changes

When one piece of work touches both package files and personal-only files —
say a layout change plus the `AGENTS.md` paragraph that describes it — split
by hunk, not by intention:

1. Assign every hunk to exactly one side. Anything under the personal-only
   paths is personal; everything else is public.
2. Commit the public hunks on the feature branch off `main`. Before each
   commit, check the staged diff for personal paths, machine paths, agent
   tooling and Nix material.
3. Merge into `main`, push, and merge `main` into `personal`.
4. Only then commit the personal hunks on `personal`, on top of the public
   change they depend on.

If a merge into `personal` conflicts, the intended result is always the
`main` version of package content plus the existing personal-only files.
Never resolve a conflict by moving personal content into the public diff.

## Keeping `personal` current

`personal` is refreshed by merging, never rebasing — its history is published
on `origin`.

```sh
git checkout personal
git merge main
git diff --name-status main personal
```

That last command is the check that matters. It must list exactly the
personal-only paths above as `A` lines and nothing else (today every
`docs/ropensci/` file appears individually; the Nix three appear only after
they leave `main`). Anything missing was deleted on `main` since the last
merge, and the merge faithfully propagated that deletion onto `personal`. Put
it back in a follow-up commit, for example:

```sh
del=$(git log -1 --diff-filter=D --format=%H main -- flake.nix)  # the deleting commit
git checkout "$del^" -- flake.nix flake.lock .envrc
git commit -m "Restore the Nix environment on personal"
```

Any `M` or `D` line means `personal` has drifted from `main` on a package
file, which should never happen: find out how before doing anything else.

Two things are easy to get wrong:

- **The restore is not needed after every merge.** A merge only re-deletes a
  file that `main` deleted since the last merge. Run the check and act on
  what it says.
- **A file can survive the merge and still be stale.** `AGENTS.md` describes
  the package layout, the R files, the build commands and the worktree
  location. When `main` changes any of those, read the personal-only files
  and ask whether they still describe the truth; a name-status diff cannot
  see that kind of drift.

## Publishing

```sh
git push origin main       # after merging a feature branch into main
git push origin personal   # after refreshing personal
```

Both branches have upstream tracking, and these two commands are the entire
procedure. Both are plain fast-forwards. A non-fast-forward rejection means
`origin` has commits you do not: stop and look. Never resolve it with
`--force`. CI workflows trigger only on `main`, so pushing `personal` runs no
checks; use the `ci-green-after-push` skill after pushing `main`.

## Never

- **Never merge `personal` into `main`,** or push `personal` (or any refspec
  ending in `:main`) to `main`. It publishes every personal-only path to the
  branch under review.
- **Never cut a public feature branch from `personal`.**
- **Never rebase `personal`, and never force-push either branch.**
- **Never commit credentials or survey data to either branch.** `personal`
  is public too.
- **Never "fix" personal-only files to satisfy package hooks.** If a
  pre-commit hook fails solely because of personal-only files, commit with
  `--no-verify` and say so.
- **Never edit `.gitignore` or `.Rbuildignore` on `personal`** to hide or
  reveal personal files; use `git add -f`.

## Verifying you got it right

Fetch first, then check. Each pair reports `0	0`; a nonzero left number
means unpushed work, a nonzero right number means someone else pushed.

```sh
git fetch origin
git rev-list --left-right --count main...origin/main          # expect 0  0
git rev-list --left-right --count personal...origin/personal  # expect 0  0
git diff --name-status main personal   # expect only A lines, all personal-only paths
git log --oneline main..personal       # personal's own commits plus merges of main
git rev-list --count personal..main    # expect 0 right after a refresh
```
