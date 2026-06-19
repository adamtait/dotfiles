# Implementation notes — Add `qmd` via Bun

Running log of decisions, deviations, and tradeoffs while implementing the `add-qmd-via-bun`
branch. These are things **not** spelled out in the approved plan
(`~/.claude/plans/delegated-churning-balloon.md`) that came up during implementation.

> Note: a separate, pre-existing `IMPLEMENTATION-NOTES.md` documents an earlier chezmoi
> migration — left untouched. This file is scoped to the qmd/Bun change only.

## What was built
- `home/dot_config/zsh/conf.d/75-bun.zsh` — puts Bun on PATH + loads completions.
- `home/run_once_after_23-install-bun.sh.tmpl` — installs Bun via the official installer.
- `home/run_once_after_24-install-qmd.sh.tmpl` — `bun install -g @tobilu/qmd`.
- `home/.chezmoidata/packages.yaml` — added `sqlite` to `darwin.brew`.

## Decisions / deviations from the spec

### 1. Notes kept in a separate file (didn't touch `IMPLEMENTATION-NOTES.md`)
The repo already has a committed `IMPLEMENTATION-NOTES.md` documenting an earlier chezmoi
migration. Overwriting it would destroy that history, so this change's notes live in
`implementation-notes-qmd.md` instead.

### 2. `sqlite` added to `darwin.brew` only — not to Linux
qmd's README says *macOS users* need Homebrew SQLite. On Linux, Bun ships a built-in SQLite
(`bun:sqlite`), and qmd runs on Bun here, so no apt SQLite package was added. If qmd turns
out to need a system `libsqlite3` on Linux at runtime, add `libsqlite3-dev` (or `sqlite3`)
to `linux.apt`. Left out for now to avoid an unneeded dependency.

### 3. Bun installer may edit `~/.zshrc` — PATH is owned by `75-bun.zsh` instead
The official `bun.sh/install` script appends a Bun block to detected shell rc files
(`~/.zshrc`/`~/.bashrc`). Since `~/.zshrc` is chezmoi-managed (`home/dot_zshrc`), those edits
are redundant and get normalized on the next `chezmoi apply`. PATH/completions are owned by
the managed `75-bun.zsh` module, so functionality doesn't depend on the installer's rc edits.
Tradeoff: there can be a transient "modified" state on `~/.zshrc` after the first install
until the next apply. Did not try to suppress the installer's rc edit (no clean flag for it).

### 4. `run_once` (not `run_onchange`) for both scripts
Mirrors the existing `install-python`/`install-gcloud` scripts. Consequence: bumping qmd
later is a manual `bun update -g @tobilu/qmd`, or edit the script to change its content hash
and re-trigger. Chose consistency with the repo over auto-update.

### 5. `fnm` left in place (coexists with Bun)
The user said to disregard fnm *for this task* and make Bun the default. fnm only manages the
`node` binary and doesn't conflict with Bun, so I did not remove it from `packages.yaml` or
delete `70-node.zsh` — that would be a broader, separate change. Easy to remove later if
wanted.

### 6. zsh module numbered `75-bun.zsh`; couldn't run `zsh -n`
Placed after `73-go.zsh`, before the `80-*` modules. zsh is **not installed** on this
workstation, so `zsh -n` lint couldn't run. The file mirrors `73-go.zsh`'s
`path=("…" $path)` idiom exactly (and relies on `00-path.zsh`'s `typeset -U path` for
de-duplication), so it's low-risk. A CI `zsh -n` pass would cover it.

### 7. Idempotency via `command -v` early-outs
Both scripts early-out if the target binary (`bun`, then `qmd`) is already on PATH — matching
the `install-gcloud` pattern. The qmd script also guards on `bun` being present and skips
(non-fatal) if Bun didn't install.

## Code review (`/review`, high effort) — outcome

**Fixed — finding #1 (real bug):** the official Bun installer appends a `# bun` block to
`~/.zshrc` whenever it's writable (confirmed against the installer source; no opt-out flag).
`~/.zshrc` is chezmoi-managed, so that out-of-band edit would make the *next*
`chezmoi apply` see the file as modified and abort trying to prompt — fatal on a no-TTY box
(the same failure mode already seen with `~/.config/git/ignore`). Fix in
`run_once_after_23-install-bun`: temporarily strip the user write bit on `~/.zshrc` around the
installer call so the installer's own `[[ -w $rc ]]` guard skips it, then restore. PATH and
completions are already owned by `75-bun.zsh`, so nothing is lost.

**Acknowledged, not changed — finding #2 (by design):** `run_once` + non-fatal `exit 0`
means a transient install failure is recorded as done and won't auto-retry on a later apply
(run_once tracks the content hash; neither `run_once` nor `run_onchange` re-runs on a clean
apply). This matches the existing `install-python`/`install-gcloud` scripts and the approved
plan, so it's left as-is for consistency. To force a retry after a failure: re-run the
script manually, or edit it to change its content hash. (An always-run `run_after_` script
guarded by the existing `command -v` early-out would retry every apply but would deviate from
the sibling convention.)

## Verification notes
- chezmoi's real source dir is `~/.local/share/chezmoi`, **not** this repo, so all checks
  used `chezmoi execute-template --source ./home` / `chezmoi apply --dry-run --source ./home`
  to render from the working tree.
- Both rendered scripts pass `bash -n`. `.packages.darwin.brew` contains `sqlite`. The
  `75-bun.zsh` module and both run_once scripts show up under `chezmoi managed`.
- `chezmoi apply --dry-run` exits 1, but the failure is **pre-existing and unrelated**:
  chezmoi found `~/.config/git/ignore` was modified outside chezmoi and tried to prompt with
  no TTY available. No template errors were reported, and nothing in the failure relates to
  the bun/qmd changes.
