# Implementation notes — chezmoi migration

Running log of decisions, deviations, and tradeoffs made during the chezmoi rebuild that
were **not** fully specified in the approved plan. Read this to understand *why* the tree
looks the way it does.

## Environment facts discovered

- `chezmoi` is **not installed** on this workstation; **Go 1.26.4 is**. Plan to verify by
  `go install github.com/twpayne/chezmoi@latest` (needs network) — see Verification at bottom.
- This is the Linux/code-oss side of the story, so anything mac-only could only be reasoned
  about, not executed here.

## Decisions not in the spec

### 1. Source layout uses `.chezmoiroot = home`
Confirmed from plan. All managed files live under `home/`. Repo-level files (README,
IMPLEMENTATION-NOTES, LICENSE, `.github/`) stay out of the target. `chezmoi init` auto-detects
`.chezmoiroot`.

### 2. `.chezmoidata` is split, not one file
The plan said `.chezmoidata/packages.yaml`. chezmoi merges every file in `.chezmoidata/`, so I
kept a single `packages.yaml`. Data keys are referenced as `.packages.*` in templates.

### 3. Prompt: chose **Starship** over powerlevel10k or hand-rolled zsh
The plan left the prompt open ("vendored prompt via .chezmoiexternal or small native zsh
prompt"). I chose **Starship**: single static binary, identical on macOS + Linux, no plugin
framework, no font-dependency beyond a Nerd Font (already in package list), and it reads OS/git/
runtime context itself so no runtime `uname` branching is needed. Added to package lists; init
line in `40-prompt.zsh`. A minimal `starship.toml` is managed. (powerlevel10k needs a plugin
loader + instant-prompt cache files that are awkward under chezmoi; rejected.)

### 4. zsh plugins via `.chezmoiexternal` — kept to two, pinned by tag
Only `zsh-autosuggestions` and `zsh-syntax-highlighting` (fast-syntax-highlighting was
considered; stuck with the canonical one). Pinned to release tags with `refreshPeriod` so they
don't refetch on every apply. No completion framework (zsh's built-in `compinit` is enough);
this lets me delete the old 71 KB vendored `bash_completion`.

### 5. Aliases: dropped several legacy ones; guarded tool-replacement aliases
- Dropped `alias find=fd`, `alias du=ncdu` (shadowing core tools by name breaks scripts and
  muscle memory across machines where the tool may be missing). Instead bound `fd`/`ncdu`/`bat`
  under their own names and only aliased `cat`→`bat` style replacements **guarded** by
  `(( $+commands[...] ))`.
- The `myip`/`ifconfig` alias was macOS-only; replaced with an OS-branched template using
  `ip addr` on Linux.
- Kept the large `ls`-family aliases but switched `ls` colour flags to be GNU/BSD-aware.
- `alias d='cd ~/.dotfiles'` → `cd` to the chezmoi source dir via `chezmoi source-path`.

### 6. git global excludes handled declaratively
The old `git.sh` ran `echo .DS_Store >> ~/.gitignore_global` on every shell start (side-effectful
and duplicating). Replaced with a managed `dot_config/git/ignore` + `core.excludesfile` set in
gitconfig. No shell-startup mutation.

### 7. gitconfig: kept all aliases/pager; switched diff tool to **delta**
The old config piped through `diff-so-fancy` (a vendored script). Switched to `git-delta`
(maintained, single binary, cross-platform, in package list). `[include] path = .gituser`
retained so machine-local identity overrides still work; `~/.gituser` is **not** managed (left
for per-machine secrets/overrides).

### 8. Java version: pinned to **21** (LTS), not 11
Old repo hardcoded Corretto 11. JDK 11 is past its prime; chose OpenJDK 21 (current LTS). Clojure
+ all listed tooling run fine on 21. `JAVA_HOME` is resolved dynamically, not hardcoded.

### 9. Node version manager: **fnm** (plan recommended, confirmed)
Dropped nvm and the `~/.nvm` mkdir entirely.

### 10. ghostty config + cloud handling
`ghostty` config is managed but `.chezmoiignore`'d when `machine == cloud-workstation` (code-oss
runs in a browser; no local terminal emulator there). On a desktop Linux it still applies.

### 11. Clojure deps.edn — stripped Datomic
Removed `:mvn/repos` (datomic-cloud), `:dev`, `:ion-dev`, `:log` (Datomic/logback-coupled),
`:httpd`. Kept `:repl` (cider-nrepl, bumped versions), `:test`, `:run-tests` (kaocha), `:new`,
`:profiler`. Bumped cider-nrepl + clj-new to current-ish versions.

### 12. Agent config — template-include for instructions, symlink for AGENTS.md
- Shared guidance lives once in `.chezmoitemplates/agents-instructions.md.tmpl`.
- Claude `CLAUDE.md` includes it via `{{ template ... }}` (Claude wants its own file).
- The neutral canonical `~/.config/agents/AGENTS.md` is rendered from the same template.
- Codex + Antigravity get a **symlink** `AGENTS.md -> ~/.config/agents/AGENTS.md` (no dup).
- MCP servers: canonical map in `.chezmoitemplates/mcp-servers.json.tmpl` rendered into each
  tool's schema (Claude `settings.json` JSON, Codex `config.toml` TOML, Antigravity JSON).
- **Secrets**: no tokens inlined. MCP entries that need keys reference `${ENV_VAR}` and a comment
  points at chezmoi's `onepasswordRead`/`pass` functions for those who want vault integration.
  Left as env-var references by default to keep zero external dependencies.

### 13. Antigravity config path is a best-effort guess
Google Antigravity is new and its exact dotfile location/schema isn't something I can verify
offline. I used `~/.antigravity/config.json` + `AGENTS.md`. **Flagged for you to confirm** — if
the real path differs, only the `private_dot_antigravity/` dir name needs to change.

### 14. Emacs config — package.el + use-package, `:ensure t`, no straight.el
Fresh minimal config per plan. Notable choices:
- `package-vc`/`use-package` built-ins (Emacs 29+); assumes Emacs ≥29 (current on brew + recent
  apt). Documented as a requirement.
- eglot (built-in) over lsp-mode. treesit auto-install for go/python where grammars exist.
- A `run_onchange_after` script byte-installs packages headlessly so first interactive launch is
  fast.

### 15. Removed the whole legacy tree
Deleted `install/`, `setup/`, `submodules/`, `.gitmodules`, `bin/` (kept only
`git-prune-merged-branches`), and `configuration/`. Old content was mined first (git history
preserves the rest).

## Tradeoffs / things to know

- **Could not run `chezmoi apply` for real** here (not installed + would mutate $HOME). Verified
  by installing chezmoi via `go install` and running `chezmoi doctor` / `execute-template` /
  `diff` in a throwaway source — see Verification section, populated during the verify step.
- **Linux package coverage**: some tools (gcloud, pulumi, terraform, fnm, clojure) aren't reliably
  in apt; the Linux installer uses Homebrew-on-Linux for those. This assumes the cloud workstation
  permits installing linuxbrew (it generally does in `$HOME`). Documented in README.
- **`chsh` to zsh** may require a password / may be disallowed on locked-down cloud workstations.
  The `run_once` shell-switch script is **idempotent and non-fatal** (logs a warning instead of
  failing the whole apply) so a restricted box still completes.
- The macOS `DefaultKeyBinding.dict` and Hammerspoon Lua are copied verbatim from the old repo
  (still valid, mac-only, ignored on Linux).

## Verification (performed)

Installed chezmoi **v2.70.5** via the official `get.chezmoi.io` script (note: `go install
github.com/twpayne/chezmoi/v2@latest` fails because chezmoi's go.mod has `exclude` directives —
the install script is the supported path). Verified on this Linux box with two isolated
throwaway HOMEs (`machine=personal` and `machine=cloud-workstation`):

- `chezmoi doctor` — all green except environmental items (repo is a dirty git working tree;
  `/tmp` is a different device so the hardlink self-test fails — neither affects real use).
- **Prompt data** resolves for both machine types; `.packages` / `.pythonVersion` load from
  `.chezmoidata`.
- **`.chezmoiignore` logic correct**: on Linux `personal`, ghostty is managed but Hammerspoon +
  `~/Library` are not; on `cloud-workstation`, ghostty is also excluded. (Confirmed via
  `chezmoi managed` and `chezmoi archive`.)
- **Rendered output checked**: `~/.gitconfig` (Linux → `credential.helper = cache`, delta pager,
  identity from prompts); `00-path.zsh` (Linux → linuxbrew shellenv, zero `/usr/local/opt`
  references); `~/.claude/settings.json` and `~/.antigravity/config.json` are **valid JSON** with
  `mcpServers = [filesystem, git, fetch]`; `~/.codex/config.toml` is **valid TOML** with the same
  servers; the Codex/Antigravity `AGENTS.md` symlinks point at `~/.config/agents/AGENTS.md`.
- **Per-OS scripts**: the darwin package script renders to *empty* on Linux (OS guard works); the
  Linux script renders apt + linuxbrew logic; the Emacs preinstall script's content-hash line
  renders a real sha256 of `init.el`.
- **Full `chezmoi apply --dry-run`** is clean (exit 0, no template errors) for both machine types.

### Not verified here (and why)
- **macOS-only execution paths** of templates: chezmoi has no flag to fake `.chezmoi.os`, and this
  box is Linux. Go templates *parse* in full regardless of branch (so syntax errors anywhere are
  caught), but darwin-branch *execution* errors are not exercised. These branches were
  hand-reviewed; they use only standard template functions. **Recommend running the suggested CI
  matrix (macos-latest + ubuntu-latest) before relying on a mac.**
- **`zsh -n` lint**: zsh isn't installed on this box and couldn't be installed offline. The conf.d
  files are simple and were rendered successfully by chezmoi; the CI matrix should add a
  `zsh -n` pass.

### A `chezmoi` binary was left at `~/.local/bin/chezmoi`
Installed during verification; it's the actual tool these dotfiles use, so it was left in place
(outside the repo, not part of the PR). The stale v1 binary from the failed `go install` was
removed.
