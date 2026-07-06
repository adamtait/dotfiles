# dotfiles

Cross-platform dotfiles managed with [chezmoi](https://chezmoi.io). Works on **macOS**
(Intel + Apple Silicon) and **Linux** (including code-oss / VS Code cloud workstations) from a
single source tree, using templates to handle the OS differences.

## What's managed

| Area | Details |
|------|---------|
| Shell | **zsh**, modular config under `~/.config/zsh/conf.d/`, Starship prompt |
| Terminal | **ghostty** (skipped on headless cloud workstations) |
| Editor | **Emacs** — fresh, minimal, CLI-first config (Clojure / Python / Go / Markdown) |
| Git | templated `~/.gitconfig` (identity + OS-aware credential helper), delta pager, global ignore |
| Multiplexer | tmux (+ TPM plugins) |
| Languages | Node (fnm), Python (pyenv), Java (OpenJDK 21), Go, Clojure CLI |
| Cloud | AWS CLI, gcloud, Terraform, Pulumi |
| macOS only | Hammerspoon, `~/Library/KeyBindings` (auto-skipped on Linux) |
| Coding agents | Claude Code, Codex CLI, Antigravity — sharing one `AGENTS.md` + one MCP server map |

Package installation is declarative: edit `home/.chezmoidata/packages.yaml` and the install
scripts re-run on the next `chezmoi apply`.

## Bootstrap a new machine

One line installs chezmoi, clones this repo, prompts for name/email/machine-type, applies
everything, and runs the package + bootstrap scripts:

```sh
sh -c "$(curl -fsSL https://get.chezmoi.io)" -- init --apply <your-github-username>
```

You'll be asked to pick a **machine type**: `personal`, `work`, or `cloud-workstation`. The
`cloud-workstation` choice skips GUI/macOS-only pieces (Hammerspoon, ghostty).

The bootstrap binary chezmoi uses to apply may not persist on PATH afterward, so a `run_once`
script (`run_once_after_10-install-chezmoi.sh.tmpl`) reinstalls chezmoi into `~/.local/bin`
(always on PATH via `00-path.zsh`). To restore it manually:

```sh
sh -c "$(curl -fsSL https://get.chezmoi.io)" -- -b ~/.local/bin
```

### Linux notes
- Base packages come from `apt`; tools that apt lacks or ships stale (fnm, clojure, pyenv,
  starship, git-delta, awscli, terraform, pulumi) are installed via **Homebrew on Linux**, which
  installs into `$HOME` and does not need root.
- zsh is installed via `apt` (it's in `packages.yaml`), then made the login shell. The shell
  change tries unprivileged `chsh` first, then falls back to a passwordless-`sudo`
  `chsh`/`usermod` — so it succeeds on typical cloud workstations. If even sudo is disallowed it
  warns instead of failing, so apply still completes; set the shell manually in that case.

## Day-to-day

```sh
chezmoi edit ~/.zshrc       # edit a managed file in the source, then apply
chezmoi diff                # preview what apply would change
chezmoi apply               # apply changes to $HOME
chezmoi update              # pull latest from git + apply
cd "$(chezmoi source-path)" # jump to this repo (also: `dotfiles` alias)
```

## Layout

```
.chezmoiroot           -> "home" (the chezmoi source root)
home/
  .chezmoi.toml.tmpl   prompts + per-machine data driving all templates
  .chezmoiignore       templated OS/cloud exclusions
  .chezmoidata/        declarative package + version data
  .chezmoiexternal.toml.tmpl  vendored externals (zsh plugins, tpm)
  .chezmoitemplates/   shared partials (agent instructions, MCP servers)
  dot_*                files that map to ~/.* (zshrc, gitconfig, tmux.conf, ...)
  dot_config/          ~/.config/* (zsh, ghostty, emacs, git, starship, agents)
  private_dot_claude/  private_dot_codex/  private_dot_antigravity/  agent configs
  run_*                package install + bootstrap scripts
```

See [IMPLEMENTATION-NOTES.md](IMPLEMENTATION-NOTES.md) for design decisions, deviations from the
original plan, and known caveats.
