# Ruby via rv (spinel-coop) — fast, single-binary version + gem manager. Replaces rbenv.
# rv is installed onto PATH by the package step; `rv shell init` registers a preexec
# hook that switches Ruby per .ruby-version/.tool-versions. Completions come from
# Homebrew's site-functions via compinit (see 30-completion.zsh), so none are sourced here.

if (( $+commands[rv] )); then
  eval "$(rv shell init zsh)"
fi
