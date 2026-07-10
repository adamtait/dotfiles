#!/bin/bash
# Codex plugin — expose the codex CLI on PATH for every login shell.
#
# codex is installed by the standalone installer at first boot
# (250_codex-install.sh) into the persistent ~/.local/bin (a symlink into
# ~/.codex/packages/standalone/current), not into /usr/local/bin, so it is not on
# the default PATH. Rather than teach every shell (bash, zsh, ...) to add
# ~/.local/bin — which zsh does not pick up from /etc/profile.d/* — we bake a
# symlink into /usr/local/bin, which is already on the baseline PATH of every
# shell and every invocation type (interactive, and `ra connect --command=...`).
#
# Why build time, and why a dangling link is fine: the container root (including
# /usr/local/bin) is ephemeral — reset from the image on every stop/start — while
# ~/.local/bin lives on the persistent /home disk. A symlink created at *runtime*
# would vanish on the next start and the marker-gated installer would not recreate
# it. Baking it into the image means it is present on every boot; it simply points
# at nothing until the first-boot install populates the target, then resolves for
# the workstation's whole life. `-f` keeps this idempotent across rebuilds.
set -euo pipefail

ln -sf /home/user/.local/bin/codex /usr/local/bin/codex
