#!/bin/bash
# Claude plugin — expose the claude CLI on PATH for every login shell.
#
# claude is installed with `npm install -g` into the persistent ~/.npm-global at
# first boot (250_claude-install.sh), not into /usr/local/bin, so it is not on
# the default PATH. Rather than teach every shell (bash, zsh, ...) to add
# ~/.npm-global/bin — which zsh does not pick up from /etc/profile.d/* — we bake a
# symlink into /usr/local/bin, which is already on the baseline PATH of every
# shell and every invocation type (interactive, and `ra connect --command=...`).
#
# Why build time, and why a dangling link is fine: the container root (including
# /usr/local/bin) is ephemeral — reset from the image on every stop/start — while
# ~/.npm-global lives on the persistent /home disk. A symlink created at *runtime*
# would vanish on the next start and the marker-gated installer would not recreate
# it. Baking it into the image means it is present on every boot; it simply points
# at nothing until the first-boot install populates the target, then resolves for
# the workstation's whole life. `-f` keeps this idempotent across rebuilds.
set -euo pipefail

ln -sf /home/user/.npm-global/bin/claude /usr/local/bin/claude
