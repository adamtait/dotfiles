#!/bin/bash
# ra user-global build hook — install the current stable Go toolchain.
#
# Why this exists: the code-oss base image ships Go 1.22 (apt, /usr/bin/go).
# With GOTOOLCHAIN=auto (the default), any repo whose go.mod `go` directive is
# newer than the installed toolchain makes every `go` invocation — including a
# bare `go version` — download and switch to the newer toolchain first. On a
# fresh workstation that download is ~9s and stalls the first `go version` the
# starship prompt runs on `cd` into a Go repo, blowing past command_timeout.
# Installing a current toolchain as the default `go` means GOTOOLCHAIN=auto is
# already satisfied, so nothing downloads at prompt time.
#
# Numbered 050 so Go is on PATH before any plugin build hook (100+) that might
# want it. We resolve "most recent stable" at build time from go.dev rather than
# pinning, and SHA256-verify the tarball against go.dev's signed manifest.
set -euo pipefail

# Build hooks run over the network at image-build time; retry transient blips so
# a momentary go.dev hiccup doesn't fail the whole `ra create`.
curl() { command curl --retry 3 --retry-connrefused --retry-delay 2 "$@"; }

case "$(uname -m)" in
    x86_64 | amd64) ARCH=amd64 ;;
    aarch64 | arm64) ARCH=arm64 ;;
    *) echo "golang-install: unsupported arch $(uname -m)" >&2; exit 1 ;;
esac

# Latest stable version, e.g. "go1.26.2" (first line; a build-time line follows).
VERSION="$(curl -fsSL 'https://go.dev/VERSION?m=text' | head -n1)"
[ -n "$VERSION" ] || { echo "golang-install: could not resolve latest Go version" >&2; exit 1; }

# Skip if the target toolchain is already the one on PATH. GOTOOLCHAIN=local
# forces the version probe to use the installed toolchain — so this check can
# never itself trigger the toolchain download we're here to prevent, whatever
# the build's cwd/go.mod happens to be.
if command -v go >/dev/null 2>&1 && \
    [ "$(GOTOOLCHAIN=local go version 2>/dev/null | awk '{print $3}')" = "$VERSION" ]; then
    echo "golang-install: ${VERSION} already installed, skipping."
    exit 0
fi

TARBALL="${VERSION}.linux-${ARCH}.tar.gz"
SHA256="$(curl -fsSL 'https://go.dev/dl/?mode=json&include=all' \
    | jq -r --arg f "$TARBALL" '.[].files[] | select(.filename == $f) | .sha256')"
[ -n "$SHA256" ] || { echo "golang-install: no checksum for ${TARBALL}" >&2; exit 1; }

curl -fsSL -o /tmp/go.tgz "https://go.dev/dl/${TARBALL}"
echo "${SHA256}  /tmp/go.tgz" | sha256sum -c -

# Replace any previous /usr/local/go and expose go/gofmt on PATH ahead of the
# apt binary (/usr/local/bin precedes /usr/bin in the default PATH).
rm -rf /usr/local/go
tar -C /usr/local -xzf /tmp/go.tgz
rm -f /tmp/go.tgz
ln -sf /usr/local/go/bin/go /usr/local/bin/go
ln -sf /usr/local/go/bin/gofmt /usr/local/bin/gofmt

/usr/local/bin/go version
