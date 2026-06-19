#!/bin/bash
# Gemini plugin — every-boot auth configuration.
#
# Runs from /etc/workstation-startup.d/, after 200_remote-agent-setup.sh
# has fetched secrets and written /run/ra/env, and after
# 250_gemini-install.sh has installed the gemini binary.
set -euo pipefail

USER_NAME="user"
USER_HOME="/home/${USER_NAME}"

# Source fetched secrets (sets GEMINI_API_KEY when apikey provider is selected).
[ -r /run/ra/env ] && set -a && . /run/ra/env && set +a

[ "${RA_PLUGIN_GEMINI_ENABLED:-false}" = "true" ] || exit 0

GEMINI_SETTINGS_DIR="${USER_HOME}/.gemini"
GEMINI_SETTINGS_PATH="${GEMINI_SETTINGS_DIR}/settings.json"
GEMINI_BIN_PATH="${USER_HOME}/.npm-global/bin/gemini"

log() { echo "[gemini-setup] $(date -u +%H:%M:%S) $*"; }

# Source the shared ra-env helper (ra_env_set / ra_env_unset), installed by
# 200_remote-agent-setup.sh at boot. Each function dedups and updates BOTH
# /etc/environment and /run/ra/env. Fall back to a thin inline implementation
# if the lib is missing (e.g. a newer plugin booting against an older core).
if [ -r /usr/local/lib/ra/ra-env.sh ]; then
    # shellcheck source=/dev/null
    . /usr/local/lib/ra/ra-env.sh
else
    log "WARNING: /usr/local/lib/ra/ra-env.sh missing; using inline fallback"
    ra_env_set() {
        local key="$1" val="$2"
        [ -f /etc/environment ] || : > /etc/environment
        sed -i "/^${key}=/d" /etc/environment
        echo "${key}=${val}" >> /etc/environment
    }
    ra_env_unset() {
        local key="$1"
        [ -f /etc/environment ] && sed -i "/^${key}=/d" /etc/environment
        [ -f /run/ra/env ] && sed -i "/^${key}=/d" /run/ra/env
        return 0
    }
fi

_clear_stale_state() {
    # Clear prior auth from BOTH /etc/environment and /run/ra/env so a provider
    # switch can't leave stale values where login shells still source them.
    ra_env_unset GEMINI_API_KEY
    ra_env_unset GOOGLE_GENAI_USE_VERTEXAI
    ra_env_unset GOOGLE_CLOUD_PROJECT
    ra_env_unset GOOGLE_CLOUD_LOCATION

    if [[ -f "${GEMINI_SETTINGS_PATH}" ]] && command -v jq >/dev/null 2>&1; then
        # A pre-existing corrupt/empty settings.json makes jq fail; the bare
        # `jq ... && mv` would then return non-zero and trip `set -e`. Handle
        # the failure explicitly and leave the file as-is.
        local tmp; tmp=$(mktemp)
        if jq 'del(.selectedAuthType)' "${GEMINI_SETTINGS_PATH}" > "${tmp}" 2>/dev/null; then
            mv "${tmp}" "${GEMINI_SETTINGS_PATH}"
            chown "${USER_NAME}:${USER_NAME}" "${GEMINI_SETTINGS_PATH}"
        else
            rm -f "${tmp}"
            log "could not strip selectedAuthType (invalid settings.json?); leaving as-is"
        fi
    fi
}

# _write_settings merges {selectedAuthType, theme, hasSeenIdeIntegrationNudge}
# into ~/.gemini/settings.json, creating the file if absent. The keys are
# what gemini-cli reads on startup to skip its first-run pickers; values
# may need tweaking against future gemini-cli versions.
_write_settings() {
    local auth_type="$1"
    mkdir -p "${GEMINI_SETTINGS_DIR}"
    chown "${USER_NAME}:${USER_NAME}" "${GEMINI_SETTINGS_DIR}"
    if ! command -v jq >/dev/null 2>&1; then
        log "jq not available; cannot seed ${GEMINI_SETTINGS_PATH}"
        return 0
    fi
    local tmp; tmp=$(mktemp)
    if [[ -f "${GEMINI_SETTINGS_PATH}" ]]; then
        jq --arg auth "${auth_type}" '
            .selectedAuthType = $auth
            | .theme = (.theme // "Default")
            | .hasSeenIdeIntegrationNudge = true
        ' "${GEMINI_SETTINGS_PATH}" > "${tmp}" \
            || { rm -f "${tmp}"; log "jq merge failed"; return 0; }
    else
        jq -n --arg auth "${auth_type}" '
            {selectedAuthType: $auth, theme: "Default", hasSeenIdeIntegrationNudge: true}
        ' > "${tmp}" \
            || { rm -f "${tmp}"; log "jq render failed"; return 0; }
    fi
    mv "${tmp}" "${GEMINI_SETTINGS_PATH}"
    chown "${USER_NAME}:${USER_NAME}" "${GEMINI_SETTINGS_PATH}"
    chmod 600 "${GEMINI_SETTINGS_PATH}"
}

# _smoke_test runs `gemini -p "respond with OK"` as the workstation user.
# Args are forwarded to `env` so the caller can inject the auth-related
# variables sudo would otherwise strip (GEMINI_API_KEY for apikey mode;
# GOOGLE_GENAI_USE_VERTEXAI / GOOGLE_CLOUD_* for vertex mode).
_smoke_test() {
    if ! [ -x "${GEMINI_BIN_PATH}" ]; then
        log "gemini binary missing; skipping smoke test"
        return 0
    fi
    if sudo -u "${USER_NAME}" env "$@" \
            "${GEMINI_BIN_PATH}" -p "respond with OK" 2>/dev/null \
            | grep -q 'OK'; then
        log "smoke test OK"
    else
        log "WARNING: smoke test did not return OK; gemini auth may be misconfigured"
    fi
}

_clear_stale_state

case "${RA_PLUGIN_GEMINI_AUTH_PROVIDER:-}" in
    apikey)
        if [[ -z "${GEMINI_API_KEY:-}" ]]; then
            log "GEMINI_API_KEY not set; auth may fail"
        else
            ra_env_set GEMINI_API_KEY "${GEMINI_API_KEY}"
            log "exported GEMINI_API_KEY"
            _write_settings "USE_GEMINI"
            _smoke_test "GEMINI_API_KEY=${GEMINI_API_KEY}"
        fi
        ;;
    vertex)
        : "${GCP_PROJECT_ID:?GCP_PROJECT_ID not set — required for vertex auth}"
        region="${RA_PLUGIN_GEMINI_REGION:-us-central1}"
        [ -n "${region}" ] || region="us-central1"
        ra_env_set GOOGLE_GENAI_USE_VERTEXAI true
        ra_env_set GOOGLE_CLOUD_PROJECT "${GCP_PROJECT_ID}"
        ra_env_set GOOGLE_CLOUD_LOCATION "${region}"
        log "configured Vertex AI mode (project=${GCP_PROJECT_ID}, location=${region})"
        _write_settings "USE_VERTEX_AI"
        _smoke_test \
            "GOOGLE_GENAI_USE_VERTEXAI=true" \
            "GOOGLE_CLOUD_PROJECT=${GCP_PROJECT_ID}" \
            "GOOGLE_CLOUD_LOCATION=${region}"
        ;;
    *)
        log "unknown auth provider '${RA_PLUGIN_GEMINI_AUTH_PROVIDER:-}'"
        ;;
esac
