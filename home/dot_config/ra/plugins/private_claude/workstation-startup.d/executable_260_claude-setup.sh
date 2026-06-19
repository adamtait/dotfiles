#!/bin/bash
# Claude plugin — every-boot auth configuration.
#
# Runs from /etc/workstation-startup.d/, after 200_remote-agent-setup.sh
# has fetched secrets and written /run/ra/env, and after
# 250_claude-install.sh has installed the claude binary.
set -euo pipefail

USER_NAME="user"
USER_HOME="/home/${USER_NAME}"

# Source fetched secrets (sets CLAUDE_CODE_OAUTH_TOKEN, ANTHROPIC_API_KEY, etc.).
[ -r /run/ra/env ] && set -a && . /run/ra/env && set +a

[ "${RA_PLUGIN_CLAUDE_ENABLED:-false}" = "true" ] || exit 0

APIKEY_HELPER_PATH="/usr/local/bin/claude-apikey-helper.sh"
CLAUDE_SETTINGS_DIR="${USER_HOME}/.claude"
CLAUDE_SETTINGS_PATH="${CLAUDE_SETTINGS_DIR}/settings.json"
CLAUDE_CONFIG_PATH="${USER_HOME}/.claude.json"
CLAUDE_BIN_PATH="${USER_HOME}/.npm-global/bin/claude"

log() { echo "[claude-setup] $(date -u +%H:%M:%S) $*"; }

# Source the shared ra-env helper (ra_env_set / ra_env_unset), installed by
# 200_remote-agent-setup.sh at boot. Each updates BOTH /etc/environment and
# /run/ra/env, deduped. If the lib is somehow absent (e.g. a newer plugin
# booting against an older core that predates it), fall back to an inline copy
# with the SAME both-file contract — so behavior is identical whether or not the
# lib is present, rather than silently diverging.
if [ -r /usr/local/lib/ra/ra-env.sh ]; then
    # shellcheck source=/dev/null
    . /usr/local/lib/ra/ra-env.sh
else
    log "WARNING: /usr/local/lib/ra/ra-env.sh missing; using inline fallback"
    : "${RA_ENV_FILE:=/run/ra/env}"
    ra_env_set() {
        local key="$1" val="$2"
        [ -f /etc/environment ] || : > /etc/environment
        sed -i "/^${key}=/d" /etc/environment
        echo "${key}=${val}" >> /etc/environment
        if [ ! -f "${RA_ENV_FILE}" ]; then
            ( umask 077; : > "${RA_ENV_FILE}" )
            chown user:user "${RA_ENV_FILE}"
            chmod 0600 "${RA_ENV_FILE}"
        fi
        sed -i "/^${key}=/d" "${RA_ENV_FILE}"
        local quoted="${val//\'/\'\\\'\'}"
        printf "%s='%s'\n" "${key}" "${quoted}" >> "${RA_ENV_FILE}"
    }
    ra_env_unset() {
        local key="$1"
        [ -f /etc/environment ] && sed -i "/^${key}=/d" /etc/environment
        [ -f "${RA_ENV_FILE}" ] && sed -i "/^${key}=/d" "${RA_ENV_FILE}"
        return 0
    }
fi

_clear_stale_state() {
    rm -f "${APIKEY_HELPER_PATH}"
    # Clear prior auth from BOTH /etc/environment and /run/ra/env so a provider
    # switch can't leave a stale token where login shells still source it.
    ra_env_unset CLAUDE_CODE_OAUTH_TOKEN
    ra_env_unset ANTHROPIC_API_KEY

    if [[ -f "${CLAUDE_SETTINGS_PATH}" ]] && command -v jq >/dev/null 2>&1; then
        if jq -e '.apiKeyHelper' "${CLAUDE_SETTINGS_PATH}" >/dev/null 2>&1; then
            # Handle a jq failure explicitly so a bare `jq ... && mv` can't
            # return non-zero and trip `set -e`; leave the file as-is on error.
            local tmp; tmp=$(mktemp)
            if jq 'del(.apiKeyHelper)' "${CLAUDE_SETTINGS_PATH}" > "${tmp}" 2>/dev/null; then
                mv "${tmp}" "${CLAUDE_SETTINGS_PATH}"
                chown "${USER_NAME}:${USER_NAME}" "${CLAUDE_SETTINGS_PATH}"
            else
                rm -f "${tmp}"
                log "could not strip apiKeyHelper; leaving settings.json as-is"
            fi
        fi
    fi
}

_seed_onboarding() {
    local token="$1"
    local version
    version=$(sudo -u "${USER_NAME}" "${CLAUDE_BIN_PATH}" --version 2>/dev/null \
        | awk '{print $1}' || true)
    [[ "${version}" =~ ^[0-9]+\.[0-9]+\.[0-9]+ ]] || version=""

    local tmp; tmp=$(mktemp)
    if [[ -f "${CLAUDE_CONFIG_PATH}" ]]; then
        jq --arg ver "${version}" '
            .hasCompletedOnboarding = true
            | (if $ver != "" then .lastOnboardingVersion = $ver else . end)
        ' "${CLAUDE_CONFIG_PATH}" > "${tmp}" \
            || { rm -f "${tmp}"; log "jq merge failed"; return 0; }
    else
        jq -n --arg ver "${version}" '
            {hasCompletedOnboarding: true}
            | (if $ver != "" then .lastOnboardingVersion = $ver else . end)
        ' > "${tmp}" \
            || { rm -f "${tmp}"; log "jq render failed"; return 0; }
    fi
    mv "${tmp}" "${CLAUDE_CONFIG_PATH}"
    chown "${USER_NAME}:${USER_NAME}" "${CLAUDE_CONFIG_PATH}"
    chmod 600 "${CLAUDE_CONFIG_PATH}"

    if sudo -u "${USER_NAME}" env CLAUDE_CODE_OAUTH_TOKEN="${token}" \
            "${CLAUDE_BIN_PATH}" -p "respond with OK" 2>/dev/null | grep -q 'OK'; then
        log "smoke test OK"
    else
        log "WARNING: smoke test did not return OK; claude auth may be misconfigured"
    fi
}

_clear_stale_state

case "${RA_PLUGIN_CLAUDE_AUTH_PROVIDER:-}" in
    oauth)
        if [[ -z "${CLAUDE_CODE_OAUTH_TOKEN:-}" ]]; then
            log "CLAUDE_CODE_OAUTH_TOKEN not set; auth may fail"
        else
            ra_env_set CLAUDE_CODE_OAUTH_TOKEN "${CLAUDE_CODE_OAUTH_TOKEN}"
            log "exported CLAUDE_CODE_OAUTH_TOKEN"
            _seed_onboarding "${CLAUDE_CODE_OAUTH_TOKEN}"
        fi
        ;;
    apikey)
        if [[ -z "${ANTHROPIC_API_KEY:-}" ]]; then
            log "ANTHROPIC_API_KEY not set; auth may fail"
        else
            cat > "${APIKEY_HELPER_PATH}" <<EOF
#!/bin/bash
echo "${ANTHROPIC_API_KEY}"
EOF
            chmod 0755 "${APIKEY_HELPER_PATH}"
            mkdir -p "${CLAUDE_SETTINGS_DIR}"
            chown "${USER_NAME}:${USER_NAME}" "${CLAUDE_SETTINGS_DIR}"
            if [[ ! -f "${CLAUDE_SETTINGS_PATH}" ]]; then
                printf '{"apiKeyHelper": "%s"}\n' "${APIKEY_HELPER_PATH}" > "${CLAUDE_SETTINGS_PATH}"
            elif command -v jq >/dev/null 2>&1; then
                tmp=$(mktemp)
                if jq --arg h "${APIKEY_HELPER_PATH}" '.apiKeyHelper = $h' \
                        "${CLAUDE_SETTINGS_PATH}" > "${tmp}" 2>/dev/null; then
                    mv "${tmp}" "${CLAUDE_SETTINGS_PATH}"
                else
                    rm -f "${tmp}"
                    log "WARNING: could not update ${CLAUDE_SETTINGS_PATH} apiKeyHelper; apikey auth may fail"
                fi
            else
                # settings.json exists but jq is unavailable, so apiKeyHelper
                # can't be merged in without clobbering existing keys. Surface
                # it instead of silently leaving auth unconfigured.
                log "WARNING: jq unavailable and ${CLAUDE_SETTINGS_PATH} exists; cannot set apiKeyHelper, apikey auth may fail"
            fi
            chown "${USER_NAME}:${USER_NAME}" "${CLAUDE_SETTINGS_PATH}"
            log "configured apikey mode"
        fi
        ;;
    *)
        log "unknown auth provider '${RA_PLUGIN_CLAUDE_AUTH_PROVIDER:-}'"
        ;;
esac
