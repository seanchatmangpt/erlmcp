#!/usr/bin/env bash
# erlmcp.profile - Environment setup for OTP 28.3.1 and rebar3
# Source this file to use the custom OTP 28.3.1 installation

export ERLMCP_ROOT="${ERLMCP_ROOT:-.}"
export ERLMCP_OTP_BIN="${ERLMCP_ROOT}/.erlmcp/otp-28.3.1/bin"
export PATH="${ERLMCP_OTP_BIN}:${PATH}"

# Cloud-specific settings
export CLAUDE_CODE_REMOTE=true
export ERLMCP_PROFILE=cloud
export ERLMCP_CACHE="${ERLMCP_ROOT}/.erlmcp/cache/"
export TERM=dumb
export REBAR_COLOR=none
export ERL_AFLAGS="-kernel shell_history enabled"

# Verify OTP is available
if [[ -f "${ERLMCP_OTP_BIN}/erl" ]]; then
    OTP_VERSION=$("${ERLMCP_OTP_BIN}/erl" -noshell -eval 'io:format("~s", [erlang:system_info(otp_release)]), halt().' 2>/dev/null)
    if [[ "$OTP_VERSION" == "28" ]]; then
        echo "✓ OTP 28.3.1 ready (via ${ERLMCP_OTP_BIN})"
    else
        echo "✗ OTP version mismatch: $OTP_VERSION (expected 28.x)"
    fi
else
    echo "✗ OTP binary not found at ${ERLMCP_OTP_BIN}/erl"
fi

mkdir -p "$ERLMCP_CACHE"
