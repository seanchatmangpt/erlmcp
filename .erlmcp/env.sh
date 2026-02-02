#!/usr/bin/env bash
# erlmcp environment setup
# Auto-generated for Linux system

export ERLMCP_OTP_VERSION="28.3.1"
export ERLMCP_OTP_BIN_PATH="/home/user/erlmcp/.erlmcp/otp-28.3.1/bin"
export ERLMCP_OTP_LIB="/home/user/erlmcp/.erlmcp/otp-28.3.1/lib/erlang"
export ERLMCP_CACHE="/home/user/erlmcp/.erlmcp/cache/"

# Add OTP tools to PATH
export PATH="${ERLMCP_OTP_BIN_PATH}:${PATH}"

# Set up rebar3 environment
export REBAR_BASE_DIR="${ERLMCP_CACHE}rebar3"

# For hex.pm access
export HEX_HOME="${ERLMCP_CACHE}hex"

# Erlang environment
export ERL_TOP="/home/user/erlmcp/.erlmcp/otp-28.3.1/lib/erlang"

# Profile
export ERLMCP_PROFILE="cloud"
export CLAUDE_CODE_REMOTE="true"
