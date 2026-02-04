#!/bin/bash
# Temporary test script to bypass version check and run CT tests

echo "Bypassing version check and running Common Tests..."

# Set environment to bypass version check
export ERLANG_COOKIE="test_cookie"
export ERLMCP_ENV="test"

# Change to workspace directory
cd /workspace

# Run rebar3 ct directly
echo "Running rebar3 ct..."
rebar3 ct apps/erlmcp_new_features

echo "Common Test completed with exit code: $?"