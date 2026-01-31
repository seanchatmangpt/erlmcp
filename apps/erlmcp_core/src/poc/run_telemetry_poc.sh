#!/bin/bash
# ============================================================================
# run_telemetry_poc.sh - Run Telemetry POC Demo
# ============================================================================

set -e

echo "========================================"
echo "ERLMCP Telemetry POC Demo"
echo "========================================"
echo ""

# Check for rebar3
if ! command -v rebar3 &> /dev/null; then
    echo "ERROR: rebar3 not found in PATH"
    echo "Install rebar3 from: https://www.rebar3.org/"
    exit 1
fi

# Step 1: Fetch dependencies
echo "Step 1: Fetching dependencies (including telemetry)..."
rebar3 get-deps
echo "✓ Dependencies fetched"
echo ""

# Step 2: Compile project
echo "Step 2: Compiling erlmcp with telemetry POC..."
TERM=dumb rebar3 compile
echo "✓ Compilation complete"
echo ""

# Step 3: Run demo
echo "Step 3: Starting Erlang shell and running demo..."
echo ""
echo "Commands to run in the Erlang shell:"
echo "  erlmcp_telemetry_poc:run_demo()."
echo ""
echo "Press Ctrl+C twice to exit the shell."
echo ""

# Start Erlang shell with all apps loaded
erl -pa _build/default/lib/*/ebin \
    -eval "application:ensure_all_started(erlmcp_core)" \
    -eval "application:ensure_all_started(telemetry)" \
    -eval "erlmcp_telemetry_poc:run_demo()"
