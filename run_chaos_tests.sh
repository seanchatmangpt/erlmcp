#!/bin/bash
# run_chaos_tests.sh - Run erlmcp chaos tests (standalone)
# Works around rebar3 build issues by compiling modules directly

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
EUNIT_INCLUDE="/usr/local/lib/erlang/lib/eunit-*/include"

echo "========================================="
echo "erlmcp Chaos Tests - Standalone Runner"
echo "========================================="
echo ""

# Clean previous build
echo "Cleaning previous build..."
rm -rf "$SCRIPT_DIR/ebin"
mkdir -p "$SCRIPT_DIR/ebin"

echo "Compiling chaos modules..."
echo ""

# Compile refusal module (dependency)
echo "1. Compiling erlmcp_refusal..."
erlc -I "$SCRIPT_DIR/apps/erlmcp_core/include" \
     -o "$SCRIPT_DIR/ebin" \
     "$SCRIPT_DIR/apps/erlmcp_core/src/erlmcp_refusal.erl" || {
    echo "Failed to compile erlmcp_refusal"
    exit 1
}

# Compile chaos modules
echo "2. Compiling erlmcp_chaos..."
erlc -I "$SCRIPT_DIR/apps/erlmcp_core/include" \
     -I "$SCRIPT_DIR/apps/erlmcp_observability/include" \
     -o "$SCRIPT_DIR/ebin" \
     "$SCRIPT_DIR/apps/erlmcp_observability/src/erlmcp_chaos.erl" || {
    echo "Failed to compile erlmcp_chaos"
    exit 1
}

echo "3. Compiling erlmcp_chaos_network..."
erlc -I "$SCRIPT_DIR/apps/erlmcp_core/include" \
     -I "$SCRIPT_DIR/apps/erlmcp_observability/include" \
     -o "$SCRIPT_DIR/ebin" \
     "$SCRIPT_DIR/apps/erlmcp_observability/src/erlmcp_chaos_network.erl" || {
    echo "Failed to compile erlmcp_chaos_network"
    exit 1
}

echo "4. Compiling erlmcp_chaos_process..."
erlc -I "$SCRIPT_DIR/apps/erlmcp_core/include" \
     -I "$SCRIPT_DIR/apps/erlmcp_observability/include" \
     -o "$SCRIPT_DIR/ebin" \
     "$SCRIPT_DIR/apps/erlmcp_observability/src/erlmcp_chaos_process.erl" || {
    echo "Failed to compile erlmcp_chaos_process"
    exit 1
}

echo "5. Compiling erlmcp_chaos_resource..."
erlc -I "$SCRIPT_DIR/apps/erlmcp_core/include" \
     -I "$SCRIPT_DIR/apps/erlmcp_observability/include" \
     -o "$SCRIPT_DIR/ebin" \
     "$SCRIPT_DIR/apps/erlmcp_observability/src/erlmcp_chaos_resource.erl" || {
    echo "Failed to compile erlmcp_chaos_resource"
    exit 1
}

echo "6. Compiling erlmcp_chaos_tests..."
erlc -I "$SCRIPT_DIR/apps/erlmcp_core/include" \
     -I "$SCRIPT_DIR/apps/erlmcp_observability/include" \
     -I "$EUNIT_INCLUDE" \
     -o "$SCRIPT_DIR/ebin" \
     "$SCRIPT_DIR/apps/erlmcp_observability/test/erlmcp_chaos_tests.erl" || {
    echo "Failed to compile erlmcp_chaos_tests"
    exit 1
}

echo ""
echo "========================================="
echo "Running chaos tests..."
echo "========================================="
echo ""

# Run tests
erl -pa "$SCRIPT_DIR/ebin" \
    -pa "$EUNIT_INCLUDE" \
    -eval "
        case eunit:test(erlmcp_chaos_tests, [verbose]) of
            ok -> io:format('~n~n=== ALL TESTS PASSED ===~n~n');
            error -> io:format('~n~n=== TESTS FAILED ===~n~n'), halt(1)
        end
    " \
    -noshell \
    -s init stop

echo ""
echo "========================================="
echo "Test run complete!"
echo "========================================="
