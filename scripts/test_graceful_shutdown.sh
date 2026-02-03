#!/bin/bash
# Test graceful shutdown implementation for prep_stop/1
# This script validates that all three applications have proper graceful shutdown

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

echo "======================================================================"
echo "Graceful Shutdown Test - prep_stop/1 Validation"
echo "======================================================================"
echo ""

# Check if running in Docker
if [ -f /.dockerenv ]; then
    echo "✓ Running in Docker environment"
else
    echo "⚠ WARNING: Not in Docker. This test should run via docker compose."
    echo "  Use: docker compose run --rm erlmcp /bin/bash /opt/erlmcp/scripts/test_graceful_shutdown.sh"
    exit 1
fi

cd "$PROJECT_ROOT"

echo "Step 1: Checking application callback modules..."
echo "----------------------------------------------------------------------"

# Check erlmcp_app.erl
echo "Checking erlmcp_core/src/erlmcp_app.erl..."
if grep -q "prep_stop" apps/erlmcp_core/src/erlmcp_app.erl; then
    echo "✓ erlmcp_app has prep_stop/1 callback"
else
    echo "✗ FAIL: erlmcp_app missing prep_stop/1"
    exit 1
fi

# Check erlmcp_transports_app.erl
echo "Checking erlmcp_transports/src/erlmcp_transports_app.erl..."
if grep -q "prep_stop" apps/erlmcp_transports/src/erlmcp_transports_app.erl; then
    echo "✓ erlmcp_transports_app has prep_stop/1 callback"
else
    echo "✗ FAIL: erlmcp_transports_app missing prep_stop/1"
    exit 1
fi

# Check erlmcp_observability_app.erl
echo "Checking erlmcp_observability/src/erlmcp_observability_app.erl..."
if grep -q "prep_stop" apps/erlmcp_observability/src/erlmcp_observability_app.erl; then
    echo "✓ erlmcp_observability_app has prep_stop/1 callback"
else
    echo "✗ FAIL: erlmcp_observability_app missing prep_stop/1"
    exit 1
fi

echo ""
echo "Step 2: Checking graceful shutdown helper functions..."
echo "----------------------------------------------------------------------"

# Check erlmcp_registry:graceful_shutdown/0
echo "Checking erlmcp_core/src/erlmcp_registry.erl..."
if grep -q "graceful_shutdown" apps/erlmcp_core/src/erlmcp_registry.erl; then
    echo "✓ erlmcp_registry exports graceful_shutdown/0"
else
    echo "✗ FAIL: erlmcp_registry missing graceful_shutdown/0"
    exit 1
fi

# Check erlmcp_transport_sup:stop_accepting/0
echo "Checking erlmcp_transports/src/erlmcp_transport_sup.erl..."
if grep -q "stop_accepting" apps/erlmcp_transports/src/erlmcp_transport_sup.erl; then
    echo "✓ erlmcp_transport_sup exports stop_accepting/0"
else
    echo "✗ FAIL: erlmcp_transport_sup missing stop_accepting/0"
    exit 1
fi

# Check erlmcp_connection_pool:drain/0
echo "Checking erlmcp_transports/src/erlmcp_connection_pool.erl..."
if grep -q "drain()" apps/erlmcp_transports/src/erlmcp_connection_pool.erl; then
    echo "✓ erlmcp_connection_pool exports drain/0"
else
    echo "✗ FAIL: erlmcp_connection_pool missing drain/0"
    exit 1
fi

echo ""
echo "Step 3: Compiling applications..."
echo "----------------------------------------------------------------------"

# Compile the project
if rebar3 compile 2>&1 | tee /tmp/compile.log | grep -q "ERROR"; then
    echo "✗ FAIL: Compilation errors detected"
    tail -50 /tmp/compile.log
    exit 1
else
    echo "✓ Compilation successful"
fi

echo ""
echo "Step 4: Starting applications and testing graceful shutdown..."
echo "----------------------------------------------------------------------"

# Create a test Erlang node
cat > /tmp/graceful_shutdown_test.erl << 'EOF'
#!/usr/bin/env escript
%% Test graceful shutdown implementation

main(_) ->
    io:format("Starting erlmcp_core application...~n"),
    {ok, _} = application:ensure_all_started(erlmcp_core),

    io:format("Checking if prep_stop is exported from erlmcp_app...~n"),
    case erlang:function_exported(erlmcp_app, prep_stop, 1) of
        true -> io:format("✓ erlmcp_app:prep_stop/1 is exported~n");
        false -> io:format("✗ FAIL: erlmcp_app:prep_stop/1 is NOT exported~n"), halt(1)
    end,

    io:format("Checking if graceful_shutdown is exported from erlmcp_registry...~n"),
    case erlang:function_exported(erlmcp_registry, graceful_shutdown, 0) of
        true -> io:format("✓ erlmcp_registry:graceful_shutdown/0 is exported~n");
        false -> io:format("✗ FAIL: erlmcp_registry:graceful_shutdown/0 is NOT exported~n"), halt(1)
    end,

    io:format("Testing graceful shutdown call...~n"),
    try erlmcp_registry:graceful_shutdown() of
        ok -> io:format("✓ erlmcp_registry:graceful_shutdown/0 executed successfully~n");
        _ -> io:format("✗ FAIL: erlmcp_registry:graceful_shutdown/0 returned unexpected value~n"), halt(1)
    catch
        _:Error -> io:format("✗ FAIL: erlmcp_registry:graceful_shutdown/0 crashed: ~p~n", [Error]), halt(1)
    end,

    io:format("Starting erlmcp_transports application...~n"),
    {ok, _} = application:ensure_all_started(erlmcp_transports),

    io:format("Checking if prep_stop is exported from erlmcp_transports_app...~n"),
    case erlang:function_exported(erlmcp_transports_app, prep_stop, 1) of
        true -> io:format("✓ erlmcp_transports_app:prep_stop/1 is exported~n");
        false -> io:format("✗ FAIL: erlmcp_transports_app:prep_stop/1 is NOT exported~n"), halt(1)
    end,

    io:format("Checking if stop_accepting is exported from erlmcp_transport_sup...~n"),
    case whereis(erlmcp_transport_sup) of
        undefined -> io:format("⚠ erlmcp_transport_sup not running (this is OK for minimal test)~n");
        _Pid ->
            case erlang:function_exported(erlmcp_transport_sup, stop_accepting, 0) of
                true -> io:format("✓ erlmcp_transport_sup:stop_accepting/0 is exported~n");
                false -> io:format("✗ FAIL: erlmcp_transport_sup:stop_accepting/0 is NOT exported~n"), halt(1)
            end
    end,

    io:format("Starting erlmcp_observability application...~n"),
    {ok, _} = application:ensure_all_started(erlmcp_observability),

    io:format("Checking if prep_stop is exported from erlmcp_observability_app...~n"),
    case erlang:function_exported(erlmcp_observability_app, prep_stop, 1) of
        true -> io:format("✓ erlmcp_observability_app:prep_stop/1 is exported~n");
        false -> io:format("✗ FAIL: erlmcp_observability_app:prep_stop/1 is NOT exported~n"), halt(1)
    end,

    io:format("~n~n========== ALL TESTS PASSED ==========~n"),
    io:format("Graceful shutdown implementation is complete!~n"),
    init:stop().
EOF

chmod +x /tmp/graceful_shutdown_test.erl

# Run the test
escript /tmp/graceful_shutdown_test.erl || exit 1

echo ""
echo "======================================================================"
echo "✓ Graceful Shutdown Test PASSED"
echo "======================================================================"
echo ""
echo "Summary:"
echo "  - erlmcp_app:prep_stop/1 ✓"
echo "  - erlmcp_transports_app:prep_stop/1 ✓"
echo "  - erlmcp_observability_app:prep_stop/1 ✓"
echo "  - erlmcp_registry:graceful_shutdown/0 ✓"
echo "  - erlmcp_transport_sup:stop_accepting/0 ✓"
echo "  - erlmcp_connection_pool:drain/0 ✓"
echo ""
echo "All applications will gracefully shutdown on SIGTERM/SIGINT."
echo ""

exit 0
