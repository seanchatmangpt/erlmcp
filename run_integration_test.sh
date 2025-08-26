#!/bin/bash

# ErlMCP Integration Test Runner
# This script compiles and runs the integration test suite

set -e

echo "=========================================="
echo "ErlMCP Integration Test Runner"
echo "=========================================="
echo

# Compile the project first
echo "Compiling ErlMCP..."
rebar3 compile

# Compile the integration test
echo "Compiling integration test..."
erlc -I include -o test test/erlmcp_integration_test.erl

# Run the integration test
echo "Running integration test..."
echo

erl -pa _build/default/lib/*/ebin -pa test -noshell -eval "
try
    application:start(sasl),
    application:start(jsx), 
    erlmcp_integration_test:run()
catch
    Class:Error:Stack ->
        io:format(\"Critical Test Framework Error: ~p:~p~n~p~n\", [Class, Error, Stack]),
        halt(1)
end."

echo
echo "Integration test completed."