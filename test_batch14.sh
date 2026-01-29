#!/bin/bash
# Batch 14 Resource Monitoring Test Script
# Tests MCP server/client resource monitoring with change notifications

echo "=== Batch 14 Resource Monitoring Test ==="
echo "Testing MCP servers 66-70 with resource monitoring..."
echo ""

# Start Erlang node and run test
erl -pa _build/default/lib/*/ebin -pa _build/test/lib/*/ebin \
    -eval "application:ensure_all_started(erlmcp_core)" \
    -eval "application:ensure_all_started(erlmcp_transports)" \
    -eval "eunit:test(erlmcp_roundtrip_batch14_tests, [verbose])" \
    -s init stop \
    -noshell

echo ""
echo "=== Batch 14 Test Complete ==="
