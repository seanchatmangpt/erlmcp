#!/bin/bash
# Run timeout storm destructive test

echo "========================================"
echo "DESTRUCTIVE STRESS TEST #10: TIMEOUT STORM"
echo "========================================"
echo ""
echo "OBJECTIVE: Send 100,000 requests with 1ms timeout simultaneously"
echo "Overwhelm timeout handlers and find breaking point"
echo ""
echo "Test Protocol:"
echo "1. Spawn MCP server on port 10010 with slow tools (10s delay)"
echo "2. Send 100K-1M concurrent requests with 1ms timeout"
echo "3. All requests timeout simultaneously"
echo "4. Monitor: pending requests, timeout queue, memory, deadlock"
echo ""
echo "Press Enter to continue or Ctrl+C to abort..."
read

echo ""
echo "Starting timeout storm test..."
echo ""

# Run the test
rebar3 ct --suite=erlmcp_timeout_storm --verbose

echo ""
echo "========================================"
echo "TEST COMPLETE"
echo "========================================"
echo ""
echo "Results saved to: _build/test/logs/"
echo ""
