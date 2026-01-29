#!/bin/bash
# Port Exhaustion Stress Test Runner
# WARNING: This is a DESTRUCTIVE test that will exhaust TCP ports

set -e

echo "==================================================================="
echo "       PORT EXHAUSTION DESTRUCTIVE STRESS TEST"
echo "==================================================================="
echo ""
echo "WARNING: This test will exhaust OS TCP ports!"
echo "DO NOT run on production systems!"
echo ""
echo "Press Ctrl+C to cancel, or"
read -p "Press Enter to continue..."
echo ""

# Get system info
echo "=== SYSTEM INFORMATION ==="
echo "OS: $(uname -s) $(uname -r)"
echo "File descriptor limit: $(ulimit -n)"
if [[ "$OSTYPE" == "darwin"* ]]; then
    echo "Ephemeral port range: $(sysctl -n net.inet.ip.portrange.first)-$(sysctl -n net.inet.ip.portrange.last)"
elif [[ "$OSTYPE" == "linux"* ]]; then
    echo "Ephemeral port range: $(cat /proc/sys/net/ipv4/ip_local_port_range | tr '\t' '-')"
fi
echo ""

# Compile the benchmark
echo "=== COMPILING BENCHMARK ==="
erlc -I apps -I include -o . bench/erlmcp_bench_port_exhaustion.erl
echo "Compilation complete"
echo ""

# Run the test
echo "=== RUNNING PORT EXHAUSTION TEST ==="
echo ""

# Create Erlang script to run the test
cat > /tmp/run_port_exhaustion.erl << 'ERL'
#!/usr/bin/env escript
main(_) =>
    io:format("Starting port exhaustion test...~n"),
    code:add_patha("."),
    code:add_patha("_build/default/lib/*/ebin"),
    
    % Run client port exhaustion test
    Result = erlmcp_bench_port_exhaustion:run_client_exhaustion(),
    io:format("~nTest complete.~n"),
    
    % Print result summary
    io:format("Successful connections: ~p~n", [maps:get(successful_connections, Result)]),
    io:format("Failed connections: ~p~n", [maps:get(failed_connections, Result)]),
    io:format("Final error: ~p~n", [maps:get(final_error, Result)]).
ERL

# Run with escript
escript /tmp/run_port_exhaustion.erl

echo ""
echo "=== TEST COMPLETE ==="
echo "Check logs above for detailed results"
