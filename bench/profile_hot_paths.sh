#!/bin/bash
# ERLMCP Performance Profiling Script
# Profiles hot paths to identify bottlenecks

set -e

echo "=== ERLMCP Hot Path Profiling ==="
echo "Date: $(date)"
echo "OTP: $(erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().' -noshell)"
echo ""

# Create results directory
RESULT_DIR="bench/results/profile_$(date +%Y%m%d_%H%M%S)"
mkdir -p "$RESULT_DIR"

echo "Results will be saved to: $RESULT_DIR"
echo ""

# Check if fprof trace exists
if [ -f "fprof.trace" ]; then
    echo "Existing fprof.trace found, backing up..."
    mv fprof.trace "fprof.trace.backup.$(date +%s)"
fi

# Run profiling
echo "Step 1: Starting Erlang node for profiling..."
erl -pa _build/default/lib/*/ebin \
    -eval "
        application:ensure_all_started(gproc),
        
        % Profile registry operations
        {ok, RegistryPid} = erlmcp_registry:start_link(),
        
        io:format('Starting fprof trace...~n'),
        fprof:trace([start, {procs, [RegistryPid]}]),
        
        % Run 10K unregistrations (bottleneck operation)
        io:format('Running 10K unregistrations...~n'),
        lists:foreach(fun(N) ->
            ServerId = list_to_binary([\"test_\", integer_to_list(N)]),
            try
                gproc:reg({n, l, {mcp, server, ServerId}}, self(), #{}),
                gproc:unreg({n, l, {mcp, server, ServerId}})
            catch
                _:_ -> ok
            end
        end, lists:seq(1, 10000)),
        
        io:format('Stopping fprof trace...~n'),
        fprof:profile(),
        fprof:analyse([{dest, \\\"$RESULT_DIR/registry_analysis.txt\\\"}, {cols, 120}]),
        
        io:format('Profiling complete!~n'),
        halt()
    " -noshell

echo ""
echo "Step 2: Analysis complete. Results:"
echo "  - $RESULT_DIR/registry_analysis.txt"
echo ""

# Extract top functions
echo "=== Top 20 Functions by Own Time ==="
grep -A 100 "CNT" "$RESULT_DIR/registry_analysis.txt" | head -25

echo ""
echo "=== Profiling Complete ==="
echo "Full analysis: $RESULT_DIR/registry_analysis.txt"
echo ""
echo "To visualize:"
echo "  cat $RESULT_DIR/registry_analysis.txt | less"
