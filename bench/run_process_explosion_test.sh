#!/usr/bin/env bash
set -euo pipefail

# Process Explosion Stress Test
# DANGER: This test will push the Erlang VM to absolute limits
# DO NOT run on production systems

echo "======================================================================"
echo " DANGER: PROCESS EXPLOSION CRASH TEST"
echo "======================================================================"
echo ""
echo "This test will spawn processes until:"
echo "  - VM process limit is exceeded"
echo "  - Supervisor collapses"
echo "  - VM crashes or becomes unresponsive"
echo ""
echo "Expected duration: 10-30 minutes (or until crash)"
echo ""
read -p "Continue? (yes/no): " confirm

if [ "$confirm" != "yes" ]; then
    echo "Aborted"
    exit 0
fi

echo ""
echo "Starting process explosion test..."
echo ""

# Compile the test module
cd /Users/sac/erlmcp/apps/erlmcp_core
rebar3 compile

# Create output directory
mkdir -p /Users/sac/erlmcp/bench/results/explosion

# Run the test
erl -pa _build/default/lib/*/ebin -noshell -eval "
    io:format(\"Starting process explosion test...~n\"),
    
    % Get initial stats
    ProcessLimit = erlang:system_info(process_limit),
    InitialCount = erlang:system_info(process_count),
    io:format(\"Process Limit: ~p~n\", [ProcessLimit]),
    io:format(\"Initial Count: ~p~n\", [InitialCount]),
    io:format(\"Available: ~p~n\", [ProcessLimit - InitialCount]),
    io:format(\"~nStarting explosion...~n~n\"),
    
    % Load and run the test
    code:add_patha(\"/Users/sac/erlmcp/bench\"),
    case erlmcp_bench_process_explosion:run() of
        {ok, Result} ->
            io:format(\"~nTest complete!~n\"),
            io:format(\"Result: ~p~n\", [Result]),
            
            % Save result to file
            Timestamp = integer_to_list(os:system_time(second)),
            Filename = \"/Users/sac/erlmcp/bench/results/explosion/explosion_\" ++ Timestamp ++ \".json\",
            file:write_file(Filename, jsx:encode(Result)),
            io:format(\"~nResults saved to: ~s~n\", [Filename]),
            halt(0);
        {error, Reason} ->
            io:format(\"~nTest failed: ~p~n\", [Reason]),
            halt(1)
    end
"
