#!/bin/bash
set -e

# ============================================================================
# Race Condition Bombardment Test Runner
# DESTRUCTIVE TEST #12: Extreme concurrent modification testing
# ============================================================================

cd "$(dirname "$0")/../"

echo "========================================================================"
echo "RACE CONDITION BOMBARDMENT CRASH TEST"
echo "========================================================================"
echo ""
echo "Test Protocol:"
echo "1. Spawn MCP server with shared ETS counter"
echo "2. Launch 10K→100K concurrent client processes"
echo "3. Each client performs 1K operations (10M→100M total)"
echo "4. Mix: 70% increment, 20% read, 10% set"
echo "5. No locking, no transactions - pure race chaos"
echo "6. Measure: corruption, lost updates, inconsistent reads"
echo ""
echo "⚠️  WARNING: This will attempt to corrupt ETS tables"
echo "========================================================================"
echo ""

# Ensure compilation
echo "Step 1: Compiling race condition test module..."
TERM=dumb rebar3 compile 2>&1 | tail -5

echo ""
echo "Step 2: Starting Erlang node..."
echo ""

# Create the test runner script
cat > /tmp/race_test_runner.erl << 'ERLANG'
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa _build/default/lib/*/ebin -smp enable -kernel shell_history enabled

main(_) ->
    io:format("~n=== RACE CONDITION BOMBARDMENT CRASH TEST ===~n~n"),
    
    io:format("Test Configuration:~n"),
    io:format("  Shared Resource: ETS counter~n"),
    io:format("  Initial Value: 0~n"),
    io:format("  Concurrent Clients: 10K → 100K~n"),
    io:format("  Operations per Client: 1,000~n"),
    io:format("  Total Operations: 10M → 100M~n"),
    io:format("~n"),
    
    io:format("Operation Mix:~n"),
    io:format("  Increment: 70% (should be 7M)~n"),
    io:format("  Read: 20%~n"),
    io:format("  Set: 10%~n"),
    io:format("~n"),
    
    case code:load_file(erlmcp_bench_race_conditions) of
        {module, erlmcp_bench_race_conditions} ->
            io:format("✓ Module loaded successfully~n~n"),
            
            % Run all tests
            io:format("Starting race condition bombardment...~n~n"),
            
            case erlmcp_bench_race_conditions:run_all_tests() of
                {ok, Results} ->
                    io:format("~n✓ All tests completed~n~n"),
                    print_detailed_results(Results),
                    halt(0);
                {error, Reason} ->
                    io:format("✗ Test failed: ~p~n", [Reason]),
                    halt(1)
            end;
        {error, Reason} ->
            io:format("✗ Failed to load module: ~p~n", [Reason]),
            io:format("Ensure erlmcp_bench_race_conditions.erl is compiled~n"),
            halt(1)
    end.

print_detailed_results(Results) ->
    io:format("=== DETAILED RESULTS ===~n~n"),
    
    lists:foreach(fun(Result) ->
        print_result(Result)
    end, Results),
    
    TotalPassed = length([R || R <- Results, maps:get(<<"test_passed">>, R, false)]),
    TotalTests = length(Results),
    
    io:format("~n=== SUMMARY ===~n"),
    io:format("Total Tests: ~p~n", [TotalTests]),
    io:format("Passed: ~p~n", [TotalPassed]),
    io:format("Failed: ~p~n", [TotalTests - TotalPassed]),
    
    case TotalPassed of
        TotalTests ->
            io:format("~n✓ NO CORRUPTION DETECTED - System is race-condition safe!~n");
        _ ->
            io:format("~n✗ CORRUPTION DETECTED - See details above~n")
    end.

print_result(Result) ->
    TestId = maps:get(<<"test">>, Result),
    Clients = maps:get(<<"concurrent_clients">>, Result),
    Ops = maps:get(<<"total_operations">>, Result),
    Expected = maps:get(<<"expected_final_value">>, Result),
    Actual = maps:get(<<"actual_final_value">>, Result),
    Lost = maps:get(<<"lost_updates">>, Result),
    NegReads = maps:get(<<"negative_values_read">>, Result),
    Impossible = maps:get(<<"impossible_values">>, Result),
    Inconsistent = maps:get(<<"inconsistent_reads">>, Result),
    Corruption = maps:get(<<"ets_corruption">>, Result),
    ClientCrashes = maps:get(<<"client_crashes">>, Result),
    ServerCrashes = maps:get(<<"server_crashes">>, Result),
    EtsErrors = maps:get(<<"ets_errors">>, Result),
    Duration = maps:get(<<"test_duration_s">>, Result),
    OpsPerSec = maps:get(<<"ops_per_second">>, Result),
    
    io:format("Test: ~s~n", [TestId]),
    io:format("  Configuration:~n"),
    io:format("    Concurrent Clients: ~p~n", [Clients]),
    io:format("    Operations per Client: ~p~n", [round(Ops / Clients)]),
    io:format("    Total Operations: ~p~n", [Ops]),
    io:format("~n"),
    
    io:format("  RESULTS:~n"),
    io:format("    Expected Final Value: ~p~n", [Expected]),
    io:format("    Actual Final Value: ~p~n", [Actual]),
    io:format("    Lost Updates: ~p (~.2f%)~n", [Lost, (Lost / Expected * 100)]),
    io:format("    Update Success Rate: ~.2f%~n", [100 - (Lost / Expected * 100)]),
    io:format("~n"),
    
    io:format("  CORRUPTION DETECTED:~n"),
    io:format("    Negative Values Read: ~p~n", [NegReads]),
    io:format("    Impossible Values: ~p~n", [Impossible]),
    io:format("    Inconsistent Reads: ~p~n", [Inconsistent]),
    io:format("    ETS Corruption: ~p~n", [Corruption]),
    io:format("~n"),
    
    io:format("  READ CONSISTENCY:~n"),
    TotalReads = round(Ops * 0.2),
    ReadConsistency = case TotalReads of
        0 -> 100.0;
        _ -> 100 - (Inconsistent / TotalReads * 100)
    end,
    io:format("    Reads Saw Zero: unknown~n"),
    io:format("    Reads Saw Negative: ~p~n", [NegReads]),
    io:format("    Reads Saw Unexpected: ~p~n", [Impossible]),
    io:format("    Read Consistency: ~.2f%~n", [ReadConsistency]),
    io:format("~n"),
    
    io:format("  PROCESS FAILURES:~n"),
    io:format("    Client Crashes: ~p~n", [ClientCrashes]),
    io:format("    Server Crashes: ~p~n", [ServerCrashes]),
    io:format("    ETS Errors: ~p~n", [EtsErrors]),
    io:format("~n"),
    
    io:format("  PERFORMANCE:~n"),
    io:format("    Test Duration: ~.2f seconds~n", [Duration]),
    io:format("    Operations/Second: ~.0f~n", [OpsPerSec]),
    io:format("~n"),
    
    case maps:get(<<"test_passed">>, Result, false) of
        true ->
            io:format("  ✓ PASSED - No corruption detected~n");
        false ->
            io:format("  ✗ FAILED - Race conditions detected!~n")
    end,
    
    io:format("~n").
ERLANG

# Run the test
echo "Step 3: Executing race condition test..."
escript /tmp/race_test_runner.erl

echo ""
echo "========================================================================"
echo "Race condition bombardment test complete!"
echo "========================================================================"
echo ""
echo "Results saved to: bench/results/race_conditions_report_*.json"
echo ""
