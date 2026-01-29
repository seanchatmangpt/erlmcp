#!/bin/bash

# Binary Heap Exhaustion Stress Test Runner
# DESTRUCTIVE TEST - Will crash the VM

set -e

echo "========================================================================"
echo "DESTRUCTIVE STRESS TEST #6: Binary Heap Exhaustion"
echo "========================================================================"
echo ""
echo "WARNING: This test will allocate binary heap until VM crash."
echo "Do NOT run in production environments!"
echo ""
read -p "Continue? (y/N): " -n 1 -r
echo ""

if [[ ! $REPLY =~ ^[Yy]$ ]]; then
    echo "Test aborted."
    exit 1
fi

# Change to script directory
cd "$(dirname "$0")"

# Compile the module
echo "Compiling erlmcp_bench_binary_exhaustion..."
erlc -I ../../include -o ../../_build/default/lib/erlmcp/ebin/ erlmcp_bench_binary_exhaustion.erl

# Check compilation
if [ ! -f "../../_build/default/lib/erlmcp/ebin/erlmcp_bench_binary_exhaustion.beam" ]; then
    echo "ERROR: Compilation failed"
    exit 1
fi

echo "Compilation successful."
echo ""

# Run options
echo "Select test intensity:"
echo "  1) Quick Test (10GB, safer)"
echo "  2) Standard Test (1TB, may crash)"
echo "  3) Crash Test (100TB, WILL crash)"
echo ""
read -p "Choice (1-3): " choice

case $choice in
    1)
        FUNCTION="quick_test()"
        ;;
    2)
        FUNCTION="run()"
        ;;
    3)
        FUNCTION="crash_test()"
        ;;
    *)
        echo "Invalid choice, using standard test"
        FUNCTION="run()"
        ;;
esac

echo ""
echo "Starting binary exhaustion test..."
echo "Function: $FUNCTION"
echo ""

# Create Erlang script to run the test
cat > /tmp/run_binary_exhaustion.erl << 'ERLSCRIPT'
#!/usr/bin/env escript

main(_) ->
    code:add_patha("_build/default/lib/erlmcp/ebin/"),
    code:add_patha("_build/default/lib/jsx/ebin/"),
    code:add_patha("_build/default/lib/gproc/ebin/"),
    code:add_patha("_build/default/lib/jesse/ebin/"),
    code:add_patha("_build/default/lib/ranch/ebin/"),
    code:add_patha("_build/default/lib/gun/ebin/"),
    code:add_patha("_build/default/lib/poolboy/ebin/"),
    
    io:format("Starting binary exhaustion test...~n"),
    
    %% Run the test based on function choice
    FunctionStr = case init:get_argument(test_function) of
        {ok, [["quick_test"]]} -> "quick_test()";
        {ok, [["crash_test"]]} -> "crash_test()";
        _ -> "run()"
    end,
    
    try
        Result = case FunctionStr of
            "quick_test()" -> erlmcp_bench_binary_exhaustion:quick_test();
            "crash_test()" -> erlmcp_bench_binary_exhaustion:crash_test();
            "run()" -> erlmcp_bench_binary_exhaustion:run()
        end,
        
        io:format("~nTest completed.~n"),
        erlmcp_bench_binary_exhaustion:print_report(Result),
        
        %% Save report to file
        ReportFile = "binary_exhaustion_report_" ++ integer_to_list(erlang:system_time(second)) ++ ".txt",
        {ok, F} = file:open(ReportFile, [write]),
        io:format(F, "~p~n", [Result]),
        file:close(F),
        
        io:format("~nReport saved to: ~s~n", [ReportFile]),
        
        %% Also save as JSON
        JsonReport = jsx:encode(Result),
        JsonFile = "binary_exhaustion_report_" ++ integer_to_list(erlang:system_time(second)) ++ ".json",
        {ok, J} = file:open(JsonFile, [write]),
        io:format(J, "~s~n", [JsonReport]),
        file:close(J),
        
        io:format("JSON report saved to: ~s~n", [JsonFile]),
        
        init:stop(0)
    catch
        Class:Reason:Stacktrace ->
            io:format("~nTest CRASHED (as expected):~n"),
            io:format("  Class: ~p~n", [Class]),
            io:format("  Reason: ~p~n", [Reason]),
            io:format("  Stacktrace: ~p~n", [Stacktrace]),
            init:stop(1)
    end.
ERLSCRIPT

# Run the test
echo "========================================"
echo "Note: If the VM crashes, the test succeeded!"
echo "========================================"
echo ""

# Determine function argument
case $choice in
    1)
        FUNC_ARG="quick_test"
        ;;
    3)
        FUNC_ARG="crash_test"
        ;;
    *)
        FUNC_ARG="standard"
        ;;
esac

escript /tmp/run_binary_exhaustion.erl -test_function $FUNC_ARG

echo ""
echo "Test completed."
echo ""
echo "Note: If the VM crashed during execution, this is EXPECTED behavior."
echo "The test is designed to exhaust binary heap until OOM crash."

