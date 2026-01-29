#!/bin/bash
set -e

echo "Starting destructive memory exhaustion test..."
echo "Objective: Find exact breaking point of MCP server"
echo "NO MERCY - PUSH UNTIL CRASH"
echo ""

# Start Erlang node
erl -pa _build/default/lib/*/ebin \
    -pa _build/default/lib/*/include \
    -pa /Users/sac/erlmcp/test \
    -smp enable \
    -setcookie destructivetest \
    -sname destructivetest \
    -eval "
        io:format('~n=== LOADING DESTRUCTIVE TEST MODULE ===~n~n'),
        code:load_file(destructive_memory_exhaustion_test),
        io:format('Module loaded successfully~n~n'),
        io:format('~n=== STARTING DESTRUCTIVE TEST ===~n~n'),
        try
            destructive_memory_exhaustion_test:run(),
            io:format('~n=== TEST COMPLETE ===~n')
        catch
            Class:Reason:Stacktrace ->
                io:format('~n=== TEST CRASHED: ~p:~p ===~n', [Class, Reason]),
                io:format('Stacktrace: ~p~n', [Stacktrace])
        end,
        timer:sleep(1000),
        init:stop()
    " 2>&1 | tee /tmp/destructive_memory_test.log

echo ""
echo "Test complete. Log saved to /tmp/destructive_memory_test.log"
