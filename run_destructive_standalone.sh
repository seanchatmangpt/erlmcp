#!/bin/bash

echo "Starting DESTRUCTIVE memory exhaustion test..."
echo "Objective: Find exact breaking point"
echo "NO MERCY - PUSH UNTIL CRASH"
echo ""

erl -pa /Users/sac/erlmcp/test \
    -smp enable \
    -eval "
        io:format('~n=== STARTING DESTRUCTIVE TEST ===~n~n'),
        try
            destructive_memory_standalone:run(),
            io:format('~n=== TEST COMPLETE ===~n')
        catch
            Class:Reason:Stacktrace ->
                io:format('~n=== TEST CRASHED: ~p:~p ===~n', [Class, Reason]),
                io:format('Stacktrace: ~p~n', [Stacktrace])
        end,
        timer:sleep(1000),
        init:stop()
    " 2>&1 | tee /tmp/destructive_standalone.log

echo ""
echo "Test complete. Log saved to /tmp/destructive_standalone.log"
