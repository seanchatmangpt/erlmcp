#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa _build/default/lib/*/ebin

-mode(compile).

main([NumClientsStr]) ->
    NumClients = list_to_integer(NumClientsStr),
    
    io:format("~n=== CONCURRENT STATE CORRUPTION CRASH TEST ===~n", []),
    io:format("Test ID: corruption_test_~p~n", [NumClients]),
    io:format("Concurrent Writers: ~p~n", [NumClients]),
    io:format("Target: Shared registry state~n~n", []),
    
    % Start applications
    {ok, _} = application:ensure_all_started(erlmcp_core),
    io:format("Core application started~n", []),
    
    % Run the test
    TestId = <<"corruption_test_", (list_to_binary(NumClientsStr))/binary>>,
    erlmcp_bench_state_corruption:run(TestId, NumClients),
    
    io:format("~nTest complete~n", []),
    ok;
main(_) ->
    io:format("Usage: run_corruption_test.erl <num_clients>~n", []),
    io:format("Example: run_corruption_test.erl 10000~n", []),
    halt(1).
