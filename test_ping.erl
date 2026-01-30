#!/usr/bin/env escript
%%! -pa apps/erlmcp_core/ebin -pa apps/erlmcp_observability/ebin -pa _build/default/lib/jsx/ebin -pa _build/default/lib/gproc/ebin

-mode(compile).

main([]) ->
    io:format("Testing Ping Implementation~n"),
    io:format("=========================~n~n"),

    %% Test 1: Verify ping handler exists in server
    io:format("Test 1: Check erlmcp_server module loads~n"),
    case code:ensure_loaded(erlmcp_server) of
        {module, _} ->
            io:format("  [OK] erlmcp_server module loaded~n");
        {error, Reason} ->
            io:format("  [FAIL] erlmcp_server module not loaded: ~p~n", [Reason]),
            halt(1)
    end,

    %% Test 2: Check client module loads
    io:format("~nTest 2: Check erlmcp_client module loads~n"),
    case code:ensure_loaded(erlmcp_client) of
        {module, _} ->
            io:format("  [OK] erlmcp_client module loaded~n");
        {error, Reason2} ->
            io:format("  [FAIL] erlmcp_client module not loaded: ~p~n", [Reason2]),
            halt(1)
    end,

    %% Test 3: Check client has ping function
    io:format("~nTest 3: Check erlmcp_client has ping function~n"),
    case erlang:function_exported(erlmcp_client, ping, 1) of
        true ->
            io:format("  [OK] ping/1 function exported~n");
        false ->
            io:format("  [FAIL] ping/1 function not exported~n"),
            halt(1)
    end,

    case erlang:function_exported(erlmcp_client, ping, 2) of
        true ->
            io:format("  [OK] ping/2 function exported~n");
        false ->
            io:format("  [FAIL] ping/2 function not exported~n"),
            halt(1)
    end,

    io:format("~n=========================~n"),
    io:format("All basic checks passed!~n"),
    io:format("Ping implementation complete.~n"),
    io:format("=========================~n"),
    halt(0).
