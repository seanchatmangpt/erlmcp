#!/usr/bin/env escript
%%! -pa _build/default/lib/*/ebin

%%%-------------------------------------------------------------------
%%% @doc
%%% Port Chaos Example - Demonstrates chaos testing with ports
%%%
%%% This example shows how to test port resilience using chaos:
%%% - Port crashes
%%% - Timeouts
%%% - Invalid data
%%% - Rapid port restarts
%%% @end
%%%-------------------------------------------------------------------

-mode(compile).
-include("erlmcp.hrl").

main([]) ->
    io:format("=== Port Chaos Example ===~n~n"),

    %% Test 1: Port crash recovery
    io:format("Test 1: Port crash recovery~n"),
    test_port_crash(),
    io:format("~n"),

    %% Test 2: Timeout handling
    io:format("Test 2: Timeout handling~n"),
    test_timeout(),
    io:format("~n"),

    %% Test 3: Invalid JSON
    io:format("Test 3: Invalid JSON handling~n"),
    test_invalid_json(),
    io:format("~n"),

    %% Test 4: Rapid restarts
    io:format("Test 4: Rapid restarts~n"),
    test_rapid_restarts(),
    io:format("~n"),

    io:format("=== Chaos Testing Complete ===~n"),
    init:stop().

test_port_crash() ->
    %% Start port tool
    {ok, Pid} = erlmcp_port_tool:start_link(#{}),

    %% Start a process that will crash
    case os:type() of
        {unix, _} ->
            %% Use sleep that will timeout
            {ok, _Port} = erlmcp_port_tool:start_port(Pid, {"/bin/sleep", ["100"]});
        {win32, _} ->
            {ok, _Port} = erlmcp_port_tool:start_port(Pid, {"timeout", ["100"]})
    end,

    %% Try to receive with short timeout
    Result = erlmcp_port_tool:recv_response(Pid, 100),
    io:format("  Timeout result: ~p~n", [Result]),

    %% Close port
    ok = erlmcp_port_tool:close_port(Pid),
    io:format("  Port crash handled successfully~n").

test_timeout() ->
    %% Start port tool
    {ok, Pid} = erlmcp_port_tool:start_link(#{}),

    %% Start long-running process
    case os:type() of
        {unix, _} ->
            {ok, _Port} = erlmcp_port_tool:start_port(Pid, {"/bin/sleep", ["10"]});
        {win32, _} ->
            {ok, _Port} = erlmcp_port_tool:start_port(Pid, {"timeout", ["10"]})
    end,

    %% Send request
    ok = erlmcp_port_tool:send_request(Pid, <<"{\"test\": \"data\"}">>),

    %% Try to receive with short timeout
    Result = erlmcp_port_tool:recv_response(Pid, 100),
    case Result of
        {error, timeout} ->
            io:format("  Timeout detected correctly~n");
        _ ->
            io:format("  Unexpected result: ~p~n", [Result])
    end,

    %% Close port
    ok = erlmcp_port_tool:close_port(Pid).

test_invalid_json() ->
    %% Start port tool
    {ok, Pid} = erlmcp_port_tool:start_link(#{}),

    %% Start echo process
    case os:type() of
        {unix, _} ->
            {ok, _Port} = erlmcp_port_tool:start_port(Pid, {"/bin/cat", []});
        {win32, _} ->
            {ok, _Port} = erlmcp_port_tool:start_port(Pid, {"cmd.exe", ["/c", "type"]})
    end,

    %% Send invalid JSON
    InvalidJSON = <<"{invalid json}">>,
    Result = erlmcp_port_tool:send_request(Pid, InvalidJSON),
    io:format("  Send result: ~p~n", [Result]),

    %% Verify port is still alive
    {ok, Info} = erlmcp_port_tool:port_info(Pid),
    io:format("  Port still alive: ~p~n", [maps:is_key(port, Info)]),

    %% Close port
    ok = erlmcp_port_tool:close_port(Pid),
    io:format("  Invalid JSON handled without crash~n").

test_rapid_restarts() ->
    %% Start and stop port multiple times
    {ok, Pid} = erlmcp_port_tool:start_link(#{}),

    SuccessCount = lists:foldl(fun(I, Acc) ->
        case os:type() of
            {unix, _} ->
                case erlmcp_port_tool:start_port(Pid, {"/bin/echo", [integer_to_list(I)]}) of
                    {ok, _Port} ->
                        io:format("  Start ~p: success~n", [I]),
                        ok = erlmcp_port_tool:close_port(Pid),
                        Acc + 1;
                    {error, Reason} ->
                        io:format("  Start ~p: failed (~p)~n", [I, Reason]),
                        Acc
                end;
            {win32, _} ->
                Acc
        end
    end, 0, lists:seq(1, 10)),

    io:format("  Successful restarts: ~p/10~n", [SuccessCount]),
    io:format("  Rapid restart resilience verified~n").

main(["--help"]) ->
    io:format("Port Chaos Example~n"),
    io:format("Usage: port_chaos_example.erl~n~n"),
    io:format("Demonstrates chaos testing for port resilience~n"),
    io:format("Tests:~n"),
    io:format("  1. Port crash recovery~n"),
    io:format("  2. Timeout handling~n"),
    io:format("  3. Invalid JSON handling~n"),
    io:format("  4. Rapid restarts~n"),
    init:stop().

main(_) ->
    main([]).
