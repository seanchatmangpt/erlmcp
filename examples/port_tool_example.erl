#!/usr/bin/env escript
%%! -pa _build/default/lib/*/ebin

%%%-------------------------------------------------------------------
%%% @doc
%%% Port Tool Example - Demonstrates basic port driver usage
%%%
%%% This example shows how to use erlmcp_port_tool for:
%%% - Starting external processes as ports
%%% - Sending requests to ports
%%% - Receiving responses from ports
%%% - Handling port lifecycle
%%% @end
%%%-------------------------------------------------------------------

-mode(compile).
-include("erlmcp.hrl").

main([]) ->
    io:format("=== Port Tool Example ===~n~n"),

    %% Start port tool
    io:format("1. Starting port tool...~n"),
    {ok, PortToolPid} = erlmcp_port_tool:start_link(#{}),
    io:format("   Port tool started: ~p~n~n", [PortToolPid]),

    %% Start echo process
    io:format("2. Starting echo process (cat)...~n"),
    case os:type() of
        {unix, _} ->
            {ok, Port} = erlmcp_port_tool:start_port(PortToolPid, {"/bin/cat", []});
        {win32, _} ->
            {ok, Port} = erlmcp_port_tool:start_port(PortToolPid, {"cmd.exe", ["/c", "type"]})
    end,
    io:format("   Port started: ~p~n~n", [Port]),

    %% Get port info
    io:format("3. Getting port info...~n"),
    {ok, Info} = erlmcp_port_tool:port_info(PortToolPid),
    io:format("   Command: ~p~n", [maps:get(command, Info)]),
    io:format("   Args: ~p~n", [maps:get(args, Info)]),
    io:format("   Pending requests: ~p~n~n", [maps:get(pending_requests, Info)]),

    %% Send request
    io:format("4. Sending request to port...~n"),
    Request = <<"{\"message\": \"Hello, port!\"}">>,
    ok = erlmcp_port_tool:send_request(PortToolPid, Request),
    io:format("   Request sent: ~s~n~n", [Request]),

    %% Close port
    io:format("5. Closing port...~n"),
    ok = erlmcp_port_tool:close_port(PortToolPid),
    io:format("   Port closed~n~n"),

    io:format("=== Example Complete ===~n"),
    init:stop().

main(["--help"]) ->
    io:format("Port Tool Example~n"),
    io:format("Usage: port_tool_example.erl~n~n"),
    io:format("Demonstrates basic port driver usage~n"),
    init:stop().

main(_) ->
    main([]).
