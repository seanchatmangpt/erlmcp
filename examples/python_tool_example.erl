#!/usr/bin/env escript
%%! -pa _build/default/lib/*/ebin

%%%-------------------------------------------------------------------
%%% @doc
%%% Python Tool Example - Demonstrates Python bridge usage
%%%
%%% This example shows how to use erlmcp_python_bridge for:
%%% - Starting Python subprocess with MCP tool server
%%% - Invoking Python tools from Erlang
%%% - Receiving results from Python
%%% - Handling tool errors
%%% @end
%%%-------------------------------------------------------------------

-mode(compile).
-include("erlmcp.hrl").

main([]) ->
    io:format("=== Python Tool Example ===~n~n"),

    %% Check if Python is available
    case os:find_executable("python3") of
        false ->
            io:format("Error: Python 3 not found in PATH~n"),
            init:stop();
        _PythonPath ->
            io:format("Python 3 found: ~s~n~n", [_PythonPath]),
            run_example()
    end.

run_example() ->
    %% Start Python bridge
    io:format("1. Starting Python bridge...~n"),
    case erlmcp_python_bridge:start_link(#{}) of
        {ok, BridgePid} ->
            io:format("   Bridge started: ~p~n~n", [BridgePid]),

            %% List available tools
            io:format("2. Listing available tools...~n"),
            case erlmcp_python_bridge:list_tools(BridgePid) of
                {ok, Tools} ->
                    io:format("   Available tools: ~p~n~n", [Tools]),
                    invoke_tools(BridgePid);
                {error, Reason} ->
                    io:format("   Error listing tools: ~p~n", [Reason]),
                    invoke_tools(BridgePid)
            end;
        {error, Reason} ->
            io:format("Error starting bridge: ~p~n", [Reason]),
            io:format("Make sure mcp_tool_server.py is in priv directory~n"),
            init:stop()
    end.

invoke_tools(BridgePid) ->
    %% Invoke echo tool
    io:format("3. Invoking echo tool...~n"),
    case erlmcp_python_bridge:invoke_tool(BridgePid,
                                          <<"echo">>,
                                          #{<<"message">> => <<"Hello from Python!">>}) of
        {ok, Response} ->
            io:format("   Response: ~p~n~n", [Response]);
        {error, Reason} ->
            io:format("   Error: ~p~n~n", [Reason])
    end,

    %% Invoke calculator tool
    io:format("4. Invoking calculator tool...~n"),
    case erlmcp_python_bridge:invoke_tool(BridgePid,
                                          <<"calculator">>,
                                          #{<<"expression">> => <<"2 * 3 + 4">>}) of
        {ok, Response} ->
            io:format("   Response: ~p~n~n", [Response]);
        {error, Reason} ->
            io:format("   Error: ~p~n~n", [Reason])
    end,

    %% Invoke string transform tool
    io:format("5. Invoking string transform tool...~n"),
    case erlmcp_python_bridge:invoke_tool(BridgePid,
                                          <<"string_transform">>,
                                          #{<<"text">> => <<"hello erlang">>,
                                           <<"operation">> => <<"title">>}) of
        {ok, Response} ->
            io:format("   Response: ~p~n~n", [Response]);
        {error, Reason} ->
            io:format("   Error: ~p~n~n", [Reason])
    end,

    %% Try non-existent tool
    io:format("6. Invoking non-existent tool (error test)...~n"),
    case erlmcp_python_bridge:invoke_tool(BridgePid,
                                          <<"nonexistent">>,
                                          #{}) of
        {ok, Response} ->
            io:format("   Unexpected success: ~p~n~n", [Response]);
        {error, Reason} ->
            io:format("   Expected error: ~p~n~n", [Reason])
    end,

    io:format("=== Example Complete ===~n"),
    init:stop().

main(["--help"]) ->
    io:format("Python Tool Example~n"),
    io:format("Usage: python_tool_example.erl~n~n"),
    io:format("Requirements:~n"),
    io:format("  - Python 3 in PATH~n"),
    io:format("  - mcp_tool_server.py in priv directory~n~n"),
    io:format("Demonstrates Python bridge for MCP tool execution~n"),
    init:stop().

main(_) ->
    main([]).
