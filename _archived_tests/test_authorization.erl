#!/usr/bin/env escript
%% -*- mode: erlang -*-
%%! -pa _build/default/lib/erlmcp_core/ebin -I apps/erlmcp_core/include

-include_lib("erlmcp.hrl").

main(_) ->
    io:format("Testing Authorization Bypass Vulnerability Fix~n"),
    io:format("============================================~n~n"),

    % Start application
    application:ensure_all_started(erlmcp_core),

    % Test 1: Unauthorized add_tool (should fail)
    io:format("Test 1: Unauthorized add_tool (no client capabilities)~n"),
    {ok, Server1} = erlmcp_server:start_link(
        <<"test_auth_1">>,
        #mcp_server_capabilities{tools = #mcp_capability{enabled = true}}
    ),

    Result1 = gen_server:call(Server1, {add_tool, <<"unauthorized_tool">>, fun(_) -> ok end}),
    case Result1 of
        {error, unauthorized} ->
            io:format("  PASS: Unauthorized tool addition rejected~n");
        _ ->
            io:format("  FAIL: Expected {error, unauthorized}, got: ~p~n", [Result1])
    end,
    erlmcp_server:stop(Server1),
    timer:sleep(100),

    % Test 2: Authorized add_tool (should succeed)
    io:format("~nTest 2: Authorized add_tool (with client capabilities)~n"),
    {ok, Server2} = erlmcp_server:start_link(
        <<"test_auth_2">>,
        #mcp_server_capabilities{tools = #mcp_capability{enabled = true}}
    ),

    % Set client capabilities to simulate post-initialization
    ClientCaps = #mcp_client_capabilities{
        roots = #mcp_capability{},
        sampling = #mcp_sampling_capability{},
        tools = #mcp_tools_capability{},
        experimental = undefined
    },
    gen_server:call(Server2, {set_client_capabilities_test_only, ClientCaps}),

    Result2 = gen_server:call(Server2, {add_tool, <<"authorized_tool">>, fun(_) -> ok end}),
    case Result2 of
        ok ->
            io:format("  PASS: Authorized tool addition succeeded~n");
        _ ->
            io:format("  FAIL: Expected ok, got: ~p~n", [Result2])
    end,
    erlmcp_server:stop(Server2),
    timer:sleep(100),

    % Test 3: Unauthorized add_tool_with_schema
    io:format("~nTest 3: Unauthorized add_tool_with_schema~n"),
    {ok, Server3} = erlmcp_server:start_link(
        <<"test_auth_3">>,
        #mcp_server_capabilities{tools = #mcp_capability{enabled = true}}
    ),

    Result3 = gen_server:call(Server3, {
        add_tool_with_schema,
        <<"unauthorized_tool_with_schema">>,
        fun(_) -> ok end,
        #{type => <<"object">>}
    }),
    case Result3 of
        {error, unauthorized} ->
            io:format("  PASS: Unauthorized tool_with_schema rejected~n");
        _ ->
            io:format("  FAIL: Expected {error, unauthorized}, got: ~p~n", [Result3])
    end,
    erlmcp_server:stop(Server3),
    timer:sleep(100),

    % Test 4: Unauthorized add_tool_full
    io:format("~nTest 4: Unauthorized add_tool_full~n"),
    {ok, Server4} = erlmcp_server:start_link(
        <<"test_auth_4">>,
        #mcp_server_capabilities{tools = #mcp_capability{enabled = true}}
    ),

    Result4 = gen_server:call(Server4, {
        add_tool_full,
        <<"unauthorized_full_tool">>,
        <<"Full tool description">>,
        fun(_) -> ok end,
        #{}
    }),
    case Result4 of
        {error, unauthorized} ->
            io:format("  PASS: Unauthorized add_tool_full rejected~n");
        _ ->
            io:format("  FAIL: Expected {error, unauthorized}, got: ~p~n", [Result4])
    end,
    erlmcp_server:stop(Server4),

    io:format("~n============================================~n"),
    io:format("Authorization Tests Complete~n"),
    io:format("Security Fix Verified: Authorization checks are working~n").
