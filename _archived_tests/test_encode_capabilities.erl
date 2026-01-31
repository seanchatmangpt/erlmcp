#!/usr/bin/env escript
%%! -pa apps/erlmcp_core/ebin -pa apps/erlmcp_core/_build/test/lib/*/ebin -pa _build/test/lib/*/ebin

-include_lib("erlmcp/include/erlmcp.hrl").

main([]) ->
    %% Start applications
    application:ensure_all_started(jsx),

    io:format("Testing erlmcp_client:encode_capabilities/1~n~n"),

    %% Test 1: Tuple format
    io:format("Test 1: Tuple format {Name, Version}...~n"),
    Input1 = {<<"test_client">>, <<"1.0.0">>},
    Result1 = erlmcp_client:encode_capabilities(Input1),
    case Result1 of
        #{name := <<"test_client">>, version := <<"1.0.0">>} ->
            io:format("  ✅ PASS: Tuple format works~n");
        _ ->
            io:format("  ❌ FAIL: Expected #{name => <<\"test_client\">>, version => <<\"1.0.0\">>}~n"),
            io:format("  Got: ~p~n", [Result1]),
            halt(1)
    end,

    %% Test 2: Map format with name/version
    io:format("Test 2: Map format with name/version...~n"),
    Input2 = #{name => <<"client">>, version => <<"2.0.0">>},
    Result2 = erlmcp_client:encode_capabilities(Input2),
    case Result2 of
        #{name := <<"client">>, version := <<"2.0.0">>} ->
            io:format("  ✅ PASS: Map format works~n");
        _ ->
            io:format("  ❌ FAIL: Expected #{name => <<\"client\">>, version => <<\"2.0.0\">>}~n"),
            io:format("  Got: ~p~n", [Result2]),
            halt(1)
    end,

    %% Test 3: Plain map pass-through
    io:format("Test 3: Plain map pass-through...~n"),
    Input3 = #{custom_field => <<"value">>, other => 123},
    Result3 = erlmcp_client:encode_capabilities(Input3),
    case Result3 of
        #{custom_field := <<"value">>, other := 123} ->
            io:format("  ✅ PASS: Plain map pass-through works~n");
        _ ->
            io:format("  ❌ FAIL: Expected #{custom_field => <<\"value\">>, other => 123}~n"),
            io:format("  Got: ~p~n", [Result3]),
            halt(1)
    end,

    %% Test 4: MCP client capabilities record
    io:format("Test 4: MCP client capabilities record...~n"),
    Input4 = #mcp_client_capabilities{
        roots = #mcp_capability{enabled = true},
        sampling = #mcp_capability{enabled = true},
        experimental = #{feature1 => true}
    },
    Result4 = erlmcp_client:encode_capabilities(Input4),
    HasRoots = maps:is_key(<<"roots">>, Result4),
    HasSampling = maps:is_key(<<"sampling">>, Result4),
    HasFeature = maps:is_key(feature1, Result4),
    case {HasRoots, HasSampling, HasFeature} of
        {true, true, true} ->
            io:format("  ✅ PASS: Record format works~n");
        _ ->
            io:format("  ❌ FAIL: Expected roots, sampling, and feature1 keys~n"),
            io:format("  Got: ~p~n", [Result4]),
            halt(1)
    end,

    io:format("~n✅ ALL TESTS PASSED~n"),
    halt(0).
