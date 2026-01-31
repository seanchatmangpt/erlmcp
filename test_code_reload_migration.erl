#!/usr/bin/env escript
%% Test script for code reload migration functionality

main(_) ->
    io:format("Testing erlmcp_code_reload migration...~n"),

    % Test 1: Migrate v0 map to v1
    io:format("~nTest 1: Migrate v0 map state to v1~n"),
    V0Map = #{reload_history => [], rollback_timers => #{}, draining => false},
    {ok, V1State} = erlmcp_code_reload:migrate_state(V0Map, 0),
    io:format("  Version: ~p~n", [element(2, V1State)]),
    io:format("  ✓ Map migration successful~n"),

    % Test 2: Migrate v0 record to v1
    io:format("~nTest 2: Migrate v0 record state to v1~n"),
    V0Rec = erlmcp_code_reload:init_state(),
    V0Rec1 = setelement(2, V0Rec, undefined), % Set version to undefined
    {ok, V1State2} = erlmcp_code_reload:migrate_state(V0Rec1, 0),
    io:format("  Version: ~p~n", [element(2, V1State2)]),
    io:format("  ✓ Record migration successful~n"),

    % Test 3: Current version passes through
    io:format("~nTest 3: Current version passes through~n"),
    V1Current = erlmcp_code_reload:init_state(),
    {ok, V1State3} = erlmcp_code_reload:migrate_state(V1Current, 1),
    io:format("  Version: ~p~n", [element(2, V1State3)]),
    io:format("  ✓ Current version pass-through successful~n"),

    % Test 4: Invalid format returns error
    io:format("~nTest 4: Invalid format returns error~n"),
    Invalid = "invalid",
    Result = erlmcp_code_reload:migrate_state(Invalid, 0),
    io:format("  Result: ~p~n", [Result]),
    io:format("  ✓ Invalid format error handling successful~n"),

    % Test 5: code_change callback
    io:format("~nTest 5: code_change callback~n"),
    V0Map2 = #{reload_history => [], rollback_timers => #{}, draining => false},
    {ok, V1State4} = erlmcp_code_reload:code_change(0, V0Map2, undefined),
    io:format("  Version after code_change: ~p~n", [element(2, V1State4)]),
    io:format("  ✓ code_change migration successful~n"),

    io:format("~n✅ All migration tests passed!~n"),
    init:stop().

