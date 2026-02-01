%% Automatically generated reproducer
%% Created: 2026-02-01 12:00:00 UTC
%% Rule ID: INITIALIZE_MISSING_CLIENT_INFO
%% Description: Initialize request missing required clientInfo field

-module(reproducer_20260201_120000_003).
-export([run/0, scenario/0]).

-include_lib("eunit/include/eunit.hrl").

%% @doc Get scenario definition
scenario() ->
    #{
        rule_id => <<"INITIALIZE_MISSING_CLIENT_INFO">>,
        description => <<"Initialize request without clientInfo">>,
        category => protocol,
        input => [<<"{\"jsonrpc\":\"2.0\",\"method\":\"initialize\",\"id\":1,\"params\":{\"protocolVersion\":\"2025-11-25\",\"capabilities\":{}}}">>],
        expected => {error, {invalid_params, <<"Missing required field: clientInfo">>}},
        actual => {error, {parse_error, invalid_json}}
    }.

%% @doc Run reproducer test
run() ->
    S = scenario(),
    Input = maps:get(input, S),
    Expected = maps:get(expected, S),

    %% Replay scenario
    Result = erlmcp_reproducer:replay(S),

    %% Verify expected behavior
    case Result of
        {ok, Expected} ->
            {ok, fixed};
        {error, {mismatch, _}} ->
            {error, still_failing};
        _ ->
            {error, unexpected_result}
    end.

%% @doc EUnit test wrapper
reproducer_test() ->
    %% This test will fail until validation is improved
    Result = run(),
    case Result of
        {ok, fixed} ->
            ok;
        {error, still_failing} ->
            %% Expected failure - better error message needed
            ?assertEqual({ok, fixed}, {error, still_failing})
    end.
