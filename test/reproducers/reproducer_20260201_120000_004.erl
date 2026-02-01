%% Automatically generated reproducer
%% Created: 2026-02-01 12:00:00 UTC
%% Rule ID: INVALID_ERROR_CODE
%% Description: Server returns non-standard error code (-99999)

-module(reproducer_20260201_120000_004).
-export([run/0, scenario/0]).

-include_lib("eunit/include/eunit.hrl").

%% @doc Get scenario definition
scenario() ->
    #{
        rule_id => <<"INVALID_ERROR_CODE">>,
        description => <<"Server returns error code outside valid range">>,
        category => protocol,
        input => [<<"{\"jsonrpc\":\"2.0\",\"id\":1,\"error\":{\"code\":-99999,\"message\":\"Unknown error\"}}">>],
        expected => {error, invalid_error_code},
        actual => {ok, parsed_despite_invalid_code}
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
    %% This test will fail - error code validation should reject -99999
    Result = run(),
    case Result of
        {ok, fixed} ->
            ok;
        {error, still_failing} ->
            %% Expected failure - need stricter error code validation
            ?assertEqual({ok, fixed}, {error, still_failing})
    end.
