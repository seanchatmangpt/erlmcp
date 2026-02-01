%% Automatically generated reproducer
%% Created: 2026-02-01 12:00:00 UTC
%% Rule ID: MESSAGE_TOO_LARGE
%% Description: Message exceeds transport size limit (16MB)

-module(reproducer_20260201_120000_005).
-export([run/0, scenario/0]).

-include_lib("eunit/include/eunit.hrl").

%% @doc Get scenario definition
scenario() ->
    LargePayload = binary:copy(<<"X">>, 17 * 1024 * 1024),  %% 17MB
    #{
        rule_id => <<"MESSAGE_TOO_LARGE">>,
        description => <<"Message exceeds 16MB size limit">>,
        category => transport,
        input => [{stdio, LargePayload}],
        expected => {error, {message_too_large, 16777216}},
        actual => {ok, accepted_oversized_message}
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
    %% This test will fail - size limit should be enforced
    Result = run(),
    case Result of
        {ok, fixed} ->
            ok;
        {error, still_failing} ->
            %% Expected failure - message size validation needed
            ?assertEqual({ok, fixed}, {error, still_failing})
    end.
