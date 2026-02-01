%% Automatically generated reproducer
%% Created: 2026-02-01 12:00:00 UTC
%% Rule ID: SSE_INVALID_RESUME_ID
%% Description: SSE client sends invalid resume-id format

-module(reproducer_20260201_120000_001).
-export([run/0, scenario/0]).

-include_lib("eunit/include/eunit.hrl").

%% @doc Get scenario definition
scenario() ->
    #{
        rule_id => <<"SSE_INVALID_RESUME_ID">>,
        description => <<"SSE client sends non-string resume-id">>,
        category => sse,
        input => [
            {sse_connect, #{session_id => <<"session_123">>}},
            {sse_message, #{resume_id => 12345}}  %% Invalid: should be binary
        ],
        expected => {error, invalid_resume_id},
        actual => {error, {bad_resume_id, 12345}}
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
    %% This test will fail until the bug is fixed
    Result = run(),
    case Result of
        {ok, fixed} ->
            ok;
        {error, still_failing} ->
            %% Expected failure - reproducer still demonstrates bug
            ?assertEqual({ok, fixed}, {error, still_failing})
    end.
