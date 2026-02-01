%% Automatically generated reproducer
%% Created: 2026-02-01 12:00:00 UTC
%% Rule ID: JSONRPC_WRONG_VERSION
%% Description: Client sends JSON-RPC 1.0 instead of 2.0

-module(reproducer_20260201_120000_002).
-export([run/0, scenario/0]).

-include_lib("eunit/include/eunit.hrl").

%% @doc Get scenario definition
scenario() ->
    #{
        rule_id => <<"JSONRPC_WRONG_VERSION">>,
        description => <<"Client sends jsonrpc version 1.0 instead of 2.0">>,
        category => protocol,
        input => [<<"{\"jsonrpc\":\"1.0\",\"method\":\"ping\",\"id\":1}">>],
        expected => {error, {invalid_request, {wrong_version, <<"1.0">>}}},
        actual => {error, {invalid_request, {wrong_version, <<"1.0">>}}}
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
        {error, {mismatch, Details}} ->
            %% Check if actual matches expected (bug is fixed)
            Actual = maps:get(actual, Details, undefined),
            case Actual of
                Expected -> {ok, fixed};
                _ -> {error, still_failing}
            end;
        _ ->
            {error, unexpected_result}
    end.

%% @doc EUnit test wrapper
reproducer_test() ->
    %% This test should pass - protocol validator correctly rejects wrong version
    {ok, fixed} = run().
