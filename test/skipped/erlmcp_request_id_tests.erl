%%%-------------------------------------------------------------------
%%% @doc
%%% Unit tests for erlmcp_request_id module.
%%%
%%% Tests safe request ID increment with overflow protection.
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_request_id_tests).
-author("erlmcp").

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Data
%%====================================================================

-define(MAX_SAFE_REQUEST_ID, 1152921504606846975).  % 2^60 - 1
-define(MIN_REQUEST_ID, 1).

%%====================================================================
%% Test Generators
%%====================================================================

%% Test normal increment
safe_increment_normal_test_() ->
    [
        ?_assertEqual({ok, 2}, erlmcp_request_id:safe_increment(1)),
        ?_assertEqual({ok, 100}, erlmcp_request_id:safe_increment(99)),
        ?_assertEqual({ok, 1000}, erlmcp_request_id:safe_increment(999)),
        ?_assertEqual({ok, ?MAX_SAFE_REQUEST_ID}, erlmcp_request_id:safe_increment(?MAX_SAFE_REQUEST_ID - 1))
    ].

%% Test overflow detection
safe_increment_overflow_test_() ->
    [
        ?_assertEqual({error, overflow}, erlmcp_request_id:safe_increment(?MAX_SAFE_REQUEST_ID)),
        ?_assertEqual({error, overflow}, erlmcp_request_id:safe_increment(?MAX_SAFE_REQUEST_ID + 1))
    ].

%% Test invalid input handling
safe_increment_invalid_test_() ->
    [
        ?_assertEqual({error, invalid_id}, erlmcp_request_id:safe_increment(0)),
        ?_assertEqual({error, invalid_id}, erlmcp_request_id:safe_increment(-1)),
        ?_assertEqual({error, invalid_id}, erlmcp_request_id:safe_increment(-100)),
        ?_assertEqual({error, invalid_id}, erlmcp_request_id:safe_increment(abc)),
        ?_assertEqual({error, invalid_id}, erlmcp_request_id:safe_increment([])),
        ?_assertEqual({error, invalid_id}, erlmcp_request_id:safe_increment({})),
        ?_assertEqual({error, invalid_id}, erlmcp_request_id:safe_increment(undefined))
    ].

%% Test validate_id with valid IDs
validate_id_valid_test_() ->
    [
        ?_assertEqual(ok, erlmcp_request_id:validate_id(1)),
        ?_assertEqual(ok, erlmcp_request_id:validate_id(100)),
        ?_assertEqual(ok, erlmcp_request_id:validate_id(?MAX_SAFE_REQUEST_ID)),
        ?_assertEqual(ok, erlmcp_request_id:validate_id(?MAX_SAFE_REQUEST_ID div 2))
    ].

%% Test validate_id with invalid IDs
validate_id_invalid_test_() ->
    [
        ?_assertEqual({error, {invalid_id, 0}}, erlmcp_request_id:validate_id(0)),
        ?_assertEqual({error, {invalid_id, -1}}, erlmcp_request_id:validate_id(-1)),
        ?_assertEqual({error, {exceeds_maximum, ?MAX_SAFE_REQUEST_ID + 1}}, erlmcp_request_id:validate_id(?MAX_SAFE_REQUEST_ID + 1)),
        ?_assertEqual({error, {not_integer, abc}}, erlmcp_request_id:validate_id(abc)),
        ?_assertEqual({error, {not_integer, []}}, erlmcp_request_id:validate_id([])),
        ?_assertEqual({error, {not_integer, undefined}}, erlmcp_request_id:validate_id(undefined))
    ].

%%====================================================================
%% Integration Tests
%%====================================================================

%% Test realistic usage pattern
realistic_usage_test() ->
    % Simulate generating 1000 request IDs
    FinalId = lists:foldl(
        fun(_N, AccId) ->
            {ok, NextId} = erlmcp_request_id:safe_increment(AccId),
            NextId
        end,
        1,
        lists:seq(1, 1000)
    ),
    ?assertEqual(1001, FinalId).

%% Test boundary conditions
boundary_test_() ->
    [
        ?_assertEqual({ok, 2}, erlmcp_request_id:safe_increment(?MIN_REQUEST_ID)),
        ?_assertEqual({ok, ?MAX_SAFE_REQUEST_ID}, erlmcp_request_id:safe_increment(?MAX_SAFE_REQUEST_ID - 1)),
        ?_assertEqual({error, overflow}, erlmcp_request_id:safe_increment(?MAX_SAFE_REQUEST_ID))
    ].
