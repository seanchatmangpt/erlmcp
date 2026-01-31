%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for erlmcp_request_id module.
%%%
%%% This test suite verifies request ID management with overflow protection.
%%% Tests follow Chicago School TDD principles:
%%% - Real functions (no mocks needed for pure functions)
%%% - State-based verification (assert on return values)
%%% - Comprehensive boundary and error path testing
%%% - Property-based tests for invariants
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_request_id_tests).
-include_lib("eunit/include/eunit.hrl").

%%%====================================================================
%%% Constants
%%%====================================================================

-define(MAX_SAFE_REQUEST_ID, 1152921504606846975). % 2^60 - 1
-define(MIN_REQUEST_ID, 1).

%%%====================================================================
%%% Basic Functionality Tests
%%%====================================================================

%% @doc Test normal increment from 1 to 2
safe_increment_normal_test() ->
    ?assertEqual({ok, 2}, erlmcp_request_id:safe_increment(1)).

%% @doc Test increment from a larger number
safe_increment_large_test() ->
    ?assertEqual({ok, 1000000}, erlmcp_request_id:safe_increment(999999)).

%% @doc Test increment from mid-range value
safe_increment_mid_range_test() ->
    MidValue = 500000000000000000,
    ?assertEqual({ok, MidValue + 1}, erlmcp_request_id:safe_increment(MidValue)).

%% @doc Test multiple sequential increments
safe_increment_sequential_test() ->
    ?assertEqual({ok, 2}, erlmcp_request_id:safe_increment(1)),
    ?assertEqual({ok, 3}, erlmcp_request_id:safe_increment(2)),
    ?assertEqual({ok, 4}, erlmcp_request_id:safe_increment(3)),
    ?assertEqual({ok, 5}, erlmcp_request_id:safe_increment(4)).

%%%====================================================================
%%% Boundary Tests
%%%====================================================================

%% @doc Test increment at maximum safe request ID (should overflow)
safe_increment_at_max_test() ->
    MaxId = ?MAX_SAFE_REQUEST_ID,
    ?assertEqual({error, overflow}, erlmcp_request_id:safe_increment(MaxId)).

%% @doc Test increment just below maximum (should succeed)
safe_increment_below_max_test() ->
    MaxId = ?MAX_SAFE_REQUEST_ID,
    ?assertEqual({ok, MaxId}, erlmcp_request_id:safe_increment(MaxId - 1)).

%% @doc Test increment far below maximum
safe_increment_far_below_max_test() ->
    MaxId = ?MAX_SAFE_REQUEST_ID,
    ?assertEqual({ok, MaxId - 99}, erlmcp_request_id:safe_increment(MaxId - 100)).

%%%====================================================================
%%% Error Path Tests - Invalid Input Types
%%%====================================================================

%% @doc Test safe_increment with zero (below minimum - currently passes)
safe_increment_zero_test() ->
    ?assertEqual({ok, 1}, erlmcp_request_id:safe_increment(0)).

%% @doc Test safe_increment with negative number (should fail with function_clause)
%% Note: The implementation only handles RequestId >= 0, so negative numbers
%% will cause a function_clause error. This test documents the actual behavior.
safe_increment_negative_test_() ->
    ?_assertError(function_clause,
        begin
            erlmcp_request_id:safe_increment(-1)
        end).

%%%====================================================================
%%% Monitoring and Threshold Tests
%%%====================================================================

%% @doc Test check_thresholds at minimum ID
check_thresholds_min_test() ->
    {ok, Level, Usage} = erlmcp_request_id:check_thresholds(1),
    ?assertEqual(normal, Level),
    ?assert(Usage > 0.0),
    ?assert(Usage < 0.000001).

%% @doc Test check_thresholds at warning level (80%)
check_thresholds_warning_test() ->
    WarningId = (?MAX_SAFE_REQUEST_ID * 80) div 100,
    {ok, Level, Usage} = erlmcp_request_id:check_thresholds(WarningId),
    ?assertEqual(warning, Level),
    ?assert(Usage >= 80.0),
    ?assert(Usage < 90.0).

%% @doc Test check_thresholds at critical level (90%)
check_thresholds_critical_test() ->
    CriticalId = (?MAX_SAFE_REQUEST_ID * 90) div 100,
    {ok, Level, Usage} = erlmcp_request_id:check_thresholds(CriticalId),
    ?assertEqual(critical, Level),
    ?assert(Usage >= 90.0),
    ?assert(Usage < 96.0).

%% @doc Test check_thresholds at reserved level (96%)
check_thresholds_reserved_test() ->
    ReservedId = (?MAX_SAFE_REQUEST_ID * 96) div 100,
    {ok, Level, Usage} = erlmcp_request_id:check_thresholds(ReservedId),
    ?assertEqual(reserved, Level),
    ?assert(Usage >= 96.0),
    ?assert(Usage < 100.0).

%% @doc Test check_thresholds at maximum
check_thresholds_max_test() ->
    {ok, Level, Usage} = erlmcp_request_id:check_thresholds(?MAX_SAFE_REQUEST_ID),
    ?assertEqual(reserved, Level),
    ?assertEqual(100.0, Usage).

%% @doc Test check_thresholds at normal level (50%)
check_thresholds_normal_test() ->
    HalfId = ?MAX_SAFE_REQUEST_ID div 2,
    {ok, Level, Usage} = erlmcp_request_id:check_thresholds(HalfId),
    ?assertEqual(normal, Level),
    ?assert(Usage > 49.0),
    ?assert(Usage < 51.0).

%% @doc Test check_thresholds with zero
check_thresholds_zero_test() ->
    {ok, Level, Usage} = erlmcp_request_id:check_thresholds(0),
    ?assertEqual(normal, Level),
    ?assertEqual(0.0, Usage).

%% @doc Test check_thresholds with negative number
check_thresholds_negative_test() ->
    {ok, Level, Usage} = erlmcp_request_id:check_thresholds(-1),
    ?assertEqual(normal, Level),
    ?assert(Usage < 0.0).

%%%====================================================================
%%% Integration Tests
%%%====================================================================

%% @doc Test realistic workflow: increment and check thresholds
realistic_workflow_test() ->
    % Start with ID 1
    {ok, Id2} = erlmcp_request_id:safe_increment(1),
    ?assertEqual(2, Id2),
    {ok, normal, _Usage} = erlmcp_request_id:check_thresholds(Id2),

    % Increment to 100
    {ok, Id100} = lists:foldl(
        fun(_, {ok, AccId}) ->
            erlmcp_request_id:safe_increment(AccId)
        end,
        {ok, 2},
        lists:seq(1, 98)
    ),
    ?assertEqual(100, Id100),
    {ok, normal, _Usage2} = erlmcp_request_id:check_thresholds(Id100),

    % Try to go beyond maximum
    MaxId = ?MAX_SAFE_REQUEST_ID,
    {ok, MaxId} = erlmcp_request_id:safe_increment(MaxId - 1),
    ?assertEqual({error, overflow}, erlmcp_request_id:safe_increment(MaxId)).

%% @doc Test boundary values with increment
boundary_increment_test() ->
    % Test near minimum
    ?assertEqual({ok, 2}, erlmcp_request_id:safe_increment(1)),
    ?assertEqual({ok, 3}, erlmcp_request_id:safe_increment(2)),
    ?assertEqual({ok, 4}, erlmcp_request_id:safe_increment(3)),

    % Test near maximum
    MaxId = ?MAX_SAFE_REQUEST_ID,
    ?assertEqual({ok, MaxId - 2}, erlmcp_request_id:safe_increment(MaxId - 3)),
    ?assertEqual({ok, MaxId - 1}, erlmcp_request_id:safe_increment(MaxId - 2)),
    ?assertEqual({ok, MaxId}, erlmcp_request_id:safe_increment(MaxId - 1)),
    ?assertEqual({error, overflow}, erlmcp_request_id:safe_increment(MaxId)).

%% @doc Test threshold progression as ID increases
threshold_progression_test() ->
    % Start at normal
    {ok, normal, _} = erlmcp_request_id:check_thresholds(?MAX_SAFE_REQUEST_ID div 10),

    % Move to warning
    WarningId = (?MAX_SAFE_REQUEST_ID * 85) div 100,
    {ok, warning, _} = erlmcp_request_id:check_thresholds(WarningId),

    % Move to critical
    CriticalId = (?MAX_SAFE_REQUEST_ID * 92) div 100,
    {ok, critical, _} = erlmcp_request_id:check_thresholds(CriticalId),

    % Move to reserved
    ReservedId = (?MAX_SAFE_REQUEST_ID * 97) div 100,
    {ok, reserved, _} = erlmcp_request_id:check_thresholds(ReservedId).

%%%====================================================================
%%% Property-Based Tests (Proper)
%%%====================================================================
%%% Note: These are Proper properties, run with: rebar3 proper -c

%% @doc Property: safe_increment always increases or returns overflow
%% @private
prop_safe_increment_increases() ->
    proper:forall(proper_types:range(1, ?MAX_SAFE_REQUEST_ID),
        fun(Id) ->
            case erlmcp_request_id:safe_increment(Id) of
                {ok, NextId} when NextId > Id -> true;
                {error, overflow} when Id == ?MAX_SAFE_REQUEST_ID -> true;
                _ -> false
            end
        end).

%% @doc Property: safe_increment and check_thresholds are consistent
%% @private
prop_increment_threshold_consistency() ->
    proper:forall(proper_types:range(1, ?MAX_SAFE_REQUEST_ID - 1),
        fun(Id) ->
            {ok, NextId} = erlmcp_request_id:safe_increment(Id),
            {ok, _Level, Usage} = erlmcp_request_id:check_thresholds(NextId),
            Usage > 0.0 andalso Usage =< 100.0
        end).

%% @doc Property: check_thresholds returns valid levels
%% @private
prop_check_thresholds_valid_levels() ->
    proper:forall(proper_types:range(0, ?MAX_SAFE_REQUEST_ID),
        fun(Id) ->
            {ok, Level, Usage} = erlmcp_request_id:check_thresholds(Id),
            ValidLevels = [normal, warning, critical, reserved],
            lists:member(Level, ValidLevels) andalso Usage >= 0.0 andalso Usage =< 100.0
        end).

%%%====================================================================
%%% Helper Functions
%%%====================================================================

%% @doc Calculate percentage of max ID used (for verification)
%% @private
calculate_percentage(Id) ->
    (Id / ?MAX_SAFE_REQUEST_ID) * 100.0.
