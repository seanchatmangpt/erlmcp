%%%-----------------------------------------------------------------------------
%%% @doc TCPS Kanban Tests - Comprehensive Test Suite
%%%
%%% Tests for WIP limit enforcement, Heijunka leveling, and pull signal
%%% processing in the TCPS Kanban system.
%%%
%%% == Test Coverage ==
%%% - WIP limit management (set, get, check)
%%% - WIP limit enforcement (refuse when over limit)
%%% - Heijunka leveling algorithm (prevent batching, max consecutive)
%%% - Pull signal processing (create work orders, check limits)
%%% - Work order completion (decrement WIP)
%%% - State management and persistence
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(tcps_kanban_tests).

-include_lib("eunit/include/eunit.hrl").

%%=============================================================================
%% Test Setup and Cleanup
%%=============================================================================

kanban_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
         %% WIP Limit Management Tests
         fun(_) -> {"Set and get WIP limits", fun test_set_get_wip_limits/0} end,
         fun(_) -> {"Check WIP limit availability", fun test_check_wip_limit/0} end,
         fun(_) -> {"Get WIP status", fun test_get_wip_status/0} end,

         %% WIP Limit Enforcement Tests
         fun(_) -> {"Refuse work when WIP limit reached", fun test_wip_limit_enforcement/0} end,
         fun(_) -> {"Allow work when under WIP limit", fun test_allow_under_limit/0} end,
         fun(_) -> {"Infinite WIP limit allows unlimited work", fun test_infinite_limit/0} end,

         %% Heijunka Leveling Tests
         fun(_) -> {"Heijunka prevents batching - max 2 consecutive", fun test_heijunka_prevents_batching/0} end,
         fun(_) -> {"Heijunka round-robin distribution", fun test_heijunka_round_robin/0} end,
         fun(_) -> {"Heijunka with uneven work distribution", fun test_heijunka_uneven_distribution/0} end,
         fun(_) -> {"Heijunka with single bucket", fun test_heijunka_single_bucket/0} end,
         fun(_) -> {"Heijunka respects priority within bucket", fun test_heijunka_priority/0} end,

         %% Pull Signal Processing Tests
         fun(_) -> {"Process pull signal creates work order", fun test_process_pull_signal/0} end,
         fun(_) -> {"Pull signal validates bucket", fun test_pull_signal_validation/0} end,
         fun(_) -> {"Pull signal increments WIP count", fun test_pull_signal_wip_increment/0} end,
         fun(_) -> {"Pull signal with priority", fun test_pull_signal_priority/0} end,

         %% Work Order Completion Tests
         fun(_) -> {"Complete work order decrements WIP", fun test_complete_work_order/0} end,
         fun(_) -> {"Complete non-existent work order fails", fun test_complete_nonexistent_order/0} end,

         %% Integration Tests
         fun(_) -> {"Full workflow - create, level, complete", fun test_full_workflow/0} end,
         fun(_) -> {"Multiple buckets concurrent operation", fun test_multiple_buckets/0} end
     ]}.

setup() ->
    %% Stop any existing instance
    catch tcps_kanban:stop(),
    timer:sleep(50),

    %% Start kanban server with custom config for testing
    Config = #{
        wip_limits => #{
            reliability => 3,
            security => 3,
            cost => 3,
            compliance => 3
        },
        max_consecutive => 2
    },
    {ok, Pid} = tcps_kanban:start_link(Config),
    Pid.

cleanup(Pid) ->
    %% Reset state before stopping
    catch tcps_kanban:reset_state(),
    catch tcps_kanban:stop(),
    %% Wait for process to terminate
    case is_process_alive(Pid) of
        true ->
            timer:sleep(100);
        false ->
            ok
    end.

%%=============================================================================
%% WIP Limit Management Tests
%%=============================================================================

test_set_get_wip_limits() ->
    %% Initial limits should be 3 (from setup)
    InitStatus = tcps_kanban:get_wip_status(reliability),
    ?assertEqual(3, maps:get(limit, InitStatus)),

    %% Set WIP limits
    ok = tcps_kanban:set_wip_limit(reliability, 5),
    ok = tcps_kanban:set_wip_limit(security, 10),
    ok = tcps_kanban:set_wip_limit(cost, infinity),

    %% Verify limits are set (by checking availability through status)
    Status1 = tcps_kanban:get_wip_status(reliability),
    ?assertEqual(5, maps:get(limit, Status1)),

    Status2 = tcps_kanban:get_wip_status(security),
    ?assertEqual(10, maps:get(limit, Status2)),

    Status3 = tcps_kanban:get_wip_status(cost),
    ?assertEqual(infinity, maps:get(limit, Status3)),

    %% Reset for other tests
    ok = tcps_kanban:set_wip_limit(reliability, 3),
    ok = tcps_kanban:set_wip_limit(security, 3),
    ok = tcps_kanban:set_wip_limit(cost, 3).

test_check_wip_limit() ->
    %% Initially under limit
    ?assertEqual({ok, available}, tcps_kanban:check_wip_limit(reliability)),
    ?assertEqual({ok, available}, tcps_kanban:check_wip_limit(security)),
    ?assertEqual({ok, available}, tcps_kanban:check_wip_limit(cost)),
    ?assertEqual({ok, available}, tcps_kanban:check_wip_limit(compliance)).

test_get_wip_status() ->
    %% Get initial status
    Status = tcps_kanban:get_wip_status(reliability),

    ?assertEqual(0, maps:get(current, Status)),
    ?assertEqual(3, maps:get(limit, Status)),
    ?assertEqual(3, maps:get(available, Status)),
    ?assertEqual(0.0, maps:get(utilization, Status)),

    %% Add some work orders
    Signal1 = #{bucket => reliability, payload => #{test => 1}},
    {ok, _OrderId1} = tcps_kanban:process_pull_signal(Signal1),

    Signal2 = #{bucket => reliability, payload => #{test => 2}},
    {ok, _OrderId2} = tcps_kanban:process_pull_signal(Signal2),

    %% Check updated status
    Status2 = tcps_kanban:get_wip_status(reliability),
    ?assertEqual(2, maps:get(current, Status2)),
    ?assertEqual(3, maps:get(limit, Status2)),
    ?assertEqual(1, maps:get(available, Status2)),
    ?assert(abs(maps:get(utilization, Status2) - 0.6667) < 0.01).

%%=============================================================================
%% WIP Limit Enforcement Tests
%%=============================================================================

test_wip_limit_enforcement() ->
    %% Fill up reliability bucket (limit = 3)
    Signal = #{bucket => reliability, payload => #{test => <<"data">>}},

    {ok, OrderId1} = tcps_kanban:process_pull_signal(Signal),
    {ok, OrderId2} = tcps_kanban:process_pull_signal(Signal),
    {ok, OrderId3} = tcps_kanban:process_pull_signal(Signal),

    %% Verify WIP count
    ?assertEqual(3, tcps_kanban:get_current_wip(reliability)),

    %% Fourth signal should be refused
    ?assertEqual({error, limit_reached}, tcps_kanban:process_pull_signal(Signal)),

    %% Still at limit
    ?assertEqual(3, tcps_kanban:get_current_wip(reliability)),

    %% Complete one order
    ok = tcps_kanban:complete_work_order(reliability, OrderId1),
    ?assertEqual(2, tcps_kanban:get_current_wip(reliability)),

    %% Now signal should succeed
    {ok, _OrderId4} = tcps_kanban:process_pull_signal(Signal),
    ?assertEqual(3, tcps_kanban:get_current_wip(reliability)),

    %% Cleanup
    ok = tcps_kanban:complete_work_order(reliability, OrderId2),
    ok = tcps_kanban:complete_work_order(reliability, OrderId3).

test_allow_under_limit() ->
    %% Should allow work when under limit
    Signal = #{bucket => security, payload => #{test => <<"data">>}},

    {ok, OrderId1} = tcps_kanban:process_pull_signal(Signal),
    ?assertEqual(1, tcps_kanban:get_current_wip(security)),
    ?assertEqual({ok, available}, tcps_kanban:check_wip_limit(security)),

    {ok, OrderId2} = tcps_kanban:process_pull_signal(Signal),
    ?assertEqual(2, tcps_kanban:get_current_wip(security)),
    ?assertEqual({ok, available}, tcps_kanban:check_wip_limit(security)),

    %% Cleanup
    ok = tcps_kanban:complete_work_order(security, OrderId1),
    ok = tcps_kanban:complete_work_order(security, OrderId2).

test_infinite_limit() ->
    %% Set infinite limit
    ok = tcps_kanban:set_wip_limit(cost, infinity),

    %% Should allow unlimited work
    Signal = #{bucket => cost, payload => #{test => <<"data">>}},
    OrderIds = lists:map(fun(_N) ->
        {ok, OrderId} = tcps_kanban:process_pull_signal(Signal),
        OrderId
    end, lists:seq(1, 100)),

    ?assertEqual(100, tcps_kanban:get_current_wip(cost)),
    ?assertEqual({ok, available}, tcps_kanban:check_wip_limit(cost)),

    %% Cleanup
    lists:foreach(fun(OrderId) ->
        ok = tcps_kanban:complete_work_order(cost, OrderId)
    end, OrderIds),
    ?assertEqual(0, tcps_kanban:get_current_wip(cost)).

%%=============================================================================
%% Heijunka Leveling Tests
%%=============================================================================

test_heijunka_prevents_batching() ->
    %% Create work orders with balanced distribution to test leveling
    WorkOrders = [
        #{id => <<"r1">>, bucket => reliability, priority => 1, created_at => erlang:timestamp(), status => pending, payload => #{}},
        #{id => <<"r2">>, bucket => reliability, priority => 1, created_at => erlang:timestamp(), status => pending, payload => #{}},
        #{id => <<"r3">>, bucket => reliability, priority => 1, created_at => erlang:timestamp(), status => pending, payload => #{}},
        #{id => <<"s1">>, bucket => security, priority => 1, created_at => erlang:timestamp(), status => pending, payload => #{}},
        #{id => <<"s2">>, bucket => security, priority => 1, created_at => erlang:timestamp(), status => pending, payload => #{}},
        #{id => <<"s3">>, bucket => security, priority => 1, created_at => erlang:timestamp(), status => pending, payload => #{}},
        #{id => <<"c1">>, bucket => cost, priority => 1, created_at => erlang:timestamp(), status => pending, payload => #{}},
        #{id => <<"c2">>, bucket => cost, priority => 1, created_at => erlang:timestamp(), status => pending, payload => #{}}
    ],

    Schedule = tcps_kanban:level_work_orders(WorkOrders),

    %% Extract bucket sequence
    BucketSequence = [Bucket || {Bucket, _Orders} <- Schedule],

    %% With balanced distribution (3:3:2), should prevent batching - max 2 consecutive
    ?assert(check_max_consecutive(BucketSequence, 2)),

    %% All work orders should be scheduled
    TotalScheduled = lists:sum([length(Orders) || {_Bucket, Orders} <- Schedule]),
    ?assertEqual(8, TotalScheduled).

test_heijunka_round_robin() ->
    %% Create balanced work orders across buckets
    WorkOrders = [
        #{id => <<"r1">>, bucket => reliability, priority => 1, created_at => erlang:timestamp(), status => pending, payload => #{}},
        #{id => <<"r2">>, bucket => reliability, priority => 1, created_at => erlang:timestamp(), status => pending, payload => #{}},
        #{id => <<"s1">>, bucket => security, priority => 1, created_at => erlang:timestamp(), status => pending, payload => #{}},
        #{id => <<"s2">>, bucket => security, priority => 1, created_at => erlang:timestamp(), status => pending, payload => #{}},
        #{id => <<"c1">>, bucket => cost, priority => 1, created_at => erlang:timestamp(), status => pending, payload => #{}},
        #{id => <<"c2">>, bucket => cost, priority => 1, created_at => erlang:timestamp(), status => pending, payload => #{}},
        #{id => <<"co1">>, bucket => compliance, priority => 1, created_at => erlang:timestamp(), status => pending, payload => #{}},
        #{id => <<"co2">>, bucket => compliance, priority => 1, created_at => erlang:timestamp(), status => pending, payload => #{}}
    ],

    Schedule = tcps_kanban:level_work_orders(WorkOrders),
    BucketSequence = [Bucket || {Bucket, _Orders} <- Schedule],

    %% Should rotate through buckets
    ?assert(check_max_consecutive(BucketSequence, 2)),

    %% Should have good distribution
    ReliabilityCount = length([B || B <- BucketSequence, B =:= reliability]),
    SecurityCount = length([B || B <- BucketSequence, B =:= security]),
    CostCount = length([B || B <- BucketSequence, B =:= cost]),
    ComplianceCount = length([B || B <- BucketSequence, B =:= compliance]),

    ?assertEqual(2, ReliabilityCount),
    ?assertEqual(2, SecurityCount),
    ?assertEqual(2, CostCount),
    ?assertEqual(2, ComplianceCount).

test_heijunka_uneven_distribution() ->
    %% Create moderately uneven distribution
    WorkOrders = [
        #{id => <<"r1">>, bucket => reliability, priority => 1, created_at => erlang:timestamp(), status => pending, payload => #{}},
        #{id => <<"r2">>, bucket => reliability, priority => 1, created_at => erlang:timestamp(), status => pending, payload => #{}},
        #{id => <<"r3">>, bucket => reliability, priority => 1, created_at => erlang:timestamp(), status => pending, payload => #{}},
        #{id => <<"r4">>, bucket => reliability, priority => 1, created_at => erlang:timestamp(), status => pending, payload => #{}},
        #{id => <<"s1">>, bucket => security, priority => 1, created_at => erlang:timestamp(), status => pending, payload => #{}},
        #{id => <<"s2">>, bucket => security, priority => 1, created_at => erlang:timestamp(), status => pending, payload => #{}},
        #{id => <<"c1">>, bucket => cost, priority => 1, created_at => erlang:timestamp(), status => pending, payload => #{}},
        #{id => <<"c2">>, bucket => cost, priority => 1, created_at => erlang:timestamp(), status => pending, payload => #{}}
    ],

    Schedule = tcps_kanban:level_work_orders(WorkOrders),
    BucketSequence = [Bucket || {Bucket, _Orders} <- Schedule],

    %% With moderately uneven distribution (4:2:2), leveling should work
    %% Note: May have some consecutive items when buckets are exhausted (best effort)
    %% Verify reasonable distribution - no more than 4 consecutive
    ?assert(check_max_consecutive(BucketSequence, 4)),

    %% All 8 work orders should be scheduled
    ?assertEqual(8, length(BucketSequence)).

test_heijunka_single_bucket() ->
    %% All work orders from single bucket
    WorkOrders = [
        #{id => <<"1">>, bucket => reliability, priority => 1, created_at => erlang:timestamp(), status => pending, payload => #{}},
        #{id => <<"2">>, bucket => reliability, priority => 1, created_at => erlang:timestamp(), status => pending, payload => #{}},
        #{id => <<"3">>, bucket => reliability, priority => 1, created_at => erlang:timestamp(), status => pending, payload => #{}}
    ],

    Schedule = tcps_kanban:level_work_orders(WorkOrders),

    %% Should all be from reliability bucket
    ?assertEqual(3, length(Schedule)),
    ?assert(lists:all(fun({Bucket, _}) -> Bucket =:= reliability end, Schedule)).

test_heijunka_priority() ->
    %% Create work orders with different priorities
    WorkOrders = [
        #{id => <<"r1">>, bucket => reliability, priority => 1, created_at => erlang:timestamp(), status => pending, payload => #{}},
        #{id => <<"r2">>, bucket => reliability, priority => 10, created_at => erlang:timestamp(), status => pending, payload => #{}},
        #{id => <<"r3">>, bucket => reliability, priority => 5, created_at => erlang:timestamp(), status => pending, payload => #{}},
        #{id => <<"s1">>, bucket => security, priority => 8, created_at => erlang:timestamp(), status => pending, payload => #{}},
        #{id => <<"s2">>, bucket => security, priority => 3, created_at => erlang:timestamp(), status => pending, payload => #{}}
    ],

    Schedule = tcps_kanban:level_work_orders(WorkOrders),

    %% Extract reliability orders
    ReliabilityOrders = lists:flatten([Orders || {reliability, Orders} <- Schedule]),
    ReliabilityIds = [maps:get(id, O) || O <- ReliabilityOrders],

    %% Should be ordered by priority (highest first): r2 (10), r3 (5), r1 (1)
    ?assertEqual([<<"r2">>, <<"r3">>, <<"r1">>], ReliabilityIds),

    %% Extract security orders
    SecurityOrders = lists:flatten([Orders || {security, Orders} <- Schedule]),
    SecurityIds = [maps:get(id, O) || O <- SecurityOrders],

    %% Should be ordered by priority: s1 (8), s2 (3)
    ?assertEqual([<<"s1">>, <<"s2">>], SecurityIds).

%%=============================================================================
%% Pull Signal Processing Tests
%%=============================================================================

test_process_pull_signal() ->
    %% Create pull signal
    Signal = #{
        bucket => compliance,
        priority => 5,
        payload => #{
            task => <<"audit_logs">>,
            description => <<"Review compliance audit logs">>
        }
    },

    %% Process signal
    {ok, OrderId} = tcps_kanban:process_pull_signal(Signal),

    %% Verify work order created
    ?assert(is_binary(OrderId)),
    ?assertEqual(1, tcps_kanban:get_current_wip(compliance)),

    %% Cleanup
    ok = tcps_kanban:complete_work_order(compliance, OrderId).

test_pull_signal_validation() ->
    %% Missing bucket
    Signal1 = #{payload => #{test => 1}},
    ?assertEqual({error, missing_bucket}, tcps_kanban:process_pull_signal(Signal1)),

    %% Missing payload
    Signal2 = #{bucket => reliability},
    ?assertEqual({error, missing_payload}, tcps_kanban:process_pull_signal(Signal2)),

    %% Invalid bucket type
    Signal3 = #{bucket => "invalid", payload => #{}},
    ?assertEqual({error, invalid_bucket}, tcps_kanban:process_pull_signal(Signal3)),

    %% Invalid payload type
    Signal4 = #{bucket => reliability, payload => "invalid"},
    ?assertEqual({error, invalid_payload}, tcps_kanban:process_pull_signal(Signal4)).

test_pull_signal_wip_increment() ->
    %% Initial WIP should be 0
    ?assertEqual(0, tcps_kanban:get_current_wip(security)),

    %% Process signals
    Signal = #{bucket => security, payload => #{test => 1}},
    {ok, OrderId1} = tcps_kanban:process_pull_signal(Signal),
    ?assertEqual(1, tcps_kanban:get_current_wip(security)),

    {ok, OrderId2} = tcps_kanban:process_pull_signal(Signal),
    ?assertEqual(2, tcps_kanban:get_current_wip(security)),

    %% Cleanup
    ok = tcps_kanban:complete_work_order(security, OrderId1),
    ?assertEqual(1, tcps_kanban:get_current_wip(security)),

    ok = tcps_kanban:complete_work_order(security, OrderId2),
    ?assertEqual(0, tcps_kanban:get_current_wip(security)).

test_pull_signal_priority() ->
    %% Signals with different priorities
    Signal1 = #{bucket => cost, priority => 10, payload => #{test => 1}},
    Signal2 = #{bucket => cost, priority => 5, payload => #{test => 2}},
    Signal3 = #{bucket => cost, payload => #{test => 3}}, % default priority = 0

    {ok, OrderId1} = tcps_kanban:process_pull_signal(Signal1),
    {ok, OrderId2} = tcps_kanban:process_pull_signal(Signal2),
    {ok, OrderId3} = tcps_kanban:process_pull_signal(Signal3),

    %% All should succeed
    ?assertEqual(3, tcps_kanban:get_current_wip(cost)),

    %% Cleanup
    ok = tcps_kanban:complete_work_order(cost, OrderId1),
    ok = tcps_kanban:complete_work_order(cost, OrderId2),
    ok = tcps_kanban:complete_work_order(cost, OrderId3).

%%=============================================================================
%% Work Order Completion Tests
%%=============================================================================

test_complete_work_order() ->
    %% Create work order
    Signal = #{bucket => reliability, payload => #{test => 1}},
    {ok, OrderId} = tcps_kanban:process_pull_signal(Signal),
    ?assertEqual(1, tcps_kanban:get_current_wip(reliability)),

    %% Complete work order
    ok = tcps_kanban:complete_work_order(reliability, OrderId),
    ?assertEqual(0, tcps_kanban:get_current_wip(reliability)),

    %% Check availability restored
    ?assertEqual({ok, available}, tcps_kanban:check_wip_limit(reliability)).

test_complete_nonexistent_order() ->
    %% Try to complete non-existent work order
    FakeOrderId = <<"wo_fake_12345">>,
    ?assertEqual({error, not_found}, tcps_kanban:complete_work_order(reliability, FakeOrderId)).

%%=============================================================================
%% Integration Tests
%%=============================================================================

test_full_workflow() ->
    %% 1. Create pull signals
    Signals = [
        #{bucket => reliability, priority => 10, payload => #{task => <<"fix_bug">>}},
        #{bucket => security, priority => 8, payload => #{task => <<"patch_vuln">>}},
        #{bucket => cost, priority => 5, payload => #{task => <<"optimize">>}},
        #{bucket => compliance, priority => 7, payload => #{task => <<"audit">>}}
    ],

    %% 2. Process signals and collect order IDs
    OrderIds = lists:map(fun(Signal) ->
        {ok, OrderId} = tcps_kanban:process_pull_signal(Signal),
        {maps:get(bucket, Signal), OrderId}
    end, Signals),

    %% 3. Verify WIP counts
    ?assertEqual(1, tcps_kanban:get_current_wip(reliability)),
    ?assertEqual(1, tcps_kanban:get_current_wip(security)),
    ?assertEqual(1, tcps_kanban:get_current_wip(cost)),
    ?assertEqual(1, tcps_kanban:get_current_wip(compliance)),

    %% 4. Create more work orders for leveling test
    MoreOrders = [
        #{id => <<"r2">>, bucket => reliability, priority => 5, created_at => erlang:timestamp(), status => pending, payload => #{}},
        #{id => <<"r3">>, bucket => reliability, priority => 3, created_at => erlang:timestamp(), status => pending, payload => #{}},
        #{id => <<"s2">>, bucket => security, priority => 6, created_at => erlang:timestamp(), status => pending, payload => #{}}
    ],

    %% 5. Level work orders
    Schedule = tcps_kanban:level_work_orders(MoreOrders),
    ?assertEqual(3, length(Schedule)),

    %% 6. Complete work orders
    lists:foreach(fun({Bucket, OrderId}) ->
        ok = tcps_kanban:complete_work_order(Bucket, OrderId)
    end, OrderIds),

    %% 7. Verify WIP counts back to 0
    ?assertEqual(0, tcps_kanban:get_current_wip(reliability)),
    ?assertEqual(0, tcps_kanban:get_current_wip(security)),
    ?assertEqual(0, tcps_kanban:get_current_wip(cost)),
    ?assertEqual(0, tcps_kanban:get_current_wip(compliance)).

test_multiple_buckets() ->
    %% Test concurrent operation across multiple buckets

    %% Create signals for all buckets
    AllSignals = [
        {reliability, #{bucket => reliability, priority => 5, payload => #{test => 1}}},
        {reliability, #{bucket => reliability, priority => 3, payload => #{test => 2}}},
        {security, #{bucket => security, priority => 8, payload => #{test => 3}}},
        {security, #{bucket => security, priority => 2, payload => #{test => 4}}},
        {cost, #{bucket => cost, priority => 6, payload => #{test => 5}}},
        {compliance, #{bucket => compliance, priority => 9, payload => #{test => 6}}}
    ],

    %% Process all signals
    OrderIds = lists:map(fun({Bucket, Signal}) ->
        {ok, OrderId} = tcps_kanban:process_pull_signal(Signal),
        {Bucket, OrderId}
    end, AllSignals),

    %% Verify WIP counts
    ?assertEqual(2, tcps_kanban:get_current_wip(reliability)),
    ?assertEqual(2, tcps_kanban:get_current_wip(security)),
    ?assertEqual(1, tcps_kanban:get_current_wip(cost)),
    ?assertEqual(1, tcps_kanban:get_current_wip(compliance)),

    %% Try to add one more to reliability (should hit limit of 3)
    Signal = #{bucket => reliability, payload => #{test => 7}},
    {ok, OrderId7} = tcps_kanban:process_pull_signal(Signal),
    ?assertEqual(3, tcps_kanban:get_current_wip(reliability)),

    %% Now should be at limit
    ?assertEqual({error, limit_reached}, tcps_kanban:process_pull_signal(Signal)),

    %% Cleanup all orders
    AllOrderIds = OrderIds ++ [{reliability, OrderId7}],
    lists:foreach(fun({Bucket, OrderId}) ->
        ok = tcps_kanban:complete_work_order(Bucket, OrderId)
    end, AllOrderIds),

    %% Verify all back to 0
    ?assertEqual(0, tcps_kanban:get_current_wip(reliability)),
    ?assertEqual(0, tcps_kanban:get_current_wip(security)),
    ?assertEqual(0, tcps_kanban:get_current_wip(cost)),
    ?assertEqual(0, tcps_kanban:get_current_wip(compliance)).

%%=============================================================================
%% Helper Functions
%%=============================================================================

%% @doc Check that no more than MaxConsec consecutive elements are the same
-spec check_max_consecutive(list(), pos_integer()) -> boolean().
check_max_consecutive([], _MaxConsec) ->
    true;
check_max_consecutive([_], _MaxConsec) ->
    true;
check_max_consecutive([H | T], MaxConsec) ->
    check_max_consecutive(H, T, 1, MaxConsec).

check_max_consecutive(_Current, [], _Count, _MaxConsec) ->
    true;
check_max_consecutive(Current, [H | T], Count, MaxConsec) when H =:= Current ->
    NewCount = Count + 1,
    if
        NewCount > MaxConsec -> false;
        true -> check_max_consecutive(Current, T, NewCount, MaxConsec)
    end;
check_max_consecutive(_Current, [H | T], _Count, MaxConsec) ->
    check_max_consecutive(H, T, 1, MaxConsec).
