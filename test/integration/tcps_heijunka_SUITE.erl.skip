%%%-------------------------------------------------------------------
%%% @doc TCPS Heijunka (Production Leveling) Test Suite
%%%
%%% Comprehensive testing of Heijunka scheduling including:
%%% - Load leveling across buckets
%%% - WIP limit enforcement
%%% - Anti-batching verification
%%% - Bucket balancing
%%% - Dynamic priority handling
%%% - Throughput optimization
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(tcps_heijunka_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% CT callbacks
-export([all/0,
         suite/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2]).

%% Test cases
-export([
    test_prevents_batching/1,
    test_respects_wip_limits/1,
    test_bucket_distribution/1,
    test_priority_handling/1,
    test_leveling_score/1,
    test_dynamic_rebalancing/1,
    test_throughput_optimization/1,
    test_mixed_workload/1,
    test_burst_handling/1,
    test_sustained_leveling/1,
    test_wip_limit_per_bucket/1,
    test_emergency_priority/1,
    test_schedule_consistency/1,
    test_optimal_interleaving/1,
    test_capacity_planning/1
]).

%%%===================================================================
%%% CT Callbacks
%%%===================================================================

suite() ->
    [{timetrap, {minutes, 5}}].

all() ->
    [
        test_prevents_batching,
        test_respects_wip_limits,
        test_bucket_distribution,
        test_priority_handling,
        test_leveling_score,
        test_dynamic_rebalancing,
        test_throughput_optimization,
        test_mixed_workload,
        test_burst_handling,
        test_sustained_leveling,
        test_wip_limit_per_bucket,
        test_emergency_priority,
        test_schedule_consistency,
        test_optimal_interleaving,
        test_capacity_planning
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(erlmcp),
    ok = tcps_test_utils:init_test_env(),
    Config.

end_per_suite(_Config) ->
    ok = tcps_test_utils:cleanup_test_env(),
    ok = application:stop(tcps),
    ok.

init_per_testcase(TestCase, Config) ->
    ct:pal("~n=== Starting test case: ~p ===~n", [TestCase]),
    ok = tcps_test_utils:clear_all_data(),
    Config.

end_per_testcase(TestCase, _Config) ->
    ct:pal("~n=== Completed test case: ~p ===~n", [TestCase]),
    ok.

%%%===================================================================
%%% Test Cases
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Test that Heijunka prevents batching
%% @end
%%--------------------------------------------------------------------
test_prevents_batching(_Config) ->
    ct:pal("~n=== Testing Prevents Batching ===~n"),

    %% Create 20 work orders: 10 security, 10 reliability
    SecurityWOs = tcps_test_utils:create_work_orders(security, 10),
    ReliabilityWOs = tcps_test_utils:create_work_orders(reliability, 10),

    AllWOs = SecurityWOs ++ ReliabilityWOs,
    ct:pal("Created 20 work orders: 10 security, 10 reliability~n"),

    %% Get Heijunka schedule
    Schedule = tcps_kanban:heijunka_schedule(),
    ?assertEqual(20, length(Schedule)),

    %% Verify no more than 2 consecutive from same bucket
    ok = verify_no_batching(Schedule, 2),
    ct:pal("No batching detected (max 2 consecutive)~n"),

    %% Calculate actual batching metric
    MaxConsecutive = calculate_max_consecutive(Schedule),
    ?assert(MaxConsecutive =< 2),
    ct:pal("Maximum consecutive from same bucket: ~p~n", [MaxConsecutive]),

    ct:pal("~n=== Prevents Batching: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test that WIP limits are respected
%% @end
%%--------------------------------------------------------------------
test_respects_wip_limits(_Config) ->
    ct:pal("~n=== Testing Respects WIP Limits ===~n"),

    %% Set WIP limit to 5 per bucket
    lists:foreach(fun(Bucket) ->
        ok = tcps_kanban:set_wip_limit(Bucket, 5)
    end, [reliability, security, cost, compliance]),

    ct:pal("Set WIP limits: 5 per bucket~n"),

    %% Try to start 10 security work orders
    SecurityWOs = tcps_test_utils:create_work_orders(security, 10),

    Results = lists:map(fun(WO) ->
        tcps_kanban:start_work_order(WO)
    end, SecurityWOs),

    %% Count results
    Started = length([ok || ok <- Results]),
    Rejected = length([{error, wip_limit} || {error, wip_limit} <- Results]),

    ?assertEqual(5, Started),
    ?assertEqual(5, Rejected),
    ct:pal("Started: ~p, Rejected: ~p~n", [Started, Rejected]),

    %% Verify WIP limit enforced
    {ok, WIPStatus} = tcps_kanban:get_wip_status(security),
    ?assertEqual(5, maps:get(current, WIPStatus)),
    ?assertEqual(5, maps:get(limit, WIPStatus)),

    ct:pal("~n=== Respects WIP Limits: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test bucket distribution
%% @end
%%--------------------------------------------------------------------
test_bucket_distribution(_Config) ->
    ct:pal("~n=== Testing Bucket Distribution ===~n"),

    %% Create equal work orders across buckets
    Buckets = [reliability, security, cost, compliance],
    AllWOs = lists:flatmap(fun(Bucket) ->
        tcps_test_utils:create_work_orders(Bucket, 5)
    end, Buckets),

    ?assertEqual(20, length(AllWOs)),
    ct:pal("Created 20 work orders (5 per bucket)~n"),

    %% Get schedule
    Schedule = tcps_kanban:heijunka_schedule(),

    %% Verify distribution
    Distribution = calculate_distribution(Schedule),
    lists:foreach(fun(Bucket) ->
        Count = maps:get(Bucket, Distribution),
        ?assertEqual(5, Count),
        ct:pal("~p: ~p work orders~n", [Bucket, Count])
    end, Buckets),

    %% Calculate distribution uniformity
    Uniformity = calculate_uniformity(Schedule),
    ?assert(Uniformity >= 0.8),
    ct:pal("Distribution uniformity: ~.2f~n", [Uniformity]),

    ct:pal("~n=== Bucket Distribution: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test priority handling
%% @end
%%--------------------------------------------------------------------
test_priority_handling(_Config) ->
    ct:pal("~n=== Testing Priority Handling ===~n"),

    %% Create work orders with different priorities
    {ok, WO_Low} = tcps_test_utils:create_test_work_order(#{
        bucket => cost,
        priority => low
    }),
    {ok, WO_Medium} = tcps_test_utils:create_test_work_order(#{
        bucket => reliability,
        priority => medium
    }),
    {ok, WO_High} = tcps_test_utils:create_test_work_order(#{
        bucket => security,
        priority => high
    }),
    {ok, WO_Critical} = tcps_test_utils:create_test_work_order(#{
        bucket => security,
        priority => critical
    }),

    ct:pal("Created work orders with varying priorities~n"),

    %% Get schedule
    Schedule = tcps_kanban:heijunka_schedule(),

    %% Critical should be first
    ?assertEqual(WO_Critical, hd(Schedule)),
    ct:pal("Critical work order scheduled first~n"),

    %% Verify high priority before medium
    CriticalPos = position(WO_Critical, Schedule),
    HighPos = position(WO_High, Schedule),
    MediumPos = position(WO_Medium, Schedule),
    LowPos = position(WO_Low, Schedule),

    ?assert(CriticalPos < HighPos),
    ?assert(HighPos < MediumPos),
    ?assert(MediumPos < LowPos),

    ct:pal("Priority order respected: Critical(~p) < High(~p) < Medium(~p) < Low(~p)~n",
           [CriticalPos, HighPos, MediumPos, LowPos]),

    ct:pal("~n=== Priority Handling: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test leveling score calculation
%% @end
%%--------------------------------------------------------------------
test_leveling_score(_Config) ->
    ct:pal("~n=== Testing Leveling Score ===~n"),

    %% Create well-leveled schedule
    WellLeveled = lists:flatmap(fun(Bucket) ->
        tcps_test_utils:create_work_orders(Bucket, 5)
    end, [reliability, security, cost, compliance]),

    Schedule1 = tcps_kanban:heijunka_schedule(),
    Score1 = tcps_kanban:calculate_leveling_score(Schedule1),
    ?assert(Score1 >= 0.8),
    ct:pal("Well-leveled score: ~.2f~n", [Score1]),

    %% Create poorly-leveled schedule (all security)
    ok = tcps_test_utils:clear_all_data(),
    PoorlyLeveled = tcps_test_utils:create_work_orders(security, 20),

    Schedule2 = tcps_kanban:heijunka_schedule(),
    Score2 = tcps_kanban:calculate_leveling_score(Schedule2),
    ?assert(Score2 < 0.5),
    ct:pal("Poorly-leveled score: ~.2f~n", [Score2]),

    %% Verify well-leveled > poorly-leveled
    ?assert(Score1 > Score2),

    ct:pal("~n=== Leveling Score: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test dynamic rebalancing
%% @end
%%--------------------------------------------------------------------
test_dynamic_rebalancing(_Config) ->
    ct:pal("~n=== Testing Dynamic Rebalancing ===~n"),

    %% Start with balanced load
    lists:foreach(fun(Bucket) ->
        tcps_test_utils:create_work_orders(Bucket, 5)
    end, [reliability, security, cost, compliance]),

    Schedule1 = tcps_kanban:heijunka_schedule(),
    Score1 = tcps_kanban:calculate_leveling_score(Schedule1),
    ct:pal("Initial leveling score: ~.2f~n", [Score1]),

    %% Add burst of security work orders
    tcps_test_utils:create_work_orders(security, 10),

    Schedule2 = tcps_kanban:heijunka_schedule(),
    Score2 = tcps_kanban:calculate_leveling_score(Schedule2),
    ct:pal("After security burst: ~.2f~n", [Score2]),

    %% Score should still be reasonable (rebalanced)
    ?assert(Score2 >= 0.6),

    %% Verify security distributed
    FirstHalf = lists:sublist(Schedule2, 1, 15),
    SecondHalf = lists:sublist(Schedule2, 16, 15),

    SecurityInFirst = count_bucket_in_list(security, FirstHalf),
    SecurityInSecond = count_bucket_in_list(security, SecondHalf),

    %% Should be distributed, not all at start or end
    ?assert(SecurityInFirst >= 5),
    ?assert(SecurityInSecond >= 5),
    ct:pal("Security distributed: ~p in first half, ~p in second half~n",
           [SecurityInFirst, SecurityInSecond]),

    ct:pal("~n=== Dynamic Rebalancing: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test throughput optimization
%% @end
%%--------------------------------------------------------------------
test_throughput_optimization(_Config) ->
    ct:pal("~n=== Testing Throughput Optimization ===~n"),

    %% Create diverse workload
    lists:foreach(fun(Bucket) ->
        tcps_test_utils:create_work_orders(Bucket, 10)
    end, [reliability, security, cost, compliance]),

    %% Process with Heijunka scheduling
    StartTime1 = erlang:monotonic_time(millisecond),
    Schedule = tcps_kanban:heijunka_schedule(),
    lists:foreach(fun(WO) ->
        ok = tcps_kanban:start_work_order(WO),
        ok = tcps_work_order:complete(WO, <<"sku-test">>)
    end, lists:sublist(Schedule, 20)),
    EndTime1 = erlang:monotonic_time(millisecond),
    Duration1 = EndTime1 - StartTime1,

    ct:pal("Heijunka throughput: 20 work orders in ~p ms~n", [Duration1]),

    %% Compare with random ordering (reset)
    ok = tcps_test_utils:clear_all_data(),
    lists:foreach(fun(Bucket) ->
        tcps_test_utils:create_work_orders(Bucket, 10)
    end, [reliability, security, cost, compliance]),

    AllWOs = tcps_work_order:list_all(),
    RandomOrder = tcps_test_utils:shuffle(AllWOs),

    StartTime2 = erlang:monotonic_time(millisecond),
    lists:foreach(fun(WO) ->
        case tcps_kanban:start_work_order(WO) of
            ok -> ok = tcps_work_order:complete(WO, <<"sku-test">>);
            _ -> skip
        end
    end, lists:sublist(RandomOrder, 20)),
    EndTime2 = erlang:monotonic_time(millisecond),
    Duration2 = EndTime2 - StartTime2,

    ct:pal("Random throughput: 20 work orders in ~p ms~n", [Duration2]),

    %% Heijunka should be at least as fast (ideally faster)
    ?assert(Duration1 =< Duration2 * 1.2), % Allow 20% margin

    ct:pal("~n=== Throughput Optimization: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test mixed workload handling
%% @end
%%--------------------------------------------------------------------
test_mixed_workload(_Config) ->
    ct:pal("~n=== Testing Mixed Workload ===~n"),

    %% Create imbalanced workload
    tcps_test_utils:create_work_orders(security, 15),
    tcps_test_utils:create_work_orders(reliability, 3),
    tcps_test_utils:create_work_orders(cost, 1),
    tcps_test_utils:create_work_orders(compliance, 1),

    ct:pal("Created imbalanced workload: 15-3-1-1~n"),

    Schedule = tcps_kanban:heijunka_schedule(),

    %% Verify no batching despite imbalance
    ok = verify_no_batching(Schedule, 3),
    ct:pal("No excessive batching~n"),

    %% Verify minority buckets represented
    Distribution = calculate_distribution(Schedule),
    ?assert(maps:get(cost, Distribution) >= 1),
    ?assert(maps:get(compliance, Distribution) >= 1),

    ct:pal("All buckets represented in schedule~n"),

    ct:pal("~n=== Mixed Workload: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test burst handling
%% @end
%%--------------------------------------------------------------------
test_burst_handling(_Config) ->
    ct:pal("~n=== Testing Burst Handling ===~n"),

    %% Start with steady state
    lists:foreach(fun(Bucket) ->
        tcps_test_utils:create_work_orders(Bucket, 5)
    end, [reliability, security, cost, compliance]),

    InitialSchedule = tcps_kanban:heijunka_schedule(),
    ?assertEqual(20, length(InitialSchedule)),

    %% Add burst of 20 security work orders
    tcps_test_utils:create_work_orders(security, 20),

    BurstSchedule = tcps_kanban:heijunka_schedule(),
    ?assertEqual(40, length(BurstSchedule)),
    ct:pal("Schedule grew from 20 to 40 work orders~n"),

    %% Verify burst integrated smoothly
    LevelingScore = tcps_kanban:calculate_leveling_score(BurstSchedule),
    ?assert(LevelingScore >= 0.5),
    ct:pal("Leveling score after burst: ~.2f~n", [LevelingScore]),

    %% Verify no excessive batching
    ok = verify_no_batching(BurstSchedule, 4),

    ct:pal("~n=== Burst Handling: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test sustained leveling over time
%% @end
%%--------------------------------------------------------------------
test_sustained_leveling(_Config) ->
    ct:pal("~n=== Testing Sustained Leveling ===~n"),

    %% Simulate sustained operation
    Iterations = 10,
    Scores = lists:map(fun(N) ->
        %% Add work orders
        Bucket = lists:nth((N rem 4) + 1,
                          [reliability, security, cost, compliance]),
        tcps_test_utils:create_work_orders(Bucket, 5),

        %% Complete some
        Schedule = tcps_kanban:heijunka_schedule(),
        ToComplete = lists:sublist(Schedule, 3),
        lists:foreach(fun(WO) ->
            case tcps_kanban:start_work_order(WO) of
                ok -> ok = tcps_work_order:complete(WO, <<"sku-test">>);
                _ -> skip
            end
        end, ToComplete),

        %% Calculate score
        NewSchedule = tcps_kanban:heijunka_schedule(),
        tcps_kanban:calculate_leveling_score(NewSchedule)
    end, lists:seq(1, Iterations)),

    %% Verify consistent leveling
    AvgScore = lists:sum(Scores) / length(Scores),
    ?assert(AvgScore >= 0.7),
    ct:pal("Average leveling score over ~p iterations: ~.2f~n",
           [Iterations, AvgScore]),

    %% Verify no degradation
    [FirstScore | _] = Scores,
    LastScore = lists:last(Scores),
    ?assert(LastScore >= FirstScore * 0.9), % Allow 10% variation

    ct:pal("~n=== Sustained Leveling: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test WIP limit per bucket
%% @end
%%--------------------------------------------------------------------
test_wip_limit_per_bucket(_Config) ->
    ct:pal("~n=== Testing WIP Limit Per Bucket ===~n"),

    %% Set different limits per bucket
    ok = tcps_kanban:set_wip_limit(reliability, 10),
    ok = tcps_kanban:set_wip_limit(security, 5),
    ok = tcps_kanban:set_wip_limit(cost, 3),
    ok = tcps_kanban:set_wip_limit(compliance, 8),

    ct:pal("Set different WIP limits per bucket~n"),

    %% Create and try to start work orders
    lists:foreach(fun(Bucket) ->
        WOs = tcps_test_utils:create_work_orders(Bucket, 15),

        Results = lists:map(fun(WO) ->
            tcps_kanban:start_work_order(WO)
        end, WOs),

        Started = length([ok || ok <- Results]),

        ExpectedLimit = case Bucket of
            reliability -> 10;
            security -> 5;
            cost -> 3;
            compliance -> 8
        end,

        ?assertEqual(ExpectedLimit, Started),
        ct:pal("~p: Started ~p (limit ~p)~n",
               [Bucket, Started, ExpectedLimit])
    end, [reliability, security, cost, compliance]),

    ct:pal("~n=== WIP Limit Per Bucket: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test emergency priority work orders
%% @end
%%--------------------------------------------------------------------
test_emergency_priority(_Config) ->
    ct:pal("~n=== Testing Emergency Priority ===~n"),

    %% Create normal workload
    lists:foreach(fun(Bucket) ->
        tcps_test_utils:create_work_orders(Bucket, 5)
    end, [reliability, security, cost, compliance]),

    InitialSchedule = tcps_kanban:heijunka_schedule(),
    ct:pal("Initial schedule: ~p work orders~n", [length(InitialSchedule)]),

    %% Add emergency work order
    {ok, EmergencyWO} = tcps_test_utils:create_test_work_order(#{
        bucket => security,
        priority => emergency
    }),

    ct:pal("Added emergency work order~n"),

    %% Get new schedule
    NewSchedule = tcps_kanban:heijunka_schedule(),

    %% Emergency should be first
    ?assertEqual(EmergencyWO, hd(NewSchedule)),
    ct:pal("Emergency work order scheduled first~n"),

    %% Verify rest still leveled
    RestSchedule = tl(NewSchedule),
    LevelingScore = tcps_kanban:calculate_leveling_score(RestSchedule),
    ?assert(LevelingScore >= 0.7),

    ct:pal("~n=== Emergency Priority: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test schedule consistency
%% @end
%%--------------------------------------------------------------------
test_schedule_consistency(_Config) ->
    ct:pal("~n=== Testing Schedule Consistency ===~n"),

    %% Create work orders
    lists:foreach(fun(Bucket) ->
        tcps_test_utils:create_work_orders(Bucket, 10)
    end, [reliability, security, cost, compliance]),

    %% Get schedule multiple times
    Schedule1 = tcps_kanban:heijunka_schedule(),
    Schedule2 = tcps_kanban:heijunka_schedule(),
    Schedule3 = tcps_kanban:heijunka_schedule(),

    %% Should be identical (no work orders added/removed)
    ?assertEqual(Schedule1, Schedule2),
    ?assertEqual(Schedule2, Schedule3),
    ct:pal("Schedule consistent across multiple calls~n"),

    %% Complete one work order
    [FirstWO | _] = Schedule1,
    ok = tcps_kanban:start_work_order(FirstWO),
    ok = tcps_work_order:complete(FirstWO, <<"sku-test">>),

    %% New schedule should exclude completed
    Schedule4 = tcps_kanban:heijunka_schedule(),
    ?assertEqual(39, length(Schedule4)),
    ?assertNot(lists:member(FirstWO, Schedule4)),
    ct:pal("Schedule updated after completion~n"),

    ct:pal("~n=== Schedule Consistency: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test optimal interleaving
%% @end
%%--------------------------------------------------------------------
test_optimal_interleaving(_Config) ->
    ct:pal("~n=== Testing Optimal Interleaving ===~n"),

    %% Create work orders
    lists:foreach(fun(Bucket) ->
        tcps_test_utils:create_work_orders(Bucket, 8)
    end, [reliability, security, cost, compliance]),

    Schedule = tcps_kanban:heijunka_schedule(),

    %% Verify interleaving pattern
    %% Check every 4-work-order window has all 4 buckets represented
    Windows = create_windows(Schedule, 4),

    AllDiverse = lists:all(fun(Window) ->
        Buckets = get_buckets_in_window(Window),
        length(lists:usort(Buckets)) >= 3 % At least 3 different buckets
    end, Windows),

    ?assert(AllDiverse),
    ct:pal("All 4-work-order windows have diverse buckets~n"),

    ct:pal("~n=== Optimal Interleaving: SUCCESS ===~n"),
    ok.

%%--------------------------------------------------------------------
%% @doc Test capacity planning
%% @end
%%--------------------------------------------------------------------
test_capacity_planning(_Config) ->
    ct:pal("~n=== Testing Capacity Planning ===~n"),

    %% Set WIP limits based on capacity
    ok = tcps_kanban:set_wip_limit(reliability, 10),
    ok = tcps_kanban:set_wip_limit(security, 8),
    ok = tcps_kanban:set_wip_limit(cost, 5),
    ok = tcps_kanban:set_wip_limit(compliance, 7),

    TotalCapacity = 10 + 8 + 5 + 7,
    ?assertEqual(30, TotalCapacity),

    %% Create work orders exceeding capacity
    lists:foreach(fun(Bucket) ->
        tcps_test_utils:create_work_orders(Bucket, 20)
    end, [reliability, security, cost, compliance]),

    Schedule = tcps_kanban:heijunka_schedule(),
    ?assertEqual(80, length(Schedule)),

    %% Try to start all
    Started = lists:foldl(fun(WO, Acc) ->
        case tcps_kanban:start_work_order(WO) of
            ok -> Acc + 1;
            {error, wip_limit} -> Acc
        end
    end, 0, Schedule),

    %% Should respect total capacity
    ?assertEqual(TotalCapacity, Started),
    ct:pal("Started ~p work orders (capacity: ~p)~n",
           [Started, TotalCapacity]),

    %% Verify capacity utilization
    {ok, Utilization} = tcps_kanban:get_capacity_utilization(),
    ?assertEqual(1.0, Utilization), % 100% utilized

    ct:pal("Capacity utilization: 100%%~n"),

    ct:pal("~n=== Capacity Planning: SUCCESS ===~n"),
    ok.

%%%===================================================================
%%% Helper Functions
%%%===================================================================

verify_no_batching(Schedule, MaxConsecutive) ->
    verify_no_batching(Schedule, MaxConsecutive, undefined, 0).

verify_no_batching([], _, _, _) ->
    ok;
verify_no_batching([WO | Rest], MaxConsecutive, PrevBucket, Count) ->
    {ok, WOData} = tcps_work_order:get(WO),
    Bucket = maps:get(bucket, WOData),

    NewCount = case Bucket =:= PrevBucket of
        true -> Count + 1;
        false -> 1
    end,

    case NewCount > MaxConsecutive of
        true -> {error, batching_detected};
        false -> verify_no_batching(Rest, MaxConsecutive, Bucket, NewCount)
    end.

calculate_max_consecutive(Schedule) ->
    calculate_max_consecutive(Schedule, undefined, 0, 0).

calculate_max_consecutive([], _, CurrentCount, MaxCount) ->
    max(CurrentCount, MaxCount);
calculate_max_consecutive([WO | Rest], PrevBucket, CurrentCount, MaxCount) ->
    {ok, WOData} = tcps_work_order:get(WO),
    Bucket = maps:get(bucket, WOData),

    {NewCount, NewMax} = case Bucket =:= PrevBucket of
        true -> {CurrentCount + 1, max(CurrentCount + 1, MaxCount)};
        false -> {1, MaxCount}
    end,

    calculate_max_consecutive(Rest, Bucket, NewCount, NewMax).

calculate_distribution(Schedule) ->
    lists:foldl(fun(WO, Acc) ->
        {ok, WOData} = tcps_work_order:get(WO),
        Bucket = maps:get(bucket, WOData),
        maps:update_with(Bucket, fun(V) -> V + 1 end, 1, Acc)
    end, #{}, Schedule).

calculate_uniformity(Schedule) ->
    Distribution = calculate_distribution(Schedule),
    Counts = maps:values(Distribution),
    case length(Counts) of
        0 -> 0.0;
        _ ->
            Avg = lists:sum(Counts) / length(Counts),
            Variance = lists:sum([math:pow(C - Avg, 2) || C <- Counts]) /
                       length(Counts),
            StdDev = math:sqrt(Variance),
            %% Return normalized uniformity (1.0 = perfect, 0.0 = terrible)
            case Avg of
                0 -> 0.0;
                _ -> max(0.0, 1.0 - (StdDev / Avg))
            end
    end.

count_bucket_in_list(Bucket, List) ->
    length([WO || WO <- List,
                  {ok, WOData} <- [tcps_work_order:get(WO)],
                  maps:get(bucket, WOData) =:= Bucket]).

position(Element, List) ->
    position(Element, List, 1).

position(Element, [Element | _], Pos) -> Pos;
position(Element, [_ | Rest], Pos) -> position(Element, Rest, Pos + 1);
position(_, [], _) -> not_found.

create_windows(List, WindowSize) ->
    create_windows(List, WindowSize, []).

create_windows(List, WindowSize, Acc) when length(List) < WindowSize ->
    lists:reverse(Acc);
create_windows(List, WindowSize, Acc) ->
    Window = lists:sublist(List, WindowSize),
    create_windows(tl(List), WindowSize, [Window | Acc]).

get_buckets_in_window(Window) ->
    lists:map(fun(WO) ->
        {ok, WOData} = tcps_work_order:get(WO),
        maps:get(bucket, WOData)
    end, Window).
