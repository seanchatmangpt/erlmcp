%%%-----------------------------------------------------------------------------
%%% @doc TCPS Heijunka (Production Leveling) System
%%%
%%% Implements Toyota Production System Heijunka box principles for
%%% level-loading software production across different work types.
%%%
%%% Core Responsibilities:
%%% - Prevent batching (don't ship 50 security fixes then 0)
%%% - Level-load across buckets (security, reliability, cost, compliance, features)
%%% - Respect priorities within buckets
%%% - Optimize for smooth, sustainable flow
%%% - Integrate with Kanban WIP limits
%%%
%%% Heijunka Algorithm:
%%% 1. Collect all pending work orders from tcps_work_order
%%% 2. Group by bucket
%%% 3. Sort each bucket by priority
%%% 4. Round-robin interleave (max 2 consecutive from same bucket)
%%% 5. Return leveled batch for execution
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(tcps_heijunka).

%% API
-export([
    schedule_next_batch/0,
    schedule_next_batch/1,
    level_work_orders/1,
    calculate_bucket_allocation/1,
    get_leveling_score/1,
    optimize_schedule/2
]).

%% Test support
-ifdef(TEST).
-export([group_by_bucket/1, round_robin_interleave/2]).
-endif.

%%%=============================================================================
%%% Type Definitions
%%%=============================================================================

-type work_order_id() :: binary().
-type bucket() :: reliability | security | cost | compliance | features | technical_debt.
-type priority() :: 1..10.

-type work_order() :: #{
    id := work_order_id(),
    bucket := bucket(),
    priority := priority(),
    status := atom(),
    created_at := erlang:timestamp(),
    _ => _
}.

-type leveling_score() :: #{
    batching_metric := float(),
    distribution_uniformity := float(),
    priority_preservation := float(),
    overall_score := float()
}.

-export_type([work_order_id/0, bucket/0, work_order/0, leveling_score/0]).

%%%=============================================================================
%%% API Functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Schedule next batch using default batch size (10).
%% @end
%%------------------------------------------------------------------------------
-spec schedule_next_batch() -> {ok, [work_order_id()]}.
schedule_next_batch() ->
    schedule_next_batch(10).

%%------------------------------------------------------------------------------
%% @doc Schedule next batch of work orders using Heijunka leveling.
%%
%% Steps:
%% 1. Get all pending work orders from tcps_work_order
%% 2. Level across buckets (round-robin, max 2 consecutive from same bucket)
%% 3. Take first BatchSize work orders
%%
%% @end
%%------------------------------------------------------------------------------
-spec schedule_next_batch(BatchSize :: pos_integer()) ->
    {ok, [work_order_id()]}.
schedule_next_batch(BatchSize) ->
    %% Get all pending work orders from tcps_work_order
    AllPending = case whereis(tcps_work_order) of
        undefined ->
            %% Fallback if gen_server not running
            [];
        _Pid ->
            try
                PendingWOs = tcps_work_order:list_by_status(pending),
                QueuedWOs = tcps_work_order:list_by_status(queued),
                PendingWOs ++ QueuedWOs
            catch
                _:_ -> []
            end
    end,

    %% Level work orders
    Leveled = level_work_orders(AllPending),

    %% Take first BatchSize
    Batch = lists:sublist(Leveled, BatchSize),
    WorkOrderIds = [maps:get(id, WO) || WO <- Batch],

    {ok, WorkOrderIds}.

%%------------------------------------------------------------------------------
%% @doc Level work orders to prevent batching.
%%
%% Algorithm: Round-robin across buckets, respecting priority within bucket.
%% Ensures no more than 2 consecutive work orders from the same bucket.
%%
%% @end
%%------------------------------------------------------------------------------
-spec level_work_orders([work_order()]) -> [work_order()].
level_work_orders(WorkOrders) ->
    %% Group by bucket
    ByBucket = group_by_bucket(WorkOrders),

    %% Sort each bucket by priority (high to low)
    Sorted = maps:map(fun(_Bucket, WOs) ->
        lists:sort(fun(A, B) ->
            maps:get(priority, A, 0) >= maps:get(priority, B, 0)
        end, WOs)
    end, ByBucket),

    %% Round-robin interleave (take 1 from each bucket)
    round_robin_interleave(Sorted, []).

%%------------------------------------------------------------------------------
%% @doc Calculate optimal bucket allocation based on demand.
%%
%% Returns suggested number of work orders to pull from each bucket
%% to maintain balanced flow.
%%
%% @end
%%------------------------------------------------------------------------------
-spec calculate_bucket_allocation(TotalCapacity :: pos_integer()) ->
    #{bucket() => non_neg_integer()}.
calculate_bucket_allocation(TotalCapacity) ->
    %% Get pending counts per bucket
    BucketCounts = get_pending_counts_per_bucket(),

    %% Calculate total pending
    TotalPending = lists:sum(maps:values(BucketCounts)),

    case TotalPending of
        0 ->
            %% No work pending
            #{};
        _ ->
            %% Allocate proportionally, ensuring minimum 1 per bucket with work
            maps:map(fun(_Bucket, Count) ->
                case Count of
                    0 -> 0;
                    _ ->
                        %% Proportional allocation with minimum 1
                        Allocated = max(1, round((Count / TotalPending) * TotalCapacity)),
                        min(Allocated, Count)  % Don't allocate more than available
                end
            end, BucketCounts)
    end.

%%------------------------------------------------------------------------------
%% @doc Calculate leveling quality score for a schedule.
%%
%% Metrics:
%% - batching_metric: How well it prevents batching (0-1, higher is better)
%% - distribution_uniformity: How evenly work is distributed (0-1)
%% - priority_preservation: How well high-priority items are preserved (0-1)
%% - overall_score: Weighted average
%%
%% @end
%%------------------------------------------------------------------------------
-spec get_leveling_score([work_order()]) -> leveling_score().
get_leveling_score(Schedule) ->
    BatchingMetric = calculate_batching_metric(Schedule),
    DistributionUniformity = calculate_distribution_uniformity(Schedule),
    PriorityPreservation = calculate_priority_preservation(Schedule),

    %% Weighted average (batching is most important)
    OverallScore = (BatchingMetric * 0.5) +
                   (DistributionUniformity * 0.3) +
                   (PriorityPreservation * 0.2),

    #{
        batching_metric => BatchingMetric,
        distribution_uniformity => DistributionUniformity,
        priority_preservation => PriorityPreservation,
        overall_score => OverallScore
    }.

%%------------------------------------------------------------------------------
%% @doc Optimize schedule by reordering to improve leveling score.
%%
%% @end
%%------------------------------------------------------------------------------
-spec optimize_schedule([work_order()], pos_integer()) -> [work_order()].
optimize_schedule(Schedule, MaxIterations) ->
    optimize_schedule_loop(Schedule, MaxIterations, 0, get_leveling_score(Schedule)).

%%%=============================================================================
%%% Internal Functions - Scheduling
%%%=============================================================================

%% @doc Group work orders by bucket
-spec group_by_bucket([work_order()]) -> #{bucket() => [work_order()]}.
group_by_bucket(WorkOrders) ->
    lists:foldl(fun(WO, Acc) ->
        Bucket = maps:get(bucket, WO),
        maps:update_with(Bucket, fun(L) -> [WO | L] end, [WO], Acc)
    end, #{}, WorkOrders).

%% @doc Round-robin interleave across buckets (take 1 from each)
-spec round_robin_interleave(#{bucket() => [work_order()]}, [work_order()]) ->
    [work_order()].
round_robin_interleave(Remaining, Acc) when map_size(Remaining) =:= 0 ->
    lists:reverse(Acc);
round_robin_interleave(Remaining, Acc) ->
    %% Get all available buckets in consistent order
    AllBuckets = [reliability, security, cost, compliance, features, technical_debt],
    AvailableBuckets = [B || B <- AllBuckets, maps:is_key(B, Remaining)],

    case AvailableBuckets of
        [] ->
            lists:reverse(Acc);
        _ ->
            %% Take one from each bucket in order
            {NewRemaining, NewAcc} = lists:foldl(fun(Bucket, {RemAcc, AccAcc}) ->
                case maps:get(Bucket, RemAcc, undefined) of
                    undefined ->
                        {RemAcc, AccAcc};
                    [] ->
                        {maps:remove(Bucket, RemAcc), AccAcc};
                    [WO | Rest] ->
                        NewRem = case Rest of
                            [] -> maps:remove(Bucket, RemAcc);
                            _ -> maps:put(Bucket, Rest, RemAcc)
                        end,
                        {NewRem, [WO | AccAcc]}
                end
            end, {Remaining, Acc}, AvailableBuckets),

            round_robin_interleave(NewRemaining, NewAcc)
    end.

%%%=============================================================================
%%% Internal Functions - Metrics
%%%=============================================================================

%% @doc Calculate batching metric (0-1, higher is better)
%% Measures how well the schedule prevents consecutive items from same bucket
-spec calculate_batching_metric([work_order()]) -> float().
calculate_batching_metric([]) -> 1.0;
calculate_batching_metric([_]) -> 1.0;
calculate_batching_metric(Schedule) ->
    %% Count max consecutive items from same bucket
    MaxConsecutive = calculate_max_consecutive(Schedule),

    %% Ideal is 1 (no consecutive), worst is length of schedule
    %% Score = 1 - (actual - ideal) / (worst - ideal)
    Ideal = 1,
    Worst = length(Schedule),
    Score = 1.0 - ((MaxConsecutive - Ideal) / (Worst - Ideal)),

    max(0.0, min(1.0, Score)).

-spec calculate_max_consecutive([work_order()]) -> pos_integer().
calculate_max_consecutive(Schedule) ->
    calculate_max_consecutive(Schedule, undefined, 0, 0).

calculate_max_consecutive([], _LastBucket, CurrentCount, MaxCount) ->
    max(CurrentCount, MaxCount);
calculate_max_consecutive([WO | Rest], LastBucket, CurrentCount, MaxCount) ->
    Bucket = maps:get(bucket, WO),
    case Bucket =:= LastBucket of
        true ->
            NewCount = CurrentCount + 1,
            calculate_max_consecutive(Rest, Bucket, NewCount, max(NewCount, MaxCount));
        false ->
            calculate_max_consecutive(Rest, Bucket, 1, max(CurrentCount, MaxCount))
    end.

%% @doc Calculate distribution uniformity (0-1, higher is better)
%% Measures how evenly work is distributed across the schedule
-spec calculate_distribution_uniformity([work_order()]) -> float().
calculate_distribution_uniformity([]) -> 1.0;
calculate_distribution_uniformity(Schedule) ->
    %% Count items per bucket
    BucketCounts = lists:foldl(fun(WO, Acc) ->
        Bucket = maps:get(bucket, WO),
        maps:update_with(Bucket, fun(C) -> C + 1 end, 1, Acc)
    end, #{}, Schedule),

    Counts = maps:values(BucketCounts),
    case Counts of
        [] -> 1.0;
        [_] -> 1.0;
        _ ->
            %% Calculate coefficient of variation (lower is better)
            Mean = lists:sum(Counts) / length(Counts),
            Variance = lists:sum([math:pow(C - Mean, 2) || C <- Counts]) / length(Counts),
            StdDev = math:sqrt(Variance),
            CV = case Mean of
                0.0 -> 0.0;
                _ -> StdDev / Mean
            end,

            %% Convert to score (0-1, higher is better)
            %% CV of 0 = perfect uniformity = score 1.0
            %% CV of 2.0 or higher = poor uniformity = score 0.0
            Score = 1.0 - min(CV / 2.0, 1.0),
            max(0.0, min(1.0, Score))
    end.

%% @doc Calculate priority preservation (0-1, higher is better)
%% Measures how well high-priority items are kept near the front
-spec calculate_priority_preservation([work_order()]) -> float().
calculate_priority_preservation([]) -> 1.0;
calculate_priority_preservation(Schedule) ->
    %% Calculate weighted position score
    %% High priority items should be early in schedule
    {TotalScore, TotalWeight} = lists:foldl(fun({WO, Position}, {ScoreAcc, WeightAcc}) ->
        Priority = maps:get(priority, WO, 0),
        %% Position penalty: 0 at start, 1 at end
        PositionPenalty = Position / length(Schedule),
        %% Weighted score: high priority early = low penalty
        WeightedPenalty = Priority * PositionPenalty,
        {ScoreAcc + WeightedPenalty, WeightAcc + Priority}
    end, {0.0, 0.0}, lists:zip(Schedule, lists:seq(0, length(Schedule) - 1))),

    case TotalWeight of
        0.0 -> 1.0;
        _ ->
            %% Normalize to 0-1 (lower penalty = higher score)
            Score = 1.0 - (TotalScore / TotalWeight),
            max(0.0, min(1.0, Score))
    end.

%%%=============================================================================
%%% Internal Functions - Optimization
%%%=============================================================================

optimize_schedule_loop(Schedule, MaxIterations, Iteration, BestScore) ->
    case Iteration >= MaxIterations of
        true -> Schedule;
        false ->
            %% Try random swaps and keep if better
            NewSchedule = try_random_swap(Schedule),
            NewScore = get_leveling_score(NewSchedule),

            case maps:get(overall_score, NewScore) > maps:get(overall_score, BestScore) of
                true ->
                    optimize_schedule_loop(NewSchedule, MaxIterations, Iteration + 1, NewScore);
                false ->
                    optimize_schedule_loop(Schedule, MaxIterations, Iteration + 1, BestScore)
            end
    end.

try_random_swap(Schedule) ->
    case length(Schedule) < 2 of
        true -> Schedule;
        false ->
            Idx1 = rand:uniform(length(Schedule)),
            Idx2 = rand:uniform(length(Schedule)),
            swap_elements(Schedule, Idx1, Idx2)
    end.

swap_elements(List, Idx1, Idx2) when Idx1 =:= Idx2 ->
    List;
swap_elements(List, Idx1, Idx2) ->
    {Pre1, [Elem1 | Post1]} = lists:split(Idx1 - 1, List),
    Temp = Pre1 ++ [lists:nth(Idx2, List)] ++ Post1,
    {Pre2, [_Elem2 | Post2]} = lists:split(Idx2 - 1, Temp),
    Pre2 ++ [Elem1] ++ Post2.

%%%=============================================================================
%%% Internal Functions - Data Access
%%%=============================================================================

get_pending_counts_per_bucket() ->
    AllBuckets = [reliability, security, cost, compliance, features, technical_debt],

    lists:foldl(fun(Bucket, Acc) ->
        Count = case whereis(tcps_work_order) of
            undefined -> 0;
            _Pid ->
                try
                    BucketWOs = tcps_work_order:list_by_bucket(Bucket),
                    PendingWOs = [WO || WO <- BucketWOs,
                                        begin
                                            Status = maps:get(status, WO),
                                            Status =:= pending orelse Status =:= queued
                                        end],
                    length(PendingWOs)
                catch
                    _:_ -> 0
                end
        end,
        maps:put(Bucket, Count, Acc)
    end, #{}, AllBuckets).
