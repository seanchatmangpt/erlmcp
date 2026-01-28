%%%-------------------------------------------------------------------
%%% @doc
%%% Latency Profiler for Scale Testing - Identify Slow Operations at 100K Scale
%%%
%%% Measures operation latencies and identifies slow paths:
%%% - Fast operations (<50ms)
%%% - Slow operations (50-100ms)
%%% - Very slow operations (100-500ms)
%%% - Critical bottlenecks (>500ms)
%%%
%%% Provides percentile analysis: p50, p95, p99, p99.9, max
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_latency_profiler).

-export([
    start_profiling/0,
    stop_profiling/0,
    measure_operation/2,
    measure_operation/3,
    get_latency_stats/0,
    get_percentiles/0,
    get_slow_operations/1,
    analyze_latency_distribution/0,
    get_profiling_report/0,
    classify_latency/1
]).

-include_lib("kernel/include/logger.hrl").

-define(LATENCY_TABLE, erlmcp_latency_profiling).

-record(latency_sample, {
    operation :: atom(),
    timestamp :: integer(),
    duration_us :: integer(),
    category :: atom()
}).

-record(latency_profile, {
    start_time :: integer(),
    samples = [] :: [#latency_sample{}],
    operation_counts = #{} :: map(),
    operation_times = #{} :: map(),
    total_samples = 0 :: integer(),
    enabled = false :: boolean()
}).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start latency profiling
-spec start_profiling() -> ok.
start_profiling() ->
    case ets:whereis(?LATENCY_TABLE) of
        undefined ->
            ets:new(?LATENCY_TABLE, [
                named_table,
                public,
                {write_concurrency, true},
                {read_concurrency, true}
            ]);
        _ ->
            ets:delete_all_objects(?LATENCY_TABLE)
    end,

    Profile = #latency_profile{
        start_time = erlang:system_time(microsecond),
        enabled = true
    },

    ets:insert(?LATENCY_TABLE, {profile, Profile}),
    logger:info("Latency profiling started"),
    ok.

%% @doc Stop latency profiling
-spec stop_profiling() -> map().
stop_profiling() ->
    case ets:lookup(?LATENCY_TABLE, profile) of
        [{profile, Profile}] ->
            Samples = lists:reverse(Profile#latency_profile.samples),
            Durations = [S#latency_sample.duration_us || S <- Samples],

            Report = #{
                total_samples => length(Samples),
                elapsed_us => erlang:system_time(microsecond) - Profile#latency_profile.start_time,
                min_latency_us => case Durations of [] -> 0; _ -> lists:min(Durations) end,
                max_latency_us => case Durations of [] -> 0; _ -> lists:max(Durations) end,
                avg_latency_us => avg_list(Durations),
                median_latency_us => median_list(Durations),
                operations_tracked => map_size(Profile#latency_profile.operation_counts)
            },

            ets:insert(?LATENCY_TABLE, {stopped, true}),
            Report;
        [] ->
            {error, profiling_not_started}
    end.

%% @doc Measure operation latency
-spec measure_operation(atom(), integer()) -> ok.
measure_operation(Operation, DurationUs) when is_atom(Operation), is_integer(DurationUs) ->
    measure_operation(Operation, DurationUs, []).

-spec measure_operation(atom(), integer(), list()) -> ok.
measure_operation(Operation, DurationUs, _Context) when is_atom(Operation), is_integer(DurationUs) ->
    case ets:lookup(?LATENCY_TABLE, profile) of
        [{profile, Profile}] ->
            case Profile#latency_profile.enabled of
                true ->
                    Category = classify_latency(DurationUs),
                    Sample = #latency_sample{
                        operation = Operation,
                        timestamp = erlang:system_time(microsecond),
                        duration_us = DurationUs,
                        category = Category
                    },

                    OpCounts = Profile#latency_profile.operation_counts,
                    CurrentCount = maps:get(Operation, OpCounts, 0),
                    NewOpCounts = OpCounts#{Operation => CurrentCount + 1},

                    OpTimes = Profile#latency_profile.operation_times,
                    {OldSum, OldMax, OldMin} = maps:get(Operation, OpTimes, {0, 0, DurationUs}),
                    NewOpTimes = OpTimes#{
                        Operation => {
                            OldSum + DurationUs,
                            max(OldMax, DurationUs),
                            min(OldMin, DurationUs)
                        }
                    },

                    UpdatedProfile = Profile#latency_profile{
                        samples = [Sample | Profile#latency_profile.samples],
                        operation_counts = NewOpCounts,
                        operation_times = NewOpTimes,
                        total_samples = Profile#latency_profile.total_samples + 1
                    },

                    ets:insert(?LATENCY_TABLE, {profile, UpdatedProfile}),
                    ok;
                false ->
                    ok
            end;
        [] ->
            ok
    end.

%% @doc Classify latency into category
-spec classify_latency(integer()) -> atom().
classify_latency(DurationUs) ->
    case DurationUs of
        D when D < 50000 -> fast;           %% <50ms
        D when D < 100000 -> slow;          %% 50-100ms
        D when D < 500000 -> very_slow;     %% 100-500ms
        _ -> critical                       %% >500ms
    end.

%% @doc Get latency statistics
-spec get_latency_stats() -> map().
get_latency_stats() ->
    case ets:lookup(?LATENCY_TABLE, profile) of
        [{profile, Profile}] ->
            Samples = lists:reverse(Profile#latency_profile.samples),
            Durations = [S#latency_sample.duration_us || S <- Samples],

            CategoryCounts = count_by_category(Samples),

            #{
                total_samples => length(Samples),
                min_latency_us => case Durations of [] -> 0; _ -> lists:min(Durations) end,
                max_latency_us => case Durations of [] -> 0; _ -> lists:max(Durations) end,
                avg_latency_us => avg_list(Durations),
                median_latency_us => median_list(Durations),
                stdev_latency_us => stdev_list(Durations),
                fast_count => maps:get(fast, CategoryCounts, 0),
                slow_count => maps:get(slow, CategoryCounts, 0),
                very_slow_count => maps:get(very_slow, CategoryCounts, 0),
                critical_count => maps:get(critical, CategoryCounts, 0)
            };
        [] ->
            {error, profiling_not_started}
    end.

%% @doc Get latency percentiles
-spec get_percentiles() -> map().
get_percentiles() ->
    case ets:lookup(?LATENCY_TABLE, profile) of
        [{profile, Profile}] ->
            Samples = lists:reverse(Profile#latency_profile.samples),
            Durations = lists:sort([S#latency_sample.duration_us || S <- Samples]),

            case Durations of
                [] ->
                    {error, no_samples};
                _ ->
                    #{
                        p50 => percentile(Durations, 50),
                        p75 => percentile(Durations, 75),
                        p90 => percentile(Durations, 90),
                        p95 => percentile(Durations, 95),
                        p99 => percentile(Durations, 99),
                        p99_9 => percentile(Durations, 99.9),
                        p100 => lists:last(Durations),
                        total_samples => length(Durations)
                    }
            end;
        [] ->
            {error, profiling_not_started}
    end.

%% @doc Get slow operations exceeding threshold (in microseconds)
-spec get_slow_operations(integer()) -> [map()].
get_slow_operations(ThresholdUs) when is_integer(ThresholdUs) ->
    case ets:lookup(?LATENCY_TABLE, profile) of
        [{profile, Profile}] ->
            Samples = lists:reverse(Profile#latency_profile.samples),
            SlowSamples = [S || S <- Samples, S#latency_sample.duration_us > ThresholdUs],

            [#{
                operation => S#latency_sample.operation,
                duration_us => S#latency_sample.duration_us,
                duration_ms => S#latency_sample.duration_us / 1000.0,
                timestamp => S#latency_sample.timestamp,
                category => S#latency_sample.category
            } || S <- lists:sublist(SlowSamples, 100)];
        [] ->
            []
    end.

%% @doc Analyze latency distribution
-spec analyze_latency_distribution() -> map().
analyze_latency_distribution() ->
    case ets:lookup(?LATENCY_TABLE, profile) of
        [{profile, Profile}] ->
            Samples = lists:reverse(Profile#latency_profile.samples),
            Durations = [S#latency_sample.duration_us || S <- Samples],

            case Durations of
                [] ->
                    {error, no_data};
                _ ->
                    CategoryCounts = count_by_category(Samples),
                    Total = length(Samples),

                    #{
                        total_operations => Total,
                        fast_percent => (maps:get(fast, CategoryCounts, 0) / Total) * 100.0,
                        slow_percent => (maps:get(slow, CategoryCounts, 0) / Total) * 100.0,
                        very_slow_percent => (maps:get(very_slow, CategoryCounts, 0) / Total) * 100.0,
                        critical_percent => (maps:get(critical, CategoryCounts, 0) / Total) * 100.0,
                        mean_latency_us => avg_list(Durations),
                        median_latency_us => median_list(Durations),
                        stdev_latency_us => stdev_list(Durations)
                    }
            end;
        [] ->
            {error, profiling_not_started}
    end.

%% @doc Get profiling report
-spec get_profiling_report() -> map().
get_profiling_report() ->
    case ets:lookup(?LATENCY_TABLE, profile) of
        [{profile, Profile}] ->
            Samples = lists:reverse(Profile#latency_profile.samples),
            Durations = lists:sort([S#latency_sample.duration_us || S <- Samples]),
            SlowOps = get_slow_operations(100000),  %% >100ms

            #{
                total_samples => length(Samples),
                elapsed_us => erlang:system_time(microsecond) - Profile#latency_profile.start_time,
                min_latency_us => case Durations of [] -> 0; _ -> lists:min(Durations) end,
                max_latency_us => case Durations of [] -> 0; _ -> lists:max(Durations) end,
                avg_latency_us => avg_list(Durations),
                median_latency_us => median_list(Durations),
                percentiles => maps:get(p95, get_percentiles(), 0),
                slow_operations_count => length(SlowOps),
                operations_tracked => map_size(Profile#latency_profile.operation_counts)
            };
        [] ->
            {error, profiling_not_started}
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

-spec count_by_category([#latency_sample{}]) -> map().
count_by_category(Samples) ->
    lists:foldl(
        fun(Sample, Acc) ->
            Cat = Sample#latency_sample.category,
            Count = maps:get(Cat, Acc, 0),
            Acc#{Cat => Count + 1}
        end,
        #{},
        Samples
    ).

-spec percentile(list(integer()), float()) -> integer().
percentile([], _) ->
    0;
percentile(Values, Percent) when Percent >= 0, Percent =< 100 ->
    Sorted = lists:sort(Values),
    Length = length(Sorted),
    Index = max(1, round((Percent / 100.0) * Length)),
    lists:nth(Index, Sorted).

-spec avg_list([integer()]) -> float().
avg_list([]) ->
    0.0;
avg_list(Values) ->
    lists:sum(Values) / length(Values).

-spec median_list([integer()]) -> float().
median_list([]) ->
    0.0;
median_list(Values) ->
    Sorted = lists:sort(Values),
    Length = length(Sorted),
    case Length rem 2 of
        0 ->
            (lists:nth(Length div 2, Sorted) + lists:nth(Length div 2 + 1, Sorted)) / 2.0;
        1 ->
            lists:nth((Length + 1) div 2, Sorted)
    end.

-spec stdev_list([integer()]) -> float().
stdev_list([]) ->
    0.0;
stdev_list([_]) ->
    0.0;
stdev_list(Values) ->
    Mean = avg_list(Values),
    Variance = lists:sum([math:pow(X - Mean, 2) || X <- Values]) / (length(Values) - 1),
    math:sqrt(Variance).
