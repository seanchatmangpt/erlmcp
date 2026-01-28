%%%-------------------------------------------------------------------
%%% @doc
%%% CPU Profiler for Scale Testing - Identify Hot Functions at 100K Scale
%%%
%%% Tracks CPU usage by function, identifies hotspots consuming the most
%%% CPU cycles, and provides detailed analysis of function call patterns.
%%%
%%% Key Measurements:
%%% - Function call counts
%%% - Cumulative execution time per function
%%% - Average time per call
%%% - CPU time percentage
%%% - Top N hottest functions
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cpu_profiler).

-export([
    start_profiling/0,
    start_profiling/1,
    stop_profiling/0,
    get_cpu_stats/0,
    get_top_functions/1,
    analyze_cpu_usage/0,
    measure_function_call/1,
    measure_function_call/2,
    get_profiling_report/0,
    get_cpu_overhead/0
]).

-include_lib("kernel/include/logger.hrl").

-define(CPU_TABLE, erlmcp_cpu_profiling).
-define(DEFAULT_TOP_N, 10).

-record(function_stats, {
    mfa :: mfa(),
    call_count = 0 :: integer(),
    total_time_us = 0 :: integer(),
    min_time_us = 999999999 :: integer(),
    max_time_us = 0 :: integer(),
    avg_time_us = 0.0 :: float(),
    cpu_percentage = 0.0 :: float()
}).

-record(cpu_profile, {
    start_time :: integer(),
    stop_time :: integer() | undefined,
    start_cpu :: integer(),
    stop_cpu :: integer() | undefined,
    function_stats = #{} :: map(),
    total_time_us = 0 :: integer(),
    profiler_overhead_us = 0 :: integer(),
    enabled = false :: boolean()
}).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start CPU profiling with default settings
-spec start_profiling() -> ok.
start_profiling() ->
    start_profiling(#{}).

%% @doc Start CPU profiling with options
-spec start_profiling(map()) -> ok.
start_profiling(_Options) ->
    case ets:whereis(?CPU_TABLE) of
        undefined ->
            ets:new(?CPU_TABLE, [
                named_table,
                public,
                {write_concurrency, true},
                {read_concurrency, true}
            ]);
        _ ->
            ets:delete_all_objects(?CPU_TABLE)
    end,

    {CpuTime, _} = erlang:statistics(runtime),
    Profile = #cpu_profile{
        start_time = erlang:system_time(microsecond),
        start_cpu = CpuTime,
        function_stats = #{},
        enabled = true
    },

    ets:insert(?CPU_TABLE, {profile, Profile}),
    logger:info("CPU profiling started"),
    ok.

%% @doc Stop CPU profiling and return statistics
-spec stop_profiling() -> map().
stop_profiling() ->
    case ets:lookup(?CPU_TABLE, profile) of
        [{profile, Profile}] ->
            {CpuTime, _} = erlang:statistics(runtime),
            ElapsedUs = erlang:system_time(microsecond) - Profile#cpu_profile.start_time,
            CpuDeltaMs = CpuTime - Profile#cpu_profile.start_cpu,

            UpdatedProfile = Profile#cpu_profile{
                stop_time = erlang:system_time(microsecond),
                stop_cpu = CpuTime,
                total_time_us = ElapsedUs,
                profiler_overhead_us = calculate_overhead(Profile)
            },

            ets:insert(?CPU_TABLE, {profile, UpdatedProfile}),

            #{
                elapsed_us => ElapsedUs,
                cpu_time_ms => CpuDeltaMs,
                cpu_time_seconds => CpuDeltaMs / 1000.0,
                functions_tracked => map_size(Profile#cpu_profile.function_stats),
                profiler_overhead_us => UpdatedProfile#cpu_profile.profiler_overhead_us,
                profiler_overhead_percentage =>
                    case CpuDeltaMs of
                        0 -> 0.0;
                        _ -> (UpdatedProfile#cpu_profile.profiler_overhead_us / 1000.0) / CpuDeltaMs * 100.0
                    end
            };
        [] ->
            {error, profiling_not_started}
    end.

%% @doc Get current CPU statistics
-spec get_cpu_stats() -> map() | {error, term()}.
get_cpu_stats() ->
    case ets:lookup(?CPU_TABLE, profile) of
        [{profile, Profile}] ->
            Stats = Profile#cpu_profile.function_stats,
            {CpuTime, _} = erlang:statistics(runtime),
            ElapsedUs = erlang:system_time(microsecond) - Profile#cpu_profile.start_time,

            #{
                elapsed_us => ElapsedUs,
                cpu_time_ms => CpuTime - Profile#cpu_profile.start_cpu,
                total_functions => map_size(Stats),
                total_calls => sum_calls(Stats),
                total_time_us => sum_times(Stats)
            };
        [] ->
            {error, profiling_not_started}
    end.

%% @doc Get top N hottest functions
-spec get_top_functions(pos_integer()) -> [map()].
get_top_functions(N) when N > 0 ->
    case ets:lookup(?CPU_TABLE, profile) of
        [{profile, Profile}] ->
            Stats = Profile#cpu_profile.function_stats,
            TotalTime = sum_times(Stats),

            SortedStats = lists:sort(
                fun(#{total_time_us := T1}, #{total_time_us := T2}) ->
                    T1 > T2
                end,
                [function_stats_to_map(FStats, TotalTime) || FStats <- maps:values(Stats)]
            ),

            lists:sublist(SortedStats, N);
        [] ->
            []
    end.

%% @doc Analyze CPU usage patterns
-spec analyze_cpu_usage() -> map().
analyze_cpu_usage() ->
    case ets:lookup(?CPU_TABLE, profile) of
        [{profile, Profile}] ->
            Stats = Profile#cpu_profile.function_stats,
            TotalTime = sum_times(Stats),
            TotalCalls = sum_calls(Stats),

            case TotalTime of
                0 -> {error, no_data};
                _ ->
                    TopFunctions = get_top_functions(5),
                    #{
                        total_time_us => TotalTime,
                        total_calls => TotalCalls,
                        unique_functions => map_size(Stats),
                        avg_call_time_us => TotalTime / max(TotalCalls, 1),
                        top_5_functions => TopFunctions,
                        profile_age_us => erlang:system_time(microsecond) - Profile#cpu_profile.start_time
                    }
            end;
        [] ->
            {error, profiling_not_started}
    end.

%% @doc Measure a single function call and record statistics
-spec measure_function_call(mfa()) -> ok.
measure_function_call(MFA) ->
    measure_function_call(MFA, 0).

-spec measure_function_call(mfa(), integer()) -> ok.
measure_function_call(MFA, ExecutionTimeUs) when ExecutionTimeUs >= 0 ->
    case ets:lookup(?CPU_TABLE, profile) of
        [{profile, Profile}] ->
            case Profile#cpu_profile.enabled of
                true ->
                    Stats = Profile#cpu_profile.function_stats,
                    CurrentStats = maps:get(MFA, Stats, #function_stats{mfa = MFA}),

                    UpdatedStats = CurrentStats#function_stats{
                        call_count = CurrentStats#function_stats.call_count + 1,
                        total_time_us = CurrentStats#function_stats.total_time_us + ExecutionTimeUs,
                        min_time_us = min(CurrentStats#function_stats.min_time_us, ExecutionTimeUs),
                        max_time_us = max(CurrentStats#function_stats.max_time_us, ExecutionTimeUs)
                    },

                    NewStats = Stats#{MFA => UpdatedStats},
                    UpdatedProfile = Profile#cpu_profile{function_stats = NewStats},
                    ets:insert(?CPU_TABLE, {profile, UpdatedProfile});
                false ->
                    ok
            end;
        [] ->
            ok
    end.

%% @doc Get profiling report
-spec get_profiling_report() -> map().
get_profiling_report() ->
    case ets:lookup(?CPU_TABLE, profile) of
        [{profile, Profile}] ->
            TopFunctions = get_top_functions(?DEFAULT_TOP_N),
            Stats = Profile#cpu_profile.function_stats,
            TotalTime = sum_times(Stats),

            #{
                status => case Profile#cpu_profile.stop_time of
                    undefined -> active;
                    _ -> stopped
                end,
                elapsed_us => erlang:system_time(microsecond) - Profile#cpu_profile.start_time,
                total_functions => map_size(Stats),
                total_calls => sum_calls(Stats),
                total_time_us => TotalTime,
                profiler_overhead_us => Profile#cpu_profile.profiler_overhead_us,
                top_10_functions => TopFunctions
            };
        [] ->
            {error, profiling_not_started}
    end.

%% @doc Get profiler overhead as percentage
-spec get_cpu_overhead() -> float().
get_cpu_overhead() ->
    case ets:lookup(?CPU_TABLE, profile) of
        [{profile, Profile}] ->
            {CpuTime, _} = erlang:statistics(runtime),
            CpuDeltaMs = (CpuTime - Profile#cpu_profile.start_cpu),
            OverheadUs = Profile#cpu_profile.profiler_overhead_us,
            OverheadMs = OverheadUs / 1000.0,

            case CpuDeltaMs of
                0 -> 0.0;
                _ -> (OverheadMs / CpuDeltaMs) * 100.0
            end;
        [] ->
            0.0
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

-spec calculate_overhead(#cpu_profile{}) -> integer().
calculate_overhead(Profile) ->
    %% Estimate: each measure_function_call takes ~10 microseconds overhead
    CallCount = sum_calls(Profile#cpu_profile.function_stats),
    CallCount * 10.

-spec sum_calls(map()) -> integer().
sum_calls(Stats) ->
    maps:fold(
        fun(_, FStats, Acc) ->
            Acc + FStats#function_stats.call_count
        end,
        0,
        Stats
    ).

-spec sum_times(map()) -> integer().
sum_times(Stats) ->
    maps:fold(
        fun(_, FStats, Acc) ->
            Acc + FStats#function_stats.total_time_us
        end,
        0,
        Stats
    ).

-spec function_stats_to_map(#function_stats{}, integer()) -> map().
function_stats_to_map(FStats, TotalTime) ->
    CallCount = FStats#function_stats.call_count,
    TotalTime_ = FStats#function_stats.total_time_us,
    AvgTime = case CallCount of
        0 -> 0.0;
        _ -> TotalTime_ / CallCount
    end,
    CpuPercent = case TotalTime of
        0 -> 0.0;
        _ -> (TotalTime_ / TotalTime) * 100.0
    end,

    #{
        mfa => FStats#function_stats.mfa,
        call_count => CallCount,
        total_time_us => TotalTime_,
        avg_time_us => AvgTime,
        min_time_us => FStats#function_stats.min_time_us,
        max_time_us => FStats#function_stats.max_time_us,
        cpu_percentage => CpuPercent
    }.
