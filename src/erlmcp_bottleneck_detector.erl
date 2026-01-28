%%%-------------------------------------------------------------------
%%% @doc
%%% Bottleneck Detector - Automatic Performance Bottleneck Detection
%%%
%%% Monitors CPU, memory, and latency profiles to automatically detect
%%% performance bottlenecks and generate alerts when thresholds are exceeded.
%%%
%%% Detection Thresholds:
%%% - CPU >80% utilization
%%% - Memory pressure >85% heap
%%% - Latency p99 >500ms
%%% - Process count growth >10K/min
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_bottleneck_detector).

-export([
    start_detection/0,
    start_detection/1,
    stop_detection/0,
    check_bottlenecks/0,
    get_alerts/0,
    get_bottleneck_report/0,
    get_recommendations/0,
    reset_alerts/0,
    subscribe_to_alerts/1
]).

-include_lib("kernel/include/logger.hrl").

-define(DETECTOR_TABLE, erlmcp_bottleneck_detection).

%% Import memory snapshot record definition
-record(memory_snapshot, {
    timestamp :: integer(),
    process_count :: integer(),
    total_memory :: integer(),
    process_memory :: integer(),
    binary_memory :: integer(),
    ets_memory :: integer(),
    atom_memory :: integer(),
    gc_collections :: integer(),
    gc_reclaimed :: integer(),
    gc_pause_max :: integer()
}).
-define(CHECK_INTERVAL_MS, 5000).

-record(alert, {
    timestamp :: integer(),
    severity :: critical | warning | info,
    type :: atom(),
    message :: string(),
    value :: number(),
    threshold :: number(),
    recommendation :: string()
}).

-record(detector_state, {
    enabled = false :: boolean(),
    start_time :: integer(),
    alerts = [] :: [#alert{}],
    subscribers = [] :: [pid()],
    cpu_high_count = 0 :: integer(),
    latency_high_count = 0 :: integer(),
    memory_high_count = 0 :: integer()
}).

-define(CPU_THRESHOLD, 80).
-define(LATENCY_P99_THRESHOLD_US, 500000).
-define(MEMORY_PRESSURE_THRESHOLD, 85).
-define(PROCESS_GROWTH_THRESHOLD, 10000).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start bottleneck detection
-spec start_detection() -> ok.
start_detection() ->
    start_detection(#{}).

-spec start_detection(map()) -> ok.
start_detection(_Options) ->
    case ets:whereis(?DETECTOR_TABLE) of
        undefined ->
            ets:new(?DETECTOR_TABLE, [
                named_table,
                public,
                {write_concurrency, true}
            ]);
        _ ->
            ets:delete_all_objects(?DETECTOR_TABLE)
    end,

    State = #detector_state{
        enabled = true,
        start_time = erlang:system_time(microsecond)
    },

    ets:insert(?DETECTOR_TABLE, {state, State}),
    logger:info("Bottleneck detection started"),
    ok.

%% @doc Stop bottleneck detection
-spec stop_detection() -> map().
stop_detection() ->
    case ets:lookup(?DETECTOR_TABLE, state) of
        [{state, State}] ->
            #{
                alerts_generated => length(State#detector_state.alerts),
                cpu_high_incidents => State#detector_state.cpu_high_count,
                latency_high_incidents => State#detector_state.latency_high_count,
                memory_high_incidents => State#detector_state.memory_high_count
            };
        [] ->
            {error, detection_not_started}
    end.

%% @doc Check for bottlenecks and generate alerts
-spec check_bottlenecks() -> [map()].
check_bottlenecks() ->
    case ets:lookup(?DETECTOR_TABLE, state) of
        [{state, State}] ->
            case State#detector_state.enabled of
                true ->
                    Alerts = [],
                    Alerts1 = check_cpu(Alerts, State),
                    Alerts2 = check_latency(Alerts1, State),
                    Alerts3 = check_memory(Alerts2, State),
                    Alerts4 = check_process_growth(Alerts3, State),

                    case Alerts4 of
                        [] -> ok;
                        NewAlerts ->
                            AllAlerts = State#detector_state.alerts ++ NewAlerts,
                            UpdatedState = State#detector_state{alerts = AllAlerts},
                            ets:insert(?DETECTOR_TABLE, {state, UpdatedState}),
                            notify_subscribers(NewAlerts, State#detector_state.subscribers)
                    end,

                    [alert_to_map(A) || A <- Alerts4];
                false ->
                    []
            end;
        [] ->
            []
    end.

%% @doc Get all alerts
-spec get_alerts() -> [map()].
get_alerts() ->
    case ets:lookup(?DETECTOR_TABLE, state) of
        [{state, State}] ->
            [alert_to_map(A) || A <- lists:reverse(State#detector_state.alerts)];
        [] ->
            []
    end.

%% @doc Get bottleneck report
-spec get_bottleneck_report() -> map().
get_bottleneck_report() ->
    case ets:lookup(?DETECTOR_TABLE, state) of
        [{state, State}] ->
            Alerts = State#detector_state.alerts,
            CriticalCount = length([A || A <- Alerts, A#alert.severity =:= critical]),
            WarningCount = length([A || A <- Alerts, A#alert.severity =:= warning]),

            RecentAlerts = lists:sublist(lists:reverse(Alerts), 10),

            #{
                total_alerts => length(Alerts),
                critical_alerts => CriticalCount,
                warning_alerts => WarningCount,
                info_alerts => length(Alerts) - CriticalCount - WarningCount,
                cpu_incidents => State#detector_state.cpu_high_count,
                latency_incidents => State#detector_state.latency_high_count,
                memory_incidents => State#detector_state.memory_high_count,
                recent_alerts => [alert_to_map(A) || A <- RecentAlerts],
                uptime_seconds => (erlang:system_time(microsecond) - State#detector_state.start_time) div 1000000
            };
        [] ->
            {error, detection_not_started}
    end.

%% @doc Get recommendations for resolved bottlenecks
-spec get_recommendations() -> [string()].
get_recommendations() ->
    case ets:lookup(?DETECTOR_TABLE, state) of
        [{state, State}] ->
            Alerts = State#detector_state.alerts,
            Recs = [A#alert.recommendation || A <- Alerts, A#alert.recommendation =/= ""],
            lists:usort(Recs);
        [] ->
            []
    end.

%% @doc Reset all alerts
-spec reset_alerts() -> ok.
reset_alerts() ->
    case ets:lookup(?DETECTOR_TABLE, state) of
        [{state, State}] ->
            UpdatedState = State#detector_state{
                alerts = [],
                cpu_high_count = 0,
                latency_high_count = 0,
                memory_high_count = 0
            },
            ets:insert(?DETECTOR_TABLE, {state, UpdatedState}),
            ok;
        [] ->
            ok
    end.

%% @doc Subscribe to alert notifications
-spec subscribe_to_alerts(pid()) -> ok.
subscribe_to_alerts(Pid) when is_pid(Pid) ->
    case ets:lookup(?DETECTOR_TABLE, state) of
        [{state, State}] ->
            UpdatedState = State#detector_state{
                subscribers = [Pid | State#detector_state.subscribers]
            },
            ets:insert(?DETECTOR_TABLE, {state, UpdatedState}),
            ok;
        [] ->
            ok
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

-spec check_cpu([#alert{}], #detector_state{}) -> [#alert{}].
check_cpu(Alerts, State) ->
    {CpuTime, _} = erlang:statistics(runtime),
    {_, Runtime} = erlang:statistics(wall_clock),

    case Runtime of
        0 -> Alerts;
        _ ->
            CpuPercent = (CpuTime / Runtime) * 100,

            case CpuPercent > ?CPU_THRESHOLD of
                true ->
                    UpdatedState = State#detector_state{
                        cpu_high_count = State#detector_state.cpu_high_count + 1
                    },
                    ets:insert(?DETECTOR_TABLE, {state, UpdatedState}),

                    Alert = #alert{
                        timestamp = erlang:system_time(microsecond),
                        severity = critical,
                        type = cpu_high,
                        message = io_lib:format("CPU utilization >80%: ~.1f%", [CpuPercent]),
                        value = CpuPercent,
                        threshold = ?CPU_THRESHOLD,
                        recommendation = "Consider increasing process pool size or optimizing hot functions"
                    },
                    Alerts ++ [Alert];
                false ->
                    Alerts
            end
    end.

-spec check_latency([#alert{}], #detector_state{}) -> [#alert{}].
check_latency(Alerts, State) ->
    try erlmcp_latency_profiler:get_percentiles() of
        {error, _} -> Alerts;
        Percentiles ->
            P99 = maps:get(p99, Percentiles, 0),

            case P99 > ?LATENCY_P99_THRESHOLD_US of
                true ->
                    UpdatedState = State#detector_state{
                        latency_high_count = State#detector_state.latency_high_count + 1
                    },
                    ets:insert(?DETECTOR_TABLE, {state, UpdatedState}),

                    Alert = #alert{
                        timestamp = erlang:system_time(microsecond),
                        severity = warning,
                        type = latency_high,
                        message = io_lib:format("p99 latency >500ms: ~.0fus", [P99]),
                        value = P99,
                        threshold = ?LATENCY_P99_THRESHOLD_US,
                        recommendation = "Identify slow functions using CPU profiler, optimize JSON parsing or message routing"
                    },
                    Alerts ++ [Alert];
                false ->
                    Alerts
            end
    catch
        _:_ -> Alerts
    end.

-spec check_memory([#alert{}], #detector_state{}) -> [#alert{}].
check_memory(Alerts, State) ->
    try erlmcp_memory_profiler:measure_memory_snapshot() of
        #memory_snapshot{total_memory = Total} ->
            MaxMemory = erlang:memory(maximum),
            MemPercent = (Total / MaxMemory) * 100,

            case MemPercent > ?MEMORY_PRESSURE_THRESHOLD of
                true ->
                    UpdatedState = State#detector_state{
                        memory_high_count = State#detector_state.memory_high_count + 1
                    },
                    ets:insert(?DETECTOR_TABLE, {state, UpdatedState}),

                    Alert = #alert{
                        timestamp = erlang:system_time(microsecond),
                        severity = warning,
                        type = memory_high,
                        message = io_lib:format("Memory pressure >85%: ~.1f%", [MemPercent]),
                        value = MemPercent,
                        threshold = ?MEMORY_PRESSURE_THRESHOLD,
                        recommendation = "Check for memory leaks using memory profiler, reduce connection keep-alive timeouts"
                    },
                    Alerts ++ [Alert];
                false ->
                    Alerts
            end;
        _ -> Alerts
    catch
        _:_ -> Alerts
    end.

-spec check_process_growth([#alert{}], #detector_state{}) -> [#alert{}].
check_process_growth(Alerts, _State) ->
    ProcCount = erlang:system_info(process_count),

    case ets:lookup(?DETECTOR_TABLE, prev_process_count) of
        [] ->
            ets:insert(?DETECTOR_TABLE, {prev_process_count, ProcCount}),
            Alerts;
        [{prev_process_count, PrevCount}] ->
            Growth = ProcCount - PrevCount,
            TimeDeltaS = 5,  % Check interval in seconds

            GrowthPerMin = (Growth / TimeDeltaS) * 60,

            case GrowthPerMin > ?PROCESS_GROWTH_THRESHOLD of
                true ->
                    Alert = #alert{
                        timestamp = erlang:system_time(microsecond),
                        severity = warning,
                        type = process_growth_high,
                        message = io_lib:format("Process creation rate >10K/min: ~.0f/min", [GrowthPerMin]),
                        value = GrowthPerMin,
                        threshold = ?PROCESS_GROWTH_THRESHOLD,
                        recommendation = "Check for process leaks, verify connection cleanup on disconnect"
                    },
                    ets:insert(?DETECTOR_TABLE, {prev_process_count, ProcCount}),
                    Alerts ++ [Alert];
                false ->
                    ets:insert(?DETECTOR_TABLE, {prev_process_count, ProcCount}),
                    Alerts
            end
    end.

-spec alert_to_map(#alert{}) -> map().
alert_to_map(Alert) ->
    #{
        timestamp => Alert#alert.timestamp,
        severity => Alert#alert.severity,
        type => Alert#alert.type,
        message => Alert#alert.message,
        value => Alert#alert.value,
        threshold => Alert#alert.threshold,
        recommendation => Alert#alert.recommendation
    }.

-spec notify_subscribers([#alert{}], [pid()]) -> ok.
notify_subscribers(Alerts, Subscribers) ->
    lists:foreach(
        fun(Pid) ->
            catch (Pid ! {bottleneck_alerts, [alert_to_map(A) || A <- Alerts]})
        end,
        Subscribers
    ),
    ok.
