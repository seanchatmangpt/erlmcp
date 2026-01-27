%%%-------------------------------------------------------------------
%%% @doc TCPS Metrics Aggregator - Real-time metrics collection and aggregation
%%%
%%% Aggregates metrics from all TCPS subsystems for dashboard display.
%%% Provides comprehensive real-time monitoring data.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(tcps_metrics_aggregator).
-behaviour(gen_server).

%% API
-export([start_link/0, stop/0]).
-export([get_overview_metrics/0, get_quality_metrics/0, get_kanban_metrics/0]).
-export([get_andon_metrics/0, get_kaizen_metrics/0, get_flow_metrics/0]).
-export([get_all_metrics/0, force_refresh/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

-define(SERVER, ?MODULE).
-define(REFRESH_INTERVAL, 5000). %% 5 seconds
-define(CACHE_TTL, 60). %% 60 seconds

-record(state, {
    last_refresh :: erlang:timestamp() | undefined,
    metrics_cache :: map(),
    refresh_timer :: reference() | undefined
}).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec stop() -> ok.
stop() ->
    gen_server:stop(?SERVER).

%% @doc Get overview metrics (dashboard summary)
-spec get_overview_metrics() -> map().
get_overview_metrics() ->
    gen_server:call(?SERVER, get_overview_metrics).

%% @doc Get quality gate metrics
-spec get_quality_metrics() -> map().
get_quality_metrics() ->
    gen_server:call(?SERVER, get_quality_metrics).

%% @doc Get Kanban board metrics
-spec get_kanban_metrics() -> map().
get_kanban_metrics() ->
    gen_server:call(?SERVER, get_kanban_metrics).

%% @doc Get Andon alert metrics
-spec get_andon_metrics() -> map().
get_andon_metrics() ->
    gen_server:call(?SERVER, get_andon_metrics).

%% @doc Get Kaizen improvement metrics
-spec get_kaizen_metrics() -> map().
get_kaizen_metrics() ->
    gen_server:call(?SERVER, get_kaizen_metrics).

%% @doc Get production flow metrics
-spec get_flow_metrics() -> map().
get_flow_metrics() ->
    gen_server:call(?SERVER, get_flow_metrics).

%% @doc Get all metrics at once
-spec get_all_metrics() -> map().
get_all_metrics() ->
    gen_server:call(?SERVER, get_all_metrics).

%% @doc Force immediate metrics refresh
-spec force_refresh() -> ok.
force_refresh() ->
    gen_server:cast(?SERVER, force_refresh).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    ?LOG_INFO("Starting TCPS Metrics Aggregator"),

    %% Initialize cache
    tcps_metrics_cache:init_cache(),

    %% Start periodic refresh
    TimerRef = erlang:send_after(?REFRESH_INTERVAL, self(), refresh_metrics),

    %% Do initial collection
    InitialMetrics = collect_all_metrics(),

    State = #state{
        last_refresh = erlang:timestamp(),
        metrics_cache = InitialMetrics,
        refresh_timer = TimerRef
    },

    {ok, State}.

handle_call(get_overview_metrics, _From, State) ->
    Metrics = get_cached_or_collect(overview, State),
    {reply, Metrics, State};

handle_call(get_quality_metrics, _From, State) ->
    Metrics = get_cached_or_collect(quality, State),
    {reply, Metrics, State};

handle_call(get_kanban_metrics, _From, State) ->
    Metrics = get_cached_or_collect(kanban, State),
    {reply, Metrics, State};

handle_call(get_andon_metrics, _From, State) ->
    Metrics = get_cached_or_collect(andon, State),
    {reply, Metrics, State};

handle_call(get_kaizen_metrics, _From, State) ->
    Metrics = get_cached_or_collect(kaizen, State),
    {reply, Metrics, State};

handle_call(get_flow_metrics, _From, State) ->
    Metrics = get_cached_or_collect(flow, State),
    {reply, Metrics, State};

handle_call(get_all_metrics, _From, State) ->
    {reply, State#state.metrics_cache, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(force_refresh, State) ->
    NewMetrics = collect_all_metrics(),
    NewState = State#state{
        metrics_cache = NewMetrics,
        last_refresh = erlang:timestamp()
    },

    %% Broadcast update to SSE clients
    broadcast_metrics_update(NewMetrics),

    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(refresh_metrics, State) ->
    %% Collect fresh metrics
    NewMetrics = collect_all_metrics(),

    %% Update cache
    NewState = State#state{
        metrics_cache = NewMetrics,
        last_refresh = erlang:timestamp()
    },

    %% Broadcast to SSE clients
    broadcast_metrics_update(NewMetrics),

    %% Schedule next refresh
    NewTimerRef = erlang:send_after(?REFRESH_INTERVAL, self(), refresh_metrics),

    {noreply, NewState#state{refresh_timer = NewTimerRef}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    case State#state.refresh_timer of
        undefined -> ok;
        TimerRef -> erlang:cancel_timer(TimerRef)
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions - Metrics Collection
%%%===================================================================

%% @private Collect all metrics from all subsystems
collect_all_metrics() ->
    #{
        overview => collect_overview_metrics(),
        quality => collect_quality_metrics(),
        kanban => collect_kanban_metrics(),
        andon => collect_andon_metrics(),
        kaizen => collect_kaizen_metrics(),
        flow => collect_flow_metrics(),
        timestamp => erlang:timestamp()
    }.

%% @private Collect overview metrics
collect_overview_metrics() ->
    %% Get work order counts
    {TotalWO, ActiveWO} = count_work_orders(),

    %% Get open Andons count
    OpenAndons = count_open_andons(),

    %% Calculate quality pass rate
    PassRate = calculate_quality_pass_rate(),

    %% Get average lead time
    AvgLeadTime = calculate_average_lead_time(),

    %% Calculate throughput
    Throughput = calculate_throughput_per_hour(),

    #{
        total_work_orders => TotalWO,
        active_work_orders => ActiveWO,
        open_andons => OpenAndons,
        quality_gate_pass_rate => PassRate,
        avg_lead_time_hours => AvgLeadTime,
        throughput_per_hour => Throughput,
        system_health => calculate_system_health(),
        timestamp => erlang:system_time(millisecond)
    }.

%% @private Collect quality gate metrics
collect_quality_metrics() ->
    %% Mock implementation - integrate with actual quality system
    #{
        test_pass_rate => #{
            value => calculate_test_pass_rate(),
            target => 0.80,
            status => if_pass(calculate_test_pass_rate(), 0.80)
        },
        code_coverage => #{
            value => 0.85,
            target => 0.80,
            status => pass
        },
        defect_rate => #{
            value => 0.005,
            target => 0.01,
            status => pass
        },
        first_pass_yield => #{
            value => 0.96,
            target => 0.95,
            status => pass
        },
        security_scan_pass => #{
            value => 0.98,
            target => 0.95,
            status => pass
        },
        timestamp => erlang:system_time(millisecond)
    }.

%% @private Collect Kanban board metrics
collect_kanban_metrics() ->
    case whereis(tcps_kanban) of
        undefined ->
            #{error => kanban_not_running};
        _Pid ->
            Buckets = [reliability, security, cost, compliance],
            BucketMetrics = lists:foldl(fun(Bucket, Acc) ->
                WipStatus = safe_call(tcps_kanban, get_wip_status, [Bucket]),
                Acc#{Bucket => WipStatus}
            end, #{}, Buckets),

            BucketMetrics#{
                total_wip => sum_current_wip(BucketMetrics),
                timestamp => erlang:system_time(millisecond)
            }
    end.

%% @private Collect Andon alert metrics
collect_andon_metrics() ->
    %% Get all open Andon events from ETS
    AllEvents = safe_ets_tab2list(tcps_andon_events),

    OpenEvents = lists:filter(fun({_Id, Event}) ->
        is_map(Event) andalso maps:get(status, Event, undefined) =:= open
    end, AllEvents),

    %% Group by severity
    BySeverity = lists:foldl(fun({_Id, Event}, Acc) ->
        Severity = infer_severity(Event),
        maps:update_with(Severity, fun(Count) -> Count + 1 end, 1, Acc)
    end, #{critical => 0, warning => 0, info => 0}, OpenEvents),

    %% Format for display
    AndonList = lists:map(fun({Id, Event}) ->
        format_andon_event(Id, Event)
    end, lists:sublist(OpenEvents, 10)),  %% Show top 10

    #{
        total_open => length(OpenEvents),
        by_severity => BySeverity,
        recent_events => AndonList,
        timestamp => erlang:system_time(millisecond)
    }.

%% @private Collect Kaizen improvement metrics
collect_kaizen_metrics() ->
    %% Calculate week-over-week improvements
    WoWImprovements = #{
        lead_time_reduction => calculate_wow_improvement(lead_time),
        defect_reduction => calculate_wow_improvement(defects),
        throughput_increase => calculate_wow_improvement(throughput)
    },

    %% Identify top waste points
    TopWaste = [
        #{category => waiting, percentage => 0.35},
        #{category => rework, percentage => 0.25},
        #{category => overprocessing, percentage => 0.20},
        #{category => defects, percentage => 0.12},
        #{category => motion, percentage => 0.08}
    ],

    #{
        week_over_week => WoWImprovements,
        top_waste_points => TopWaste,
        improvement_proposals => get_improvement_proposals(),
        trend_data => generate_kaizen_trends(),
        timestamp => erlang:system_time(millisecond)
    }.

%% @private Collect production flow metrics
collect_flow_metrics() ->
    %% Get active work orders from all buckets
    ActiveWOs = get_active_work_orders(),

    %% Calculate throughput rate
    ThroughputRate = #{
        skus_per_day => calculate_daily_throughput(),
        trend => calculate_throughput_trend()
    },

    %% Get cycle time distribution
    CycleTimeDist = calculate_cycle_time_distribution(),

    #{
        active_work_orders => lists:sublist(ActiveWOs, 10),
        throughput_rate => ThroughputRate,
        cycle_time_distribution => CycleTimeDist,
        bottlenecks => identify_bottlenecks(),
        timestamp => erlang:system_time(millisecond)
    }.

%%%===================================================================
%%% Internal functions - Metric Calculations
%%%===================================================================

%% @private Count work orders
count_work_orders() ->
    AllWOs = safe_ets_tab2list(tcps_work_orders),
    Total = length(AllWOs),
    Active = length([WO || {_Id, WO} <- AllWOs,
                          is_map(WO),
                          maps:get(status, WO, undefined) =:= in_progress]),
    {Total, Active}.

%% @private Count open Andons
count_open_andons() ->
    AllEvents = safe_ets_tab2list(tcps_andon_events),
    length([E || {_Id, E} <- AllEvents,
                 is_map(E),
                 maps:get(status, E, undefined) =:= open]).

%% @private Calculate quality pass rate
calculate_quality_pass_rate() ->
    %% Mock - integrate with actual quality system
    0.92.

%% @private Calculate test pass rate
calculate_test_pass_rate() ->
    %% Mock - would integrate with test results
    0.94.

%% @private Calculate average lead time
calculate_average_lead_time() ->
    AllWOs = safe_ets_tab2list(tcps_work_orders),
    CompletedWOs = [{Id, WO} || {Id, WO} <- AllWOs,
                                is_map(WO),
                                maps:get(status, WO, undefined) =:= completed],

    case CompletedWOs of
        [] -> 0.0;
        _ ->
            LeadTimes = lists:map(fun({_Id, WO}) ->
                calculate_wo_lead_time(WO)
            end, CompletedWOs),

            lists:sum(LeadTimes) / length(LeadTimes)
    end.

%% @private Calculate work order lead time in hours
calculate_wo_lead_time(WorkOrder) ->
    case {maps:find(created_at, WorkOrder), maps:find(completed_at, WorkOrder)} of
        {{ok, CreatedAt}, {ok, CompletedAt}} ->
            DiffSeconds = timestamp_diff_seconds(CompletedAt, CreatedAt),
            DiffSeconds / 3600;
        _ ->
            0.0
    end.

%% @private Calculate throughput per hour
calculate_throughput_per_hour() ->
    %% Get completed work orders in last 24 hours
    Now = erlang:timestamp(),
    TwentyFourHoursAgo = subtract_hours(Now, 24),

    AllWOs = safe_ets_tab2list(tcps_work_orders),
    RecentCompleted = length([WO || {_Id, WO} <- AllWOs,
                                    is_map(WO),
                                    maps:get(status, WO, undefined) =:= completed,
                                    is_recent(maps:get(completed_at, WO, undefined), TwentyFourHoursAgo)]),

    RecentCompleted / 24.

%% @private Calculate daily throughput
calculate_daily_throughput() ->
    calculate_throughput_per_hour() * 24.

%% @private Calculate throughput trend
calculate_throughput_trend() ->
    %% Compare current week to previous week
    CurrentWeek = calculate_weekly_throughput(0),
    PreviousWeek = calculate_weekly_throughput(7),

    if
        PreviousWeek == 0 -> stable;
        CurrentWeek > PreviousWeek -> increasing;
        CurrentWeek < PreviousWeek -> decreasing;
        true -> stable
    end.

%% @private Calculate weekly throughput
calculate_weekly_throughput(DaysAgo) ->
    Now = erlang:timestamp(),
    StartTime = subtract_hours(Now, (DaysAgo + 7) * 24),
    EndTime = subtract_hours(Now, DaysAgo * 24),

    AllWOs = safe_ets_tab2list(tcps_work_orders),
    length([WO || {_Id, WO} <- AllWOs,
                  is_map(WO),
                  maps:get(status, WO, undefined) =:= completed,
                  is_in_range(maps:get(completed_at, WO, undefined), StartTime, EndTime)]).

%% @private Calculate cycle time distribution
calculate_cycle_time_distribution() ->
    AllWOs = safe_ets_tab2list(tcps_work_orders),
    CompletedWOs = [{Id, WO} || {Id, WO} <- AllWOs,
                                is_map(WO),
                                maps:get(status, WO, undefined) =:= completed],

    %% Bucket by cycle time ranges
    lists:foldl(fun({_Id, WO}, Acc) ->
        LeadTime = calculate_wo_lead_time(WO),
        Bucket = classify_cycle_time(LeadTime),
        maps:update_with(Bucket, fun(Count) -> Count + 1 end, 1, Acc)
    end, #{
        <<"0-24h">> => 0,
        <<"24-48h">> => 0,
        <<"48-72h">> => 0,
        <<"72h+">> => 0
    }, CompletedWOs).

%% @private Classify cycle time into bucket
classify_cycle_time(Hours) when Hours =< 24 -> <<"0-24h">>;
classify_cycle_time(Hours) when Hours =< 48 -> <<"24-48h">>;
classify_cycle_time(Hours) when Hours =< 72 -> <<"48-72h">>;
classify_cycle_time(_) -> <<"72h+">>.

%% @private Calculate system health score
calculate_system_health() ->
    %% Health = weighted average of key metrics
    QualityWeight = 0.3,
    AndonWeight = 0.3,
    ThroughputWeight = 0.2,
    LeadTimeWeight = 0.2,

    QualityScore = calculate_quality_pass_rate(),
    AndonScore = 1.0 - min(1.0, count_open_andons() / 10.0),  %% Penalty for open Andons
    ThroughputScore = min(1.0, calculate_throughput_per_hour() / 5.0),  %% Target: 5/hr
    LeadTimeScore = max(0.0, 1.0 - (calculate_average_lead_time() / 100.0)),  %% Penalty for high lead time

    Overall = QualityWeight * QualityScore +
              AndonWeight * AndonScore +
              ThroughputWeight * ThroughputScore +
              LeadTimeWeight * LeadTimeScore,

    #{
        overall => Overall,
        quality => QualityScore,
        andon => AndonScore,
        throughput => ThroughputScore,
        lead_time => LeadTimeScore
    }.

%% @private Get active work orders
get_active_work_orders() ->
    AllWOs = safe_ets_tab2list(tcps_work_orders),
    Active = [{Id, WO} || {Id, WO} <- AllWOs,
                          is_map(WO),
                          maps:get(status, WO, undefined) =:= in_progress],

    lists:map(fun({Id, WO}) ->
        #{
            id => Id,
            bucket => maps:get(bucket, WO, unknown),
            priority => maps:get(priority, WO, 0),
            current_stage => maps:get(current_stage, WO, unknown),
            elapsed_hours => calculate_elapsed_hours(WO)
        }
    end, Active).

%% @private Calculate elapsed hours for work order
calculate_elapsed_hours(WorkOrder) ->
    case maps:find(started_at, WorkOrder) of
        {ok, StartedAt} ->
            Now = erlang:timestamp(),
            DiffSeconds = timestamp_diff_seconds(Now, StartedAt),
            DiffSeconds / 3600;
        error ->
            0.0
    end.

%% @private Identify bottlenecks
identify_bottlenecks() ->
    %% Get WIP status for all buckets
    case whereis(tcps_kanban) of
        undefined -> [];
        _Pid ->
            Buckets = [reliability, security, cost, compliance],
            Bottlenecks = lists:filtermap(fun(Bucket) ->
                WipStatus = safe_call(tcps_kanban, get_wip_status, [Bucket]),
                case WipStatus of
                    #{utilization := Util} when Util > 0.8 ->
                        {true, #{bucket => Bucket, utilization => Util}};
                    _ ->
                        false
                end
            end, Buckets),
            Bottlenecks
    end.

%% @private Calculate week-over-week improvement
calculate_wow_improvement(Metric) ->
    %% Mock - would track historical data
    case Metric of
        lead_time -> -0.12;  %% 12% reduction
        defects -> -0.08;    %% 8% reduction
        throughput -> 0.15   %% 15% increase
    end.

%% @private Get improvement proposals
get_improvement_proposals() ->
    %% Mock - would integrate with Kaizen system
    [
        #{
            id => <<"kaizen-001">>,
            title => <<"Automate quality checks">>,
            estimated_roi => 0.30,
            status => in_progress,
            priority => high
        },
        #{
            id => <<"kaizen-002">>,
            title => <<"Reduce WIP limits to improve flow">>,
            estimated_roi => 0.25,
            status => proposed,
            priority => medium
        }
    ].

%% @private Generate Kaizen trend data
generate_kaizen_trends() ->
    %% Mock - would track historical metrics
    Metrics = [lead_time, defect_rate, throughput],
    lists:map(fun(Metric) ->
        #{
            metric => Metric,
            data => generate_trend_points(Metric, 12)
        }
    end, Metrics).

%% @private Generate trend points for metric
generate_trend_points(Metric, Weeks) ->
    lists:map(fun(Week) ->
        #{
            week => Week,
            value => calculate_historical_value(Metric, Week)
        }
    end, lists:seq(1, Weeks)).

%% @private Calculate historical value (mock)
calculate_historical_value(lead_time, Week) ->
    50.0 - (Week * 2.0);  %% Improving over time
calculate_historical_value(defect_rate, Week) ->
    0.10 - (Week * 0.005);  %% Reducing over time
calculate_historical_value(throughput, Week) ->
    3.0 + (Week * 0.2).  %% Increasing over time

%%%===================================================================
%%% Helper functions
%%%===================================================================

%% @private Get cached metrics or collect fresh
get_cached_or_collect(MetricType, State) ->
    case tcps_metrics_cache:get_cached(MetricType) of
        {ok, Metrics} ->
            Metrics;
        _ ->
            %% Cache miss, return from state cache
            AllMetrics = State#state.metrics_cache,
            maps:get(MetricType, AllMetrics, #{})
    end.

%% @private Broadcast metrics update to SSE clients
broadcast_metrics_update(Metrics) ->
    case whereis(tcps_sse_manager) of
        undefined -> ok;
        _Pid ->
            tcps_sse_manager:broadcast_update(#{
                type => metrics_update,
                data => Metrics,
                timestamp => erlang:timestamp()
            })
    end.

%% @private Safe ETS table to list
safe_ets_tab2list(Table) ->
    case ets:info(Table) of
        undefined -> [];
        _ -> ets:tab2list(Table)
    end.

%% @private Safe gen_server call
safe_call(Module, Function, Args) ->
    try
        erlang:apply(Module, Function, Args)
    catch
        _:_ -> #{error => call_failed}
    end.

%% @private Sum current WIP across buckets
sum_current_wip(BucketMetrics) ->
    maps:fold(fun(_Bucket, #{current := Current}, Acc) ->
        Acc + Current;
    (_, _, Acc) ->
        Acc
    end, 0, BucketMetrics).

%% @private Infer severity from Andon event
infer_severity(Event) ->
    FailureType = maps:get(failure_type, Event, unknown),
    case FailureType of
        shacl_violation -> critical;
        test_failure -> warning;
        compilation_failure -> critical;
        _ -> info
    end.

%% @private Format Andon event for display
format_andon_event(Id, Event) ->
    #{
        id => Id,
        failure_type => maps:get(failure_type, Event, unknown),
        sku_id => maps:get(sku_id, Event, <<"unknown">>),
        stage => maps:get(stage, Event, unknown),
        severity => infer_severity(Event),
        elapsed_seconds => calculate_andon_elapsed(Event),
        timestamp => maps:get(timestamp, Event, 0)
    }.

%% @private Calculate Andon elapsed time
calculate_andon_elapsed(Event) ->
    case maps:find(timestamp, Event) of
        {ok, Timestamp} when is_integer(Timestamp) ->
            Now = erlang:system_time(millisecond),
            (Now - Timestamp) div 1000;
        _ ->
            0
    end.

%% @private Check if pass based on threshold
if_pass(Value, Target) ->
    if Value >= Target -> pass; true -> fail end.

%% @private Timestamp difference in seconds
timestamp_diff_seconds({Mega1, Secs1, Micro1}, {Mega2, Secs2, Micro2}) ->
    TotalMicro1 = Mega1 * 1000000000000 + Secs1 * 1000000 + Micro1,
    TotalMicro2 = Mega2 * 1000000000000 + Secs2 * 1000000 + Micro2,
    abs(TotalMicro1 - TotalMicro2) / 1000000.

%% @private Subtract hours from timestamp
subtract_hours({MegaSecs, Secs, MicroSecs}, Hours) ->
    SecondsDiff = round(Hours * 3600),
    NewSecs = Secs - SecondsDiff,

    case NewSecs < 0 of
        true ->
            {MegaSecs - 1, NewSecs + 1000000, MicroSecs};
        false ->
            {MegaSecs, NewSecs, MicroSecs}
    end.

%% @private Check if timestamp is recent
is_recent(undefined, _Threshold) -> false;
is_recent(Timestamp, Threshold) ->
    compare_timestamps(Timestamp, Threshold) =/= less.

%% @private Check if timestamp is in range
is_in_range(undefined, _Start, _End) -> false;
is_in_range(Timestamp, Start, End) ->
    compare_timestamps(Timestamp, Start) =/= less andalso
    compare_timestamps(Timestamp, End) =/= greater.

%% @private Compare timestamps
compare_timestamps({Mega1, Secs1, Micro1}, {Mega2, Secs2, Micro2}) ->
    case Mega1 of
        M when M < Mega2 -> less;
        M when M > Mega2 -> greater;
        _ ->
            case Secs1 of
                S when S < Secs2 -> less;
                S when S > Secs2 -> greater;
                _ ->
                    case Micro1 of
                        Mi when Mi < Micro2 -> less;
                        Mi when Mi > Micro2 -> greater;
                        _ -> equal
                    end
            end
    end.
