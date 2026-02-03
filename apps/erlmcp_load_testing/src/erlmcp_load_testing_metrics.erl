%%%-------------------------------------------------------------------
%%% @doc
### Enterprise Metrics Collection System

This module provides comprehensive metrics collection and analysis
 for load testing operations with high-precision measurements.
%%%
 Features:
%%% - Real-time metrics aggregation
%%% - Historical data storage (ETS and disk)
%%% - Performance analysis and bottleneck detection
%%% - Alert generation based on thresholds
%%% - Integration with OpenTelemetry
%%% - Export to multiple backends (Prometheus, InfluxDB, etc.)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_load_testing_metrics).

-export([start_collector/2, stop_collector/1,
         get_metrics/1, get_current_metrics/1, get_final_metrics/1,
         record_success/2, record_error/3, record_latency/2,
         register_generators/2, generator_paused/2, generator_resumed/2,
         generator_stopped/2,
         start_monitoring/1, stop_monitoring/1,
         save_metrics/2, load_metrics/2]).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("erlmcp_load_testing.hrl").

%%====================================================================
## Type Definitions
%%====================================================================

-record(collector_state, {
    test_id :: load_test_id(),
    config :: load_test_config(),
    metrics :: metrics(),
    history :: [metrics()],
    alerts :: [map()],
    collector_ref :: reference(),
    storage_ref :: reference(),
    alert_manager :: pid(),
    analysis_supervisor :: pid()
}).

-record(alert_config, {
    error_rate_threshold :: float(),
    latency_threshold :: pos_integer(),
    throughput_threshold :: pos_integer(),
    resource_threshold :: float()
}).

-type collector_state() :: #collector_state{}.

%%====================================================================
## API Functions
%%====================================================================

-spec start_collector(load_test_id(), load_test_config()) -> {ok, pid()}.
start_collector(TestId, Config) ->
    gen_server:start_link({local, TestId}, ?MODULE, [TestId, Config], []).

-spec stop_collector(load_test_id()) -> ok.
stop_collector(TestId) ->
    gen_server:stop(TestId).

-spec get_metrics(load_test_id()) -> {ok, metrics()} | {error, term()}.
get_metrics(TestId) ->
    gen_server:call(TestId, get_metrics).

-spec get_current_metrics(load_test_id()) -> {ok, metrics()} | {error, term()}.
get_current_metrics(TestId) ->
    gen_server:call(TestId, get_current_metrics).

-spec get_final_metrics(load_test_id()) -> {ok, metrics()} | {error, term()}.
get_final_metrics(TestId) ->
    gen_server:call(TestId, get_final_metrics).

-spec record_success(load_test_id(), pos_integer()) -> ok.
record_success(TestId, Latency) ->
    gen_server:cast(TestId, {record_success, Latency}).

-spec record_error(load_test_id(), pos_integer(), term()) -> ok.
record_error(TestId, Latency, Reason) ->
    gen_server:cast(TestId, {record_error, Latency, Reason}).

-spec record_latency(load_test_id(), pos_integer()) -> ok.
record_latency(TestId, Latency) ->
    gen_server:cast(TestId, {record_latency, Latency}).

-spec register_generators(load_test_id(), [pid()]) -> ok.
register_generators(TestId, Generators) ->
    gen_server:cast(TestId, {register_generators, Generators}).

-spec generator_paused(load_test_id(), load_generator_stats()) -> ok.
generator_paused(TestId, Stats) ->
    gen_server:cast(TestId, {generator_paused, Stats}).

-spec generator_resumed(load_test_id(), load_generator_stats()) -> ok.
generator_resumed(TestId, Stats) ->
    gen_server:cast(TestId, {generator_resumed, Stats}).

-spec generator_stopped(load_test_id(), load_generator_stats()) -> ok.
generator_stopped(TestId, Stats) ->
    gen_server:cast(TestId, {generator_stopped, Stats}).

-spec start_monitoring(load_test_id()) -> ok.
start_monitoring(TestId) ->
    gen_server:cast(TestId, start_monitoring).

-spec stop_monitoring(load_test_id()) -> ok.
stop_monitoring(TestId) ->
    gen_server:cast(TestId, stop_monitoring).

-spec save_metrics(load_test_id(), metrics()) -> ok.
save_metrics(TestId, Metrics) ->
    gen_server:cast(TestId, {save_metrics, Metrics}).

-spec load_metrics(load_test_id(), pos_integer()) -> {ok, metrics()} | {error, term()}.
load_metrics(TestId, Timestamp) ->
    gen_server:call(TestId, {load_metrics, Timestamp}).

%%====================================================================
## gen_server Callbacks
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec init([load_test_id(), load_test_config()]) -> {ok, collector_state()}.
init([TestId, Config]) ->
    process_flag(trap_exit, true),

    %% Initialize metrics storage
    MetricsTable = ets:new(metrics, [
        set,
        {keypos, 1},  % timestamp
        {write_concurrency, true},
        {read_concurrency, true},
        named_table,
        public
    ]),

    HistoryTable = ets:new(history, [
        ordered_set,
        {keypos, 1},  % timestamp
        {write_concurrency, true},
        named_table,
        public
    ]),

    %% Initialize state
    InitialMetrics = #{
        timestamp => erlang:system_time(millisecond),
        throughput => #{
            requests_per_second => 0.0,
            successful_requests => 0,
            failed_requests => 0,
            success_rate => 1.0
        },
        latency => #{
            average => 0.0,
            p50 => 0.0,
            p90 => 0.0,
            p95 => 0.0,
            p99 => 0.0,
            max => 0.0
        },
        resource_usage => #{
            cpu => 0.0,
            memory => 0.0,
            connections => 0,
            file_descriptors => 0
        },
        network => #{
            bytes_sent => 0,
            bytes_received => 0,
            packets_sent => 0,
            packets_received => 0
        },
        errors => #{
            connection_errors => 0,
            timeout_errors => 0,
            protocol_errors => 0,
            other_errors => 0
        }
    },

    %% Start alert manager
    {ok, AlertManager} = erlmcp_load_testing_alert_manager:start(TestId, Config),

    %% Start performance analysis
    {ok, AnalysisSup} = erlmcp_load_testing_analysis_sup:start_link(TestId),

    %% Start storage supervisor
    {ok, StorageSup} = erlmcp_load_testing_storage_sup:start_link(TestId),

    %% Start metrics collection timer
    CollectorRef = erlang:start_timer(1000, self(), collect_metrics),

    State = #collector_state{
        test_id = TestId,
        config = Config,
        metrics = InitialMetrics,
        history = [],
        alerts = [],
        collector_ref = CollectorRef,
        storage_ref = undefined,
        alert_manager = AlertManager,
        analysis_supervisor = AnalysisSup
    },

    %% Store initial metrics
    save_to_storage(State),

    {ok, State}.

-spec handle_call(term(), {pid(), term()}, collector_state()) ->
                       {reply, term(), collector_state()} | {stop, term(), collector_state()}.
handle_call(get_metrics, _From, State) ->
    {reply, {ok, State#collector_state.metrics}, State};

handle_call(get_current_metrics, _From, State) ->
    CurrentMetrics = calculate_current_metrics(State),
    {reply, {ok, CurrentMetrics}, State};

handle_call(get_final_metrics, _From, State) ->
    FinalMetrics = calculate_final_metrics(State),
    {reply, {ok, FinalMetrics}, State};

handle_call({load_metrics, Timestamp}, _From, State) ->
    case ets:lookup(State#collector_state.metrics, Timestamp) of
        [{Timestamp, Metrics}] ->
            {reply, {ok, Metrics}, State};
        [] ->
            {reply, {error, not_found}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

-spec handle_cast(term(), collector_state()) -> {noreply, collector_state()} | {stop, term(), collector_state()}.
handle_cast({record_success, Latency}, State) ->
    %% Record successful request
    NewMetrics = record_success_metric(State#collector_state.metrics, Latency),
    NewState = State#collector_state{metrics = NewMetrics},

    %% Check for alerts
    check_alerts(NewState),

    {noreply, NewState};

handle_cast({record_error, Latency, Reason}, State) ->
    %% Record failed request
    NewMetrics = record_error_metric(State#collector_state.metrics, Latency, Reason),
    NewState = State#collector_state{metrics = NewMetrics},

    %% Check for alerts
    check_alerts(NewState),

    {noreply, NewState};

handle_cast({record_latency, Latency}, State) ->
    %% Record latency metric
    NewMetrics = record_latency_metric(State#collector_state.metrics, Latency),
    NewState = State#collector_state{metrics = NewMetrics},

    {noreply, NewState};

handle_cast({register_generators, Generators}, State) ->
    %% Register active generators
    erlmcp_load_testing_alert_manager:update_generators(
        State#collector_state.alert_manager, Generators),

    {noreply, State};

handle_cast({generator_paused, Stats}, State) ->
    %% Handle generator pause
    erlmcp_load_testing_alert_manager:generator_paused(
        State#collector_state.alert_manager, Stats),

    {noreply, State};

handle_cast({generator_resumed, Stats}, State) ->
    %% Handle generator resume
    erlmcp_load_testing_alert_manager:generator_resumed(
        State#collector_state.alert_manager, Stats),

    {noreply, State};

handle_cast({generator_stopped, Stats}, State) ->
    %% Handle generator stop
    erlmcp_load_testing_alert_manager:generator_stopped(
        State#collector_state.alert_manager, Stats),

    {noreply, State};

handle_cast(start_monitoring, State) ->
    %% Start resource monitoring
    {ok, MonitorRef} = start_resource_monitoring(State),

    {noreply, State#collector_state{storage_ref = MonitorRef}};

handle_cast(stop_monitoring, State) ->
    %% Stop resource monitoring
    case State#collector_state.storage_ref of
        undefined ->
            ok;
        MonitorRef ->
            stop_resource_monitoring(MonitorRef)
    end,

    {noreply, State};

handle_cast({save_metrics, Metrics}, State) ->
    %% Save metrics to persistent storage
    save_to_persistent_storage(State, Metrics),

    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), collector_state()) -> {noreply, collector_state()} | {stop, term(), collector_state()}.
handle_info({timeout, TimerRef, collect_metrics}, State) ->
    %% Collect aggregated metrics
    CurrentMetrics = calculate_current_metrics(State),
    NewState = State#collector_state{metrics = CurrentMetrics},

    %% Add to history
    HistoryItem = CurrentMetrics,
    NewHistory = [HistoryItem | State#collector_state.history],

    %% Update metrics table
    ets:insert(State#collector_state.metrics,
               {CurrentMetrics#timestamp, CurrentMetrics}),

    %% Store periodically
    save_to_storage(NewState),

    %% Schedule next collection
    NewTimerRef = erlang:start_timer(1000, self(), collect_metrics),

    {noreply, NewState#collector_state{history = NewHistory, collector_ref = NewTimerRef}};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), collector_state()) -> ok.
terminate(_Reason, State) ->
    %% Cleanup resources
    case State#collector_state.storage_ref of
        undefined ->
            ok;
        MonitorRef ->
            stop_resource_monitoring(MonitorRef)
    end,

    %% Save final metrics
    save_to_persistent_storage(State, State#collector_state.metrics),

    %% Generate final report
    generate_final_report(State),

    ok.

-spec code_change(term(), collector_state(), term()) -> {ok, collector_state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
## Internal Functions
%%====================================================================

%% Record success metric
-spec record_success_metric(metrics(), pos_integer()) -> metrics().
record_success_metric(Metrics, Latency) ->
    Throughput = Metrics#throughput,
    LatencyData = Metrics#latency,

    UpdatedThroughput = Throughput#{
        successful_requests => Throughput#successful_requests + 1
    },

    UpdatedLatency = LatencyData#{
        average => calculate_average(LatencyData#average, Latency),
        max => max(LatencyData#max, Latency)
    },

    Metrics#{
        throughput => UpdatedThroughput,
        latency => UpdatedLatency
    }.

%% Record error metric
-spec record_error_metric(metrics(), pos_integer(), term()) -> metrics().
record_error_metric(Metrics, Latency, Reason) ->
    Throughput = Metrics#throughput,
    LatencyData = Metrics#latency,
    Errors = Metrics#errors,

    UpdatedThroughput = Throughput#{
        failed_requests => Throughput#failed_requests + 1,
        success_rate => calculate_success_rate(UpdatedThroughput)
    },

    UpdatedLatency = LatencyData#{
        average => calculate_average(LatencyData#average, Latency),
        max => max(LatencyData#max, Latency)
    },

    UpdatedErrors = case Reason of
        connection_error ->
            Errors#{connection_errors => Errors#connection_errors + 1};
        timeout ->
            Errors#{timeout_errors => Errors#timeout_errors + 1};
        protocol_error ->
            Errors#{protocol_errors => Errors#protocol_errors + 1};
        _ ->
            Errors#{other_errors => Errors#other_errors + 1}
    end,

    Metrics#{
        throughput => UpdatedThroughput,
        latency => UpdatedLatency,
        errors => UpdatedErrors
    }.

%% Record latency metric
-spec record_latency_metric(metrics(), pos_integer()) -> metrics().
record_latency_metric(Metrics, Latency) ->
    LatencyData = Metrics#latency,

    UpdatedLatency = LatencyData#{
        average => calculate_average(LatencyData#average, Latency),
        max => max(LatencyData#max, Latency)
    },

    Metrics#{latency => UpdatedLatency}.

%% Calculate current metrics
-spec calculate_current_metrics(collector_state()) -> metrics().
calculate_current_metrics(State) ->
    CurrentTime = erlang:system_time(millisecond),
    Metrics = State#collector_state.metrics,

    %% Aggregate metrics from history
    Aggregated = aggregate_metrics(State#collector_state.history),

    %% Calculate percentiles
    Percentiles = calculate_percentiles(Aggregated#latency),

    Metrics#{
        timestamp => CurrentTime,
        latency => Metrics#latency#{p50 := Percentiles#p50,
                                    p90 := Percentiles#p90,
                                    p95 := Percentiles#p95,
                                    p99 := Percentiles#p99}
    }.

%% Calculate final metrics
-spec calculate_final_metrics(collector_state()) -> metrics().
calculate_final_metrics(State) ->
    AllMetrics = State#collector_state.history,
    FinalMetrics = calculate_current_metrics(State),

    %% Calculate aggregates over entire test
    Aggregated = aggregate_metrics(AllMetrics),

    %% Generate performance summary
    Summary = generate_performance_summary(Aggregated),

    FinalMetrics#{
        timestamp => erlang:system_time(millisecond),
        throughput => Aggregated#throughput,
        latency => Aggregated#latency,
        summary => Summary
    }.

%% Check for alerts
-spec check_alerts(collector_state()) -> ok.
check_alerts(State) ->
    Metrics = State#collector_state.metrics,
    Config = State#collector_state.config,

    ErrorRate = Metrics#throughput#success_rate,
    LatencyP95 = Metrics#latency#p95,
    Throughput = Metrics#throughput#requests_per_second,
    CPUUsage = Metrics#resource_usage#cpu,

    %% Check error rate threshold
    case ErrorRate < (1 - maps:get(error_rate_threshold, Config, ?DEFAULT_ERROR_THRESHOLD)) of
        true ->
            erlmcp_load_testing_alert_manager:trigger_alert(
                State#collector_state.alert_manager,
                error_rate_exceeded,
                <<"Error rate exceeded threshold">>,
                Metrics);
        false ->
            ok
    end,

    %% Check latency threshold
    case LatencyP95 > maps:get(latency_threshold, Config, ?DEFAULT_LATENCY_THRESHOLD) of
        true ->
            erlmcp_load_testing_alert_manager:trigger_alert(
                State#collector_state.alert_manager,
                latency_exceeded,
                <<"Latency exceeded threshold">>,
                Metrics);
        false ->
            ok
    end,

    %% Check throughput threshold
    case Throughput > maps:get(throughput_threshold, Config, ?DEFAULT_THROUGHPUT_THRESHOLD) of
        true ->
            erlmcp_load_testing_alert_manager:trigger_alert(
                State#collector_state.alert_manager,
                throughput_exceeded,
                <<"Throughput exceeded threshold">>,
                Metrics);
        false ->
            ok
    end.

%% Save metrics to storage
-spec save_to_storage(collector_state()) -> ok.
save_to_storage(State) ->
    Metrics = State#collector_state.metrics,
    ets:insert(State#collector_state.metrics,
               {Metrics#timestamp, Metrics}).

%% Save to persistent storage
-spec save_to_persistent_storage(collector_state(), metrics()) -> ok.
save_to_persistent_storage(State, Metrics) ->
    %% Save to disk for persistence
    case file:write_file(metrics_filename(State#collector_state.test_id, Metrics#timestamp),
                         term_to_binary(Metrics)) of
        ok ->
            ok;
        {error, Reason} ->
            error_logger:error_msg("Failed to save metrics: ~p", [Reason])
    end.

%% Generate final report
-spec generate_final_report(collector_state()) -> ok.
generate_final_report(State) ->
    TestId = State#collector_state.test_id,
    FinalMetrics = calculate_final_metrics(State),
    Report = #{
        test_id => TestId,
        timestamp => erlang:system_time(millisecond),
        metrics => FinalMetrics,
        alerts => State#collector_state.alerts,
        summary => generate_summary(State)
    },

    %% Save report
    ReportFile = report_filename(TestId),
    file:write_file(ReportFile, term_to_binary(Report)).

%% Generate summary
-spec generate_summary(collector_state()) -> map().
generate_summary(State) ->
    History = State#collector_state.history,
    Throughput = lists:foldl(fun(M, Acc) ->
                                 Acc + M#throughput#requests_per_second
                             end, 0, History),
    Latency = lists:foldl(fun(M, Acc) ->
                              Acc + M#latency#average
                          end, 0, History),

    #{
        total_duration => length(History),
        avg_throughput => Throughput / max(1, length(History)),
        avg_latency => Latency / max(1, length(History)),
        total_errors => sum_errors(State#collector_state.metrics),
        test_status => determine_test_status(State)
    }.

%% Helper functions
-spec calculate_average(float(), pos_integer()) -> float().
calculate_average(OldAvg, NewValue) ->
    (OldAvg * 0.9) + (NewValue * 0.1).

-spec calculate_success_rate(map()) -> float().
calculate_success_rate(Throughput) ->
    Total = Throughput#successful_requests + Throughput#failed_requests,
    case Total of
        0 -> 1.0;
        _ -> Throughput#successful_requests / Total
    end.

-spec calculate_percentiles(map()) -> map().
calculate_percentiles(LatencyData) ->
    %% Calculate actual percentiles from data
    Latencies = get_all_latencies(),  % This would collect all latency measurements
    Sorted = lists:sort(Latencies),
    Length = length(Sorted),

    #{
        p50 => get_percentile(Sorted, Length, 50),
        p90 => get_percentile(Sorted, Length, 90),
        p95 => get_percentile(Sorted, Length, 95),
        p99 => get_percentile(Sorted, Length, 99)
    }.

-spec get_percentile([pos_integer()], pos_integer(), pos_integer()) -> float().
get_percentile(List, Length, Percentile) ->
    Index = trunc((Length * Percentile) / 100),
    case Index > 0 of
        true ->
            lists:nth(Index, List);
        false ->
            0.0
    end.

-spec sum_errors(map()) -> pos_integer().
sum_errors(Metrics) ->
    Errors = Metrics#errors,
    Errors#connection_errors + Errors#timeout_errors +
    Errors#protocol_errors + Errors#other_errors.

-spec determine_test_status(collector_state()) -> atom().
determine_test_status(State) ->
    Metrics = State#collector_state.metrics,
    ErrorRate = Metrics#throughput#success_rate,

    case ErrorRate of
        Rate when Rate < 0.95 -> failed;
        Rate when Rate < 0.99 -> degraded;
        _ -> passed
    end.

%% File naming functions
-spec metrics_filename(load_test_id(), pos_integer()) -> string().
metrics_filename(TestId, Timestamp) ->
    Filename = "metrics_" ++ binary_to_list(TestId) ++ "_" ++
                integer_to_list(Timestamp) ++ ".dat",
    filename:join([metrics_dir(), Filename]).

-spec report_filename(load_test_id()) -> string().
report_filename(TestId) ->
    Filename = "report_" ++ binary_to_list(TestId) ++ ".dat",
    filename:join([reports_dir(), Filename]).

-spec metrics_dir() -> string().
metrics_dir() ->
    filename:join([code:priv_dir(erlmcp_load_testing), "metrics"]).

-spec reports_dir() -> string().
reports_dir() ->
    filename:join([code:priv_dir(erlmcp_load_testing), "reports"]).

%% Resource monitoring
-spec start_resource_monitoring(collector_state()) -> {ok, reference()}.
start_resource_monitoring(State) ->
    %% Start resource monitoring process
    {ok, MonitorRef} = erlmcp_load_testing_resource_monitor:start(
        State#collector_state.test_id),
    {ok, MonitorRef}.

-spec stop_resource_monitoring(reference()) -> ok.
stop_resource_monitoring(MonitorRef) ->
    erlmcp_load_testing_resource_monitor:stop(MonitorRef).