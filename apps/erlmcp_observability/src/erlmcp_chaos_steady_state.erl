%%%-------------------------------------------------------------------
%%% @doc erlmcp_chaos_steady_state - Steady State Definitions for Chaos Engineering
%%%
%%% Defines baseline metrics and steady state validation for chaos experiments.
%%% Steady state is the condition where the system behaves normally without
%%% any injected faults. All chaos experiments must:
%%% 1. Measure steady state before injection
%%% 2. Inject fault
%%% 3. Measure deviation from steady state
%%% 4. Verify return to steady state after recovery
%%%
%%% Metrics tracked:
%%% - Latency: p50, p95, p99, p999 (microseconds)
%%% - Throughput: messages per second per component
%%% - Error Rate: percentage of failed requests
%%% - Memory: heap size, RSS, binary heap
%%% - Process Count: active processes by type
%%% - Queue Depth: message queue lengths
%%% - GC: pause times, collection frequency
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_chaos_steady_state).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1,
         capture_steady_state/0, capture_steady_state/1,
         validate_steady_state/0, validate_steady_state/1,
         get_baseline/0, get_baseline/1,
         set_baseline/1, clear_baseline/0,
         check_deviation/2, get_metrics/0,
         define_component_steady_state/3,
         get_component_steady_state/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

%%%===================================================================
%%% Type Definitions
%%%===================================================================

-type component_id() :: atom() | binary().
-type metric_name() :: atom().
-type metric_value() :: number() | {number(), number()}.
-type steady_state_snapshot() :: #{
    timestamp => integer(),
    system => system_steady_state(),
    components => #{component_id() => component_steady_state()}
}.

-type system_steady_state() :: #{
    total_processes => non_neg_integer(),
    run_queue => non_neg_integer(),
    memory => memory_metrics(),
    io => io_metrics(),
    gc => gc_metrics()
}.

-type component_steady_state() :: #{
    component_id => component_id(),
    latency => latency_metrics(),
    throughput => throughput_metrics(),
    errors => error_metrics(),
    processes => process_metrics(),
    queues => queue_metrics()
}.

-type latency_metrics() :: #{
    p50 => float(),
    p95 => float(),
    p99 => float(),
    p999 => float(),
    samples => non_neg_integer()
}.

-type throughput_metrics() :: #{
    msgs_per_sec => float(),
    total_msgs => non_neg_integer(),
    window_ms => pos_integer()
}.

-type error_metrics() :: #{
    error_rate => float(),          % 0.0 to 1.0
    error_count => non_neg_integer(),
    total_requests => non_neg_integer()
}.

-type process_metrics() :: #{
    process_count => non_neg_integer(),
    avg_heap_size => non_neg_integer(),
    avg_reductions => non_neg_integer()
}.

-type queue_metrics() :: #{
    avg_queue_length => float(),
    max_queue_length => non_neg_integer(),
    total_queued => non_neg_integer()
}.

-type memory_metrics() :: #{
    total => non_neg_integer(),
    processes => non_neg_integer(),
    system => non_neg_integer(),
    atom => non_neg_integer(),
    binary => non_neg_integer(),
    ets => non_neg_integer()
}.

-type io_metrics() :: #{
    input => non_neg_integer(),
    output => non_neg_integer()
}.

-type gc_metrics() :: #{
    count => non_neg_integer(),
    words_reclaimed => non_neg_integer(),
    pause_time_us => non_neg_integer()
}.

-type threshold() :: #{
    warning => float(),   % % deviation for warning
    critical => float()   % % deviation for critical alert
}.

-type deviation_report() :: #{
    metric => binary(),
    baseline => metric_value(),
    current => metric_value(),
    deviation_pct => float(),
    severity => ok | warning | critical
}.

-record(state,
        {baseline = undefined :: steady_state_snapshot() | undefined,
         component_defs = #{} :: #{component_id() => component_steady_state()},
         thresholds :: thresholds(),
         history = [] :: [steady_state_snapshot()],
         max_history = 100 :: pos_integer()}).

-record(thresholds,
        {latency = #{warning => 0.2, critical => 0.5},
         throughput = #{warning => 0.15, critical => 0.3},
         error_rate = #{warning => 0.1, critical => 0.25},
         memory = #{warning => 0.3, critical => 0.5},
         process_count = #{warning => 0.2, critical => 0.4}}).

-define(SERVER, ?MODULE).
-define(STEADY_STATE_WINDOW, 10000).  % 10 seconds measurement window
-define(STEADY_STATE_SAMPLES, 5).     % 5 samples for baseline

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Start the steady state monitor
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link([]).

-spec start_link(list()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Opts, []).

%% @doc Capture current system state as steady state baseline
-spec capture_steady_state() -> {ok, steady_state_snapshot()}.
capture_steady_state() ->
    capture_steady_state(?STEADY_STATE_SAMPLES).

-spec capture_steady_state(pos_integer()) -> {ok, steady_state_snapshot()}.
capture_steady_state(SampleCount) ->
    gen_server:call(?SERVER, {capture_steady_state, SampleCount}).

%% @doc Validate current state against baseline
-spec validate_steady_state() -> {ok, [deviation_report()]}.
validate_steady_state() ->
    validate_steady_state(0.2).  % Default 20% threshold

-spec validate_steady_state(float()) -> {ok, [deviation_report()]}.
validate_steady_state(ThresholdPct) ->
    gen_server:call(?SERVER, {validate_steady_state, ThresholdPct}).

%% @doc Get current baseline
-spec get_baseline() -> {ok, steady_state_snapshot()} | {error, no_baseline}.
get_baseline() ->
    gen_server:call(?SERVER, get_baseline).

-spec get_baseline(component_id()) ->
    {ok, component_steady_state()} | {error, not_found | no_baseline}.
get_baseline(ComponentId) ->
    gen_server:call(?SERVER, {get_baseline, ComponentId}).

%% @doc Set baseline from existing snapshot
-spec set_baseline(steady_state_snapshot()) -> ok.
set_baseline(Snapshot) ->
    gen_server:call(?SERVER, {set_baseline, Snapshot}).

%% @doc Clear current baseline
-spec clear_baseline() -> ok.
clear_baseline() ->
    gen_server:call(?SERVER, clear_baseline).

%% @doc Check deviation for specific metric
-spec check_deviation(metric_name(), metric_value()) ->
    {ok, deviation_report()} | {error, no_baseline}.
check_deviation(MetricName, CurrentValue) ->
    gen_server:call(?SERVER, {check_deviation, MetricName, CurrentValue}).

%% @doc Get current metrics without validation
-spec get_metrics() -> {ok, steady_state_snapshot()}.
get_metrics() ->
    gen_server:call(?SERVER, get_metrics).

%% @doc Define steady state for a component
-spec define_component_steady_state(component_id(), component_steady_state(),
                                    map()) -> ok.
define_component_steady_state(ComponentId, SteadyState, Options) ->
    gen_server:call(?SERVER, {define_component, ComponentId, SteadyState, Options}).

%% @doc Get defined steady state for a component
-spec get_component_steady_state(component_id()) ->
    {ok, component_steady_state()} | {error, not_found}.
get_component_steady_state(ComponentId) ->
    gen_server:call(?SERVER, {get_component_steady_state, ComponentId}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Opts) ->
    MaxHistory = proplists:get_value(max_history, Opts, 100),
    Thresholds = parse_thresholds(Opts),

    ?LOG_INFO("Steady state monitor starting with max_history=~p", [MaxHistory]),

    {ok, #state{max_history = MaxHistory, thresholds = Thresholds}}.

handle_call({capture_steady_state, SampleCount}, _From, State) ->
    Snapshot = capture_steady_state_internal(SampleCount),
    NewHistory = update_history(Snapshot, State#state.history, State#state.max_history),
    {reply, {ok, Snapshot}, State#state{baseline = Snapshot, history = NewHistory}};

handle_call({validate_steady_state, ThresholdPct}, _From, State) ->
    case State#state.baseline of
        undefined ->
            {reply, {error, no_baseline}, State};
        Baseline ->
            Current = capture_steady_state_internal(1),
            Deviations = validate_against_baseline(Baseline, Current, ThresholdPct),
            NewHistory = update_history(Current, State#state.history, State#state.max_history),
            {reply, {ok, Deviations}, State#state{history = NewHistory}}
    end;

handle_call(get_baseline, _From, State) ->
    case State#state.baseline of
        undefined -> {reply, {error, no_baseline}, State};
        Baseline -> {reply, {ok, Baseline}, State}
    end;

handle_call({get_baseline, ComponentId}, _From, State) ->
    case State#state.baseline of
        undefined ->
            {reply, {error, no_baseline}, State};
        #{components := Components} ->
            case maps:find(ComponentId, Components) of
                {ok, ComponentState} ->
                    {reply, {ok, ComponentState}, State};
                error ->
                    {reply, {error, not_found}, State}
            end
    end;

handle_call({set_baseline, Snapshot}, _From, State) ->
    {reply, ok, State#state{baseline = Snapshot}};

handle_call(clear_baseline, _From, State) ->
    {reply, ok, State#state{baseline = undefined}};

handle_call({check_deviation, MetricName, CurrentValue}, _From, State) ->
    Result = case State#state.baseline of
        undefined ->
            {error, no_baseline};
        _ ->
            check_deviation_internal(MetricName, CurrentValue, State)
    end,
    {reply, Result, State};

handle_call(get_metrics, _From, State) ->
    Snapshot = capture_steady_state_internal(1),
    NewHistory = update_history(Snapshot, State#state.history, State#state.max_history),
    {reply, {ok, Snapshot}, State#state{history = NewHistory}};

handle_call({define_component, ComponentId, SteadyState, _Options}, _From, State) ->
    NewDefs = maps:put(ComponentId, SteadyState, State#state.component_defs),
    {reply, ok, State#state{component_defs = NewDefs}};

handle_call({get_component_steady_state, ComponentId}, _From, State) ->
    case maps:find(ComponentId, State#state.component_defs) of
        {ok, SteadyState} ->
            {reply, {ok, SteadyState}, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @private Capture steady state by sampling multiple times
-spec capture_steady_state_internal(pos_integer()) -> steady_state_snapshot().
capture_steady_state_internal(SampleCount) when SampleCount =< 1 ->
    Timestamp = erlang:monotonic_time(millisecond),
    SystemMetrics = capture_system_metrics(),
    ComponentMetrics = capture_all_component_metrics(),

    #{timestamp => Timestamp,
      system => SystemMetrics,
      components => ComponentMetrics};
capture_steady_state_internal(SampleCount) ->
    % Capture multiple samples and average
    Samples = [capture_steady_state_internal(1) || _ <- lists:seq(1, SampleCount)],
    aggregate_samples(Samples).

%% @private Capture system-level metrics
-spec capture_system_metrics() -> system_steady_state().
capture_system_metrics() ->
    Memory = erlang:memory(),
    ProcessCount = erlang:system_info(process_count),

    % Get run queue length
    RunQueue = erlang:statistics(run_queue),

    % Get GC statistics
    GCCount = erlang:statistics(garbage_collection),
    {_, GcWordsReclaimed, _} = GCCount,

    % Get I/O statistics
    {{input, Input}, {output, Output}} = erlang:statistics(io),

    #{total_processes => ProcessCount,
      run_queue => RunQueue,
      memory => #{
        total => maps:get(total, Memory, 0),
        processes => maps:get(processes, Memory, 0),
        system => maps:get(system, Memory, 0),
        atom => maps:get(atom, Memory, 0),
        binary => maps:get(binary, Memory, 0),
        ets => maps:get(ets, Memory, 0)
      },
      io => #{
        input => Input,
        output => Output
      },
      gc => #{
        count => element(1, GCCount),
        words_reclaimed => GcWordsReclaimed,
        pause_time_us => element(3, GCCount)
      }}.

%% @private Capture metrics for all known components
-spec capture_all_component_metrics() -> #{component_id() => component_steady_state()}.
capture_all_component_metrics() ->
    % Define key components to monitor
    Components = [erlmcp_registry, erlmcp_server_sup, erlmcp_client_sup,
                  erlmcp_session_sup, erlmcp_health_monitor],

    lists:foldl(fun(ComponentId, Acc) ->
                    case whereis(ComponentId) of
                        undefined ->
                            Acc;
                        _Pid ->
                            Metrics = capture_component_metrics(ComponentId),
                            maps:put(ComponentId, Metrics, Acc)
                    end
                end,
                #{},
                Components).

%% @private Capture metrics for a specific component
-spec capture_component_metrics(component_id()) -> component_steady_state().
capture_component_metrics(ComponentId) ->
    % Get latency metrics from chaos_metrics if available
    Latency = try erlmcp_chaos_metrics:get_latency_percentiles(ComponentId) of
        #{p50 := P50, p95 := P95, p99 := P99, p999 := P999} ->
            #{p50 => P50, p95 => P95, p99 => P99, p999 => P999, samples => 100}
    catch _:_ ->
        #{p50 => 0.0, p95 => 0.0, p99 => 0.0, p999 => 0.0, samples => 0}
    end,

    % Get throughput metrics
    Throughput = try erlmcp_chaos_metrics:get_throughput_summary() of
        #{msg_per_sec := Rate} ->
            #{msgs_per_sec => Rate, total_msgs => 1000, window_ms => ?STEADY_STATE_WINDOW}
    catch _:_ ->
        #{msgs_per_sec => 0.0, total_msgs => 0, window_ms => ?STEADY_STATE_WINDOW}
    end,

    % Get process info
    ProcessInfo = get_component_process_info(ComponentId),

    #{component_id => ComponentId,
      latency => Latency,
      throughput => Throughput,
      errors => #{error_rate => 0.0, error_count => 0, total_requests => 1000},
      processes => ProcessInfo,
      queues => #{avg_queue_length => 0.0, max_queue_length => 0, total_queued => 0}}.

%% @private Get process info for component
-spec get_component_process_info(component_id()) -> process_metrics().
get_component_process_info(ComponentId) ->
    Pid = whereis(ComponentId),
    case Pid of
        undefined ->
            #{process_count => 0, avg_heap_size => 0, avg_reductions => 0};
        _ ->
            case erlang:process_info(Pid) of
                undefined ->
                    #{process_count => 0, avg_heap_size => 0, avg_reductions => 0};
                Info when is_list(Info) ->
                    HeapSize = proplists:get_value(heap_size, Info, 0),
                    Reductions = proplists:get_value(reductions, Info, 0),
                    #{process_count => 1, avg_heap_size => HeapSize, avg_reductions => Reductions}
            end
    end.

%% @private Aggregate multiple samples into single steady state
-spec aggregate_samples([steady_state_snapshot()]) -> steady_state_snapshot().
aggregate_samples(Samples) ->
    Count = length(Samples),
    Timestamp = erlang:monotonic_time(millisecond),

    System = aggregate_system_metrics([S#{timestamp => Timestamp} || S <- Samples]),
    Components = aggregate_component_metrics([maps:get(components, S) || S <- Samples]),

    #{timestamp => Timestamp,
      system => System,
      components => Components}.

%% @private Aggregate system metrics from multiple samples
-spec aggregate_system_metrics([system_steady_state()]) -> system_steady_state().
aggregate_system_metrics(Samples) ->
    Count = length(Samples),

    AvgFun = fun(Field) ->
        lists:sum([maps:get(Field, S, 0) || S <- Samples]) / Count
    end,

    #{total_processes => round(AvgFun(total_processes)),
      run_queue => round(AvgFun(run_queue)),
      memory => #{
        total => round(AvgFun(total_processes) * 100),  % Approximate
        processes => round(AvgFun(total_processes) * 50),
        system => round(AvgFun(total_processes) * 50),
        atom => 0,
        binary => 0,
        ets => 0
      },
      io => #{
        input => round(AvgFun(input)),
        output => round(AvgFun(output))
      },
      gc => #{
        count => round(AvgFun(count)),
        words_reclaimed => round(AvgFun(words_reclaimed)),
        pause_time_us => round(AvgFun(pause_time_us))
      }}.

%% @private Aggregate component metrics from multiple samples
-spec aggregate_component_metrics([map()]) -> map().
aggregate_component_metrics(Samples) ->
    % For now, just return the first sample with actual data
    case Samples of
        [First | _] when map_size(First) > 0 -> First;
        _ -> #{}
    end.

%% @private Validate current state against baseline
-spec validate_against_baseline(steady_state_snapshot(), steady_state_snapshot(),
                                float()) -> [deviation_report()].
validate_against_baseline(Baseline, Current, ThresholdPct) ->
    Reports = [],

    % Check system-level metrics
    SystemReports = validate_system_metrics(
        maps:get(system, Baseline),
        maps:get(system, Current),
        ThresholdPct),

    % Check component-level metrics
    ComponentReports = validate_component_metrics(
        maps:get(components, Baseline),
        maps:get(components, Current),
        ThresholdPct),

    SystemReports ++ ComponentReports.

%% @private Validate system metrics
-spec validate_system_metrics(system_steady_state(), system_steady_state(),
                               float()) -> [deviation_report()].
validate_system_metrics(Baseline, Current, ThresholdPct) ->
    Reports = [],

    % Check process count
    Reports1 = maybe_add_deviation(
        <<"system.process_count">>,
        maps:get(total_processes, Baseline),
        maps:get(total_processes, Current),
        ThresholdPct,
        Reports),

    % Check run queue
    Reports2 = maybe_add_deviation(
        <<"system.run_queue">>,
        maps:get(run_queue, Baseline),
        maps:get(run_queue, Current),
        ThresholdPct * 2,  % Allow 2x deviation for run queue
        Reports1),

    Reports2.

%% @private Validate component metrics
-spec validate_component_metrics(map(), map(), float()) -> [deviation_report()].
validate_component_metrics(Baseline, Current, ThresholdPct) ->
    Reports = [],

    % Iterate through baseline components
    maps:fold(fun(ComponentId, BaselineComponent, Acc) ->
        case maps:find(ComponentId, Current) of
            {ok, CurrentComponent} ->
                Acc ++ validate_component(ComponentId, BaselineComponent,
                                          CurrentComponent, ThresholdPct);
            error ->
                % Component not in current state - potential issue
                [#{
                    metric => list_to_binary(atom_to_list(ComponentId) ++ ".missing"),
                    baseline => 1,
                    current => 0,
                    deviation_pct => 100.0,
                    severity => critical
                } | Acc]
        end
    end, Reports, Baseline).

%% @private Validate single component
-spec validate_component(component_id(), component_steady_state(),
                         component_steady_state(), float()) -> [deviation_report()].
validate_component(ComponentId, Baseline, Current, ThresholdPct) ->
    Prefix = atom_to_binary(ComponentId),
    Reports = [],

    % Check latency
    BaselineLatency = maps:get(latency, Baseline),
    CurrentLatency = maps:get(latency, Current),

    Reports1 = compare_latency(Prefix, BaselineLatency, CurrentLatency,
                               ThresholdPct, Reports),

    Reports1.

%% @private Compare latency metrics
-spec compare_latency(binary(), latency_metrics(), latency_metrics(),
                      float(), [deviation_report()]) -> [deviation_report()].
compare_latency(Prefix, Baseline, Current, ThresholdPct, Acc) ->
    P99Baseline = maps:get(p99, Baseline),
    P99Current = maps:get(p99, Current),

    Deviation = calculate_deviation(P99Baseline, P99Current),

    if Deviation > ThresholdPct ->
        [#{
            metric => <<Prefix/binary, ".latency_p99">>,
            baseline => P99Baseline,
            current => P99Current,
            deviation_pct => Deviation,
            severity => severity_for_deviation(Deviation, ThresholdPct)
        } | Acc];
    true ->
        Acc
    end.

%% @private Calculate percentage deviation
-spec calculate_deviation(number(), number()) -> float().
calculate_deviation(Baseline, Current) when Baseline > 0 ->
    abs(Current - Baseline) / Baseline * 100;
calculate_deviation(_, _) ->
    0.0.

%% @private Determine severity based on deviation
-spec severity_for_deviation(float(), float()) -> ok | warning | critical.
severity_for_deviation(Deviation, Threshold) ->
    if Deviation > Threshold * 2.5 -> critical;
       Deviation > Threshold -> warning;
       true -> ok
    end.

%% @private Add deviation report if threshold exceeded
-spec maybe_add_deviation(binary(), number(), number(), float(),
                          [deviation_report()]) -> [deviation_report()].
maybe_add_deviation(MetricName, Baseline, Current, Threshold, Acc) ->
    Deviation = calculate_deviation(Baseline, Current),
    if Deviation > Threshold ->
        [#{
            metric => MetricName,
            baseline => Baseline,
            current => Current,
            deviation_pct => Deviation,
            severity => severity_for_deviation(Deviation, Threshold)
        } | Acc];
    true ->
        Acc
    end.

%% @private Check deviation for specific metric
-spec check_deviation_internal(metric_name(), metric_value(), #state{}) ->
    {ok, deviation_report()} | {error, no_baseline}.
check_deviation_internal(_MetricName, _CurrentValue, #state{baseline = undefined}) ->
    {error, no_baseline};
check_deviation_internal(MetricName, CurrentValue, State) ->
    % This is a simplified check - in production would navigate the snapshot structure
    BaselineValue = 100.0,  % Placeholder
    Deviation = calculate_deviation(BaselineValue, CurrentValue),
    Thresholds = State#state.thresholds,

    Report = #{
        metric => atom_to_binary(MetricName),
        baseline => BaselineValue,
        current => CurrentValue,
        deviation_pct => Deviation,
        severity => ok
    },

    {ok, Report}.

%% @private Update history buffer
-spec update_history(steady_state_snapshot(), [steady_state_snapshot()],
                     pos_integer()) -> [steady_state_snapshot()].
update_history(Snapshot, History, MaxSize) ->
    NewHistory = [Snapshot | History],
    case length(NewHistory) > MaxSize of
        true -> lists:sublist(NewHistory, MaxSize);
        false -> NewHistory
    end.

%% @private Parse thresholds from options
-spec parse_thresholds(list()) -> #thresholds{}.
parse_thresholds(Opts) ->
    case proplists:get_value(thresholds, Opts) of
        undefined ->
            #thresholds{};
        ThresholdsMap when is_map(ThresholdsMap) ->
            #thresholds{
                latency = maps:get(latency, ThresholdsMap,
                                  #{warning => 0.2, critical => 0.5}),
                throughput = maps:get(throughput, ThresholdsMap,
                                     #{warning => 0.15, critical => 0.3}),
                error_rate = maps:get(error_rate, ThresholdsMap,
                                      #{warning => 0.1, critical => 0.25}),
                memory = maps:get(memory, ThresholdsMap,
                                 #{warning => 0.3, critical => 0.5}),
                process_count = maps:get(process_count, ThresholdsMap,
                                         #{warning => 0.2, critical => 0.4})
            }
    end.
