-module(erlmcp_throughput_test).

-author("erlmcp AGI Swarm").
-vsn("3.0.0").

-behaviour(gen_server).

%% API exports
-export([
    start/1,
    stop/1,
    ramp_up/2,
    maintain_throughput/2,
    get_metrics/1,
    set_target_throughput/2
]).

%% gen_server exports
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% TYPE DEFINITIONS
%%====================================================================

-type request_type() :: #{
    name := binary(),
    weight := float(),
    payload := map(),
    timeout := pos_integer(),
    distribution := constant | exponential | normal | burst
}.

-type throughput_config() :: #{
    request_types := [request_type()],
    target_throughput => pos_integer(),
    max_workers => pos_integer(),
    worker_batch_size => pos_integer(),
    ramp_up_duration => pos_integer(),
    test_duration => pos_integer(),
    error_scenarios => [map()]
}.

-type worker_state() :: #{
    pid := pid(),
    request_type => request_type(),
    current_rate := float(),
    request_count := non_neg_integer(),
    success_count := non_neg_integer(),
    error_count := non_neg_integer(),
    total_latency := non_neg_integer(),
    last_request_time := integer(),
    metrics := map()
}.

%%====================================================================
%% GEN_SERVER STATE
%%====================================================================

-record(state, {
    config :: throughput_config(),
    workers :: map(),  #{pid() => worker_state()}
    target_throughput :: pos_integer(),
    current_throughput :: float(),
    test_start_time :: integer(),
    test_phase :: ramp_up | steady_state | ramp_down | completed,
    metrics :: map(),
    control_timer :: reference() | undefined,
    metrics_timer :: reference() | undefined,
    failure_injector :: reference() | undefined
}).

%%====================================================================
%% API FUNCTIONS
%%====================================================================

-spec start(throughput_config()) -> {ok, pid()} | {error, term()}.
start(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:call(Pid, stop, 30000).

-spec ramp_up(pid(), pos_integer()) -> ok.
ramp_up(Pid, TargetThroughput) ->
    gen_server:call(Pid, {ramp_up, TargetThroughput}, 10000).

-spec maintain_throughput(pid(), pos_integer()) -> ok.
maintain_throughput(Pid, TargetThroughput) ->
    gen_server:call(Pid, {maintain_throughput, TargetThroughput}, 10000).

-spec get_metrics(pid()) -> map().
get_metrics(Pid) ->
    gen_server:call(Pid, get_metrics, 5000).

-spec set_target_throughput(pid(), pos_integer()) -> ok.
set_target_throughput(Pid, TargetThroughput) ->
    gen_server:call(Pid, {set_target_throughput, TargetThroughput}, 5000).

%%====================================================================
%% GEN_SERVER CALLBACKS
%%====================================================================

init(Config) ->
    ?LOG_INFO("Starting throughput test with config: ~p", [Config]),

    State = #state{
        config = Config,
        workers = #{},
        target_throughput = ?config(target_throughput, Config),
        current_throughput = 0.0,
        test_start_time = erlang:system_time(millisecond),
        test_phase = ramp_up,
        metrics = initialize_metrics(),
        control_timer = undefined,
        metrics_timer = undefined,
        failure_injector = undefined
    },

    %% Start control loop
    ControlTimer = erlang:send_after(1000, self(), control_tick),

    %% Start metrics collection
    MetricsTimer = erlang:send_after(?config(sampling_interval, Config), self(), collect_metrics),

    %% Start failure injection
    case ?config(error_scenarios, Config) of
        [] -> ok;
        _ -> FailureTimer = erlang:send_after(30000, self(), inject_failures)
    end,

    {ok, State#state{control_timer = ControlTimer, metrics_timer = MetricsTimer}}.

handle_call({ramp_up, TargetThroughput}, _From, State) ->
    ?LOG_INFO("Ramping up throughput to ~p req/s", [TargetThroughput]),
    {reply, ok, State#state{target_throughput = TargetThroughput}};

handle_call({maintain_throughput, TargetThroughput}, _From, State) ->
    ?LOG_INFO("Maintaining throughput at ~p req/s", [TargetThroughput]),
    {reply, ok, State#state{target_throughput = TargetThroughput}};

handle_call({set_target_throughput, TargetThroughput}, _From, State) ->
    ?LOG_INFO("Setting target throughput to ~p req/s", [TargetThroughput]),
    {reply, ok, State#state{target_throughput = TargetThroughput}};

handle_call(get_metrics, _From, State) ->
    Metrics = generate_throughput_metrics(State),
    {reply, Metrics, State};

handle_call(stop, _From, State) ->
    %% Initiate graceful shutdown
    ShutdownTimer = erlang:send_after(5000, self(), graceful_shutdown),
    {reply, ok, State#state{control_timer = ShutdownTimer}}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(control_tick, State) ->
    %% Control throughput based on current phase
    NewState = control_throughput(State),

    %% Schedule next control tick
    ControlTimer = erlang:send_after(1000, self(), control_tick),

    {noreply, NewState#state{control_timer = ControlTimer}};

handle_info(collect_metrics, State) ->
    %% Collect metrics from all workers
    Metrics = collect_worker_metrics(State),

    %% Update state metrics
    NewState = State#state{
        metrics = update_metrics(State#state.metrics, Metrics)
    },

    %% Schedule next metrics collection
    MetricsTimer = erlang:send_after(?config(sampling_interval, State#state.config),
                                    self(), collect_metrics),

    {noreply, NewState#state{metrics_timer = MetricsTimer}};

handle_info(inject_failures, State) ->
    ?LOG_INFO("Injecting test failures"),

    %% Inject failures in workers
    lists:foreach(fun(WorkerPid) ->
        gen_server:cast(WorkerPid, inject_failure)
    end, maps:keys(State#state.workers)),

    %% Schedule next failure injection
    FailureTimer = erlang:send_after(rand:uniform(60000) + 30000, self(), inject_failures),

    {noreply, State#state{failure_injector = FailureTimer}};

handle_info(graceful_shutdown, State) ->
    %% Stop all workers
    StopMessage = {stop, shutdown},
    lists:foreach(fun(WorkerPid) ->
        WorkerPid ! StopMessage
    end, maps:keys(State#state.workers)),

    {noreply, State};

handle_info({worker_completed, WorkerPid, WorkerStats}, State) ->
    %% Remove completed worker
    NewWorkers = maps:remove(WorkerPid, State#state.workers),

    %% Update metrics
    UpdatedMetrics = update_worker_metrics(State#state.metrics, WorkerStats),

    NewState = State#state{
        workers = NewWorkers,
        metrics = UpdatedMetrics
    },

    ?LOG_DEBUG("Worker completed: ~p", [WorkerPid]),

    {noreply, NewState}.

terminate(_Reason, State) ->
    ?LOG_INFO("Terminating throughput test, active workers: ~p",
             [maps:size(State#state.workers)]),

    %% Stop all timers
    [erlang:cancel_timer(Timer) || Timer <-
        [State#state.control_timer, State#state.metrics_timer, State#state.failure_injector]
    ],

    %% Stop all workers
    lists:foreach(fun(WorkerPid) ->
        WorkerPid ! {stop, shutdown}
    end, maps:keys(State#state.workers)),

    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% INTERNAL FUNCTIONS
##====================================================================

initialize_metrics() ->
    #{
        timestamp => erlang:system_time(millisecond),
        target_throughput => 0,
        actual_throughput => 0,
        throughput_variance => 0,
        success_rate => 0.0,
        error_rate => 0.0,
        average_latency => 0.0,
        p95_latency => 0.0,
        p99_latency => 0.0,
        response_time_distribution => #{},
        throughput_history => [],
        latency_history => [],
        error_history => [],
        phase_history => [],
        worker_stats => #{},
        bottleneck_analysis => #{},
        scaling_recommendations => []
    }.

control_throughput(State) ->
    CurrentPhase = State#state.test_phase,
    TargetTPS = State#state.target_throughput,
    CurrentTPS = State#state.current_throughput,

    case CurrentPhase of
        ramp_up ->
            RampUpState = handle_ramp_up(State, TargetTPS, CurrentTPS),
            RampUpState#state{test_phase = steady_state};
        steady_state ->
            SteadyStateState = handle_steady_state(State, TargetTPS, CurrentTPS),
            SteadyStateState;
        ramp_down ->
            RampDownState = handle_ramp_down(State, TargetTPS, CurrentTPS),
            RampDownState#state{test_phase = completed};
        completed ->
            State
    end.

handle_ramp_up(State, TargetTPS, CurrentTPS) ->
    RampDuration = ?config(ramp_up_duration, State#state.config),
    ElapsedTime = erlang:system_time(millisecond) - State#state.test_start_time,
    RampProgress = min(1.0, ElapsedTime / RampDuration),

    TargetTPSForPhase = TargetTPS * RampProgress,

    if TargetTPSForPhase > CurrentTPS ->
        ramp_up_workers(State, TargetTPSForPhase);
    true ->
        State
    end.

handle_steady_state(State, TargetTPS, CurrentTPS) ->
    Duration = ?config(test_duration, State#state.config),
    ElapsedTime = erlang:system_time(millisecond) - State#state.test_start_time,

    if ElapsedTime < Duration ->
        maintain_worker_count(State, TargetTPS, CurrentTPS);
    true ->
        State#state{test_phase = ramp_down}
    end.

handle_ramp_down(State, TargetTPS, CurrentTPS) ->
    RampDuration = ?config(ramp_down_duration, State#state.config),
    ElapsedTime = erlang:system_time(millisecond) - State#state.test_start_time,
    RampProgress = min(1.0, ElapsedTime / RampDuration),

    TargetTPSForPhase = TargetTPS * (1.0 - RampProgress),

    ramp_down_workers(State, TargetTPSForPhase).

ramp_up_workers(State, TargetTPS) ->
    CurrentTPS = State#state.current_throughput,
    RequiredTPS = TargetTPS - CurrentTPS,

    if RequiredTPS > 0 ->
        CalculateWorkerCount = calculate_required_workers(RequiredTPS, State#state.config),
        StartWorkers = min(CalculateWorkerCount, ?config(max_workers, State#state.config) -
                          maps:size(State#state.workers)),

        {NewWorkers, NewState} = lists:foldl(fun(_, {AccWorkers, AccState}) ->
            case create_worker(State#state.config) of
                {ok, WorkerPid, WorkerState} ->
                    NewAccWorkers = maps:put(WorkerPid, WorkerState, AccWorkers),
                    {NewAccWorkers, AccState};
                {error, Reason} ->
                    ?LOG_ERROR("Failed to create worker: ~p", [Reason]),
                    {AccWorkers, AccState}
            end
        end, {#{}, State}, lists:seq(1, StartWorkers)),

        NewState#state{
            workers = maps:merge(State#state.workers, NewWorkers),
            current_throughput = State#state.current_throughput + (StartWorkers * 10)
        };
    else ->
        State
    end.

ramp_down_workers(State, TargetTPS) ->
    CurrentTPS = State#state.current_throughput,
    ExcessTPS = CurrentTPS - TargetTPS,

    if ExcessTPS > 0 ->
        WorkersToRemove = calculate_workers_to_remove(ExcessTPS, State),
        StopWorkers = lists:sublist(maps:keys(State#state.workers), min(WorkersToRemove,
                          maps:size(State#state.workers))),

        lists:foreach(fun(WorkerPid) ->
            WorkerPid ! {stop, ramp_down}
        end, StopWorkers),

        #state{
            workers = lists:fold(fun(Pid, Acc) ->
                case lists:member(Pid, StopWorkers) of
                    true -> Acc;
                    false -> maps:put(Pid, maps:get(Pid, State#state.workers), Acc)
                end
            end, #{}, maps:keys(State#state.workers)),
            current_throughput = State#state.current_throughput - (WorkersToRemove * 10)
        };
    else ->
        State
    end.

maintain_worker_count(State, TargetTPS, CurrentTPS) ->
    CurrentWorkers = maps:size(State#state.workers),
    TargetWorkers = calculate_required_workers(TargetTPS, State#state.config),

    if CurrentWorkers < TargetWorkers ->
        ramp_up_workers(State, TargetTPS);
    elif CurrentWorkers > TargetWorkers ->
        ramp_down_workers(State, TargetTPS);
    else ->
        adjust_worker_rates(State, TargetTPS)
    end.

calculate_required_workers(TargetTPS, Config) ->
    %% Calculate number of workers needed to achieve target TPS
    %% Based on request type and worker capabilities
    RequestComplexity = calculate_average_request_complexity(?config(request_types, Config)),
    WorkersPerTPS = 10 / RequestComplexity,  % Estimate based on testing

    trunc(TargetTPS * WorkersPerTPS).

calculate_workers_to_remove(ExcessTPS, State) ->
    WorkersToRemove = trunc(ExcessTPS / 10),  % Each worker contributes ~10 TPS
    max(WorkersToRemove, 1).

adjust_worker_rates(State, TargetTPS) ->
    Workers = State#state.workers,
    TargetPerWorker = TargetTPS / maps:size(Workers),

    lists:foldl(fun(WorkerPid, AccState) ->
        WorkerState = maps:get(WorkerPid, Workers),
        NewRate = WorkerState#worker_state.current_rate *
                 (TargetPerWorker / State#state.current_throughput *
                  maps:size(Workers)),
        adjust_worker_rate(WorkerPid, NewRate),
        AccState
    end, State, maps:keys(Workers)).

create_worker(Config) ->
    RequestTypes = ?config(request_types, Config),
    SelectedType = select_request_type(RequestTypes),

    WorkerPid = spawn_link(fun() -> worker_loop(Config, SelectedType) end),

    WorkerState = #{
        pid => WorkerPid,
        request_type => SelectedType,
        current_rate => 10.0,  % Start with 10 TPS
        request_count => 0,
        success_count => 0,
        error_count => 0,
        total_latency => 0,
        last_request_time => erlang:system_time(millisecond),
        metrics => initialize_worker_metrics()
    },

    {ok, WorkerPid, WorkerState}.

select_request_type(RequestTypes) ->
    %% Select request type based on weight distribution
    TotalWeight = lists:sum([Type#weight || Type <- RequestTypes]),
    Random = rand:uniform() * TotalWeight,

    lists:foldl(fun(Type, Acc) ->
        {Type, Weight} = Type,
        case Acc of
            {selected, Selected} -> {selected, Selected};
            _ ->
                if Weight >= Random -> {selected, Type};
                true -> Acc
                end
        end
    end, {not_selected, none}, RequestTypes).

worker_loop(Config, RequestType) ->
    worker_loop(Config, RequestType, 0).

worker_loop(Config, RequestType, NextRequestTime) ->
    receive
        {stop, Reason} ->
            Reason;
        inject_failure ->
            handle_injected_failure(),
            worker_loop(Config, RequestType, erlang:system_time(millisecond))
    after max(0, NextRequestTime - erlang:system_time(millisecond)) ->
        %% Make request
        StartTime = erlang:system_time(millisecond),
        Result = make_throughput_request(Config, RequestType),
        EndTime = erlang:system_time(millisecond),
        Latency = EndTime - StartTime,

        %% Update metrics
        UpdatedMetrics = update_worker_request_metrics(Result, Latency),

        %% Schedule next request based on current rate
        Interval = calculate_request_interval(RequestType, UpdatedMetrics),
        NextTime = erlang:system_time(millisecond) + Interval,

        worker_loop(Config, RequestType, NextTime)
    end.

make_throughput_request(Config, RequestType) ->
    #{payload := Payload, timeout := Timeout} = RequestType,

    try
        %% Make request to erlmcp
        Result = erlmcp_client:call_tool(<<"throughput_test">>, Payload),

        case Result of
            {ok, _} -> success;
            {error, _} -> error
        end
    catch
        Error:Reason ->
            ?LOG_ERROR("Throughput request failed: ~p:~p", [Error, Reason]),
            error
    end.

calculate_request_interval(RequestType, Metrics) ->
    CurrentRate = Metrics#metrics.current_rate,
    BaseInterval = 1000 / CurrentRate,  # ms per request

    %% Add jitter for realistic load
    Jitter = rand:uniform(100) - 50,
    max(BaseInterval + Jitter, 10).  % Minimum 10ms

handle_injected_failure() ->
    %% Simulate failure
    ?LOG_INFO("Handling injected throughput failure"),
    timer:sleep(1000 + rand:uniform(2000)).

collect_worker_metrics(State) ->
    lists:foldl(fun(WorkerPid, Acc) ->
        case gen_server:call(WorkerPid, get_metrics, 1000) of
            {ok, Metrics} -> merge_metrics(Acc, Metrics);
            {error, _} -> Acc
        end
    end, #{}, maps:keys(State#state.workers)).

merge_metrics(Base, New) ->
    %% Merge metrics from multiple workers
    lists:fold(fun({Key, Value}, Acc) ->
        case maps:get(Key, Acc, undefined) of
            undefined -> Acc#{Key => Value};
            Existing -> Acc#{Key => lists:flatten([Existing, Value])}
        end
    end, Base, maps:to_list(New)).

update_metrics(StateMetrics, WorkerMetrics) ->
    %% Update state metrics with worker data
    StateMetrics#metrics{
        actual_throughput = calculate_actual_throughput(WorkerMetrics),
        throughput_variance = calculate_throughput_variance(WorkerMetrics),
        success_rate = calculate_success_rate(WorkerMetrics),
        average_latency = calculate_average_latency(WorkerMetrics),
        p95_latency = calculate_p95_latency(WorkerMetrics),
        p99_latency = calculate_p99_latency(WorkerMetrics),
        response_time_distribution = calculate_response_time_distribution(WorkerMetrics),
        throughput_history = add_to_history(StateMetrics#metrics.throughput_history,
                                           calculate_actual_throughput(WorkerMetrics)),
        latency_history = add_to_history(StateMetrics#metrics.latency_history,
                                       calculate_average_latency(WorkerMetrics)),
        error_history = add_to_history(StateMetrics#metrics.error_history,
                                     calculate_error_rate(WorkerMetrics)),
        phase_history = add_to_history(StateMetrics#metrics.phase_history, State#state.test_phase)
    }.

update_worker_metrics(StateMetrics, WorkerStats) ->
    %% Update metrics based on worker completion
    WorkerPid = maps:get(pid, WorkerStats),
    StateMetrics#metrics{
        worker_stats = maps:put(WorkerPid, WorkerStats, StateMetrics#metrics.worker_stats),
        bottleneck_analysis = analyze_bottlenecks(StateMetrics),
        scaling_recommendations = generate_scaling_recommendations(StateMetrics)
    }.

generate_throughput_metrics(State) ->
    #{
        timestamp => erlang:system_time(millisecond),
        target_throughput => State#state.target_throughput,
        actual_throughput => State#state.current_throughput,
        throughput_variance => State#state.metrics#metrics.throughput_variance,
        success_rate => State#state.metrics#metrics.success_rate,
        error_rate => 1.0 - State#state.metrics#metrics.success_rate,
        average_latency => State#state.metrics#metrics.average_latency,
        p95_latency => State#state.metrics#metrics.p95_latency,
        p99_latency => State#state.metrics#metrics.p99_latency,
        response_time_distribution => State#state.metrics#metrics.response_time_distribution,
        worker_count => maps:size(State#state.workers),
        test_phase => State#state.test_phase,
        test_duration => erlang:system_time(millisecond) - State#state.test_start_time,
        bottleneck_analysis => State#state.metrics#metrics.bottleneck_analysis,
        scaling_recommendations => State#state.metrics#metrics.scaling_recommendations
    }.

calculate_actual_throughput(WorkerMetrics) ->
    lists:sum([maps:get(current_rate, Metrics, 0) || Metrics <- maps:values(WorkerMetrics)]).

calculate_throughput_variance(WorkerMetrics) ->
    Throughputs = [maps:get(current_rate, Metrics, 0) || Metrics <- maps:values(WorkerMetrics)],
    case length(Throughputs) of
        0 -> 0.0;
        _ -> calculate_variance(Throughputs)
    end.

calculate_variance(List) ->
    Mean = lists:sum(List) / length(List),
    SumSquares = lists:foldl(fun(X, Acc) -> Acc + math:pow(X - Mean, 2) end, 0.0, List),
    math:sqrt(SumSquares / length(List)).

calculate_success_rate(WorkerMetrics) ->
    TotalRequests = lists:sum([maps:get(request_count, Metrics, 0) || Metrics <- maps:values(WorkerMetrics)]),
    SuccessRequests = lists:sum([maps:get(success_count, Metrics, 0) || Metrics <- maps:values(WorkerMetrics)]),
    if TotalRequests > 0 -> SuccessRequests / TotalRequests; true -> 0.0 end.

calculate_average_latency(WorkerMetrics) ->
    TotalLatency = lists:sum([maps:get(total_latency, Metrics, 0) || Metrics <- maps:values(WorkerMetrics)]),
    TotalRequests = lists:sum([maps:get(request_count, Metrics, 0) || Metrics <- maps:values(WorkerMetrics)]),
    if TotalRequests > 0 -> TotalLatency / TotalRequests; true -> 0.0 end.

calculate_p95_latency(WorkerMetrics) ->
    Latencies = [maps:get(total_latency, Metrics, 0) || Metrics <- maps:values(WorkerMetrics)],
    case length(Latencies) of
        0 -> 0;
        _ -> lists:nth(floor(0.95 * length(Latencies)), lists:sort(Latencies))
    end.

calculate_p99_latency(WorkerMetrics) ->
    Latencies = [maps:get(total_latency, Metrics, 0) || Metrics <- maps:values(WorkerMetrics)],
    case length(Latencies) of
        0 -> 0;
        _ -> lists:nth(floor(0.99 * length(Latencies)), lists:sort(Latencies))
    end.

calculate_error_rate(WorkerMetrics) ->
    TotalRequests = lists:sum([maps:get(request_count, Metrics, 0) || Metrics <- maps:values(WorkerMetrics)]),
    ErrorRequests = lists:sum([maps:get(error_count, Metrics, 0) || Metrics <- maps:values(WorkerMetrics)]),
    if TotalRequests > 0 -> ErrorRequests / TotalRequests; true -> 0.0 end.

calculate_response_time_distribution(WorkerMetrics) ->
    Latencies = [maps:get(total_latency, Metrics, 0) || Metrics <- maps:values(WorkerMetrics)],

    %% Create buckets
    Buckets = [
        {0-10, []}, {10-50, []}, {50-100, []},
        {100-250, []}, {250-500, []}, {500-1000, []}, {1000-2000, []}, {2000, []}
    ],

    lists:foldl(fun(Latency, Acc) ->
        case Latency of
            L when L < 10 -> update_bucket(0-10, L, Acc);
            L when L < 50 -> update_bucket(10-50, L, Acc);
            L when L < 100 -> update_bucket(50-100, L, Acc);
            L when L < 250 -> update_bucket(100-250, L, Acc);
            L when L < 500 -> update_bucket(250-500, L, Acc);
            L when L < 1000 -> update_bucket(500-1000, L, Acc);
            L when L < 2000 -> update_bucket(1000-2000, L, Acc);
            _ -> update_bucket(2000, Latency, Acc)
        end
    end, Buckets, Latencies).

update_bucket(Bucket, Value, Buckets) ->
    lists:foldl(fun({Range, Values}, Acc) ->
        case Range of
            Bucket -> [{Range, [Value | Values]} | Acc];
            _ -> [Range, Values | Acc]
        end
    end, [], Buckets).

add_to_history(History, Value) ->
    lists:sublist([Value | History], 100).  % Keep last 100 values

initialize_worker_metrics() ->
    #{
        request_count => 0,
        success_count => 0,
        error_count => 0,
        total_latency => 0,
        last_request_time => erlang:system_time(millisecond),
        current_rate => 10.0
    }.

update_worker_request_metrics(Result, Latency) ->
    Metrics = initialize_worker_metrics(),
    case Result of
        success -> Metrics#{success_count := 1, total_latency := Latency};
        error -> Metrics#{error_count := 1, total_latency := Latency}
    end.

analyze_bottlenecks(StateMetrics) ->
    %% Analyze throughput bottlenecks
    Throughputs = StateMetrics#metrics.throughput_history,
    Latencies = StateMetrics#metrics.latency_history,

    Bottlenecks = #{
        cpu_bottleneck => detect_cpu_bottleneck(Throughputs),
        memory_bottleneck => detect_memory_bottleneck(Throughputs),
        network_bottleneck => detect_network_bottleneck(Latencies),
        io_bottleneck => detect_io_bottleneck(Latencies)
    },

    Bottlenecks.

generate_scaling_recommendations(StateMetrics) ->
    %% Generate scaling recommendations based on metrics
    Recommendations = [],

    case StateMetrics#metrics.success_rate < 0.95 of
        true ->
            [#{type => capacity, action => increase_workers, reason => "Low success rate"} | Recommendations];
        false ->
            Recommendations
    end.

detect_cpu_bottleneck(Throughputs) ->
    %% Simple CPU bottleneck detection based on throughput patterns
    case Throughputs of
        [_] -> false;
        _ ->
            Recent = lists:sublist(Throughputs, 10),
            if Recent =/= Throughputs ->
                drop_percentage = calculate_drop_percentage(lists:last(Recent), lists:nth(1, Recent)),
                drop_percentage > 20;
            true -> false
            end
    end.

detect_memory_bottleneck(Throughputs) ->
    %% Memory bottleneck detection
    case Throughputs of
        [_] -> false;
        _ ->
            %% Check for performance degradation over time
            has_degraded_performance(Throughputs)
    end.

detect_network_bottleneck(Latencies) ->
    %% Network bottleneck detection based on latency
    case Latencies of
        [_] -> false;
        _ ->
            AverageLatency = lists:sum(Latencies) / length(Latencies),
            LastLatency = lists:last(Latencies),
            LastLatency > AverageLatency * 2
    end.

detect_io_bottleneck(Latencies) ->
    %% I/O bottleneck detection
    %% Check for spikes in latency
    case Latencies of
        [_] -> false;
        _ ->
            Spikes = [L || L <- Latencies, L > 1000],
            length(Spikes) > length(Latencies) * 0.1  % More than 10% are slow
    end.

calculate_drop_percentage(Newer, Older) ->
    if Older > 0 ->
        ((Older - Newer) / Older) * 100;
    true -> 0
    end.

has_degraded_performance(List) ->
    case length(List) of
        N when N > 5 ->
            FirstHalf = lists:sublist(List, div(N, 2)),
            SecondHalf = lists:nthtail(div(N, 2), List),
            lists:sum(SecondHalf) / length(SecondHalf) < lists:sum(FirstHalf) / length(FirstHalf) * 0.8;
        _ -> false
    end.

calculate_average_request_complexity(RequestTypes) ->
    %% Calculate average complexity of requests
    Complexities = [1.0, 1.5, 2.0, 2.5, 3.0],  % Relative complexity factors
    TypeCount = length(RequestTypes),
    lists:sum([lists:nth((N-1 rem 5) + 1, Complexities) || N <- lists:seq(1, TypeCount)]) / TypeCount.

adjust_worker_rate(WorkerPid, NewRate) ->
    WorkerPid ! {adjust_rate, NewRate}.