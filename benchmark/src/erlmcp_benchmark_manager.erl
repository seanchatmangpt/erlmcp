%% @doc Enterprise Benchmark Manager
%% Coordinates all benchmark operations, manages test sessions, and provides
%% enterprise-grade performance monitoring and analysis capabilities.
%% @copyright 2026 erlmcp
%% @version 3.0.0
-module(erlmcp_benchmark_manager).

-behaviour(gen_server).

%% API
-export([
    start_link/0,
    start_group_session/2,
    stop_group_session/0,
    submit_benchmark/2,
    get_session_status/0,
    get_metrics_summary/1,
    stop_benchmark/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%% Types
-type session_id() :: binary().
-type group_id() :: atom().
-type benchmark_id() :: binary().
-type benchmark_type() :: throughput | latency | scalability | resources | data | system | regression.

-type benchmark_config() :: #{
    duration := pos_integer(),
    rate := pos_integer(),
    nodes := pos_integer(),
    data_size := pos_integer(),
    concurrency := pos_integer(),
    replicates := pos_integer(),
    warmup := pos_integer(),
    metrics := [atom()],
    scenario := atom()
}.

-type session_state() :: #{
    group_id => group_id(),
    start_time => integer(),
    status => 'active' | 'completed' | 'failed',
    benchmarks => [benchmark_id()],
    metrics := map(),
    results := map(),
    resource_limits := map()
}.

-type benchmark_state() :: #{
    id := benchmark_id(),
    type := benchmark_type(),
    config := benchmark_config(),
    status := 'queued' | 'running' | 'completed' | 'failed',
    start_time => integer(),
    end_time => integer(),
    results => map(),
    metrics := map(),
    errors := list()
}.

-record(state, {
    current_session :: session_id() | undefined,
    group_session :: session_state() | undefined,
    active_benchmarks :: map(),  #{benchmark_id() => benchmark_state()}
}).

-define(TAB, erlmcp_benchmark_manager).
-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start_group_session(GroupId, Config) ->
    gen_server:call(?SERVER, {start_group_session, GroupId, Config}).

stop_group_session() ->
    gen_server:call(?SERVER, stop_group_session).

submit_benchmark(Type, Config) ->
    gen_server:call(?SERVER, {submit_benchmark, Type, Config}).

get_session_status() ->
    gen_server:call(?SERVER, get_session_status).

get_metrics_summary(BenchmarkId) ->
    gen_server:call(?SERVER, {get_metrics_summary, BenchmarkId}).

stop_benchmark(BenchmarkId) ->
    gen_server:call(?SERVER, {stop_benchmark, BenchmarkId}).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

init([]) ->
    %% Initialize ets tables for benchmark state management
    case ets:info(?TAB) of
        undefined ->
            ets:new(?TAB, [set, public, named_table, {keypos, #benchmark_state.id}]);
        _ ->
            ok
    end,

    %% Initialize metrics collection
    ok = initialize_metrics_collection(),

    %% Initialize enterprise resource monitors
    ok = start_resource_monitors(),

    State = #state{
        current_session = generate_session_id(),
        group_session = undefined,
        active_benchmarks = #{}
    },

    {ok, State}.

handle_call({start_group_session, GroupId, Config}, _From, State) ->
    %% Validate group configuration
    case validate_group_config(Config) of
        {ok, ValidConfig} ->
            SessionState = #{
                group_id => GroupId,
                start_time => erlang:system_time(millisecond),
                status => active,
                benchmarks => [],
                metrics => #{},
                results => #{},
                resource_limits => get_enterprise_resource_limits()
            },

            %% Start group session
            NewState = State#state{
                group_session = SessionState,
                current_session = generate_session_id()
            },

            %% Configure enterprise-level monitoring
            configure_group_session(GroupId, ValidConfig),

            %% Setup load generators for this group
            setup_load_generators_for_group(GroupId, ValidConfig),

            {reply, {ok, SessionState#state.current_session}, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(stop_group_session, _From, State) ->
    case State#state.group_session of
        undefined ->
            {reply, {error, no_active_session}, State};
        SessionState ->
            %% Complete all active benchmarks
            ActiveBenchmarks = State#state.active_benchmarks,
            CompletionResults = complete_benchmarks(ActiveBenchmarks),

            %% Generate group summary
            Summary = generate_group_summary(SessionState, CompletionResults),

            %% Stop resource monitors
            stop_resource_monitors(),

            %% Save group results
            save_group_results(SessionState, Summary),

            %% Update session status
            CompletedSession = SessionState#{
                status => completed,
                end_time => erlang:system_time(millisecond),
                results => Summary
            },

            NewState = State#state{
                group_session = CompletedSession,
                active_benchmarks = #{}
            },

            {reply, {ok, Summary}, NewState}
    end;

handle_call({submit_benchmark, Type, Config}, _From, State) ->
    case State#state.group_session of
        undefined ->
            {reply, {error, no_active_group_session}, State};
        SessionState ->
            BenchmarkId = generate_benchmark_id(),
            BenchmarkState = create_benchmark_state(BenchmarkId, Type, Config),

            %% Store benchmark state
            ets:insert(?TAB, BenchmarkState),

            %% Add to active benchmarks
            NewActiveBenchmarks = maps:put(BenchmarkId, BenchmarkState, State#state.active_benchmarks),

            %% Add benchmark to session
            UpdatedBenchmarks = [BenchmarkId | maps:get(benchmarks, SessionState, [])],
            UpdatedSession = SessionState#{benchmarks => UpdatedBenchmarks},

            %% Start benchmark asynchronously
            start_benchmark_async(BenchmarkId, Type, Config),

            NewState = State#state{
                group_session => UpdatedSession,
                active_benchmarks => NewActiveBenchmarks
            },

            {reply, {ok, BenchmarkId}, NewState}
    end;

handle_call(get_session_status, _From, State) ->
    Status = case State#state.group_session of
        undefined -> no_session;
        SessionState -> SessionState
    end,
    {reply, {ok, Status}, State};

handle_call({get_metrics_summary, BenchmarkId}, _From, State) ->
    case ets:lookup(?TAB, BenchmarkId) of
        [BenchmarkState] ->
            Summary = extract_metrics_summary(BenchmarkState),
            {reply, {ok, Summary}, State};
        [] ->
            case maps:get(BenchmarkId, State#state.active_benchmarks, undefined) of
                undefined ->
                    {reply, {error, not_found}, State};
                ActiveState ->
                    Summary = extract_metrics_summary(ActiveState),
                    {reply, {ok, Summary}, State}
            end
    end;

handle_call({stop_benchmark, BenchmarkId}, _From, State) ->
    case maps:get(BenchmarkId, State#state.active_benchmarks, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        BenchmarkState ->
            %% Stop benchmark gracefully
            ok = stop_benchmark_async(BenchmarkId),

            %% Update status
            UpdatedState = BenchmarkState#{
                status => stopped,
                end_time => erlang:system_time(millisecond)
            },

            %% Store updated state
            ets:insert(?TAB, UpdatedState),

            %% Remove from active benchmarks
            NewActiveBenchmarks = maps:remove(BenchmarkId, State#state.active_benchmarks),

            NewState = State#state{
                active_benchmarks => NewActiveBenchmarks
            },

            {reply, {ok, BenchmarkId}, NewState}
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({benchmark_complete, BenchmarkId, Results}, State) ->
    %% Update benchmark state
    case ets:lookup(?TAB, BenchmarkId) of
        [BenchmarkState] ->
            UpdatedState = BenchmarkState#{
                status => completed,
                end_time => erlang:system_time(millisecond),
                results => Results
            },

            %% Store updated state
            ets:insert(?TAB, UpdatedState),

            %% Update group session results
            UpdatedState;
        _ ->
            not_found
    end,

    %% Check if all benchmarks in group are complete
    case check_group_completion(State) of
        true ->
            %% Trigger group completion
            gen_server:cast(?SERVER, {group_complete, BenchmarkId});
        false ->
            ok
    end,

    {noreply, State};

handle_info({benchmark_failed, BenchmarkId, Error}, State) ->
    %% Update benchmark state
    case ets:lookup(?TAB, BenchmarkId) of
        [BenchmarkState] ->
            UpdatedState = BenchmarkState#{
                status => failed,
                end_time => erlang:system_time(millisecond),
                errors => [Error]
            },

            %% Store updated state
            ets:insert(?TAB, UpdatedState);
        _ ->
            not_found
    end,

    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    %% Cleanup resources
    ok = stop_resource_monitors(),
    ok = export_benchmark_results(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

generate_session_id() ->
    SessionPrefix = "session_" ++ integer_to_list(erlang:system_time(second), 36),
    binary_to_list(crypto:strong_rand_bytes(8), 1) ++ SessionPrefix.

generate_benchmark_id() ->
    "bm_" ++ integer_to_list(erlang:system_time(millisecond), 36).

validate_group_config(Config) ->
    RequiredFields = [duration, rate, nodes],

    case maps:without(RequiredFields, Config) of
        Config when map_size(Config) == 0 ->
            {ok, Config};
        _ ->
            {error, missing_required_fields}
    end.

get_enterprise_resource_limits() ->
    #{
        cpu => 0.95,      % 95% CPU utilization limit
        memory => 0.90,   % 90% memory utilization limit
        network => 10000, % 10Gbps network limit
        disk => 0.85      % 85% disk utilization limit
    }.

initialize_metrics_collection() ->
    %% Initialize enterprise metrics collection
    ok = erlmcp_metrics_collector:start(),
    ok = erlmcp_metrics_collector:configure_enterprise_metrics(),
    ok.

start_resource_monitors() ->
    %% Start enterprise resource monitors
    ok = erlmcp_resource_monitor:start(),
    ok = erlmcp_resource_monitor:configure_enterprise_limits(),
    ok.

stop_resource_monitors() ->
    ok = erlmcp_resource_monitor:stop(),
    ok.

configure_group_session(GroupId, Config) ->
    %% Configure group-specific monitoring
    ok = erlmcp_metrics_collector:configure_group_session(GroupId, Config),

    %% Configure enterprise-level alerting
    configure_enterprise_alerting(Config),

    %% Initialize capacity planning
    ok = erlmcp_capacity_planner:start_session(GroupId, Config),
    ok.

setup_load_generators_for_group(GroupId, Config) ->
    %% Setup distributed load generators
    ok = erlmcp_load_generator:setup_group(GroupId, Config),

    %% Configure enterprise traffic patterns
    configure_enterprise_traffic_patterns(GroupId, Config),
    ok.

create_benchmark_state(BenchmarkId, Type, Config) ->
    #benchmark_state{
        id = BenchmarkId,
        type = Type,
        config = Config,
        status = queued,
        start_time = erlang:system_time(millisecond),
        results = #{},
        metrics = #{},
        errors = []
    }.

start_benchmark_async(BenchmarkId, Type, Config) ->
    %% Start benchmark in a separate process
    Pid = spawn_link(fun() ->
        case run_benchmark(Type, Config) of
            {ok, Results} ->
                gen_server:cast(?SERVER, {benchmark_complete, BenchmarkId, Results});
            {error, Reason} ->
                gen_server:cast(?SERVER, {benchmark_failed, BenchmarkId, Reason})
        end
    end),

    %% Store process reference for monitoring
    ets:insert(?TAB, {BenchmarkId, Pid}),
    ok.

run_benchmark(Type, Config) ->
    case Type of
        throughput ->
            erlmcp_benchmark_throughtput:run(benchmark, Config);
        latency ->
            erlmcp_benchmark_latency:run(benchmark, Config);
        scalability ->
            erlmcp_benchmark_scalability:run(benchmark, Config);
        resources ->
            erlmcp_benchmark_resources:run(benchmark, Config);
        data ->
            erlmcp_benchmark_data:run(benchmark, Config);
        system ->
            erlmcp_benchmark_system:run(benchmark, Config);
        regression ->
            erlmcp_benchmark_regression:run(benchmark, Config)
    end.

stop_benchmark_async(BenchmarkId) ->
    case ets:lookup(?TAB, BenchmarkId) of
        [{BenchmarkId, Pid}] when is_pid(Pid) ->
            Pid ! {stop, self()},
            monitor(process, Pid),
            receive
                {'DOWN', _, process, Pid, _} ->
                    ok
            after 5000 ->
                {error, timeout}
            end;
        _ ->
            {error, not_found}
    end.

complete_benchmarks(ActiveBenchmarks) ->
    %% Complete all active benchmarks and collect results
    maps:fold(fun(BenchmarkId, _State, Acc) ->
        case ets:lookup(?TAB, BenchmarkId) of
            [BenchmarkState] ->
                Results = BenchmarkState#benchmark_state.results,
                Acc#{BenchmarkId => Results};
            _ ->
                Acc
        end
    end, #{}, ActiveBenchmarks).

generate_group_summary(SessionState, CompletionResults) ->
    #{
        group_id => maps:get(group_id, SessionState),
        total_duration => maps:get(end_time, SessionState) - maps:get(start_time, SessionState),
        total_benchmarks => length(maps:get(benchmarks, SessionState)),
        completed_benchmarks => maps:size(CompletionResults),
        throughput_summary => calculate_throughput_summary(CompletionResults),
        latency_summary => calculate_latency_summary(CompletionResults),
        resource_summary => calculate_resource_summary(CompletionResults),
        efficiency_metrics => calculate_efficiency_metrics(CompletionResults),
        recommendations => generate_recommendations(CompletionResults)
    }.

calculate_throughput_summary(Results) ->
    ThroughputValues = maps:fold(fun(_, Result, Acc) ->
        case maps:is_key(throughput, Result) of
            true -> [maps:get(throughput, Result) | Acc];
            false -> Acc
        end
    end, [], Results),

    case ThroughputValues of
        [] -> #{};
        Values ->
            #{
                average => lists:sum(Values) / length(Values),
                maximum => lists:max(Values),
                minimum => lists:min(Values),
                p95 => calculate_percentile(95, Values),
                p99 => calculate_percentile(99, Values)
            }
    end.

calculate_latency_summary(Results) ->
    LatencyValues = maps:fold(fun(_, Result, Acc) ->
        case maps:is_key(latency, Result) of
            true -> [maps:get(latency, Result) | Acc];
            false -> Acc
        end
    end, [], Results),

    case LatencyValues of
        [] -> #{};
        Values ->
            #{
                p50 => calculate_percentile(50, Values),
                p95 => calculate_percentile(95, Values),
                p99 => calculate_percentile(99, Values),
                p99_9 => calculate_percentile(99.9, Values)
            }
    end.

calculate_resource_summary(Results) ->
    ResourceValues = maps:fold(fun(_, Result, Acc) ->
        case maps:is_key(resource_usage, Result) of
            true -> [maps:get(resource_usage, Result) | Acc];
            false -> Acc
        end
    end, [], Results),

    case ResourceValues of
        [] -> #{};
        Values ->
            CPUValues = [maps:get(cpu, R) || R <- Values, maps:is_key(cpu, R)],
            MemoryValues = [maps:get(memory, R) || R <- Values, maps:is_key(memory, R)],
            NetworkValues = [maps:get(network, R) || R <- Values, maps:is_key(network, R)],

            #{
                cpu => #{
                    average => calculate_average(CPUValues),
                    max => calculate_max(CPUValues)
                },
                memory => #{
                    average => calculate_average(MemoryValues),
                    max => calculate_max(MemoryValues)
                },
                network => #{
                    average => calculate_average(NetworkValues),
                    max => calculate_max(NetworkValues)
                }
            }
    end.

calculate_efficiency_metrics(Results) ->
    %% Calculate various efficiency metrics
    CompletedCount = maps:size(Results),
    TotalThroughput = maps:fold(fun(_, Result, Acc) ->
        case maps:is_key(throughput, Result) of
            true -> Acc + maps:get(throughput, Result);
            false -> Acc
        end
    end, 0, Results),

    #{
        benchmarks_completed => CompletedCount,
        total_throughput => TotalThroughput,
        avg_throughput_per_benchmark => TotalThroughput / CompletedCount,
        success_rate => calculate_success_rate(Results)
    }.

generate_recommendations(Results) ->
    %% Generate optimization recommendations based on results
    Recommendations = [],

    %% Check for throughput bottlenecks
    ThroughputSummary = calculate_throughput_summary(Results),
    case maps:get(average, ThroughputSummary, 0) of
        T when T < 10000 ->
            Recommendations#{throughput => optimize_throughput};
        _ ->
            Recommendations
    end,

    %% Check for latency issues
    LatencySummary = calculate_latency_summary(Results),
    case maps:get(p99, LatencySummary, 0) of
        L when L > 200 ->
            Recommendations#{latency => optimize_latency};
        _ ->
            Recommendations
    end,

    %% Check for resource utilization
    ResourceSummary = calculate_resource_summary(Results),
    CPUUtilization = maps:get(average, maps:get(cpu, ResourceSummary, #{}), 0),
    case CPUUtilization > 0.85 of
        true ->
            Recommendations#{cpu => optimize_cpu};
        false ->
            Recommendations
    end,

    Recommendations.

calculate_percentile(P, Values) ->
    Sorted = lists:sort(Values),
    Length = length(Sorted),
    Index = trunc(P * Length / 100),
    lists:nth(Index + 1, Sorted).

calculate_average(Values) ->
    case Values of
        [] -> 0;
        _ -> lists:sum(Values) / length(Values)
    end.

calculate_max(Values) ->
    case Values of
        [] -> 0;
        _ -> lists:max(Values)
    end.

calculate_success_rate(Results) ->
    SuccessCount = maps:fold(fun(_, Result, Acc) ->
        case maps:is_key(success_rate, Result) of
            true ->
                SR = maps:get(success_rate, Result),
                case SR of
                    SR when SR >= 0.99 -> Acc + 1;
                    _ -> Acc
                end;
            false -> Acc
        end
    end, 0, Results),
    maps:size(Results) / 100.

check_group_completion(State) ->
    %% Check if all benchmarks in active group are complete
    case State#state.group_session of
        undefined -> false;
        SessionState ->
            ActiveIds = maps:keys(State#state.active_benchmarks),
            SessionIds = maps:get(benchmarks, SessionState, []),
            lists:all(fun(Id) ->
                not lists:member(Id, ActiveIds)
            end, SessionIds)
    end.

save_group_results(SessionState, Summary) ->
    %% Save group results to persistent storage
    GroupId = maps:get(group_id, SessionState),
    ok = erlmcp_benchmark_storage:save_group_results(GroupId, Summary),
    ok.

configure_enterprise_alerting(Config) ->
    %% Configure enterprise alerting based on requirements
    AlertThresholds = #{
        response_time => 100,    % 100ms
        error_rate => 0.01,      % 1%
        cpu_utilization => 0.90, % 90%
        memory_utilization => 0.85 % 85%
    },

    ok = erlmcp_alert_manager:configure(AlertThresholds),
    ok.

configure_enterprise_traffic_patterns(GroupId, Config) ->
    %% Configure enterprise traffic patterns (bursty, constant, mixed)
    TrafficProfile = maps:get(traffic_profile, Config, mixed),

    ok = erlmcp_traffic_generator:configure_profile(GroupId, TrafficProfile),
    ok.

extract_metrics_summary(BenchmarkState) ->
    #{
        id => BenchmarkState#benchmark_state.id,
        type => BenchmarkState#benchmark_state.type,
        status => BenchmarkState#benchmark_state.status,
        start_time => BenchmarkState#benchmark_state.start_time,
        end_time => BenchmarkState#benchmark_state.end_time,
        duration => case BenchmarkState#benchmark_state.end_time of
            undefined -> undefined;
            ET -> ET - BenchmarkState#benchmark_state.start_time
        end,
        results => BenchmarkState#benchmark_state.results,
        metrics => BenchmarkState#benchmark_state.metrics,
        errors => BenchmarkState#benchmark_state.errors
    }.

export_benchmark_results() ->
    %% Export all benchmark results to external systems
    Results = ets:tab2list(?TAB),
    ok = erlmcp_benchmark_storage:export_enterprise_results(Results),
    ok.