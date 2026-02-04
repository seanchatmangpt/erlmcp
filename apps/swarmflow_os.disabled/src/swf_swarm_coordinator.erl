%%%====================================================================
%%% @doc SwarmFlow Swarm Coordinator
%%%
%%% Coordinates the process-mining swarm that proposes workflow improvements.
%%% Manages pools of specialized workers (conformance_checker, replay_scorer,
%%% patch_proposer, etc.) with load balancing and health monitoring.
%%%
%%% Architecture:
%%% - Worker pools organized by type (conformance_checker, replay_scorer, etc.)
%%% - Round-robin + least-loaded worker selection for task distribution
%%% - Result aggregation with timeout handling
%%% - Patch proposal ranking by confidence and expected improvement
%%% - Integration with swf_promotion_engine for patch promotion
%%% - Periodic health checks with automatic worker restart
%%%
%%% @end
%%%====================================================================
-module(swf_swarm_coordinator).

-behaviour(gen_server).

-include("swarmflow.hrl").
-include_lib("kernel/include/logger.hrl").

%% API exports
-export([
    start_link/1,
    spawn_worker/1,
    spawn_worker/2,
    terminate_worker/1,
    submit_task/2,
    submit_task/3,
    get_workers/0,
    get_workers/1,
    collect_proposals/1,
    collect_proposals/2,
    aggregate_results/2,
    set_worker_count/2,
    get_stats/0,
    health_check/0
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

%%====================================================================
%% Types
%%====================================================================

-type worker_id() :: binary().
-type task_id() :: binary().
-type net_id() :: binary().

-type task() :: #{
    id := task_id(),
    type := swarm_worker_type(),
    payload := term(),
    priority => integer(),
    timeout_ms => pos_integer(),
    callback => {module(), atom(), [term()]}
}.

-type task_result() :: #{
    task_id := task_id(),
    worker_id := worker_id(),
    status := completed | failed | timeout,
    result => term(),
    error => term(),
    duration_ms := non_neg_integer()
}.

-type aggregation_strategy() ::
    first |
    all |
    majority |
    best_score |
    {custom, fun(([task_result()]) -> term())}.

-export_type([worker_id/0, task_id/0, task/0, task_result/0, aggregation_strategy/0]).

%%====================================================================
%% State record
%%====================================================================

-record(pending_task, {
    task_id :: task_id(),
    worker_id :: worker_id(),
    from :: {pid(), term()},
    start_time :: integer(),
    timeout_ref :: reference()
}).

-record(state, {
    %% Worker pools by type: #{swarm_worker_type() => [#swf_swarm_worker{}]}
    worker_pools :: #{swarm_worker_type() => [#swf_swarm_worker{}]},

    %% Quick worker lookup by ID
    worker_index :: ets:tid(),

    %% Pending tasks awaiting results
    pending_tasks :: #{task_id() => #pending_task{}},

    %% Aggregated results by task group
    result_buffers :: #{term() => [task_result()]},

    %% Collected patch proposals by net_id
    proposals :: #{net_id() => [#swf_patch{}]},

    %% Worker counts per type (desired)
    target_counts :: #{swarm_worker_type() => non_neg_integer()},

    %% Round-robin counters for load balancing
    rr_counters :: #{swarm_worker_type() => non_neg_integer()},

    %% Health check timer
    health_timer :: reference() | undefined,

    %% Configuration
    config :: map(),

    %% Metrics
    stats :: #{
        tasks_submitted => non_neg_integer(),
        tasks_completed => non_neg_integer(),
        tasks_failed => non_neg_integer(),
        tasks_timeout => non_neg_integer(),
        workers_spawned => non_neg_integer(),
        workers_terminated => non_neg_integer()
    }
}).

%%====================================================================
%% Macros
%%====================================================================

-define(DEFAULT_WORKER_COUNT, 2).
-define(DEFAULT_TASK_TIMEOUT_MS, 30000).
-define(HEALTH_CHECK_INTERVAL_MS, 10000).
-define(WORKER_RESTART_DELAY_MS, 1000).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the swarm coordinator with configuration
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

%% @doc Spawn a swarm worker of given type with default config
-spec spawn_worker(swarm_worker_type()) -> {ok, worker_id()} | {error, term()}.
spawn_worker(Type) ->
    spawn_worker(Type, #{}).

%% @doc Spawn a swarm worker of given type with config
-spec spawn_worker(swarm_worker_type(), map()) -> {ok, worker_id()} | {error, term()}.
spawn_worker(Type, WorkerConfig) ->
    gen_server:call(?MODULE, {spawn_worker, Type, WorkerConfig}, 10000).

%% @doc Terminate a specific worker
-spec terminate_worker(worker_id()) -> ok | {error, not_found}.
terminate_worker(WorkerId) ->
    gen_server:call(?MODULE, {terminate_worker, WorkerId}, 5000).

%% @doc Submit a task to appropriate worker(s)
-spec submit_task(swarm_worker_type(), term()) -> {ok, task_id()} | {error, term()}.
submit_task(Type, Payload) ->
    submit_task(Type, Payload, #{}).

%% @doc Submit a task with options
-spec submit_task(swarm_worker_type(), term(), map()) ->
    {ok, task_id()} | {ok, task_id(), term()} | {error, term()}.
submit_task(Type, Payload, Opts) ->
    TaskId = generate_task_id(),
    Task = #{
        id => TaskId,
        type => Type,
        payload => Payload,
        priority => maps:get(priority, Opts, 0),
        timeout_ms => maps:get(timeout_ms, Opts, ?DEFAULT_TASK_TIMEOUT_MS)
    },
    case maps:get(sync, Opts, false) of
        true ->
            Timeout = maps:get(timeout_ms, Opts, ?DEFAULT_TASK_TIMEOUT_MS),
            gen_server:call(?MODULE, {submit_task_sync, Task}, Timeout + 1000);
        false ->
            gen_server:call(?MODULE, {submit_task, Task}, 5000)
    end.

%% @doc List all workers
-spec get_workers() -> [#swf_swarm_worker{}].
get_workers() ->
    gen_server:call(?MODULE, get_workers, 5000).

%% @doc List workers of a specific type
-spec get_workers(swarm_worker_type()) -> [#swf_swarm_worker{}].
get_workers(Type) ->
    gen_server:call(?MODULE, {get_workers, Type}, 5000).

%% @doc Collect patch proposals for a specific net
-spec collect_proposals(net_id()) -> {ok, [#swf_patch{}]}.
collect_proposals(NetId) ->
    collect_proposals(NetId, #{}).

%% @doc Collect patch proposals with options (ranking, filtering)
-spec collect_proposals(net_id(), map()) -> {ok, [#swf_patch{}]}.
collect_proposals(NetId, Opts) ->
    gen_server:call(?MODULE, {collect_proposals, NetId, Opts}, 10000).

%% @doc Aggregate results from multiple workers using a strategy
-spec aggregate_results(term(), aggregation_strategy()) -> {ok, term()} | {error, term()}.
aggregate_results(GroupKey, Strategy) ->
    gen_server:call(?MODULE, {aggregate_results, GroupKey, Strategy}, 5000).

%% @doc Adjust worker pool size for a specific type
-spec set_worker_count(swarm_worker_type(), non_neg_integer()) -> ok.
set_worker_count(Type, Count) ->
    gen_server:call(?MODULE, {set_worker_count, Type, Count}, 10000).

%% @doc Get coordinator statistics
-spec get_stats() -> map().
get_stats() ->
    gen_server:call(?MODULE, get_stats, 5000).

%% @doc Perform health check on all workers
-spec health_check() -> #{healthy => [worker_id()], unhealthy => [worker_id()]}.
health_check() ->
    gen_server:call(?MODULE, health_check, 10000).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init(map()) -> {ok, #state{}}.
init(Config) ->
    process_flag(trap_exit, true),

    %% Create worker index ETS table
    WorkerIndex = ets:new(swf_worker_index, [
        set,
        protected,
        {read_concurrency, true}
    ]),

    %% Extract configuration
    DefaultCounts = maps:get(default_worker_counts, Config, #{}),
    HealthInterval = maps:get(health_check_interval_ms, Config, ?HEALTH_CHECK_INTERVAL_MS),

    %% Initialize target counts for each worker type
    AllTypes = [
        conformance_checker,
        replay_scorer,
        patch_proposer,
        pattern_miner,
        anomaly_detector,
        performance_analyzer,
        bottleneck_finder,
        prediction_engine
    ],
    TargetCounts = lists:foldl(
        fun(Type, Acc) ->
            Count = maps:get(Type, DefaultCounts, 0),
            Acc#{Type => Count}
        end,
        #{},
        AllTypes
    ),

    %% Initialize round-robin counters
    RRCounters = lists:foldl(
        fun(Type, Acc) -> Acc#{Type => 0} end,
        #{},
        AllTypes
    ),

    %% Start health check timer
    HealthTimer = erlang:send_after(HealthInterval, self(), health_check),

    State = #state{
        worker_pools = #{},
        worker_index = WorkerIndex,
        pending_tasks = #{},
        result_buffers = #{},
        proposals = #{},
        target_counts = TargetCounts,
        rr_counters = RRCounters,
        health_timer = HealthTimer,
        config = Config,
        stats = #{
            tasks_submitted => 0,
            tasks_completed => 0,
            tasks_failed => 0,
            tasks_timeout => 0,
            workers_spawned => 0,
            workers_terminated => 0
        }
    },

    %% Spawn initial workers based on target counts
    State2 = spawn_initial_workers(State),

    ?LOG_INFO("swf_swarm_coordinator started with config: ~p", [Config]),
    {ok, State2}.

-spec handle_call(term(), {pid(), term()}, #state{}) -> {reply, term(), #state{}}.

%% Spawn a new worker
handle_call({spawn_worker, Type, WorkerConfig}, _From, State) ->
    case do_spawn_worker(Type, WorkerConfig, State) of
        {ok, WorkerId, NewState} ->
            {reply, {ok, WorkerId}, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

%% Terminate a specific worker
handle_call({terminate_worker, WorkerId}, _From, State) ->
    case do_terminate_worker(WorkerId, State) of
        {ok, NewState} ->
            {reply, ok, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

%% Submit task asynchronously
handle_call({submit_task, Task}, _From, State) ->
    case do_submit_task(Task, undefined, State) of
        {ok, TaskId, NewState} ->
            {reply, {ok, TaskId}, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

%% Submit task synchronously (wait for result)
handle_call({submit_task_sync, Task}, From, State) ->
    case do_submit_task(Task, From, State) of
        {ok, _TaskId, NewState} ->
            %% Don't reply now - will reply when result arrives
            {noreply, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

%% Get all workers
handle_call(get_workers, _From, State) ->
    Workers = lists:flatten(maps:values(State#state.worker_pools)),
    {reply, Workers, State};

%% Get workers by type
handle_call({get_workers, Type}, _From, State) ->
    Workers = maps:get(Type, State#state.worker_pools, []),
    {reply, Workers, State};

%% Collect proposals for a net
handle_call({collect_proposals, NetId, Opts}, _From, State) ->
    Proposals = maps:get(NetId, State#state.proposals, []),

    %% Apply filtering
    MinConfidence = maps:get(min_confidence, Opts, 0.0),
    MaxRisk = maps:get(max_risk, Opts, 1.0),
    Filtered = lists:filter(
        fun(#swf_patch{confidence = C, risk_score = R}) ->
            C >= MinConfidence andalso R =< MaxRisk
        end,
        Proposals
    ),

    %% Apply ranking
    Ranked = case maps:get(rank_by, Opts, confidence) of
        confidence ->
            lists:sort(
                fun(#swf_patch{confidence = C1}, #swf_patch{confidence = C2}) ->
                    C1 >= C2
                end,
                Filtered
            );
        improvement ->
            lists:sort(
                fun(#swf_patch{expected_improvement = I1}, #swf_patch{expected_improvement = I2}) ->
                    I1 >= I2
                end,
                Filtered
            );
        risk ->
            lists:sort(
                fun(#swf_patch{risk_score = R1}, #swf_patch{risk_score = R2}) ->
                    R1 =< R2
                end,
                Filtered
            );
        composite ->
            %% Score = confidence * improvement / (1 + risk)
            lists:sort(
                fun(#swf_patch{confidence = C1, expected_improvement = I1, risk_score = R1},
                    #swf_patch{confidence = C2, expected_improvement = I2, risk_score = R2}) ->
                    Score1 = C1 * I1 / (1 + R1),
                    Score2 = C2 * I2 / (1 + R2),
                    Score1 >= Score2
                end,
                Filtered
            )
    end,

    %% Apply limit
    Limited = case maps:get(limit, Opts, undefined) of
        undefined -> Ranked;
        Limit when is_integer(Limit), Limit > 0 ->
            lists:sublist(Ranked, Limit)
    end,

    {reply, {ok, Limited}, State};

%% Aggregate results
handle_call({aggregate_results, GroupKey, Strategy}, _From, State) ->
    Results = maps:get(GroupKey, State#state.result_buffers, []),
    Aggregated = do_aggregate(Results, Strategy),
    %% Clear the buffer after aggregation
    NewBuffers = maps:remove(GroupKey, State#state.result_buffers),
    {reply, Aggregated, State#state{result_buffers = NewBuffers}};

%% Set worker count
handle_call({set_worker_count, Type, Count}, _From, State) ->
    NewTargetCounts = maps:put(Type, Count, State#state.target_counts),
    NewState = State#state{target_counts = NewTargetCounts},
    %% Adjust pool size
    AdjustedState = adjust_pool_size(Type, NewState),
    {reply, ok, AdjustedState};

%% Get statistics
handle_call(get_stats, _From, State) ->
    WorkerCounts = maps:map(
        fun(_Type, Workers) -> length(Workers) end,
        State#state.worker_pools
    ),
    Stats = (State#state.stats)#{
        worker_counts => WorkerCounts,
        pending_tasks => maps:size(State#state.pending_tasks),
        buffered_results => maps:size(State#state.result_buffers),
        proposals_collected => maps:fold(
            fun(_K, V, Acc) -> Acc + length(V) end,
            0,
            State#state.proposals
        )
    },
    {reply, Stats, State};

%% Health check
handle_call(health_check, _From, State) ->
    {Healthy, Unhealthy} = do_health_check(State),
    {reply, #{healthy => Healthy, unhealthy => Unhealthy}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.

%% Worker reports task result
handle_cast({task_result, TaskId, WorkerId, Result}, State) ->
    NewState = handle_task_result(TaskId, WorkerId, Result, State),
    {noreply, NewState};

%% Worker proposes a patch
handle_cast({propose_patch, Patch}, State) ->
    #swf_patch{net_id = NetId} = Patch,
    CurrentProposals = maps:get(NetId, State#state.proposals, []),
    NewProposals = [Patch | CurrentProposals],

    %% Attempt to promote via promotion engine if available
    maybe_promote_patch(Patch),

    {noreply, State#state{
        proposals = maps:put(NetId, NewProposals, State#state.proposals)
    }};

%% Worker status update
handle_cast({worker_status, WorkerId, Status}, State) ->
    NewState = update_worker_status(WorkerId, Status, State),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.

%% Periodic health check
handle_info(health_check, State) ->
    {_Healthy, Unhealthy} = do_health_check(State),

    %% Restart unhealthy workers
    NewState = lists:foldl(
        fun(WorkerId, S) ->
            case ets:lookup(S#state.worker_index, WorkerId) of
                [{WorkerId, Worker}] ->
                    ?LOG_WARNING("Restarting unhealthy worker: ~p (~p)",
                                [WorkerId, Worker#swf_swarm_worker.type]),
                    Type = Worker#swf_swarm_worker.type,
                    Config = Worker#swf_swarm_worker.config,
                    {ok, S2} = do_terminate_worker(WorkerId, S),
                    timer:sleep(?WORKER_RESTART_DELAY_MS),
                    case do_spawn_worker(Type, Config, S2) of
                        {ok, _NewWorkerId, S3} -> S3;
                        {error, _} -> S2
                    end;
                [] ->
                    S
            end
        end,
        State,
        Unhealthy
    ),

    %% Ensure pool sizes match targets
    FinalState = lists:foldl(
        fun(Type, S) -> adjust_pool_size(Type, S) end,
        NewState,
        maps:keys(State#state.target_counts)
    ),

    %% Reschedule health check
    Interval = maps:get(health_check_interval_ms, State#state.config, ?HEALTH_CHECK_INTERVAL_MS),
    HealthTimer = erlang:send_after(Interval, self(), health_check),

    {noreply, FinalState#state{health_timer = HealthTimer}};

%% Task timeout
handle_info({task_timeout, TaskId}, State) ->
    case maps:get(TaskId, State#state.pending_tasks, undefined) of
        undefined ->
            {noreply, State};
        #pending_task{from = From, worker_id = WorkerId} = _PendingTask ->
            %% Update worker metrics
            NewState = update_worker_task_failed(WorkerId, State),
            NewPending = maps:remove(TaskId, NewState#state.pending_tasks),

            %% Reply if synchronous
            case From of
                undefined -> ok;
                _ -> gen_server:reply(From, {error, timeout})
            end,

            Stats = NewState#state.stats,
            NewStats = Stats#{
                tasks_timeout => maps:get(tasks_timeout, Stats, 0) + 1
            },

            {noreply, NewState#state{
                pending_tasks = NewPending,
                stats = NewStats
            }}
    end;

%% Worker process exited
handle_info({'EXIT', Pid, Reason}, State) ->
    case find_worker_by_pid(Pid, State) of
        {ok, Worker} ->
            WorkerId = Worker#swf_swarm_worker.id,
            ?LOG_WARNING("Worker ~p exited: ~p", [WorkerId, Reason]),
            {ok, NewState} = do_terminate_worker(WorkerId, State),

            %% Fail any pending tasks for this worker
            FinalState = fail_pending_tasks_for_worker(WorkerId, Reason, NewState),

            {noreply, FinalState};
        not_found ->
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(Reason, State) ->
    %% Cancel health timer
    case State#state.health_timer of
        undefined -> ok;
        Ref -> erlang:cancel_timer(Ref)
    end,

    %% Terminate all workers
    AllWorkers = lists:flatten(maps:values(State#state.worker_pools)),
    lists:foreach(
        fun(#swf_swarm_worker{pid = Pid}) when is_pid(Pid) ->
            exit(Pid, shutdown);
           (_) ->
            ok
        end,
        AllWorkers
    ),

    %% Clean up ETS
    ets:delete(State#state.worker_index),

    ?LOG_INFO("swf_swarm_coordinator terminating: ~p", [Reason]),
    ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions - Worker Management
%%====================================================================

-spec spawn_initial_workers(#state{}) -> #state{}.
spawn_initial_workers(State) ->
    maps:fold(
        fun(Type, Count, AccState) ->
            lists:foldl(
                fun(_, S) ->
                    case do_spawn_worker(Type, #{}, S) of
                        {ok, _WorkerId, NewS} -> NewS;
                        {error, _} -> S
                    end
                end,
                AccState,
                lists:seq(1, Count)
            )
        end,
        State,
        State#state.target_counts
    ).

-spec do_spawn_worker(swarm_worker_type(), map(), #state{}) ->
    {ok, worker_id(), #state{}} | {error, term()}.
do_spawn_worker(Type, WorkerConfig, State) ->
    WorkerId = generate_worker_id(Type),

    %% Create the worker process
    %% In production, this would start a proper supervised worker via swf_swarm_worker_sup
    case start_worker_process(Type, WorkerId, WorkerConfig) of
        {ok, Pid} ->
            %% Link to monitor the worker
            link(Pid),

            Metrics = #swf_worker_metrics{
                tasks_completed = 0,
                tasks_failed = 0,
                avg_duration_ms = 0.0,
                proposals_accepted = 0,
                proposals_rejected = 0,
                last_active = erlang:system_time(millisecond)
            },

            Worker = #swf_swarm_worker{
                id = WorkerId,
                type = Type,
                status = idle,
                current_task = undefined,
                metrics = Metrics,
                config = WorkerConfig,
                pid = Pid
            },

            %% Add to pool
            CurrentPool = maps:get(Type, State#state.worker_pools, []),
            NewPools = maps:put(Type, [Worker | CurrentPool], State#state.worker_pools),

            %% Add to index
            ets:insert(State#state.worker_index, {WorkerId, Worker}),

            %% Update stats
            Stats = State#state.stats,
            NewStats = Stats#{
                workers_spawned => maps:get(workers_spawned, Stats, 0) + 1
            },

            ?LOG_INFO("Spawned worker: ~p (type: ~p, pid: ~p)", [WorkerId, Type, Pid]),

            {ok, WorkerId, State#state{
                worker_pools = NewPools,
                stats = NewStats
            }};
        {error, Reason} ->
            {error, Reason}
    end.

-spec start_worker_process(swarm_worker_type(), worker_id(), map()) ->
    {ok, pid()} | {error, term()}.
start_worker_process(Type, WorkerId, Config) ->
    %% Spawn a lightweight worker process
    %% In production, use proper supervision via swf_swarm_worker_sup:start_worker/3
    Coordinator = self(),
    Pid = spawn_link(fun() ->
        worker_loop(Type, WorkerId, Config, Coordinator)
    end),
    {ok, Pid}.

-spec worker_loop(swarm_worker_type(), worker_id(), map(), pid()) -> no_return().
worker_loop(Type, WorkerId, Config, Coordinator) ->
    receive
        {execute_task, TaskId, Payload} ->
            %% Execute the task based on worker type
            StartTime = erlang:monotonic_time(millisecond),
            Result = execute_worker_task(Type, Payload, Config),
            Duration = erlang:monotonic_time(millisecond) - StartTime,

            %% Report result back to coordinator
            gen_server:cast(Coordinator, {task_result, TaskId, WorkerId, #{
                status => case Result of
                    {ok, _} -> completed;
                    {error, _} -> failed
                end,
                result => Result,
                duration_ms => Duration
            }}),

            worker_loop(Type, WorkerId, Config, Coordinator);

        {health_ping, From, Ref} ->
            %% Respond to health check
            From ! {health_pong, Ref, WorkerId},
            worker_loop(Type, WorkerId, Config, Coordinator);

        shutdown ->
            ok;

        _ ->
            worker_loop(Type, WorkerId, Config, Coordinator)
    end.

-spec execute_worker_task(swarm_worker_type(), term(), map()) -> {ok, term()} | {error, term()}.
execute_worker_task(conformance_checker, Payload, _Config) ->
    %% Check conformance of a case against a net
    #{case_events := Events, net := Net} = Payload,
    Result = check_conformance(Events, Net),
    {ok, Result};

execute_worker_task(replay_scorer, Payload, _Config) ->
    %% Score replay of event log
    #{events := Events, net := Net} = Payload,
    Score = score_replay(Events, Net),
    {ok, Score};

execute_worker_task(patch_proposer, Payload, _Config) ->
    %% Propose patches based on deviations
    #{deviations := Deviations, net := Net} = Payload,
    Patches = propose_patches(Deviations, Net),
    {ok, Patches};

execute_worker_task(pattern_miner, Payload, _Config) ->
    %% Mine patterns from event logs
    #{events := Events} = Payload,
    Patterns = mine_patterns(Events),
    {ok, Patterns};

execute_worker_task(anomaly_detector, Payload, _Config) ->
    %% Detect anomalies in behavior
    #{events := Events, baseline := Baseline} = Payload,
    Anomalies = detect_anomalies(Events, Baseline),
    {ok, Anomalies};

execute_worker_task(performance_analyzer, Payload, _Config) ->
    %% Analyze performance metrics
    #{metrics := Metrics} = Payload,
    Analysis = analyze_performance(Metrics),
    {ok, Analysis};

execute_worker_task(bottleneck_finder, Payload, _Config) ->
    %% Find bottlenecks in workflow
    #{events := Events, net := Net} = Payload,
    Bottlenecks = find_bottlenecks(Events, Net),
    {ok, Bottlenecks};

execute_worker_task(prediction_engine, Payload, _Config) ->
    %% Predict case outcomes
    #{case_state := CaseState, model := Model} = Payload,
    Prediction = predict_outcome(CaseState, Model),
    {ok, Prediction};

execute_worker_task(_Type, _Payload, _Config) ->
    {error, unknown_worker_type}.

-spec do_terminate_worker(worker_id(), #state{}) -> {ok, #state{}} | {error, not_found}.
do_terminate_worker(WorkerId, State) ->
    case ets:lookup(State#state.worker_index, WorkerId) of
        [{WorkerId, Worker}] ->
            %% Stop the worker process
            case Worker#swf_swarm_worker.pid of
                Pid when is_pid(Pid) ->
                    unlink(Pid),
                    Pid ! shutdown;
                _ ->
                    ok
            end,

            %% Remove from pool
            Type = Worker#swf_swarm_worker.type,
            CurrentPool = maps:get(Type, State#state.worker_pools, []),
            NewPool = lists:filter(
                fun(W) -> W#swf_swarm_worker.id =/= WorkerId end,
                CurrentPool
            ),
            NewPools = maps:put(Type, NewPool, State#state.worker_pools),

            %% Remove from index
            ets:delete(State#state.worker_index, WorkerId),

            %% Update stats
            Stats = State#state.stats,
            NewStats = Stats#{
                workers_terminated => maps:get(workers_terminated, Stats, 0) + 1
            },

            ?LOG_INFO("Terminated worker: ~p", [WorkerId]),

            {ok, State#state{
                worker_pools = NewPools,
                stats = NewStats
            }};
        [] ->
            {error, not_found}
    end.

-spec adjust_pool_size(swarm_worker_type(), #state{}) -> #state{}.
adjust_pool_size(Type, State) ->
    TargetCount = maps:get(Type, State#state.target_counts, 0),
    CurrentPool = maps:get(Type, State#state.worker_pools, []),
    CurrentCount = length(CurrentPool),

    if
        CurrentCount < TargetCount ->
            %% Need to spawn more workers
            lists:foldl(
                fun(_, S) ->
                    case do_spawn_worker(Type, #{}, S) of
                        {ok, _WorkerId, NewS} -> NewS;
                        {error, _} -> S
                    end
                end,
                State,
                lists:seq(1, TargetCount - CurrentCount)
            );
        CurrentCount > TargetCount ->
            %% Need to terminate excess workers (prefer idle ones)
            {IdleWorkers, WorkingWorkers} = lists:partition(
                fun(W) -> W#swf_swarm_worker.status =:= idle end,
                CurrentPool
            ),
            ToTerminate = CurrentCount - TargetCount,

            %% Terminate idle workers first
            WorkersToKill = lists:sublist(IdleWorkers ++ WorkingWorkers, ToTerminate),
            lists:foldl(
                fun(Worker, S) ->
                    case do_terminate_worker(Worker#swf_swarm_worker.id, S) of
                        {ok, NewS} -> NewS;
                        {error, _} -> S
                    end
                end,
                State,
                WorkersToKill
            );
        true ->
            State
    end.

%%====================================================================
%% Internal functions - Task Management
%%====================================================================

-spec do_submit_task(task(), {pid(), term()} | undefined, #state{}) ->
    {ok, task_id(), #state{}} | {error, term()}.
do_submit_task(Task, From, State) ->
    #{id := TaskId, type := Type, payload := Payload, timeout_ms := Timeout} = Task,

    %% Select a worker using load balancing
    case select_worker(Type, State) of
        {ok, Worker, NewState} ->
            WorkerId = Worker#swf_swarm_worker.id,
            Pid = Worker#swf_swarm_worker.pid,

            %% Send task to worker
            Pid ! {execute_task, TaskId, Payload},

            %% Set up timeout
            TimeoutRef = erlang:send_after(Timeout, self(), {task_timeout, TaskId}),

            %% Track pending task
            PendingTask = #pending_task{
                task_id = TaskId,
                worker_id = WorkerId,
                from = From,
                start_time = erlang:monotonic_time(millisecond),
                timeout_ref = TimeoutRef
            },
            NewPending = maps:put(TaskId, PendingTask, NewState#state.pending_tasks),

            %% Update worker status
            UpdatedWorker = Worker#swf_swarm_worker{
                status = working,
                current_task = TaskId
            },
            ets:insert(NewState#state.worker_index, {WorkerId, UpdatedWorker}),

            %% Update pool
            Pool = maps:get(Type, NewState#state.worker_pools, []),
            NewPool = lists:map(
                fun(W) when W#swf_swarm_worker.id =:= WorkerId -> UpdatedWorker;
                   (W) -> W
                end,
                Pool
            ),
            NewPools = maps:put(Type, NewPool, NewState#state.worker_pools),

            %% Update stats
            Stats = NewState#state.stats,
            NewStats = Stats#{
                tasks_submitted => maps:get(tasks_submitted, Stats, 0) + 1
            },

            {ok, TaskId, NewState#state{
                pending_tasks = NewPending,
                worker_pools = NewPools,
                stats = NewStats
            }};
        {error, no_workers} ->
            {error, no_workers_available}
    end.

-spec select_worker(swarm_worker_type(), #state{}) ->
    {ok, #swf_swarm_worker{}, #state{}} | {error, no_workers}.
select_worker(Type, State) ->
    Pool = maps:get(Type, State#state.worker_pools, []),

    %% Filter to idle workers
    IdleWorkers = [W || W <- Pool, W#swf_swarm_worker.status =:= idle],

    case IdleWorkers of
        [] ->
            %% No idle workers, try least loaded
            case Pool of
                [] -> {error, no_workers};
                _ ->
                    %% Round-robin as fallback
                    Counter = maps:get(Type, State#state.rr_counters, 0),
                    Index = Counter rem length(Pool),
                    Worker = lists:nth(Index + 1, Pool),
                    NewCounters = maps:put(Type, Counter + 1, State#state.rr_counters),
                    {ok, Worker, State#state{rr_counters = NewCounters}}
            end;
        _ ->
            %% Select least loaded idle worker (by tasks completed for even distribution)
            Sorted = lists:sort(
                fun(#swf_swarm_worker{metrics = M1}, #swf_swarm_worker{metrics = M2}) ->
                    M1#swf_worker_metrics.tasks_completed =< M2#swf_worker_metrics.tasks_completed
                end,
                IdleWorkers
            ),
            {ok, hd(Sorted), State}
    end.

-spec handle_task_result(task_id(), worker_id(), map(), #state{}) -> #state{}.
handle_task_result(TaskId, WorkerId, Result, State) ->
    case maps:get(TaskId, State#state.pending_tasks, undefined) of
        undefined ->
            %% Task already timed out or unknown
            State;
        #pending_task{from = From, timeout_ref = TimeoutRef, start_time = StartTime} ->
            %% Cancel timeout
            erlang:cancel_timer(TimeoutRef),

            Duration = erlang:monotonic_time(millisecond) - StartTime,

            %% Build task result
            TaskResult = #{
                task_id => TaskId,
                worker_id => WorkerId,
                status => maps:get(status, Result, completed),
                result => maps:get(result, Result, undefined),
                error => maps:get(error, Result, undefined),
                duration_ms => Duration
            },

            %% Update worker metrics and status
            NewState = case maps:get(status, Result, completed) of
                completed ->
                    update_worker_task_completed(WorkerId, Duration, State);
                failed ->
                    update_worker_task_failed(WorkerId, State)
            end,

            %% Reply if synchronous
            case From of
                undefined -> ok;
                _ -> gen_server:reply(From, TaskResult)
            end,

            %% Remove from pending
            NewPending = maps:remove(TaskId, NewState#state.pending_tasks),

            %% Update stats
            Stats = NewState#state.stats,
            StatusKey = case maps:get(status, Result, completed) of
                completed -> tasks_completed;
                failed -> tasks_failed
            end,
            NewStats = Stats#{
                StatusKey => maps:get(StatusKey, Stats, 0) + 1
            },

            NewState#state{
                pending_tasks = NewPending,
                stats = NewStats
            }
    end.

-spec update_worker_task_completed(worker_id(), non_neg_integer(), #state{}) -> #state{}.
update_worker_task_completed(WorkerId, Duration, State) ->
    case ets:lookup(State#state.worker_index, WorkerId) of
        [{WorkerId, Worker}] ->
            Metrics = Worker#swf_swarm_worker.metrics,
            OldCount = Metrics#swf_worker_metrics.tasks_completed,
            OldAvg = Metrics#swf_worker_metrics.avg_duration_ms,

            %% Compute new running average
            NewCount = OldCount + 1,
            NewAvg = ((OldAvg * OldCount) + Duration) / NewCount,

            NewMetrics = Metrics#swf_worker_metrics{
                tasks_completed = NewCount,
                avg_duration_ms = NewAvg,
                last_active = erlang:system_time(millisecond)
            },

            NewWorker = Worker#swf_swarm_worker{
                status = idle,
                current_task = undefined,
                metrics = NewMetrics
            },

            ets:insert(State#state.worker_index, {WorkerId, NewWorker}),
            update_worker_in_pool(NewWorker, State);
        [] ->
            State
    end.

-spec update_worker_task_failed(worker_id(), #state{}) -> #state{}.
update_worker_task_failed(WorkerId, State) ->
    case ets:lookup(State#state.worker_index, WorkerId) of
        [{WorkerId, Worker}] ->
            Metrics = Worker#swf_swarm_worker.metrics,
            NewMetrics = Metrics#swf_worker_metrics{
                tasks_failed = Metrics#swf_worker_metrics.tasks_failed + 1,
                last_active = erlang:system_time(millisecond)
            },

            NewWorker = Worker#swf_swarm_worker{
                status = idle,
                current_task = undefined,
                metrics = NewMetrics
            },

            ets:insert(State#state.worker_index, {WorkerId, NewWorker}),
            update_worker_in_pool(NewWorker, State);
        [] ->
            State
    end.

-spec update_worker_in_pool(#swf_swarm_worker{}, #state{}) -> #state{}.
update_worker_in_pool(Worker, State) ->
    Type = Worker#swf_swarm_worker.type,
    WorkerId = Worker#swf_swarm_worker.id,
    Pool = maps:get(Type, State#state.worker_pools, []),
    NewPool = lists:map(
        fun(W) when W#swf_swarm_worker.id =:= WorkerId -> Worker;
           (W) -> W
        end,
        Pool
    ),
    State#state{worker_pools = maps:put(Type, NewPool, State#state.worker_pools)}.

-spec update_worker_status(worker_id(), atom(), #state{}) -> #state{}.
update_worker_status(WorkerId, Status, State) ->
    case ets:lookup(State#state.worker_index, WorkerId) of
        [{WorkerId, Worker}] ->
            NewWorker = Worker#swf_swarm_worker{status = Status},
            ets:insert(State#state.worker_index, {WorkerId, NewWorker}),
            update_worker_in_pool(NewWorker, State);
        [] ->
            State
    end.

-spec fail_pending_tasks_for_worker(worker_id(), term(), #state{}) -> #state{}.
fail_pending_tasks_for_worker(WorkerId, Reason, State) ->
    %% Find all pending tasks for this worker
    TasksToFail = maps:filter(
        fun(_TaskId, #pending_task{worker_id = WId}) ->
            WId =:= WorkerId
        end,
        State#state.pending_tasks
    ),

    %% Fail each task
    maps:fold(
        fun(TaskId, #pending_task{from = From, timeout_ref = TimeoutRef}, S) ->
            erlang:cancel_timer(TimeoutRef),
            case From of
                undefined -> ok;
                _ -> gen_server:reply(From, {error, {worker_died, Reason}})
            end,
            NewPending = maps:remove(TaskId, S#state.pending_tasks),
            Stats = S#state.stats,
            NewStats = Stats#{
                tasks_failed => maps:get(tasks_failed, Stats, 0) + 1
            },
            S#state{pending_tasks = NewPending, stats = NewStats}
        end,
        State,
        TasksToFail
    ).

%%====================================================================
%% Internal functions - Result Aggregation
%%====================================================================

-spec do_aggregate([task_result()], aggregation_strategy()) -> {ok, term()} | {error, term()}.
do_aggregate([], _Strategy) ->
    {error, no_results};

do_aggregate(Results, first) ->
    {ok, hd(Results)};

do_aggregate(Results, all) ->
    {ok, Results};

do_aggregate(Results, majority) ->
    %% Group by result value and pick the most common
    Grouped = lists:foldl(
        fun(#{result := R}, Acc) ->
            Count = maps:get(R, Acc, 0),
            maps:put(R, Count + 1, Acc)
        end,
        #{},
        Results
    ),
    {MajorityResult, _Count} = lists:foldl(
        fun({R, C}, {BestR, BestC}) ->
            if C > BestC -> {R, C};
               true -> {BestR, BestC}
            end
        end,
        {undefined, 0},
        maps:to_list(Grouped)
    ),
    {ok, MajorityResult};

do_aggregate(Results, best_score) ->
    %% Assume results have a score field, pick highest
    Sorted = lists:sort(
        fun(#{result := {ok, #{score := S1}}}, #{result := {ok, #{score := S2}}}) ->
            S1 >= S2;
           (_, _) ->
            false
        end,
        Results
    ),
    {ok, hd(Sorted)};

do_aggregate(Results, {custom, Fun}) when is_function(Fun, 1) ->
    {ok, Fun(Results)}.

%%====================================================================
%% Internal functions - Health Checking
%%====================================================================

-spec do_health_check(#state{}) -> {[worker_id()], [worker_id()]}.
do_health_check(State) ->
    AllWorkers = lists:flatten(maps:values(State#state.worker_pools)),

    %% Ping each worker and collect responses
    Ref = make_ref(),
    Self = self(),

    lists:foreach(
        fun(#swf_swarm_worker{pid = Pid}) when is_pid(Pid) ->
            Pid ! {health_ping, Self, Ref};
           (_) ->
            ok
        end,
        AllWorkers
    ),

    %% Collect responses with timeout
    Healthy = collect_health_responses(Ref, length(AllWorkers), 1000, []),

    %% Workers that didn't respond are unhealthy
    AllIds = [W#swf_swarm_worker.id || W <- AllWorkers],
    Unhealthy = AllIds -- Healthy,

    {Healthy, Unhealthy}.

-spec collect_health_responses(reference(), non_neg_integer(), non_neg_integer(), [worker_id()]) ->
    [worker_id()].
collect_health_responses(_Ref, 0, _Timeout, Acc) ->
    Acc;
collect_health_responses(_Ref, _Remaining, Timeout, Acc) when Timeout =< 0 ->
    Acc;
collect_health_responses(Ref, Remaining, Timeout, Acc) ->
    StartTime = erlang:monotonic_time(millisecond),
    receive
        {health_pong, Ref, WorkerId} ->
            Elapsed = erlang:monotonic_time(millisecond) - StartTime,
            collect_health_responses(Ref, Remaining - 1, Timeout - Elapsed, [WorkerId | Acc])
    after Timeout ->
        Acc
    end.

-spec find_worker_by_pid(pid(), #state{}) -> {ok, #swf_swarm_worker{}} | not_found.
find_worker_by_pid(Pid, State) ->
    AllWorkers = lists:flatten(maps:values(State#state.worker_pools)),
    case lists:filter(
        fun(#swf_swarm_worker{pid = P}) -> P =:= Pid end,
        AllWorkers
    ) of
        [Worker] -> {ok, Worker};
        [] -> not_found
    end.

%%====================================================================
%% Internal functions - Promotion Engine Integration
%%====================================================================

-spec maybe_promote_patch(#swf_patch{}) -> ok.
maybe_promote_patch(Patch) ->
    %% Try to call swf_promotion_engine if available
    case erlang:whereis(swf_promotion_engine) of
        undefined ->
            ?LOG_DEBUG("Promotion engine not available, patch stored only: ~p",
                      [Patch#swf_patch.id]),
            ok;
        _Pid ->
            %% Submit patch for evaluation
            try
                swf_promotion_engine:submit_patch(Patch)
            catch
                _:_ ->
                    ?LOG_WARNING("Failed to submit patch to promotion engine: ~p",
                               [Patch#swf_patch.id]),
                    ok
            end
    end.

%%====================================================================
%% Internal functions - Worker Task Implementations (Stubs)
%%====================================================================

%% These are placeholder implementations - real ones would be more sophisticated

-spec check_conformance([#swf_event{}], #swf_net{}) -> #swf_conformance_result{}.
check_conformance(_Events, _Net) ->
    %% Placeholder conformance checking
    #swf_conformance_result{
        case_id = <<"placeholder">>,
        net_id = <<"placeholder">>,
        fitness = 0.95,
        precision = 0.90,
        generalization = 0.85,
        simplicity = 0.80,
        deviations = [],
        computed_at = erlang:system_time(millisecond)
    }.

-spec score_replay([#swf_event{}], #swf_net{}) -> float().
score_replay(_Events, _Net) ->
    %% Placeholder replay scoring
    0.92.

-spec propose_patches([#swf_deviation{}], #swf_net{}) -> [#swf_patch{}].
propose_patches(_Deviations, _Net) ->
    %% Placeholder patch proposal
    [].

-spec mine_patterns([#swf_event{}]) -> [map()].
mine_patterns(_Events) ->
    %% Placeholder pattern mining
    [].

-spec detect_anomalies([#swf_event{}], map()) -> [map()].
detect_anomalies(_Events, _Baseline) ->
    %% Placeholder anomaly detection
    [].

-spec analyze_performance(map()) -> map().
analyze_performance(_Metrics) ->
    %% Placeholder performance analysis
    #{}.

-spec find_bottlenecks([#swf_event{}], #swf_net{}) -> [map()].
find_bottlenecks(_Events, _Net) ->
    %% Placeholder bottleneck finding
    [].

-spec predict_outcome(map(), map()) -> map().
predict_outcome(_CaseState, _Model) ->
    %% Placeholder prediction
    #{}.

%%====================================================================
%% Internal functions - Utilities
%%====================================================================

-spec generate_worker_id(swarm_worker_type()) -> worker_id().
generate_worker_id(Type) ->
    Timestamp = erlang:system_time(microsecond),
    Random = rand:uniform(16#FFFFFF),
    TypeBin = atom_to_binary(Type, utf8),
    iolist_to_binary([
        TypeBin, <<"-">>,
        integer_to_binary(Timestamp, 16), <<"-">>,
        integer_to_binary(Random, 16)
    ]).

-spec generate_task_id() -> task_id().
generate_task_id() ->
    Timestamp = erlang:system_time(microsecond),
    Random = rand:uniform(16#FFFFFF),
    iolist_to_binary([
        <<"task-">>,
        integer_to_binary(Timestamp, 16), <<"-">>,
        integer_to_binary(Random, 16)
    ]).
