# erlmcp_flow_agent Design Specification v1.0.0

**Author**: Erlang OTP Developer + Erlang Architect
**Date**: 2026-02-02
**Status**: Specification Complete
**Module**: `erlmcp_flow_agent`

---

## Executive Summary

The `erlmcp_flow_agent` is a gen_server that implements an autonomous agent with:
- **State Machine**: idle → assigned → executing → done
- **Non-blocking init**: Async initialization via `{continue, connect}`
- **Process Isolation**: Process-per-agent, no shared state
- **Error Recovery**: Exponential backoff with task requeue
- **Monitoring**: Health checks, metrics, distributed tracing
- **Message Protocols**: Defined interfaces with swarm_coordinator and consensus engines

**Key Features**:
- ✅ OTP 28.3.1 gen_server behavior
- ✅ Let-it-crash fault tolerance
- ✅ Request-ID correlation (UUID-based)
- ✅ O(1) state transitions
- ✅ Supervised by `erlmcp_flow_agent_sup`

---

## 1. State Machine Design

### 1.1 State Transitions

```
┌──────────┐
│   IDLE   │ ◄────────────────────────┐
└────┬─────┘                          │
     │ assign_task cast               │
     │                                │
     ▼                                │
┌──────────┐                          │
│ ASSIGNED │                          │
└────┬─────┘                          │
     │ execute_task internal          │
     │                                │
     ▼                                │
┌───────────┐                         │
│ EXECUTING │ ──► monitor task        │
└────┬──────┘     execution          │
     │                                │
     │ task_complete cast             │
     │                                │
     ▼                                │
┌──────────┐                          │
│   DONE   │ ──► report_result ───────┘
└──────────┘     transition to idle
```

### 1.2 State Record

```erlang
-module(erlmcp_flow_agent).
-behaviour(gen_server).

-include("erlmcp_flow.hrl").

%% Agent role types
-type agent_role() :: worker | specialist | scout | coordinator.

%% Agent state in state machine
-type agent_status() :: idle | assigned | executing | done | failed | paused.

%% Task record
-record(task, {
    id :: binary(),                      % UUID v4
    type :: binary(),                    % Task type (capability)
    priority :: low | normal | high,
    params :: map(),                     % Task parameters
    timeout :: pos_integer(),            % Timeout in ms
    retry_count = 0 :: non_neg_integer(),
    created_at :: erlang:timestamp(),
    deadline :: erlang:timestamp() | undefined,
    trace_context :: trace_context() | undefined
}).

%% Agent state record
-record(state, {
    id :: binary(),                      % Agent UUID
    role :: agent_role(),                % Agent role/type
    status = idle :: agent_status(),     % Current state in state machine

    %% Task management
    task_queue = queue:new() :: queue:queue(task()),  % Pending tasks (FIFO)
    executing_task :: task() | undefined,             % Currently executing task
    task_history = [] :: [task()],                    % Last 100 completed tasks

    %% Statistics
    stats = #{
        tasks_completed => 0,
        tasks_failed => 0,
        total_execution_time => 0,       % Microseconds
        avg_latency => 0,                % Microseconds
        success_rate => 1.0
    } :: map(),

    %% Monitoring
    health_check_timer :: reference() | undefined,
    heartbeat_interval = 5000 :: pos_integer(),  % 5 seconds
    last_heartbeat :: erlang:timestamp() | undefined,

    %% Error recovery
    retry_state = #{
        max_retries => 3,
        base_delay => 100,               % ms
        max_delay => 5000,               % ms
        backoff_multiplier => 2.0
    } :: map(),

    %% Coordination
    swarm_coordinator :: pid() | undefined,
    consensus_engine :: pid() | undefined,

    %% Metrics
    metrics_ets :: ets:tid() | undefined,

    %% Configuration
    config :: map()
}).

-type state() :: #state{}.
```

---

## 2. Callbacks Design

### 2.1 init/1 - Non-blocking Initialization

```erlang
-spec init([{agent_id, binary()} | {role, agent_role()} | {config, map()}]) ->
    {ok, state(), {continue, connect}}.

init(Args) ->
    process_flag(trap_exit, true),

    %% Extract configuration
    AgentId = proplists:get_value(agent_id, Args, generate_agent_id()),
    Role = proplists:get_value(role, Args, worker),
    Config = proplists:get_value(config, Args, #{}),

    %% Create metrics ETS table
    MetricsETS = ets:new(agent_metrics, [set, private]),

    %% Initialize state
    State = #state{
        id = AgentId,
        role = Role,
        status = idle,
        metrics_ets = MetricsETS,
        config = Config
    },

    logger:info("Agent ~p (~p) starting (async init)", [AgentId, Role]),

    %% Schedule async connection - won't block supervisor
    {ok, State, {continue, connect}}.
```

### 2.2 handle_continue/2 - Async Connection

```erlang
-spec handle_continue(connect, state()) -> {noreply, state()}.

handle_continue(connect, State) ->
    %% Connect to swarm coordinator
    SwarmCoordinator = whereis(erlmcp_flow_swarm_coordinator),
    ConsensusEngine = whereis(erlmcp_flow_consensus_engine),

    %% Register with registry
    ok = erlmcp_flow_registry:register_agent(
        State#state.id,
        self(),
        get_agent_capabilities(State#state.role)
    ),

    %% Start health check timer
    TimerRef = erlang:send_after(
        State#state.heartbeat_interval,
        self(),
        heartbeat
    ),

    %% Notify coordinator
    case SwarmCoordinator of
        undefined -> ok;
        Pid when is_pid(Pid) ->
            Pid ! {agent_ready, State#state.id, State#state.role, self()}
    end,

    NewState = State#state{
        swarm_coordinator = SwarmCoordinator,
        consensus_engine = ConsensusEngine,
        health_check_timer = TimerRef,
        last_heartbeat = erlang:timestamp()
    },

    logger:info("Agent ~p connected to swarm", [State#state.id]),

    {noreply, NewState}.
```

### 2.3 handle_call/3 - Synchronous Requests

```erlang
%% Get agent state
-spec handle_call(get_state, From, state()) -> {reply, map(), state()}.

handle_call(get_state, _From, State) ->
    Reply = #{
        id => State#state.id,
        role => State#state.role,
        status => State#state.status,
        queue_length => queue:len(State#state.task_queue),
        executing_task =>
            case State#state.executing_task of
                undefined -> null;
                Task -> Task#task.id
            end,
        stats => State#state.stats
    },
    {reply, Reply, State};

%% Cancel current task
handle_call(cancel_task, _From, #state{status = executing, executing_task = Task} = State) ->
    logger:warning("Agent ~p: Cancelling task ~p", [State#state.id, Task#task.id]),

    %% Send cancellation notification
    notify_task_cancelled(Task, State),

    %% Transition to idle
    NewState = transition_to_idle(State),

    {reply, ok, NewState};

handle_call(cancel_task, _From, State) ->
    {reply, {error, no_task_executing}, State};

%% Get agent metrics
handle_call(get_metrics, _From, State) ->
    Metrics = #{
        agent_id => State#state.id,
        role => State#state.role,
        status => State#state.status,
        stats => State#state.stats,
        queue_length => queue:len(State#state.task_queue),
        uptime => calculate_uptime(State)
    },
    {reply, {ok, Metrics}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.
```

### 2.4 handle_cast/2 - Asynchronous Messages

```erlang
%% Assign new task
-spec handle_cast({assign_task, task()}, state()) -> {noreply, state()}.

handle_cast({assign_task, Task}, #state{status = idle} = State) ->
    %% Immediate execution if idle
    logger:info("Agent ~p: Assigned task ~p (immediate)", [State#state.id, Task#task.id]),

    NewState = execute_task_async(Task, State),

    {noreply, NewState};

handle_cast({assign_task, Task}, State) ->
    %% Queue task if busy
    logger:info("Agent ~p: Queued task ~p", [State#state.id, Task#task.id]),

    NewQueue = queue:in(Task, State#state.task_queue),
    NewState = State#state{task_queue = NewQueue},

    {noreply, NewState};

%% Task completed successfully
handle_cast({task_complete, TaskId, Result},
            #state{status = executing, executing_task = Task} = State)
            when Task#task.id =:= TaskId ->

    logger:info("Agent ~p: Task ~p completed", [State#state.id, TaskId]),

    %% Update statistics
    ExecutionTime = timer:now_diff(erlang:timestamp(), Task#task.created_at),
    NewStats = update_stats_success(State#state.stats, ExecutionTime),

    %% Notify coordinator
    notify_task_complete(Task, Result, State),

    %% Store in history
    History = store_in_history(Task, State#state.task_history),

    %% Transition to done, then check queue
    NewState = State#state{
        status = done,
        executing_task = undefined,
        stats = NewStats,
        task_history = History
    },

    %% Process next task if available
    FinalState = process_next_task(NewState),

    {noreply, FinalState};

%% Task failed
handle_cast({task_failed, TaskId, Reason},
            #state{status = executing, executing_task = Task} = State)
            when Task#task.id =:= TaskId ->

    logger:error("Agent ~p: Task ~p failed: ~p", [State#state.id, TaskId, Reason]),

    %% Update statistics
    NewStats = update_stats_failure(State#state.stats),

    %% Retry logic
    case should_retry(Task, State#state.retry_state) of
        {true, Delay} ->
            %% Requeue with backoff
            logger:info("Agent ~p: Retrying task ~p after ~pms",
                       [State#state.id, TaskId, Delay]),

            erlang:send_after(Delay, self(), {retry_task, Task#task{retry_count = Task#task.retry_count + 1}}),

            NewState = State#state{
                status = idle,
                executing_task = undefined,
                stats = NewStats
            },

            {noreply, NewState};

        false ->
            %% Max retries exhausted
            logger:error("Agent ~p: Task ~p exhausted retries", [State#state.id, TaskId]),

            notify_task_failed(Task, Reason, State),

            NewState = State#state{
                status = failed,
                executing_task = undefined,
                stats = NewStats
            },

            %% Transition to idle and process next
            FinalState = process_next_task(NewState#state{status = idle}),

            {noreply, FinalState}
    end;

%% Agent crash notification from peer
handle_cast({agent_crash, AgentId, Reason}, State) ->
    logger:warning("Agent ~p: Peer agent ~p crashed: ~p",
                  [State#state.id, AgentId, Reason]),

    %% Optionally take over failed agent's tasks via coordinator
    case State#state.swarm_coordinator of
        undefined -> ok;
        Pid -> Pid ! {request_failover_tasks, AgentId, self()}
    end,

    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.
```

### 2.5 handle_info/2 - Timeouts and Monitoring

```erlang
%% Heartbeat timer
-spec handle_info(heartbeat, state()) -> {noreply, state()}.

handle_info(heartbeat, State) ->
    %% Send heartbeat to coordinator
    case State#state.swarm_coordinator of
        undefined -> ok;
        Pid -> Pid ! {agent_heartbeat, State#state.id, State#state.status, self()}
    end,

    %% Update metrics
    update_health_metrics(State),

    %% Schedule next heartbeat
    NewTimer = erlang:send_after(
        State#state.heartbeat_interval,
        self(),
        heartbeat
    ),

    NewState = State#state{
        health_check_timer = NewTimer,
        last_heartbeat = erlang:timestamp()
    },

    {noreply, NewState};

%% Task timeout
handle_info({task_timeout, TaskId},
            #state{status = executing, executing_task = Task} = State)
            when Task#task.id =:= TaskId ->

    logger:error("Agent ~p: Task ~p timed out", [State#state.id, TaskId]),

    %% Treat as failure
    self() ! {task_failed, TaskId, timeout},

    {noreply, State};

%% Retry task after backoff
handle_info({retry_task, Task}, #state{status = idle} = State) ->
    logger:info("Agent ~p: Retrying task ~p", [State#state.id, Task#task.id]),

    NewState = execute_task_async(Task, State),

    {noreply, NewState};

%% Peer message from another agent
handle_info({peer_message, FromAgentId, Message}, State) ->
    logger:debug("Agent ~p: Received peer message from ~p: ~p",
                [State#state.id, FromAgentId, Message]),

    %% Handle peer coordination messages
    handle_peer_message(FromAgentId, Message, State);

%% Monitor down (coordinator or consensus engine crashed)
handle_info({'DOWN', _Ref, process, Pid, Reason}, State) ->
    logger:warning("Agent ~p: Monitored process ~p died: ~p",
                  [State#state.id, Pid, Reason]),

    %% Attempt reconnection
    self() ! reconnect,

    {noreply, State};

%% Reconnect to coordinator
handle_info(reconnect, State) ->
    logger:info("Agent ~p: Attempting reconnection", [State#state.id]),

    %% Trigger async reconnection
    {noreply, State, {continue, connect}};

handle_info(_Info, State) ->
    {noreply, State}.
```

### 2.6 terminate/2 - Cleanup

```erlang
-spec terminate(term(), state()) -> ok.

terminate(Reason, State) ->
    logger:info("Agent ~p terminating: ~p", [State#state.id, Reason]),

    %% Notify coordinator
    case State#state.swarm_coordinator of
        undefined -> ok;
        Pid -> Pid ! {agent_terminating, State#state.id, Reason}
    end,

    %% Unregister from registry
    erlmcp_flow_registry:unregister_agent(State#state.id),

    %% Cancel timers
    case State#state.health_check_timer of
        undefined -> ok;
        Ref -> erlang:cancel_timer(Ref)
    end,

    %% Clean up ETS
    case State#state.metrics_ets of
        undefined -> ok;
        ETS -> ets:delete(ETS)
    end,

    ok.
```

### 2.7 code_change/3 - Hot Code Reloading

```erlang
-spec code_change(term(), state(), term()) -> {ok, state()}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
```

---

## 3. Error Recovery Design

### 3.1 Retry Logic with Exponential Backoff

```erlang
%% Check if task should be retried
-spec should_retry(task(), map()) -> {true, Delay :: pos_integer()} | false.

should_retry(#task{retry_count = RetryCount}, #{max_retries := MaxRetries})
    when RetryCount >= MaxRetries ->
    false;

should_retry(#task{retry_count = RetryCount},
             #{base_delay := BaseDelay, max_delay := MaxDelay, backoff_multiplier := Multiplier}) ->
    %% Exponential backoff: BaseDelay * Multiplier^RetryCount
    Delay = trunc(BaseDelay * math:pow(Multiplier, RetryCount)),
    ClampedDelay = min(Delay, MaxDelay),

    %% Add jitter (±10%) to prevent thundering herd
    Jitter = trunc(ClampedDelay * 0.1 * (rand:uniform() - 0.5)),
    FinalDelay = max(1, ClampedDelay + Jitter),

    {true, FinalDelay}.
```

### 3.2 Task Requeue Strategy

```erlang
%% Requeue failed task with updated retry count
-spec requeue_task(task(), state()) -> state().

requeue_task(Task, State) ->
    UpdatedTask = Task#task{retry_count = Task#task.retry_count + 1},

    %% Add to front of queue for priority retry
    NewQueue = queue:in_r(UpdatedTask, State#state.task_queue),

    State#state{task_queue = NewQueue}.
```

---

## 4. Message Protocols

### 4.1 Agent ↔ Swarm Coordinator Messages

```erlang
%% Agent → Coordinator
{agent_ready, AgentId :: binary(), Role :: agent_role(), Pid :: pid()}
{agent_heartbeat, AgentId :: binary(), Status :: agent_status(), Pid :: pid()}
{task_complete, TaskId :: binary(), Result :: term(), AgentId :: binary()}
{task_failed, TaskId :: binary(), Reason :: term(), AgentId :: binary()}
{agent_terminating, AgentId :: binary(), Reason :: term()}
{request_failover_tasks, FailedAgentId :: binary(), RequesterPid :: pid()}

%% Coordinator → Agent
{assign_task, Task :: task()}
{cancel_task, TaskId :: binary()}
{pause_agent}
{resume_agent}
{update_config, NewConfig :: map()}
```

### 4.2 Agent ↔ Consensus Engine Messages

```erlang
%% Agent → Consensus Engine
{propose_task_assignment, TaskId :: binary(), AgentId :: binary(), Priority :: pos_integer()}
{vote_for_leader, CandidateId :: binary(), Term :: pos_integer()}
{acknowledge_commit, TaskId :: binary(), AgentId :: binary()}

%% Consensus Engine → Agent
{task_committed, TaskId :: binary(), CommitIndex :: non_neg_integer()}
{leader_elected, LeaderId :: binary(), Term :: pos_integer()}
{consensus_timeout, TaskId :: binary()}
```

### 4.3 Agent ↔ Agent (Peer) Messages

```erlang
%% Peer coordination
{peer_message, FromAgentId :: binary(), Message :: term()}
{peer_request, RequestId :: binary(), Request :: term()}
{peer_response, RequestId :: binary(), Response :: term()}
{peer_status_update, AgentId :: binary(), Status :: agent_status()}
```

---

## 5. Monitoring & Health Checks

### 5.1 Health Metrics

```erlang
-spec update_health_metrics(state()) -> ok.

update_health_metrics(State) ->
    Metrics = #{
        agent_id => State#state.id,
        status => State#state.status,
        queue_length => queue:len(State#state.task_queue),
        tasks_completed => maps:get(tasks_completed, State#state.stats),
        tasks_failed => maps:get(tasks_failed, State#state.stats),
        success_rate => maps:get(success_rate, State#state.stats),
        avg_latency => maps:get(avg_latency, State#state.stats),
        timestamp => erlang:system_time(millisecond)
    },

    %% Store in ETS
    case State#state.metrics_ets of
        undefined -> ok;
        ETS -> ets:insert(ETS, {health_metrics, Metrics})
    end,

    %% Report to coordinator
    case State#state.swarm_coordinator of
        undefined -> ok;
        Pid -> Pid ! {agent_metrics, State#state.id, Metrics}
    end,

    ok.
```

### 5.2 Heartbeat Timeout Detection

```erlang
%% Coordinator monitors agent heartbeats
%% If no heartbeat for 3x interval, consider agent dead

-define(HEARTBEAT_TIMEOUT_MULTIPLIER, 3).

is_agent_alive(AgentId, LastHeartbeat, HeartbeatInterval) ->
    Now = erlang:timestamp(),
    TimeSinceHeartbeat = timer:now_diff(Now, LastHeartbeat) / 1000,  % ms

    Timeout = HeartbeatInterval * ?HEARTBEAT_TIMEOUT_MULTIPLIER,

    TimeSinceHeartbeat < Timeout.
```

---

## 6. Statistics Collection

```erlang
%% Update statistics on successful task completion
-spec update_stats_success(map(), ExecutionTime :: pos_integer()) -> map().

update_stats_success(Stats, ExecutionTime) ->
    TasksCompleted = maps:get(tasks_completed, Stats) + 1,
    TotalTime = maps:get(total_execution_time, Stats) + ExecutionTime,
    AvgLatency = TotalTime div max(1, TasksCompleted),

    TotalTasks = TasksCompleted + maps:get(tasks_failed, Stats),
    SuccessRate = TasksCompleted / max(1, TotalTasks),

    Stats#{
        tasks_completed => TasksCompleted,
        total_execution_time => TotalTime,
        avg_latency => AvgLatency,
        success_rate => SuccessRate
    }.

%% Update statistics on task failure
-spec update_stats_failure(map()) -> map().

update_stats_failure(Stats) ->
    TasksFailed = maps:get(tasks_failed, Stats) + 1,
    TotalTasks = maps:get(tasks_completed, Stats) + TasksFailed,
    SuccessRate = maps:get(tasks_completed, Stats) / max(1, TotalTasks),

    Stats#{
        tasks_failed => TasksFailed,
        success_rate => SuccessRate
    }.
```

---

## 7. Isolation Guarantees

### 7.1 Process-per-Agent

- ✅ Each agent runs in isolated process
- ✅ No shared ETS tables (private metrics only)
- ✅ No global state
- ✅ Failure isolated to single agent

### 7.2 Supervision Strategy

```erlang
%% erlmcp_flow_agent_sup.erl
-module(erlmcp_flow_agent_sup).
-behaviour(supervisor).

init([]) ->
    SupFlags = #{
        strategy => simple_one_for_one,  % Dynamic agent pool
        intensity => 5,
        period => 60
    },

    AgentSpec = #{
        id => erlmcp_flow_agent,
        start => {erlmcp_flow_agent, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [erlmcp_flow_agent]
    },

    {ok, {SupFlags, [AgentSpec]}}.

%% Start new agent
-spec start_agent(agent_role(), map()) -> {ok, pid()} | {error, term()}.

start_agent(Role, Config) ->
    supervisor:start_child(?MODULE, [
        {agent_id, generate_agent_id()},
        {role, Role},
        {config, Config}
    ]).
```

---

## 8. Testing Strategy (Chicago TDD)

### 8.1 Black-Box Tests

```erlang
-module(erlmcp_flow_agent_tests).
-include_lib("eunit/include/eunit.hrl").

%% Test state transitions
state_transitions_test() ->
    {ok, Agent} = start_test_agent(worker),

    %% Initial state
    ?assertEqual(idle, get_agent_status(Agent)),

    %% Assign task
    Task = create_test_task(),
    erlmcp_flow_agent:assign_task(Agent, Task),

    %% Wait for execution
    timer:sleep(100),
    ?assertEqual(executing, get_agent_status(Agent)),

    %% Complete task
    erlmcp_flow_agent:complete_task(Agent, Task#task.id, ok),

    %% Wait for transition
    timer:sleep(100),
    ?assertEqual(idle, get_agent_status(Agent)),

    erlmcp_flow_agent:stop(Agent).

%% Test error recovery
error_recovery_test() ->
    {ok, Agent} = start_test_agent(worker),

    %% Assign failing task
    Task = create_failing_task(),
    erlmcp_flow_agent:assign_task(Agent, Task),

    %% Wait for failure and retry
    timer:sleep(500),

    %% Check retry count incremented
    {ok, Metrics} = erlmcp_flow_agent:get_metrics(Agent),
    ?assert(maps:get(tasks_failed, Metrics) > 0),

    erlmcp_flow_agent:stop(Agent).

%% Test task queue
task_queue_test() ->
    {ok, Agent} = start_test_agent(worker),

    %% Assign multiple tasks
    Tasks = [create_test_task() || _ <- lists:seq(1, 10)],
    [erlmcp_flow_agent:assign_task(Agent, T) || T <- Tasks],

    %% Wait for all to complete
    timer:sleep(2000),

    %% Verify all completed
    {ok, Metrics} = erlmcp_flow_agent:get_metrics(Agent),
    ?assertEqual(10, maps:get(tasks_completed, Metrics)),

    erlmcp_flow_agent:stop(Agent).
```

---

## 9. Performance Targets

| Metric | Target | Acceptance |
|--------|--------|------------|
| Task assignment latency | < 1ms | < 5ms |
| State transition time | < 10μs | < 100μs |
| Heartbeat overhead | < 100μs | < 500μs |
| Memory per agent | < 512KB | < 1MB |
| Task throughput | > 1K tasks/sec | > 500 tasks/sec |

---

## 10. Integration Points

### 10.1 With Registry

```erlang
%% Register agent on startup
erlmcp_flow_registry:register_agent(AgentId, Pid, Capabilities)

%% Lookup agent by ID
{ok, Pid} = erlmcp_flow_registry:find_agent(AgentId)

%% Update agent load
erlmcp_flow_registry:update_agent_load(AgentId, QueueLength)
```

### 10.2 With Swarm Coordinator

```erlang
%% Notify ready
SwarmCoordinator ! {agent_ready, AgentId, Role, self()}

%% Report task completion
SwarmCoordinator ! {task_complete, TaskId, Result, AgentId}
```

### 10.3 With Consensus Engine

```erlang
%% Propose task assignment (Raft)
ConsensusEngine ! {propose_task_assignment, TaskId, AgentId, Priority}

%% Acknowledge commit
ConsensusEngine ! {acknowledge_commit, TaskId, AgentId}
```

---

## Summary

This design provides a complete OTP-compliant agent implementation with:

- ✅ State machine (idle → assigned → executing → done)
- ✅ Non-blocking init via `{continue, connect}`
- ✅ All gen_server callbacks implemented
- ✅ Error recovery with exponential backoff
- ✅ Process isolation guarantees
- ✅ Comprehensive message protocols
- ✅ Health checks and metrics
- ✅ Chicago TDD test strategy

**Next Steps**:
1. Implement `erlmcp_flow_agent.erl` module
2. Write EUnit tests
3. Create supervision tree
4. Integration with registry/coordinator
5. Performance benchmarking
