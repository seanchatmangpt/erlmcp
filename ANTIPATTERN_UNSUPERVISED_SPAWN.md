# Antipattern Analysis: Unsupervised spawn/1 and Unmonitored Processes

**Analysis Date**: 2026-02-01
**Antipattern**: #2 - Unsupervised spawn/1 and unmonitored processes
**Severity**: HIGH - Violates let-it-crash principle and OTP supervision architecture
**Files Analyzed**: 164 modules (apps/erlmcp_*/src/*.erl and apps/erlmcp_*/test/*.erl)

---

## Executive Summary

**Total spawn patterns found**: 800+ instances
**Production code violations**: 18 critical violations
**Test code instances**: 780+ (acceptable in test context)
**Correct patterns (proc_lib)**: 2 instances

**Risk Assessment**:
- **Critical**: 8 unsupervised spawn() calls in production code
- **High**: 10 spawn_link() calls without proper supervision
- **Medium**: CLI/validation modules using spawn for background tasks

---

## 1. CRITICAL VIOLATIONS - Unsupervised spawn()

### 1.1 erlmcp_chaos_resource.erl (Line 96)

**Location**: `apps/erlmcp_observability/src/erlmcp_chaos_resource.erl:96`

**Code**:
```erlang
Workers = [spawn(fun() -> cpu_burn_loop() end) || _ <- lists:seq(1, WorkerCount)],
```

**Context**: Spawns CPU-intensive workers for chaos engineering without supervision.

**Current Monitoring**: None - workers are killed via `exit(Pid, kill)` after duration.

**Risk**: If chaos experiment crashes, workers become orphaned and continue burning CPU indefinitely.

**Recommended Fix**:
```erlang
%% Create erlmcp_chaos_worker_sup (already exists for other chaos workers)
%% Modify to support CPU burn workers:
{ok, WorkerPid} = supervisor:start_child(erlmcp_chaos_worker_sup, [
    #{type => cpu_burn, duration => Duration}
]).
```

**Supervision Strategy**: Use existing `erlmcp_chaos_worker_sup` (simple_one_for_one).

---

### 1.2 erlmcp_observability_app.erl (Line 36)

**Location**: `apps/erlmcp_observability/src/erlmcp_observability_app.erl:36`

**Code**:
```erlang
spawn(fun() ->
    timer:sleep(100),  % Small delay to let supervisor initialize
    init_otel()
end);
```

**Context**: Asynchronous OTEL initialization spawned in application:start/2 callback.

**Current Monitoring**: None - if OTEL init crashes, no supervision restart.

**Risk**:
- OTEL initialization failure goes unnoticed
- Application startup blocks if init_otel/0 has issues
- No crash reports or supervision tree integration

**Recommended Fix**:
```erlang
%% Option 1: Use proc_lib for proper OTP integration
proc_lib:spawn(fun() ->
    timer:sleep(100),
    init_otel()
end);

%% Option 2: Add OTEL initializer to supervision tree
%% In erlmcp_observability_sup:init/1:
#{
    id => otel_initializer,
    start => {erlmcp_otel_initializer, start_link, []},
    restart => transient,  % Only restart if abnormal exit
    shutdown => 5000,
    type => worker
}
```

**Supervision Strategy**: Add to `erlmcp_observability_sup` as transient worker.

---

### 1.3 erlmcp_memory_analyzer.erl (Line 236)

**Location**: `apps/erlmcp_observability/src/erlmcp_memory_analyzer.erl:236`

**Code**:
```erlang
-spec memory_trends(pos_integer()) -> {ok, pid()}.
memory_trends(IntervalMs) ->
    Pid = spawn(fun() -> trend_tracker(IntervalMs, []) end),
    {ok, Pid}.
```

**Context**: Memory trend tracking process for observability.

**Current Monitoring**: None - caller receives Pid but no supervision.

**Risk**: Trend tracker crashes silently, no memory trend data collected.

**Recommended Fix**:
```erlang
%% Add to erlmcp_observability_sup as dynamic child
memory_trends(IntervalMs) ->
    ChildSpec = #{
        id => make_ref(),  % Unique ID for dynamic child
        start => {erlmcp_memory_trend_tracker, start_link, [IntervalMs]},
        restart => temporary,
        shutdown => 5000,
        type => worker
    },
    supervisor:start_child(erlmcp_observability_sup, ChildSpec).
```

**Supervision Strategy**: Add to `erlmcp_observability_sup` as temporary worker.

---

### 1.4 erlmcp_profiler.erl (Line 357)

**Location**: `apps/erlmcp_observability/src/erlmcp_profiler.erl:357`

**Code**:
```erlang
-spec trace_messages(pid(), pos_integer()) -> {ok, [term()]}.
trace_messages(Pid, Duration) ->
    Tracer = spawn(fun() -> message_tracer(Pid, []) end),
    erlang:trace(Pid, true, ['receive', {tracer, Tracer}]),
    % ...
```

**Context**: Message tracing for profiling.

**Current Monitoring**: None - short-lived tracer process.

**Risk**: Tracer crash leaves trace enabled on target process indefinitely.

**Recommended Fix**:
```erlang
trace_messages(Pid, Duration) ->
    Parent = self(),
    Tracer = proc_lib:spawn_link(fun() ->
        message_tracer(Parent, Pid, [])
    end),
    erlang:trace(Pid, true, ['receive', {tracer, Tracer}]),
    % Ensure cleanup on any exit
    try
        % ... existing code ...
    after
        erlang:trace(Pid, false, ['receive'])
    end.
```

**Supervision Strategy**: Use proc_lib:spawn_link for OTP compliance.

---

### 1.5 erlmcp_debugger.erl (Lines 166, 189, 194, 302, 309)

**Location**: `apps/erlmcp_observability/src/erlmcp_debugger.erl`

**Code**:
```erlang
% Line 166
Tracer = spawn(fun() -> call_tracer(Ref, []) end),

% Line 189
Tracer = spawn(fun() -> message_tracer(Ref, Pid, []) end),

% Line 194
spawn(fun() -> % trace continuation ...

% Line 302
Collector = spawn(fun() -> call_graph_collector(Ref, []) end),

% Line 309
spawn(fun() -> % call graph analysis ...
```

**Context**: Multiple debugger tracer processes.

**Current Monitoring**: None - all short-lived debug tracers.

**Risk**: Tracer crashes leave tracing enabled system-wide (erlang:trace(all, true, ...)).

**Recommended Fix**:
```erlang
%% All debugger tracers should use proc_lib:spawn_link
%% Example:
trace_calls(Module, Function, Arity) ->
    Ref = make_ref(),
    Parent = self(),
    Tracer = proc_lib:spawn_link(fun() ->
        call_tracer(Parent, Ref, [])
    end),

    %% Register cleanup on exit
    erlang:put(tracer_cleanup, fun() ->
        erlang:trace(all, false, [call]),
        erlang:trace_pattern({'_', '_', '_'}, false, [local])
    end),

    % ... rest of tracing setup ...
```

**Supervision Strategy**: Use proc_lib:spawn_link + cleanup on exit.

---

### 1.6 erlmcp_health_monitor.erl (Line 428)

**Location**: `apps/erlmcp_observability/src/erlmcp_health_monitor.erl:428`

**Code**:
```erlang
-spec execute_with_timeout(fun(() -> term()), pos_integer()) ->
                              {health_status(), term()} | health_status().
execute_with_timeout(Fun, Timeout) ->
    Parent = self(),
    Ref = make_ref(),
    Pid = spawn(fun() ->
                   Result = Fun(),
                   Parent ! {Ref, Result}
                end),
    receive
        {Ref, Result} -> Result
    after Timeout ->
        exit(Pid, kill),
        {unhealthy, timeout}
    end.
```

**Context**: Timeout-protected health check execution.

**Current Monitoring**: None - short-lived execution wrapper.

**Risk**: If parent crashes during receive, spawned process becomes orphaned.

**Recommended Fix**:
```erlang
execute_with_timeout(Fun, Timeout) ->
    Parent = self(),
    Ref = make_ref(),
    Pid = spawn_monitor(fun() ->
                   Result = Fun(),
                   Parent ! {Ref, Result}
                end),
    receive
        {Ref, Result} ->
            erlang:demonitor(element(2, Pid), [flush]),
            Result;
        {'DOWN', _, process, _, Reason} ->
            {unhealthy, {executor_died, Reason}}
    after Timeout ->
        exit(element(1, Pid), kill),
        {unhealthy, timeout}
    end.
```

**Supervision Strategy**: Use spawn_monitor for automatic cleanup.

---

### 1.7 CLI Modules (erlmcp_cli_observer.erl, erlmcp_cli_tracer.erl)

**Location**:
- `apps/erlmcp_validation/src/erlmcp_cli_observer.erl:68`
- `apps/erlmcp_validation/src/erlmcp_cli_tracer.erl:232`

**Code**:
```erlang
% erlmcp_cli_observer.erl:68
Pid = spawn(fun() ->
    watch_background_loop(Opts, RefreshInterval, OutputFile, [])
end),
erlang:register(erlmcp_cli_watch, Pid),

% erlmcp_cli_tracer.erl:232
CollectorPid = spawn(fun() ->
    trace_collector_loop([], MaxEvents)
end),
erlang:register(erlmcp_trace_collector, CollectorPid),
```

**Context**: CLI background processes registered globally.

**Current Monitoring**: Registered with global names, but no supervision.

**Risk**: CLI tool crashes leave orphaned registered processes.

**Recommended Fix**:
```erlang
%% Option 1: Use proc_lib + register cleanup
start_watch(Opts) ->
    Pid = proc_lib:spawn_link(fun() ->
        erlang:register(erlmcp_cli_watch, self()),
        watch_background_loop(Opts, RefreshInterval, OutputFile, [])
    end),
    {ok, Pid}.

%% Option 2: Add CLI supervisor (if CLI becomes long-lived)
%% Create erlmcp_cli_sup for all CLI background processes
```

**Supervision Strategy**: Use proc_lib:spawn_link or add CLI supervisor.

---

### 1.8 erlmcp_performance_validator.erl (Line 575)

**Location**: `apps/erlmcp_validation/src/erlmcp_performance_validator.erl:575`

**Code**:
```erlang
Clients = lists:map(fun(_) ->
    spawn(fun() ->
        case erlmcp_test_client:start_test_client(Transport, #{
            owner => self(),
            test_mode => true
        }) of
            {ok, Pid} -> self() ! {result, {ok, Pid}};
            {error, Reason} -> self() ! {result, {error, Reason}}
        end
    end)
end, lists:seq(1, NumConnections)),
```

**Context**: Performance test connection spawning.

**Current Monitoring**: None - validation/test module.

**Risk**: Medium - validation context, but still violates OTP principles.

**Recommended Fix**:
```erlang
%% Use spawn_monitor for test processes
Clients = lists:map(fun(_) ->
    spawn_monitor(fun() ->
        % ... test client logic ...
    end)
end, lists:seq(1, NumConnections)),
```

**Supervision Strategy**: Use spawn_monitor for automatic cleanup.

---

## 2. HIGH SEVERITY - spawn_link() Without Supervision

### 2.1 erlmcp_completion.erl (Line 544)

**Location**: `apps/erlmcp_core/src/erlmcp_completion.erl:544`

**Code**:
```erlang
Pid = spawn_link(?MODULE, stream_loop, [
    CompletionId, Ref, Argument, Context, Handler,
    State#state.max_results, State#state.ranking_threshold
]),
```

**Context**: Spawns streaming completion handler in gen_server callback.

**Current Monitoring**: Linked to gen_server, but not supervised.

**Risk**: Stream loop crash kills gen_server, which may not be desired behavior.

**Recommended Fix**:
```erlang
%% Option 1: Use proc_lib for proper OTP integration
Pid = proc_lib:spawn_link(?MODULE, stream_loop, [
    CompletionId, Ref, Argument, Context, Handler,
    State#state.max_results, State#state.ranking_threshold
]),

%% Option 2: Add completion worker supervisor
%% In erlmcp_sup, add erlmcp_completion_worker_sup (simple_one_for_one)
{ok, Pid} = supervisor:start_child(erlmcp_completion_worker_sup, [
    CompletionId, Ref, Argument, Context, Handler, MaxResults, RankingThreshold
]).
```

**Where It Should Be Supervised**: Under `erlmcp_core_sup` or new `erlmcp_completion_worker_sup`.

---

### 2.2 erlmcp_batch.erl (Line 372)

**Location**: `apps/erlmcp_core/src/erlmcp_batch.erl:372`

**Code**:
```erlang
Refs = [begin
    Ref = make_ref(),
    spawn_link(fun() ->
        ChunkResults = try
            Executor(Chunk)
        catch
            Class:Reason:Stack ->
                logger:error("Batch executor crashed: ~p:~p~n~p", [Class, Reason, Stack]),
                [{error, {executor_crashed, Reason}} || _ <- Chunk]
        end,
        Parent ! {batch_result, Ref, ChunkResults}
    end),
    Ref
end || Chunk <- Chunks],
```

**Context**: Parallel batch execution workers.

**Current Monitoring**: Linked to caller, error caught internally.

**Risk**: Worker crash propagates to caller even though error is caught.

**Recommended Fix**:
```erlang
%% Use proc_lib for OTP compliance
Refs = [begin
    Ref = make_ref(),
    proc_lib:spawn_link(fun() ->
        ChunkResults = try
            Executor(Chunk)
        catch
            Class:Reason:Stack ->
                logger:error("Batch executor crashed: ~p:~p~n~p", [Class, Reason, Stack]),
                [{error, {executor_crashed, Reason}} || _ <- Chunk]
        end,
        Parent ! {batch_result, Ref, ChunkResults}
    end),
    Ref
end || Chunk <- Chunks],
```

**Where It Should Be Supervised**: Use proc_lib:spawn_link (self-supervised).

---

### 2.3 erlmcp_control_plane.erl (Line 170)

**Location**: `apps/erlmcp_core/src/erlmcp_control_plane.erl:170`

**Code**:
```erlang
HandlerPid = spawn_link(fun() -> component_handler_loop(ComponentId, HandlerFun) end),
MonitorRef = monitor(process, HandlerPid),
```

**Context**: Component handler process for control plane.

**Current Monitoring**: Both linked AND monitored (redundant).

**Risk**: Link propagates crashes; monitor is redundant.

**Recommended Fix**:
```erlang
%% Option 1: Use monitor only (no link)
HandlerPid = spawn(fun() -> component_handler_loop(ComponentId, HandlerFun) end),
MonitorRef = monitor(process, HandlerPid),

%% Option 2: Add component handler supervisor
%% Create erlmcp_component_handler_sup (simple_one_for_one)
{ok, HandlerPid} = supervisor:start_child(erlmcp_component_handler_sup, [
    ComponentId, HandlerFun
]).
```

**Where It Should Be Supervised**: Under `erlmcp_core_sup` or new `erlmcp_component_handler_sup`.

---

### 2.4 erlmcp_server.erl (Line 2416)

**Location**: `apps/erlmcp_core/src/erlmcp_server.erl:2416`

**Code**:
```erlang
spawn_link(fun() ->
    execute_streaming_tool(StreamId, ProgressToken, ServerPid, Name, Handler, Arguments, StreamOpts)
end),
```

**Context**: Streaming tool execution in server.

**Current Monitoring**: Linked to server gen_server.

**Risk**: Tool execution crash kills server, losing all server state.

**Recommended Fix**:
```erlang
%% Use proc_lib for proper OTP integration
proc_lib:spawn_link(fun() ->
    execute_streaming_tool(StreamId, ProgressToken, ServerPid, Name, Handler, Arguments, StreamOpts)
end),

%% Better: Use task_runner (already exists!)
erlmcp_task_runner:start_task(
    fun() -> execute_streaming_tool(...) end,
    StreamId,
    #{timeout => maps:get(timeout, StreamOpts, 30000)}
).
```

**Where It Should Be Supervised**: Use existing `erlmcp_task_runner` infrastructure.

---

### 2.5 erlmcp_transport_stdio.erl (Line 170)

**Location**: `apps/erlmcp_transports/src/erlmcp_transport_stdio.erl:170`

**Code**:
```erlang
ReaderPid = spawn_link(fun() -> read_loop(self(), Owner, MaxMessageSize) end),
{ok, State#state{reader = ReaderPid}}
```

**Context**: STDIO reader process for transport.

**Current Monitoring**: Linked to transport gen_server.

**Risk**: Reader crash kills transport, but this may be desired for stdio failures.

**Assessment**: **ACCEPTABLE** - stdio reader crash should kill transport (let-it-crash).

**Recommended Enhancement**:
```erlang
%% Use proc_lib for proper OTP integration (crash reports)
ReaderPid = proc_lib:spawn_link(fun() ->
    read_loop(self(), Owner, MaxMessageSize)
end),
```

**Where It Should Be Supervised**: Already supervised via `erlmcp_transport_sup` (parent of transport).

---

### 2.6 erlmcp_memory_manager.erl (Line 98)

**Location**: `apps/erlmcp_validation/src/erlmcp_memory_manager.erl:98`

**Code**:
```erlang
handle_call(start_memory_monitor, _From, State) ->
    MonitorPid = spawn_link(fun() -> memory_monitor_loop(self()) end),
    {reply, {ok, MonitorPid}, State};
```

**Context**: Memory monitor loop in gen_server callback.

**Current Monitoring**: Linked to memory_manager gen_server.

**Risk**: Monitor crash kills memory_manager.

**Recommended Fix**:
```erlang
handle_call(start_memory_monitor, _From, State) ->
    MonitorPid = proc_lib:spawn_link(fun() ->
        memory_monitor_loop(self())
    end),
    {reply, {ok, MonitorPid}, State};
```

**Where It Should Be Supervised**: Via `erlmcp_validation_sup` (parent of memory_manager).

---

### 2.7 erlmcp_cli_interactive.erl (Line 190)

**Location**: `apps/erlmcp_validation/src/erlmcp_cli_interactive.erl:190`

**Code**:
```erlang
handle_info(run_repl, State) ->
    % Run REPL loop in separate process to avoid blocking gen_server
    spawn_link(fun() -> repl_loop(State) end),
    {noreply, State};
```

**Context**: Interactive REPL loop for CLI.

**Current Monitoring**: Linked to CLI interactive gen_server.

**Risk**: REPL crash kills CLI server.

**Recommended Fix**:
```erlang
handle_info(run_repl, State) ->
    proc_lib:spawn_link(fun() -> repl_loop(State) end),
    {noreply, State};
```

**Where It Should Be Supervised**: Via `erlmcp_validation_sup` (parent of cli_interactive).

---

### 2.8 erlmcp_apps_server.erl (Line 425) - HAS TODO!

**Location**: `apps/erlmcp_core/src/erlmcp_apps_server.erl:425`

**Code**:
```erlang
start_app_process(AppId, Manifest, Config) ->
    %% Simplified app process starter
    %% In production, this would use a proper supervisor  ← TODO COMMENT!
    try
        Pid = spawn_link(fun() ->
            logger:info("App ~p started with config ~p", [AppId, Config]),
            app_loop(AppId, Manifest, Config)
        end),
        {ok, Pid}
    catch
        _:Reason -> {error, Reason}
    end.
```

**Context**: App process starter with explicit TODO about supervision.

**Current Monitoring**: Linked to apps_server.

**Risk**: App crash kills apps_server, losing all app state.

**Recommended Fix**:
```erlang
%% Create erlmcp_app_worker_sup (simple_one_for_one)
start_app_process(AppId, Manifest, Config) ->
    ChildSpec = #{
        id => AppId,
        start => {erlmcp_app_worker, start_link, [AppId, Manifest, Config]},
        restart => transient,
        shutdown => 5000,
        type => worker
    },
    supervisor:start_child(erlmcp_app_worker_sup, ChildSpec).
```

**Where It Should Be Supervised**: New `erlmcp_app_worker_sup` under `erlmcp_core_sup`.

---

### 2.9 erlmcp_test_client.erl (Lines 770, 775, 1021)

**Location**: `apps/erlmcp_validation/src/erlmcp_test_client.erl`

**Code**:
```erlang
% Line 770
spawn_link(fun() -> handle_concurrent_requests(Requests, Options, From, State) end),

% Line 775
spawn_link(fun() -> handle_sequence_execution(Sequence, From, State) end),

% Line 1021
spawn_link(fun() -> % worker execution ...
```

**Context**: Test client request handlers.

**Current Monitoring**: Linked to test_client gen_server.

**Risk**: Handler crash kills test client during tests.

**Assessment**: **MEDIUM** - test/validation context, but should still follow OTP patterns.

**Recommended Fix**:
```erlang
%% Use proc_lib for all test client workers
proc_lib:spawn_link(fun() ->
    handle_concurrent_requests(Requests, Options, From, State)
end),
```

**Where It Should Be Supervised**: Via `erlmcp_validation_sup` (parent of test_client).

---

## 3. CORRECT PATTERNS - Using proc_lib

### 3.1 erlmcp_task_runner.erl (Lines 99, 125) ✅

**Location**: `apps/erlmcp_core/src/erlmcp_task_runner.erl`

**Code**:
```erlang
% Line 99
Pid = proc_lib:spawn_link(?MODULE, init, [Parent, TaskSpec]),

% Line 125
Pid = proc_lib:spawn_link(?MODULE, init, [Parent, TaskSpec]),
```

**Assessment**: **CORRECT** - Uses proc_lib:spawn_link for proper OTP integration.

**Why This Is Good**:
- proc_lib provides proper crash reports
- Integrates with OTP supervision trees
- Supports sys module debugging
- Maintains process dictionary entries for OTP compliance

**Pattern To Follow**: All spawn_link calls should use proc_lib:spawn_link.

---

## 4. TEST CODE - Acceptable Spawn Patterns

**Total Test Instances**: 780+ spawn/spawn_link calls in test files.

**Assessment**: **ACCEPTABLE** - Test code spawn patterns are acceptable because:
1. Tests create temporary processes for concurrency testing
2. Test processes are short-lived and cleaned up after test
3. Test supervision would add unnecessary complexity
4. Tests explicitly verify crash/error behavior

**Example Test Patterns** (acceptable):
```erlang
% Concurrency test
Pids = [spawn(fun() -> worker_test_loop() end) || _ <- lists:seq(1, 100)],

% Timeout test
TestPid = spawn(fun() -> timer:sleep(infinity) end),

% Crash test
Pid = spawn(fun() -> receive die -> ok end end),
```

**Recommendation**: No changes needed for test code spawn patterns.

---

## 5. POC CODE - Should Be Supervised Before Production

### 5.1 erlmcp_streaming_poc.erl (Lines 243, 392)

**Location**: `apps/erlmcp_core/src/poc/erlmcp_streaming_poc.erl`

**Code**:
```erlang
% Line 243
spawn(fun() -> execute_tool_async(Self, ExecutionId, ToolName, Params) end),

% Line 392
spawn(fun() -> % async stream handler ...
```

**Context**: Proof-of-concept streaming implementation.

**Assessment**: **POC - NOT PRODUCTION READY**

**Recommended Fix Before Production**:
```erlang
%% Use erlmcp_task_runner for production
erlmcp_task_runner:start_task(
    fun() -> execute_tool_async(Self, ExecutionId, ToolName, Params) end,
    ExecutionId,
    #{timeout => 30000}
).
```

---

## 6. Supervision Architecture Recommendations

### 6.1 Current Supervision Tree

```
erlmcp_sup (one_for_one)
├── erlmcp_core_sup (one_for_one)
│   ├── erlmcp_infrastructure_sup
│   │   ├── erlmcp_registry
│   │   ├── erlmcp_client_sup (simple_one_for_one) ✅
│   │   └── erlmcp_server_sup (simple_one_for_one) ✅
│   ├── erlmcp_session_sup
│   ├── erlmcp_resource_sup
│   └── erlmcp_resilience_sup
├── erlmcp_observability_sup (one_for_one)
│   ├── erlmcp_metrics_server ✅
│   ├── erlmcp_health_monitor ✅
│   ├── erlmcp_chaos_worker_sup (simple_one_for_one) ✅
│   └── erlmcp_dashboard_server ✅
├── erlmcp_validation_sup (one_for_one)
│   └── erlmcp_memory_manager ✅
└── erlmcp_transports_sup (one_for_one)
    └── erlmcp_transport_sup (one_for_one) ✅
```

### 6.2 Recommended New Supervisors

```
erlmcp_core_sup (one_for_one)
  ├── [EXISTING]
  ├── erlmcp_completion_worker_sup (simple_one_for_one) [NEW]
  │   └── Dynamic: completion stream workers
  ├── erlmcp_component_handler_sup (simple_one_for_one) [NEW]
  │   └── Dynamic: control plane component handlers
  └── erlmcp_app_worker_sup (simple_one_for_one) [NEW]
      └── Dynamic: application workers

erlmcp_observability_sup (one_for_one)
  ├── [EXISTING]
  ├── erlmcp_otel_initializer [NEW - transient worker]
  ├── erlmcp_memory_trend_sup (simple_one_for_one) [NEW]
  │   └── Dynamic: memory trend trackers
  └── erlmcp_profiler_sup (simple_one_for_one) [NEW]
      └── Dynamic: profiler/debugger tracers

erlmcp_validation_sup (one_for_one)
  ├── [EXISTING]
  └── erlmcp_cli_sup (one_for_one) [NEW - if CLI becomes long-lived]
      ├── erlmcp_cli_observer
      └── erlmcp_cli_tracer
```

### 6.3 Supervisor Child Specs

#### erlmcp_completion_worker_sup

```erlang
-module(erlmcp_completion_worker_sup).
-behaviour(supervisor).

init([]) ->
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 10,
        period => 5
    },

    ChildSpec = #{
        id => completion_worker,
        start => {erlmcp_completion_worker, start_link, []},
        restart => temporary,  % Completion streams are temporary
        shutdown => 5000,
        type => worker,
        modules => [erlmcp_completion_worker]
    },

    {ok, {SupFlags, [ChildSpec]}}.
```

#### erlmcp_otel_initializer (transient worker)

```erlang
-module(erlmcp_otel_initializer).
-behaviour(gen_server).

%% Started by erlmcp_observability_sup
%% Transient restart: only restart if abnormal exit

init([]) ->
    %% Async initialization
    self() ! init_otel,
    {ok, #state{}}.

handle_info(init_otel, State) ->
    timer:sleep(100),  % Small delay for supervisor startup
    case init_otel() of
        ok -> {stop, normal, State};  % Normal exit = don't restart
        {error, Reason} -> {stop, Reason, State}  % Abnormal = restart
    end.
```

---

## 7. Migration Path to Proper Supervision

### Phase 1: Quick Wins (Low Risk)

**Priority**: HIGH
**Effort**: Low (1-2 hours)
**Impact**: Immediate OTP compliance for short-lived processes

1. Replace all spawn_link with proc_lib:spawn_link (9 instances)
2. Add proc_lib to debugger/profiler tracers (5 instances)
3. Fix spawn_monitor pattern in health_monitor (1 instance)

**Files to Change**:
- erlmcp_completion.erl
- erlmcp_batch.erl
- erlmcp_server.erl
- erlmcp_memory_manager.erl
- erlmcp_cli_interactive.erl
- erlmcp_debugger.erl
- erlmcp_profiler.erl
- erlmcp_health_monitor.erl

### Phase 2: Add Supervisors (Medium Risk)

**Priority**: HIGH
**Effort**: Medium (4-6 hours)
**Impact**: Full supervision for critical production processes

1. Create erlmcp_completion_worker_sup
2. Create erlmcp_component_handler_sup
3. Create erlmcp_app_worker_sup
4. Move completion streams to supervisor
5. Move component handlers to supervisor
6. Move app workers to supervisor

**New Files**:
- apps/erlmcp_core/src/erlmcp_completion_worker_sup.erl
- apps/erlmcp_core/src/erlmcp_completion_worker.erl
- apps/erlmcp_core/src/erlmcp_component_handler_sup.erl
- apps/erlmcp_core/src/erlmcp_component_handler.erl
- apps/erlmcp_core/src/erlmcp_app_worker_sup.erl
- apps/erlmcp_core/src/erlmcp_app_worker.erl

### Phase 3: Observability Supervision (Low Risk)

**Priority**: MEDIUM
**Effort**: Medium (3-4 hours)
**Impact**: Proper supervision for observability tools

1. Create erlmcp_otel_initializer worker
2. Create erlmcp_memory_trend_sup
3. Create erlmcp_profiler_sup
4. Move OTEL init to supervisor
5. Move memory trends to supervisor
6. Move profiler/debugger tracers to supervisor

**New Files**:
- apps/erlmcp_observability/src/erlmcp_otel_initializer.erl
- apps/erlmcp_observability/src/erlmcp_memory_trend_sup.erl
- apps/erlmcp_observability/src/erlmcp_memory_trend_tracker.erl
- apps/erlmcp_observability/src/erlmcp_profiler_sup.erl
- apps/erlmcp_observability/src/erlmcp_profiler_worker.erl

### Phase 4: Chaos Engineering (Low Priority)

**Priority**: LOW
**Effort**: Low (1-2 hours)
**Impact**: Supervision for chaos experiment workers

1. Extend erlmcp_chaos_worker_sup to support CPU burn workers
2. Move chaos resource CPU burn to supervisor

**Files to Change**:
- apps/erlmcp_observability/src/erlmcp_chaos_worker_sup.erl
- apps/erlmcp_observability/src/erlmcp_chaos_resource.erl

### Phase 5: CLI Supervision (Optional)

**Priority**: LOW (only if CLI becomes long-lived service)
**Effort**: Medium (2-3 hours)
**Impact**: Supervision for CLI background processes

1. Create erlmcp_cli_sup
2. Move CLI observer/tracer to supervisor

---

## 8. Testing Strategy for Migration

### 8.1 Test Each Phase Independently

**Phase 1 Tests**:
```erlang
% Verify proc_lib integration
test_proc_lib_crash_reports() ->
    % Spawn process using proc_lib
    Pid = proc_lib:spawn_link(fun() -> error(intentional_crash) end),

    % Verify crash report generated
    timer:sleep(100),
    CrashReports = application:get_env(sasl, errlog_type, all),
    ?assert(lists:any(fun(Report) ->
        proplists:get_value(pid, Report) =:= Pid
    end, CrashReports)).
```

**Phase 2 Tests**:
```erlang
% Verify supervisor restart behavior
test_completion_worker_restart() ->
    % Start completion worker via supervisor
    {ok, Pid} = supervisor:start_child(erlmcp_completion_worker_sup, [Args]),

    % Kill worker
    exit(Pid, kill),

    % Verify supervisor handles death (temporary = no restart)
    timer:sleep(100),
    ?assertEqual([], supervisor:which_children(erlmcp_completion_worker_sup)).
```

### 8.2 Chaos Testing

**Verify let-it-crash behavior**:
```erlang
% Chaos test: kill random supervised processes
test_supervision_resilience() ->
    % Start system
    {ok, _} = application:start(erlmcp_core),

    % Kill 10 random supervised processes
    lists:foreach(fun(_) ->
        Pids = lists:filter(fun erlang:is_process_alive/1,
                           erlang:processes()),
        Pid = lists:nth(rand:uniform(length(Pids)), Pids),
        exit(Pid, kill),
        timer:sleep(100)
    end, lists:seq(1, 10)),

    % Verify system still healthy
    ?assertEqual(pong, erlmcp_health_monitor:ping()).
```

---

## 9. Summary of Violations by Severity

| Severity | Count | Category | Action Required |
|----------|-------|----------|----------------|
| CRITICAL | 8 | Unsupervised spawn() | Phase 1 + Phase 2 |
| HIGH | 10 | spawn_link() without supervision | Phase 1 + Phase 2 |
| MEDIUM | 3 | CLI background processes | Phase 5 (optional) |
| LOW | 1 | Chaos CPU burn workers | Phase 4 |
| POC | 2 | Proof-of-concept code | Document as non-production |
| ACCEPTABLE | 780+ | Test code | No action needed |

**Total Production Violations**: 22 instances
**Total Files Affected**: 18 production files

---

## 10. Conclusion

### Key Findings

1. **18 production files** violate unsupervised spawn antipattern
2. **8 critical violations** using spawn() without any supervision
3. **10 high severity violations** using spawn_link() but missing supervisor
4. **1 module (erlmcp_task_runner)** correctly uses proc_lib ✅

### Immediate Actions Required

1. **Replace spawn_link → proc_lib:spawn_link** (9 instances, 1-2 hours)
2. **Add supervision for completion workers** (HIGH priority)
3. **Add supervision for component handlers** (HIGH priority)
4. **Add supervision for app workers** (HIGH priority, has TODO!)
5. **Fix OTEL initialization** (CRITICAL, blocks app startup)

### Armstrong Principle Compliance

**Current Compliance**: ❌ VIOLATION
**After Phase 1**: ⚠️ IMPROVED (proc_lib integration)
**After Phase 2**: ✅ COMPLIANT (full supervision)

> "Let it crash" requires supervision. Unsupervised processes cannot crash safely.
> — Joe Armstrong

### Estimated Total Effort

- Phase 1 (Quick Wins): 1-2 hours ✅ DO FIRST
- Phase 2 (Supervisors): 4-6 hours ✅ HIGH PRIORITY
- Phase 3 (Observability): 3-4 hours ⚠️ MEDIUM PRIORITY
- Phase 4 (Chaos): 1-2 hours ⚠️ LOW PRIORITY
- Phase 5 (CLI): 2-3 hours ⚠️ OPTIONAL

**Total**: 11-17 hours for full OTP compliance

---

## Appendix: Supervision Pattern Quick Reference

### ❌ WRONG: Unsupervised spawn

```erlang
Pid = spawn(fun() -> worker_loop() end).
```

### ⚠️ BETTER: spawn_link (but still not supervised)

```erlang
Pid = spawn_link(fun() -> worker_loop() end).
```

### ✅ GOOD: proc_lib (OTP integration)

```erlang
Pid = proc_lib:spawn_link(fun() -> worker_loop() end).
```

### ✅ BEST: Supervised via supervisor

```erlang
{ok, Pid} = supervisor:start_child(my_worker_sup, [Args]).
```

---

**Report Generated**: 2026-02-01
**Analyst**: Erlang Architect Agent
**Next Review**: After Phase 2 completion
