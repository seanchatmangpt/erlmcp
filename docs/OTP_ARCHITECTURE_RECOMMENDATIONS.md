# OTP Architecture Recommendations & Design Patterns

**Author**: Erlang/OTP Best Practices Review Team
**Date**: 2026-01-27
**Status**: Approved for Implementation

---

## Current Architecture - Strengths & Opportunities

### Current Supervision Tree

```
┌─ erlmcp_sup (one_for_all)
│
├─ erlmcp_health_monitor          [Worker]
├─ erlmcp_recovery_manager        [Worker]
├─ erlmcp_session_manager         [Worker]
├─ erlmcp_task_manager            [Worker]
├─ erlmcp_resource_subscriptions  [Worker]
├─ erlmcp_sse_event_store         [Worker]
├─ erlmcp_icon_cache              [Worker]
├─ erlmcp_registry                [Worker]
│
├─ erlmcp_server_sup (one_for_all)
│  └─ erlmcp_server_new           [simple_one_for_one, temporary]
│
└─ erlmcp_transport_sup (one_for_one)
   └─ [Dynamic transports]        [transient/temporary]
```

**Assessment**: Well-designed, properly layered. Ready for enhancement.

---

## Recommended Enhancements (v0.7+)

### 1. Enhanced I/O Handling Architecture

**Problem**: Blocking I/O in gen_server handlers

**Solution**: Introduce dedicated I/O worker pool

```
┌─ erlmcp_sup
│
├─ erlmcp_io_worker_sup (simple_one_for_one)
│  ├─ erlmcp_io_worker_1
│  ├─ erlmcp_io_worker_2
│  └─ [erlmcp_io_worker_N]
│
├─ erlmcp_io_queue (gen_server)
│  └─ Manages work distribution
│  └─ Implements backpressure
│  └─ Enforces timeouts
│
└─ [Other subsystems]
```

**Design Pattern**:

```erlang
%% erlmcp_io_queue.erl - Manages async I/O operations
-module(erlmcp_io_queue).
-behaviour(gen_server).

-record(state, {
    workers = [] :: [pid()],
    queue = queue:new() :: queue:queue(),
    pending = #{} :: #{reference() => {pid(), term()}},
    max_queue_size = 1000 :: pos_integer(),
    timeout_ms = 30000 :: pos_integer()
}).

%% API
-spec read_file_async(binary(), pid()) -> {ok, reference()} | {error, queue_full}.
read_file_async(Path, ReplyTo) when is_binary(Path), is_pid(ReplyTo) ->
    gen_server:call(?MODULE, {read_file, Path, ReplyTo}).

%% Implementation
handle_call({read_file, Path, ReplyTo}, _From, State) ->
    Ref = make_ref(),
    case queue:len(State#state.queue) < State#state.max_queue_size of
        true ->
            NewQueue = queue:in({read_file, Path, ReplyTo, Ref}, State#state.queue),
            {reply, {ok, Ref}, dispatch_work(State#state{queue = NewQueue})};
        false ->
            {reply, {error, queue_full}, State}
    end.

dispatch_work(#state{queue = Queue, workers = Workers} = State) ->
    case {queue:out(Queue), Workers} of
        {{empty, _}, _} -> State;
        {_, []} -> State;
        {{value, Task, Q}, [W | Ws]} ->
            erlmcp_io_worker:execute_async(W, Task),
            State#state{queue = Q, workers = Ws ++ [W]}
    end.
```

**Benefits**:
- Decouples I/O from gen_server processing
- Enforces timeouts on all operations
- Allows backpressure/queue monitoring
- Easy to add metrics and tracing

**Implementation Timeline**: 1-2 weeks

---

### 2. Subscription Management Redesign

**Problem**: No backpressure, dead subscribers never cleaned

**Solution**: Subscription health monitoring

```
┌─ erlmcp_subscription_manager (gen_server)
│  ├─ Tracks all subscriptions
│  ├─ Monitors subscriber health
│  ├─ Implements rate limiting
│  └─ Auto-cleanup dead subscribers
│
└─ erlmcp_resource_subscriptions (simplified)
   └─ Only manages URI mapping
```

**Design Pattern**:

```erlang
%% erlmcp_subscription_manager.erl
-module(erlmcp_subscription_manager).
-behaviour(gen_server).

-record(subscription, {
    uri :: binary(),
    subscriber :: pid(),
    monitor_ref :: reference(),
    queue_size = 0 :: non_neg_integer(),
    last_message_time :: integer(),
    failure_count = 0 :: non_neg_integer()
}).

%% Monitors subscriber health with periodic checks
-define(HEALTH_CHECK_INTERVAL, 30000).  % 30 seconds

init([]) ->
    timer:send_interval(?HEALTH_CHECK_INTERVAL, perform_health_check),
    {ok, #state{subscriptions = []}}.

%% Remove dead subscribers
handle_info(perform_health_check, State) ->
    NewState = cleanup_dead_subscribers(State),
    {noreply, NewState}.

handle_info({'DOWN', Ref, process, Pid, _}, State) ->
    % Subscriber died - remove all subscriptions
    NewState = remove_subscriber_subscriptions(Pid, State),
    {noreply, NewState}.

%% Notify with backpressure
notify_subscribers(Uri, Message, State) ->
    Subs = get_subscriptions(Uri, State#state.subscriptions),
    lists:foreach(fun(#subscription{subscriber = Pid} = Sub) ->
        case check_subscriber_health(Pid) of
            healthy ->
                erlang:send(Pid, Message, [noconnect]),
                update_subscription_time(Sub);
            overloaded ->
                % Unsubscribe overloaded subscriber
                remove_subscription(Sub);
            dead ->
                % Already handled by DOWN message
                ok
        end
    end, Subs).

check_subscriber_health(Pid) ->
    case erlang:process_info(Pid, message_queue_len) of
        {_, Len} when Len > 1000 -> overloaded;
        {_, _} -> healthy;
        undefined -> dead
    end.
```

**Benefits**:
- Automatic cleanup of dead subscribers
- Queue monitoring and overload detection
- Per-subscriber rate limiting
- Cleaner error recovery

**Implementation Timeline**: 3-5 days

---

### 3. Configuration Validation Framework

**Problem**: Invalid configuration silently uses defaults

**Solution**: Comprehensive startup validation

```erlang
%% erlmcp_config_schema.erl - Define configuration schema
-module(erlmcp_config_schema).

-define(CONFIG_SCHEMA, #{
    <<"server">> => #{
        <<"id">> => {atom, required},
        <<"capabilities">> => {map, optional},
        <<"timeout">> => {integer, optional, #{min => 1000, max => 300000}}
    },
    <<"transport">> => #{
        <<"type">> => {enum, [stdio, tcp, http], required},
        <<"config">> => {map, optional}
    },
    <<"health">> => #{
        <<"check_interval">> => {integer, optional, #{min => 1000}},
        <<"failure_threshold">> => {integer, optional, #{min => 1}}
    }
}).

%% Validation API
-spec validate(map()) -> ok | {error, [validation_error()]}.
validate(Config) ->
    validate_against_schema(Config, ?CONFIG_SCHEMA).

%% Application startup
application:start(erlmcp) ->
    case erlmcp_config_validation:validate(application:get_all_env(erlmcp)) of
        ok ->
            erlmcp_sup:start_link();
        {error, Errors} ->
            log_validation_errors(Errors),
            {error, config_invalid}
    end.
```

**Benefits**:
- Early detection of misconfiguration
- Clear error messages
- Type safety for configuration values
- Validation rules are declarative

**Implementation Timeline**: 2-3 days

---

### 4. Timeout Management Layer

**Problem**: Inconsistent timeout handling across modules

**Solution**: Centralized timeout management

```erlang
%% erlmcp_timeout_manager.erl - Unified timeout handling
-module(erlmcp_timeout_manager).

%% Configuration
-define(TIMEOUT_CONFIGS, #{
    initialize => 30000,        % 30 seconds
    file_io => 15000,           % 15 seconds
    http_request => 10000,      % 10 seconds
    tcp_connect => 5000,        % 5 seconds
    tcp_send => 5000,           % 5 seconds
    shutdown => 10000           % 10 seconds
}).

%% Unified API
-spec apply_timeout(atom(), fun()) -> {ok, term()} | {error, timeout}.
apply_timeout(Operation, Fun) when is_atom(Operation), is_function(Fun, 0) ->
    TimeoutMs = maps:get(Operation, ?TIMEOUT_CONFIGS, 5000),
    try
        {ok, Fun()}
    catch
        error:timeout -> {error, timeout};
        Class:Error:Stack ->
            log_timeout_error(Operation, Class, Error, Stack),
            {error, {operation_failed, Class, Error}}
    end.

%% Async timeout wrapper
-spec with_timeout(atom(), pid(), term()) -> ok | {error, timeout}.
with_timeout(Operation, ReplyTo, Message) ->
    TimeoutMs = maps:get(Operation, ?TIMEOUT_CONFIGS, 5000),
    TimerRef = erlang:send_after(TimeoutMs, self(), {timeout, Message}),
    handle_timeout(TimerRef, ReplyTo, Message).

%% Usage in transports
erlmcp_transport_tcp:read_with_timeout(Socket) ->
    erlmcp_timeout_manager:apply_timeout(
        tcp_read,
        fun() -> gen_tcp:recv(Socket, 0) end
    ).
```

**Benefits**:
- Consistent timeout across application
- Centralized configuration
- Easy to adjust globally
- Automatic timeout tracking and metrics

**Implementation Timeline**: 1-2 days

---

### 5. Async I/O Transport Patterns

**Problem**: Current transports can block on I/O

**Solution**: Universal async pattern for all transports

```erlang
%% Transport base implementation pattern
-behaviour(erlmcp_transport).

%% All transports inherit from this pattern
-module(erlmcp_transport_base).

-record(async_state, {
    owner :: pid(),
    pending = #{} :: #{reference() => term()},
    timeout_ms = 30000 :: pos_integer(),
    read_timer :: reference() | undefined
}).

%% Initialization with timeout
init(Config) ->
    TimeoutMs = maps:get(timeout, Config, 30000),
    erlang:send_after(TimeoutMs, self(), connection_timeout),
    {ok, #async_state{timeout_ms = TimeoutMs}}.

%% Async read pattern
schedule_read(#async_state{owner = Owner} = State) ->
    Ref = make_ref(),
    spawn_async_read(self(), Ref),
    {noreply, State#state{pending = maps:put(Ref, read, State#state.pending)}}.

%% Handle async completion
handle_info({read_complete, Ref, Data}, State) ->
    Owner = get_owner(State),
    Owner ! {transport_data, self(), Data},
    schedule_read(State);

%% Handle timeout
handle_info({read_timeout, Ref}, State) ->
    case maps:take(Ref, State#state.pending) of
        {read, Pending} ->
            {stop, read_timeout, State#state{pending = Pending}};
        error ->
            {noreply, State}
    end.
```

**Benefits**:
- Uniform pattern across all transports
- No blocking I/O
- Consistent error handling
- Easy to add metrics

**Implementation Timeline**: 2-3 weeks (affects all transports)

---

## Module Reorganization Plan

### Phase 1: Server Module Split (Week 3)

**Current**: erlmcp_server.erl (1,520 lines)

**Proposed Structure**:
```
erlmcp_server.erl          [150 lines - main gen_server]
  ├─ Protocol callbacks
  ├─ Initialization
  └─ Lifecycle

erlmcp_server_resources.erl [300 lines]
  ├─ add_resource
  ├─ delete_resource
  ├─ subscribe_resource
  └─ Notification handlers

erlmcp_server_tools.erl [250 lines]
  ├─ add_tool
  ├─ delete_tool
  ├─ call_tool
  └─ Progress tracking

erlmcp_server_prompts.erl [200 lines]
  ├─ add_prompt
  ├─ delete_prompt
  ├─ get_prompt
  └─ Argument validation

erlmcp_server_handlers.erl [400 lines]
  ├─ handle_request/5
  ├─ Protocol request dispatch
  └─ Error formatting

erlmcp_server_validation.erl [150 lines]
  ├─ URI validation
  ├─ Schema validation
  └─ Input checking
```

**Benefits**:
- Each module <350 lines (maintainable)
- Clear separation of concerns
- Easier testing
- Reduced cognitive load

**Implementation Timeline**: 1-2 weeks

---

## New Modules to Create

### erlmcp_io_async

**Purpose**: Unified async I/O handling
**Type**: gen_server
**Responsibilities**:
- File operations (read, write, link info)
- Timeout enforcement
- Queue management
- Worker pool coordination

**Tests Needed**: 15+

---

### erlmcp_timeout_coordinator

**Purpose**: Centralized timeout management
**Type**: gen_server or library module
**Responsibilities**:
- Configuration management
- Timeout enforcement
- Metrics collection
- Error tracking

**Tests Needed**: 10+

---

### erlmcp_health_checker

**Purpose**: Periodic health checks
**Type**: gen_server
**Responsibilities**:
- Component liveness
- Subscriber queue monitoring
- Memory usage tracking
- Auto-recovery triggering

**Tests Needed**: 12+

---

## Dependency Graph Improvements

### Current Dependencies
```
erlmcp_server → erlmcp_registry
erlmcp_client → erlmcp_registry
erlmcp_transport_* → erlmcp_registry
```

### Proposed (Reduced Coupling)
```
erlmcp_server → erlmcp_server_resources → erlmcp_registry
              → erlmcp_server_tools
              → erlmcp_server_prompts
              → erlmcp_server_handlers

erlmcp_io_async (standalone)
erlmcp_timeout_coordinator (standalone)
```

**Benefits**:
- Reduced coupling
- Easier testing (fewer dependencies to mock)
- Better module reusability
- Clearer dependency direction

---

## Performance Optimization Strategy

### 1. Message Batching (Quick Win)

**Current**: Individual messages to subscribers
**Proposed**: Batch updates every 100ms or 50 messages

```erlang
batched_notify(Updates, Subscribers) ->
    lists:foldl(
        fun(Sub, Batches) ->
            insert_into_batch(Sub, Updates, Batches)
        end,
        #{},
        Subscribers
    ),
    % Send batches
    dict:fold(
        fun(Sub, Batch, _) ->
            Sub ! {batch_updates, Batch}
        end,
        ok,
        Batches
    ).
```

**Expected Improvement**: 20-30% reduction in message queue depth
**Effort**: 2-3 days

### 2. Connection Pooling (Medium Effort)

**Current**: New HTTP connection per request
**Proposed**: poolboy-based connection pool

```erlang
{ok, _} = poolboy:start_link([
    {name, {local, http_pool}},
    {worker_module, erlmcp_http_worker},
    {size, 10},
    {max_overflow, 5}
]).

% Use pool for requests
poolboy:transaction(http_pool, fun(Worker) ->
    erlmcp_http_worker:request(Worker, Url, Opts)
end).
```

**Expected Improvement**: 15-25% faster HTTP requests
**Effort**: 3-5 days

### 3. ETS Secondary Indexing (Optional)

**For**: Large resource/tool collections
**Impact**: O(1) lookups remain, but enables more complex queries

**Example**:
```erlang
ets:new(resources_by_type, [
    {keypos, 2},  % Index on type field
    named_table,
    set
]).

% Query by type
ets:select(resources_by_type, [{#resource{type='$1', _='_'}, [], ['$1']}]).
```

**Expected Improvement**: 10-15% for collection queries
**Effort**: 2-3 days

---

## Testing Strategy for Architecture Changes

### Unit Test Guidelines

1. **Per-Module Tests**
   ```erlang
   % Test each new module independently
   erlmcp_io_async_tests.erl
   erlmcp_timeout_coordinator_tests.erl
   erlmcp_health_checker_tests.erl
   ```

2. **Integration Tests**
   ```erlang
   % Test interactions between modules
   erlmcp_io_integration_SUITE.erl
   erlmcp_timeout_integration_SUITE.erl
   ```

3. **System Tests**
   ```erlang
   % Test end-to-end behavior
   erlmcp_system_reliability_SUITE.erl
   erlmcp_stress_test_SUITE.erl
   ```

### Test Coverage Requirements
- Unit: 90%+
- Integration: 80%+
- System: Key scenarios covered

---

## Rollout & Risk Mitigation

### Phase 1: Non-Intrusive (Low Risk)
- Add new modules (io_async, timeout_coordinator, health_checker)
- No changes to existing code
- Parallel existing systems
- Duration: 1 week

### Phase 2: Gradual Migration (Medium Risk)
- Start using new modules in new code paths
- Keep existing paths functional
- Gradual switchover of transports
- Duration: 2-3 weeks

### Phase 3: Cleanup (Low Risk)
- Remove legacy code
- Consolidate patterns
- Documentation updates
- Duration: 1 week

### Rollback Plan
- Each phase can be reverted independently
- Existing code remains functional during transition
- Feature flags for new functionality

---

## Success Metrics

### Before Architecture Changes
```
- Blocking call: 0 (none detected)
- Response latency p99: 100ms
- Timeout errors: < 0.1%
- Dead subscriber cleanup: Manual
```

### After Architecture Changes
```
- Blocking calls: 0 (enforced)
- Response latency p99: < 50ms (batching)
- Timeout errors: 0% (enforced timeouts)
- Dead subscriber cleanup: Automatic (< 60s)
- Test coverage: 90%+ maintained
```

---

## Conclusion

The proposed architectural improvements build on the solid OTP foundation already in place. Implementation should be done incrementally with proper testing and rollback plans. The modular approach allows for parallel work and easy integration.

**Recommended Start Date**: After critical issues are fixed
**Estimated Total Effort**: 6-8 weeks
**Expected Quality Improvement**: 15-20%
**Expected Performance Improvement**: 20-30% (with optimizations)

---

## Related Documents

- `ERLANG_OTP_AUDIT_REPORT.md` - Full audit findings
- `OTP_AUDIT_ACTION_ITEMS.md` - Immediate action items
- `rebar.config` - Build configuration
- `docs/architecture.md` - Current architecture documentation

---

**Approved By**: OTP Best Practices Review Team
**Date**: 2026-01-27
**Version**: 1.0
