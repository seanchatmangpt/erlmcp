# MCP 2025-11-25 Features - Architecture Design

**Project**: erlmcp (Erlang/OTP MCP SDK)
**Phase**: SPARC Phase 3 (Architecture)
**Date**: 2026-01-29
**Status**: Architecture Design

---

## Overview

This document defines the system architecture for implementing missing MCP 2025-11-25 features. All designs follow OTP supervision principles with proper isolation and fault containment.

---

## Supervision Tree

```
erlmcp_core_sup (one_for_all)
├── erlmcp_server_sup (simple_one_for_one)
│   ├── erlmcp_server (per connection)
│   └── erlmcp_connection_manager
├── erlmcp_task_manager_sup (one_for_one)
│   └── erlmcp_task_manager (singleton)
├── erlmcp_completion_sup (one_for_one)
│   └── erlmcp_completion (singleton)
├── erlmcp_elicitation_sup (one_for_one)
│   └── erlmcp_elicitation (singleton)
├── erlmcp_cancellation_sup (one_for_one)
│   └── erlmcp_cancellation (singleton)
├── erlmcp_progress_sup (one_for_one)
│   └── erlmcp_progress (singleton)
├── erlmcp_sampling_sup (one_for_one)
│   └── erlmcp_sampling (singleton)
└── erlmcp_registry_sup (simple_one_for_one)
    └── erlmcp_registry (per process)
```

---

## Module Decomposition

### New Modules

#### erlmcp_task_manager.erl

**Purpose**: Task lifecycle management (create, list, get, cancel)

**Behavior**: gen_server

**Responsibilities**:
- Task creation with ID generation
- Task state management (pending → working → completed/failed/cancelled)
- Task listing with pagination (cursor-based)
- Task retrieval by ID
- Task cancellation integration
- Progress token generation for tasks
- Concurrent task limit enforcement

**Dependencies**:
- erlmcp_cancellation (for task cancellation)
- erlmcp_progress (for progress updates)
- erlmcp_registry (for process registration)

**API**:
```erlang
start_link() -> {ok, pid()}
create_task(Action, Metadata) -> {ok, TaskId} | {error, term()}
list_tasks(Cursor, Limit) -> {ok, TaskList} | {error, term()}
get_task(TaskId) -> {ok, Task} | {error, not_found}
cancel_task(TaskId, Reason) -> {ok, cancelled} | {error, term()}
get_task_result(TaskId) -> {ok, Result} | {error, term()}
```

**Supervision**: one_for_one (restart on crash, task state persisted)

---

#### erlmcp_completion.erl

**Purpose**: Text completion and suggestion generation

**Behavior**: gen_server

**Responsibilities**:
- Generate completions for resources (file paths, database keys)
- Generate completions for commands (tool names, arguments)
- Rank completions by relevance (frequency, recency, context)
- Cache completion results
- Handle fuzzy matching for typos

**Dependencies**:
- erlmcp_server (for resource access)
- erlmcp_registry (for caching)

**API**:
```erlang
start_link() -> {ok, pid()}
complete(Ref, Argument) -> {ok, CompletionResult} | {error, term()}
invalidate_cache(Key) -> ok
```

**Supervision**: one_for_one (restart on crash, cache rebuilds)

---

#### erlmcp_elicitation.erl

**Purpose**: URL elicitation for client-side permission flows

**Behavior**: gen_server

**Responsibilities**:
- Create elicitation requests (URL generation)
- Handle elicitation completion (value submission)
- Validate elicitation values (URL scheme validation)
- Manage elicitation expiry (timers)
- Notify clients of completion/expiration

**Dependencies**:
- erlmcp_registry (for process registration)
- crypto (for URL signing)

**API**:
```erlang
start_link() -> {ok, pid()}
create(Requests, ClientPid) -> {ok, [Elicitation]} | {error, term()}
complete(ElicitationId, Value) -> {ok, completed} | {error, term()}
get_elicitation(ElicitationId) -> {ok, Elicitation} | {error, not_found}
```

**Supervision**: one_for_one (restart on crash, state recovers from client)

---

### Enhanced Modules

#### erlmcp_server.erl (Changes)

**New Handler Registrations**:
```erlang
%% Task management handlers
register_task_handler(ServerPid, HandlerFun) -> ok

%% Completion handler
handle_completion(Ref, Argument, State) -> {reply, Response, State}

%% Elicitation handler
handle_elicitation_create(Requests, State) -> {reply, Response, State}
handle_elicitation_complete(ElicitationId, Value, State) -> {reply, Response, State}
```

**Integration Points**:
- `handle_call({tasks, create}, ...)` → Forward to erlmcp_task_manager
- `handle_call({completion, complete}, ...)` → Forward to erlmcp_completion
- `handle_call({elicitation, create}, ...)` → Forward to erlmcp_elicitation

---

#### erlmcp_client.erl (Changes)

**New API Functions**:
```erlang
%% Task management
create_task(Client, Action, Metadata) -> {ok, TaskId} | {error, term()}
list_tasks(Client, Cursor, Limit) -> {ok, TaskList} | {error, term()}
get_task(Client, TaskId) -> {ok, Task} | {error, term()}
cancel_task(Client, TaskId, Reason) -> {ok, cancelled} | {error, term()}
get_task_result(Client, TaskId) -> {ok, Result} | {error, term()}

%% Completion
complete(Client, Ref, Argument) -> {ok, CompletionResult} | {error, term()}

%% Elicitation
create_elicitation(Client, Requests) -> {ok, [Elicitation]} | {error, term()}
complete_elicitation(Client, ElicitationId, Value) -> {ok, completed} | {error, term()}
```

---

#### erlmcp_json_rpc.erl (Changes)

**New Message Encoders**:
```erlang
%% Task management
encode_tasks_create_request(Id, Action, Metadata) -> binary()
encode_tasks_list_request(Id, Cursor, Limit) -> binary()
encode_tasks_get_request(Id, TaskId) -> binary()
encode_tasks_cancel_request(Id, TaskId, Reason) -> binary()
decode_tasks_response(Json) -> {ok, Response} | {error, term()}

%% Completion
encode_completion_complete_request(Id, Ref, Argument) -> binary()
decode_completion_response(Json) -> {ok, Response} | {error, term()}

%% Elicitation
encode_elicitation_create_request(Id, Requests) -> binary()
decode_elicitation_response(Json) -> {ok, Response} | {error, term()}
```

---

## Data Flow Diagrams

### Task Creation Flow

```
Client                          erlmcp_client                  erlmcp_server                    erlmcp_task_manager
  |                                   |                                |                                    |
  |--create_task(Action)------------->|                               |                                    |
  |                                   |--JSON-RPC request------------->|                                    |
  |                                   |                               |--create_task(Action)----------->|
  |                                   |                               |                                    |--[spawn task execution]
  |                                   |                               |                                    |
  |                                   |                               |--{ok, TaskId}--------------------|
  |                                   |<--JSON-RPC response-----------|                                    |
  |<--{ok, TaskId}--------------------|                               |                                    |
  |                                   |                               |                                    |
  |                                   |                               |           [Task Execution]          |
  |                                   |                               |<--progress notifications---------|
  |                                   |<--progress notifications-------|                                    |
  |<--progress notifications---------|                               |                                    |
  |                                   |                               |                                    |
  |                                   |                               |<--task completed------------------|
  |                                   |<--notification-----------------|                                    |
  |<--notification--------------------|                               |                                    |
```

### Completion Flow

```
Client                          erlmcp_client                  erlmcp_server                    erlmcp_completion
  |                                   |                                |                                    |
  |--complete(Ref, Arg)-------------->|                               |                                    |
  |                                   |--JSON-RPC request------------->|                                    |
  |                                   |                               |--complete(Ref, Arg)------------->|
  |                                   |                               |                                    |--[check cache]
  |                                   |                               |                                    |--[generate candidates]
  |                                   |                               |                                    |--[rank results]
  |                                   |                               |                                    |
  |                                   |                               |--{ok, Result}-------------------->|
  |                                   |<--JSON-RPC response-----------|                                    |
  |<--{ok, CompletionResult}---------|                               |                                    |
```

### Elicitation Flow

```
Client                          erlmcp_client                  erlmcp_server                    erlmcp_elicitation
  |                                   |                                |                                    |
  |--create_elicitation(Requests)---->|                               |                                    |
  |                                   |--JSON-RPC request------------->|                                    |
  |                                   |                               |--create(Requests, ClientPid)---->|
  |                                   |                               |                                    |--[generate URLs]
  |                                   |                               |                                    |--[start expiry timers]
  |                                   |                               |                                    |
  |                                   |                               |--{ok, [Elicitation]}------------->|
  |                                   |<--JSON-RPC response-----------|                                    |
  |<--{ok, [Elicitation]}-------------|                               |                                    |
  |                                   |                                |                                    |
  |          [User provides URL]       |                                |                                    |
  |--complete_elicitation(Id, Url)----|                               |                                    |
  |                                   |--JSON-RPC request------------->|                                    |
  |                                   |                               |--complete(Id, Url)-------------->|
  |                                   |                               |                                    |--[validate URL]
  |                                   |                               |                                    |--[update status]
  |                                   |                               |                                    |
  |                                   |                               |--{ok, completed}----------------->|
  |                                   |<--JSON-RPC response-----------|                                    |
  |<--{ok, completed}-----------------|                               |                                    |
```

---

## Module Interaction Matrix

| Module | erlmcp_task_manager | erlmcp_completion | erlmcp_elicitation | erlmcp_cancellation | erlmcp_progress | erlmcp_sampling |
|--------|---------------------|-------------------|---------------------|---------------------|-----------------|-----------------|
| erlmcp_server | Calls (task operations) | Calls (complete) | Calls (create/complete) | Uses (for tool cancel) | Uses (for progress) | Calls (create_message) |
| erlmcp_client | Calls (task API) | Calls (complete API) | Calls (elicitation API) | Receives notifications | Receives notifications | Sets handler |
| erlmcp_json_rpc | Encodes/decodes | Encodes/decodes | Encodes/decodes | Encodes/decodes | Encodes/decodes | Encodes/decodes |
| erlmcp_cancellation | Registers tasks | N/A | N/A | N/A | N/A | N/A |
| erlmcp_progress | Creates tokens | N/A | N/A | N/A | N/A | N/A |
| erlmcp_sampling | N/A | N/A | Calls (include context) | N/A | N/A | N/A |

---

## Failure Scenarios and Recovery

### Scenario 1: Task Manager Crash

**Failure**: erlmcp_task_manager crashes during task execution

**Recovery Strategy**:
1. Supervisor restarts erlmcp_task_manager
2. Task state restored from Mnesia (if persisted) or marked as failed
3. In-progress tasks cancelled via erlmcp_cancellation
4. Clients notified of task failure

**Supervision**: one_for_one with 10 restart intensity, 60 seconds period

### Scenario 2: Completion Cache Corruption

**Failure**: ETS table corrupted or lost

**Recovery Strategy**:
1. Supervisor restarts erlmcp_completion
2. Cache recreated on init
3. Subsequent requests rebuild cache
4. No data loss (cache is ephemeral)

**Supervision**: one_for_one with 5 restart intensity, 30 seconds period

### Scenario 3: Elicitation Timeout Storm

**Failure**: Many elicitations expire simultaneously

**Recovery Strategy**:
1. Use timer wheel for efficient timer management
2. Batch expiry notifications to clients
3. Limit concurrent expiry cleanups
4. Backpressure on new elicitation creation

**Supervision**: one_for_one with unlimited restarts (stateless)

### Scenario 4: Sampling Provider Failure

**Failure**: LLM provider becomes unavailable

**Recovery Strategy**:
1. Circuit breaker opens after N failures
2. Fallback to mock provider for testing
3. Provider health checks with exponential backoff
4. Graceful degradation (return error to client)

**Supervision**: one_for_one with 3 restart intensity, 10 seconds period

---

## Performance Considerations

### Concurrency Model

**Task Manager**:
- Tasks execute in separate processes (spawn_link)
- No blocking operations in gen_server callbacks
- Async task execution with result notification

**Completion**:
- ETS table with read_concurrency for cache
- Ranking computation in separate process
- Bounded completion generation (max 100 candidates)

**Elicitation**:
- Stateless operations (all state in records)
- Timer wheel for efficient expiry management
- Batch notifications to reduce message passing

### Bottleneck Identification

**Potential Bottlenecks**:
1. Task listing with large task sets → Use pagination and cursor navigation
2. Completion ranking with many candidates → Pre-compute scores, cache results
3. Elicitation expiry timers → Use timer wheel, avoid per-timer process

**Mitigation Strategies**:
1. Limit concurrent tasks (configurable, default: 1000)
2. Cache completion results (TTL: 5 minutes)
3. Batch expiry processing (every 10 seconds)

### Memory Management

**Task Storage**:
- In-memory ETS table for active tasks
- Mnesia for persistence (optional)
- Archive completed tasks after 24 hours

**Completion Cache**:
- ETS table with auto-expiry
- Max cache size: 10,000 entries
- LRU eviction when full

**Elicitation State**:
- In-memory map for active elicitations
- Auto-cleanup 60s after expiry/completion
- Max active elicitations: 100 per client

---

## Security Considerations

### Input Validation

**Task Manager**:
- Validate Action is binary (max 1024 bytes)
- Validate Metadata is map (max 10 keys)
- Sanitize TaskId format (UUID or integer)

**Completion**:
- Validate URI scheme (file://, db://, http://, https://)
- Sanitize file paths (prevent directory traversal)
- Limit argument value length (max 256 bytes)

**Elicitation**:
- Validate URL scheme (https:// required)
- Sanitize URL parameters
- Limit elicitation description length (max 512 bytes)

### Access Control

**Capability Checks**:
```erlang
%% Server-side capability check
handle_call({tasks, create}, _From, State) ->
    case check_capability(State, tasks) of
        true -> handle_task_create(...);
        false -> {reply, {error, capability_not_supported}, State}
    end.
```

**Rate Limiting**:
- Task creation: 10 tasks/sec per client
- Completion requests: 100 requests/sec per client
- Elicitation creation: 5 requests/sec per client

### Audit Logging

**Log Events**:
- Task creation, cancellation, completion
- Completion requests (with ref and argument)
- Elicitation creation, completion, expiry
- All failures and errors

**Log Format**:
```erlang
logger:info("Task created: id=~p, action=~p, client=~p",
            [TaskId, Action, ClientPid]).
```

---

## Testing Architecture

### Unit Test Structure

```
apps/erlmcp_core/test/
├── erlmcp_task_manager_tests.erl
├── erlmcp_completion_tests.erl
├── erlmcp_elicitation_tests.erl
├── erlmcp_task_manager_integration_tests.erl
└── erlmcp_mcp_2025_SUITE.erl
```

### Integration Test Structure

```
apps/erlmcp_core/test/
└── erlmcp_mcp_2025_SUITE.erl
    ├── task_creation_test
    ├── task_cancellation_test
    ├── completion_resource_test
    ├── completion_command_test
    ├── elicitation_creation_test
    ├── elicitation_completion_test
    └── end_to_end_workflow_test
```

### Performance Benchmarks

```
bench/
├── erlmcp_task_manager_bench.erl
│   ├── task_creation_throughput
│   ├── task_listing_latency
│   └── concurrent_task_execution
├── erlmcp_completion_bench.erl
│   ├── completion_generation_latency
│   ├── cache_hit_rate
│   └── ranking_performance
└── erlmcp_elicitation_bench.erl
    ├── elicitation_creation_throughput
    ├── expiry_timer_efficiency
    └── url_validation_performance
```

---

## Deployment Considerations

### Configuration

**sys.config additions**:
```erlang
{erlmcp, [
    {task_manager, [
        {max_concurrent_tasks, 1000},
        {task_persistence, mnesia},  % mnesia | ets
        {task_archive_ttl, 86400000}  % 24 hours
    ]},
    {completion, [
        {cache_ttl, 300000},  % 5 minutes
        {cache_max_size, 10000},
        {ranking_weights, #{
            frequency => 0.4,
            recency => 0.3,
            context => 0.3
        }}
    ]},
    {elicitation, [
        {expiry_ttl, 300000},  % 5 minutes
        {max_elicitations_per_client, 100},
        {base_url, <<"https://erlmcp.local/elicit">>}
    ]}
]}.
```

### Monitoring

**Metrics to Track**:
- Task creation rate (tasks/sec)
- Task completion time (p50, p95, p99)
- Completion cache hit rate (%)
- Elicitation success rate (%)
- Module restart counts (per supervisor)

**Telemetry Events**:
```erlang
telemetry:execute(
    [erlmcp, task, created],
    #{count => 1},
    #{action => Action}
).
```

### Upgrade Strategy

**AppUp File**:
```erlang
{erlmcp_task_manager, soft_purge, soft_purge, []}.
{erlmcp_completion, soft_purge, soft_purge, []}.
{erlmcp_elicitation, soft_purge, soft_purge, []}.
```

**Data Migration**:
- Task state format changes: Mnesia schema migration
- Completion cache: Clear and rebuild (safe)
- Elicitation state: Clear expired, migrate active

---

## Next Steps

1. **Review** architecture design with team
2. **Validate** supervision tree structure
3. **Create** module stubs with behavior exports
4. **Implement** supervisor children
5. **Write** initial unit tests
6. **Proceed** to Phase 4 (Refinement - Implementation)
