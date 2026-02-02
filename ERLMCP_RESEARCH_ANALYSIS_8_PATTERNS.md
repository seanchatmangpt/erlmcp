# erlmcp Codebase Research: 8 Reusable Patterns for Multi-Agent Systems

**Research Date**: 2026-02-02
**Scope**: erlmcp_core (164 modules, 97 core modules)
**Focus**: Architecture patterns suitable for multi-agent orchestration

---

## Executive Summary

This research documents 8 critical architectural patterns found in erlmcp that are directly reusable for building distributed multi-agent systems. Each pattern addresses a core concern in agent orchestration: process isolation, routing, state management, error handling, observability, and resilience.

**Key Insight**: erlmcp implements the "process-per-connection" model using OTP supervisors and gproc routing, enabling 40-50K connections/node with automatic supervision. This same pattern scales to agent-per-task scenarios.

---

## Pattern 1: Process-Per-Connection Isolation

### Location
- `apps/erlmcp_core/src/erlmcp_server_sup.erl` (lines 1-47)
- `apps/erlmcp_core/src/erlmcp_client_sup.erl` (lines 1-71)
- `apps/erlmcp_core/src/erlmcp_server.erl` (lines 1-150, gen_server skeleton)

### How It Works
```erlang
%% Template for dynamic instance creation (simple_one_for_one)
init([]) ->
    SupFlags = #{strategy => simple_one_for_one,  % Dynamic children
                 intensity => 5,                   % 5 restarts
                 period => 60},                    % per 60 seconds

    ChildSpecs = [#{id => erlmcp_server,
                    start => {erlmcp_server, start_link, [undefined, #{}]},
                    restart => temporary,  % No auto-restart on crash
                    shutdown => 5000,      % 5s graceful shutdown
                    type => worker,
                    modules => [erlmcp_server]}],

    {ok, {SupFlags, ChildSpecs}}.
```

### Key Characteristics
- **Strategy**: `simple_one_for_one` - allows dynamic spawning of children
- **Restart Policy**: `temporary` - children NOT restarted on crash (process-per-connection dies with connection)
- **Isolation**: Each connection/agent gets its own gen_server process with its own state
- **Lifecycle**: Process crashes don't affect siblings (let-it-crash principle)

### Reuse for Multi-Agent Systems
```
Agent Pattern:
┌─ Agent Supervisor (simple_one_for_one)
│  ├─ Agent₁ gen_server (state: tasks, pending_requests)
│  ├─ Agent₂ gen_server
│  └─ AgentN gen_server
│
Each agent is isolated - failures don't cascade
```

### Data Flow
```
start_agent(Config)
  → supervisor:start_child(agent_sup, [Config])
  → erlmcp_server:start_link(AgentId, Config)
    → gen_server loop with #state{}
```

### Supervision Tree Integration
Located in tier 2 of 3-tier supervision:
```
erlmcp_sup (one_for_one)
  ├─ TIER 1: erlmcp_core_sup (infrastructure)
  ├─ TIER 2: erlmcp_server_sup (simple_one_for_one) ← Process isolation
  └─ TIER 3: erlmcp_observability_sup (isolated)
```

---

## Pattern 2: Registry-Based Routing (gproc)

### Location
- `apps/erlmcp_core/src/erlmcp_registry.erl` (lines 1-100+)
- `apps/erlmcp_core/src/erlmcp_registry_behavior.erl` (interface)
- `apps/erlmcp_core/src/erlmcp_registry_distributed.erl` (distributed backend)

### How It Works
```erlang
%% Register a server/agent with the registry
register_server(ServerId, ServerPid, Config) ->
    gen_server:call(?MODULE, {register_server, ServerId, ServerPid, Config}).

%% Find a server/agent by ID
find_server(ServerId) ->
    gen_server:call(?MODULE, {find_server, ServerId}).
    % Returns: {ok, {ServerPid, Config}} or {error, not_found}

%% Route a message to a server/agent
route_to_server(ServerId, TransportId, Message) ->
    gen_server:cast(?MODULE, {route_to_server, ServerId, TransportId, Message}).
```

### Key Characteristics
- **Backend**: Two implementations:
  1. **Local**: gproc (O(log N), ~553K msg/s baseline)
  2. **Distributed**: global + pg (slower but cluster-native)
- **Behavior**: Abstract interface allows backend swapping
- **Transport Binding**: Maps transport_id → server_id for bidirectional routing
- **State**: Minimal (only `#{transport_id => server_id}` mapping)

### Registry State Record
```erlang
-record(registry_state, {
    server_transport_map = #{} :: #{transport_id() => server_id()}
}).
```

### Reuse for Multi-Agent Systems
```
Agent Registry Pattern:
┌─ erlmcp_registry gen_server
│  ├─ "agent:task_001" → Pid₁ (Processing)
│  ├─ "agent:task_002" → Pid₂ (Waiting)
│  └─ "agent:task_003" → Pid₃ (Complete)
│
Route messages: find_agent(AgentId) → send_message(Pid, Msg)
```

### Performance Characteristics
```
Local gproc:  553K msg/sec
Distributed:  ~150-200K msg/sec (global locks)
Timeout:      gen_server:call/3 with infinity or 5000ms
```

### API Integration
```erlang
erlmcp_registry:register_server(agent_id, AgentPid, #{role => orchestrator})
erlmcp_registry:find_server(agent_id)  % {ok, {Pid, Config}}
erlmcp_registry:route_message(agent_id, Message)  % cast
```

---

## Pattern 3: 3-Tier Supervision Tree

### Location
- `apps/erlmcp_core/src/erlmcp_sup.erl` (lines 1-208)
- `apps/erlmcp_core/src/erlmcp_core_sup.erl` (lines 1-289)

### Architecture
```
erlmcp_sup (one_for_one, intensity=5, period=60s)
│
├─ TIER 1: erlmcp_core_sup (Registry + Infrastructure)
│   ├─ erlmcp_registry (worker, permanent)
│   ├─ erlmcp_health (worker, permanent)
│   ├─ erlmcp_session_manager (worker, permanent)
│   ├─ erlmcp_cache (worker, permanent)
│   ├─ erlmcp_rate_limiter (worker, permanent)
│   ├─ erlmcp_circuit_breaker (worker, permanent)
│   └─ erlmcp_connection_limiter (worker, permanent)
│
├─ TIER 2: erlmcp_server_sup (simple_one_for_one)
│   └─ [Dynamic server instances - temporary]
│
└─ TIER 3: erlmcp_observability_sup (Isolated)
    ├─ Metrics collectors
    ├─ Health monitors
    └─ Tracing
```

### Why 3 Tiers?
1. **TIER 1 (Infrastructure)**: Foundation with zero external deps
   - Failures restart in isolation
   - New registrations may fail during recovery
   - Recovery: automatic (one_for_one strategy)

2. **TIER 2 (Protocol)**: Dynamic connections
   - Individual failures don't affect others
   - In-flight requests to failed server are lost
   - Recovery: clients reconnect to new server instance

3. **TIER 3 (Observability)**: Isolated monitoring
   - Failures DON'T affect core or protocol layers
   - Monitoring data may be incomplete during recovery
   - Recovery: automatic (one_for_one strategy)

### Key Design Decisions
```erlang
init([]) ->
    SupFlags = #{strategy => one_for_one,  % NOT rest_for_one!
                 intensity => 5,
                 period => 60},
    % NO cascading failures between tiers
```

### Reuse for Multi-Agent Systems
```
Master Supervisor (one_for_one)
  ├─ TIER 1: Coordinator (Registry, State, Auth)
  ├─ TIER 2: Agents (simple_one_for_one for dynamic agents)
  └─ TIER 3: Observability (Metrics, Tracing, Health)
```

---

## Pattern 4: Request Correlation & State Management

### Location
- `apps/erlmcp_core/src/erlmcp_client.erl` (lines 44-66, state record)
- `apps/erlmcp_core/src/erlmcp_request_id.erl` (lines 1-54)
- `apps/erlmcp_core/src/erlmcp_session_manager.erl` (lines 1-100+)

### State Record Pattern
```erlang
%% Client state with request correlation
-record(state, {
    transport :: module(),
    transport_state :: term(),
    phase = pre_initialization :: client_phase(),  % Lifecycle tracking
    capabilities :: #mcp_server_capabilities{},
    request_id = 1 :: request_id(),  % Monotonic counter
    pending_requests = #{} :: #{request_id() => {atom(), pid()}},  % Correlation
    batch_requests = #{} :: #{batch_id() => [{request_id(), binary(), map()}]},
    notification_handlers = #{} :: #{binary() => notification_handler()},
    subscriptions = sets:set() :: sets:set(binary()),
    timeout = 5000 :: timeout(),
    correlation_table :: ets:tid()  % Persistent correlation storage
}).
```

### Request ID Overflow Protection
```erlang
%% Per RPN 720: P0 SECURITY - Prevent request ID exhaustion
safe_increment(RequestId) when RequestId >= ?MAX_SAFE_REQUEST_ID ->
    {error, overflow};
safe_increment(RequestId) ->
    {ok, RequestId + 1}.

check_thresholds(RequestId) ->
    Usage = RequestId / ?MAX_SAFE_REQUEST_ID * 100.0,
    Level = determine_threshold_level(Usage),  % normal|warning|critical|reserved
    {ok, Level, Usage}.
```

### Session Management
```erlang
%% Session data with TTL and replication
-type session_data() :: #{
    id := session_id(),
    created_at := integer(),
    last_accessed := integer(),
    timeout_ms := pos_integer() | infinity,
    metadata := map(),
    replication_ref => reference()
}.

%% Backend selection: ETS (fast) | DETS (durable) | Mnesia (cluster-wide)
-record(state, {
    version = v1 :: state_version(),
    table :: ets:tid(),  % In-memory store
    cleanup_timer :: reference(),  % Auto-expiry
    default_timeout_ms = 3600000,  % 1 hour
    persistent_enabled = false  % Mnesia fallback
}).
```

### Reuse for Multi-Agent Systems
```
Agent State Correlation:
┌─ Agent state record
│  ├─ agent_id :: atom()
│  ├─ task_id :: binary()
│  ├─ pending_tasks = #{} :: #{task_id() => {status(), reference()}}
│  ├─ pending_requests = #{} :: #{request_id() => {target_agent(), deadline()}}
│  └─ correlation_table :: ets:tid()  % For cross-agent tracing
│
Lifecycle tracking:
│  ├─ phase :: initializing | ready | executing | error | shutdown
│  └─ timeout_ref :: reference()
```

### Correlation Table Pattern
```erlang
%% Persistent storage for request correlation across processes
correlation_table = ets:new(agent_correlations,
    [bag, public, {write_concurrency, true}])

%% Store: {request_id, agent_id, target_agent_id, timestamp, deadline}
ets:insert(correlation_table,
    {ReqId, AgentId, TargetAgent, Now, Deadline})

%% Query: Find all pending requests for an agent
ets:match(correlation_table, {'$1', AgentId, '_', '_', '_'})
```

---

## Pattern 5: Error Handling & Standardized Responses

### Location
- `apps/erlmcp_core/src/erlmcp_errors.erl` (lines 1-100+)
- `apps/erlmcp_core/src/erlmcp_json_rpc.erl` (lines 1-80+)

### Error Code Categories
```erlang
%% JSON-RPC standard errors
error_code(parse_error) -> -32700;
error_code(invalid_request) -> -32600;
error_code(method_not_found) -> -32601;
error_code(invalid_params) -> -32602;
error_code(internal_error) -> -32603;

%% MCP core errors
error_code(resource_not_found) -> -32001;
error_code(tool_not_found) -> -32002;
error_code(not_initialized) -> -32005;

%% Refusal errors (1001-1089)
refusal_reason(1001) -> <<"Content violates policy">>;
refusal_reason(1003) -> <<"Rate limit exceeded">>;
refusal_reason(1005) -> <<"Permission denied">>;
```

### Error Response Format
```erlang
%% Encode error response
encode_error_response(Id, Code, Message, Data) ->
    Error = build_error_object(Code, Message, Data),
    Response = #json_rpc_response{id = Id, error = Error},
    encode_message(Response).

%% Example output:
% {
%   "jsonrpc": "2.0",
%   "error": {
%     "code": -32601,
%     "message": "Method not found",
%     "data": {"requested_method": "nonexistent"}
%   },
%   "id": 42
% }
```

### Refusal Error Pattern
```erlang
%% Check if error is a refusal
is_refusal_error(Code) when Code >= 1001, Code =< 1089 -> true;
is_refusal_error(_) -> false.

%% Refusal codes for LLM safety
1001 => <<"Content violates policy">>
1002 => <<"Violates safety guidelines">>
1005 => <<"Permission denied">>
1007 => <<"Unsupported operation">>
1008 => <<"Temporarily unavailable">>
```

### Reuse for Multi-Agent Systems
```erlang
%% Agent-specific error codes (9000-9999)
error_code(agent_not_found) -> -9001;
error_code(agent_timeout) -> -9002;
error_code(agent_capacity_exceeded) -> -9003;
error_code(task_dependency_failed) -> -9004;
error_code(agent_unreachable) -> -9005;

%% Standard error response across all agents
format_agent_error(AgentId, Code, Message, Context) ->
    #{<<"agent_id">> => AgentId,
      <<"code">> => Code,
      <<"message">> => Message,
      <<"context">> => Context,
      <<"timestamp">> => erlang:system_time(millisecond)}.
```

---

## Pattern 6: JSON-RPC Message Protocol & Serialization

### Location
- `apps/erlmcp_core/src/erlmcp_json_rpc.erl` (lines 1-100+)
- `apps/erlmcp_core/src/erlmcp_json_codec.erl`
- `apps/erlmcp_core/src/erlmcp_message_parser.erl`

### Message Types
```erlang
-type json_rpc_message() ::
    #json_rpc_request{} |
    #json_rpc_response{} |
    #json_rpc_notification{}.

%% Request (requires id)
encode_request(Id, Method, Params) ->
    Request = #json_rpc_request{id = Id, method = Method, params = Params},
    encode_message(Request).
% {"jsonrpc":"2.0","id":42,"method":"list_resources","params":{}}

%% Response (echoes id)
encode_response(Id, Result) ->
    Response = #json_rpc_response{id = Id, result = Result},
    encode_message(Response).
% {"jsonrpc":"2.0","id":42,"result":{}}

%% Notification (NO id)
encode_notification(Method, Params) ->
    Notification = #json_rpc_notification{method = Method, params = Params},
    encode_message(Notification).
% {"jsonrpc":"2.0","method":"resource_updated","params":{}}
```

### Batch Message Handling
```erlang
is_batch_request(Message) ->
    case Message of
        [_ | _] -> true;  % List of messages = batch
        _ -> false
    end.

encode_batch(Requests) ->
    % Encode list of requests as JSON array
    JsonMessages = lists:map(fun encode_message/1, Requests),
    jsx:encode(JsonMessages).
```

### Message Size Validation
```erlang
%% Gap #45: Message Size Limits
decode_message(Json, TransportType) when is_binary(Json) ->
    case erlmcp_message_size:validate(Json, TransportType) of
        ok -> decode_json_rpc(Json);
        {error, Reason} -> {error, Reason}
    end.
```

### Reuse for Multi-Agent Systems
```erlang
%% Agent RPC format (extends JSON-RPC 2.0)
-record(agent_request, {
    jsonrpc = <<"2.0">> :: binary(),
    id :: pos_integer() | binary(),
    method :: binary(),              % "agent.execute_task", "agent.get_status"
    params :: map(),
    agent_id :: binary(),             % NEW: target agent
    from_agent_id :: binary(),        % NEW: source agent
    priority :: high | normal | low,  % NEW: task priority
    deadline :: integer()             % NEW: unix ms timestamp
}).

encode_agent_request(Id, Method, Params, AgentId, Priority) ->
    Request = #agent_request{
        id = Id,
        method = Method,
        params = Params,
        agent_id = AgentId,
        from_agent_id = erlang:node(),
        priority = Priority,
        deadline = erlang:system_time(millisecond) + 30000
    },
    jsx:encode(request_to_map(Request)).
```

---

## Pattern 7: Observability & Health Monitoring

### Location
- `apps/erlmcp_core/src/erlmcp_health.erl` (lines 1-80+)
- `apps/erlmcp_core/src/erlmcp_connection_monitor.erl` (lines 1-80+)
- `apps/erlmcp_core/src/erlmcp_cache.erl` (lines 1-100+)

### Health Check Pattern
```erlang
%% Process-per-health-check pattern (Joe Armstrong principle)
-record(state, {
    checks :: #{check_name() => check_fun()}  % {module, function, args}
}).

%% Register health checks
register_check(registry, {erlmcp_registry, get_pid, []}).
register_check(session_manager, {erlmcp_session_manager, list_sessions, []}).

%% Run all checks
check() ->
    gen_server:call(?MODULE, check).
% Returns: #{
%   healthy => true | false,
%   checks => #{
%     registry => healthy,
%     session_manager => healthy
%   }
% }
```

### Connection Monitoring (gen_statem)
```erlang
%% State machine for monitoring (not gen_server!)
-behaviour(gen_statem).

callback_mode() -> state_functions.

%% States: disconnected → connecting → connected → reconnecting
disconnected(enter, _OldState, Data) ->
    % Initialize ETS table
    ets:new(erlmcp_connections, [bag, public, ...]),
    {next_state, connecting, Data}.

connected(enter, _OldState, Data) ->
    % Normal operation - monitoring active connections
    {next_state, connected, Data}.

%% Leak detection
-define(DEFAULT_LEAK_THRESHOLD, 100).  % connections/minute
is_leak_detected() ->
    ConnectionsPerMinute = calculate_growth_rate(),
    ConnectionsPerMinute > ?DEFAULT_LEAK_THRESHOLD.
```

### Metrics Collection (3-Tier Cache)
```erlang
-record(state, {
    %% ... cache configuration ...
    stats = #{
        hits => 0,           % Total successful reads
        misses => 0,         % Total failed reads
        l1_hits => 0,       % ETS layer hits
        l2_hits => 0,       % Mnesia layer hits
        l3_hits => 0,       % External cache hits
        evictions => 0,     % LRU evictions
        expirations => 0,   % TTL expirations
        writes => 0,        % Total writes
        deletes => 0        % Total deletes
    } :: map()
}).

stats() ->
    gen_server:call(?MODULE, stats).
% Returns: #{hit_rate => 0.85, miss_rate => 0.15, eviction_rate => 0.02}
```

### Reuse for Multi-Agent Systems
```erlang
%% Agent Health Check
agent_health_check(AgentId) ->
    case whereis(AgentId) of
        undefined -> {error, not_found};
        Pid ->
            {ProcessInfo} = process_info(Pid, [
                message_queue_len,  % Backlog indicator
                heap_size,          % Memory usage
                status              % running | waiting | ...
            ]),
            Status = case {ProcessInfo['message_queue_len'], ProcessInfo['heap_size']} of
                {Q, H} when Q > 1000, H > 50000000 -> degraded;
                {Q, _} when Q > 10000 -> unhealthy;
                _ -> healthy
            end,
            {ok, #{
                status => Status,
                queue_depth => ProcessInfo['message_queue_len'],
                heap_mb => ProcessInfo['heap_size'] / 1000000
            }}
    end.

%% Agent Correlation Tracing
trace_agent_request(RequestId, SourceAgent, TargetAgent) ->
    TraceInfo = #{
        request_id => RequestId,
        source => SourceAgent,
        target => TargetAgent,
        started_at => erlang:system_time(millisecond),
        hops => [SourceAgent, TargetAgent]
    },
    erlmcp_cache:put(<<"trace_", RequestId/binary>>, TraceInfo, {ttl, 3600}).
```

---

## Pattern 8: Stateful Session & Cache Management

### Location
- `apps/erlmcp_core/src/erlmcp_session_manager.erl` (lines 1-100+)
- `apps/erlmcp_core/src/erlmcp_session_ets.erl` | `erlmcp_session_dets.erl` | `erlmcp_session_mnesia.erl`
- `apps/erlmcp_core/src/erlmcp_cache.erl` (lines 1-100+)

### Session Manager Architecture
```erlang
%% Backend abstraction (swappable)
-spec session_backend() :: ets | dets | mnesia.

%% Session data with automatic TTL cleanup
-record(state, {
    version = v1 :: state_version(),
    table :: ets:tid(),                  % In-memory index
    cleanup_timer :: reference(),        % Expiry timer
    cleanup_interval_ms = 60000,        % Every 1 minute
    default_timeout_ms = 3600000,       % Sessions expire after 1 hour
    persistent_enabled = false           % Mnesia replication
}).

%% Session record for Mnesia
-record(persistent_session, {
    session_id :: binary(),
    session :: map(),
    created_at :: integer(),
    last_accessed :: integer(),
    ttl :: integer()
}).
```

### 3-Tier Cache Architecture
```erlang
%% Layer 1: ETS (in-process, no serialization)
% Speed: ~1M ops/sec
% Scope: Local node only
% Eviction: LRU when max_size exceeded

%% Layer 2: Mnesia (durable, replicated)
% Speed: ~100K ops/sec
% Scope: Cluster-wide
% Eviction: TTL-based expiration

%% Layer 3: External (Redis/Memcached)
% Speed: ~10K ops/sec (network latency)
% Scope: Unlimited
% Eviction: Server-managed

-record(cache_entry, {
    key :: cache_key(),
    value :: cache_value(),
    level :: cache_level(),              % l1 | l2 | l3
    inserted_at :: integer(),            % monotonic time (us)
    expires_at :: integer() | infinity,  % TTL deadline
    access_count :: non_neg_integer(),  % LRU tracking
    last_accessed :: integer(),
    etag :: binary() | undefined,
    tags :: [binary()],                 % For invalidation
    dependencies :: [cache_key()],      % Cascade invalidation
    strategy :: cache_strategy()        % ttl | lru | write_through | write_back
}).
```

### Tag-Based Invalidation
```erlang
%% Invalidate all cache entries with a given tag
invalidate_by_tag(Tag) ->
    gen_server:call(?MODULE, {invalidate_by_tag, Tag}).

%% Example: Invalidate all user-related caches
put(<<"user_123_profile">>, Profile, #{tags => [<<"user_123">>, <<"profiles">>]}),
put(<<"user_123_settings">>, Settings, #{tags => [<<"user_123">>, <<"settings">>]}),

invalidate_by_tag(<<"user_123">>),  % Clears both entries
```

### Reuse for Multi-Agent Systems
```erlang
%% Agent State Cache (L1: local, L2: replicated, L3: external)
-record(agent_state_cache, {
    agent_id :: binary(),
    state :: #{
        tasks_pending => [task_id()],
        tasks_completed => [task_id()],
        memory_usage => integer(),
        cpu_usage => float(),
        last_heartbeat => integer()
    },
    tags => [<<"agent_id">>, <<"status">>, <<"task_pending">>],
    dependencies => [<<"agent_registry">>],  % Invalidate when registry changes
    ttl => 5 * 60 * 1000  % 5 minute cache
}).

%% Agent Correlation Cache
put(<<"correlation_", RequestId/binary>>, #{
    request_id => RequestId,
    initiator => AgentId,
    hops => [Agent1, Agent2, Agent3],
    started_at => Now,
    deadline => Now + 30000
}, #{
    tags => [<<"correlation">>, AgentId],
    ttl => 30000  % Clear after deadline
}).

%% Fast session lookup for agent state
erlmcp_session_manager:create_session(#{
    agent_id => AgentId,
    role => orchestrator,
    capabilities => [execute, delegate, observe],
    context => RequestContext
}, infinity, #{persistent => true}).  % Replicate to Mnesia
```

---

## Integration: Multi-Agent System Reference Architecture

### Combining All 8 Patterns
```
┌─────────────────────────────────────────────────────────────────┐
│ Master Agent Supervisor (one_for_one) [Pattern 3]              │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  ┌─ TIER 1: Core Infrastructure                               │
│  │  ├─ Agent Registry (gproc) [Pattern 2]                     │
│  │  ├─ Session Manager [Pattern 8]                           │
│  │  ├─ Cache Manager (3-tier) [Pattern 8]                    │
│  │  ├─ Rate Limiter                                           │
│  │  └─ Circuit Breaker                                        │
│  │                                                             │
│  ├─ TIER 2: Agent Instances (simple_one_for_one) [Pattern 1] │
│  │  ├─ Agent₁ gen_server                                      │
│  │  │  ├─ state: #agent_state{} [Pattern 4]                  │
│  │  │  ├─ request_correlation: ets:tid() [Pattern 4]         │
│  │  │  └─ error_handler: erlmcp_errors [Pattern 5]           │
│  │  │                                                         │
│  │  ├─ Agent₂ gen_server                                      │
│  │  └─ AgentN gen_server                                      │
│  │                                                             │
│  ├─ TIER 3: Observability & Monitoring                        │
│  │  ├─ Health Monitor [Pattern 7]                            │
│  │  ├─ Connection Monitor (gen_statem) [Pattern 7]           │
│  │  ├─ Metrics Collector                                      │
│  │  └─ Trace Aggregator                                       │
│  │                                                             │
│  └─ JSON-RPC Protocol Handler [Pattern 6]                    │
│     ├─ encode_agent_request/5                                 │
│     ├─ decode_agent_response/1                                │
│     └─ handle_batch_requests/1                                │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘

Request Flow:
1. Client calls agent_rpc:execute_task(AgentId, Task) [JSON-RPC, Pattern 6]
2. Registry finds agent (Pattern 2)
3. Message routed to agent process (Pattern 1)
4. Agent state updated with pending request (Pattern 4)
5. Response cached with TTL tags (Pattern 8)
6. Health monitor tracks queue depth (Pattern 7)
7. Error formatted and returned (Pattern 5)
```

---

## Implementation Checklist

### To build a distributed multi-agent system:

- [ ] **Pattern 1**: Create agent supervisor with `simple_one_for_one` strategy
- [ ] **Pattern 2**: Implement agent registry with gproc (local) or global+pg (distributed)
- [ ] **Pattern 3**: Design 3-tier supervision (Core, Agents, Observability)
- [ ] **Pattern 4**: Add request correlation tables (ETS) with UUID tracking
- [ ] **Pattern 5**: Define agent-specific error codes (-9001 to -9999)
- [ ] **Pattern 6**: Extend JSON-RPC with agent_id, priority, deadline fields
- [ ] **Pattern 7**: Deploy health checks and connection monitors
- [ ] **Pattern 8**: Configure session backend (ETS/DETS/Mnesia) and 3-tier cache

### Configuration in sys.config:
```erlang
{erlmcp, [
    {cluster_enabled, true},
    {registry_backend, distributed},  % global + pg
    {session_backend, mnesia},         % Replicated sessions
    {cache_levels, [l1, l2, l3]},     % 3-tier caching
    {health_checks, [
        {agents, {erlmcp_health, check_agents, []}},
        {registry, {erlmcp_registry, get_pid, []}}
    ]}
]}.
```

---

## Performance Baselines (Jan 2026)

| Metric | Value | Notes |
|--------|-------|-------|
| Registry (gproc) | 553K msg/sec | Local, O(log N) |
| Registry (distributed) | 150-200K msg/sec | global locks impact |
| Connections/node | 40-50K | Sustained |
| Cache L1 (ETS) | ~1M ops/sec | In-process |
| Cache L2 (Mnesia) | ~100K ops/sec | Replicated |
| Cache L3 (Redis) | ~10K ops/sec | Network latency |
| Health check | <1ms | Process call overhead |
| Session lookup (ETS) | <0.1ms | Direct memory access |

---

## References

### Key Files for Further Study
1. **Supervision**: `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_sup.erl` (lines 147-208)
2. **Registry**: `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_registry.erl` (full)
3. **Server Gen_Server**: `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_server.erl` (lines 1-150)
4. **Client Gen_Server**: `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_client.erl` (lines 1-120)
5. **JSON-RPC**: `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_json_rpc.erl` (lines 1-100)
6. **Errors**: `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_errors.erl` (full)
7. **Health**: `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_health.erl` (lines 1-80)
8. **Cache**: `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_cache.erl` (lines 1-100)
9. **Sessions**: `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_session_manager.erl` (lines 1-100)
10. **Monitoring**: `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_connection_monitor.erl` (lines 1-80)

### CLAUDE.md Reference
- Supervision patterns: Joe Armstrong's "let-it-crash" principle
- Architecture: OTP 28.3.1 behaviors (gen_server, gen_statem, supervisor)
- Quality gates: Compile, Test, Coverage, Dialyzer, Xref
- Coordination: process-per-connection with gproc routing

---

**Analysis Complete**
Document created: `/home/user/erlmcp/ERLMCP_RESEARCH_ANALYSIS_8_PATTERNS.md`
