# erlmcp 100% MCP Specification Support - Architecture Design

**Version**: 3.0.0 Target
**Date**: 2026-02-02
**Status**: Architectural Design
**MCP Specification**: 2025-11-25 (100% compliance target)

---

## Executive Summary

This document defines the complete architecture for achieving 100% Model Context Protocol (MCP) specification compliance in erlmcp v3.0.0. The design addresses all 65 specification features, 89 refusal codes, 16 core capabilities, and performance targets while maintaining OTP principles.

**Current State**: 65% compliance (42/65 features at ≥80%)
**Target State**: 95%+ compliance (62/65 features at ≥80%)
**Key Innovation**: 4-tier supervision architecture with dynamic protocol routing

**Critical Design Goals**:
1. **100% Protocol Coverage**: All 16 MCP capabilities fully implemented
2. **Zero Performance Regression**: p99 latency <1ms maintained
3. **OTP Compliance**: Supervision trees, let-it-crash, bulkhead pattern
4. **Backward Compatibility**: Existing API unchanged
5. **SONA Integration**: Claude-flow routing hooks

---

## 1. OVERALL SYSTEM ARCHITECTURE

### 1.1 High-Level Component View

```
┌──────────────────────────────────────────────────────────────────┐
│                     erlmcp v3.0.0 Architecture                   │
├──────────────────────────────────────────────────────────────────┤
│                                                                  │
│  ┌────────────────────────────────────────────────────────┐    │
│  │         TIER 1: CORE FOUNDATION                        │    │
│  │  (Registry, Infrastructure, Resource Guards)           │    │
│  │  • erlmcp_registry (gproc)                             │    │
│  │  • erlmcp_session_manager                              │    │
│  │  • erlmcp_connection_limiter                           │    │
│  │  • erlmcp_rate_limiter                                 │    │
│  │  • erlmcp_circuit_breaker                              │    │
│  └────────────────────────────────────────────────────────┘    │
│                          ↓                                      │
│  ┌────────────────────────────────────────────────────────┐    │
│  │         TIER 2: PROTOCOL LAYER (NEW)                   │    │
│  │  (Method Registry, Capability Manager, Schema Cache)   │    │
│  │  • erlmcp_method_registry      ← Dynamic routing      │    │
│  │  • erlmcp_capability_manager   ← Feature flags        │    │
│  │  • erlmcp_schema_cache         ← Compiled schemas     │    │
│  │  • erlmcp_error_mapper         ← 89 refusal codes     │    │
│  │  • erlmcp_batch_processor      ← Async batching       │    │
│  │  • erlmcp_sona_router          ← Claude-flow bridge   │    │
│  └────────────────────────────────────────────────────────┘    │
│                          ↓                                      │
│  ┌────────────────────────────────────────────────────────┐    │
│  │         TIER 3: SERVICES & HANDLERS                    │    │
│  │  (Servers, Clients, Capabilities, Sessions)            │    │
│  │  • erlmcp_server_sup (simple_one_for_one)              │    │
│  │  • erlmcp_client_sup (simple_one_for_one)              │    │
│  │  • erlmcp_tasks_manager        ← Async tasks          │    │
│  │  • erlmcp_sampling_coordinator ← LLM integration      │    │
│  │  • erlmcp_elicitation_server   ← User interaction     │    │
│  │  • erlmcp_completion_server    ← Autocomplete         │    │
│  └────────────────────────────────────────────────────────┘    │
│                          ↓                                      │
│  ┌────────────────────────────────────────────────────────┐    │
│  │         TIER 4: OBSERVABILITY (ISOLATED)               │    │
│  │  (Metrics, Traces, Health, Chaos, Audit)               │    │
│  │  • erlmcp_metrics                                      │    │
│  │  • erlmcp_health_monitor                               │    │
│  │  • erlmcp_audit_log                                    │    │
│  │  • erlmcp_chaos (resilience testing)                   │    │
│  └────────────────────────────────────────────────────────┘    │
│                                                                  │
│  ┌────────────────────────────────────────────────────────┐    │
│  │         TRANSPORT LAYER (Separate App)                 │    │
│  │  • erlmcp_transport_stdio                              │    │
│  │  • erlmcp_transport_tcp                                │    │
│  │  • erlmcp_transport_http                               │    │
│  │  • erlmcp_transport_ws                                 │    │
│  │  • erlmcp_transport_sse                                │    │
│  │  • erlmcp_session_correlator   ← Cross-transport      │    │
│  └────────────────────────────────────────────────────────┘    │
│                                                                  │
└──────────────────────────────────────────────────────────────────┘
```

### 1.2 Key Architectural Principles

**Joe Armstrong's Principles Applied**:
1. **Process Isolation**: Each capability = separate gen_server
2. **Let It Crash**: No defensive programming, supervisors handle recovery
3. **Message Passing**: All communication via registered processes (gproc)
4. **No Shared State**: Each process owns its state
5. **Supervision Trees**: 4-tier hierarchy with bulkhead isolation

**OTP Patterns**:
- `gen_server`: Protocol workers (registry, cache, capability manager)
- `supervisor`: 4-tier supervision (one_for_one strategy)
- `gen_statem`: Connection state machines (init → connected → ready)
- `gproc`: Process registry with automatic monitoring

---

## 2. TIER 2: PROTOCOL LAYER (NEW ARCHITECTURE)

### 2.1 Protocol Layer Overview

**Purpose**: Centralized protocol routing, capability negotiation, and performance optimization

**Key Innovation**: Dynamic method registry eliminates hardcoded routing

### 2.2 Method Registry System

**Module**: `erlmcp_method_registry` (NEW)
**Behavior**: gen_server
**Supervision**: erlmcp_protocol_sup

**Purpose**:
- Dynamic method registration and routing
- Versioned method support (backward compatibility)
- Handler validation and capability checking
- Method metadata (description, schema, deprecation)

**State**:
```erlang
-record(method_state, {
    methods :: #{binary() => method_handler()},  % Method name → handler
    capabilities :: #{binary() => capability()}, % Required capabilities
    validators :: #{binary() => validator()},    % Input validators
    metadata :: #{binary() => metadata()},       % Method metadata
    stats :: ets:tab()                          % Performance stats (ETS)
}).

-type method_handler() :: #{
    module => module(),
    function => atom(),
    min_arity => non_neg_integer(),
    max_arity => non_neg_integer(),
    capability_required => [atom()],
    schema => jesse:json_schema(),
    deprecated => boolean(),
    version => binary()
}.
```

**API**:
```erlang
%% Registration
-spec register_method(binary(), module(), atom(), map()) -> ok | {error, term()}.
register_method(Method, Module, Function, Opts) ->
    gen_server:call(erlmcp_method_registry,
                    {register, Method, Module, Function, Opts}).

%% Routing
-spec route_method(binary(), map(), map()) ->
    {ok, term()} | {error, {Code :: integer(), Message :: binary()}}.
route_method(Method, Params, Context) ->
    gen_server:call(erlmcp_method_registry, {route, Method, Params, Context}).

%% Capabilities
-spec method_requires_capability(binary(), atom()) -> boolean().
method_requires_capability(Method, Capability) ->
    gen_server:call(erlmcp_method_registry, {requires_capability, Method, Capability}).
```

**Method Registration Examples**:
```erlang
%% Initialize method (always registered)
ok = erlmcp_method_registry:register_method(
    <<"initialize">>,
    erlmcp_initialize_handler,
    handle,
    #{capability_required => [],
      schema => initialize_schema(),
      version => <<"2025-11-25">>}
).

%% Tools/call (requires tools capability)
ok = erlmcp_method_registry:register_method(
    <<"tools/call">>,
    erlmcp_tool,
    call_tool,
    #{capability_required => [tools],
      schema => tool_call_schema(),
      version => <<"2025-11-25">>}
).

%% sampling/createMessage (requires sampling capability)
ok = erlmcp_method_registry:register_method(
    <<"sampling/createMessage">>,
    erlmcp_sampling_coordinator,
    create_message,
    #{capability_required => [sampling],
      schema => sampling_schema(),
      version => <<"2025-11-25">>,
      direction => server_to_client}
).
```

**Performance Optimization**:
- ETS table for O(1) method lookup
- Compiled schema cache (see 2.3)
- Pre-validated handler functions (validated at registration)

### 2.3 Schema Validation Cache

**Module**: `erlmcp_schema_cache` (NEW)
**Behavior**: gen_server + ETS
**Supervision**: erlmcp_protocol_sup

**Purpose**:
- **CRITICAL**: Eliminates 5-20ms schema validation bottleneck
- Caches compiled JSON Schema validators (jesse)
- Invalidates cache when schemas change
- Supports schema versioning

**Problem Statement**:
Current implementation validates against JSON Schema on every request:
- `jesse:validate/3` compiles schema on each call: 5-20ms
- 1000 req/s × 10ms = 10 seconds of CPU time wasted
- p99 latency target <1ms violated

**Solution**:
Pre-compile schemas at registration, cache compiled validators

**State**:
```erlang
-record(schema_cache_state, {
    compiled_schemas :: ets:tab(),  % Binary schema → compiled validator
    schema_versions :: ets:tab(),   % Version tracking
    hit_stats :: #{binary() => {hits, misses}},
    ttl :: non_neg_integer()        % Cache TTL (default: infinity)
}).

-type compiled_schema() :: #{
    schema_hash => binary(),         % SHA-256 of schema
    compiled => jesse:compiled_schema(),
    version => binary(),
    compiled_at => erlang:timestamp()
}.
```

**API**:
```erlang
%% Compile and cache schema
-spec compile_and_cache(binary(), jesse:json_schema()) ->
    {ok, jesse:compiled_schema()} | {error, term()}.
compile_and_cache(SchemaName, Schema) ->
    gen_server:call(erlmcp_schema_cache, {compile, SchemaName, Schema}).

%% Validate using cached schema
-spec validate(binary(), map()) -> ok | {error, term()}.
validate(SchemaName, Data) ->
    case ets:lookup(schema_cache, SchemaName) of
        [{_, CompiledSchema}] ->
            jesse:validate_with_schema(CompiledSchema, Data);
        [] ->
            {error, schema_not_found}
    end.

%% Invalidate cache (when schema updated)
-spec invalidate(binary()) -> ok.
invalidate(SchemaName) ->
    gen_server:cast(erlmcp_schema_cache, {invalidate, SchemaName}).
```

**Performance Impact**:
- Before: 5-20ms per validation (compile + validate)
- After: <0.1ms per validation (ETS lookup + validate only)
- **100-200x speedup** for schema-heavy operations
- Throughput: 50K req/s → 500K req/s (theoretical)

**Cache Invalidation Strategy**:
1. Time-based: TTL expiry (default: infinity for stable schemas)
2. Event-based: Schema update triggers invalidation
3. Manual: `invalidate/1` API call
4. Version-based: Schema version mismatch triggers recompilation

### 2.4 Capability Manager

**Module**: `erlmcp_capability_manager` (NEW)
**Behavior**: gen_server
**Supervision**: erlmcp_protocol_sup

**Purpose**:
- Feature flagging for capabilities
- Capability negotiation during initialize
- Runtime capability checks
- Capability dependencies and conflicts

**State**:
```erlang
-record(capability_state, {
    server_capabilities :: #{atom() => capability_config()},
    client_capabilities :: #{atom() => boolean()},
    dependencies :: #{atom() => [atom()]},  % Capability dependencies
    conflicts :: #{atom() => [atom()]},     % Conflicting capabilities
    experimental :: [atom()]                % Experimental features
}).

-type capability_config() :: #{
    enabled => boolean(),
    version => binary(),
    options => #{atom() => term()},
    required_capabilities => [atom()],
    experimental => boolean()
}.
```

**API**:
```erlang
%% Register server capability
-spec enable_capability(atom(), map()) -> ok | {error, term()}.
enable_capability(Capability, Options) ->
    gen_server:call(erlmcp_capability_manager, {enable, Capability, Options}).

%% Check if capability enabled
-spec capability_enabled(atom()) -> boolean().
capability_enabled(Capability) ->
    gen_server:call(erlmcp_capability_manager, {enabled, Capability}).

%% Negotiate capabilities during initialize
-spec negotiate_capabilities(#{atom() => term()}, #{atom() => term()}) ->
    {ok, #{atom() => term()}} | {error, term()}.
negotiate_capabilities(ServerCaps, ClientCaps) ->
    gen_server:call(erlmcp_capability_manager,
                    {negotiate, ServerCaps, ClientCaps}).
```

**Capability Registration Examples**:
```erlang
%% Enable resources capability with subscription support
ok = erlmcp_capability_manager:enable_capability(
    resources,
    #{subscribe => true,
      listChanged => true,
      version => <<"2025-11-25">>}
).

%% Enable sampling capability (requires client support)
ok = erlmcp_capability_manager:enable_capability(
    sampling,
    #{enabled => true,
      experimental => false}
).

%% Enable tasks capability (experimental)
ok = erlmcp_capability_manager:enable_capability(
    tasks,
    #{enabled => true,
      experimental => true,
      required_capabilities => []}
).
```

**Capability Dependency Resolution**:
```erlang
%% Dependencies
#{resources := {depends_on, []},
  tools := {depends_on, []},
  sampling := {depends_on, []},
  tasks := {depends_on, [experimental]},
  elicitation := {depends_on, [experimental]}}

%% Conflicts
#{stdio_transport := {conflicts_with, [http_transport, ws_transport]}}
```

### 2.5 Error Code Mapper

**Module**: `erlmcp_error_mapper` (NEW)
**Behavior**: Library module (no process)
**Purpose**: Maps Erlang errors to 89 MCP refusal codes

**API**:
```erlang
%% Map Erlang error to MCP error code
-spec map_error(term()) -> {Code :: integer(), Message :: binary(), Data :: map()}.
map_error({not_found, resource}) -> {1046, <<"Resource not found">>, #{}};
map_error({rate_limited, Details}) -> {1056, <<"Rate limit exceeded">>, Details};
map_error({auth_failed, Reason}) -> {1011, <<"Authentication failed">>, #{reason => Reason}};
map_error(timeout) -> {1069, <<"Request timeout">>, #{}};
map_error({validation_failed, Field}) ->
    {1021, <<"Invalid params">>, #{field => Field}};
...
```

**All 89 Refusal Codes Mapped**:
- Queue & Backpressure: 1001-1005
- Authentication: 1011-1016
- Validation: 1021-1029
- Path Security: 1036-1040
- Resources: 1046-1052
- Rate Limiting: 1056-1060
- Protocol: 1066-1070
- Server State: 1076-1080
- Circuit Breaker: 1086-1089
- Experimental: 1090-1099

### 2.6 Batch Processor

**Module**: `erlmcp_batch_processor` (NEW)
**Behavior**: gen_server
**Supervision**: erlmcp_protocol_sup

**Purpose**:
- Async batch request processing
- Parallel execution with result ordering
- Partial failure handling (all-or-nothing for errors)

**State**:
```erlang
-record(batch_state, {
    pending_batches :: #{batch_id() => batch_info()},
    workers :: pid(),  % Worker pool (poolboy)
    max_batch_size :: pos_integer(),
    timeout :: timeout()
}).

-type batch_info() :: #{
    requests => [jsonrpc_request()],
    caller => pid(),
    started_at => erlang:timestamp(),
    results => [term()]
}.
```

**API**:
```erlang
%% Process batch request
-spec process_batch([jsonrpc_request()], timeout()) -> [jsonrpc_response()].
process_batch(Requests, Timeout) ->
    gen_server:call(erlmcp_batch_processor, {batch, Requests}, Timeout).
```

**Processing Strategy**:
1. Validate batch structure (non-empty array)
2. Spawn worker per request (up to pool limit)
3. Collect results in order
4. Filter out notification results (no response)
5. Return response array

**Performance**: 10-100x faster than sequential processing for large batches

### 2.7 SONA Router (Claude-Flow Integration)

**Module**: `erlmcp_sona_router` (NEW)
**Behavior**: gen_server
**Supervision**: erlmcp_protocol_sup

**Purpose**:
- Bridge between erlmcp and claude-flow SONA routing
- Semantic routing based on method/content
- Integration hooks for external routing decisions

**State**:
```erlang
-record(sona_state, {
    routing_rules :: [routing_rule()],
    external_router :: {module(), atom()} | undefined,
    cache :: ets:tab()  % Routing decision cache
}).

-type routing_rule() :: #{
    pattern => binary() | regex(),
    destination => node | {node, node()},
    priority => integer()
}.
```

**API**:
```erlang
%% Route request through SONA
-spec route(binary(), map()) -> {node(), term()} | local.
route(Method, Params) ->
    gen_server:call(erlmcp_sona_router, {route, Method, Params}).

%% Register routing rule
-spec add_routing_rule(routing_rule()) -> ok.
add_routing_rule(Rule) ->
    gen_server:call(erlmcp_sona_router, {add_rule, Rule}).
```

**Integration Points**:
1. Method registry calls SONA router before local dispatch
2. SONA router checks routing rules
3. Local execution OR remote node dispatch
4. Result returned to caller

---

## 3. SUPERVISION TREE ARCHITECTURE

### 3.1 Complete 4-Tier Supervision Hierarchy

```
erlmcp_app
└── erlmcp_sup (one_for_one)
    ├── TIER 1: erlmcp_core_sup (one_for_one)
    │   ├── erlmcp_registry (gproc)
    │   ├── erlmcp_session_manager
    │   ├── erlmcp_connection_limiter
    │   ├── erlmcp_rate_limiter
    │   ├── erlmcp_circuit_breaker
    │   ├── erlmcp_health
    │   ├── erlmcp_reload_sup
    │   ├── erlmcp_resource_subscriptions
    │   ├── erlmcp_sse_event_store
    │   ├── erlmcp_icon_cache
    │   ├── erlmcp_cache
    │   ├── erlmcp_cache_warmer_sup
    │   ├── erlmcp_session_replicator
    │   ├── erlmcp_session_failover
    │   ├── erlmcp_failover_worker_sup
    │   ├── erlmcp_connection_monitor
    │   ├── erlmcp_memory_monitor
    │   ├── erlmcp_cpu_quota
    │   ├── erlmcp_cancellation
    │   ├── erlmcp_pagination
    │   ├── erlmcp_notification_handler_sup
    │   ├── erlmcp_client_sup
    │   └── erlmcp_plugin_sup
    │
    ├── TIER 2: erlmcp_protocol_sup (one_for_one) ← NEW
    │   ├── erlmcp_method_registry ← Dynamic routing
    │   ├── erlmcp_capability_manager ← Feature flags
    │   ├── erlmcp_schema_cache ← Compiled validators
    │   ├── erlmcp_batch_processor ← Async batching
    │   └── erlmcp_sona_router ← Claude-flow bridge
    │
    ├── TIER 3: erlmcp_services_sup (one_for_one) ← REORGANIZED
    │   ├── erlmcp_server_sup (simple_one_for_one)
    │   ├── erlmcp_tasks_manager ← NEW (async tasks)
    │   ├── erlmcp_sampling_coordinator ← ENHANCED (streaming)
    │   ├── erlmcp_elicitation_server ← ENHANCED (all modes)
    │   ├── erlmcp_completion_server ← ENHANCED (all ref types)
    │   ├── erlmcp_roots_server ← Existing
    │   └── erlmcp_apps_server ← Existing
    │
    └── TIER 4: erlmcp_observability_sup (one_for_one) ← UNCHANGED
        ├── erlmcp_event_manager
        ├── erlmcp_metrics
        ├── erlmcp_metrics_server
        ├── erlmcp_metrics_aggregator
        ├── erlmcp_dashboard_server
        ├── erlmcp_health_monitor
        ├── erlmcp_recovery_manager
        ├── erlmcp_chaos
        ├── erlmcp_chaos_worker_sup
        ├── erlmcp_process_monitor
        └── erlmcp_audit_log
```

### 3.2 Supervision Strategy Rationale

**TIER 1 (Core Foundation)**:
- **Strategy**: one_for_one
- **Rationale**: Each infrastructure component fails independently
- **Failure Impact**: New connections may fail; existing continue
- **Recovery**: <500ms per component

**TIER 2 (Protocol Layer)** - NEW:
- **Strategy**: one_for_one
- **Rationale**: Protocol components are stateless (ETS-based)
- **Failure Impact**: In-flight requests may fail; cache rebuilds from ETS
- **Recovery**: <100ms (ETS survives restarts)

**TIER 3 (Services)**:
- **Strategy**: one_for_one for coordinators, simple_one_for_one for servers
- **Rationale**: Service failures don't cascade
- **Failure Impact**: Individual service unavailable; others continue
- **Recovery**: <1s per service

**TIER 4 (Observability)** - UNCHANGED:
- **Strategy**: one_for_one
- **Rationale**: Observability failures NEVER affect protocol
- **Failure Impact**: Metrics/logs unavailable; protocol unaffected
- **Recovery**: <500ms

### 3.3 Child Specifications (NEW Modules)

**erlmcp_protocol_sup**:
```erlang
#{id => erlmcp_protocol_sup,
  start => {erlmcp_protocol_sup, start_link, []},
  restart => permanent,
  shutdown => infinity,  % Supervisor
  type => supervisor,
  modules => [erlmcp_protocol_sup]}
```

**erlmcp_method_registry**:
```erlang
#{id => erlmcp_method_registry,
  start => {erlmcp_method_registry, start_link, []},
  restart => permanent,
  shutdown => 5000,
  type => worker,
  modules => [erlmcp_method_registry]}
```

**erlmcp_schema_cache**:
```erlang
#{id => erlmcp_schema_cache,
  start => {erlmcp_schema_cache, start_link, []},
  restart => permanent,
  shutdown => 5000,
  type => worker,
  modules => [erlmcp_schema_cache]}
```

**erlmcp_capability_manager**:
```erlang
#{id => erlmcp_capability_manager,
  start => {erlmcp_capability_manager, start_link, []},
  restart => permanent,
  shutdown => 5000,
  type => worker,
  modules => [erlmcp_capability_manager]}
```

---

## 4. TRANSPORT LAYER ARCHITECTURE

### 4.1 Unified Transport Abstraction

**Current**: 5 separate transport modules (stdio, tcp, http, ws, sse)
**Enhancement**: Add session correlation layer

**New Module**: `erlmcp_session_correlator` (gen_server)
**Purpose**: Map sessions across transports (HTTP POST + SSE, WebSocket)

**State**:
```erlang
-record(session_correlator_state, {
    sessions :: ets:tab(),  % SessionId → {Transport, Pid, LastActivity}
    transports :: #{session_id() => [transport_id()]},
    cleanup_timer :: reference()
}).
```

**API**:
```erlang
%% Correlate transport with session
-spec correlate_session(session_id(), transport_id(), pid()) -> ok.
correlate_session(SessionId, TransportId, Pid) ->
    gen_server:call(erlmcp_session_correlator,
                    {correlate, SessionId, TransportId, Pid}).

%% Find transport for session
-spec find_transport(session_id()) -> {ok, pid()} | {error, not_found}.
find_transport(SessionId) ->
    gen_server:call(erlmcp_session_correlator, {find, SessionId}).

%% List all transports for session
-spec list_transports(session_id()) -> [{transport_id(), pid()}].
list_transports(SessionId) ->
    gen_server:call(erlmcp_session_correlator, {list, SessionId}).
```

**Session Correlation Use Cases**:
1. **HTTP + SSE**: Client POSTs on `/mcp`, receives responses via SSE `/events`
   - POST handler: Creates session, stores in correlator
   - SSE handler: Looks up session, attaches to event stream
   - Response routing: Registry routes to SSE transport via session ID

2. **WebSocket**: Persistent connection with session affinity
   - WS upgrade: Creates session, single transport
   - Messages: Routed via session ID in WS state

3. **TCP**: Direct connection = session
   - TCP accept: Creates session, stores socket
   - Messages: Routed via socket

### 4.2 Transport-Session Mapping

```
Session Lifecycle:
1. initialize request → Create session → Store in correlator
2. Subsequent requests → Lookup session → Route to correct transport
3. Transport disconnect → Clean up session → Notify handlers
4. Session timeout (30min) → Cleanup
```

**Session Cleanup**:
- Timer-based: 30-minute idle timeout
- Event-based: Transport disconnect triggers cleanup
- Manual: `/shutdown` request

### 4.3 HTTP/SSE Session Affinity

**Problem**: HTTP is stateless; SSE stream needs session context

**Solution**: Session cookies + correlator

**Flow**:
```
1. Client POSTs initialize to /mcp
   → Server responds with Set-Cookie: session_id=XYZ
   → Server stores XYZ in correlator

2. Client opens SSE connection to /events with Cookie: session_id=XYZ
   → Server looks up XYZ in correlator
   → Server attaches SSE transport to session

3. Server sends notification
   → Registry routes to session XYZ
   → Correlator finds SSE transport
   → Message sent via SSE
```

---

## 5. PERFORMANCE OPTIMIZATIONS

### 5.1 Schema Validation Caching (Detailed)

**Current Bottleneck**:
```erlang
%% BEFORE (5-20ms per call)
validate_tool_arguments(Args, InputSchema) ->
    jesse:validate(InputSchema, Args).  % Compiles schema every time!
```

**Optimized Version**:
```erlang
%% AFTER (<0.1ms per call)
validate_tool_arguments(Args, ToolName) ->
    SchemaName = tool_schema_name(ToolName),
    erlmcp_schema_cache:validate(SchemaName, Args).
```

**Compilation Strategy**:
```erlang
%% On tool registration
register_tool(ToolName, InputSchema) ->
    % Compile schema once
    ok = erlmcp_schema_cache:compile_and_cache(
        tool_schema_name(ToolName),
        InputSchema
    ),
    % Store tool with schema reference
    ets:insert(tools_table, {ToolName, {schema, tool_schema_name(ToolName)}}).
```

**Cache Invalidation**:
```erlang
%% On tool update
update_tool(ToolName, NewInputSchema) ->
    % Invalidate old schema
    ok = erlmcp_schema_cache:invalidate(tool_schema_name(ToolName)),
    % Compile new schema
    ok = erlmcp_schema_cache:compile_and_cache(
        tool_schema_name(ToolName),
        NewInputSchema
    ).
```

**Performance Impact**:
- 100-200x speedup (5-20ms → <0.1ms)
- Throughput: 50K req/s → 500K req/s
- p99 latency: <1ms achieved

### 5.2 Async Batch Processing Pipeline

**Current**: Sequential batch processing
**Enhanced**: Parallel execution with result ordering

**Processing Pipeline**:
```
Batch Request (10 items)
    ↓
Split into chunks (2 per worker, 5 workers)
    ↓
Worker Pool (poolboy)
    ├─ Worker 1 → [Req 1, Req 2]
    ├─ Worker 2 → [Req 3, Req 4]
    ├─ Worker 3 → [Req 5, Req 6]
    ├─ Worker 4 → [Req 7, Req 8]
    └─ Worker 5 → [Req 9, Req 10]
    ↓
Collect results in order
    ↓
Return response array [Res 1, Res 2, ..., Res 10]
```

**Speedup**: 5-10x for large batches

### 5.3 Resource Subscription Fan-Out Optimization

**Current Gap**: Compliance matrix shows 85% (needs fan-out optimization)

**Problem**: Sending notifications to 1000s of subscribers is slow
**Solution**: Async fan-out with batching

**Implementation**:
```erlang
%% BEFORE (blocking)
notify_subscribers(Uri, SubscriberList) ->
    lists:foreach(fun(Pid) ->
        Pid ! {resource_updated, Uri}
    end, SubscriberList).

%% AFTER (async with batching)
notify_subscribers(Uri, SubscriberList) ->
    erlmcp_resource_subscriptions:async_notify(Uri, SubscriberList).

%% In erlmcp_resource_subscriptions
async_notify(Uri, SubscriberList) ->
    % Spawn worker per 100 subscribers
    ChunkSize = 100,
    Chunks = chunk_list(SubscriberList, ChunkSize),
    lists:foreach(fun(Chunk) ->
        spawn(fun() ->
            lists:foreach(fun(Pid) ->
                Pid ! {resource_updated, Uri}
            end, Chunk)
        end)
    end, Chunks).
```

**Performance**: 1000 subscribers notified in <10ms (vs 100ms before)

---

## 6. MODULE DEPENDENCY GRAPH

### 6.1 Core Dependencies

```
erlmcp_registry (gproc)
    ↓
    ├── erlmcp_server (registered processes)
    ├── erlmcp_client (registered processes)
    └── erlmcp_method_registry (lookup handlers)

erlmcp_method_registry
    ↓
    ├── erlmcp_capability_manager (capability checks)
    ├── erlmcp_schema_cache (input validation)
    └── erlmcp_error_mapper (error responses)

erlmcp_schema_cache
    ↓
    └── jesse (JSON Schema validation library)

erlmcp_capability_manager
    ↓
    ├── erlmcp_method_registry (method capabilities)
    └── erlmcp_server (capability announcement)
```

### 6.2 Protocol Layer Dependencies

```
erlmcp_method_registry
    ↑
    ├── erlmcp_tool (tools/call handler)
    ├── erlmcp_resources (resources/* handlers)
    ├── erlmcp_prompt_template (prompts/* handlers)
    ├── erlmcp_sampling_coordinator (sampling/* handler)
    ├── erlmcp_tasks_manager (tasks/* handlers)
    ├── erlmcp_elicitation_server (elicitation/* handlers)
    └── erlmcp_completion_server (completion/* handler)
```

### 6.3 Transport Dependencies

```
erlmcp_transport_*
    ↓
    ├── erlmcp_session_correlator (session affinity)
    ├── erlmcp_registry (process routing)
    └── erlmcp_server (message delivery)
```

### 6.4 Circular Dependency Prevention

**Potential Cycle**:
```
erlmcp_method_registry → erlmcp_capability_manager → erlmcp_method_registry
```

**Resolution**:
- Capability manager stores state, method registry queries it
- No callbacks from capability manager to method registry
- Uni-directional dependency: method_registry → capability_manager

---

## 7. CAPABILITY IMPLEMENTATION STATUS

### 7.1 NEW Modules (100% MCP Support)

| Module | Capability | Status | Implementation |
|--------|-----------|--------|----------------|
| `erlmcp_tasks_manager` | Tasks API | NEW | Async task execution, persistence, status tracking |
| `erlmcp_sampling_coordinator` | Sampling (Enhanced) | ENHANCED | Streaming support, all model parameters |
| `erlmcp_elicitation_server` | Elicitation (Enhanced) | ENHANCED | Inline, URL, terminal modes |
| `erlmcp_completion_server` | Completion (Enhanced) | ENHANCED | Tool, resource, prompt ref completion |
| `erlmcp_method_registry` | Protocol Routing | NEW | Dynamic method registration |
| `erlmcp_capability_manager` | Capability Negotiation | NEW | Feature flags, dependencies |
| `erlmcp_schema_cache` | Validation Performance | NEW | Compiled schema cache |
| `erlmcp_batch_processor` | Batch Optimization | NEW | Async parallel processing |
| `erlmcp_sona_router` | SONA Integration | NEW | Claude-flow routing bridge |
| `erlmcp_session_correlator` | Transport Affinity | NEW | Cross-transport sessions |

### 7.2 Capability Compliance Roadmap

| Capability | Current | Target | Gap | Modules |
|-----------|---------|--------|-----|---------|
| **Core Protocol** | 93% | 100% | -7% | erlmcp_error_mapper (SEP-1303) |
| **Resources** | 82% | 100% | -18% | erlmcp_resource_subscriptions (fan-out) |
| **Tools** | 76% | 100% | -24% | erlmcp_schema_cache (performance) |
| **Prompts** | 73% | 100% | -27% | erlmcp_prompt_template (verification) |
| **Sampling** | 18% | 100% | -82% | erlmcp_sampling_coordinator (streaming) |
| **Logging** | 100% | 100% | 0% | No changes |
| **Completion** | 42% | 100% | -58% | erlmcp_completion_server (all refs) |
| **Roots** | 40% | 100% | -60% | erlmcp_roots_server (validation) |
| **Cancellation** | 100% | 100% | 0% | No changes |
| **Progress** | 100% | 100% | 0% | No changes |
| **Tasks** | 0% | 100% | -100% | erlmcp_tasks_manager (NEW) |
| **Elicitation** | 1% | 100% | -99% | erlmcp_elicitation_server (all modes) |
| **Security** | 26% | 100% | -74% | erlmcp_auth, erlmcp_oauth (OIDC) |
| **Transports** | 65% | 100% | -35% | SSE polling streams |
| **Schema** | 67% | 100% | -33% | erlmcp_schema_cache (caching) |
| **Metadata** | 23% | 100% | -77% | erlmcp_icon_cache (icons) |

**Overall**: 65% → 95%+ compliance

---

## 8. ERROR HANDLING ARCHITECTURE

### 8.1 Error Code Mapping (89 Refusal Codes)

**Module**: `erlmcp_error_mapper`

**Complete Mapping**:
```erlang
-spec map_error(term()) -> {Code, Message, Data}.

% Queue & Backpressure (1001-1005)
map_error({queue_full, _}) -> {1001, <<"Queue capacity exceeded">>, #{}};
map_error({queue_byte_limit, _}) -> {1002, <<"Queue byte capacity exceeded">>, #{}};
map_error({backpressure, _}) -> {1005, <<"Backpressure active">>, #{}};

% Authentication (1011-1016)
map_error({auth_failed, R}) -> {1011, <<"Authentication failed">>, #{reason => R}};
map_error({auth_expired, _}) -> {1012, <<"Auth expired">>, #{}};
map_error({forbidden, _}) -> {1014, <<"Authorization forbidden">>, #{}};

% Validation (1021-1029)
map_error({invalid_params, F}) -> {1021, <<"Invalid params">>, #{field => F}};
map_error({invalid_schema, _}) -> {1022, <<"Invalid JSON schema">>, #{}};
map_error({missing_field, F}) -> {1028, <<"Missing required field">>, #{field => F}};

% Path Security (1036-1040)
map_error({path_traversal, P}) -> {1036, <<"Path traversal detected">>, #{path => P}};
map_error({symlink_traversal, _}) -> {1038, <<"Symlink traversal detected">>, #{}};

% Resources (1046-1052)
map_error({resource_not_found, U}) -> {1046, <<"Resource not found">>, #{uri => U}};
map_error({tool_not_found, N}) -> {1048, <<"Tool not found">>, #{name => N}};
map_error({prompt_not_found, N}) -> {1050, <<"Prompt not found">>, #{name => N}};

% Rate Limiting (1056-1060)
map_error({rate_limited, D}) -> {1056, <<"Rate limit exceeded">>, D};
map_error({quota_exceeded, _}) -> {1059, <<"Quota exceeded">>, #{}};

% Protocol (1066-1070)
map_error({protocol_error, _}) -> {1066, <<"Protocol error">>, #{}};
map_error({message_too_large, S}) -> {1068, <<"Message too large">>, #{max_size => S}};
map_error(timeout) -> {1069, <<"Timeout">>, #{}};

% Server State (1076-1080)
map_error({not_initialized, _}) -> {1076, <<"Server uninitialized">>, #{}};
map_error({shutting_down, _}) -> {1077, <<"Server shutting down">>, #{}};
map_error({internal_error, R}) -> {1079, <<"Internal error">>, #{reason => R}};

% Circuit Breaker (1086-1089)
map_error({circuit_open, _}) -> {1086, <<"Circuit breaker open">>, #{}};
map_error({resource_exhausted, _}) -> {1089, <<"Resource exhausted">>, #{}};

% Experimental (1090-1099)
map_error({task_not_found, I}) -> {1095, <<"Task not found">>, #{id => I}};
map_error({task_timeout, _}) -> {1098, <<"Task timeout">>, #{}}.
```

### 8.2 Error Response Generation

**JSON-RPC Error Format**:
```erlang
-spec format_error(integer(), binary(), map()) -> jsonrpc_error().
format_error(Code, Message, Data) ->
    #{<<"code">> => Code,
      <<"message">> => Message,
      <<"data">> => Data}.
```

**Response Construction**:
```erlang
-spec error_response(request_id(), term()) -> jsonrpc_response().
error_response(RequestId, Error) ->
    {Code, Message, Data} = erlmcp_error_mapper:map_error(Error),
    #{<<"jsonrpc">> => <<"2.0">>,
      <<"id">> => RequestId,
      <<"error">> => format_error(Code, Message, Data)}.
```

---

## 9. SONA ROUTING INTEGRATION

### 9.1 Integration Points

**SONA Router Module**: `erlmcp_sona_router`

**Hooks in Method Registry**:
```erlang
%% In erlmcp_method_registry:route_method/3
route_method(Method, Params, Context) ->
    % 1. Check if SONA routing enabled
    case application:get_env(erlmcp_core, sona_routing_enabled, false) of
        true ->
            % 2. Query SONA router for destination
            case erlmcp_sona_router:route(Method, Params) of
                local ->
                    % 3. Execute locally
                    local_execute(Method, Params, Context);
                {node, Node} ->
                    % 4. Execute on remote node
                    remote_execute(Node, Method, Params, Context)
            end;
        false ->
            % SONA disabled, execute locally
            local_execute(Method, Params, Context)
    end.
```

### 9.2 Routing Rules

**Example Rules**:
```erlang
% Route sampling requests to GPU nodes
#{pattern => <<"sampling/.*">>,
  destination => {node, 'gpu_node@host'},
  priority => 10}

% Route tool calls matching pattern to specialized node
#{pattern => {regex, <<"tools/call">>, #{name => <<".*_llm">>}},
  destination => {node, 'llm_node@host'},
  priority => 5}

% Default: local execution
#{pattern => <<".*">>,
  destination => local,
  priority => 0}
```

### 9.3 Performance Implications

**Routing Decision**: <0.1ms (ETS cache lookup)
**Remote Execution Overhead**: 1-5ms (depends on network)
**Total Impact**: Minimal (<1% latency increase)

---

## 10. DESIGN RATIONALE & TRADE-OFFS

### 10.1 Why 4-Tier Supervision?

**Rationale**:
- TIER 1: Infrastructure (core, stable, rarely fails)
- TIER 2: Protocol (dynamic, stateless, fast recovery)
- TIER 3: Services (user-facing, isolated failures)
- TIER 4: Observability (completely isolated, zero protocol impact)

**Trade-off**: More complexity vs better failure isolation
**Decision**: Better isolation wins (Joe Armstrong principle)

### 10.2 Why Dynamic Method Registry?

**Alternative**: Hardcoded pattern matching in handle_call
```erlang
% BAD: Hardcoded
handle_call({request, <<"tools/call">>, Params}, ...) -> ...;
handle_call({request, <<"resources/list">>, Params}, ...) -> ...
```

**Chosen**: Dynamic registry with ETS
```erlang
% GOOD: Dynamic
route_method(Method, Params, Context) ->
    case ets:lookup(method_registry, Method) of
        [{_, Handler}] -> apply_handler(Handler, Params, Context);
        [] -> {error, {-32601, <<"Method not found">>}}
    end.
```

**Rationale**:
- Extensibility: New methods without code changes
- Versioning: Multiple method versions supported
- Performance: O(1) ETS lookup vs pattern matching
- Testing: Easier to mock/inject handlers

**Trade-off**: Extra indirection vs flexibility
**Decision**: Flexibility wins (supports experimental features)

### 10.3 Why Schema Caching?

**Problem**: jesse:validate/3 compiles schema every time (5-20ms)
**Alternative**: Pre-compile schemas at startup
**Chosen**: Lazy compilation + ETS cache

**Rationale**:
- Not all schemas used (100s registered, 10s used)
- Lazy compilation = faster startup
- ETS cache = survives process restarts
- Invalidation = easy updates

**Trade-off**: Memory usage vs performance
**Decision**: Performance wins (memory is cheap, time is expensive)

**Performance Gain**: 100-200x speedup

### 10.4 Why Async Batch Processing?

**Alternative**: Sequential processing
```erlang
% BAD: Sequential
Results = lists:map(fun(Req) ->
    process_request(Req)
end, Requests).
```

**Chosen**: Parallel with poolboy
```erlang
% GOOD: Parallel
Results = erlmcp_batch_processor:process_batch(Requests, Timeout).
```

**Rationale**:
- Batch requests are independent (JSON-RPC spec)
- Parallel execution = 5-10x speedup for large batches
- Poolboy = controlled concurrency (no resource exhaustion)

**Trade-off**: Complexity vs performance
**Decision**: Performance wins (batches are common in MCP)

### 10.5 Why SONA Router?

**Alternative**: Static node assignments
**Chosen**: Dynamic routing with rules

**Rationale**:
- Claude-flow uses SONA for semantic routing
- Routing rules = flexible deployment strategies
- GPU-intensive operations → GPU nodes
- Local operations → local execution

**Trade-off**: Routing overhead (<1ms) vs flexibility
**Decision**: Flexibility wins (enables multi-node deployments)

---

## 11. IMPLEMENTATION ROADMAP

### 11.1 Phase 1 (Weeks 1-6): Protocol Layer Foundation

**Deliverables**:
1. `erlmcp_protocol_sup` supervisor
2. `erlmcp_method_registry` implementation
3. `erlmcp_schema_cache` implementation
4. `erlmcp_capability_manager` implementation
5. `erlmcp_error_mapper` implementation
6. All existing methods migrated to registry

**Acceptance Criteria**:
- All tests pass
- p99 latency <1ms achieved
- Schema validation: <0.1ms
- 100% backward compatibility

### 11.2 Phase 2 (Weeks 7-14): Missing Capabilities

**Deliverables**:
1. `erlmcp_tasks_manager` (Tasks API 0% → 100%)
2. `erlmcp_sampling_coordinator` (Sampling 18% → 100%)
3. `erlmcp_elicitation_server` (Elicitation 1% → 100%)
4. `erlmcp_completion_server` (Completion 42% → 100%)
5. `erlmcp_roots_server` enhancements (Roots 40% → 100%)

**Acceptance Criteria**:
- All 16 capabilities ≥80% compliant
- Integration tests passing
- Documentation complete

### 11.3 Phase 3 (Weeks 15-20): Performance & Optimization

**Deliverables**:
1. `erlmcp_batch_processor` (async batching)
2. Resource subscription fan-out optimization
3. SSE polling streams implementation
4. HTTP/2 multiplexing optimization

**Acceptance Criteria**:
- Throughput: 500K req/s (tools/call)
- p99 latency: <1ms maintained
- Fan-out: 1000 subscribers in <10ms

### 11.4 Phase 4 (Weeks 21-24): SONA & Integration

**Deliverables**:
1. `erlmcp_sona_router` implementation
2. `erlmcp_session_correlator` implementation
3. Claude-flow integration tests
4. Multi-node deployment testing

**Acceptance Criteria**:
- SONA routing: <0.1ms overhead
- Cross-transport sessions working
- Distributed tests passing

### 11.5 Phase 5 (Weeks 25-30): Security & Compliance

**Deliverables**:
1. OAuth 2.0 enhancements (OIDC, incremental consent)
2. Security validators (path traversal, input validation)
3. Audit logging (tamper-proof hash chain)
4. Compliance documentation

**Acceptance Criteria**:
- Security: 26% → 100%
- All 89 refusal codes tested
- Penetration testing passed

---

## 12. TESTING STRATEGY

### 12.1 Unit Tests (Per Module)

**New Modules Tests**:
- `erlmcp_method_registry_tests.erl`
- `erlmcp_schema_cache_tests.erl`
- `erlmcp_capability_manager_tests.erl`
- `erlmcp_batch_processor_tests.erl`
- `erlmcp_tasks_manager_tests.erl`
- `erlmcp_sampling_coordinator_tests.erl`
- `erlmcp_elicitation_server_tests.erl`

**Coverage Target**: ≥80% per module

### 12.2 Integration Tests (Cross-Module)

**Test Suites**:
- `erlmcp_protocol_integration_SUITE.erl`
- `erlmcp_capability_negotiation_SUITE.erl`
- `erlmcp_transport_affinity_SUITE.erl`
- `erlmcp_sona_routing_SUITE.erl`

**Scenarios**:
- Full initialize → capabilities → method call flow
- Batch requests with mixed methods
- Session affinity across HTTP/SSE
- SONA routing to remote node

### 12.3 Compliance Tests (MCP Spec)

**Test Suite**: `erlmcp_mcp_compliance_SUITE.erl`

**Coverage**:
- All 16 capabilities tested
- All 30+ RPC methods tested
- All 89 refusal codes triggered
- All error conditions tested

**Validation**:
- MCP official test suite (if available)
- JSON-RPC 2.0 compliance validator

### 12.4 Performance Tests

**Benchmarks**:
- Schema validation: <0.1ms (before cache: 5-20ms)
- Method routing: <0.1ms
- Batch processing: 5-10x speedup vs sequential
- Fan-out: 1000 subscribers in <10ms

**Load Tests**:
- Sustained load: 500K req/s for 60 seconds
- Concurrent connections: 50K
- Memory usage: <5MB per connection

### 12.5 Chaos Engineering Tests

**Scenarios**:
- Kill random processes (supervisor recovery)
- Network partitions (cluster failover)
- Resource exhaustion (circuit breaker triggers)
- Schema cache invalidation (rebuild from ETS)

**Validation**: All systems recover automatically

---

## 13. MONITORING & OBSERVABILITY

### 13.1 Metrics (Additions)

**Method Registry Metrics**:
- `method_registry.registrations.total` (counter)
- `method_registry.route.duration_ms` (histogram)
- `method_registry.route.errors` (counter by error code)

**Schema Cache Metrics**:
- `schema_cache.hits` (counter)
- `schema_cache.misses` (counter)
- `schema_cache.compilations` (counter)
- `schema_cache.invalidations` (counter)
- `schema_cache.compile_duration_ms` (histogram)

**Capability Manager Metrics**:
- `capability_manager.negotiations.total` (counter)
- `capability_manager.enabled_capabilities` (gauge)
- `capability_manager.conflicts` (counter)

**Batch Processor Metrics**:
- `batch_processor.batches.total` (counter)
- `batch_processor.batch_size` (histogram)
- `batch_processor.duration_ms` (histogram)
- `batch_processor.parallelism` (gauge)

### 13.2 Traces (OpenTelemetry)

**Span Hierarchy**:
```
request (root)
├── method_registry.route
│   ├── capability_manager.check
│   ├── schema_cache.validate
│   └── handler.execute
│       ├── tool.call (example)
│       └── resource.read (example)
└── response.encode
```

**Trace Attributes**:
- `method.name`: Method being called
- `method.version`: Method version
- `capability.required`: Required capabilities
- `schema.cached`: Whether schema was cached
- `batch.size`: Batch size (if applicable)

### 13.3 Health Checks

**New Health Checks**:
- `erlmcp_method_registry:health/0` → `ok | degraded | error`
- `erlmcp_schema_cache:health/0` → `{ok, HitRate}` (hit rate %)
- `erlmcp_capability_manager:health/0` → `ok`
- `erlmcp_batch_processor:health/0` → `{ok, QueueSize}`

**Aggregated Health**:
```erlang
erlmcp_health:check() ->
    #{protocol_layer => #{
        method_registry => ok,
        schema_cache => {ok, 98.5},  % 98.5% hit rate
        capability_manager => ok,
        batch_processor => {ok, 42}  % 42 batches queued
    }}.
```

---

## 14. MIGRATION GUIDE (v2.x → v3.0)

### 14.1 Breaking Changes

**NONE**: 100% backward compatible at API level

### 14.2 Opt-In Features

**Method Registry** (automatic):
- All existing methods auto-registered on startup
- No code changes required
- Performance improvement automatic

**Schema Caching** (automatic):
- Schemas compiled on first use
- No code changes required
- 100-200x speedup automatic

**Capability Manager** (opt-in):
```erlang
% Enable new capabilities
application:set_env(erlmcp_core, capabilities, #{
    tasks => #{enabled => true, experimental => true},
    sampling => #{enabled => true, streaming => true}
}).
```

### 14.3 Configuration Changes

**New Config Options**:
```erlang
{erlmcp_core, [
    {method_registry_enabled, true},
    {schema_cache_enabled, true},
    {schema_cache_ttl, infinity},
    {capability_manager_enabled, true},
    {batch_processing_enabled, true},
    {sona_routing_enabled, false}  % Opt-in
]}
```

### 14.4 Migration Steps

1. **Upgrade to v3.0.0**
2. **No code changes required** (automatic migration)
3. **Enable optional features** (capability_manager, sona_routing)
4. **Test**: `make check`
5. **Deploy**: Rolling deployment (no downtime)

---

## 15. APPENDIX: MODULE REFERENCE

### 15.1 NEW Modules Summary

| Module | Type | Purpose | LOC | Tests |
|--------|------|---------|-----|-------|
| `erlmcp_protocol_sup` | supervisor | Protocol layer supervisor | 50 | CT |
| `erlmcp_method_registry` | gen_server | Dynamic method routing | 300 | EUnit+CT |
| `erlmcp_schema_cache` | gen_server | Compiled schema cache | 200 | EUnit+CT |
| `erlmcp_capability_manager` | gen_server | Capability negotiation | 250 | EUnit+CT |
| `erlmcp_error_mapper` | library | 89 refusal code mapping | 150 | EUnit |
| `erlmcp_batch_processor` | gen_server | Async batch processing | 200 | EUnit+CT |
| `erlmcp_sona_router` | gen_server | SONA routing bridge | 150 | CT |
| `erlmcp_session_correlator` | gen_server | Cross-transport sessions | 150 | CT |
| `erlmcp_tasks_manager` | gen_server | Tasks API | 400 | EUnit+CT |
| `erlmcp_sampling_coordinator` | gen_server | Enhanced sampling | 300 | EUnit+CT |
| `erlmcp_elicitation_server` | gen_server | Enhanced elicitation | 250 | EUnit+CT |
| `erlmcp_completion_server` | gen_server | Enhanced completion | 200 | EUnit+CT |

**Total NEW Code**: ~2,650 LOC + ~2,000 LOC tests = ~4,650 LOC

### 15.2 ENHANCED Modules

| Module | Current | Enhancement | LOC Change |
|--------|---------|-------------|------------|
| `erlmcp_server` | Existing | Method registry integration | +50 |
| `erlmcp_client` | Existing | Capability manager integration | +50 |
| `erlmcp_resources` | Existing | Fan-out optimization | +100 |
| `erlmcp_tool` | Existing | Schema cache integration | +50 |
| `erlmcp_roots_server` | Existing | Validation enhancements | +100 |
| `erlmcp_auth` | Existing | OAuth 2.0 enhancements | +200 |

**Total ENHANCED Code**: ~550 LOC

**Grand Total**: ~5,200 LOC (modules + tests)

---

## 16. SUCCESS CRITERIA

### 16.1 Compliance

- [ ] 95%+ overall compliance (62/65 features at ≥80%)
- [ ] All 16 capabilities ≥80% compliant
- [ ] All 89 refusal codes tested and working
- [ ] All 30+ RPC methods implemented

### 16.2 Performance

- [ ] p99 latency <1ms (simple operations)
- [ ] Throughput: 500K req/s (tools/call)
- [ ] Schema validation: <0.1ms (cached)
- [ ] Batch processing: 5-10x speedup

### 16.3 Quality

- [ ] Test coverage ≥80% per module
- [ ] All tests passing (EUnit + CT)
- [ ] Dialyzer warnings: 0
- [ ] Xref undefined: 0
- [ ] Documentation: 100% coverage

### 16.4 Operations

- [ ] Zero downtime deployment
- [ ] Backward compatible (v2.x → v3.0)
- [ ] Health checks: 100% coverage
- [ ] Metrics: All new modules instrumented
- [ ] Observability: Zero protocol impact

---

## 17. CONCLUSION

This architecture design provides a complete roadmap to 100% MCP specification compliance while maintaining OTP principles and performance targets. The 4-tier supervision architecture with dynamic protocol routing enables:

1. **Extensibility**: New methods/capabilities without code changes
2. **Performance**: 100-200x speedup via schema caching
3. **Reliability**: Bulkhead isolation prevents cascading failures
4. **Scalability**: SONA routing enables multi-node deployments
5. **Compliance**: All 65 features, 89 refusal codes, 16 capabilities

**Next Steps**:
1. Review this architecture design
2. Approve/modify proposed changes
3. Begin Phase 1 implementation (Protocol Layer Foundation)
4. Iterate based on testing and feedback

**Target Timeline**: 30 weeks (7 months) to 95%+ compliance

---

**Document End**

**References**:
- MCP Specification 2025-11-25: https://modelcontextprotocol.io
- erlmcp Architecture v2.1.0: `/home/user/erlmcp/docs/architecture.md`
- MCP Compliance Matrix: `/home/user/erlmcp/docs/MCP_SPECIFICATION_COMPLIANCE_MATRIX.md`
- OTP Design Principles: https://www.erlang.org/doc/design_principles/des_princ.html
