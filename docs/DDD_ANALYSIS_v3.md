# Domain-Driven Design Analysis: erlmcp v3

**Version**: 3.0.0
**Date**: 2026-02-02
**Status**: Enterprise Architecture Analysis
**Target**: /Users/sac/erlmcp

---

## Executive Summary

erlmcp v3 is an Erlang/OTP implementation of the Model Context Protocol (MCP) v2025-11-25. This DDD analysis identifies **5 bounded contexts**, **8 aggregate roots**, **47 domain events**, and provides refactoring recommendations to strengthen the domain model for enterprise-scale deployment.

### Key Findings

| Aspect | Current State | Recommendation |
|--------|---------------|----------------|
| Bounded Contexts | 5 identified, some coupling | Clear boundaries needed |
| Aggregates | 8 roots, some unclear boundaries | Refine for consistency |
| Domain Events | Ad-hoc notification | Centralize event catalog |
| CQRS | Partial separation | Complete read/write separation |
| Event Sourcing | Raft log for config | Extend to domain events |

---

## 1. Bounded Contexts

### 1.1 Context Map

```
                    +-------------------------------------+
                    |      MCP Protocol Core Context      |
                    |  (Server, Client, JSON-RPC, Auth)   |
                    |         Type: Core Domain           |
                    +------------------+------------------+
                                       |
          +----------------------------+----------------------------+
          |                            |                            |
          v                            v                            v
+---------------------+   +------------------------+   +-------------------------+
| Transport Layer     |   | Distributed            |   | Observability           |
| Context             |   | Coordination Context   |   | Context                 |
| Type: Generic       |   | Type: Supporting       |   | Type: Supporting        |
+---------------------+   +------------------------+   +-------------------------+
          |                            |                            |
          +------------------------+---+----------------------------+
                                   |
                                   v
                     +-------------------------------+
                     | Validation Context            |
                     | Type: Supporting              |
                     +-------------------------------+
```

### 1.2 Context Definitions

#### 1.2.1 MCP Protocol Core Context (Core Domain)

**Responsibility**: MCP protocol implementation, server/client lifecycle, JSON-RPC messaging

**Key Entities**:
- `erlmcp_server_fsm` - Server state machine
- `erlmcp_client_fsm` - Client state machine
- `erlmcp_json_rpc` - JSON-RPC protocol handler
- `erlmcp_auth_*` - Authentication modules
- `erlmcp_session_*` - Session management

**Ubiquitous Language**:
- **Server**: MCP server endpoint hosting tools and resources
- **Client**: MCP client consuming server capabilities
- **Session**: Logical connection between client and server
- **Capability**: Feature declaration (tools, resources, prompts)
- **Request**: JSON-RPC method invocation

**Modules** (97 total):
```
apps/erlmcp_core/src/
├── erlmcp_server_fsm.erl          - Server lifecycle state machine
├── erlmcp_client_fsm.erl          - Client lifecycle state machine
├── erlmcp_json_rpc.erl            - JSON-RPC protocol handler
├── erlmcp_auth_mtls.erl           - mTLS authentication
├── erlmcp_auth_rate_limiter.erl   - Rate limiting
├── erlmcp_session.erl             - Session data structure
├── erlmcp_session_*.erl           - Session backends (ETS, DETS, Mnesia)
├── erlmcp_secrets.erl             - Secrets management
└── ...
```

#### 1.2.2 Transport Layer Context (Generic Domain)

**Responsibility**: Physical communication channels (stdio, TCP, HTTP, WebSocket, SSE)

**Key Entities**:
- `erlmcp_transport_behavior` - Transport behavior contract
- `erlmcp_transport_stdio` - Standard I/O transport
- `erlmcp_transport_tcp` - TCP transport
- `erlmcp_transport_http` - HTTP/SSE transport
- `erlmcp_transport_ws` - WebSocket transport
- `erlmcp_transport_pool` - Connection pooling

**Ubiquitous Language**:
- **Transport**: Physical communication channel
- **Connection**: Active transport instance
- **Pool**: Reusable connection collection
- **Message**: Binary data frame

**Modules** (23 total):
```
apps/erlmcp_transports/src/
├── erlmcp_transport_behavior.erl  - Transport behavior spec
├── erlmcp_transport_stdio.erl     - stdio implementation
├── erlmcp_transport_tcp.erl       - TCP implementation
├── erlmcp_transport_http.erl      - HTTP implementation
├── erlmcp_transport_ws.erl        - WebSocket implementation
├── erlmcp_transport_sse.erl       - Server-Sent Events
├── erlmcp_transport_pool.erl      - Connection pooling
└── ...
```

**Context Relationships**:
- **Customer/Supplier**: Protocol Core defines requirements, Transport implements
- **Conformist**: Transport conforms to MCP wire protocol specification

#### 1.2.3 Distributed Coordination Context (Supporting Domain)

**Responsibility**: Multi-node coordination, consensus, distributed state

**Key Entities**:
- `erlmcp_raft` - Raft consensus implementation
- `erlmcp_cluster` - Node clustering
- `erlmcp_registry_*` - Distributed registry
- `erlmcp_failover_*` - Failover mechanisms
- `erlmcp_split_brain_detector` - Network partition handling

**Ubiquitous Language**:
- **Cluster**: Set of cooperating nodes
- **Leader**: Coordinator for distributed operations
- **Follower**: Replicating node
- **Term**: Logical election period
- **Log**: Ordered operation sequence

**Modules**:
```
apps/erlmcp_core/src/
├── erlmcp_raft.erl                - Raft consensus algorithm
├── erlmcp_cluster.erl             - Multi-node clustering
├── erlmcp_registry_distributed.erl - Distributed registry
├── erlmcp_failover_worker.erl     - Failover handling
├── erlmcp_session_replicator.erl  - Session replication
└── erlmcp_split_brain_detector.erl
```

**Context Relationships**:
- **Partnership**: Works with Protocol Core for distributed session management
- **Open Host Service**: Exposes standard Raft interface

#### 1.2.4 Observability Context (Supporting Domain)

**Responsibility**: Monitoring, tracing, metrics, chaos engineering

**Key Entities**:
- `erlmcp_otel` - OpenTelemetry integration
- `erlmcp_chaos` - Chaos engineering framework
- `erlmcp_health_monitor` - Health monitoring
- `erlmcp_dashboard_server` - Metrics dashboard
- `erlmcp_audit_log` - Audit trail

**Ubiquitous Language**:
- **Span**: Distributed trace unit
- **Metric**: Numeric measurement
- **Experiment**: Chaos test scenario
- **Steady State**: System baseline behavior
- **SLO**: Service Level Objective

**Modules** (31 total):
```
apps/erlmcp_observability/src/
├── erlmcp_otel.erl                - OpenTelemetry integration
├── erlmcp_chaos.erl               - Chaos engineering
├── erlmcp_health_monitor.erl      - Health checks
├── erlmcp_dashboard_server.erl    - Metrics dashboard
├── erlmcp_audit_log.erl           - Audit logging
├── erlmcp_tracer.erl              - Distributed tracing
└── ...
```

**Context Relationships**:
- **Published Language**: Emits events for all contexts to consume
- **Anti-Corruption Layer**: Normalizes OTEL protocol for internal consumption

#### 1.2.5 Validation Context (Supporting Domain)

**Responsibility**: Protocol validation, compliance checking, spec verification

**Key Entities**:
- `erlmcp_protocol_validator` - Protocol compliance
- `erlmcp_transport_validator` - Transport validation
- `erlmcp_security_validator` - Security checks
- `erlmcp_compliance_report` - Compliance reporting
- `erlmcp_spec_parser` - Specification parsing

**Ubiquitous Language**:
- **Validation**: Rule verification
- **Compliance**: Standard adherence
- **Spec**: Protocol definition
- **Report**: Validation result

**Modules** (13 total):
```
apps/erlmcp_validation/src/
├── erlmcp_protocol_validator.erl  - Protocol validation
├── erlmcp_transport_validator.erl - Transport validation
├── erlmcp_security_validator.erl  - Security checks
├── erlmcp_compliance_report.erl   - Compliance reporting
└── erlmcp_spec_parser.erl         - Spec parser
```

**Context Relationships**:
- **Anti-Corruption Layer**: Validates external protocol compliance
- **Supplier**: Provides validation results to all contexts

---

## 2. Aggregates

### 2.1 Aggregate Definitions

| Aggregate Root | Context | Invariants | Consistency Boundary |
|----------------|---------|------------|---------------------|
| **Server** | Protocol Core | Phase transitions valid, tools/resources consistent | Single server process |
| **Session** | Protocol Core | ID unique, timeout enforced, metadata valid | Session backend (ETS/DETS/Mnesia) |
| **Registry** | Protocol Core | Names unique to PIDs, registrations consistent | gproc/pg scope |
| **Tool** | Protocol Core | Name unique, schema valid, handler exists | Within server state |
| **Resource** | Protocol Core | URI unique, subscription valid | Resource subscription manager |
| **PromptTemplate** | Protocol Core | Template valid, variables secure | Prompt list manager |
| **RaftLog** | Distributed Coordination | Index sequential, term consistent | Single Raft cluster |
| **ChaosExperiment** | Observability | Safety constraints met, blast radius limited | Single experiment run |

### 2.2 Aggregate Details

#### 2.2.1 Server Aggregate

**Root**: `erlmcp_server_fsm` (gen_statem)

**Entities**:
- Server (root)
- Capabilities (value object)
- Tool collection (entity map)
- Resource collection (entity map)
- Prompt collection (entity map)

**Value Objects**:
```
-record(mcp_server_capabilities, {
    tools :: boolean(),
    resources :: boolean(),
    prompts :: map(),
    logging :: map()
}).
```

**Invariants**:
1. State transition: initialization -> accepting -> drain -> shutdown
2. Tools added only in accepting state
3. Capabilities declared before initialization complete
4. No new connections in drain/shutdown state

**Consistency Boundary**: Single gen_statem process

**State Transitions**:
```
        [initialize()]
        init
    [accept_connections()]
initialization -----------> accepting
        |                        |
        |                        | [drain()]
        v                        v
     shutdown <-------------- drain
        ^                        |
        |                        | [timeout]
        +------------------------+
```

#### 2.2.2 Session Aggregate

**Root**: `erlmcp_session` module with backend storage

**Entities**:
- Session (root)
- Metadata (value object)
- SessionBackend (entity)

**Value Objects**:
```erlang
-type session() ::
    #{id := session_id(),
      created_at := integer(),
      last_accessed := integer(),
      timeout_ms := pos_integer() | infinity,
      metadata := map()}.
```

**Invariants**:
1. Session ID globally unique
2. Default TTL = 1 hour (3600000ms) - security fix
3. last_accessed updated on each access
4. Expired sessions auto-cleanup

**Consistency Boundary**: Backend storage (ETS/DETS/Mnesia)

**Repository Pattern**:
```erlang
%% Session Manager acts as Repository
erlmcp_session_manager:create_session/2
erlmcp_session_manager:get_session/1
erlmcp_session_manager:update_session/2
erlmcp_session_manager:delete_session/1
```

#### 2.2.3 Registry Aggregate

**Root**: `erlmcp_registry_behavior` implementations

**Entities**:
- Registry (root)
- Registration (entity)

**Invariants**:
1. Name -> PID mapping unique
2. Monitors cleanup on process exit
3. Distributed consistency (pg scope)

**Consistency Boundary**: gproc local or pg distributed

**Behavior Contract**:
```erlang
-callback register(Type :: entity_type(),
                   Id :: entity_id(),
                   Pid :: pid(),
                   Config :: entity_config()) -> ok | {error, Reason}.

-callback whereis(Type :: entity_type(),
                  Id :: entity_id()) -> {ok, {Pid, Config}} | {error, not_found}.
```

#### 2.2.4 Tool Aggregate

**Root**: Contained within Server state

**Entities**:
- Tool (root within server)
- ToolHandler (entity)

**Value Objects**:
```erlang
-record(mcp_tool, {
    name :: binary(),
    description :: binary(),
    input_schema :: jesse:schema(),
    handler :: tool_handler()
}).
```

**Invariants**:
1. Tool name unique within server
2. Input schema valid (Jesse validation)
3. Handler function exists and is callable

**Lifecycle**:
```erlang
%% Server manages tool lifecycle
erlmcp_server:add_tool/2
erlmcp_server:remove_tool/2
erlmcp_server:call_tool/3
```

#### 2.2.5 Resource Aggregate

**Root**: `erlmcp_resource_subscriptions`

**Entities**:
- Resource (root)
- Subscription (entity)

**Value Objects**:
```erlang
-record(mcp_resource, {
    uri :: binary(),
    name :: binary(),
    description :: binary() | undefined,
    mime_type :: binary() | undefined,
    metadata :: map() | undefined
}).
```

**Invariants**:
1. URI unique within server
2. URI valid format
3. Subscriptions receive updates

**Repository Pattern**:
```erlang
%% Resource subscriptions manager
erlmcp_resource_subscriptions:subscribe/2
erlmcp_resource_subscriptions:unsubscribe/2
erlmcp_resource_subscriptions:list_subscriptions/1
```

#### 2.2.6 RaftLog Aggregate

**Root**: `erlmcp_raft`

**Entities**:
- RaftLog (root)
- LogEntry (entity)

**Value Objects**:
```erlang
-record(raft_log_entry, {
    index :: pos_integer(),
    term :: pos_integer(),
    command :: raft_command(),
    result :: term() | undefined
}).
```

**Invariants**:
1. Log indices sequential (no gaps)
2. Entries immutable after commit
3. Term monotonic increasing
4. Majority consensus required

**Consistency Boundary**: Raft cluster (leader + followers)

**Event Sourcing**: Log IS the event source for configuration changes

---

## 3. Domain Events

### 3.1 Event Catalog

| Event | Source | Subscribers | Payload |
|-------|--------|-------------|---------|
| `ServerInitialized` | Server FSM | Observability | server_id, capabilities |
| `ServerStateTransition` | Server FSM | Observability | server_id, from_state, to_state |
| `SessionCreated` | Session Manager | Failover, Observability | session_id, metadata |
| `SessionExpired` | Session Manager | Cleanup, Observability | session_id |
| `SessionReplicated` | Session Replicator | Cluster | session_id, target_nodes |
| `ToolAdded` | Server | Registry, Observability | server_id, tool_name |
| `ToolCalled` | Server | Observability | server_id, tool_name, result |
| `ResourceSubscribed` | Resource Subscriptions | Observability | resource_uri, subscriber |
| `ResourceUpdated` | Server | Subscribers | resource_uri, new_content |
| `PromptListChanged` | Prompt Manager | Change Notifier | feature |
| `RaftLeaderElected` | Raft | Cluster | node_id, term |
| `RaftLogCommitted` | Raft | State Machine | index, term, command |
| `RaftClusterMemberChanged` | Raft | Cluster | members |
| `CircuitBreakerTripped` | Circuit Breaker | Control Plane | component, reason |
| `CircuitBreakerReset` | Circuit Breaker | Control Plane | component |
| `HealthCheckCompleted` | Health Monitor | Control Plane | component, status, latency_us |
| `ChaosExperimentStarted` | Chaos Engine | Observability | experiment_id, config |
| `ChaosExperimentCompleted` | Chaos Engine | Observability | experiment_id, result |
| `ChaosExperimentRolledBack` | Chaos Engine | Observability | experiment_id, reason |
| `NodeConnected` | Cluster | Registry | node |
| `NodeDisconnected` | Cluster | Registry, Failover | node, reason |
| `TransportConnected` | Transport | Server FSM | transport_type |
| `TransportDisconnected` | Transport | Server FSM | transport_type, reason |
| `DrainStarted` | Control Plane | Server FSM | server_id |
| `DrainCompleted` | Server FSM | Control Plane | server_id |
| `ShutdownInitiated` | Server FSM | All contexts | server_id |
| `MessageReceived` | JSON-RPC | Observability | message_id, method |
| `MessageSent` | JSON-RPC | Observability | message_id, result |
| `ValidationError` | Validation Context | Observability | error_code, details |
| `RateLimitExceeded` | Rate Limiter | Control Plane | client_id, limit |
| `AuthFailed` | Auth | Observability | client_id, reason |
| `AuthSucceeded` | Auth | Observability | client_id |
| `ConfigurationChanged` | Config | All contexts | key, value |
| `CodeReloaded` | Code Reload | All contexts | module |
| `SplitBrainDetected` | Split Brain Detector | Cluster | partitions |
| `ConsensusReached` | Raft | State Machine | decision |
| `PublishMessage` | PubSub | Subscribers | topic, message |
| `SubscriptionCreated` | PubSub | Observability | topic, subscriber |
| `TelemetryRecorded` | Telemetry | Metrics | metric_name, value |
| `TraceStarted` | OTEL | Observability | trace_id |
| `TraceEnded` | OTEL | Observability | trace_id, result |
| `AlertRaised` | Alert Manager | Dashboard | alert_id, severity |
| `AlertCleared` | Alert Manager | Dashboard | alert_id |
| `TaskCreated` | Task API | Observability | task_id |
| `TaskStatusChanged` | Task API | Observability | task_id, status |
| `TaskCancelled` | Control Plane | Task API | task_id, reason |

### 3.2 Event Flow Examples

#### 3.2.1 Server Lifecycle Event Flow

```
[Client]           [Server FSM]       [Observability]      [Control Plane]
   |                     |                    |                      |
   |-- initialize ------> |                    |                      |
   |                     |-- event ---------> |                      |
   |                     |  ServerInitialized |                      |
   |                     |<--- ack --------- |                      |
   |                     |                    |                      |
   |-- tools/list -----> |                    |                      |
   |                     |-- event ---------> |                      |
   |                     |  ToolCalled        |                      |
   |                     |<--- ack --------- |                      |
   |<-- response ------- |                    |                      |
   |                     |                    |                      |
   |-- drain ------------->-------------------->                      |
   |                     |                    |  DrainStarted        |
   |                     |<------------------- |                      |
   |                     |-- event ---------> |                      |
   |                     |  DrainCompleted    |                      |
```

#### 3.2.2 Raft Consensus Event Flow

```
[Leader]           [Followers]         [State Machine]      [Observability]
   |                     |                     |                      |
   |-- propose -------->|                     |                      |
   |                     |-- replicate ------> |                      |
   |                     |<--- ack ---------- |                      |
   |                     |                     |                      |
   |-- commit ---------> |---- commit -------> |                      |
   |                     |                     |-- event -----------> |
   |                     |                     |  RaftLogCommitted    |
```

---

## 4. Ubiquitous Language Glossary

| Term | Definition | Context |
|------|------------|---------|
| **Aggregate** | Consistency boundary with single root entity | DDD Core |
| **Anti-Corruption Layer** | Adapter isolating internal model from external protocols | Integration |
| **Bounded Context** | Linguistic boundary with specific meaning | DDD Core |
| **Capability** | Feature declaration (tools, resources, prompts) | MCP Protocol |
| **Circuit Breaker** | Failure isolation pattern preventing cascading failures | Resilience |
| **Client** | MCP consumer connecting to servers | MCP Protocol |
| **Cluster** | Set of cooperating Erlang nodes | Distributed |
| **Consensus** | Agreement algorithm (Raft) for distributed state | Distributed |
| **Control Plane** | Priority message system for health, drain, cancel | Operations |
| **Drain** | Graceful shutdown phase accepting no new connections | Operations |
| **Entity** | Object with identity spanning lifecycle | DDD Core |
| **Event** | Domain fact recorded in past tense | DDD Core |
| **Follower** | Raft node replicating leader's log | Distributed |
| **Health Check** | Liveness/readiness probe with <100ms SLO | Operations |
| **Leader** | Raft node coordinating log replication | Distributed |
| **Log** | Ordered sequence of Raft operations | Distributed |
| **Message** | JSON-RPC request or response | MCP Protocol |
| **Pool** | Reusable collection of transport connections | Transport |
| **Prompt** | Template for AI-assisted content generation | MCP Protocol |
| **Registry** | Name-to-PID mapping service (gproc/pg) | Core |
| **Resource** | Read-only data with URI and update notifications | MCP Protocol |
| **SLO** | Service Level Objective (e.g., health check <100ms) | Observability |
| **Server** | MCP endpoint hosting tools and resources | MCP Protocol |
| **Session** | Logical client-server connection with metadata | Core |
| **Split Brain** | Network partition causing multiple leaders | Distributed |
| **Steady State** | System baseline behavior for chaos testing | Observability |
| **Term** | Raft logical election period | Distributed |
| **Tool** | Executable function with input schema | MCP Protocol |
| **Trace** | Distributed execution path with correlated spans | Observability |
| **Transport** | Physical communication channel (stdio, TCP, HTTP, WS, SSE) | Transport |
| **Value Object** | Immutable object identified by attributes | DDD Core |

---

## 5. Context Mapping

### 5.1 Relationship Patterns

| Context A | Context B | Pattern | Description |
|-----------|-----------|---------|-------------|
| Protocol Core | Transport Layer | Customer-Supplier | Core defines requirements, Transport implements |
| Protocol Core | Distributed Coordination | Partnership | Core uses distributed features for HA |
| Protocol Core | Observability | Published Language | Core emits events, Observability consumes |
| Protocol Core | Validation | Supplier | Validation provides compliance results |
| Transport Layer | Observability | Open Host Service | Transport exposes metrics via standard interface |
| Distributed Coordination | Observability | Partnership | Coordination uses tracing for consensus debugging |
| Validation | MCP Specification | Anti-Corruption Layer | Validates external spec compliance |

### 5.2 Context Mapping Diagram

```
+----------------------+         +------------------------+
|  MCP Specification    |<------>| Validation Context     |
|  (External System)   |  ACL   |  (Anti-Corruption)      |
+----------------------+         +------------------------+
        |                                      ^
        |                                      | Supplier
        v                                      |
+----------------------+         +------------------------+
|  Protocol Core        |<------>| Distributed Coord.     |
|  (Core Domain)        | Partner |  (Supporting)           |
+----------------------+         +------------------------+
        |                                      ^
        | Customer                             |
        v                                      |
+----------------------+         +------------------------+
|  Transport Layer      |<------>| Observability           |
|  (Generic Domain)     |  OHS   |  (Supporting)           |
+----------------------+         +------------------------+

Legend:
  ACL - Anti-Corruption Layer
  OHS - Open Host Service
  Partner - Partnership
  Customer - Customer-Supplier
  Supplier - Supplier relationship
```

---

## 6. Event Sourcing Analysis

### 6.1 Current Event Sourcing Usage

| Component | Event Store | Events | Projection |
|-----------|-------------|--------|------------|
| **Raft Log** | ETS (`erlmcp_raft_log`) | RaftLogCommitted | State machine state |
| **Session** | ETS/DETS/Mnesia | SessionCreated, SessionUpdated | Session queries |
| **Audit Log** | Persistent storage | All domain events | Compliance reports |
| **Receipt Chain** | `erlmcp_receipt_chain` | Operations | Immutable audit trail |

### 6.2 Event Sourcing Recommendations

**Strengths**:
1. Raft log provides proven event sourcing pattern for configuration
2. Audit log captures domain events for compliance
3. Receipt chain enables immutable operation history

**Weaknesses**:
1. Events scattered across multiple stores
2. No unified event bus for cross-context communication
3. Limited replay capability outside Raft

**Recommended Actions**:
1. **Centralize Event Store**: Create `erlmcp_event_store` as append-only log
2. **Event Bus**: Implement `erlmcp_event_bus` for pub/sub across contexts
3. **Projections**: Separate read models from write models
4. **Replay**: Add event replay for debugging/testing

### 6.3 Proposed Event Sourcing Architecture

```
+----------------+     +----------------+     +-----------------+
| Command Handler |---->| Event Store    |---->| Projections     |
| (Aggregate)    |     | (Append-only)  |     | (Read Models)   |
+----------------+     +----------------+     +-----------------+
        |                       |                       |
        v                       v                       v
   [State Change]        [Event Log]            [Queries]
                                                     |
                                                     v
                                              +-----------+
                                              | Event Bus |
                                              +-----------+
                                                     |
                        +----------------------------+----------------------------+
                        v                            v                            v
                  +-----------+               +-----------+               +-----------+
                  | Protocol  |               | Observa-  |               | Distrib-   |
                  | Core      |               | bility    |               | uted      |
                  | Subscrip. |               | Subscrip. |               | Subscrip. |
                  +-----------+               +-----------+               +-----------+
```

---

## 7. CQRS Opportunities

### 7.1 Current CQRS Implementation

| Aggregate | Command Side | Query Side | Separation |
|-----------|--------------|------------|------------|
| **Server** | Server FSM | Tool/Resource lists | Partial |
| **Session** | Session Manager | Session queries | Partial |
| **Raft** | Leader writes | Follower reads | Full |
| **Tools** | add_tool/remove_tool | list_tools | Partial |

### 7.2 CQRS Recommendations

**Current State**: Partial CQRS implementation
- Raft achieves full read/write separation (leader writes, followers read)
- Other aggregates have command/query methods in same module

**Recommended Refactoring**:
1. **Separate Command/Query Models**:
   ```erlang
   %% Command side (write)
   -module(erlmcp_server_command).
   -export([add_tool/2, remove_tool/2, call_tool/3]).

   %% Query side (read)
   -module(erlmcp_server_query).
   -export([list_tools/1, get_tool/2, find_tools_by_schema/2]).
   ```

2. **Read Model Projections**:
   ```erlang
   %% Projection for tool listings
   -module(erlmcp_tools_projection).
   -export([handle_event/2, get_tool_list/1]).
   ```

3. **Event-Driven Updates**:
   ```erlang
   %% Command emits event -> Projection updates
   handle_command({add_tool, Tool}, State) ->
       {ok, Events} = do_add_tool(Tool, State),
       emit_events(Events),
       {ok, NewState};
   ```

### 7.3 CQRS Benefits for erlmcp

1. **Scalability**: Read-optimized queries scale independently
2. **Performance**: Query models optimized for access patterns
3. **Flexibility**: Multiple read models from single event stream
4. **Validation**: Separate validation for commands

---

## 8. Anti-Corruption Layer

### 8.1 External Integrations Requiring ACL

| External System | ACL Location | Responsibility |
|-----------------|--------------|----------------|
| **MCP Specification** | Validation Context | Translate MCP spec to internal model |
| **OpenTelemetry** | Observability Context | Normalize OTEL protocol |
| **JSON-RPC 2.0** | Protocol Core | Enforce MCP constraints |
| **Transport Protocols** | Transport Layer | Abstract physical differences |
| **Storage Backends** | Session Backends | Unified session interface |

### 8.2 ACL Examples

#### 8.2.1 MCP Specification ACL

```erlang
%% External: MCP spec format
%% Internal: Native Erlang records

-module(erlmcp_mspec_acl).

%% External -> Internal translation
-spec from_external(mcp_spec_map()) -> #mcp_server_capabilities{}.
from_external(#{<<"capabilities">> := CapMap}) ->
    #mcp_server_capabilities{
        tools = maps:get(<<"tools">>, CapMap, false),
        resources = maps:get(<<"resources">>, CapMap, false),
        prompts = translate_prompts(maps:get(<<"prompts">>, CapMap, #{}))
    }.

%% Internal -> External translation
-spec to_external(#mcp_server_capabilities{}) -> mcp_spec_map().
to_external(Capabilities) ->
    #{<<"capabilities">> => #{
        <<"tools">> => Capabilities#mcp_server_capabilities.tools,
        <<"resources">> => Capabilities#mcp_server_capabilities.resources,
        <<"prompts">> => reverse_prompts(Capabilities#mcp_server_capabilities.prompts)
    }}.
```

#### 8.2.2 OpenTelemetry ACL

```erlang
-module(erlmcp_otel_acl).

%% Normalize OTEL spans to internal format
-spec normalize_span(opentelemetry:span()) -> erlmcp_span().
normalize_span(OTelSpan) ->
    #erlmcp_span{
        trace_id = extract_trace_id(OTelSpan),
        span_id = extract_span_id(OTelSpan),
        parent_id = extract_parent_id(OTelSpan),
        name = maps:get(<<"name">>, OTelSpan, <<>>),
        start_time = maps:get(<<"start_time">>, OTelSpan, 0),
        end_time = maps:get(<<"end_time">>, OTelSpan, 0),
        attributes = normalize_attributes(maps:get(<<"attributes">>, OTelSpan, #{}))
    }.
```

---

## 9. Refactoring Recommendations

### 9.1 Priority 1: Bounded Context Separation

**Issue**: Transport modules directly import from core
**Impact**: Tight coupling, difficult to evolve independently

**Recommendation**:
1. Create explicit interfaces between contexts
2. Use behavior modules for context boundaries
3. Implement ACL for all cross-context communication

**Files to Refactor**:
- `apps/erlmcp_transports/src/*` - Remove direct core imports
- Create `apps/erlmcp_core/src/erlmcp_transport_acl.erl`

### 9.2 Priority 2: Aggregate Boundary Refinement

**Issue**: Some aggregates have unclear boundaries
**Impact**: Consistency violations, race conditions

**Recommendation**:
1. Refine Tool/Resource aggregates within Server
2. Establish clear Session aggregate boundary
3. Implement Repository pattern for all aggregates

**Files to Create**:
- `apps/erlmcp_core/src/erlmcp_server_repository.erl`
- `apps/erlmcp_core/src/erlmcp_session_repository.erl`
- `apps/erlmcp_core/src/erlmcp_tool_repository.erl`

### 9.3 Priority 3: Event Bus Implementation

**Issue**: Ad-hoc event notifications
**Impact**: No centralized event handling, difficult to track

**Recommendation**:
1. Implement `erlmcp_event_bus` using gen_server
2. Register subscribers for event types
3. Use for cross-context communication

**Files to Create**:
- `apps/erlmcp_core/src/erlmcp_event_bus.erl`
- `apps/erlmcp_core/src/erlmcp_event_store.erl`

### 9.4 Priority 4: Ubiquitous Language Enforcement

**Issue**: Inconsistent terminology across modules
**Impact**: Confusion, maintenance burden

**Recommendation**:
1. Create `erlmcp_domain.hrl` with type definitions
2. Enforce naming conventions in code review
3. Document glossary in module headers

**Files to Update**:
- Add type specifications to all public APIs
- Create glossary module: `apps/erlmcp_core/src/erlmcp_glossary.erl`

### 9.5 Priority 5: CQRS Implementation

**Issue**: Commands and queries mixed in same modules
**Impact**: Optimization conflicts, complex code

**Recommendation**:
1. Separate command and query modules
2. Create read model projections
3. Optimize queries independently

**Files to Create**:
- `apps/erlmcp_core/src/erlmcp_server_command.erl`
- `apps/erlmcp_core/src/erlmcp_server_query.erl`
- `apps/erlmcp_core/src/erlmcp_tools_projection.erl`

---

## 10. Implementation Roadmap

### Phase 1: Foundation (Weeks 1-2)
- [ ] Create event bus and event store
- [ ] Define domain event catalog
- [ ] Implement ubiquitous language types

### Phase 2: Refactoring (Weeks 3-4)
- [ ] Separate bounded contexts
- [ ] Refine aggregate boundaries
- [ ] Implement repository pattern

### Phase 3: CQRS (Weeks 5-6)
- [ ] Separate command/query models
- [ ] Create read model projections
- [ ] Optimize query paths

### Phase 4: Event Sourcing (Weeks 7-8)
- [ ] Extend event sourcing beyond Raft
- [ ] Implement replay capability
- [ ] Create migration tools

### Phase 5: Testing (Weeks 9-10)
- [ ] Property-based tests for aggregates
- [ ] Chaos tests for consistency
- [ ] Integration tests for contexts

---

## 11. Conclusion

erlmcp v3 demonstrates a solid foundation for enterprise-scale DDD implementation. The system already implements several DDD patterns:

**Strengths**:
- Clear bounded contexts (5 identified)
- Strong aggregate boundaries (8 defined)
- Event-driven architecture foundation
- Raft consensus for distributed coordination
- Comprehensive observability

**Areas for Improvement**:
- Context coupling needs reduction
- Event handling needs centralization
- CQRS needs full implementation
- Ubiquitous language needs enforcement

**Recommended Priority**:
1. Event bus implementation (enables other improvements)
2. Aggregate boundary refinement (ensures consistency)
3. CQRS separation (improves performance)
4. ACL strengthening (enables independent evolution)

---

## Appendix A: Module Inventory

### Core Application (97 modules)

**Protocol**:
- erlmcp_json_rpc, erlmcp_message_parser, erlmcp_request_id

**Server/Client**:
- erlmcp_server_fsm, erlmcp_client_fsm, erlmcp_server_sup, erlmcp_client_sup

**Session**:
- erlmcp_session, erlmcp_session_backend, erlmcp_session_ets, erlmcp_session_dets, erlmcp_session_mnesia, erlmcp_session_failover, erlmcp_session_replicator

**Security**:
- erlmcp_auth_mtls, erlmcp_auth_rate_limiter, erlmcp_secrets

**Registry**:
- erlmcp_registry_behavior, erlmcp_registry_distributed, erlmcp_registry_utils

**Distributed**:
- erlmcp_raft, erlmcp_cluster, erlmcp_failover_worker, erlmcp_split_brain_detector

**Resources**:
- erlmcp_resource, erlmcp_resource_subscriptions, erlmcp_prompt_template

**Resilience**:
- erlmcp_circuit_breaker_poc, erlmcp_resilience_sup

**Validation**:
- erlmcp_strict_validation

### Transport Application (23 modules)

**Behaviors**:
- erlmcp_transport_behavior, erlmcp_transport_contracts

**Implementations**:
- erlmcp_transport_stdio, erlmcp_transport_tcp, erlmcp_transport_http, erlmcp_transport_ws, erlmcp_transport_sse

**Infrastructure**:
- erlmcp_transport_pool, erlmcp_transport_pipeline, erlmcp_transport_registry

**Security**:
- erlmcp_http_header_validator, erlmcp_origin_validator, erlmcp_tls_validation

### Observability Application (31 modules)

**OTEL**:
- erlmcp_otel, erlmcp_otel_jaeger, erlmcp_otel_datadog, erlmcp_otel_honeycomb

**Chaos**:
- erlmcp_chaos, erlmcp_chaos_network, erlmcp_chaos_process, erlmcp_chaos_resource

**Monitoring**:
- erlmcp_health_monitor, erlmcp_dashboard_server, erlmcp_metrics, erlmcp_tracer

**Audit**:
- erlmcp_audit_log, erlmcp_receipt_chain, erlmcp_event_audit

### Validation Application (13 modules)

**Validators**:
- erlmcp_protocol_validator, erlmcp_transport_validator, erlmcp_security_validator

**Compliance**:
- erlmcp_compliance_report, erlmcp_compliance_monitor, erlmcp_quality_gates

**CLI**:
- erlmcp_validate_cli, erlmcp_cli_diagnostics

---

**Document Version**: 1.0.0
**Last Updated**: 2026-02-02
**Authors**: DDD Analysis Agent
**Status**: Enterprise Architecture Review
