# erlmcp Architecture

## Overview

The erlmcp SDK is built on Erlang/OTP principles, providing a robust and fault-tolerant implementation of the Model Context Protocol.

## System Components

```
┌─────────────────┐     ┌─────────────────┐
│   MCP Client    │     │   MCP Server    │
├─────────────────┤     ├─────────────────┤
│ erlmcp_client   │────▶│ erlmcp_server   │
│ (gen_server)    │     │ (gen_server)    │
└────────┬────────┘     └────────┬────────┘
         │                       │
         ▼                       ▼
┌─────────────────┐     ┌─────────────────┐
│ Transport Layer │     │ Transport Layer │
├─────────────────┤     ├─────────────────┤
│ - stdio         │     │ - stdio         │
│ - TCP           │     │ - TCP           │
│ - HTTP          │     │ - HTTP          │
└─────────────────┘     └─────────────────┘
```

## Core Modules

### Protocol Layer
- **erlmcp_json_rpc** - JSON-RPC 2.0 message encoding/decoding
- **erlmcp.hrl** - Protocol type definitions and records

### Client Components
- **erlmcp_client** - Main client gen_server
- **erlmcp_client_sup** - Client supervisor for managing multiple connections

### Server Components
- **erlmcp_server** - Main server gen_server
- **erlmcp_server_sup** - Server supervisor

### Transport Modules
- **erlmcp_transport_stdio** - Standard I/O transport
- **erlmcp_transport_tcp** - TCP socket transport
- **erlmcp_transport_http** - HTTP transport

## Design Principles

### 1. Process Isolation
Each connection runs in its own process, ensuring:
- Fault isolation
- Independent state management
- Concurrent operation

### 2. Supervision Trees
```
erlmcp_sup
├── erlmcp_client_sup
│   └── erlmcp_client (dynamic)
└── erlmcp_server_sup
    └── erlmcp_server (dynamic)
```

### 3. Message Flow

**Client Request Flow:**
1. API call → gen_server:call
2. Encode request (JSON-RPC)
3. Send via transport
4. Await response
5. Decode response
6. Return to caller

**Server Request Flow:**
1. Receive message from transport
2. Decode request
3. Route to handler
4. Execute handler
5. Encode response
6. Send response

### 4. State Management

**Client State:**
```erlang
#state{
    transport :: module(),
    transport_state :: term(),
    capabilities :: #mcp_server_capabilities{},
    request_id :: integer(),
    pending_requests :: map(),
    subscriptions :: sets:set()
}
```

**Server State:**
```erlang
#state{
    transport :: module(),
    transport_state :: term(),
    capabilities :: #mcp_server_capabilities{},
    resources :: map(),
    tools :: map(),
    prompts :: map(),
    subscriptions :: map()
}
```

## Library Integration (v0.6.0)

### Production-Grade Libraries

erlmcp v0.6.0 replaces ~770 LOC of custom code with battle-tested Erlang libraries:

| Component | Library | Version | Purpose |
|-----------|---------|---------|---------|
| **Registry** | gproc | 0.9.0 | Process registration and discovery |
| **HTTP Client** | gun | 2.0.1 | HTTP/1.1 and HTTP/2 transport |
| **TCP Handler** | ranch | 2.1.0 | TCP connection pooling and supervision |
| **Connection Pool** | poolboy | 1.5.2 | Worker pool management |

#### Why These Libraries?

**gproc (Registry)**
- Distributed process registry with automatic cleanup
- Built-in monitoring eliminates manual process tracking
- Used across Erlang ecosystem for production workloads
- Reduces registry code from 411 LOC → 120 LOC

**gun (HTTP Transport)**
- Modern HTTP/1.1 and HTTP/2 support
- Better connection reuse and keepalive
- Production-grade error handling
- Reduces HTTP transport from 461 LOC → 180 LOC

**ranch (TCP Transport)**
- Battle-tested by EMQX and Cowboy
- Built-in connection pooling and supervisor integration
- Excellent socket lifecycle management
- Reduces TCP transport from 349 LOC → 150 LOC

**poolboy (Connection Pooling)**
- Efficient worker pool management
- Queue management under load
- Better resource utilization
- New feature in v0.6.0

### Registry Architecture with gproc

```erlang
%% Server registration
gproc:add_local_name({mcp, server, ServerId}),
gproc:reg({p, l, {mcp_server_config, ServerId}}, Config).

%% Transport registration
gproc:add_local_name({mcp, transport, TransportId}),
gproc:reg({p, l, {mcp_transport_config, TransportId}}, Config).

%% Lookup (automatic monitoring)
case gproc:lookup_local_name({mcp, server, ServerId}) of
    undefined -> {error, not_found};
    Pid -> {ok, Pid}
end.
```

**Benefits:**
- Automatic process monitoring and cleanup
- No manual `monitor`/`demonitor` code
- Distributed registry support (if needed)
- Proven reliability

### HTTP Transport with gun

```erlang
%% Initialize gun connection
{ok, GunPid} = gun:open(Host, Port, #{
    protocols => [http2, http],
    retry => 5,
    retry_timeout => 1000
}),

%% Send request
StreamRef = gun:request(GunPid, <<"POST">>, Path, Headers, Body),

%% Handle responses
handle_info({gun_response, GunPid, StreamRef, IsFin, Status, Headers}, State) ->
    %% Process response
    {noreply, State};
handle_info({gun_data, GunPid, StreamRef, IsFin, Data}, State) ->
    %% Route to server via registry
    erlmcp_registry:route_to_server(ServerId, TransportId, Data),
    {noreply, State}.
```

**Features:**
- ✅ HTTP/1.1 and HTTP/2 support (automatic)
- ✅ Connection reuse and keepalive
- ✅ TLS/SSL support
- ✅ Timeout handling
- ✅ Better error recovery

### TCP Transport with ranch

```erlang
%% Server mode - accept connections
ranch:start_listener(
    TransportId,
    ranch_tcp,
    #{port => Port, num_acceptors => 10},
    erlmcp_transport_tcp,
    [TransportId, Config]
).

%% Client mode - connect outbound
{ok, Socket} = gen_tcp:connect(Host, Port, [
    binary,
    {active, true},
    {packet, 0}
]).
```

**Features:**
- ✅ Built-in connection pooling
- ✅ Supervisor integration (ranch handles restarts)
- ✅ TCP_NODELAY and keepalive built-in
- ✅ Backpressure handling
- ✅ Graceful shutdown

### Connection Pooling with poolboy

```erlang
%% Start connection pool
poolboy:start_link([
    {name, {local, http_pool}},
    {worker_module, erlmcp_http_worker},
    {size, 10},
    {max_overflow, 5}
]).

%% Use pooled connection
poolboy:transaction(http_pool, fun(Worker) ->
    erlmcp_http_worker:request(Worker, Url, Method)
end).
```

**Benefits:**
- Better performance under load
- Resource limiting
- Queue management
- Connection reuse

## Extension Points

### Custom Transports
Implement the enhanced transport behavior:
```erlang
-callback init(TransportId :: atom(), Config :: map()) ->
    {ok, State} | {error, Reason}.
-callback send(State, Data :: iodata()) ->
    ok | {error, Reason}.
-callback close(State) -> ok.

%% Optional callbacks
-callback get_info(State) ->
    #{type => atom(), status => atom(), peer => term()}.
-callback handle_transport_call(Request :: term(), State :: term()) ->
    {reply, Reply :: term(), NewState :: term()} | {error, Reason}.

-optional_callbacks([get_info/1, handle_transport_call/2]).
```

### Resource Handlers
```erlang
-type resource_handler() :: fun((Uri :: binary()) ->
    binary() | #mcp_content{}).
```

### Tool Handlers
```erlang
-type tool_handler() :: fun((Args :: map()) ->
    binary() | #mcp_content{} | [#mcp_content{}]).
```

## Performance Considerations

1. **Process Pooling** - poolboy manages connection pools
2. **Registry Performance** - gproc optimized for lookups
3. **HTTP/2** - gun provides HTTP/2 multiplexing
4. **ETS for Caching** - Cache frequently accessed resources
5. **Binary Handling** - Use binary strings for efficiency
6. **Lazy Evaluation** - Handlers are called only when needed

### Performance Characteristics

**Registry Lookups (gproc)**
- Local lookups: O(1) via ETS
- Distributed lookups: O(1) with gproc_dist
- Automatic cleanup on process death

**HTTP Transport (gun)**
- HTTP/2 multiplexing: Multiple streams per connection
- Connection reuse: Reduces handshake overhead
- Keepalive: Maintains persistent connections

**TCP Transport (ranch)**
- Connection pooling: Reuses established connections
- Backpressure: Handles overload gracefully
- Supervisor integration: Fast recovery

## Security Model

- Transport-level security (TLS for TCP/HTTP via gun and ranch)
- Input validation via JSON Schema
- Capability-based access control
- No direct code execution
- Library-provided security features (TLS, certificate validation)

---

## v1.3.0: Supervision Tree with Bulkheads (Failure Isolation)

### Problem Addressed
The original flat supervision tree used `one_for_all` strategy, meaning any component failure could cascade and restart the entire system. This caused:
- Transport failures → entire system restart
- Registry failures → all connections dropped
- Monitoring failures → protocol layer restart

### Solution: Five-Tier Isolation Model

The v1.3.0 redesign implements **bulkhead pattern** with `rest_for_one` at the top level:

```
erlmcp_sup (rest_for_one)
├── TIER 1: erlmcp_registry_sup (one_for_one)
│   └── No dependencies → Can fail independently
├── TIER 2: erlmcp_infrastructure_sup (one_for_one)
│   └── Depends on: Registry
├── TIER 3: erlmcp_server_sup (simple_one_for_one)
│   └── Depends on: Registry, Infrastructure
├── TIER 4: erlmcp_transport_sup (simple_one_for_one)
│   └── Depends on: All above
└── TIER 5: erlmcp_monitoring_sup (one_for_one) [INDEPENDENT]
    └── No protocol dependencies
```

### Failure Mode Analysis

| Failure | Isolation | Recovery | Impact |
|---------|-----------|----------|--------|
| **Registry crash** | Restart registry only (TIER 2-4 continue) | <500ms | New routing fails; existing connections continue |
| **Infrastructure crash** | Restart infrastructure (TIER 3-4 continue) | <1s | New sessions fail; existing connections continue |
| **Transport crash** | Restart transport only | <2s | Network connections lost; protocol servers unaffected |
| **Monitoring crash** | Isolated from protocol layer | <500ms | No observability; protocol layer 100% unaffected |
| **Cascade attempt** | Prevented by rest_for_one | N/A | No cascading failures observed |

### Key Improvements

**Before (v1.2.0)**:
```
one_for_all strategy
├─ Registry crash → Restart ALL (servers, transports, monitoring)
├─ Transport crash → Restart ALL
└─ Impact: ~3-5s downtime, all connections drop
```

**After (v1.3.0)**:
```
rest_for_one strategy with tiered architecture
├─ Registry crash → Restart Registry only (~500ms)
├─ Transport crash → Restart Transport only (~2s)
└─ Monitoring crash → Isolated, NO protocol impact (~500ms)
```

### Testing
See `test/erlmcp_supervision_SUITE.erl` for comprehensive failure scenario testing:
- Tree structure validation
- Failure isolation verification
- Recovery time measurement
- Connection survival rates
- Cascading failure prevention
- System stability under repeated crashes

Run tests: `rebar3 ct --suite=erlmcp_supervision_SUITE`

### Documentation
Complete architecture with diagrams: `docs/c4/supervision-v1.3.0.md`