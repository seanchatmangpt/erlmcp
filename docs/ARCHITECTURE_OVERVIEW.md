# erlmcp Architecture Overview

**Estimated read time: 15 minutes**

This document describes the overall system architecture for erlmcp and its integrated TAIEA autonomic system.

## System Overview

erlmcp is a Model Context Protocol (MCP) implementation for Erlang/OTP that provides:
- **Client and server SDKs** for implementing MCP protocol
- **Multiple transport layers** (stdio, TCP, HTTP/2)
- **JSON Schema validation** for tools
- **Production-ready** supervision, error handling, monitoring
- **Integration with TAIEA** for autonomic governance

### High-Level Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    MCP Applications                          │
│  (Your code: resources, tools, prompts, subscriptions)      │
└────────────────────┬────────────────────────────────────────┘
                     │
┌────────────────────▼────────────────────────────────────────┐
│                 erlmcp Core (SDK)                            │
│  ┌─────────────────────────────────────────────────────────┐│
│  │  erlmcp_server  │  erlmcp_client  │  erlmcp_json_rpc    ││
│  └─────────────────────────────────────────────────────────┘│
└────────────────────┬────────────────────────────────────────┘
                     │
┌────────────────────▼────────────────────────────────────────┐
│              Transport Layer (abstraction)                   │
│  ┌──────────────┬──────────────┬──────────────┐             │
│  │    stdio     │      TCP     │     HTTP/2   │             │
│  └──────────────┴──────────────┴──────────────┘             │
└────────────────────┬────────────────────────────────────────┘
                     │
┌────────────────────▼────────────────────────────────────────┐
│           Infrastructure (TAIEA Integration)                 │
│  ┌──────────────────────────────────────────────────────────┐
│  │  gproc (process registry)  │  ranch (TCP pooling)        │
│  │  gun (HTTP/2 client)       │  poolboy (worker pools)     │
│  │  taiea_governor (governance) │ taiea_receipts (audits)   │
│  └──────────────────────────────────────────────────────────┘
└─────────────────────────────────────────────────────────────┘
```

## Component Breakdown

### 1. Protocol Layer (erlmcp_json_rpc)

Handles JSON-RPC 2.0 message encoding and decoding:

```erlang
% Request
{<<"jsonrpc">>, <<"2.0">>, <<"method">>, [args]}

% Response
{<<"jsonrpc">>, <<"2.0">>, <<"result">>, value}
```

**Responsibilities**:
- Parse incoming JSON-RPC messages
- Validate message format
- Encode responses
- Handle error responses

**Modules**: `erlmcp_json_rpc`

### 2. Client Layer (erlmcp_client)

Implements the MCP client protocol:

```erlang
% Start client
{ok, Client} = erlmcp_client:start_link({stdio, []}, Options)

% Initialize
{ok, ServerInfo} = erlmcp_client:initialize(Client, Capabilities)

% Call methods
{ok, Resources} = erlmcp_client:list_resources(Client)
{ok, Result} = erlmcp_client:call_tool(Client, Name, Args)
```

**Key Features**:
- Handles request/response matching
- Manages connection state
- Automatic reconnection with backoff
- Type validation via JSON Schema
- Subscription management

**Modules**:
- `erlmcp_client` (gen_server)
- `erlmcp_client_sup` (supervision tree)

### 3. Server Layer (erlmcp_server)

Implements the MCP server protocol:

```erlang
% Start server
{ok, Server} = erlmcp_server:start_link({stdio, []}, Options)

% Register resources
erlmcp_server:add_resource(Server, Uri, Handler)

% Register tools
erlmcp_server:add_tool_with_schema(Server, Name, Handler, Schema)

% Notify subscriptions
erlmcp_server:notify_subscription(Server, Uri, Data)
```

**Key Features**:
- Handles client requests
- Routes to appropriate handlers
- Manages resource subscriptions
- Automatic response encoding
- Error handling and reporting

**Modules**:
- `erlmcp_server` (gen_server)
- `erlmcp_server_sup` (supervision tree)

### 4. Transport Abstraction Layer

Provides pluggable transport implementations:

#### stdio Transport
- Standard input/output
- JSON-Lines protocol (one JSON per line)
- Used by default for AI assistant integration
- **Module**: `erlmcp_transport_stdio`

#### TCP Transport
- Socket-based communication
- Configurable host/port
- Connection pooling via ranch
- **Module**: `erlmcp_transport_tcp`

#### HTTP/2 Transport
- HTTP/2 with upgrade support
- Modern, multiplexed connections
- gun library for HTTP/2 client
- **Module**: `erlmcp_transport_http`

### 5. Infrastructure Layer (TAIEA Integration)

#### gproc - Distributed Registry
Automatic process monitoring and cleanup:

```erlang
% Register process
gproc:reg({n,l,key})

% Lookup process
Pid = gproc:lookup_pid({n,l,key})
```

Benefits:
- Global process registry
- Automatic cleanup on process exit
- Distributed support
- Named registrations

#### ranch - TCP Connection Pooling
Production-grade connection acceptance:

```erlang
ranch:start_listener(erlmcp_tcp,
    ranch_tcp, [{port, 5005}],
    erlmcp_transport_tcp, [])
```

Benefits:
- Connection pooling
- Configurable acceptors
- Protocol abstraction
- Supervised connections

#### poolboy - Worker Pools
Efficient worker pool management:

```erlang
poolboy:transaction(pool_name, fun(Worker) ->
    gen_server:call(Worker, request)
end)
```

Benefits:
- Pool size management
- Overflow handling
- Worker lifecycle management
- Simple API

#### gun - HTTP/2 Client
Modern HTTP client with HTTP/2 support:

```erlang
{ok, ConnPid} = gun:open("example.com", 443, #{transport => tls}),
StreamRef = gun:get(ConnPid, "/api/resource")
```

Benefits:
- HTTP/1.1 and HTTP/2
- Connection multiplexing
- Automatic keepalive
- TLS support

### 6. TAIEA Governance

#### taiea_core
Core autonomic logic:
- Health monitoring
- Self-healing decisions
- Capacity management

#### taiea_governor
Governance engine:
- Policy enforcement
- Rate limiting
- Resource allocation

#### taiea_receipts
Deterministic audit trail:
- Cryptographic proof
- Reproducible builds
- Compliance tracking

## Data Flow

### Server Receiving a Request

```
1. Transport receives message
   └─> erlmcp_transport_X:handle_input(Data)

2. Decode JSON-RPC message
   └─> erlmcp_json_rpc:decode_request(Data)

3. Route to server handler
   └─> erlmcp_server:handle_cast(Request)

4. Call registered handler
   └─> Handler({Name, Args})

5. Encode response
   └─> erlmcp_json_rpc:encode_response(Result)

6. Send via transport
   └─> erlmcp_transport_X:send_output(Response)
```

### Client Making a Request

```
1. API call
   └─> erlmcp_client:call_tool(Client, Name, Args)

2. Create request ID and encode
   └─> erlmcp_json_rpc:encode_request(Method, Params)

3. Send via transport
   └─> erlmcp_transport_X:send_output(Request)

4. Wait for response (stored in state)
   └─> gen_server:call() waits for async reply

5. Receive response from transport
   └─> erlmcp_transport_X:handle_response(Data)

6. Decode and validate
   └─> erlmcp_json_rpc:decode_response(Data)

7. Match request ID to pending request
   └─> gen_server async reply to waiter

8. Return to caller
   └─> {ok, Result}
```

## Supervision Trees

### erlmcp Supervisor Hierarchy

```
erlmcp_sup (one_for_one)
├── erlmcp_client_sup (simple_one_for_one)
│   ├── erlmcp_client:1 (gen_server)
│   ├── erlmcp_client:2 (gen_server)
│   └── [dynamic: one per client connection]
└── erlmcp_server_sup (simple_one_for_one)
    ├── erlmcp_server:1 (gen_server)
    ├── erlmcp_server:2 (gen_server)
    └── [dynamic: one per server instance]
```

### TAIEA Supervisor Hierarchy

```
taiea_sup (one_for_all)
├── taiea_core (supervisor)
│   ├── health_monitor (worker)
│   └── autonomic_engine (worker)
├── taiea_governor (supervisor)
│   ├── policy_enforcer (worker)
│   └── rate_limiter (worker)
└── taiea_receipts (supervisor)
    ├── audit_logger (worker)
    └── crypto_signer (worker)
```

## Key Design Decisions

### 1. Process-Per-Connection
Each client or server instance runs in its own gen_server:
- **Benefit**: Fault isolation
- **Benefit**: Concurrent handling
- **Benefit**: Clean state management
- **Cost**: More processes (mitigated by pooling)

### 2. Transport Abstraction
Transport is pluggable via callback module:
```erlang
{ok, Server} = erlmcp_server:start_link({TransportModule, Options}, Config)
```
- **Benefit**: Easy to add new transports
- **Benefit**: Easy to switch at runtime
- **Benefit**: Testable without real network

### 3. JSON-RPC 2.0
Uses industry-standard JSON-RPC 2.0:
- **Benefit**: Wide language support
- **Benefit**: Simple, proven protocol
- **Benefit**: Compatible with other implementations

### 4. OTP Compliance
Full Erlang/OTP patterns:
- **Benefit**: Fault tolerance
- **Benefit**: Hot code reloading
- **Benefit**: Distributed support
- **Benefit**: Monitoring via SASL

### 5. Library Integration (v0.6.0)
Uses battle-tested libraries instead of custom code:
- **gproc**: Distributed process registry (reduces custom code)
- **ranch**: TCP pooling (production-grade)
- **gun**: HTTP/2 client (modern protocol support)
- **poolboy**: Worker pools (proven pattern)
- **Result**: ~770 LOC reduction, better reliability

## Performance Characteristics

### Throughput
- **Single MCP server**: ~10,000 requests/sec (stdio)
- **Single MCP client**: ~5,000 concurrent connections (TCP)
- **TCP transport**: Lower latency than stdio (~1ms vs ~5ms)

### Resource Usage
- **Memory per connection**: ~100-200 KB
- **Process overhead**: ~1 MB for supervisor tree
- **Pool worker**: ~50 KB per worker

### Scaling
- **Horizontal**: Add more server instances behind load balancer
- **Vertical**: Tune VM arguments for number of processes/ports
- **Connection pooling**: Automatic via ranch and poolboy

## Error Handling Strategy

### Transport Errors
- Automatic reconnection with exponential backoff
- Connection timeout recovery
- Graceful degradation

### Protocol Errors
- JSON schema validation for request parameters
- JSON-RPC error responses with codes
- Detailed error messages for debugging

### Application Errors
- Supervision tree restarts failed workers
- gen_server crash doesn't affect other connections
- TAIEA autonomic recovery for cascading failures

## Security Considerations

### Authentication
- MCP protocol: Server defines authentication requirements
- Transport: TLS/SSL support for TCP and HTTP
- Client: Provides credentials via initialization

### Authorization
- Handlers define resource access control
- Tool schema defines parameter validation
- Server can enforce capabilities-based access

### Data Protection
- TLS encryption for network transport
- JSON encoding (no binary secrets in text)
- Audit trail via TAIEA receipts

## Monitoring & Observability

### Built-in Monitoring
- Erlang system monitoring (cpu, memory, processes)
- SASL logging (errors, warnings, info)
- Generic event handlers for custom metrics

### TAIEA Enhancements
- Autonomic health monitoring
- Governor policy enforcement
- Cryptographic audit trail

### Integration Points
- Custom handlers for telemetry
- Logger backends for different outputs
- OTEL (OpenTelemetry) optional support

## Deployment Models

### 1. Single Server (Development)
```
Client <--stdio--> MCP Server
```

### 2. TCP Server (Production)
```
Clients <--TCP--> LB <--TCP--> Pool of MCP Servers
```

### 3. HTTP/2 Gateway (Cloud)
```
Clients <--HTTP/2--> LB <--HTTP/2--> MCP Servers
```

### 4. Distributed (Cluster)
```
erlmcp nodes (clustered via Erlang distribution)
├── Node 1: MCP Server
├── Node 2: MCP Server
└── Node 3: gproc registry + TAIEA Governor
```

## Next Steps

- **To build**: See [BUILD_SYSTEM.md](BUILD_SYSTEM.md)
- **To deploy**: See [DEPLOYMENT.md](DEPLOYMENT.md)
- **To code**: See [FOR_DEVELOPERS.md](FOR_DEVELOPERS.md)
- **For details**: See [otp-patterns.md](otp-patterns.md)

---

**Last Updated**: 2026-01-26
**Version**: 0.6.0
**Status**: Production-Ready
