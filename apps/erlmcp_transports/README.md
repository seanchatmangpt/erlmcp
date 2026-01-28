# erlmcp_transports - Transport Layer

**Version:** 2.0.0
**Application:** erlmcp_transports
**Modules:** 11

Production-grade transport implementations for the Model Context Protocol. Provides STDIO, TCP (ranch), HTTP/2 (gun), WebSocket, and SSE transports with connection pooling, automatic reconnection, auto-discovery, and health monitoring.

## Overview

erlmcp_transports implements the transport layer with:
- **STDIO** - Standard input/output for command-line tools
- **TCP** - Production TCP server via ranch (40-50K connections/node)
- **HTTP** - HTTP/1.1 and HTTP/2 client via gun
- **WebSocket** - WebSocket transport for browser clients
- **SSE** - Server-Sent Events for streaming
- **Behavior Interface** - Extensible transport behavior for custom implementations
- **Connection Pooling** - poolboy integration for HTTP/TCP connection reuse
- **Auto-Discovery** - DNS-SD, Consul, Kubernetes, Environment variable discovery
- **Health Monitoring** - Automatic failover and health tracking

## Transport Modules (11 total)

### Transport Implementations
- **erlmcp_transport_stdio.erl** - Standard I/O (pipes) for CLI tools
- **erlmcp_transport_tcp.erl** - TCP server/client with ranch acceptor pools
- **erlmcp_transport_http.erl** - HTTP/2 client with gun, connection pooling
- **erlmcp_transport_ws.erl** - WebSocket transport for browser integration
- **erlmcp_transport_sse.erl** - Server-Sent Events transport
- **erlmcp_transport_http_server.erl** - HTTP server implementation

### Transport Infrastructure
- **erlmcp_transport_behavior.erl** - Transport behavior definition and API
- **erlmcp_transport_sup.erl** - Transport supervisor (simple_one_for_one)

### Auto-Discovery & Registry
- **erlmcp_transport_discovery.erl** - Automatic transport detection via DNS-SD, Consul, K8s, Environment
- **erlmcp_transport_registry.erl** - Health monitoring, failover, statistics tracking

## Auto-Discovery System

The transport discovery system automatically detects and configures transports from multiple sources.

### Supported Discovery Protocols

1. **Environment Variables** (Default)
2. **DNS-SD** (Bonjour/Avahi) - Local network discovery
3. **Consul** - Service mesh integration
4. **Kubernetes** - K8s service discovery

### Environment Variable Configuration

Configure transports via environment variables:

```bash
# TCP Transport
export ERLMCP_TRANSPORT_TCP1_TYPE=tcp
export ERLMCP_TRANSPORT_TCP1_HOST=localhost
export ERLMCP_TRANSPORT_TCP1_PORT=3000
export ERLMCP_TRANSPORT_TCP1_KEEPALIVE=true

# HTTP Transport
export ERLMCP_TRANSPORT_HTTP1_TYPE=http
export ERLMCP_TRANSPORT_HTTP1_HOST=api.example.com
export ERLMCP_TRANSPORT_HTTP1_PORT=443

# WebSocket Transport
export ERLMCP_TRANSPORT_WS1_TYPE=websocket
export ERLMCP_TRANSPORT_WS1_HOST=ws.example.com
export ERLMCP_TRANSPORT_WS1_PORT=8080
```

### Discovery API

```erlang
% Start discovery with config
{ok, _Pid} = erlmcp_transport_discovery:start_link(#{
    protocols => [env, dns_sd, consul],
    scan_interval => 30000,  % 30 seconds
    auto_start => true       % Auto-start discovered transports
}).

% Trigger immediate scan
erlmcp_transport_discovery:scan_now().

% Get discovered transports
Transports = erlmcp_transport_discovery:get_discovered_transports().

% Watch for changes
erlmcp_transport_discovery:watch(fun(Event) ->
    case Event of
        {transport_added, Id, Config} ->
            io:format("New transport: ~p~n", [Id]);
        {transport_removed, Id} ->
            io:format("Transport removed: ~p~n", [Id]);
        {transport_updated, Id, NewConfig} ->
            io:format("Transport updated: ~p~n", [Id])
    end
end).

% Enable/disable protocols
erlmcp_transport_discovery:enable_protocol(consul).
erlmcp_transport_discovery:disable_protocol(dns_sd).
```

## Health Monitoring & Registry

The transport registry tracks health status and provides automatic failover.

### Health Status Levels

- **up** - Transport is healthy and available
- **degraded** - Transport experiencing issues but operational
- **down** - Transport unavailable
- **unknown** - Status not yet determined

### Registry API

```erlang
% Register transport
erlmcp_transport_registry:register_transport(tcp1, Pid, #{
    type => tcp,
    host => "localhost",
    port => 3000
}).

% Get transport info
{ok, Info} = erlmcp_transport_registry:get_transport(tcp1).

% Get health status
{ok, Status} = erlmcp_transport_registry:get_transport_status(tcp1).

% Record operations
erlmcp_transport_registry:record_success(tcp1).
erlmcp_transport_registry:record_failure(tcp1, timeout).

% Get healthy transports
HealthyTransports = erlmcp_transport_registry:get_healthy_transports().

% Select best transport by type
{ok, BestTcp} = erlmcp_transport_registry:select_transport(tcp).

% Get statistics
{ok, Stats} = erlmcp_transport_registry:get_statistics(tcp1).
% Stats = #{
%     successes => 1000,
%     failures => 5,
%     messages_sent => 1005,
%     last_success => 1234567890
% }
```

### Automatic Failover

The registry automatically selects the best healthy transport based on:
- Health status (up > degraded > down)
- Success rate (calculated from statistics)
- Transport type matching

```erlang
% Select best HTTP transport (automatically picks healthiest)
{ok, TransportId} = erlmcp_transport_registry:select_transport(http).

% Use selected transport
{ok, Client} = erlmcp_client:start_link({http, TransportId}, Opts).
```

## Dependencies

| Library | Version | Purpose |
|---------|---------|---------|
| **gun** | 2.0.1 | HTTP/1.1, HTTP/2, WebSocket client |
| **ranch** | 2.1.0 | TCP acceptor pool and connection management |
| **poolboy** | 1.5.2 | Connection pooling for HTTP/TCP |
| **erlmcp_core** | 2.0.0 | Core MCP protocol |

## Transport Behavior

All transports implement the `erlmcp_transport_behavior` behavior:

```erlang
-callback init(Config :: map()) ->
    {ok, State} | {error, Reason}.

-callback send(State, Data :: binary()) ->
    ok | {error, Reason}.

-callback close(State) -> ok.

%% Optional callbacks
-callback get_info(State) ->
    #{type => atom(), status => atom(), peer => term()}.

-callback handle_transport_call(Request :: term(), State :: term()) ->
    {reply, Reply :: term(), NewState :: term()} |
    {error, Reason :: term()}.

-optional_callbacks([get_info/1, handle_transport_call/2]).
```

## Usage Examples

### STDIO Transport (Default)

```erlang
%% Client - communicate via stdin/stdout
{ok, Client} = erlmcp_client:start_link({stdio, []}, Opts),

%% Server - read from stdin, write to stdout
{ok, Server} = erlmcp_server:start_link({stdio, []}, ServerCaps).
```

**Use cases:** CLI tools, shell scripts, process pipelines

### TCP Transport (Ranch)

```erlang
%% TCP Server - listen on port 3000 (ranch acceptor pool)
{ok, Server} = erlmcp_server:start_link({tcp, #{
    port => 3000,
    num_acceptors => 10,
    max_connections => 50000
}}, ServerCaps),

%% TCP Client - connect to server
{ok, Client} = erlmcp_client:start_link({tcp, #{
    host => "localhost",
    port => 3000,
    connect_timeout => 5000,
    keepalive => true
}}, Opts).
```

**Use cases:** High-throughput server deployments, microservices, distributed systems

**Performance:** 40-50K concurrent connections per node (validated via benchmarks)

### HTTP Transport (Gun)

```erlang
%% HTTP Client with gun (HTTP/2 support)
{ok, Client} = erlmcp_client:start_link({http, #{
    url => <<"http://localhost:3001">>,
    protocol => http2,  % or http
    connect_timeout => 5000,
    request_timeout => 30000,
    pool_size => 10     % Connection pool
}}, Opts).
```

**Features:**
- Automatic HTTP/2 upgrade when supported
- Connection pooling via poolboy
- TLS/SSL support
- Keepalive and connection reuse

**Performance:** 5-10K concurrent connections (HTTP/2 multiplexing)

### WebSocket Transport

```erlang
%% WebSocket Client
{ok, Client} = erlmcp_client:start_link({ws, #{
    url => <<"ws://localhost:8080/mcp">>,
    protocols => [<<"mcp">>],
    connect_timeout => 5000
}}, Opts).
```

**Use cases:** Browser clients, real-time dashboards, web applications

## Adding a Custom Transport

Implement the `erlmcp_transport_behavior` behavior:

```erlang
-module(erlmcp_transport_custom).
-behaviour(erlmcp_transport_behavior).
-behaviour(gen_server).

-export([init/1, send/2, close/1, get_info/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% Transport behavior callbacks
init(Config) ->
    %% Initialize transport state
    State = #{id => maps:get(transport_id, Config), config => Config},
    {ok, State}.

send(State, Data) ->
    %% Send data to peer
    ok.

close(State) ->
    %% Cleanup resources
    ok.

get_info(State) ->
    #{type => custom, status => connected}.

% gen_server callbacks
init([TransportId, Config]) ->
    {ok, State} = init(Config),
    % Register with registry
    erlmcp_transport_registry:register_transport(TransportId, self(), Config),
    {ok, State}.

handle_call({send, Data}, _From, State) ->
    Result = send(State, Data),
    {reply, Result, State}.

% ... other callbacks
```

Register in erlmcp_core:

```erlang
{ok, Client} = erlmcp_client:start_link({custom, #{}}, Opts).
```

## Configuration

Default transport config in `erlmcp_transports.app.src`:

```erlang
{transport_defaults, #{
    tcp => #{
        connect_timeout => 5000,
        keepalive => true,
        nodelay => true,
        port => 3000
    },
    http => #{
        connect_timeout => 5000,
        request_timeout => 30000,
        max_connections => 100,
        port => 3001
    },
    stdio => #{
        buffer_size => 65536,
        read_timeout => infinity
    }
}}
```

Override in `sys.config`:

```erlang
[
    {erlmcp_transports, [
        {transport_defaults, #{
            tcp => #{port => 8000},
            http => #{max_connections => 200}
        }},
        {discovery, #{
            enabled => true,
            protocols => [env, consul],
            scan_interval => 30000,
            auto_start => true
        }},
        {registry, #{
            health_check_interval => 30000,
            failure_threshold => 5,
            degraded_threshold => 3
        }}
    ]}
].
```

## Build & Test

```bash
# Compile
rebar3 compile --app erlmcp_transports

# Unit tests
rebar3 eunit --app erlmcp_transports

# Specific test modules
rebar3 eunit --module=erlmcp_transport_discovery_tests
rebar3 eunit --module=erlmcp_transport_registry_tests

# Integration tests (requires network)
rebar3 ct --app erlmcp_transports

# Type checking
rebar3 dialyzer --app erlmcp_transports
```

## Performance Characteristics

**Benchmarked (v1.5.0):**

| Transport | Throughput | Latency (p50) | Max Connections | Notes |
|-----------|------------|---------------|-----------------|-------|
| **TCP** | 43K msg/s | 11 μs | 40-50K/node | ranch acceptor pool |
| **HTTP** | 12K req/s | 87 μs | 5-10K/node | HTTP/2 multiplexing |
| **STDIO** | 5K msg/s | 203 μs | 1 (single process) | CLI tools only |
| **WebSocket** | 8K msg/s | 125 μs | 10K/node | Browser clients |

**Bottlenecks:**
- STDIO: Single-threaded by design
- HTTP: TLS handshake overhead (use connection pooling)
- TCP: File descriptor limits (increase with `ulimit -n`)

## Migration from v0.5.x

v2.0.0 moves transports into separate app. Update your code:

```erlang
% Before (v0.5.x - flat structure)
{ok, Client} = erlmcp_client:start_link(stdio, []).

% After (v2.0.0 - tuple transport config)
{ok, Client} = erlmcp_client:start_link({stdio, []}, Opts).
```

Transport modules remain in same namespace - no code changes beyond config format.



### Features

- **Query Support** - List and read tools, resources, and prompts
- **Subscription Support** - Real-time resource updates via WebSocket
- **Query Batching** - Multiple operations in single request
- **DataLoader** - N+1 query prevention
- **Query Complexity Limits** - Prevent expensive queries
- **Persisted Queries** - Performance optimization



type Tool {
  name: String!
  description: String
  inputSchema: JSON
}

type Resource {
  uri: String!
  name: String!
  mimeType: String
  description: String
}

type Prompt {
  name: String!
  description: String
  arguments: [PromptArgument!]
}

type PromptArgument {
  name: String!
  description: String
  required: Boolean
}

type Content {
  type: String!
  text: String
  data: String
  mimeType: String
}

type ToolResult {
  content: [Content!]!
  isError: Boolean
}

type Query {
  tools: [Tool!]!
  tool(name: String!): Tool
  resources: [Resource!]!
  resource(uri: String!): Resource
  prompts: [Prompt!]!
  prompt(name: String!): Prompt
}

type Mutation {
  callTool(name: String!, arguments: JSON): ToolResult
}

type Subscription {
  resourceUpdated(uri: String): Resource
}

scalar JSON
```

### Usage Examples


```erlang
Config = #{
    server_id => my_mcp_server,
    server_pid => ServerPid,
    port => 4000,
    enable_introspection => true,
    enable_subscriptions => true,
    max_query_depth => 10,
    max_query_complexity => 1000,
    enable_batching => true,
    cors_enabled => true
},

```


**List all tools:**
query {
  tools {
    name
    description
    inputSchema
  }
}
```

**Get specific tool:**
query {
  tool(name: "echo") {
    name
    description
    inputSchema
  }
}
```

**List resources:**
query {
  resources {
    uri
    name
    mimeType
  }
}
```

**Read resource:**
query {
  resource(uri: "file:///path/to/file.txt") {
    uri
    name
    mimeType
  }
}
```


**Call a tool:**
mutation {
    content {
      type
      text
    }
    isError
  }
}
```

**Call tool with complex arguments:**
mutation {
  callTool(
    name: "calculate",
    arguments: {a: 5, b: 3, operation: "add"}
  ) {
    content {
      type
      text
    }
  }
}
```


**Subscribe to resource updates:**
subscription {
  resourceUpdated(uri: "file:///watched/file.txt") {
    uri
    name
    mimeType
  }
}
```

**Subscribe to all resources:**
subscription {
  resourceUpdated {
    uri
    name
  }
}
```

### HTTP API

```bash
  -H "Content-Type: application/json" \
  -d '{
    "query": "{ tools { name description } }",
    "variables": {}
  }'
```

```bash
```

```javascript

ws.send(JSON.stringify({
  type: 'start',
  payload: {
    query: 'subscription { resourceUpdated { uri } }',
    variables: {}
  }
}));
```

### Configuration

```erlang
%% In sys.config
{erlmcp_transports, [
        port => 4000,
        enable_introspection => true,
        enable_subscriptions => true,
        max_query_depth => 10,
        max_query_complexity => 1000,
        enable_batching => true,
        enable_persisted_queries => false,
        cors_enabled => true,
        cors_origins => [<<"*">>]
    }}
]}
```

### Testing

```bash

# Test schema generation

# Test resolvers
```

### Performance

- **Query Throughput**: ~15K queries/sec (simple queries)
- **Mutation Throughput**: ~10K mutations/sec
- **Subscription Capacity**: ~5K concurrent subscriptions/node
- **Latency**: p50: 5ms, p95: 15ms, p99: 50ms

**Optimization:**
- Use query batching for multiple operations
- Enable persisted queries for production
- Set appropriate complexity limits
- Use DataLoader for N+1 prevention

## See Also

- [erlmcp_core](../erlmcp_core/README.md) - Core MCP protocol
- [erlmcp_observability](../erlmcp_observability/README.md) - Metrics for transports
- [Transport Configuration Guide](../../docs/transport_configuration.md) - Detailed config examples
- [Architecture](../../docs/architecture.md) - System design
