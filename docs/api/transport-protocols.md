# erlmcp Transport Protocols

## Overview

erlmcp supports multiple transport protocols for client-server communication. Each transport has different characteristics and use cases.

| Transport | Use Case | Pros | Cons |
|-----------|----------|------|------|
| **stdio** | Local MCP servers (Claude Desktop) | Simple, secure, fast | Single connection only |
| **TCP** | High-performance custom clients | Low overhead, efficient | No built-in security |
| **HTTP** | Web applications, REST APIs | Firewall-friendly, easy debugging | Higher latency |
| **WebSocket** | Real-time bidirectional | Low latency, efficient | Requires WS support |
| **SSE** | Server-to-client streaming | Simple, firewall-friendly | One-way only |

---

## stdio Transport

The stdio (standard input/output) transport is the simplest and most commonly used transport for local MCP servers. It communicates via stdin/stdout pipes.

### Configuration

No network configuration needed. The server reads from stdin and writes to stdout.

### Erlang API

```erlang
%% Start stdio server
{ok, Server} = erlmcp_server:start_link(my_server, Capabilities).
{ok, Transport} = erlmcp_transport_stdio:start_link(Server).

%% Or using the client
{ok, Client} = erlmcp_client:start_link({stdio, []}).
```

### Message Format

Each JSON-RPC message is sent on a single line (newline-delimited):

```
{"jsonrpc":"2.0","id":1,"method":"initialize","params":{...}}
{"jsonrpc":"2.0","id":2,"method":"tools/list"}
```

### Claude Desktop Configuration

```json
{
  "mcpServers": {
    "erlmcp-example": {
      "command": "erl",
      "args": [
        "-pa", "/path/to/erlmcp/_build/default/lib/*/ebin",
        "-eval", "my_server:start()",
        "-noshell"
      ]
    }
  }
}
```

### Best Practices

1. **Flush stdout**: Ensure messages are immediately flushed
2. **Line buffering**: Use newline-delimited JSON
3. **Error handling**: Write errors to stderr, not stdout
4. **Graceful shutdown**: Handle SIGTERM/SIGINT

### Example Implementation

```erlang
-module(my_stdio_server).
-export([start/0]).

start() ->
    %% Start application
    application:ensure_all_started(erlmcp_core),

    %% Start server
    {ok, Server} = erlmcp_server:start_link(my_server, #{
        capabilities => #mcp_server_capabilities{
            resources = #mcp_resources_capability{},
            tools = #mcp_tools_capability{},
            prompts = #mcp_prompts_capability{}
        }
    }),

    %% Add tools
    erlmcp_server:add_tool(Server, <<"echo">>, fun(Args) ->
        Text = maps:get(<<"text">>, Args, <<"">>),
        #{content => [{type => text, text => Text}]}
    end),

    %% Start stdio transport
    {ok, _Transport} = erlmcp_transport_stdio:start_link(Server),

    %% Keep process alive
    receive
        stop -> ok
    end.
```

---

## TCP Transport

The TCP transport provides raw socket communication for high-performance scenarios.

### Configuration

```erlang
%% Server
{ok, Server} = erlmcp_server:start_link(tcp_server, Capabilities).

{ok, Listener} = erlmcp_transport_tcp:start_listener([
    {port, 8765},
    {ip, {0, 0, 0, 0}},
    {active, true}
]).

%% Client
{ok, Client} = erlmcp_client:start_link({tcp, #{
    host => "localhost",
    port => 8765
}}).
```

### Options

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `port` | integer | 8765 | TCP port |
| `ip` | tuple | {0,0,0,0} | Bind address |
| `backlog` | integer | 128 | Connection backlog |
| `keepalive` | boolean | true | TCP keepalive |
| `nodelay` | boolean | true | Disable Nagle's algorithm |
| `send_timeout` | integer | 5000 | Send timeout (ms) |
| `recv_timeout` | integer | 5000 | Receive timeout (ms) |

### Message Framing

TCP uses 4-byte length prefix framing:

```
[Length (4 bytes, big-endian)][JSON-RPC Message]
```

Example:
```
00 00 00 7B {"jsonrpc":"2.0","id":1,...}
```

### Example Implementation

```erlang
%% Start TCP server
start_tcp_server() ->
    {ok, Server} = erlmcp_server:start_link(tcp_server, Capabilities),

    %% Start TCP listener
    {ok, Listener} = ranch:start_listener(
        tcp_mcp,
        ranch_tcp,
        #{socket_opts => [{port, 8765}]},
        erlmcp_tcp_protocol,
        #{server => Server}
    ),

    {ok, Listener, Server}.
```

---

## HTTP Transport

The HTTP transport provides REST endpoints compatible with web applications.

### Configuration

```erlang
%% Server
{ok, Server} = erlmcp_server:start_link(http_server, Capabilities).

{ok, _HTTP} = erlmcp_transport_http:start_link(Server, [
    {port, 8765},
    {ip, {0, 0, 0, 0}},
    {compress, true},
    {timeout, 30000}
]).
```

### Options

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `port` | integer | 8765 | HTTP port |
| `ip` | tuple | {0,0,0,0} | Bind address |
| `compress` | boolean | true | Enable gzip compression |
| `timeout` | integer | 30000 | Request timeout (ms) |
| `max_body_size` | integer | 16777216 | Max body size (16MB) |
| `cors_origins` | list | [] | CORS allowed origins |
| `enable_tls` | boolean | false | Enable HTTPS |
| `certfile` | string | - | TLS certificate file |
| `keyfile` | string | - | TLS private key file |

### HTTP Endpoints

| Endpoint | Method | Description |
|----------|--------|-------------|
| `/mcp` | POST | JSON-RPC endpoint |
| `/mcp/stream` | GET | SSE streaming endpoint |
| `/health` | GET | Health check |
| `/metrics` | GET | Prometheus metrics |

### cURL Examples

```bash
# Initialize
curl -X POST http://localhost:8765/mcp \
  -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "id": 1,
    "method": "initialize",
    "params": {
      "protocolVersion": "2025-11-25",
      "capabilities": {},
      "clientInfo": {"name": "curl", "version": "1.0"}
    }
  }'

# List tools
curl -X POST http://localhost:8765/mcp \
  -H "Content-Type: application/json" \
  -d '{"jsonrpc":"2.0","id":2,"method":"tools/list"}'

# Call tool
curl -X POST http://localhost:8765/mcp \
  -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "id": 3,
    "method": "tools/call",
    "params": {
      "name": "calculate",
      "arguments": {"a": 10, "b": 5, "op": "add"}
    }
  }'
```

### HTTPS Configuration

```erlang
start_https_server() ->
    {ok, Server} = erlmcp_server:start_link(https_server, Capabilities),

    {ok, _HTTP} = erlmcp_transport_http:start_link(Server, [
        {port, 8765},
        {enable_tls, true},
        {certfile, "/path/to/cert.pem"},
        {keyfile, "/path/to/key.pem"},
        {verify, verify_peer},
        {fail_if_no_peer_cert, true},
        {honor_cipher_order, true},
        {secure_renegotiate, true}
    ]).
```

---

## WebSocket Transport

WebSocket provides real-time bidirectional communication with low overhead.

### Configuration

```erlang
%% Server
{ok, Server} = erlmcp_server:start_link(ws_server, Capabilities).

{ok, _WS} = erlmcp_transport_ws:start_link(Server, [
    {port, 8765},
    {path, "/mcp"},
    {compress, true},
    {idle_timeout, 60000}
]).
```

### Options

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `port` | integer | 8765 | WebSocket port |
| `path` | string | "/mcp" | WebSocket path |
| `compress` | boolean | true | Enable compression |
| `idle_timeout` | integer | 60000 | Idle timeout (ms) |
| `max_frame_size` | integer | 16777216 | Max frame size (16MB) |
| `origin_check` | boolean | true | Check Origin header |

### JavaScript Client Example

```javascript
// Connect to WebSocket MCP server
const ws = new WebSocket('ws://localhost:8765/mcp');
let messageId = 0;

ws.onopen = () => {
  console.log('Connected to MCP server');

  // Initialize
  send({
    jsonrpc: '2.0',
    id: ++messageId,
    method: 'initialize',
    params: {
      protocolVersion: '2025-11-25',
      capabilities: {},
      clientInfo: { name: 'browser-client', version: '1.0.0' }
    }
  });
};

ws.onmessage = (event) => {
  const response = JSON.parse(event.data);
  console.log('Received:', response);

  if (response.result && !response.result.capabilities) {
    // After initialized, list tools
    send({
      jsonrpc: '2.0',
      id: ++messageId,
      method: 'tools/list'
    });
  }
};

function send(message) {
  ws.send(JSON.stringify(message));
}
```

### Subprotocol Support

erlmcp WebSocket uses the `mcp` subprotocol:

```javascript
const ws = new WebSocket('ws://localhost:8765/mcp', ['mcp']);
```

---

## SSE (Server-Sent Events) Transport

SSE provides server-to-client streaming for notifications.

### Configuration

```erlang
{ok, Server} = erlmcp_server:start_link(sse_server, Capabilities).

{ok, _SSE} = erlmcp_transport_sse:start_link(Server, [
    {port, 8765},
    {path, "/mcp/events"},
    {retry, 3000},
    {heartbeat, 30000}
]).
```

### Options

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `port` | integer | 8765 | HTTP port |
| `path` | string | "/mcp/events" | SSE endpoint path |
| `retry` | integer | 3000 | Retry interval (ms) |
| `heartbeat` | integer | 30000 | Heartbeat interval (ms) |
| `cors_origins` | list | ["*"] | CORS allowed origins |

### SSE Event Format

```
event: message
data: {"jsonrpc":"2.0","method":"notifications/progress","params":{...}}

event: resources/updated
data: {"uri":"mcp://config"}

: heartbeat
```

### JavaScript Client Example

```javascript
const eventSource = new EventSource('http://localhost:8765/mcp/events');

eventSource.addEventListener('message', (event) => {
  const data = JSON.parse(event.data);
  console.log('Received:', data);
});

eventSource.addEventListener('resources/updated', (event) => {
  const data = JSON.parse(event.data);
  console.log('Resource updated:', data.uri);
});

eventSource.onerror = (error) => {
  console.error('SSE error:', error);
};
```

---

## Transport Selection Guide

### Choose stdio when:
- Running locally with Claude Desktop
- Simple setup is preferred
- Only one client connection needed

### Choose TCP when:
- Building custom high-performance clients
- Need minimal protocol overhead
- Implementing custom protocol extensions

### Choose HTTP when:
- Integrating with web applications
- Need firewall-friendly communication
- Using standard HTTP tools (curl, Postman)

### Choose WebSocket when:
- Need real-time bidirectional communication
- Building browser-based clients
- Require low-latency notifications

### Choose SSE when:
- Need server-to-client streaming only
- Want simpler alternative to WebSocket
- Browser-based notification receiver

---

## Security Considerations

### Authentication

```erlang
%% Enable JWT authentication for HTTP/WebSocket
{ok, _HTTP} = erlmcp_transport_http:start_link(Server, [
    {auth, {
        method, jwt,
        secret, <<"your-secret-key">>,
        algorithm, hs256
    }}
]).
```

### TLS/mTLS

```erlang
%% Enable mutual TLS
{ok, _HTTP} = erlmcp_transport_http:start_link(Server, [
    {enable_tls, true},
    {certfile, "/path/to/server.crt"},
    {keyfile, "/path/to/server.key"},
    {cacertfile, "/path/to/ca.crt"},
    {verify, verify_peer},
    {fail_if_no_peer_cert, true}
]).
```

### Rate Limiting

```erlang
%% Configure rate limiting
{ok, _HTTP} = erlmcp_transport_http:start_link(Server, [
    {rate_limit, {
        max_requests, 100,
        window_ms, 60000
    }}
]).
```

---

## Performance Benchmarks

| Transport | Throughput | Latency (p50) | Latency (p99) |
|-----------|------------|---------------|---------------|
| stdio | 50K msg/s | 0.1ms | 0.5ms |
| TCP | 100K msg/s | 0.05ms | 0.3ms |
| HTTP | 10K req/s | 1ms | 5ms |
| WebSocket | 80K msg/s | 0.1ms | 1ms |
| SSE | 20K evt/s | 0.5ms | 2ms |

*Measured on Erlang/OTP 28, local loopback*
