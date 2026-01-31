# MCP Transport Behavior Contracts - Complete Specification

**Version**: MCP 2025-11-25
**Status**: Authoritative Reference
**Module**: `erlmcp_transport_contracts.erl`

## Overview

This document provides complete behavior contract specifications for all MCP transport types based on the official MCP 2025-11-25 specification and erlmcp implementation patterns.

## Transport Types

MCP 2025-11-25 defines 4 primary transport types:

| Transport | Use Case | Multiplexing | Backpressure |
|-----------|----------|--------------|--------------|
| **STDIO** | Local process communication | No (single stream) | Queue-based (limited) |
| **TCP** | Direct network connections | Yes (multi-connection) | TCP window-based |
| **HTTP/SSE** | Scalable web transport | Yes (concurrent requests) | Rate limiting + buffer |
| **WebSocket** | Real-time bidirectional | Yes (concurrent messages) | Frame buffer monitoring |

---

## Universal Transport Requirements

**ALL transports MUST:**

1. Implement `-behaviour(erlmcp_transport_behavior)` callbacks:
   - `init/1` - Initialize transport
   - `send/2` - Send message through transport
   - `close/1` - Clean shutdown and resource cleanup
   - `get_info/1` (optional) - Introspection and statistics
   - `handle_transport_call/2` (optional) - Transport-specific operations

2. Support **JSON-RPC 2.0 message format**:
   - Request messages (with `id`)
   - Response messages (success or error)
   - Notification messages (no `id`)

3. Enforce **UTF-8 encoding** for all message content

4. Implement **16MB default message size limit** (configurable)

5. Handle **connection lifecycle**:
   - `NOT_INITIALIZED` → `INITIALIZING` → `INITIALIZED` → `DISCONNECTED`

6. Provide **backpressure mechanisms** appropriate to transport type

7. Support **graceful shutdown** with resource cleanup guarantees

8. **Register with `erlmcp_registry`** on successful initialization

9. **Route incoming messages** via registry for centralized handling

---

## 1. STDIO Transport Contract

### Purpose
Local process communication via stdin/stdout pipes

### Use Cases
- CLI tools and command-line MCP servers
- Local process spawning (e.g., spawn external MCP server)
- Development and testing

### Message Framing
**Line-delimited JSON**
- Each message is a complete JSON object on a single line
- Messages terminated by newline (`\n`, `\r\n`, or `\r`)
- Empty lines are ignored

### Encoding
UTF-8 text only

### Connection Semantics
- **Established**: When process starts
- **EOF on stdin**: Signals disconnection
- **Process termination**: Closes connection
- **No explicit protocol**: No connect/disconnect handshake

### Concurrency
**Single stream** (sequential message processing)
- Messages processed in order received
- No parallel request handling within single connection
- Suitable for basic capabilities (tools, resources, prompts)

### Backpressure
**Queue-based (limited)**
- Blocks on write if stdout buffer full
- Reader process monitors stdin availability
- No explicit flow control protocol
- Relies on OS pipe buffering
- **Buffer size**: Configurable (default 64KB)

### Configuration

```erlang
Config = #{
    transport_id => my_stdio_transport,
    owner => self(),                    % Process receiving messages
    max_message_size => 16777216,       % 16MB default
    test_mode => false                  % Set true for testing
}
```

### Message Flow

```
stdin → read_loop → validate_size → trim_line → {transport_message, Binary}
{send, Binary} → stdout
EOF → {transport_disconnected, Reason}
```

### Error Handling

| Error | Action | Recovery |
|-------|--------|----------|
| Message size exceeded | Send JSON-RPC -32012, continue | Skip message |
| Invalid UTF-8 | Log warning, skip message | Continue reading |
| EOF | Graceful shutdown | Terminate transport |
| Read error | Log error, stop reader | Terminate reader process |

### State Management

```erlang
State = #{
    owner => pid(),                     % Receiving process
    owner_monitor => reference(),       % Monitor owner termination
    reader => pid(),                    % Background stdin reader
    buffer => binary(),                 % Incomplete line buffer
    test_mode => boolean(),            % Test environment flag
    max_message_size => pos_integer(), % Size limit
    transport_id => atom()             % Registry identifier
}
```

### Implementation Requirements

- ✅ Spawn reader process on init (unless test_mode)
- ✅ Monitor owner process for termination
- ✅ Register with `erlmcp_registry` using transport_id
- ✅ Validate message size BEFORE processing
- ✅ Send messages to owner as `{transport_message, Binary}`
- ✅ Unregister from registry on termination

---

## 2. TCP Transport Contract

### Purpose
Direct network connections with multiplexing and high performance

### Use Cases
- High-performance server-to-server communication
- Internal microservices (bypassing HTTP overhead)
- Connection pooling for load distribution
- Direct client-server connections

### Message Framing
**Line-delimited** (compatible with STDIO for simplicity)
- Each message terminated by `\n`
- Full JSON object per message
- Alternative: Length-prefixed (not implemented)

### Encoding
UTF-8 binary

### Modes

#### Server Mode (Ranch-based acceptor pool)
- Configurable number of acceptors (default: 10)
- Connection limiting (default: 1024 max connections)
- One gen_server per accepted connection
- Supervised connection handlers

#### Client Mode (Outbound connections)
- Automatic reconnection with exponential backoff
- Configurable retry attempts (default: infinity)
- Connection timeout (default: 5 seconds)

### Connection Semantics

| Event | Server | Client |
|-------|--------|--------|
| **Start** | `bind(host, port)` | `connect(host, port)` |
| **Connected** | After accept | `{transport_connected, Pid}` |
| **Disconnected** | `{tcp_closed}` → stop | `{tcp_closed}` → reconnect |
| **Error** | `{tcp_error}` → stop | `{tcp_error}` → reconnect |

### Multiplexing
**YES**
- Multiple concurrent connections per server
- Each connection is independent gen_server
- Connection pooling support via `erlmcp_pool_manager`

### Backpressure
**TCP window-based** (OS-level flow control)
- `gen_tcp:send` blocks when send buffer full
- `{active, true}` mode with monitoring
- Configurable buffer sizes (`recbuf`, `sndbuf`)
- **Default buffer**: 64KB (configurable)

### Flow Control

| Mechanism | Purpose | Default |
|-----------|---------|---------|
| **Send buffer** | Outbound data queue | 64KB |
| **Receive buffer** | Inbound data queue | 64KB |
| **Message size** | Reject oversized messages | 16MB |
| **Memory guard** | System memory protection | Enabled |

### Configuration (Server)

```erlang
Config = #{
    mode => server,
    transport_id => my_tcp_server,
    server_id => server_1,
    port => 8888,
    owner => self(),
    num_acceptors => 10,
    max_connections => 1024,
    keepalive => true,
    nodelay => true,                    % Disable Nagle algorithm
    buffer_size => 65536
}
```

### Configuration (Client)

```erlang
Config = #{
    mode => client,
    transport_id => my_tcp_client,
    host => "localhost",
    port => 8888,
    owner => self(),
    connect_timeout => 5000,
    max_reconnect_attempts => infinity,
    keepalive => true,
    nodelay => true
}
```

### Message Flow (Server)

```
ranch:accept → handshake → {active, true}
→ {tcp, Socket, Data}
→ validate_size → extract_messages
→ {transport_message, Binary}
→ route to registry
{tcp_closed, Socket} → {stop, normal}
```

### Message Flow (Client)

```
gen_tcp:connect → {active, true}
→ {transport_connected, Pid}
→ {tcp, Socket, Data}
→ {transport_message, Binary}
{tcp_closed, Socket} → reconnect_timer → retry_connection
```

### Error Handling

| Error | Code | Action | Recovery |
|-------|------|--------|----------|
| Message too large | -32012 | Send error, close | Client reconnects |
| Memory exhausted | -32603 | Send error, close | System recovery |
| Connection limit | 1001 | Reject connection | Client retries |
| TCP error | N/A | Log error | Reconnect (client) / Stop (server) |

### Connection Management

| Feature | Value | Purpose |
|---------|-------|---------|
| **Idle timeout** | 5 minutes | Close inactive connections |
| **Resource monitoring** | 60 seconds | Check memory usage |
| **Connection lease** | 30 seconds | Timeout for handler init |
| **Cleanup guarantee** | ALL paths | Release connection slot |

### State Management

```erlang
State = #{
    mode => client | server,
    socket => gen_tcp:socket() | undefined,
    connected => boolean(),
    buffer => binary(),                      % Incomplete messages
    reconnect_timer => reference(),          % Client only
    reconnect_attempts => non_neg_integer(), % Client only
    bytes_sent => non_neg_integer(),        % Statistics
    bytes_received => non_neg_integer(),    % Statistics
    idle_timer => reference(),              % Idle timeout
    resource_monitor_timer => reference(),  % Resource checks
    last_activity => integer(),             % Monotonic timestamp
    max_message_size => pos_integer(),
    initialized => boolean()                % Server: successful init
}
```

### Implementation Requirements

- ✅ Ranch-based server with acceptor pool
- ✅ Client with automatic reconnection and exponential backoff
- ✅ Connection slot management (accept/release)
- ✅ Message size validation BEFORE processing
- ✅ Memory guard integration
- ✅ Idle timeout and resource monitoring
- ✅ Graceful shutdown with guaranteed cleanup

---

## 3. HTTP/SSE Transport Contract

### Purpose
Scalable web transport with bidirectional communication

### Use Cases
- Web applications accessing MCP servers
- Firewall-friendly deployments
- Load-balanced server architectures
- Browser-based clients (via fetch + EventSource)

### Bidirectional Communication

| Direction | Mechanism | Description |
|-----------|-----------|-------------|
| **Client → Server** | HTTP POST | JSON request body |
| **Server → Client** | SSE (Server-Sent Events) | Event stream |

### Message Framing

#### HTTP POST
- Complete JSON in request body
- Content-Type: `application/json`
- Each request is independent (stateless)

#### SSE (Server-Sent Events)
- Format: `data: {JSON}\n\n` per event
- Content-Type: `text/event-stream`
- Long-lived connection for push notifications

### Encoding
UTF-8, Content-Type: `application/json`

### Connection Semantics

| Aspect | HTTP | SSE |
|--------|------|-----|
| **Connection** | Stateless | Long-lived |
| **Request** | Each independent | N/A |
| **Response** | Immediate | Continuous stream |
| **Session** | Via header/query | Event-ID based |

### Multiplexing
**YES**
- Concurrent HTTP requests supported
- Multiple SSE connections per client
- Connection pooling for HTTP client requests

### Backpressure

| Transport | Mechanism | Description |
|-----------|-----------|-------------|
| **HTTP** | Synchronous | Natural backpressure via request/response |
| **SSE** | Event buffer | Size-limited queue, reconnect on overflow |

### Flow Control

| Mechanism | Component | Purpose |
|-----------|-----------|---------|
| **Rate limiting** | `erlmcp_rate_limiter` | Prevent request floods |
| **Event queue** | SSE manager | Monitor depth |
| **Circuit breaker** | `erlmcp_circuit_breaker` | Overload protection |

### Configuration (HTTP Client)

```erlang
Config = #{
    transport_id => my_http_client,
    url => <<"https://mcp-server.example.com/mcp">>,
    owner => self(),
    method => post,
    headers => [{<<"Authorization">>, <<"Bearer token">>}],
    timeout => 30000,
    connect_timeout => 5000,
    max_retries => 3,
    retry_delay => 1000,
    ssl_options => [{verify, verify_peer}],
    pool_size => 10
}
```

### Configuration (SSE Server)

```erlang
Config = #{
    transport_id => my_sse_server,
    port => 8080,
    path => "/mcp/events",
    owner => self(),
    max_connections => 10000,
    event_buffer_size => 1000,
    keepalive_interval => 30000,
    reconnect_timeout => 3000
}
```

### Message Flow (HTTP POST)

```
Client → HTTP POST /mcp
→ Parse JSON body
→ Route to handler
→ Generate response
→ HTTP 200 with JSON body
```

### Message Flow (SSE)

```
Client → HTTP GET /mcp/events
  Header: Accept: text/event-stream
Server → HTTP 200
  Header: Content-Type: text/event-stream
Server → data: {JSON}\n\n (repeating)
Connection closed → Client reconnects with Last-Event-ID
```

### Error Handling

| Status | Category | Meaning | Recovery |
|--------|----------|---------|----------|
| **4xx** | Client error | Malformed request | Fix and retry |
| **5xx** | Server error | Processing failure | Retry with backoff |
| **Timeout** | Network | Request timeout | Retry |
| **SSE disconnect** | Network | Stream interrupted | Auto-reconnect |

### Headers

| Header | Direction | Purpose | Example |
|--------|-----------|---------|---------|
| `Content-Type` | Request | JSON format | `application/json` |
| `Accept` | Request (SSE) | Event stream | `text/event-stream` |
| `Authorization` | Request | Auth token | `Bearer <token>` |
| `X-Request-ID` | Both | Correlation | UUID |
| `Last-Event-ID` | SSE reconnect | Resume stream | Event ID |

### State Management

```erlang
% HTTP Client State
State = #{
    transport_id => atom(),
    gun_pid => pid() | undefined,           % HTTP/2 connection
    stream_ref => reference(),              % Request tracking
    url => binary(),
    method => get | post,
    headers => [{binary(), binary()}],
    pending_requests => #{term() => term()}, % Request ID → callback
    max_message_size => pos_integer()
}

% SSE Server State
State = #{
    transport_id => atom(),
    cowboy_pid => pid() | undefined,
    sse_connections => #{binary() => pid()}, % Session → stream
    event_buffer_size => pos_integer(),
    keepalive_interval => pos_integer()
}
```

### Implementation Requirements

- ✅ HTTP POST for client→server requests
- ✅ SSE stream for server→client notifications
- ✅ Session tracking via headers or query params
- ✅ Rate limiting integration
- ✅ Circuit breaker for overload protection
- ✅ Automatic SSE reconnection with Last-Event-ID

---

## 4. WebSocket Transport Contract

### Purpose
Real-time bidirectional web transport with low latency

### Use Cases
- Real-time interactive applications
- Low-latency command/control
- Browser-based MCP clients
- Streaming data processing

### Subprotocol
**`mcp.v1`**
- Negotiated during WebSocket handshake
- Header: `Sec-WebSocket-Protocol: mcp.v1`

### Message Framing

| Aspect | Specification |
|--------|---------------|
| **Frame Type** | Text frames only (not binary) |
| **Content** | Line-delimited JSON messages |
| **Delimiter** | `\n` (newline) |
| **Fragmentation** | Automatic reassembly |

### Encoding
UTF-8 text

### Connection Semantics

| Phase | Description |
|-------|-------------|
| **Upgrade** | HTTP → WebSocket via upgrade handshake |
| **Path** | Configurable (e.g., `/mcp/ws`) |
| **Established** | After successful handshake |
| **Keepalive** | Ping/pong frames every 30 seconds |
| **Idle timeout** | 5 minutes (configurable) |

### Concurrency
**Single logical stream per connection**
- Messages processed sequentially within connection
- Multiple messages can be in-flight concurrently
- Server handles multiple WebSocket connections

### Backpressure

| Metric | Default | Threshold |
|--------|---------|-----------|
| **Frame buffer size** | 100KB | Hard limit |
| **Backpressure activation** | At limit | Pause reading |
| **Drain threshold** | 50% | Resume reading |
| **Timeout** | 5 seconds | Error if not resolved |

### Flow Control

```erlang
State = #{
    frame_buffer_size => 102400,        % Max buffered data
    frame_buffer_used => 0,             % Current usage
    backpressure_state => inactive,     % inactive | active
    messages_pending => 0,              % Queued messages
    bytes_buffered => 0                 % Total buffered
}
```

### WebSocket Close Codes (RFC 6455)

| Code | Name | Reason |
|------|------|--------|
| **1000** | Normal closure | Clean disconnect |
| **1001** | Going away | Endpoint leaving |
| **1002** | Protocol error | Invalid message |
| **1003** | Unsupported data | Binary frames received |
| **1009** | Message too big | Exceeds 16MB limit |
| **1011** | Internal error | Server failure |

### Configuration

```erlang
Config = #{
    transport_id => my_ws_transport,
    port => 8080,
    path => "/mcp/ws",
    max_message_size => 16777216,       % 16MB
    strict_delimiter_check => true,     % Enforce \n delimiter
    validate_utf8 => true,              % Validate encoding
    max_connections => 1000,
    connect_timeout => 5000,
    frame_buffer_size => 102400         % 100KB
}
```

### Message Flow

```
HTTP Upgrade → WebSocket Handshake
→ websocket_init (create state)
→ Start ping timer (30s interval)
→ {text, Data} frames received
→ Validate UTF-8, size, backpressure
→ Extract messages (line-delimited)
→ Route to registry
→ {send_frame, Data} for outbound
→ Close frame → terminate
```

### Ping/Pong Protocol

| Direction | Frame | Purpose | Interval |
|-----------|-------|---------|----------|
| **Server → Client** | Ping | Keepalive check | 30 seconds |
| **Client → Server** | Pong | Acknowledge alive | On ping |
| **Client → Server** | Ping | Unsolicited (allowed) | Any time |
| **Server → Client** | Pong | Respond to client ping | On ping |

### Fragment Handling

| Aspect | Specification |
|--------|---------------|
| **Reassembly** | Automatic continuation frame handling |
| **Timeout** | 30 seconds for complete fragment |
| **Buffer** | Temporary storage for incomplete frames |
| **Cleanup** | On timeout or error |

### Error Handling

| Error | Close Code | Action | Reason |
|-------|------------|--------|--------|
| Binary frame | 1003 | Close connection | Unsupported |
| Message too big | 1009 | Close connection | > 16MB |
| Invalid UTF-8 | 1002 | Close connection | Protocol error |
| Backpressure timeout | 1011 | Close connection | Internal error |
| Fragment timeout | 1002 | Close connection | Protocol error |

### State Management

```erlang
State = #{
    transport_id => binary(),
    registry_pid => pid(),              % Message routing
    session_id => binary(),             % Unique session
    ping_timer => reference(),          % Periodic ping
    fragment_buffer => binary(),        % Incomplete frames
    fragment_start_time => integer(),   % Timeout detection
    max_message_size => integer(),      % 16MB default
    strict_delimiter_check => boolean(),
    validate_utf8 => boolean(),
    frame_buffer_size => integer(),     % Max buffer
    frame_buffer_used => integer(),     % Current usage
    backpressure_state => inactive | active,
    backpressure_timer => reference(),
    messages_pending => non_neg_integer(),
    bytes_buffered => non_neg_integer(),
    % Statistics
    messages_received => non_neg_integer(),
    messages_sent => non_neg_integer(),
    bytes_received => non_neg_integer(),
    bytes_sent => non_neg_integer(),
    ping_count => non_neg_integer(),
    pong_count => non_neg_integer(),
    connection_start_time => integer()
}
```

### Implementation Requirements

- ✅ Cowboy WebSocket handler integration
- ✅ Text frames only (reject binary)
- ✅ Line-delimited JSON message extraction
- ✅ Ping/pong keepalive (30s interval)
- ✅ Fragment reassembly with timeout
- ✅ UTF-8 validation
- ✅ Backpressure with buffer monitoring
- ✅ RFC 6455 close code compliance

---

## Capability Negotiation Per Transport

### Transport-Specific Capability Constraints

| Transport | Concurrency | Suitable For | Constraints |
|-----------|-------------|--------------|-------------|
| **STDIO** | Single stream | Basic capabilities | Sequential processing only |
| **TCP** | Multi-connection | All capabilities | Full support |
| **HTTP/SSE** | Concurrent requests | All capabilities | Stateless requests, SSE for push |
| **WebSocket** | Concurrent messages | All capabilities | Real-time, browser-compatible |

### Recommended Capabilities by Transport

| Capability | STDIO | TCP | HTTP/SSE | WebSocket |
|------------|-------|-----|----------|-----------|
| **resources** | ✅ | ✅ | ✅ | ✅ |
| **tools** | ✅ | ✅ | ✅ | ✅ |
| **prompts** | ✅ | ✅ | ✅ | ✅ |
| **sampling** | ⚠️ Limited | ✅ | ✅ | ✅ |
| **logging** | ✅ | ✅ | ✅ | ✅ |
| **completion** | ⚠️ Limited | ✅ | ✅ | ✅ |
| **roots** | ✅ | ✅ | ✅ | ✅ |

---

## Performance Requirements

### Throughput Targets

| Transport | Target | Notes |
|-----------|--------|-------|
| **STDIO** | ~10K msg/sec | Limited by pipe I/O |
| **TCP** | ~43K msg/sec | Real sockets, 4KB packets |
| **HTTP** | ~20K msg/sec | Stateless overhead |
| **WebSocket** | ~30K msg/sec | Text frames |

### Latency Targets

| Percentile | Target | Context |
|------------|--------|---------|
| **p50** | < 100 µs | In-memory processing |
| **p95** | < 500 µs | Network overhead |
| **p99** | < 1000 µs (1ms) | Acceptable max |
| **p99.9** | < 5000 µs (5ms) | Edge cases |

### Connection Scalability

| Deployment | Capacity | Memory per Connection |
|------------|----------|----------------------|
| **Single Node** | 40-50K connections | 2-5 MB |
| **Cluster (Mnesia)** | 100K+ connections | 2-5 MB |

---

## Security Requirements

### All Transports MUST

1. **Validate input**:
   - JSON Schema compliance
   - Message size limits
   - UTF-8 encoding

2. **Prevent path traversal** in resource URIs

3. **Enforce rate limiting** via `erlmcp_rate_limiter`

4. **Support authentication**:
   - API key
   - JWT (RS256, ES256)
   - OAuth 2.0
   - mTLS (mutual TLS)

5. **Protect against DoS**:
   - Message size limits (16MB)
   - Connection limits (configurable)
   - Rate limiting
   - Circuit breakers

### Transport-Specific Security

| Transport | Security Mechanism | Recommendation |
|-----------|-------------------|----------------|
| **STDIO** | Process permissions | Local only, inherit process context |
| **TCP** | Optional TLS | Use TLS for production |
| **HTTP/SSE** | TLS/SSL | **Required** for production |
| **WebSocket** | WSS (Secure) | **Required** for browser clients |

---

## Testing Requirements

### Mandatory Test Coverage

All transport implementations MUST include:

1. ✅ **Connection lifecycle tests**
   - Connect, disconnect, reconnect
   - Idle timeout
   - Connection limit enforcement

2. ✅ **Round-trip message tests**
   - Send request, receive response
   - Notification handling
   - Error response handling

3. ✅ **Error handling tests**
   - Invalid messages
   - Size limit violations
   - UTF-8 validation failures
   - Network errors

4. ✅ **Backpressure tests**
   - Buffer overflow scenarios
   - Flow control activation
   - Drain and resume

5. ✅ **Performance benchmarks**
   - Latency: p50/p95/p99
   - Throughput: messages/sec
   - Memory: bytes per connection

6. ✅ **Coverage ≥ 82%** (mandatory quality gate)

### Test Organization

```
apps/erlmcp_transports/test/
├── erlmcp_transport_stdio_tests.erl
├── erlmcp_transport_tcp_tests.erl
├── erlmcp_transport_http_tests.erl
└── erlmcp_transport_ws_tests.erl
```

---

## Implementation Examples

### STDIO Transport
See: `/home/user/erlmcp/apps/erlmcp_transports/src/erlmcp_transport_stdio.erl`

### TCP Transport
See: `/home/user/erlmcp/apps/erlmcp_transports/src/erlmcp_transport_tcp.erl`

### HTTP Transport
See: `/home/user/erlmcp/apps/erlmcp_transports/src/erlmcp_transport_http.erl`

### WebSocket Transport
See: `/home/user/erlmcp/apps/erlmcp_transports/src/erlmcp_transport_ws.erl`

---

## References

- **MCP Specification**: `docs/MCP_SPECIFICATION_COMPLETE.md`
- **Behavior Definition**: `apps/erlmcp_transports/src/erlmcp_transport_behavior.erl`
- **Contract Types**: `apps/erlmcp_transports/src/erlmcp_transport_contracts.erl`
- **Official MCP Site**: https://modelcontextprotocol.io/specification/2025-11-25

---

## Summary

This specification provides complete behavior contracts for all MCP transport types. Implementers should:

1. Choose appropriate transport for use case
2. Follow behavior contract requirements exactly
3. Implement all mandatory callbacks
4. Test comprehensively (lifecycle, messages, errors, backpressure, performance)
5. Meet performance targets (latency, throughput, scalability)
6. Enforce security requirements (validation, auth, rate limiting)

**Quality Gate**: Compilation + tests pass + coverage ≥ 82% + benchmarks meet targets
