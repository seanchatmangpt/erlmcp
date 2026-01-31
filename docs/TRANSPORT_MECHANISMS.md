# MCP Transport Mechanisms and Connection Models

## Executive Summary

The Model Context Protocol (MCP) enables structured communication between AI assistants and local services using JSON-RPC 2.0 over multiple transport mechanisms. This document provides detailed specifications for all transport types, connection establishment, protocol initialization, role definitions, and lifecycle management in the erlmcp implementation.

**MCP Protocol Version**: 2025-11-25 (ISO 8601 date format)
**JSON-RPC Version**: 2.0

---

## 1. Available Transport Types

### 1.1 Transport Overview

erlmcp implements five primary transport mechanisms, each optimized for different deployment scenarios:

| Transport | Module | Use Case | Features |
|-----------|--------|----------|----------|
| **stdio** | `erlmcp_transport_stdio` | Local process I/O | Line-based, 16MB msg limit, owner-based |
| **TCP** | `erlmcp_transport_tcp` | Network (LAN/WAN) | Ranch-based, connection pooling, keepalive |
| **HTTP** | `erlmcp_transport_http_server` | REST-style, polling | Gun/Cowboy-based, custom headers, SSL/TLS |
| **WebSocket** | `erlmcp_transport_ws` | Bidirectional streaming | Cowboy 2.10+, frame batching, backpressure |
| **SSE** | `erlmcp_transport_sse` | Server push (unidirectional) | Cowboy handler, event-based, reconnect logic |

### 1.2 Transport Behavior Interface

All transports implement a standardized behavior defined in `erlmcp_transport_behavior.erl`:

```erlang
%% Callback Interface (conceptual - actual implementations use gen_server)
-callback init(Config :: map()) ->
    {ok, State :: term()} | {error, Reason :: term()}.

-callback send(State :: term(), Data :: binary()) ->
    ok | {error, Reason :: term()}.

-callback close(State :: term()) -> ok.

-callback get_info(State :: term()) ->
    #{atom() => term()}.  % Optional

-callback handle_transport_call(Request :: term(), State :: term()) ->
    {reply, Reply :: term(), NewState :: term()} |
    {error, Reason :: term()}.  % Optional
```

### 1.3 Transport Registration and Routing

All transports register with the central registry upon initialization:

```erlang
%% Registry Integration
erlmcp_registry:register_transport(TransportId, TransportPid, Config)
```

The registry maintains bidirectional mapping:
```
TransportId ← → ServerPid
```

**Registry Functions**:
- `register_transport/3` - Register new transport
- `unregister_transport/1` - Clean up on shutdown
- `find_transport/1` - Locate transport by ID
- `get_server_for_transport/1` - Resolve server for incoming messages
- `route_message/2` - Route incoming messages to registry

---

## 2. Transport-Specific Details

### 2.1 STDIO Transport

**Module**: `erlmcp_transport_stdio`
**Behavior**: `gen_server`
**Message Framing**: Line-based (CRLF, LF, or CR)

#### Configuration
```erlang
Config = #{
    owner => pid(),              % Process to receive messages
    transport_id => atom(),      % Unique identifier
    test_mode => boolean(),      % Disable reader in tests
    max_message_size => 16777216 % 16 MB default
}
```

#### Initialization Sequence
```erlang
% 1. Start transport
{ok, StdioPid} = erlmcp_transport_stdio:start_link(Owner),

% 2. Transport spawns background reader (unless test_mode=true)
% Reader continuously reads from stdin via io:get_line/1

% 3. Incoming lines sent as: {transport_message, Line}
% Example: Owner ! {transport_message, <<"...json...">>}

% 4. Owner must decode and route JSON-RPC messages
```

#### Message Handling
```erlang
% STDIN → Reader Process → Owner ! {transport_message, Line}
% STDOUT ← send/2 function
send(TransportState, JsonBinary) ->
    io:format("~s~n", [JsonBinary]),
    ok.
```

#### Resource Cleanup
```erlang
% Monitor owner process for early termination
OwnerMonitor = monitor(process, Owner),

% On owner death → transport terminates automatically
% Reader process linked to transport (EXIT signal)
```

**Constraints**:
- Maximum message size: 16 MB (configurable)
- Single-direction per file descriptor (stdin/stdout)
- No connection authentication
- Process-local scope (owner process)

**Guarantees**:
- Line-based atomicity (full lines delivered)
- In-order delivery
- No message loss (until reader subprocess death)

---

### 2.2 TCP Transport

**Module**: `erlmcp_transport_tcp`
**Behavior**: `gen_server` + `ranch_protocol`
**Server Model**: Ranch acceptor pool
**Client Model**: gen_tcp direct connection

#### Configuration

**Server Mode**:
```erlang
ServerConfig = #{
    mode => server,
    host => "0.0.0.0",           % Listen address
    port => 5000,                % Listen port
    num_acceptors => 10,         % Ranch acceptor count
    max_connections => 1024,     % Connection limit
    server_id => my_server,      % Server identifier
    transport_id => tcp_server_1 % Transport ID
}
```

**Client Mode**:
```erlang
ClientConfig = #{
    mode => client,
    host => "localhost",         % Remote host
    port => 5000,               % Remote port
    owner => OwnerPid,          % Message receiver
    connect_timeout => 5000,    % Connection timeout
    max_reconnect_attempts => infinity,
    keepalive => true,          % TCP keepalive
    nodelay => true             % Disable Nagle algorithm
}
```

#### Connection Establishment - Server

```erlang
% 1. Start TCP server
{ok, ServerPid} = erlmcp_transport_tcp:start_server(ServerConfig),

% 2. Ranch starts acceptor pool
% 3. For each incoming connection:
%    - erlmcp_transport_tcp:start_link/3 (ranch_protocol callback)
%    - Check connection limit via erlmcp_connection_limiter:accept_connection/1
%    - If accept: create handler gen_server
%    - If reject: send error, close socket

% 4. Handler initializes:
%    - Socket obtained from ranch
%    - Register with registry
%    - Start reading from socket

% 5. Socket mode: {active, once}
%    - Receive data: {tcp, Socket, Data}
%    - Backpressure: inet:setopts(Socket, [{active, false}])
```

**Connection Acceptance Flow**:
```
Incoming TCP → Ranch Acceptor Pool
                    ↓
    erlmcp_connection_limiter:accept_connection(ServerId)
                    ↓
        ┌───────────┴───────────┐
        ↓                       ↓
    accept (slot available)   {error, too_many_connections}
        ↓                       ↓
    Start handler gen_server   Send error response
        ↓                       ↓
    Initialize socket...      Close connection
        ↓
    Register with registry
```

#### Connection Establishment - Client

```erlang
% 1. Start TCP client
{ok, ClientPid} = erlmcp_transport_tcp:start_client(ClientConfig),

% 2. Initiate connection
ok = erlmcp_transport_tcp:connect(ClientPid, ConnectOpts),

% 3. Client attempts connection with exponential backoff:
%    - Initial delay: 1000 ms
%    - Max delay: 60000 ms
%    - Multiplier: (calculated in loop)

% 4. On successful connection:
%    - Socket mode: {active, once}
%    - Begin reading messages

% 5. On connection failure:
%    - If reconnect_attempts < max: schedule retry
%    - Else: terminate with error
```

#### Message Framing

TCP uses **newline-delimited JSON**:
```
{msg1}\n{msg2}\n{msg3}\n
```

```erlang
% Sending: send/2 automatically appends newline
send(State, JsonData) ->
    gen_tcp:send(State#state.socket, [JsonData, <<"\n">>]).

% Receiving: extract_lines_from_buffer/3 handles buffering
extract_lines_from_buffer(Buffer, NewData) ->
    UpdatedBuffer = <<Buffer/binary, NewData/binary>>,
    % Split on \r\n, \n, or \r
```

#### Connection Resource Management

**Connection Limit Enforcement**:
```erlang
%% Per-server connection pool
erlmcp_connection_limiter:accept_connection(ServerId)
%% Tracks: current_connections/max_connections
%% Returns: {accept} | {error, too_many_connections}

%% On handler termination:
erlmcp_connection_limiter:release_connection(ServerId)
%% CRITICAL: Must always be called (even on init failure)
```

**Socket Options**:
```erlang
tcp_options = [
    {active, once},         % Receive one message, then pause
    {nodelay, true},        % Disable Nagle (TCP_NODELAY)
    {keepalive, true},      % Enable TCP keepalive
    {reuseaddr, true},      % Allow port reuse
    {recbuf, 65536},        % Receive buffer
    {sndbuf, 65536}         % Send buffer
]
```

**Idle Timeout**:
```erlang
-define(IDLE_TIMEOUT, 300000).  % 5 minutes
% If no data received in 5 minutes → close connection
```

#### Connection Pooling (Optional)

For client mode with high throughput:
```erlang
PoolConfig = #{
    use_pool => true,
    pool_name => tcp_pool_1,
    pool_min_size => 10,
    pool_max_size => 1000,
    pool_strategy => round_robin | least_loaded | random
}
```

**Constraints**:
- Maximum message size: 16 MB
- TCP connection overhead: ~2KB per connection
- Buffer sizes: 65KB receive/send (configurable)
- No built-in encryption (use TLS wrapper)

**Guarantees**:
- TCP-level atomicity for individual packets
- In-order delivery (TCP guarantee)
- Connection loss detection (socket errors)
- Automatic reconnection (client mode)

---

### 2.3 HTTP Transport

**Module**: `erlmcp_transport_http_server`
**Behavior**: `gen_server`
**HTTP Client**: Gun library (async)

#### Configuration

```erlang
HttpConfig = #{
    url => <<"http://localhost:8080/mcp">>,  % Server endpoint
    owner => OwnerPid,                       % Message receiver
    method => post,                          % HTTP method
    headers => [
        {<<"Content-Type">>, <<"application/json">>},
        {<<"Authorization">>, <<"Bearer token">>}
    ],
    timeout => 5000,                         % Request timeout
    connect_timeout => 5000,
    max_retries => 3,
    retry_delay => 1000,
    ssl_options => [],                       % TLS options
    pool_size => 10                          % Connection pool
}
```

#### Request-Response Pattern

```
Client                          HTTP Server
  │
  ├─ POST /mcp
  │   Body: {JSON-RPC Request}
  │──────────────────────────────────→
  │
  │                     Parse JSON-RPC
  │                     Process request
  │                     Generate response
  │
  │   ← ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─
  │   Response: {JSON-RPC Response}
  │
  └─ Handle response/error
```

#### Polling Mode (for server responses)

HTTP doesn't inherently support server-to-client messaging. For subscriptions:

```erlang
%% Option 1: Polling (client initiates)
Client polls GET /mcp/events?session_id=xxx every N seconds

%% Option 2: Long-polling (client holds connection)
Client makes request: GET /mcp/events?wait=true
Server holds response until event available or timeout

%% Option 3: Use WebSocket or SSE instead
```

#### Error Handling

```erlang
% HTTP-level errors (connection, timeout, auth)
{error, {http_error, Reason}}

% JSON-RPC errors (passed through as response)
{error, {json_rpc, Code, Message}}

% Automatic retry with exponential backoff:
Delay_n = min(retry_delay * 2^n, max_backoff)
```

**Constraints**:
- No persistent connection (request-response only)
- Maximum message size: limited by HTTP server
- No built-in flow control
- Not suitable for high-frequency bidirectional messaging

**Guarantees**:
- Request-response atomicity (per HTTP request)
- No message ordering guarantee across requests
- Connection pooling reduces overhead
- SSL/TLS support via Erlang SSL

---

### 2.4 WebSocket Transport

**Module**: `erlmcp_transport_ws`
**Behavior**: Cowboy 2.10+ WebSocket handler
**Protocol**: RFC 6455

#### Configuration

```erlang
WsConfig = #{
    transport_id => ws_server_1,
    port => 8080,
    path => "/mcp/ws",                    % URL path
    max_message_size => 16777216,         % 16 MB
    max_connections => 1000,
    strict_delimiter_check => true,
    validate_utf8 => true,
    frame_buffer_size => 102400,          % 100 KB
    %% Flow control
    backpressure_state => inactive,       % or active
    backpressure_timeout => 5000
}
```

#### Connection Establishment

```erlang
% 1. Client initiates WebSocket handshake
Client: GET /mcp/ws HTTP/1.1
        Upgrade: websocket
        Connection: Upgrade
        Sec-WebSocket-Key: ...
        Sec-WebSocket-Version: 13

% 2. Server validates handshake
%    - Upgrade header
%    - Connection header
%    - Sec-WebSocket-Key (RFC 6455 algorithm)
%    - Sec-WebSocket-Version = 13

% 3. Server responds with 101 Switching Protocols
Server: HTTP/1.1 101 Switching Protocols
        Upgrade: websocket
        Connection: Upgrade
        Sec-WebSocket-Accept: ...

% 4. Upgrade to binary/text frames
% 5. Connection kept alive indefinitely (or until close)
```

#### Message Framing

**Frame Format** (RFC 6455):
```
0                   1                   2                   3
0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
+-+-+-+-+-------+-+-------------+-------------------------------+
|F|R|R|R|opcode |M| Payload len |    Extended payload length    |
|I|S|S|S|(4)    |A|     (7)     |             (16/64)            |
|N|V|V|V|       |S|             |   (if payload len==126/127)   |
| |1|2|3|       |K|             |                               |
+-+-+-+-+-------+-+-------------+ - - - - - - - - - - - - - - - +
|     Extended payload length continued, if payload len == 127  |
+ - - - - - - - - - - - - - - - +-------------------------------+
|                               |Masking-key, if MASK set to 1  |
+-------------------------------+-------------------------------+
| Masking-key (continued)       |          Payload Data          |
+-------------------------------- - - - - - - - - - - - - - - - +
:                     Payload Data continued ...                :
+ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - +
|                     Payload Data continued ...                |
+---------------------------------------------------------------+
```

**MCP Frame Format**:
```erlang
% Text frame containing newline-delimited JSON
Frame = {text, <<"[JSON1]\n[JSON2]\n[JSON3]\n">>}

% Binary frame (not recommended for MCP)
Frame = {binary, <<...>>}

% Control frames
{ping, Payload}   → Auto-respond with {pong, Payload}
{pong, Payload}   → Acknowledge received PING
{close, Code, Reason} → Graceful close
```

#### Backpressure and Flow Control

**Problem**: Fast producer, slow consumer → buffer overflow

**Solution**: Dynamic backpressure states

```erlang
% Backpressure detection:
frame_buffer_used > frame_buffer_size * threshold
  → backpressure_state = active
  → Client: {backpressure, {buffer_full, Bytes}}

% Client pauses sending

% Buffer drains:
frame_buffer_used < frame_buffer_size * drain_threshold
  → backpressure_state = inactive
  → Client: {resume_sending}

% Client resumes
```

**Configuration**:
```erlang
-define(BACKPRESSURE_THRESHOLD, 0.8).      % 80% full
-define(BUFFER_DRAIN_THRESHOLD, 0.5).      % 50% empty → resume
-define(BACKPRESSURE_TIMEOUT, 5000).       % 5 sec max backpressure
```

#### Keep-Alive Mechanism

```erlang
-define(PING_INTERVAL, 30000).  % Send PING every 30s

% Server periodically sends:
{ping, Payload}

% Client responds:
{pong, Payload}

% If no PONG within timeout → close connection
```

#### Connection Cleanup

```erlang
% 1. Client initiates close:
{close, 1000, <<"Normal closure">>}

% 2. Server responds:
{close, 1000, <<"Going away">>}

% 3. Both sides close TCP socket

% WebSocket close codes:
-define(WS_CLOSE_NORMAL, 1000).            % Normal closure
-define(WS_CLOSE_GOING_AWAY, 1001).        % Server shutting down
-define(WS_CLOSE_PROTOCOL_ERROR, 1002).    % Protocol violation
-define(WS_CLOSE_UNSUPPORTED_DATA, 1003).  % Unsupported data type
-define(WS_CLOSE_MESSAGE_TOO_BIG, 1009).   % Message exceeds limit
-define(WS_CLOSE_INTERNAL_ERROR, 1011).    % Unexpected condition
```

#### Fragment Reassembly

**Problem**: Large messages fragmented into multiple frames

**Solution**: Fragment buffer with timeout

```erlang
% Client sends fragmented message:
Frame 1: {text, PartialJson, false}  % false = more frames coming
Frame 2: {text, MoreJson, false}
Frame 3: {text, FinalJson, true}     % true = final frame

% Server reassembles:
fragment_buffer = PartialJson + MoreJson + FinalJson

% If timeout (30s) expires before final frame → close (1009)
```

**Constraints**:
- Maximum message size: 16 MB
- Maximum connections per listener: configurable
- Frame buffer limit: 100 KB default
- No built-in encryption (use wss:// scheme with TLS)

**Guarantees**:
- TCP-level delivery (WebSocket layer)
- Frame integrity (RFC 6455 masking)
- In-order delivery (per connection)
- Automatic keep-alive via PING/PONG
- Graceful close handling

---

### 2.5 Server-Sent Events (SSE) Transport

**Module**: `erlmcp_transport_sse`
**Behavior**: Cowboy HTTP handler (streaming response)
**Protocol**: RFC 8188 (Server-Sent Events)

#### Configuration

```erlang
SseConfig = #{
    transport_id => sse_server_1,
    port => 8081,
    path => "/mcp/sse",
    max_message_size => 16777216,  % 16 MB
    retry_timeout => 5000           % Client reconnect delay
}
```

#### Connection Model - Asymmetric

**Asymmetric Architecture**:
```
Client ──────→ POST /mcp/messages (JSON-RPC requests)
              ↓
        Server processes

Client ←────── GET /mcp/sse (Server-Sent Events stream)
       (Server pushes notifications)
```

**Why Asymmetric**?
- Server → Client: HTTP streaming (SSE)
- Client → Server: Regular HTTP POST

#### SSE Event Format (RFC 8188)

```
field: value\n
field: value\n
\n

Example:
event: message\n
data: {"jsonrpc":"2.0",...}\n
id: 42\n
retry: 5000\n
\n
```

#### Message Flow

```erlang
% 1. Client establishes SSE connection
GET /mcp/sse HTTP/1.1
Accept: text/event-stream

% 2. Server responds with streaming headers
HTTP/1.1 200 OK
Content-Type: text/event-stream
Cache-Control: no-cache
Connection: keep-alive
\n\n

% 3. Server streams events indefinitely
event: keepalive\n
id: 1\n
\n

event: message\n
data: {"jsonrpc":"2.0","method":"resources/updated",...}\n
id: 2\n
\n

% 4. For client → server communication, separate POST:
POST /mcp/messages HTTP/1.1
Content-Type: application/json

{"jsonrpc":"2.0","id":1,"method":"tools/list",...}
```

#### Retry Logic

**Client Reconnection**:
```erlang
-define(DEFAULT_RETRY_TIMEOUT, 5000).  % 5 seconds

% Server sends:
event: message\n
retry: 5000\n
data: ...\n
\n

% Client reconnects after 5 seconds if connection lost
% Each reconnect includes Last-Event-ID header:
GET /mcp/sse HTTP/1.1
Last-Event-ID: 42
Accept: text/event-stream
```

#### Event Types

```erlang
-define(EVENT_TYPE_MESSAGE, <<"message">>).       % JSON-RPC message
-define(EVENT_TYPE_NOTIFICATION, <<"notification">>). % Subscription update
-define(EVENT_TYPE_ERROR, <<"error">>).           % Error notification
-define(EVENT_TYPE_KEEPALIVE, <<"keepalive">>).   % Ping (no data)
-define(EVENT_TYPE_CLOSE, <<"close">>).           % Server closing
```

#### Session Management

```erlang
% Client establishes session
POST /mcp/sessions/create HTTP/1.1
→ Response: {"session_id": "uuid"}

% Client includes session in SSE URL
GET /mcp/sse?session_id=uuid

% Server tracks session state
```

**Constraints**:
- Unidirectional server → client (requires separate channel for requests)
- Client must establish separate POST endpoint for requests
- Message size limit: 16 MB
- No built-in compression
- Connection overhead: one HTTP connection per client

**Guarantees**:
- Server → Client: in-order delivery (HTTP streaming guarantee)
- Automatic reconnection (with Last-Event-ID support)
- Event numbering (id field)
- No message loss (assuming clients respect retry timeouts)

---

## 3. Protocol Initialization Sequence

### 3.1 Phase-Based Initialization State Machine

MCP defines three connection phases:

```erlang
-type mcp_server_phase() ::
    ?MCP_PHASE_INITIALIZATION |  % Before initialize response
    ?MCP_PHASE_INITIALIZED |     % After initialize response
    ?MCP_PHASE_ERROR.           % Initialization failed

-define(MCP_PHASE_INITIALIZATION, initialization).
-define(MCP_PHASE_INITIALIZED, initialized).
-define(MCP_PHASE_ERROR, error).
-define(MCP_DEFAULT_INIT_TIMEOUT_MS, 60000).  % 60 seconds
```

### 3.2 Initialization Request (Client → Server)

**Before**: Connection established, no protocol negotiation
**After**: Both sides know capabilities, roles, protocol version

```erlang
% Client sends initialize request
Request = #{
    <<"jsonrpc">> => <<"2.0">>,
    <<"id">> => 1,
    <<"method">> => <<"initialize">>,
    <<"params">> => #{
        <<"protocolVersion">> => <<"2025-11-25">>,
        <<"capabilities">> => #{
            <<"roots">> => #{
                <<"listChanged">> => true  % Supports roots/list_changed
            },
            <<"sampling">> => #{}          % Supports sampling
        },
        <<"clientInfo">> => #{
            <<"name">> => <<"Claude">>,
            <<"version">> => <<"3.5">>
        }
    }
}
```

**Fields**:
- `protocolVersion` (required): MCP version (e.g., "2025-11-25")
- `capabilities` (required): Client capability set
- `clientInfo` (required): Client identification

**Capability Flags**:
```erlang
-record(mcp_client_capabilities, {
    roots = #mcp_capability{enabled = false},      % Workspace roots
    sampling = #mcp_capability{enabled = false}    % LLM sampling
}).

-record(mcp_server_capabilities, {
    resources = #mcp_capability{enabled = false},   % Resource support
    tools = #mcp_capability{enabled = false},       % Tool support
    prompts = #mcp_capability{enabled = false},     % Prompt support
    logging = #mcp_capability{enabled = false}      % Logging support
}).
```

### 3.3 Initialization Response (Server → Client)

```erlang
% Server responds with capabilities
Response = #{
    <<"jsonrpc">> => <<"2.0">>,
    <<"id">> => 1,
    <<"result">> => #{
        <<"protocolVersion">> => <<"2025-11-25">>,
        <<"capabilities">> => #{
            <<"resources">> => #{
                <<"subscribe">> => true,
                <<"listChanged">> => true
            },
            <<"tools">> => #{}
        },
        <<"serverInfo">> => #{
            <<"name">> => <<"my-server">>,
            <<"version">> => <<"1.0.0">>
        }
    }
}
```

**Fields**:
- `protocolVersion`: Server's protocol version (MUST match request or error)
- `capabilities`: Server's supported features
- `serverInfo`: Server identification

### 3.4 Initialization Timeout

```erlang
% Server waits for initialize request
init_timeout_ref = erlang:send_after(
    init_timeout_ms,  % Default 60 seconds
    self(),
    {init_timeout}
).

% If initialize received:
erlang:cancel_timer(init_timeout_ref),
phase = initialized.

% If timeout expires:
phase = error,
close_connection().
```

### 3.5 Phase Enforcement

**Before Initialization**:
```erlang
% Requests before initialize → error
{error, {not_initialized, Phase, <<"Client not initialized">>}}

% Only these methods allowed:
- initialize
- ping

% All other methods → error -32005 (MCP_ERROR_NOT_INITIALIZED)
```

**After Initialization**:
```erlang
% All methods available (if capabilities negotiated)

% Capability-gated methods:
capabilities.resources.enabled = true   → resources/list, resources/read, etc.
capabilities.tools.enabled = true       → tools/list, tools/call, etc.
```

### 3.6 Full Initialization Sequence Diagram

```
Client                                  Server/Transport
  │
  ├─ Establish connection (TCP/WebSocket/HTTP)
  │────────────────────────────────────→
  │                                    Register with registry
  │                                    Phase = initialization
  │                                    Start init timeout
  │
  ├─ Send initialize request
  │  {"id": 1, "method": "initialize", ...}
  │────────────────────────────────────→
  │                                    Receive request
  │                                    Parse JSON-RPC
  │                                    Validate capabilities
  │                                    Cancel timeout
  │
  │                                    Generate response
  │                                    Phase = initialized
  │
  ├─ Receive initialize response ←────
  │  {"id": 1, "result": {...}}
  │
  │ Phase = initialized
  │
  └─ Ready for method calls
     (resources/list, tools/call, etc.)
```

---

## 4. Client vs Server Role Definitions

### 4.1 Client Role

**Responsibilities**:
1. Initiate connections to servers
2. Send requests for resources, tools, prompts
3. Handle responses and errors
4. Manage subscriptions to resource updates
5. Implement sampling requests (optional)

**Module**: `erlmcp_client`
**Behavior**: `gen_server`

**API**:
```erlang
% Lifecycle
{ok, ClientPid} = erlmcp_client:start_link(TransportOpts, ClientOpts),
{ok, ServerCapabilities} = erlmcp_client:initialize(ClientPid, ClientCapabilities),

% Requests
{ok, Resources} = erlmcp_client:list_resources(ClientPid),
{ok, Content} = erlmcp_client:read_resource(ClientPid, Uri),
{ok, Tools} = erlmcp_client:list_tools(ClientPid),
{ok, Result} = erlmcp_client:call_tool(ClientPid, ToolName, Arguments),
{ok, Prompts} = erlmcp_client:list_prompts(ClientPid),
{ok, Prompt} = erlmcp_client:get_prompt(ClientPid, PromptName, Arguments),

% Subscriptions
ok = erlmcp_client:subscribe_to_resource(ClientPid, Uri),
ok = erlmcp_client:unsubscribe_from_resource(ClientPid, Uri),

% Notifications
ok = erlmcp_client:set_notification_handler(
    ClientPid,
    <<"resources/updated">>,
    fun(Params) -> handle_resource_update(Params) end
),

erlmcp_client:stop(ClientPid).
```

**State Management**:
```erlang
-record(state, {
    phase = pre_initialization :: client_phase(),  % Connection phase
    capabilities :: #mcp_server_capabilities{},    % Server capabilities
    pending_requests = #{} :: #{request_id() => {atom(), pid()}},
    subscriptions = sets:set() :: sets:set(binary()),
    notification_handlers = #{} :: #{binary() => handler()},
    timeout = 5000 :: timeout()
}).
```

**Request-Response Correlation**:
```erlang
% Each request assigned unique ID
request_id = 1, 2, 3, ...

% On response receive:
case maps:get(id, Response) of
    RequestId -> correlate_with_pending(RequestId, Response);
    _ -> {error, unknown_id}
end
```

### 4.2 Server Role

**Responsibilities**:
1. Accept incoming connections (transport-dependent)
2. Validate initialize requests
3. Expose resources, tools, prompts
4. Handle client requests with handlers
5. Send notifications (resource updates, list changes)

**Module**: `erlmcp_server`
**Behavior**: `gen_server`

**API**:
```erlang
% Lifecycle
{ok, ServerPid} = erlmcp_server:start_link(ServerId, Capabilities),

% Resources
ok = erlmcp_server:add_resource(ServerPid, Uri, Handler),
ok = erlmcp_server:add_resource_template(ServerPid, UriTemplate, Name, Handler),
ok = erlmcp_server:delete_resource(ServerPid, Uri),

% Tools
ok = erlmcp_server:add_tool(ServerPid, ToolName, Handler),
ok = erlmcp_server:add_tool_full(ServerPid, ToolName, Description, Handler, Options),
ok = erlmcp_server:delete_tool(ServerPid, ToolName),

% Prompts
ok = erlmcp_server:add_prompt(ServerPid, PromptName, Handler),
ok = erlmcp_server:add_prompt_with_args(ServerPid, PromptName, Handler, Arguments),
ok = erlmcp_server:delete_prompt(ServerPid, PromptName),

% Subscriptions
ok = erlmcp_server:subscribe_resource(ServerPid, Uri, ClientPid),
ok = erlmcp_server:unsubscribe_resource(ServerPid, Uri, ClientPid),
ok = erlmcp_server:notify_resource_updated(ServerPid, Uri, Metadata),
ok = erlmcp_server:notify_resources_changed(ServerPid),

% Progress tracking
ok = erlmcp_server:report_progress(ServerPid, ProgressToken, Progress, Total),

erlmcp_server:stop(ServerPid).
```

**Handler Types**:
```erlang
% Resource handler: Uri → Content
-type resource_handler() :: fun((binary()) ->
    {ok, #mcp_content{}} | {error, Reason}
).

% Tool handler: Arguments → Result
-type tool_handler() :: fun((map()) ->
    {ok, [#mcp_content{}]} | {error, Reason}
).

% Prompt handler: Arguments → Messages
-type prompt_handler() :: fun((map()) ->
    {ok, #mcp_prompt_message{}} | {error, Reason}
).
```

**State Management**:
```erlang
-record(state, {
    phase :: mcp_server_phase(),
    init_timeout_ref :: reference(),
    capabilities :: #mcp_server_capabilities{},
    client_capabilities :: #mcp_client_capabilities{},
    resources = #{} :: #{binary() => {#mcp_resource{}, handler()}},
    tools = #{} :: #{binary() => {#mcp_tool{}, handler()}},
    prompts = #{} :: #{binary() => {#mcp_prompt{}, handler()}},
    subscriptions = #{} :: #{binary() => sets:set(pid())}
}).
```

### 4.3 Capability Negotiation

**Both Sides Declare Capabilities**:

```erlang
% Client capabilities
ClientCaps = #mcp_client_capabilities{
    roots = #mcp_capability{enabled = true},      % I support workspace roots
    sampling = #mcp_capability{enabled = true}    % I can sample from LLM
}

% Server capabilities
ServerCaps = #mcp_server_capabilities{
    resources = #mcp_capability{enabled = true},  % I have resources
    tools = #mcp_capability{enabled = true}       % I have tools
}

% Server stores client_capabilities
% Client stores server_capabilities

% Before each operation, check:
case ServerCapabilities#mcp_server_capabilities.tools of
    #mcp_capability{enabled = true} ->
        can_call_tools();
    _ ->
        {error, {capability_not_supported, tools}}
end
```

---

## 5. Transport Layer Requirements and Guarantees

### 5.1 Delivery Guarantees

| Guarantee | STDIO | TCP | HTTP | WebSocket | SSE |
|-----------|-------|-----|------|-----------|-----|
| **Atomicity** | Line-based | Packet-based | Request-based | Frame-based | Event-based |
| **In-Order** | ✓ | ✓ | ✗ | ✓ | ✓ |
| **No Duplication** | ✓ | ✓ | ✗ | ✓ | ✓ (with ID) |
| **Persistence** | ✗ | ✗ | Optional | ✗ | ✗ |
| **Bidirectional** | ✓ | ✓ | ✓ (separate) | ✓ | ✗ (unidirectional) |
| **Backpressure** | ✗ | ✓ | Limited | ✓ | Limited |

### 5.2 Message Size Limits

**All Transports**:
```erlang
-define(DEFAULT_MAX_MESSAGE_SIZE, 16777216).  % 16 MB
```

**Validation**:
```erlang
erlmcp_message_size:validate_message_size(TransportType, JsonBinary)
→ ok | {error, {message_too_large, ErrorResponse}}
```

**Error Code**: `-32012` (MCP_ERROR_MESSAGE_TOO_LARGE)

### 5.3 Timeout Specifications

| Timeout | Value | Purpose |
|---------|-------|---------|
| **Initialize** | 60,000 ms | Client must initialize within 1 minute |
| **Request** | 5,000 ms | Default timeout for method calls |
| **TCP Connect** | 5,000 ms | Client connection attempt |
| **TCP Idle** | 300,000 ms | 5 minutes without data → close |
| **WebSocket Ping** | 30,000 ms | Send PING every 30 seconds |
| **HTTP Request** | 5,000 ms | Configurable per request |

### 5.4 Backpressure Handling

**Definition**: Producer faster than consumer

**Mechanism**:
```erlang
% Transport-specific:
- STDIO: No backpressure (owner responsible)
- TCP: {active, once} mode (explicit re-enabling)
- WebSocket: Frame buffer monitoring → adaptive flow control
- HTTP: Queue responses in handler (poolboy manages)
- SSE: Queue events server-side
```

**Example - TCP Backpressure**:
```erlang
% Server busy, cannot process more messages
inet:setopts(Socket, [{active, false}]),
% Socket not receiving data

% When ready:
inet:setopts(Socket, [{active, once}]),
% Receive one message, then wait for re-enable
```

### 5.5 Connection Persistence

**Keep-Alive Mechanisms**:
```erlang
% TCP
{keepalive, true}
↓
TCP kernel sends keep-alive probes every 2 hours (OS-dependent)

% WebSocket
erlmcp_transport_ws:ping_interval() = 30 seconds
Client must respond with pong within timeout
Otherwise → close connection (1011 Internal Error)

% HTTP
No built-in keep-alive
Use polling for server → client updates

% SSE
Connection stays open indefinitely
Periodic keepalive events recommended
```

---

## 6. Connection Lifecycle and Cleanup

### 6.1 Connection Establishment Phases

```
Phase 1: Transport Connection
  ├─ STDIO: process_flag(trap_exit, true), monitor owner
  ├─ TCP: Accept connection, start handler, increment slot counter
  ├─ WebSocket: HTTP upgrade, validate handshake
  └─ HTTP: Create session

Phase 2: MCP Registration
  ├─ Register transport with erlmcp_registry
  ├─ Bind transport to server (optional)
  └─ Initialize message routing

Phase 3: Protocol Initialization
  ├─ Await initialize request (timeout: 60s)
  ├─ Validate protocol version
  ├─ Store capabilities
  └─ Move to initialized phase

Phase 4: Ready
  ├─ Accept method requests
  ├─ Send notifications
  └─ Handle subscriptions
```

### 6.2 Graceful Shutdown

**Server-Initiated Shutdown**:
```erlang
% 1. Close listen socket (stop accepting new connections)
ranch:stop_listener(RanchRef).

% 2. Notify active connections
Connections ! {shutdown, Reason}.

% 3. Wait for graceful close
% Each connection:
%   - Finish pending requests (timeout: 5s)
%   - Send close frame (WebSocket) or close socket
%   - Cleanup state

% 4. Release resources
erlmcp_connection_limiter:release_connection(ServerId).
```

**Client-Initiated Shutdown**:
```erlang
% 1. Stop sending requests
% 2. Wait for outstanding responses (timeout: 5s)
% 3. Close transport
erlmcp_transport_tcp:close(State).

% 4. Cleanup state
erlmcp_registry:unregister_transport(TransportId).
```

### 6.3 Error Recovery Strategies

**Transport-Level Errors**:
```erlang
% Recoverable
{error, {tcp_send_failed, timeout}}
→ Retry with exponential backoff

{error, {http_error, {connection_lost, _}}}
→ Reconnect (client mode)

% Non-recoverable
{error, {tcp_send_failed, econnrefused}}
→ Terminate connection (server mode)

{error, {socket_error, closed}}
→ Cleanup and close
```

**Protocol-Level Errors**:
```erlang
% Invalid JSON-RPC
{error, {parse_error, invalid_json}}
→ Send error response, keep connection open

% Missing required field
{error, {invalid_message, missing_id}}
→ Send error response -32602 (Invalid params)

% Protocol version mismatch
{error, {protocol_version_mismatch, Expected, Got}}
→ Send error response -32062, close connection
```

**Cleanup Guarantees**:
```erlang
% Always execute cleanup, even on error
try
    Operation
catch
    Type:Reason:Stack ->
        Cleanup(),
        {error, {Type, Reason}}
end.

% Critical cleanup operations:
- erlmcp_connection_limiter:release_connection/1
- erlmcp_registry:unregister_transport/1
- erlmcp_registry:unregister_server/1
- gen_tcp:close/1
- erlang:demonitor/1 (cancel monitors)
- erlang:cancel_timer/1 (cancel timers)
```

### 6.4 Resource Leak Prevention

**Connection Pool Limits**:
```erlang
% TCP server enforces max connections
erlmcp_connection_limiter:accept_connection(ServerId)
→ Blocks new connections when limit reached
→ Returns error to client

% Prevents: unbounded memory growth, file descriptor exhaustion
```

**Timer Cleanup**:
```erlang
% Initialize timeout
init_timeout_ref = erlang:send_after(60000, self(), init_timeout),
% MUST cancel on:
- Initialize received
- Connection close
- Error during init
erlang:cancel_timer(init_timeout_ref).
```

**Socket Closure**:
```erlang
% Explicit socket close (don't rely on GC)
gen_tcp:close(Socket).

% Monitor process to detect unexpected death
erlang:monitor(process, HandlerPid),
receive
    {'DOWN', MonitorRef, process, HandlerPid, Reason} ->
        % Handler died, cleanup resources
end.
```

---

## 7. Concurrent Connection Handling

### 7.1 Connection Pooling Architecture

**For High-Concurrency Servers**:
```erlang
erlmcp_transport_tcp:start_server(#{
    mode => server,
    num_acceptors => 10,        % Ranch acceptor processes
    max_connections => 1024,    % Global limit
    server_id => my_server
}).
```

**Flow**:
```
Incoming connections
        ↓
Ranch acceptor pool (10 processes)
        ↓
erlmcp_connection_limiter:accept_connection/1
        ↓
Handler gen_server spawned
        ↓
Registered with registry
```

### 7.2 Request Concurrency - Server Side

**Per-Connection Handler**:
```erlang
% Each client connection spawns unique handler
Handler1: client_1@127.0.0.1:5000 → handler_pid_1
Handler2: client_2@127.0.0.1:5001 → handler_pid_2
...
HandlerN: client_N@127.0.0.1:5NNN → handler_pid_N

% Each handler processes messages sequentially
% (gen_server handle_call/handle_cast)
```

**Request Parallelism**:
```erlang
% Multiple clients → multiple handlers → parallelism
Client1 ! {request, tools/list}  → Handler1 processes
Client2 ! {request, tools/call}  → Handler2 processes simultaneously
```

**Tool Execution**:
```erlang
% If tool handler is slow, handler is blocked
erlmcp_server:add_tool(Server, ToolName, fun(Args) ->
    do_slow_operation()  % ← Blocks handler until complete
end).

% To avoid blocking:
erlmcp_server:add_tool(Server, ToolName, fun(Args) ->
    {ok, spawn_worker(Args)}  % Return immediately
end).
```

### 7.3 Client-Side Concurrency

**Batch Requests**:
```erlang
% Send multiple requests in one batch
erlmcp_client:send_batch_request(
    ClientPid,
    RequestBatchId,
    [
        {id, 1, method, <<"resources/list">>, params, #{}},
        {id, 2, method, <<"tools/list">>, params, #{}},
        {id, 3, method, <<"prompts/list">>, params, #{}}
    ]
),

% Receive responses (unordered)
case receive_response(Timeout) of
    {ok, Response} -> handle_response(Response);
    {error, Reason} -> handle_error(Reason)
end.
```

**Multiple Subscriptions**:
```erlang
% Subscribe to multiple resources simultaneously
ok = erlmcp_client:subscribe_to_resource(ClientPid, <<"resource/1">>),
ok = erlmcp_client:subscribe_to_resource(ClientPid, <<"resource/2">>),

% Receive updates (unordered)
set_notification_handler(ClientPid, <<"resources/updated">>,
    fun(Params) ->
        handle_update(maps:get(<<"uri">>, Params))
    end
).
```

### 7.4 Avoiding Head-of-Line Blocking

**Problem**: One slow request blocks subsequent requests in gen_server queue

**Solution 1: Spawn Worker Process**:
```erlang
% Tool handler spawns worker
add_tool(Server, Name, fun(Args) ->
    {ok, _WorkerPid} = spawn_worker(Args),
    {ok, [{<<"type">>, <<"text">>}, {<<"text">>, <<"Processing...">>}]}
end),

% Progress updates via separate channel
notify_tool_complete(WorkerId, Result).
```

**Solution 2: Progress Tokens**:
```erlang
% Start long-running operation with progress token
{ok, ProgressToken} = erlmcp_client:complete(
    ClientPid,
    <<"ref123">>,
    <<"argument">>,
    5000  % 5 second timeout for first response
),

% Client can poll or receive updates via separate channel
```

### 7.5 Load Balancing - Client Pool

**TCP Connection Pooling**:
```erlang
ClientConfig = #{
    mode => client,
    host => <<"localhost">>,
    port => 5000,
    use_pool => true,
    pool_name => tcp_pool_1,
    pool_min_size => 10,
    pool_max_size => 1000,
    pool_strategy => round_robin  % least_loaded, random
}.
```

**Strategy Selection**:
```erlang
round_robin       % Distribute evenly across connections
least_loaded      % Direct to least-busy handler
random            % Random selection
```

---

## 8. Transport-Specific Considerations

### 8.1 STDIO Considerations

**Best For**:
- Local process communication
- CLI tools
- Development/testing

**Limitations**:
- No authentication
- Single reader process (bottleneck potential)
- Owner process responsibility for routing

**Optimization**:
```erlang
% High-volume scenarios: filter early
Owner ! {transport_message, Line} only if valid JSON
```

---

### 8.2 TCP Considerations

**Best For**:
- Persistent connections
- LAN/WAN communication
- High-throughput scenarios

**Network Configuration**:
```erlang
% Disable Nagle's algorithm (important for latency)
{nodelay, true}

% Buffer sizes (tune for workload)
{recbuf, 65536},
{sndbuf, 65536}

% Keepalive (detect dead connections)
{keepalive, true}
```

**Scaling**:
```erlang
% For 10K+ concurrent connections
ranch:start_listener(
    tcp_server,
    ranch_tcp,
    #{socket_opts => [
        {reuseaddr, true},
        {nodelay, true}
    ]},
    num_acceptors => 64,         % CPU cores * 4-8
    max_connections => 10000
).
```

**Connection Limits**:
- Per-server: Configurable via `max_connections`
- Per-machine: File descriptor limit (`ulimit -n`)
- Per-port: 65K addresses (ephemeral + TIME_WAIT)

---

### 8.3 HTTP Considerations

**Best For**:
- RESTful integration
- Firewall-friendly (port 80/443)
- Stateless servers

**Stateless Design**:
```erlang
% Each request independent (no session state)
% Use session IDs in URL for correlation

POST /mcp/messages?session_id=uuid
{request}

% Response via polling
GET /mcp/events?session_id=uuid
```

**Security Headers**:
```erlang
Headers = [
    {<<"Content-Type">>, <<"application/json">>},
    {<<"Content-Security-Policy">>, <<"script-src 'self'">>},
    {<<"X-Frame-Options">>, <<"DENY">>},
    {<<"X-Content-Type-Options">>, <<"nosniff">>}
]
```

---

### 8.4 WebSocket Considerations

**Best For**:
- Bidirectional streaming
- Real-time subscriptions
- Single persistent connection

**Performance Optimization**:
```erlang
% Frame batching
send_ws_batch([Message1, Message2, Message3])
→ Reduces syscall overhead

% Compression (optional)
strict_delimiter_check => false,
compression => true
```

**Connection Health Monitoring**:
```erlang
% PING every 30 seconds
ping_interval = 30000,

% Client must respond with PONG
% Timeout → close with code 1011
```

**Browser Compatibility**:
```erlang
% Ensure CORS headers if browser client
{<<"Access-Control-Allow-Origin">>, <<"*">>},
{<<"Access-Control-Allow-Methods">>, <<"GET, POST, OPTIONS">>}
```

---

### 8.5 SSE Considerations

**Best For**:
- Server → Client push
- Unidirectional notifications
- Fallback for WebSocket-unsupported environments

**Combined with HTTP POST**:
```erlang
% Asymmetric transport
POST /mcp/messages    ← Client sends requests
GET /mcp/sse          ← Server sends notifications

% Client must maintain two connections
```

**Browser Compatibility**:
```javascript
// Client-side (browser)
const eventSource = new EventSource('/mcp/sse');
eventSource.addEventListener('message', (event) => {
    const message = JSON.parse(event.data);
});
```

**Reconnection Handling**:
```erlang
% Include Last-Event-ID on reconnect
GET /mcp/sse HTTP/1.1
Last-Event-ID: 42

% Server resends missed events (if stored)
```

---

## 9. Error Handling and Protocol Errors

### 9.1 JSON-RPC 2.0 Error Codes

**Standard Errors**:
```erlang
-define(JSONRPC_PARSE_ERROR, -32700).           % Invalid JSON
-define(JSONRPC_INVALID_REQUEST, -32600).       % Malformed request
-define(JSONRPC_METHOD_NOT_FOUND, -32601).      % Unknown method
-define(JSONRPC_INVALID_PARAMS, -32602).        % Invalid params
-define(JSONRPC_INTERNAL_ERROR, -32603).        % Server error
```

**Example Error Response**:
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "error": {
    "code": -32602,
    "message": "Invalid params",
    "data": {
      "details": "Missing required field 'uri'"
    }
  }
}
```

### 9.2 MCP-Specific Error Codes

**Resource Errors** (-32001 to -32030):
```erlang
-define(MCP_ERROR_RESOURCE_NOT_FOUND, -32001).
-define(MCP_ERROR_INVALID_URI, -32022).
```

**Tool Errors** (-32031 to -32040):
```erlang
-define(MCP_ERROR_TOOL_NOT_FOUND, -32002).
-define(MCP_ERROR_TOOL_EXECUTION_FAILED, -32031).
-define(MCP_ERROR_TOOL_TIMEOUT, -32032).
```

**Refusal Codes** (1001-1089):
```erlang
-define(ERR_REFUSED_CONTENT_POLICY, 1001).
-define(ERR_REFUSED_RATE_LIMIT, 1003).
```

### 9.3 Transport Error Handling

**TCP Connection Lost**:
```erlang
% Server-side
{tcp_closed, Socket} →
  Close handler, release connection slot

% Client-side
{error, econnrefused} →
  Retry with exponential backoff or fail
```

**Message Size Exceeded**:
```erlang
% Validation layer
Message size > 16 MB →
  Return error -32012 (MCP_ERROR_MESSAGE_TOO_LARGE)
  Close connection
```

---

## 10. Testing and Validation

### 10.1 Transport Validation

All transports must pass behavior compliance tests:

```erlang
%% Transport Behavior Validator
erlmcp_transport_validator:validate_transport(
    TransportType,
    Config
).

%% Checks:
✓ init/1 completes within 5 seconds
✓ send/2 handles data correctly
✓ close/1 releases resources
✓ get_info/1 returns valid map
✓ Accepts newline-delimited JSON
✓ Enforces 16 MB message size
✓ Handles backpressure
✓ Recovers from errors
```

### 10.2 Protocol Compliance Tests

```erlang
%% Lifecycle tests
✓ Initialize correctly
✓ Enforce phase transitions
✓ Reject pre-init requests
✓ Timeout on missing initialize

%% Capability negotiation tests
✓ Server advertises capabilities
✓ Client negotiates correctly
✓ Operations gated by capability

%% Message format tests
✓ Encode/decode JSON-RPC
✓ Handle batch requests
✓ Process notifications
✓ Validate error responses
```

---

## 11. Implementation Checklist

### Transport Implementation Checklist

```erlang
□ Define transport module (erlmcp_transport_*.erl)
□ Implement gen_server with start_link/1
□ Implement send/2 function
□ Implement close/1 function
□ Add get_info/1 (optional)
□ Add handle_transport_call/2 (optional)
□ Register with erlmcp_registry on init
□ Handle message framing (newline-delimited JSON)
□ Enforce 16 MB message size limit
□ Implement backpressure handling
□ Add comprehensive error handling
□ Include resource cleanup (monitors, timers)
□ Write unit tests (EUnit)
□ Write integration tests (Common Test)
□ Add to transport registry
□ Update documentation
```

### Server Implementation Checklist

```erlang
□ Start erlmcp_server with capabilities
□ Add resources, tools, prompts
□ Define handler functions
□ Implement initialize request handling
□ Enforce capability-based access
□ Handle subscriptions correctly
□ Send notifications to subscribers
□ Implement progress reporting
□ Add error handling for all methods
□ Write comprehensive tests
```

### Client Implementation Checklist

```erlang
□ Start erlmcp_client with transport
□ Call initialize/2 immediately
□ Check server capabilities before operations
□ Handle responses and errors
□ Implement subscription handlers
□ Add timeout for all requests
□ Clean up on shutdown
□ Handle reconnection (if applicable)
□ Write integration tests
```

---

## 12. References and Standards

- **JSON-RPC 2.0**: https://www.jsonrpc.org/specification
- **WebSocket (RFC 6455)**: https://tools.ietf.org/html/rfc6455
- **Server-Sent Events (RFC 8188)**: https://html.spec.whatwg.org/multipage/server-sent-events.html
- **MCP Specification**: https://modelcontextprotocol.io/
- **Erlang/OTP**: https://www.erlang.org/

---

## Document Metadata

- **Version**: 2.0
- **Last Updated**: 2026-01-31
- **Authors**: erlmcp contributors
- **Status**: Stable
- **MCP Protocol Version**: 2025-11-25

