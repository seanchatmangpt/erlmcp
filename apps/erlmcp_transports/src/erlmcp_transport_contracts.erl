%%%-------------------------------------------------------------------
%%% @doc
%%% MCP Transport Behavior Contracts - Complete Specification
%%%
%%% This module defines the complete behavior contracts for all MCP 2025-11-25
%%% transport types based on the official specification and erlmcp implementation
%%% requirements.
%%%
%%% == Transport Types Specification ==
%%%
%%% MCP 2025-11-25 defines 4 primary transport types:
%%% 1. STDIO (stdin/stdout) - Local process communication
%%% 2. TCP - Direct network connections
%%% 3. HTTP/SSE - Scalable web transport (bidirectional via POST + SSE)
%%% 4. WebSocket - Real-time bidirectional web transport
%%%
%%% == Universal Transport Requirements ==
%%%
%%% ALL transports MUST:
%%% - Implement `-behaviour(erlmcp_transport_behavior)` callbacks
%%% - Support JSON-RPC 2.0 message format
%%% - Enforce UTF-8 encoding
%%% - Implement 16MB default message size limit
%%% - Handle connection lifecycle (init → connected → disconnected)
%%% - Provide backpressure mechanisms
%%% - Support graceful shutdown
%%% - Register with erlmcp_registry on initialization
%%% - Route incoming messages via registry
%%%
%%% == Capability Negotiation ==
%%%
%%% Capabilities are negotiated during the `initialize` handshake:
%%% - Client sends: protocolVersion, capabilities, clientInfo
%%% - Server responds: protocolVersion, capabilities, serverInfo
%%% - Server sends notification: notifications/initialized
%%%
%%% Capabilities vary by transport:
%%% - STDIO: Limited concurrency, single stream
%%% - TCP: Multiple concurrent connections, connection pooling
%%% - HTTP/SSE: Stateless requests, server-push via SSE
%%% - WebSocket: Real-time bidirectional, subprotocol negotiation
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_transport_contracts).

-include("erlmcp.hrl").

%%====================================================================
%% STDIO Transport Contract
%%====================================================================

%%% == STDIO Transport Specification ==
%%%
%%% **Purpose**: Local process communication via stdin/stdout pipes
%%%
%%% **Use Cases**:
%%% - CLI tools and command-line MCP servers
%%% - Local process spawning (e.g., spawn external MCP server)
%%% - Development and testing
%%%
%%% **Message Framing**: Line-delimited JSON
%%% - Each message is a complete JSON object on a single line
%%% - Messages terminated by newline (`\n`, `\r\n`, or `\r`)
%%% - Empty lines are ignored
%%%
%%% **Encoding**: UTF-8 text only
%%%
%%% **Connection Semantics**:
%%% - Connection established when process starts
%%% - EOF on stdin signals disconnection
%%% - Process termination closes connection
%%% - No explicit connect/disconnect protocol
%%%
%%% **Concurrency**: Single stream (sequential message processing)
%%% - Messages processed in order received
%%% - No parallel request handling within single connection
%%%
%%% **Backpressure**:
%%% - Limited (queue-based buffering)
%%% - Blocks on write if stdout buffer full
%%% - Reader process monitors stdin availability
%%%
%%% **Flow Control**:
%%% - No explicit flow control protocol
%%% - Relies on OS pipe buffering
%%% - Buffer size: Configurable (default 64KB)
%%%
%%% **Initialization**:
%%% ```erlang
%%% Config = #{
%%%     transport_id => my_stdio_transport,
%%%     owner => self(),                    % Process receiving messages
%%%     max_message_size => 16777216,       % 16MB default
%%%     test_mode => false                  % Set true for testing
%%% }
%%% ```
%%%
%%% **Message Flow**:
%%% ```
%%% stdin → read_loop → validate_size → trim_line → {transport_message, Binary}
%%% {send, Binary} → stdout
%%% EOF → {transport_disconnected, Reason}
%%% ```
%%%
%%% **Error Handling**:
%%% - Message size exceeded: Send JSON-RPC error -32012, continue reading
%%% - Invalid UTF-8: Log warning, skip message
%%% - EOF: Graceful shutdown
%%% - Read error: Log error, stop reader process
%%%
%%% **State Management**:
%%% - owner: Pid receiving transport messages
%%% - owner_monitor: Reference to monitor owner process
%%% - reader: Pid of background stdin reader process
%%% - buffer: Incomplete line buffer (if needed)
%%% - test_mode: Boolean for test environment detection
%%% - max_message_size: Size limit in bytes
%%%
%%% **Implementation Requirements**:
%%% - Spawn reader process on init (unless test_mode)
%%% - Monitor owner process for termination
%%% - Register with erlmcp_registry using transport_id
%%% - Validate message size BEFORE processing
%%% - Send messages to owner as `{transport_message, Binary}`
%%% - Unregister from registry on termination

-type stdio_config() ::
    #{transport_id := atom(),
      owner := pid(),
      max_message_size => pos_integer(),
      test_mode => boolean()}.
-type stdio_state() ::
    #{owner := pid(),
      owner_monitor := reference() | undefined,
      reader := pid() | undefined,
      buffer := binary(),
      test_mode := boolean(),
      max_message_size := pos_integer(),
      transport_id := atom()}.

%%====================================================================
%% TCP Transport Contract
%%====================================================================

%%% == TCP Transport Specification ==
%%%
%%% **Purpose**: Direct network connections with multiplexing
%%%
%%% **Use Cases**:
%%% - High-performance server-to-server communication
%%% - Internal microservices (bypassing HTTP overhead)
%%% - Connection pooling for load distribution
%%% - Direct client-server connections
%%%
%%% **Message Framing**: Length-prefixed or line-delimited
%%% - Line-delimited: Each message terminated by `\n`
%%% - Compatible with stdio framing for simplicity
%%% - Full JSON object per message
%%%
%%% **Encoding**: UTF-8 binary
%%%
%%% **Modes**:
%%% - **Server Mode**: Ranch-based acceptor pool
%%%   - Configurable number of acceptors (default: 10)
%%%   - Connection limiting (default: 1024 max connections)
%%%   - One gen_server per accepted connection
%%%   - Supervised connection handlers
%%%
%%% - **Client Mode**: Outbound connections
%%%   - Automatic reconnection with exponential backoff
%%%   - Configurable retry attempts (default: infinity)
%%%   - Connection timeout (default: 5 seconds)
%%%
%%% **Connection Semantics**:
%%% - Server: Accept connections on bind(host, port)
%%% - Client: Connect to connect(host, port)
%%% - Keepalive: Optional TCP keepalive
%%% - Nodelay: Optional TCP_NODELAY (disable Nagle)
%%% - Connection established: `{transport_connected, Pid}`
%%% - Connection lost: `{transport_disconnected, Pid, Reason}`
%%%
%%% **Multiplexing**: YES
%%% - Multiple concurrent connections per server
%%% - Each connection is independent gen_server
%%% - Connection pooling support via erlmcp_pool_manager
%%%
%%% **Backpressure**: TCP window-based
%%% - OS-level TCP flow control
%%% - gen_tcp:send blocks when send buffer full
%%% - `{active, true}` mode with monitoring
%%% - Configurable buffer sizes (recbuf, sndbuf)
%%%
%%% **Flow Control**:
%%% - TCP window size negotiation
%%% - Send buffer: Configurable (default 64KB)
%%% - Receive buffer: Configurable (default 64KB)
%%% - Message size validation: 16MB limit enforced
%%% - Memory guard integration: Rejects messages if system memory low
%%%
%%% **Initialization (Server)**:
%%% ```erlang
%%% Config = #{
%%%     mode => server,
%%%     transport_id => my_tcp_server,
%%%     server_id => server_1,
%%%     port => 8888,
%%%     owner => self(),
%%%     num_acceptors => 10,
%%%     max_connections => 1024,
%%%     keepalive => true,
%%%     nodelay => true,
%%%     buffer_size => 65536
%%% }
%%% ```
%%%
%%% **Initialization (Client)**:
%%% ```erlang
%%% Config = #{
%%%     mode => client,
%%%     transport_id => my_tcp_client,
%%%     host => "localhost",
%%%     port => 8888,
%%%     owner => self(),
%%%     connect_timeout => 5000,
%%%     max_reconnect_attempts => infinity,
%%%     keepalive => true,
%%%     nodelay => true
%%% }
%%% ```
%%%
%%% **Message Flow (Server)**:
%%% ```
%%% ranch:accept → handshake → {active, true} → {tcp, Socket, Data}
%%% → validate_size → extract_messages → {transport_message, Binary}
%%% {tcp_closed, Socket} → {stop, normal}
%%% ```
%%%
%%% **Message Flow (Client)**:
%%% ```
%%% gen_tcp:connect → {active, true} → {transport_connected, Pid}
%%% {tcp, Socket, Data} → {transport_message, Binary}
%%% {tcp_closed, Socket} → reconnect_timer → retry_connection
%%% ```
%%%
%%% **Error Handling**:
%%% - Message size exceeded: Send JSON-RPC -32012, close connection
%%% - Memory exhausted: Send JSON-RPC -32603, close connection
%%% - Connection limit: Reject with refusal code 1001
%%% - TCP errors: Log and trigger reconnection (client) or stop (server)
%%%
%%% **Connection Management**:
%%% - Idle timeout: 5 minutes (configurable)
%%% - Resource monitoring: Check memory every 60 seconds
%%% - Connection lease: 30 second timeout for handler init
%%% - Cleanup guarantees: Release connection slot on ALL termination paths
%%%
%%% **State Management**:
%%% - mode: client | server
%%% - socket: gen_tcp socket
%%% - connected: Boolean connection status
%%% - buffer: Incomplete message buffer
%%% - reconnect_timer: Reference for client reconnection
%%% - reconnect_attempts: Counter for exponential backoff
%%% - bytes_sent/bytes_received: Statistics
%%% - idle_timer: Reference for idle timeout
%%% - resource_monitor_timer: Reference for resource checks

-type tcp_mode() :: client | server.
-type tcp_config() ::
    #{mode := tcp_mode(),
      transport_id := atom(),
      host => inet:hostname() | inet:ip_address(),
      port => inet:port_number(),
      owner => pid(),
      server_id => atom(),
      num_acceptors => pos_integer(),
      max_connections => pos_integer() | infinity,
      connect_timeout => timeout(),
      max_reconnect_attempts => pos_integer() | infinity,
      keepalive => boolean(),
      nodelay => boolean(),
      buffer_size => pos_integer(),
      use_pool => boolean(),
      pool_name => atom(),
      pool_min_size => pos_integer(),
      pool_max_size => pos_integer(),
      pool_strategy => round_robin | least_loaded | random}.

                                                  % Required for client
                   % Required for server/client

                                                  % Server only
  % Server only

                                                  % Client only
  % Client only

    % Pool configuration

-type tcp_state() ::
    #{mode := tcp_mode(),
      transport_id := atom(),
      server_id => atom(),
      socket => gen_tcp:socket() | undefined,
      ranch_ref => ranch:ref() | undefined,
      owner := pid(),
      host => inet:hostname(),
      port => inet:port_number(),
      connected := boolean(),
      buffer := binary(),
      reconnect_timer => reference() | undefined,
      reconnect_attempts := non_neg_integer(),
      max_reconnect_attempts => pos_integer() | infinity,
      bytes_sent := non_neg_integer(),
      bytes_received := non_neg_integer(),
      idle_timer => reference() | undefined,
      resource_monitor_timer => reference() | undefined,
      last_activity := integer(),
      max_message_size := pos_integer(),
      initialized => boolean()}.

                                           % Server mode only

                                            % Client mode

                              % Server: tracks successful init

%%====================================================================
%% HTTP/SSE Transport Contract
%%====================================================================

%%% == HTTP/SSE Transport Specification ==
%%%
%%% **Purpose**: Scalable web transport with bidirectional communication
%%%
%%% **Use Cases**:
%%% - Web applications accessing MCP servers
%%% - Firewall-friendly deployments
%%% - Load-balanced server architectures
%%% - Browser-based clients (via fetch + EventSource)
%%%
%%% **Bidirectional Communication**:
%%% - **Client → Server**: HTTP POST requests with JSON body
%%% - **Server → Client**: Server-Sent Events (SSE) stream
%%%
%%% **Message Framing**:
%%% - HTTP POST: Complete JSON in request body
%%% - SSE: `data: {JSON}\n\n` format per event
%%%
%%% **Encoding**: UTF-8, Content-Type: application/json
%%%
%%% **Connection Semantics**:
%%% - HTTP: Stateless, each request independent
%%% - SSE: Long-lived connection for server-push notifications
%%% - Session tracking: Via session ID in headers or query params
%%%
%%% **Multiplexing**: YES
%%% - Concurrent HTTP requests supported
%%% - Multiple SSE connections per client
%%% - Connection pooling for HTTP client requests
%%%
%%% **Backpressure**:
%%% - HTTP: Synchronous request/response (natural backpressure)
%%% - SSE: Event buffer with size limits, client reconnect on overflow
%%%
%%% **Flow Control**:
%%% - HTTP: Rate limiting via erlmcp_rate_limiter
%%% - SSE: Event queue depth monitoring
%%% - Circuit breaker for overload protection
%%%
%%% **HTTP Transport (Client)**:
%%% ```erlang
%%% Config = #{
%%%     transport_id => my_http_client,
%%%     url => <<"https://mcp-server.example.com/mcp">>,
%%%     owner => self(),
%%%     method => post,
%%%     headers => [{<<"Authorization">>, <<"Bearer token">>}],
%%%     timeout => 30000,
%%%     connect_timeout => 5000,
%%%     max_retries => 3,
%%%     retry_delay => 1000,
%%%     ssl_options => [{verify, verify_peer}],
%%%     pool_size => 10
%%% }
%%% ```
%%%
%%% **SSE Transport (Server)**:
%%% ```erlang
%%% Config = #{
%%%     transport_id => my_sse_server,
%%%     port => 8080,
%%%     path => "/mcp/events",
%%%     owner => self(),
%%%     max_connections => 10000,
%%%     event_buffer_size => 1000,
%%%     keepalive_interval => 30000,
%%%     reconnect_timeout => 3000
%%% }
%%% ```
%%%
%%% **Message Flow (HTTP POST)**:
%%% ```
%%% Client → HTTP POST /mcp
%%% → Parse JSON body
%%% → Route to handler
%%% → Generate response
%%% → HTTP 200 with JSON body
%%% ```
%%%
%%% **Message Flow (SSE)**:
%%% ```
%%% Client → HTTP GET /mcp/events → Accept: text/event-stream
%%% Server → HTTP 200, Content-Type: text/event-stream
%%% Server → data: {JSON}\n\n (repeating)
%%% Connection closed → Client reconnects with Last-Event-ID
%%% ```
%%%
%%% **Error Handling**:
%%% - HTTP 4xx: Client error (malformed request)
%%% - HTTP 5xx: Server error (processing failure)
%%% - SSE disconnect: Client auto-reconnects
%%% - Timeout: Configurable per-request timeout
%%%
%%% **Headers**:
%%% - Content-Type: application/json (POST)
%%% - Accept: text/event-stream (SSE)
%%% - Authorization: Bearer, API-Key, etc.
%%% - X-Request-ID: Request correlation
%%%
%%% **State Management**:
%%% - gun_pid: HTTP/2 connection process (gun library)
%%% - stream_ref: Reference for request tracking
%%% - pending_requests: Map of request_id → callback
%%% - sse_connections: Map of session_id → SSE stream

-type http_method() :: get | post | put | delete.
-type http_config() ::
    #{transport_id := atom(),
      url := binary() | string(),
      owner := pid(),
      method => http_method(),
      headers => [{binary(), binary()}],
      timeout => timeout(),
      connect_timeout => timeout(),
      max_retries => non_neg_integer(),
      retry_delay => pos_integer(),
      ssl_options => [ssl:tls_client_option()],
      pool_size => non_neg_integer()}.
-type sse_config() ::
    #{transport_id := atom(),
      port := inet:port_number(),
      path => binary(),
      owner := pid(),
      max_connections => pos_integer(),
      event_buffer_size => pos_integer(),
      keepalive_interval => pos_integer(),
      reconnect_timeout => timeout()}.
-type http_state() ::
    #{transport_id := atom(),
      gun_pid => pid() | undefined,
      stream_ref => reference() | undefined,
      url := binary(),
      method := http_method(),
      headers := [{binary(), binary()}],
      pending_requests := #{term() => term()},
      max_message_size := pos_integer()}.
-type sse_state() ::
    #{transport_id := atom(),
      cowboy_pid => pid() | undefined,
      sse_connections := #{binary() => pid()},
      event_buffer_size := pos_integer(),
      keepalive_interval := pos_integer()}.

%%====================================================================
%% WebSocket Transport Contract
%%====================================================================

%%% == WebSocket Transport Specification ==
%%%
%%% **Purpose**: Real-time bidirectional web transport
%%%
%%% **Use Cases**:
%%% - Real-time interactive applications
%%% - Low-latency command/control
%%% - Browser-based MCP clients
%%% - Streaming data processing
%%%
%%% **Subprotocol**: `mcp.v1`
%%% - Negotiated during WebSocket handshake
%%% - Sec-WebSocket-Protocol: mcp.v1
%%%
%%% **Message Framing**:
%%% - Frame Type: Text frames (not binary)
%%% - Each frame contains one or more line-delimited JSON messages
%%% - Message delimiter: `\n`
%%% - Fragmentation: Automatic reassembly of fragmented frames
%%%
%%% **Encoding**: UTF-8 text
%%%
%%% **Connection Semantics**:
%%% - Upgrade: HTTP → WebSocket via upgrade handshake
%%% - Path: Configurable (e.g., `/mcp/ws`)
%%% - Connection established: After upgrade handshake
%%% - Keepalive: Ping/pong frames every 30 seconds
%%% - Idle timeout: 5 minutes (configurable)
%%%
%%% **Concurrency**: Single logical stream per connection
%%% - Messages processed sequentially within connection
%%% - Multiple messages can be in-flight concurrently
%%% - Server can handle multiple WebSocket connections
%%%
%%% **Backpressure**:
%%% - Frame buffer monitoring (default 100KB)
%%% - Backpressure activated at buffer limit
%%% - Resume at 50% drain threshold
%%% - Timeout: 5 seconds for backpressure resolution
%%%
%%% **Flow Control**:
%%% - frame_buffer_size: Maximum buffered data (default 100KB)
%%% - frame_buffer_used: Current buffer usage
%%% - backpressure_state: inactive | active
%%% - messages_pending: Count of queued messages
%%% - bytes_buffered: Total buffered bytes
%%%
%%% **WebSocket Close Codes** (RFC 6455):
%%% - 1000: Normal closure
%%% - 1001: Going away
%%% - 1002: Protocol error
%%% - 1003: Unsupported data (binary frames)
%%% - 1009: Message too big (>16MB)
%%% - 1011: Internal server error
%%%
%%% **Initialization**:
%%% ```erlang
%%% Config = #{
%%%     transport_id => my_ws_transport,
%%%     port => 8080,
%%%     path => "/mcp/ws",
%%%     max_message_size => 16777216,
%%%     strict_delimiter_check => true,
%%%     validate_utf8 => true,
%%%     max_connections => 1000,
%%%     connect_timeout => 5000,
%%%     frame_buffer_size => 102400
%%% }
%%% ```
%%%
%%% **Message Flow**:
%%% ```
%%% HTTP Upgrade → WebSocket Handshake
%%% → websocket_init (create state)
%%% → Start ping timer
%%% → {text, Data} frames
%%% → Validate UTF-8, size, backpressure
%%% → Extract messages (line-delimited)
%%% → Route to registry
%%% → {send_frame, Data} for outbound
%%% → Close frame → terminate
%%% ```
%%%
%%% **Ping/Pong Protocol**:
%%% - Server sends ping every 30 seconds
%%% - Client must respond with pong
%%% - Server can receive unsolicited pings (respond with pong)
%%% - Pong frames indicate connection alive
%%%
%%% **Fragment Handling**:
%%% - Continuation frames reassembled automatically
%%% - Fragment timeout: 30 seconds
%%% - Fragment buffer cleaned on timeout
%%%
%%% **Error Handling**:
%%% - Binary frames: Close with 1003 (unsupported)
%%% - Message too big: Close with 1009
%%% - Invalid UTF-8: Close with 1002 (protocol error)
%%% - Backpressure timeout: Close with 1011 (internal error)
%%% - Fragment timeout: Close with 1002 (protocol error)
%%%
%%% **State Management**:
%%% - transport_id: Unique identifier
%%% - registry_pid: Registry process for message routing
%%% - session_id: Unique session identifier
%%% - ping_timer: Reference for periodic ping
%%% - fragment_buffer: Buffer for incomplete frames
%%% - fragment_start_time: Timestamp for timeout detection
%%% - max_message_size: Size limit (16MB)
%%% - strict_delimiter_check: Enforce newline delimiter
%%% - validate_utf8: Validate UTF-8 encoding
%%% - frame_buffer_size: Max buffer size
%%% - frame_buffer_used: Current usage
%%% - backpressure_state: Flow control state
%%% - Statistics: messages_received/sent, bytes_received/sent, ping/pong counts

-type ws_config() ::
    #{transport_id := atom(),
      port := inet:port_number(),
      path => binary(),
      max_message_size => pos_integer(),
      strict_delimiter_check => boolean(),
      validate_utf8 => boolean(),
      max_connections => pos_integer(),
      connect_timeout => timeout(),
      frame_buffer_size => pos_integer()}.
-type ws_backpressure_state() :: inactive | active.
-type ws_state() ::
    #{transport_id := binary(),
      registry_pid := pid(),
      connection_info := map(),
      session_id := binary(),
      ping_timer => reference() | undefined,
      fragment_buffer => binary() | undefined,
      fragment_start_time => integer() | undefined,
      max_message_size := integer(),
      strict_delimiter_check := boolean(),
      validate_utf8 := boolean(),
      frame_buffer_size := integer(),
      frame_buffer_used := integer(),
      backpressure_state := ws_backpressure_state(),
      backpressure_timer => reference() | undefined,
      messages_pending := non_neg_integer(),
      bytes_buffered := non_neg_integer(),
      messages_received := non_neg_integer(),
      messages_sent := non_neg_integer(),
      bytes_received := non_neg_integer(),
      bytes_sent := non_neg_integer(),
      ping_count := non_neg_integer(),
      pong_count := non_neg_integer(),
      connection_start_time := integer()}.

%%====================================================================
%% Universal Transport Behavior Callbacks
%%====================================================================

%%% All transport modules MUST implement these callbacks:
%%%
%%% ```erlang
%%% -callback init(Config :: map()) ->
%%%     {ok, State :: term()} |
%%%     {error, Reason :: term()}.
%%% ```
%%% Initialize transport with configuration. Must:
%%% - Validate configuration
%%% - Initialize transport-specific resources
%%% - Register with erlmcp_registry
%%% - Set up connection state
%%% - Return initialized state
%%%
%%% ```erlang
%%% -callback send(State :: term(), Data :: binary()) ->
%%%     ok |
%%%     {error, Reason :: term()}.
%%% ```
%%% Send message through transport. Must:
%%% - Encode/frame message appropriately
%%% - Handle transport-specific transmission
%%% - Manage connection state
%%% - Handle backpressure
%%% - Return ok or descriptive error
%%%
%%% ```erlang
%%% -callback close(State :: term()) -> ok.
%%% ```
%%% Close transport and clean up. Must:
%%% - Close connections/sockets
%%% - Clean up resources (timers, processes, files)
%%% - Unregister from registry
%%% - Always return ok (never fails)
%%%
%%% ```erlang
%%% -callback get_info(State :: term()) ->
%%%     #{atom() => term()}.
%%% ```
%%% Get transport information (optional). Should return:
%%% - transport_id: atom()
%%% - type: stdio | tcp | http | websocket
%%% - status: running | connecting | connected | disconnected | error
%%% - statistics: messages/bytes sent/received
%%% - Transport-specific fields
%%%
%%% ```erlang
%%% -callback handle_transport_call(Request :: term(), State :: term()) ->
%%%     {reply, Reply :: term(), NewState :: term()} |
%%%     {error, Reason :: term()}.
%%% ```
%%% Handle transport-specific calls (optional). Useful for:
%%% - Reconnection requests
%%% - Configuration updates
%%% - Diagnostics
%%% - Feature toggles

%%====================================================================
%% Message Protocol Requirements
%%====================================================================

%%% == JSON-RPC 2.0 Message Format ==
%%%
%%% All transports MUST support JSON-RPC 2.0 message format:
%%%
%%% **Request** (expects response):
%%% ```json
%%% {
%%%   "jsonrpc": "2.0",
%%%   "id": 1,
%%%   "method": "initialize",
%%%   "params": {}
%%% }
%%% ```
%%%
%%% **Response** (success):
%%% ```json
%%% {
%%%   "jsonrpc": "2.0",
%%%   "id": 1,
%%%   "result": {}
%%% }
%%% ```
%%%
%%% **Response** (error):
%%% ```json
%%% {
%%%   "jsonrpc": "2.0",
%%%   "id": 1,
%%%   "error": {
%%%     "code": -32600,
%%%     "message": "Invalid Request"
%%%   }
%%% }
%%% ```
%%%
%%% **Notification** (no response):
%%% ```json
%%% {
%%%   "jsonrpc": "2.0",
%%%   "method": "resources/updated",
%%%   "params": {}
%%% }
%%% ```
%%%
%%% == Initialization Protocol ==
%%%
%%% MUST support initialization sequence:
%%% 1. Client → Server: `initialize` request with protocolVersion, capabilities
%%% 2. Server → Client: `initialize` response with capabilities, serverInfo
%%% 3. Server → Client: `notifications/initialized` notification
%%% 4. Connection in INITIALIZED state, ready for requests
%%%
%%% == Message Size Limits ==
%%%
%%% Default: 16 MB (16777216 bytes)
%%% Configurable per transport via `max_message_size` config
%%% Enforcement: Validate BEFORE processing
%%% Violation: Send JSON-RPC error -32012 (MESSAGE_TOO_LARGE)

%%====================================================================
%% Capability Negotiation Per Transport
%%====================================================================

%%% == Transport-Specific Capability Constraints ==
%%%
%%% **STDIO**:
%%% - Limited to single stream
%%% - Sequential message processing
%%% - No parallel request handling
%%% - Suitable for: tools, resources, prompts (basic capabilities)
%%%
%%% **TCP**:
%%% - Multiple concurrent connections
%%% - Connection pooling supported
%%% - Full capability support
%%% - Suitable for: All MCP capabilities
%%%
%%% **HTTP/SSE**:
%%% - Stateless requests (HTTP POST)
%%% - Server-push notifications (SSE)
%%% - Full capability support
%%% - Firewall-friendly
%%% - Suitable for: Web applications, all capabilities
%%%
%%% **WebSocket**:
%%% - Real-time bidirectional
%%% - Low latency
%%% - Full capability support
%%% - Browser-compatible
%%% - Suitable for: Interactive applications, all capabilities

%%====================================================================
%% Performance Requirements
%%====================================================================

%%% == Throughput Targets ==
%%%
%%% **STDIO**: ~10K messages/sec (limited by pipe I/O)
%%% **TCP**: ~43K messages/sec (real sockets, 4KB packets)
%%% **HTTP**: ~20K messages/sec (stateless overhead)
%%% **WebSocket**: ~30K messages/sec (text frames)
%%%
%%% == Latency Targets ==
%%%
%%% **p50**: < 100 µs (in-memory processing)
%%% **p95**: < 500 µs
%%% **p99**: < 1000 µs (1ms)
%%% **p99.9**: < 5000 µs (5ms)
%%%
%%% == Connection Scalability ==
%%%
%%% **Single Node**: 40-50K concurrent connections
%%% **Cluster** (Mnesia): 100K+ connections
%%% **Memory per Connection**: 2-5 MB (varies by session state)

%%====================================================================
%% Security Requirements
%%====================================================================

%%% All transports MUST:
%%% - Validate input (JSON Schema, size, encoding)
%%% - Prevent path traversal in resource URIs
%%% - Enforce rate limiting (via erlmcp_rate_limiter)
%%% - Support authentication (API key, JWT, mTLS)
%%% - Protect against DoS (message size, connection limits)
%%%
%%% Transport-specific:
%%% - **TCP**: Optional TLS encryption
%%% - **HTTP**: TLS/SSL required for production
%%% - **WebSocket**: WSS (WebSocket Secure) for browser clients
%%% - **STDIO**: Local only, inherits process permissions

%%====================================================================
%% Testing Requirements
%%====================================================================

%%% All transport implementations MUST include:
%%% - Connection lifecycle tests (connect, disconnect, reconnect)
%%% - Round-trip message tests (send request, receive response)
%%% - Error handling tests (invalid messages, size limits)
%%% - Backpressure tests (buffer overflow, flow control)
%%% - Performance benchmarks (latency p50/p95/p99, throughput)
%%% - Coverage ≥ 82% (mandatory quality gate)

%%====================================================================
%% Export Type Specifications
%%====================================================================

-export_type([stdio_config/0, stdio_state/0, tcp_mode/0, tcp_config/0, tcp_state/0, http_method/0,
              http_config/0, http_state/0, sse_config/0, sse_state/0, ws_config/0, ws_state/0,
              ws_backpressure_state/0]).

                                            % STDIO

    % TCP

    % HTTP/SSE

    % WebSocket

%%====================================================================
%% Documentation Functions (Non-executable)
%%====================================================================

%%% This module is purely documentary and does not export executable functions.
%%% It serves as the canonical specification for all MCP transport implementations.
%%%
%%% For implementation examples, see:
%%% - erlmcp_transport_stdio.erl
%%% - erlmcp_transport_tcp.erl
%%% - erlmcp_transport_http.erl
%%% - erlmcp_transport_ws.erl
