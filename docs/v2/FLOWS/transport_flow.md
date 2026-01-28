# Transport Message Flow Diagrams

**Derived from**: `/Users/sac/erlmcp/src/erlmcp_transport_*.erl` implementation analysis
**Date**: 2026-01-27

---

## STDIO Transport Flow

### Initialization
```
erlmcp_transport_sup:start_child(stdio, test_transport, Config)
  └─> transport_module(stdio) → erlmcp_transport_stdio
  └─> erlmcp_transport_stdio:start_link(test_transport, Config)
        └─> gen_server:start_link(erlmcp_transport_stdio, [...], [])
              └─> init([Owner]):
                    ├─ trap_exit ✓
                    ├─ check test_mode (flag in process dict or env)
                    ├─ MaxMessageSize = get_max_message_size() [16MB default]
                    └─ test_mode=false:
                          └─ spawn_link(read_loop) → stdin reader process
```

### Message Send
```
Client calls: erlmcp_transport_stdio:send(Pid, <<"JSON message">>)
  └─> io:format("~s~n", [Message])
        ├─ Message is pre-JSON-encoded
        ├─ Add newline for framing
        └─> stdout (to parent process/stdio sink)
```

### Message Receive (non-test mode)
```
read_loop(ReaderPid, Owner, MaxSize):
  Loop:
    ├─ io:read_line() from stdin
    ├─ validate_message_size(Line, MaxSize)
    ├─ extract_message_lines(Buffer, NewData)
    ├─ trim_message_line(Line)
    └─> Owner ! {transport_data, Line}
```

### Cleanup
```
erlmcp_transport_sup:stop_transport(stdio)
  └─> erlmcp_transport_stdio:close(Pid)
        └─> gen_server:stop(Pid)
              └─> terminate(Reason, State):
                    ├─ reader ! 'EXIT'
                    └─ Reader process cleanup
```

---

## TCP Transport Flow

### Dual-Mode Architecture

```
erlmcp_transport_tcp:start_link(TransportId, Config)
  └─> Mode = maps:get(mode, Config, client)
        ├─ mode = server:
        │   └─> init_server(Opts)
        │         ├─ ranch:start_listener(TcpServer, Opts)
        │         ├─ NumAcceptors, MaxConnections
        │         └─ Protocol = erlmcp_transport_tcp (ranch_protocol)
        │
        └─ mode = client:
            └─> init_client(Opts)
                  ├─ Host, Port from Config
                  ├─ gen_tcp:connect(Host, Port, Options)
                  └─ Schedule reconnection on failure
```

### Client Mode: Connection with Reconnection

```
init_client(Opts) → gen_server State
  │
  Loop on handle_info(reconnect_timeout, State):
    ├─ gen_tcp:connect(Host, Port, [nodelay, keepalive, ...])
    │
    ├─ Success:
    │   ├─ State#state{socket=Socket, connected=true, reconnect_attempts=0}
    │   └─> Owner ! {transport_connected, #{host=>Host, port=>Port}}
    │
    └─ Failure:
        ├─ reconnect_attempts >= max_reconnect_attempts
        │   └─> STOP
        ├─ Otherwise:
        │   ├─ NextDelay = min(PrevDelay * 1.5, 60s) [exponential backoff]
        │   ├─ erlang:send_after(NextDelay, self(), reconnect_timeout)
        │   └─> State#state{reconnect_timer=TRef, reconnect_attempts=N+1}
        └─> Owner ! {transport_error, connect_failed, Reason}
```

### Server Mode: Ranch Protocol Handler

```
ranch:start_listener (Supervision)
  │
  Incoming TCP connection
  └─> erlmcp_transport_tcp:start_link(Socket, _Transport, _Opts) [ranch callback]
        ├─ gen_server init([Socket, ranch])
        ├─ Set to active socket: inet:setopts(Socket, [{active, once}])
        └─> State#state{socket=Socket, mode=server, connected=true}
```

### Message Send
```
erlmcp_transport_tcp:send(State, Data) when connected=true
  └─> gen_tcp:send(Socket, [Data, <<"\n">>])
        ├─ iolist encoding [Data | "\n"]
        ├─ Zero-copy optimization
        └─> Return ok | {error, Reason}
```

### Message Receive
```
handle_info({tcp, Socket, Bytes}, State)
  ├─ Buffer = State#state.buffer
  ├─ NewBuffer = <<Buffer/binary, Bytes/binary>>
  ├─ extract_message_lines(NewBuffer, [])
  │   └─> Returns {[Line1, Line2, ...], RemainingBuffer}
  ├─ [For each Line]:
  │   └─> Owner ! {transport_data, trim_message_line(Line)}
  └─> inet:setopts(Socket, [{active, once}])  [Resume reading]
```

### Graceful Shutdown
```
erlmcp_transport_tcp:close(State)
  ├─ case State#state.socket of
  │   undefined → ok
  │   Socket → gen_tcp:close(Socket)
  └─> State#state{socket=undefined, connected=false}
```

---

## HTTP Transport Flow (Complicated)

### Module Structure
```
APPLICATION CODE
  │
  └─> erlmcp_transport_http (52 LOC, interface)
        ├─> init(Config) → erlmcp_transport_http_server:start_link(Config)
        ├─> send(Pid, Data) → gen_server:call(Pid, {send, Data})
        └─> close(Pid) → gen_server:stop(Pid)
              │
              └─> erlmcp_transport_http_server (634 LOC, main impl)
                    │
                    ├─ gen_server managing gun HTTP client connection
                    ├─ Queue for pending requests
                    ├─ Retry logic with exponential backoff
                    └─ Pool-based connection management
```

### Startup
```
erlmcp_transport_http:init(#{url := Url, owner := Owner, ...})
  └─> erlmcp_transport_http_server:start_link(Config)
        └─> gen_server:start_link(erlmcp_transport_http_server, [...], [])
              └─> init([Config]):
                    ├─ parse_url(Url) → {Scheme, Host, Port, Path}
                    ├─ gun:open(Host, Port, GunOpts) → {ok, GunPid}
                    ├─ State#state{
                    │     gun_pid = GunPid,
                    │     pending_requests = #{},
                    │     message_queue = queue:new(),
                    │     pool_size = PoolSize,
                    │     active_requests = 0
                    │   }
                    └─> Monitor gun process: monitor(process, GunPid)
```

### Message Send (via HTTP POST)
```
erlmcp_transport_http:send(Pid, Data)
  └─> gen_server:call(Pid, {send, Data}, Timeout)
        └─> handle_call({send, Data}, From, State):
              ├─ Enqueue: queue:in({Data, From}, MessageQueue)
              ├─ process_queue(State#state{message_queue=NewQueue})
              │   └─> Loop while queue not empty:
              │         ├─ queue:out(MessageQueue) → {Data, RestQueue}
              │         ├─ StreamRef = gun:post(GunPid, Path, Headers, Data)
              │         ├─ Track: pending_requests[StreamRef] = {From, Timeout, Data, Retries}
              │         ├─ active_requests += 1
              │         └─> Continue loop
              └─> {noreply, NewState}
```

### HTTP Response Handling
```
handle_info({gun_response, GunPid, StreamRef, nofin, Status, Headers}, State)
  └─> Track in pending_requests[StreamRef]
        ├─ Status = 200..299
        │   └─> gen_server:reply(From, ok)
        │       └─> Caller receives ok
        │
        └─ Status = 429 | 500..599 (retryable)
            ├─ Retries < MaxRetries?
            │   ├─ Yes: Re-enqueue, schedule retry with backoff
            │   └─ No: Reply {error, max_retries_exceeded}
            │
            └─ Other error status: Reply {error, Status}
```

### Error Handling
```
handle_info({gun_error, GunPid, StreamRef, Reason}, State)
  ├─ If StreamRef in pending_requests:
  │   ├─ Retries < MaxRetries?
  │   │   ├─ Yes: Re-enqueue with backoff delay
  │   │   └─ No: Reply {error, Reason} to From
  │   └─ Delete from pending_requests
  │
  └─ If StreamRef = undefined (connection error):
        ├─ All pending requests marked for retry
        ├─ gun:close(GunPid)
        ├─ Schedule reconnection
        └─> Reconnect via gun:open(Host, Port)
```

---

## Message Flow: Transport → Registry → Server

### Complete Request-Response Cycle

```
┌─────────────────────────────────────────────────────────────┐
│ Network Message Arrives at Transport                        │
└────────────────┬────────────────────────────────────────────┘
                 │
                 ├─> erlmcp_transport_*:handle_info(...)
                 │     ├─ Extract message data
                 │     ├─ Validate/frame message
                 │     └─> Owner ! {transport_data, RawData}
                 │
                 ├─> erlmcp_transport_behavior:handle_transport_message(
                 │     TransportId, RawData)
                 │     └─> erlmcp_registry:route_message(TransportId, RawData)
                 │           └─> Deserialize JSON-RPC
                 │           └─> Find handler (client/server)
                 │           └─> Dispatch to handler process
                 │
                 ├─> Server/Client processes message
                 │     ├─ Parse JSON-RPC request
                 │     ├─ Execute RPC method
                 │     └─> Generate response message
                 │
                 ├─> Send response back
                 │     └─> erlmcp_transport_tcp:send(State, ResponseData)
                 │           └─> gen_tcp:send(Socket, [ResponseData, "\n"])
                 │                 └─> Transmit to network
                 │
                 └─> Response received by remote client

┌─────────────────────────────────────────────────────────────┐
│ Actual Integration Status:                                  │
│ ❌ handle_transport_message → registry NOT WIRED           │
│ ❌ Auto-registration in registry BROKEN                    │
│ ✅ send() function works                                    │
│ ✅ Message receipt parsing works                            │
└─────────────────────────────────────────────────────────────┘
```

---

## State Machine: TCP Transport Connection Lifecycle

```
    ┌─────────────┐
    │  INIT       │
    │  mode=client│
    └──────┬──────┘
           │
           ├─ gen_tcp:connect(Host, Port)
           │   ├─ Success
           │   │  └─> CONNECTED
           │   └─ Failure
           │      └─> RECONNECTING
           │
    ┌──────┴──────┐
    │  CONNECTED  │
    │  socket=Pid │
    └──────┬──────┘
           │
           ├─ send(Data) ─> gen_tcp:send() ─> ok
           ├─ recv(Data) ─> handle_info(tcp) ─> Owner ! msg
           │
           └─ Error: {tcp_closed, Socket}
              └─> RECONNECTING
                   │
                   ├─ backoff_delay = min(1s * 1.5^N, 60s)
                   ├─ erlang:send_after(backoff_delay, reconnect)
                   │
                   └─> Retry gen_tcp:connect()
                       ├─ Success ─> CONNECTED (reset counter)
                       └─ Failure ─> RECONNECTING (increment counter)
                           └─ If max_attempts exceeded ─> CLOSED
```

---

## Configuration Propagation

### STDIO Transport
```
Config:
  transport_id: atom() ✓ REQUIRED
  test_mode: boolean() = false
  max_message_size: integer() = 16777216

Flow:
  Config → init([Owner])
    ├─ extract transport_id (optional for stdio)
    ├─ extract test_mode (skip reader if true)
    └─ set max_message_size (used in validate_message_size/2)
```

### TCP Transport
```
Config:
  transport_id: atom() ✓ REQUIRED
  mode: client | server = client
  host: string() ✓ REQUIRED (client mode)
  port: integer() ✓ REQUIRED
  keepalive: boolean() = true
  nodelay: boolean() = true
  buffer_size: integer() = 65536
  max_reconnect_attempts: infinity | integer() = infinity
  connect_timeout: integer() = 5000

Flow:
  Config → init([TransportId, Config])
    ├─ Gen socket options: [binary, {active, once}, ...]
    ├─ Add TCP_NODELAY, TCP_KEEPALIVE
    ├─ Set buffer_size
    └─ Schedule initial connection or listen
```

### HTTP Transport
```
Config:
  transport_id: atom() ✓ REQUIRED
  url: binary() ✓ REQUIRED ("http://host:port/path")
  owner: pid() ✓ REQUIRED
  method: get | post = post
  headers: [{binary(), binary()}] = []
  timeout: integer() = 30000
  connect_timeout: integer() = 5000
  max_retries: integer() = 3
  retry_delay: integer() = 1000
  pool_size: integer() = 5
  ssl_options: [option()] = []

Flow:
  Config → init([Config])
    ├─ parse_url(Url) → {Scheme, Host, Port, Path}
    ├─ Open gun client: gun:open(Host, Port, {ssl, ssl_options if https})
    ├─ Set up message_queue: queue:new()
    └─ Initialize pending_requests: #{}
```

---

## Error Recovery Strategy

### TCP Reconnection Backoff
```erlang
Initial delay: 1000ms
Backoff formula: NextDelay = min(PrevDelay * 1.5, 60000ms)
Max attempts: ∞ (configurable)

Sequence:
  Attempt 1: Wait 1s, retry
  Attempt 2: Wait 1.5s, retry
  Attempt 3: Wait 2.25s, retry
  Attempt 4: Wait 3.375s, retry
  ...
  Attempt N: Wait min(1000 * 1.5^(N-1), 60s), retry
```

### HTTP Retry with Backoff
```erlang
Initial delay: retry_delay from config (default 1000ms)
Same exponential backoff as TCP: min(prev * 1.5, 60s)
Max retries: max_retries from config (default 3)

Retryable status codes:
  - 429 (Too Many Requests)
  - 500, 502, 503, 504 (Server errors)

Non-retryable:
  - 4xx (Client errors, except 429)
  - 3xx (Redirects - not handled)
  - 1xx (Informational)
```

---

## Summary of Transport Behaviors

| Transport | Mode | Framing | Backpressure | Buffering | Reconnect |
|-----------|------|---------|--------------|-----------|-----------|
| **STDIO** | RX/TX | Newline (`\n`) | Block on I/O | Line buffer | N/A |
| **TCP** | Client/Server | Newline (`\n`) | Active socket | 64KB default | Exponential |
| **HTTP** | POST | HTTP request | Pool-based | Queue (unlimited) | Exponential |
| **WebSocket** | Bidirectional | Frame + UTF8 | Frame buffer | 100KB buffer | (not active) |
| **SSE** | RX only | Event format | Connection mgmt | Event queue | (not active) |

---

## Key Implementation Insights

### Why erlmcp_transport_http is Complex
1. **Dual Purpose**: Both sends TO remote HTTP server AND handles FROM HTTP clients
2. **Gun Integration**: Uses gun/2.0.1 for HTTP client (connection multiplexing)
3. **Queue Management**: Messages queue up if gun connection drops; retry logic must track state
4. **Stream Tracking**: gun assigns StreamRef to each request; must map response back to originating request

### Why TCP Reconnection Works Well
1. **Clean Separation**: Connection establishment separate from message send
2. **Error Isolation**: Single socket failure doesn't block queue
3. **Exponential Backoff**: Prevents thundering herd on target failure
4. **Owner Notification**: Sends messages to owner (`{transport_error, ...}`)

### Why STDIO is Simplest
1. **Synchronous I/O**: stdin/stdout are naturally blocking
2. **Line-Based**: Standard Unix pipe framing
3. **Parent Ownership**: Reader process linked to transport gen_server
4. **No Reconnection**: If stdin closes, process terminates

---

## For Version 2.0: What to Simplify

1. **HTTP Transport**: Consolidate 4 modules → 2 (interface + impl)
2. **Auto-Registration**: Fix registry integration
3. **Behavior Migration**: Standardize on `erlmcp_transport_behavior`
4. **WebSocket/SSE**: Decide wire-in vs remove entirely
5. **Test Consolidation**: Merge duplicate test files

**Status**: Ready for cleanup decisions
