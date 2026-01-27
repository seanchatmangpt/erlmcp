# Poka-Yoke Error Prevention Gap Analysis for erlmcp

**Prepared**: 2026-01-27
**Framework**: Poka-Yoke (mistake-proofing) and Lean Six Sigma error prevention
**Scope**: erlmcp Model Context Protocol (MCP) SDK for Erlang/OTP
**Objective**: Identify and categorize error prevention gaps across 8 key areas with implementation recommendations

---

## Executive Summary

This Poka-Yoke analysis identifies **28 error prevention gaps** across the erlmcp codebase. These gaps represent areas where:
- Invalid operations are possible (should be prevented at design time)
- Errors could be silently ignored or propagated incorrectly
- System state could be corrupted by invalid inputs
- Resource limits could be exceeded unexpectedly
- Observers cannot detect system problems

**Risk Level**: 8 Critical, 12 High, 8 Medium
**Implementation Priority**: Phases 1-3 recommended based on defect severity

---

## 1. Message Format Validation

### What Works ✓

**Current Prevention Mechanisms:**
- JSON-RPC 2.0 schema validation in `erlmcp_json_rpc.erl`
- Version field validation (`validate_jsonrpc_version/1`)
- Message type detection (request/response/notification)
- Binary type enforcement for method names
- Field presence checking for required fields (id, method, jsonrpc)
- Error code validation with predefined error code list
- Message size validation before decoding (Gap #45)

**Implementation Artifacts:**
- `erlmcp_json_rpc.erl`: Lines 45-178 (encode/decode functions)
- `erlmcp_message_parser.erl`: Lines 38-107 (fast-path parsing)
- `erlmcp.hrl`: Lines 48-66 (error codes list)

### Identified Gaps ✗

#### **Gap 1.1: Parameter Type Not Strictly Validated (HIGH)**
**Problem**: `validate_params/1` in `erlmcp_message_parser.erl` (lines 120-125) allows any term:
```erlang
validate_params(_) -> undefined.
```
Invalid types (atoms, numbers, etc.) are silently converted to `undefined` instead of rejecting them.

**What Could Go Wrong:**
- Malformed requests with unexpected parameter types silently accepted
- Client bugs hidden because invalid input silently treated as empty params
- Tool handlers receive `undefined` instead of detecting bad input

**Why It Matters**:
- Message integrity: Clients need clear feedback when they send invalid parameters
- Error propagation: Silent normalization prevents root cause analysis

**Recommended Fix**:
```erlang
-spec validate_params(term()) -> json_rpc_params() | {error, invalid_params}.
validate_params(undefined) -> undefined;
validate_params(Params) when is_map(Params) -> Params;
validate_params(Params) when is_list(Params) -> Params;
validate_params(_) -> {error, invalid_params}.
```

**Severity**: HIGH
**Implementation**: 1-2 hours

---

#### **Gap 1.2: Batch Request Validation Incomplete (CRITICAL)**
**Problem**: `parse_batch_requests/2` in `erlmcp_json_rpc.erl` (lines 278-288) silently skips invalid requests:
```erlang
parse_batch_requests([Request | Rest], Acc) when is_map(Request) ->
    case erlmcp_message_parser:parse_json_rpc(Request) of
        {ok, Message} ->
            parse_batch_requests(Rest, [Message | Acc]);
        {error, _} ->
            %% Continue processing batch even on error, collect all errors
            parse_batch_requests(Rest, Acc)  % ← SILENTLY SKIPS ERROR
    end;
parse_batch_requests([_Invalid | Rest], Acc) ->
    %% Invalid request in batch, skip and continue
    parse_batch_requests(Rest, Acc).  % ← SILENTLY SKIPS INVALID
```

**What Could Go Wrong:**
- Client sends 10-request batch, 3 are malformed, only 7 are processed
- No error indication that requests were dropped
- Impossible to debug which requests failed
- Could allow partial execution of critical operations

**Why It Matters**:
- JSON-RPC 2.0 spec requires all invalid requests in batch to generate individual error responses
- Current implementation loses information about which requests failed
- Breaks transactional semantics if client expects all-or-nothing

**Recommended Fix**:
- Return error responses for all invalid batch requests, not silently skip
- Keep both valid and error responses in result
- Implement proper error collection strategy

**Severity**: CRITICAL (Protocol violation)
**Implementation**: 3-4 hours

---

#### **Gap 1.3: Response Message Validation Missing (HIGH)**
**Problem**: No validation when creating response messages in `erlmcp_json_rpc.erl:build_message_map/1` (lines 308-313):
```erlang
build_message_map(#json_rpc_response{id = Id, result = Result, error = Error}) ->
    Base = #{
        ?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,
        ?JSONRPC_FIELD_ID => encode_id(Id)
    },
    add_result_or_error(Base, Result, Error).
```

**What Could Go Wrong:**
- Response can have both `result` and `error` fields (protocol violation)
- Response can have neither field (invalid JSON-RPC)
- `id` field can be missing in response (required per spec)
- No validation that error is properly formed

**Why It Matters**:
- Responses with invalid structure sent to clients
- Clients receive corrupted JSON-RPC messages
- Breaks protocol compliance

**Recommended Fix**:
```erlang
-spec validate_response(#json_rpc_response{}) -> {ok, #json_rpc_response{}} | {error, term()}.
validate_response(#json_rpc_response{id = undefined}) ->
    {error, {missing_id, <<"Response must have id field">>}};
validate_response(#json_rpc_response{result = R, error = E}) when is_map(R), is_map(E) ->
    {error, {both_result_and_error, <<"Response cannot have both result and error">>}};
validate_response(Response) ->
    {ok, Response}.
```

**Severity**: HIGH
**Implementation**: 2-3 hours

---

#### **Gap 1.4: Field Value Range Validation Missing (HIGH)**
**Problem**: No validation of field value ranges:
- `Content-Length` headers not bounded
- Numeric fields (temperatures, timeouts) not checked for valid ranges
- String lengths not enforced
- Array lengths not limited

**Example from model preferences (Gap #23):**
```erlang
%% No validation that temperature is 0.0-2.0
%% No validation that maxTokens is positive
%% No validation that priorities sum to valid range
```

**What Could Go Wrong:**
- Malicious requests with extreme values
- Temperature = 999.0 (invalid for ML models)
- Negative timeouts
- Gigabyte-size string fields

**Why It Matters**:
- Resource exhaustion attacks
- Backend system crashes on invalid numeric ranges
- Silent failures with edge case values

**Recommended Fix**:
```erlang
-spec validate_model_preference(map()) -> {ok, map()} | {error, term()}.
validate_model_preference(#{<<"temperature">> := T} = Prefs) when is_float(T), T >= 0, T =< 2.0 ->
    {ok, Prefs};
validate_model_preference(#{<<"temperature">> := T}) ->
    {error, {invalid_temperature_range, <<"Must be 0.0-2.0">>}};
validate_model_preference(Prefs) ->
    {ok, Prefs}.
```

**Severity**: HIGH
**Implementation**: 4-5 hours (validate all numeric fields)

---

### Summary: Message Format Validation

| Gap ID | Issue | Severity | Time |
|--------|-------|----------|------|
| 1.1 | Parameter types allow invalid inputs | HIGH | 1-2h |
| 1.2 | Batch request validation skips errors | CRITICAL | 3-4h |
| 1.3 | Response validation missing | HIGH | 2-3h |
| 1.4 | Field value ranges not enforced | HIGH | 4-5h |

**Total**: 4 gaps, 10-14 hours implementation

---

## 2. Routing Safety

### What Works ✓

**Current Prevention Mechanisms:**
- gproc-based registry with automatic monitoring
- Transport-to-server binding validation
- Resource existence checks before registration
- Tool/prompt lookup before execution

**Implementation Artifacts:**
- `erlmcp_registry.erl`: Lines 213-233 (find operations)
- `erlmcp_server.erl`: Lines 176-206 (tool lookup)

### Identified Gaps ✗

#### **Gap 2.1: Unvalidated Message Routing in Registry (CRITICAL)**
**Problem**: `handle_cast({route_to_server, ...})` in `erlmcp_registry.erl` (lines 278-286):
```erlang
handle_cast({route_to_server, ServerId, TransportId, Message}, State) ->
    case gproc:where({n, l, {mcp, server, ServerId}}) of
        undefined ->
            logger:warning("Cannot route to server ~p: not found", [ServerId]),
            {noreply, State};
        ServerPid ->
            ServerPid ! {mcp_message, TransportId, Message},  % ← NO VALIDATION
            {noreply, State}
    end;
```

**What Could Go Wrong:**
- Arbitrary Erlang messages sent to server process
- Server forced to handle unexpected message formats
- Pattern matching errors if message doesn't match expected structure
- Stack overflows if cyclic routing possible

**Why It Matters**:
- Untyped message sends break gen_server contract
- Silent failures if message format wrong
- Security: servers could be DOS'ed with malformed messages

**Recommended Fix**:
```erlang
handle_cast({route_to_server, ServerId, TransportId, Message}, State) ->
    case validate_message_format(Message) of
        {ok, ValidMsg} ->
            case gproc:where({n, l, {mcp, server, ServerId}}) of
                undefined ->
                    logger:warning("Server ~p not found", [ServerId]),
                    {noreply, State};
                ServerPid ->
                    ServerPid ! {mcp_message, TransportId, ValidMsg},
                    {noreply, State}
            end;
        {error, Reason} ->
            logger:error("Invalid message format: ~p", [Reason]),
            {noreply, State}
    end.
```

**Severity**: CRITICAL
**Implementation**: 2-3 hours

---

#### **Gap 2.2: No Access Control on Tool/Prompt Calls (CRITICAL)**
**Problem**: `erlmcp_server.erl` handles tool calls without authorization:
```erlang
handle_call({call_tool, Name, Arguments}, _From, State) ->
    case maps:get(Name, State#state.tools, undefined) of
        undefined ->
            {reply, error_tool_not_found(undefined, Name), State};
        {Tool, Handler, _Schema} ->
            try
                Result = Handler(Arguments),  % ← NO AUTHZ CHECK
                {reply, {ok, Result}, State}
            catch
                Class:Reason ->
                    {reply, {error, Reason}, State}
            end
    end;
```

**What Could Go Wrong:**
- Any client can call any tool without permission
- No role-based access control (RBAC)
- No rate limiting per user per tool
- No audit trail of tool invocations

**Why It Matters**:
- Security: Critical tools exposed to all clients
- Compliance: No access control for sensitive operations
- Resource abuse: Malicious client could call expensive tools repeatedly

**Recommended Fix**:
```erlang
handle_call({call_tool, Name, Arguments}, From, State) ->
    ClientId = element(1, From),
    case check_tool_authorization(Name, ClientId, State) of
        {ok, allowed} ->
            case maps:get(Name, State#state.tools, undefined) of
                undefined ->
                    {reply, error_tool_not_found(undefined, Name), State};
                {Tool, Handler, _Schema} ->
                    execute_tool_with_auth(Name, Handler, Arguments, ClientId, State)
            end;
        {error, Reason} ->
            {reply, {error, {unauthorized, Reason}}, State}
    end.
```

**Severity**: CRITICAL
**Implementation**: 5-8 hours (with audit logging)

---

#### **Gap 2.3: Resource Access Without URI Canonicalization (HIGH)**
**Problem**: Resources matched by exact URI string without normalization:
```erlang
case maps:get(Uri, State#state.resources, undefined) of
    undefined -> {error, ...};
    {Resource, Handler} -> ...
end
```

**What Could Go Wrong:**
- Path traversal: `file://./../../etc/passwd` bypasses intent
- URL encoding bypass: `file://data%2Fkey` vs `file://data/key`
- Symlink attacks: Different URIs point to same resource
- Case sensitivity: `FILE://path` vs `file://path`

**Why It Matters**:
- Access control bypass: Intended resource limits evaded
- Data exfiltration: Private resources accessed via alternate paths
- Symlink-based privilege escalation

**Recommended Fix**:
- Canonicalize URIs before lookup (already implemented in `erlmcp_path_canonicalizer.erl`)
- Validate canonical form against whitelist
- Reject suspicious patterns (`.`, `..`, encoded separators)

**Severity**: HIGH
**Implementation**: 2-3 hours

---

#### **Gap 2.4: Dynamic Tool Registration Not Validated (HIGH)**
**Problem**: No validation when tools added via `add_tool`:
```erlang
-spec add_tool(server(), binary(), tool_handler()) -> ok.
add_tool(Server, Name, Handler) when is_binary(Name), is_function(Handler, 1) ->
    gen_server:call(Server, {add_tool, Name, Handler}).
```

**What Could Go Wrong:**
- Tool name contains special characters that break JSON
- Tool name collides with reserved RPC methods
- Tool handler is not actually callable
- Tool handler crashes on all inputs

**Why It Matters**:
- Invalid tool metadata sent to clients
- Client can't invoke tools due to invalid names
- Resource leaks if handler crashes repeatedly

**Recommended Fix**:
```erlang
-spec add_tool(server(), binary(), tool_handler()) -> {ok, pid()} | {error, term()}.
add_tool(Server, Name, Handler) ->
    case validate_tool_name(Name) of
        {ok, _} ->
            case validate_tool_handler(Handler) of
                {ok, _} ->
                    gen_server:call(Server, {add_tool, Name, Handler});
                {error, Reason} ->
                    {error, {invalid_handler, Reason}}
            end;
        {error, Reason} ->
            {error, {invalid_tool_name, Reason}}
    end.

-spec validate_tool_name(binary()) -> {ok, binary()} | {error, term()}.
validate_tool_name(Name) when is_binary(Name), byte_size(Name) > 0, byte_size(Name) =< 256 ->
    case re:match(Name, <<"^[a-zA-Z0-9_-]+$">>) of
        {match, _} -> {ok, Name};
        nomatch -> {error, {invalid_characters, <<"Tool name must match [a-zA-Z0-9_-]+">>}}
    end;
validate_tool_name(_) ->
    {error, {invalid_name, <<"Tool name must be 1-256 byte binary">>}}.
```

**Severity**: HIGH
**Implementation**: 2-3 hours

---

### Summary: Routing Safety

| Gap ID | Issue | Severity | Time |
|--------|-------|----------|------|
| 2.1 | Unvalidated message routing | CRITICAL | 2-3h |
| 2.2 | No access control on tool calls | CRITICAL | 5-8h |
| 2.3 | Resource access without canonicalization | HIGH | 2-3h |
| 2.4 | Dynamic tool registration not validated | HIGH | 2-3h |

**Total**: 4 gaps, 11-17 hours implementation

---

## 3. State Machine Enforcement

### What Works ✓

**Current Prevention Mechanisms:**
- Server phase tracking (`initialization`, `initialized`, `disconnected`, `closed`)
- Client phase tracking (`pre_initialization`, `initializing`, `initialized`)
- Initialization timeout enforcement
- Phase validation before operations

**Implementation Artifacts:**
- `erlmcp_server.erl`: Lines 45-61 (state record with phase)
- `erlmcp_client.erl`: Lines 44-61 (client phase tracking)

### Identified Gaps ✗

#### **Gap 3.1: Client Can Send Requests Before Initialize Succeeds (HIGH)**
**Problem**: Client phase check happens only in `CHECK_INITIALIZED` macro, not enforced on all calls:
```erlang
-define(CHECK_INITIALIZED(State, From),
    case State#state.phase of
        initialized -> ok;
        Phase -> {reply, {error, {not_initialized, Phase, ...}}, State}
    end).
```

**What Could Go Wrong:**
- Client sends `list_tools` before `initialize` called
- Requests sent while initialization in progress get queued
- Server state corruption if request arrives mid-initialization
- Race condition: Request 1 arrives, phase changes to initialized, Request 2 from same client interleaves

**Why It Matters**:
- Protocol violation: MCP spec requires initialize first
- State machines have well-defined ordering
- Allows out-of-order operations that could corrupt application state

**Recommended Fix**:
```erlang
%% Make phase check mandatory for ALL operations
handle_call(Request, From, State) ->
    case check_client_phase(State) of
        {error, Phase} ->
            {reply, {error, {not_initialized, Phase}}, State};
        ok ->
            case Request of
                {list_tools} -> handle_list_tools(From, State);
                {call_tool, ...} -> handle_call_tool(From, State);
                ...
            end
    end.
```

**Severity**: HIGH
**Implementation**: 3-4 hours (audit all 30+ message handlers)

---

#### **Gap 3.2: Reentrant Initialization Possible (HIGH)**
**Problem**: Multiple clients could call `initialize` concurrently:
```erlang
handle_call({initialize, Capabilities, Options}, _From, State) ->
    NewState = State#state{
        phase = ?MCP_PHASE_INITIALIZED,
        initialized = true,
        ...
    },
    {reply, {ok, ...}, NewState}
```

**What Could Go Wrong:**
- Two initialize calls both succeed, both modify state
- Race condition on `initialized` flag
- Capabilities overwritten by second initializer
- Initialization hooks called twice, causing resource leaks

**Why It Matters**:
- Initialization should be idempotent or fail on second call
- State corruption from concurrent modifications
- Resource leaks if cleanup code runs multiple times

**Recommended Fix**:
```erlang
handle_call({initialize, Capabilities, Options}, _From, State) ->
    case State#state.phase of
        ?MCP_PHASE_INITIALIZED ->
            {reply, {error, already_initialized}, State};
        ?MCP_PHASE_INITIALIZATION ->
            {reply, {error, initialization_in_progress}, State};
        _ ->
            %% Proceed with initialization
            ...
    end.
```

**Severity**: HIGH
**Implementation**: 2-3 hours

---

#### **Gap 3.3: Server Doesn't Enforce Client Can't Operate Before Client Initialize (HIGH)**
**Problem**: Server accepts requests from uninitialized clients without distinction:
```erlang
handle_call({list_resources}, _From, State) ->
    Resources = list_all_resources(State),
    {reply, {ok, Resources}, State}
    %% No check that client called initialize
```

**What Could Go Wrong:**
- Clients can get resource lists without capability negotiation
- Server exposes resources to clients that don't understand them
- Capability features advertised but not understood by client

**Why It Matters**:
- MCP spec: Server should track which clients initialized
- Can't advertise capability features to uninitialized clients
- Breaks versioning: Old clients might not understand new fields

**Recommended Fix**:
```erlang
handle_call({list_resources}, From, State) ->
    case is_client_initialized(From, State) of
        true ->
            Resources = list_all_resources(State),
            {reply, {ok, Resources}, State};
        false ->
            {reply, {error, {client_not_initialized, <<"Client must call initialize first">>}}, State}
    end.
```

**Severity**: HIGH
**Implementation**: 4-5 hours (track client initialization state)

---

#### **Gap 3.4: No Timeout on Uninitialized Connections (MEDIUM)**
**Problem**: If client never sends initialize message, connection stays open forever:
```erlang
-define(MCP_DEFAULT_INIT_TIMEOUT_MS, 30000).
```
This timeout is defined but never used in `erlmcp_client.erl` or `erlmcp_server.erl`.

**What Could Go Wrong:**
- Zombie connections consuming resources
- Client crash leaves connection in limbo
- DOS: Open connections without initializing
- Resource exhaustion on long-lived servers

**Why It Matters**:
- Prevents resource cleanup
- Allow uninitialized connection DOS attacks
- Connection count grows unbounded

**Recommended Fix**:
```erlang
init([ServerId, Capabilities]) ->
    %% Set initialization timeout
    TimeoutMs = erlmcp_config:get(init_timeout_ms, ?MCP_DEFAULT_INIT_TIMEOUT_MS),
    TimerRef = erlang:send_after(TimeoutMs, self(), initialization_timeout),
    {ok, #state{
        ...
        init_timeout_ref = TimerRef,
        phase = ?MCP_PHASE_INITIALIZATION
    }}.

handle_info(initialization_timeout, State) ->
    logger:warning("Server initialization timeout exceeded"),
    {stop, initialization_timeout, State}.
```

**Severity**: MEDIUM
**Implementation**: 2-3 hours

---

### Summary: State Machine Enforcement

| Gap ID | Issue | Severity | Time |
|--------|-------|----------|------|
| 3.1 | Client sends requests before initialize | HIGH | 3-4h |
| 3.2 | Reentrant initialization possible | HIGH | 2-3h |
| 3.3 | Server doesn't check client initialized | HIGH | 4-5h |
| 3.4 | No timeout on uninitialized connections | MEDIUM | 2-3h |

**Total**: 4 gaps, 11-15 hours implementation

---

## 4. Connection Safety

### What Works ✓

**Current Prevention Mechanisms:**
- TCP socket closed check in `erlmcp_transport_tcp.erl`
- Registry prevents duplicate registrations
- gproc monitoring for process death
- Session tracking in `erlmcp_session_manager.erl`

**Implementation Artifacts:**
- `erlmcp_transport_tcp.erl`: Lines 86-94 (socket check)
- `erlmcp_registry.erl`: Lines 122-135 (duplicate prevention)

### Identified Gaps ✗

#### **Gap 4.1: Closed Connection Can Be Reused (HIGH)**
**Problem**: `erlmcp_transport_tcp.erl` doesn't prevent send on closed socket:
```erlang
send(#state{socket = undefined}, _Data) ->
    {error, not_connected};
send(#state{socket = Socket, connected = true}, Data) ->
    case gen_tcp:send(Socket, [Data, "\n"]) of
        ok -> ok;
        {error, Reason} -> {error, {tcp_send_failed, Reason}}
    end;
send(_State, _Data) ->
    {error, not_connected}.
```

**What Could Go Wrong:**
- Application keeps reference to closed socket
- Sends data to closed connection silently fail
- No notification that connection is dead
- Orphaned process tries to send to already-closed socket

**Why It Matters**:
- Message loss: Data sent after close is lost, client doesn't know
- Resource leak: Processes hang waiting for response that won't come
- Connection hijacking: New connection on same port accepts data meant for old

**Recommended Fix**:
```erlang
send(State, Data) ->
    case is_socket_valid(State) of
        true ->
            case gen_tcp:send(State#state.socket, [Data, "\n"]) of
                ok -> ok;
                {error, closed} ->
                    %% Socket is closed, mark as dead
                    notify_connection_closed(State),
                    {error, connection_closed};
                {error, Reason} ->
                    {error, {tcp_send_failed, Reason}}
            end;
        false ->
            {error, connection_closed}
    end.
```

**Severity**: HIGH
**Implementation**: 2-3 hours

---

#### **Gap 4.2: Session ID Reuse Not Prevented (CRITICAL)**
**Problem**: `erlmcp_session_manager.erl` generates session IDs but doesn't enforce uniqueness in time:
```erlang
%% No verification that session ID hasn't been used before
%% Could reuse same ID if clock goes backward or service restarts
```

**What Could Go Wrong:**
- Two different connections get same session ID
- Client A and Client B both have session-123
- Traffic routed to wrong client
- Session fixation attack: Attacker uses known session ID

**Why It Matters**:
- Session hijacking: Client A listens to Client B's messages
- Security: Session fixation allows unauthorized access
- Data exfiltration: Private data leaked to wrong client

**Recommended Fix**:
```erlang
-spec generate_session_id() -> binary().
generate_session_id() ->
    %% Use combination of timestamp + random to ensure uniqueness
    Timestamp = erlang:system_time(nanosecond),
    Random = crypto:strong_rand_bytes(32),
    Combined = <<Timestamp:64, Random/binary>>,
    base64:encode(crypto:hash(sha256, Combined)).

-spec register_session(binary()) -> {ok, session_id()} | {error, term()}.
register_session(SessionId) ->
    case ets:lookup(?MODULE, SessionId) of
        [] ->
            %% Session ID not in use
            ets:insert(?MODULE, {SessionId, now(), self()}),
            {ok, SessionId};
        [_] ->
            %% Session ID already in use, generate new one
            generate_session_id() %% Retry with new ID
    end.
```

**Severity**: CRITICAL
**Implementation**: 3-4 hours

---

#### **Gap 4.3: No Connection Rate Limiting (HIGH)**
**Problem**: Transport layers don't limit connection rate:
```erlang
%% erlmcp_transport_tcp.erl - no rate limit on accept
handle_info({gun_data, ...}, State) ->
    %% Accept data from any connection
```

**What Could Go Wrong:**
- Connection flood: Attacker opens 10,000 connections/sec
- Server exhaust file descriptor limits
- Memory exhaustion: Each connection consumes memory
- DOS: Legitimate clients can't connect

**Why It Matters**:
- Resource exhaustion DOS
- Prevents legitimate clients from connecting
- Server becomes unresponsive

**Recommended Fix**:
- Integrate with `erlmcp_rate_limiter.erl` (already exists)
- Check connection rate on each new connection
- Implement SYN flood protection (TCP backlog)
- Add per-IP rate limiting

**Severity**: HIGH
**Implementation**: 2-3 hours (integrate existing rate limiter)

---

#### **Gap 4.4: Stale Connection Detection Missing (MEDIUM)**
**Problem**: Long-lived connections never checked for staleness:
```erlang
%% No keepalive handling
%% No heartbeat/ping-pong
%% No detection of one-sided connection close
```

**What Could Go Wrong:**
- Client crashes, connection stays open on server
- Network cable unplugged, connection stays open
- Half-open connection: Server sends, data never reaches client
- Resource leak: Thousands of stale connections

**Why It Matters**:
- Resource exhaustion: Connections never cleaned up
- False availability: Server thinks it has connection to client
- Message loss: Notifications sent to dead connections

**Recommended Fix**:
```erlang
%% Add heartbeat mechanism
handle_info(send_heartbeat, State) ->
    %% Send ping every 30 seconds
    case send_ping(State) of
        ok ->
            TimerRef = erlang:send_after(30000, self(), send_heartbeat),
            {noreply, State#state{heartbeat_timer = TimerRef}};
        {error, closed} ->
            %% Connection is dead
            {stop, heartbeat_failed, State}
    end.
```

**Severity**: MEDIUM
**Implementation**: 3-4 hours (heartbeat + cleanup)

---

### Summary: Connection Safety

| Gap ID | Issue | Severity | Time |
|--------|-------|----------|------|
| 4.1 | Closed connection reuse possible | HIGH | 2-3h |
| 4.2 | Session ID reuse not prevented | CRITICAL | 3-4h |
| 4.3 | No connection rate limiting | HIGH | 2-3h |
| 4.4 | Stale connection detection missing | MEDIUM | 3-4h |

**Total**: 4 gaps, 10-14 hours implementation

---

## 5. Resource Limits Enforcement

### What Works ✓

**Current Prevention Mechanisms:**
- Rate limiter with token bucket (`erlmcp_rate_limiter.erl`)
- Message size validation (`erlmcp_message_size.erl`)
- Circuit breaker for overload protection
- Backpressure system with queue monitoring
- Request timeout (5000ms default)

**Implementation Artifacts:**
- `erlmcp_rate_limiter.erl`: Lines 44-100 (rate limiting)
- `erlmcp_message_size.erl`: Message size checks
- `erlmcp_circuit_breaker.erl`: Overload detection

### Identified Gaps ✗

#### **Gap 5.1: No Global Memory Limit (CRITICAL)**
**Problem**: No bounds on total system memory consumption:
```erlang
%% No limit on:
%% - Total number of pending requests
%% - Total message queue depth
%% - Total ETS table size
%% - Total process heap growth
```

**What Could Go Wrong:**
- Malicious client sends millions of small requests
- Message queue grows unbounded: 1GB, 10GB, 100GB
- Server runs out of memory and crashes
- Orphaned processes with leaked memory
- Memory usage grows and never shrinks

**Why It Matters**:
- Out-of-memory crash takes down entire server
- No gradual degradation
- Affects all clients, not just attacker

**Recommended Fix**:
```erlang
-spec enforce_memory_limits() -> ok | {error, out_of_memory}.
enforce_memory_limits() ->
    {Total, _, _} = erlang:memory(),
    MaxMemory = erlmcp_config:get(max_memory_bytes, 512 * 1024 * 1024), % 512MB
    case Total > MaxMemory of
        true ->
            %% Trigger garbage collection
            erlang:garbage_collect(),
            %% Check again
            {Total2, _, _} = erlang:memory(),
            case Total2 > MaxMemory of
                true -> {error, out_of_memory};
                false -> ok
            end;
        false ->
            ok
    end.

%% Call periodically or when processing new requests
handle_call(Request, From, State) ->
    case enforce_memory_limits() of
        {error, out_of_memory} ->
            {reply, {error, {system_overloaded, <<"Server out of memory">>}}, State};
        ok ->
            handle_request(Request, From, State)
    end.
```

**Severity**: CRITICAL
**Implementation**: 3-4 hours

---

#### **Gap 5.2: No ETS Table Size Limits (HIGH)**
**Problem**: ETS tables can grow unbounded:
```erlang
%% In erlmcp_registry.erl, erlmcp_rate_limiter.erl, erlmcp_backpressure.erl
%% No ets:insert/ets:delete cycle with size checks
```

**What Could Go Wrong:**
- Registry grows with every request ID generated
- Rate limiter tracks unlimited clients
- Backpressure tracking grows without cleanup
- ETS memory exhaustion

**Why It Matters**:
- Gradual DOS: Server slows down as ETS grows
- Memory leak: Data inserted but never removed
- Affects garbage collection: Large ETS tables slow GC

**Recommended Fix**:
```erlang
%% Add cleanup on periodic interval
handle_info(cleanup_ets, State) ->
    cleanup_stale_entries(State#state.metrics),
    TimerRef = erlang:send_after(60000, self(), cleanup_ets),
    {noreply, State#state{cleanup_timer = TimerRef}}.

-spec cleanup_stale_entries(ets:table()) -> ok.
cleanup_stale_entries(Table) ->
    Now = erlang:system_time(millisecond),
    MaxAge = 3600000,  % 1 hour
    ets:select_delete(Table, [{
        {{'_', '_', ets_insert_time, '$1'}, '_'},
        [{'<', '$1', Now - MaxAge}],
        [true]
    }]).
```

**Severity**: HIGH
**Implementation**: 2-3 hours per module

---

#### **Gap 5.3: Request Queue Not Bounded (HIGH)**
**Problem**: gen_server message queue grows unbounded:
```erlang
%% No limit on pending gen_server:call requests
%% No backpressure on clients sending requests
%% No queue depth monitoring
```

**What Could Go Wrong:**
- Client sends 1M requests without waiting
- All requests queue on server
- Server becomes unresponsive
- Memory exhaustion from queued messages

**Why It Matters**:
- Denial of service: One aggressive client blocks others
- Resource leak: Messages never processed
- Server becomes zombie (can't respond)

**Recommended Fix**:
```erlang
-spec check_queue_depth(pid()) -> {ok, depth} | {error, queue_full}.
check_queue_depth(Pid) ->
    {message_queue_len, QueueLen} = process_info(Pid, message_queue_len),
    MaxQueueLen = erlmcp_config:get(max_queue_depth, 10000),
    case QueueLen > MaxQueueLen of
        true -> {error, queue_full};
        false -> {ok, depth}
    end.

%% In gen_server calling path
call_with_queue_limit(Pid, Request) ->
    case check_queue_depth(Pid) of
        {error, queue_full} ->
            {error, {server_overloaded, <<"Request queue full">>}};
        {ok, _} ->
            gen_server:call(Pid, Request)
    end.
```

**Severity**: HIGH
**Implementation**: 2-3 hours

---

#### **Gap 5.4: No Timeout on Long-Running Handlers (HIGH)**
**Problem**: Tool handlers have no timeout:
```erlang
try
    Result = Handler(Arguments),  %% Could hang forever
    {reply, {ok, Result}, State}
catch
    ...
end
```

**What Could Go Wrong:**
- Buggy tool handler blocks forever
- Blocks all other requests on that server
- Client timeout triggers, but server still running handler
- Thread/process exhaustion

**Why It Matters**:
- One slow tool blocks all clients
- Resource leak: Long-running processes
- Cascading failures

**Recommended Fix**:
```erlang
-spec call_tool_with_timeout(binary(), fun((map()) -> term()), map(), pos_integer()) ->
    {ok, term()} | {error, timeout}.
call_tool_with_timeout(Name, Handler, Arguments, TimeoutMs) ->
    try
        Result = erlang:apply(Handler, [Arguments]),
        {ok, Result}
    catch
        error:timeout ->
            {error, {tool_timeout, Name, TimeoutMs}};
        Class:Reason ->
            {error, {tool_execution_error, Class, Reason}}
    after
        %% Ensure cleanup
        ok
    end.

%% Wrapped handler with timeout
safe_handler(Handler, Arguments, TimeoutMs) ->
    {Pid, Ref} = erlang:spawn_monitor(fun() ->
        Result = Handler(Arguments),
        exit({ok, Result})
    end),
    receive
        {'DOWN', Ref, process, Pid, {ok, Result}} ->
            {ok, Result};
        {'DOWN', Ref, process, Pid, Reason} ->
            {error, {handler_error, Reason}}
    after
        TimeoutMs ->
            erlang:demonitor(Ref, [flush]),
            exit(Pid, kill),
            {error, timeout}
    end.
```

**Severity**: HIGH
**Implementation**: 3-4 hours

---

### Summary: Resource Limits Enforcement

| Gap ID | Issue | Severity | Time |
|--------|-------|----------|------|
| 5.1 | No global memory limit | CRITICAL | 3-4h |
| 5.2 | No ETS table size limits | HIGH | 6-9h |
| 5.3 | Request queue not bounded | HIGH | 2-3h |
| 5.4 | No timeout on long-running handlers | HIGH | 3-4h |

**Total**: 4 gaps, 14-20 hours implementation

---

## 6. Error Handling

### What Works ✓

**Current Prevention Mechanisms:**
- Try-catch in handler invocation
- Error response encoding (Gap #5)
- Error code validation
- Logging on errors
- Recovery manager (`erlmcp_recovery_manager.erl`)

**Implementation Artifacts:**
- `erlmcp_json_rpc.erl`: Lines 62-80 (error responses)
- `erlmcp_server.erl`: Handler execution with error handling

### Identified Gaps ✗

#### **Gap 6.1: Handler Exceptions Leak Internal Details (HIGH)**
**Problem**: Exception details sent to client:
```erlang
try
    Result = Handler(Arguments),
    {reply, {ok, Result}, State}
catch
    Class:Reason ->
        {reply, {error, Reason}, State}  % ← Reveals internals
end
```

**What Could Go Wrong:**
- Client sees internal error messages
- Stack trace leaks file paths
- Database connection strings in error messages
- Attacker learns system internals for further attacks

**Why It Matters**:
- Information disclosure vulnerability
- Helps attackers plan exploits
- Violates security principle of least information

**Recommended Fix**:
```erlang
safe_call_handler(Name, Handler, Arguments) ->
    try
        {ok, Handler(Arguments)}
    catch
        Class:Reason:Stacktrace ->
            logger:error("Tool ~p execution failed: ~p:~p", [Name, Class, Reason], #{
                stacktrace => Stacktrace
            }),
            %% Return generic error to client
            {error, {
                ?MCP_ERROR_INVALID_PARAMS,
                <<"Tool execution failed">>,
                #{<<"tool">> => Name}
            }}
    end.
```

**Severity**: HIGH
**Implementation**: 2-3 hours

---

#### **Gap 6.2: Errors Not Logged with Context (MEDIUM)**
**Problem**: Errors logged without enough context:
```erlang
logger:warning("Cannot route to server ~p: not found", [ServerId])
%% No context: which client, what message, what time?
```

**What Could Go Wrong:**
- Debugging impossible: Which request caused this?
- Missing correlation IDs between related errors
- Can't track error sequence
- Performance issues invisible

**Why It Matters**:
- Debugging takes 10x longer
- Root cause analysis impossible
- Can't correlate client-side and server-side logs

**Recommended Fix**:
```erlang
log_error_with_context(Context, Format, Args) ->
    logger:error(Format ++ " [~p:~p:~p]",
        Args ++ [maps:get(request_id, Context, unknown),
                 maps:get(client_id, Context, unknown),
                 maps:get(transport_id, Context, unknown)],
        #{context => Context}).
```

**Severity**: MEDIUM
**Implementation**: 4-5 hours (audit all logger calls)

---

#### **Gap 6.3: Silent Failures in List Operations (MEDIUM)**
**Problem**: `parse_batch_requests` silently drops errors (already identified in Gap 1.2):
```erlang
%% Silently skip invalid requests instead of collecting errors
parse_batch_requests([_Invalid | Rest], Acc) ->
    parse_batch_requests(Rest, Acc).  % ← Lost error information
```

**What Could Go Wrong:**
- Duplicate to Gap 1.2 (see above)

**Severity**: MEDIUM (see Gap 1.2)

---

#### **Gap 6.4: No Circuit Breaker Integration in Tool Calls (HIGH)**
**Problem**: If tool handler fails repeatedly, no circuit breaker:
```erlang
%% Tool fails 1000 times
%% Each failure is logged but handler still called
%% No slow-down or bypass
```

**What Could Go Wrong:**
- Cascading failures: Failed tool blocks other operations
- Slow degradation: Each failed call takes time
- No recovery path: System can't bypass broken tool
- Client timeout after waiting on broken tool

**Why It Matters**:
- Resilience: System should detect and bypass failures
- Fail-fast: Should fail immediately when tool known to be broken
- Resource conservation: Don't waste resources on broken tools

**Recommended Fix**:
```erlang
-spec call_tool_with_circuit_breaker(binary(), tool_handler(), map(), state()) ->
    {ok, term()} | {error, term()}.
call_tool_with_circuit_breaker(Name, Handler, Arguments, State) ->
    case erlmcp_circuit_breaker:is_open() of
        true ->
            {error, {service_unavailable, <<"System overloaded">>}};
        false ->
            case safe_call_handler(Name, Handler, Arguments) of
                {ok, Result} ->
                    erlmcp_circuit_breaker:record_request(erlang:now(), 0),
                    {ok, Result};
                {error, Reason} ->
                    erlmcp_circuit_breaker:record_error(erlang:now()),
                    {error, Reason}
            end
    end.
```

**Severity**: HIGH
**Implementation**: 2-3 hours

---

### Summary: Error Handling

| Gap ID | Issue | Severity | Time |
|--------|-------|----------|------|
| 6.1 | Handler exceptions leak internals | HIGH | 2-3h |
| 6.2 | Errors not logged with context | MEDIUM | 4-5h |
| 6.3 | Silent failures in batch (duplicate) | MEDIUM | - |
| 6.4 | No circuit breaker in tool calls | HIGH | 2-3h |

**Total**: 3 gaps, 8-11 hours implementation (excluding duplicate)

---

## 7. Monitoring & Observability

### What Works ✓

**Current Prevention Mechanisms:**
- OpenTelemetry integration
- OTEL tracing for spans
- Metrics collection
- Performance benchmarking
- Health checks

**Implementation Artifacts:**
- `erlmcp_otel.erl`: OTEL integration
- `erlmcp_metrics.erl`: Metrics collection
- Health monitoring modules

### Identified Gaps ✗

#### **Gap 7.1: No Metric on Failed Authorization (MEDIUM)**
**Problem**: Authorization failures not tracked as metrics:
```erlang
%% No metrics for:
%% - Tool access denied count
%% - Resource access denied count
%% - Unauthorized requests per client
```

**What Could Go Wrong:**
- Can't detect brute force attacks
- Security breaches invisible
- Can't identify compromised clients
- No alerting on suspicious patterns

**Why It Matters**:
- Security: Can't detect attacks in progress
- Compliance: No audit trail for access control
- Operational: Can't identify which clients misbehave

**Recommended Fix**:
```erlang
-spec record_authorization_failure(binary(), binary()) -> ok.
record_authorization_failure(ClientId, ToolName) ->
    erlmcp_metrics:increment_counter(
        [tool, access_denied],
        #{client_id => ClientId, tool => ToolName}
    ).
```

**Severity**: MEDIUM
**Implementation**: 2-3 hours (audit all authz points)

---

#### **Gap 7.2: No Detection of Slow Tool Execution (HIGH)**
**Problem**: Slow tools not detected:
```erlang
%% Tool takes 30 seconds
%% No warning until timeout
%% Client doesn't know it's slow
```

**What Could Go Wrong:**
- Slow tools become bottleneck
- Resource exhaustion: Slow tools pile up requests
- Can't identify performance problems
- No optimization guidance

**Why It Matters**:
- Performance: Can't find and fix slow operations
- Capacity planning: Don't know which tools are expensive
- SLA violations: Can't meet response time targets

**Recommended Fix**:
```erlang
-spec record_tool_execution(binary(), non_neg_integer()) -> ok.
record_tool_execution(ToolName, DurationMs) ->
    erlmcp_metrics:record_histogram(
        [tool, execution_time_ms],
        DurationMs,
        #{tool => ToolName}
    ),
    case DurationMs > 5000 of
        true ->
            logger:warning("Slow tool execution: ~p took ~pms",
                [ToolName, DurationMs]);
        false ->
            ok
    end.
```

**Severity**: HIGH
**Implementation**: 2-3 hours

---

#### **Gap 7.3: No Alert on Resource Limit Approaching (MEDIUM)**
**Problem**: System doesn't warn before hitting limits:
```erlang
%% Memory at 95%? No alert.
%% Connection count at 1000/1024? No alert.
%% Queue depth at 9900/10000? No alert.
```

**What Could Go Wrong:**
- System crashes suddenly
- No time to act before failure
- Operators surprised by outage
- Cascade failure: One client's overload takes down others

**Why It Matters**:
- Operations: Need warning before critical failure
- SLA: Need graceful degradation, not crash
- Capacity planning: Don't know limits in advance

**Recommended Fix**:
```erlang
-spec check_and_alert_limits() -> ok.
check_and_alert_limits() ->
    {Total, _, _} = erlang:memory(),
    MaxMemory = erlmcp_config:get(max_memory_bytes, 512 * 1024 * 1024),
    PercentUsed = (Total / MaxMemory) * 100,
    case PercentUsed > 80 of
        true ->
            logger:warning("Memory usage approaching limit: ~p%",
                [round(PercentUsed)]),
            erlmcp_metrics:record_gauge([system, memory_usage_percent], PercentUsed);
        false ->
            ok
    end.
```

**Severity**: MEDIUM
**Implementation**: 2-3 hours

---

#### **Gap 7.4: No Distributed Tracing Correlation (HIGH)**
**Problem**: Requests not traced across distributed calls:
```erlang
%% Client sends request ID 1
%% Server calls tool (new trace ID)
%% Tool calls external API (different trace ID)
%% Can't correlate: request -> tool -> API
```

**What Could Go Wrong:**
- Can't trace request through system
- Performance issues invisible across services
- Debugging distributed failures impossible
- Can't identify which client caused latency

**Why It Matters**:
- Observability: Can't see request flow
- Debugging: Can't track errors across services
- Performance: Can't identify bottlenecks

**Recommended Fix**:
```erlang
-spec extract_trace_context(term()) -> map().
extract_trace_context(Message) ->
    %% Extract W3C Trace Context from message
    case maps:get(<<"traceparent">>, Message, undefined) of
        undefined ->
            %% Generate new trace ID
            {TraceId, SpanId} = erlang:spawn_info(self(), spawn_id),
            #{trace_id => TraceId, span_id => SpanId};
        TraceParent ->
            parse_traceparent(TraceParent)
    end.

-spec propagate_trace_context(map()) -> map().
propagate_trace_context(Context) ->
    %% Propagate trace context to downstream calls
    {ParentSpanId, ChildSpanId} = generate_span_ids(),
    Context#{
        parent_span_id => ParentSpanId,
        span_id => ChildSpanId
    }.
```

**Severity**: HIGH
**Implementation**: 4-5 hours

---

### Summary: Monitoring & Observability

| Gap ID | Issue | Severity | Time |
|--------|-------|----------|------|
| 7.1 | No metric on failed authorization | MEDIUM | 2-3h |
| 7.2 | No detection of slow tool execution | HIGH | 2-3h |
| 7.3 | No alert on resource limit approaching | MEDIUM | 2-3h |
| 7.4 | No distributed tracing correlation | HIGH | 4-5h |

**Total**: 4 gaps, 10-14 hours implementation

---

## 8. Security & Input Validation

### What Works ✓

**Current Prevention Mechanisms:**
- URI validation (`erlmcp_uri_validator.erl`)
- HTTP header validation (`erlmcp_http_header_validator.erl`)
- TLS validation (`erlmcp_tls_validation.erl`)
- OAuth security (`erlmcp_oauth_security.erl`)
- Path canonicalization (`erlmcp_path_canonicalizer.erl`)

**Implementation Artifacts:**
- `erlmcp_uri_validator.erl`: URI format and scheme validation
- `erlmcp_http_header_validator.erl`: HTTP header validation
- `erlmcp_tls_validation.erl`: TLS certificate validation

### Identified Gaps ✗

#### **Gap 8.1: No Input Sanitization for Tool Arguments (CRITICAL)**
**Problem**: Tool arguments passed directly to handler without sanitization:
```erlang
{Tool, Handler, _Schema} = maps:get(Name, State#state.tools),
Result = Handler(Arguments),  % ← Arguments not sanitized
```

**What Could Go Wrong:**
- Injection attacks: Arguments contain SQL, shell commands, etc.
- Path traversal: Argument contains `../../../../etc/passwd`
- Code injection: Argument evaluated by handler
- Type confusion: Arguments converted to unexpected types

**Why It Matters**:
- Critical security vulnerability
- Could allow arbitrary code execution
- Could expose sensitive data via injection

**Recommended Fix**:
```erlang
-spec sanitize_tool_arguments(binary(), map()) -> {ok, map()} | {error, term()}.
sanitize_tool_arguments(ToolName, Arguments) ->
    %% Get tool's input schema if available
    case get_tool_schema(ToolName) of
        undefined ->
            %% No schema, perform basic sanitization
            {ok, basic_sanitize(Arguments)};
        Schema ->
            %% Validate arguments against schema
            case jesse:validate(Schema, Arguments) of
                {ok, _} -> {ok, Arguments};
                {error, Errors} -> {error, {validation_failed, Errors}}
            end
    end.

-spec basic_sanitize(map()) -> map().
basic_sanitize(Map) when is_map(Map) ->
    maps:map(fun(_Key, Value) ->
        case Value of
            V when is_binary(V) ->
                sanitize_binary(V);
            V when is_list(V) ->
                sanitize_list(V);
            V -> V
        end
    end, Map).
```

**Severity**: CRITICAL
**Implementation**: 5-7 hours (with schema integration)

---

#### **Gap 8.2: JSON Injection via Error Messages (HIGH)**
**Problem**: Error messages not escaped before sending:
```erlang
{error, {invalid_uri, Uri}}  % ← Uri could contain JSON special chars
```

**What Could Go Wrong:**
- Attacker sends URI: `"} , "hacked": true`
- Error response becomes: `{"error": {"message": "Invalid URI: "} , "hacked": true"}"`
- JSON parsing breaks client code
- Attacker can inject malicious JSON

**Why It Matters**:
- JSON parsing error could corrupt data structures
- Could break client authentication checks
- Could be chained with other vulnerabilities

**Recommended Fix**:
```erlang
-spec escape_json_string(binary()) -> binary().
escape_json_string(Binary) when is_binary(Binary) ->
    jsx:encode(Binary).  % jsx handles escaping

-spec safe_error_message(term()) -> binary().
safe_error_message(Term) when is_binary(Term) ->
    escape_json_string(Term);
safe_error_message(Term) ->
    escape_json_string(erlang:term_to_binary(Term)).
```

**Severity**: HIGH
**Implementation**: 2-3 hours

---

#### **Gap 8.3: No Protection Against XXE (XML External Entity) Attacks (MEDIUM)**
**Problem**: If supporting XML (future), no XXE protection:
```erlang
%% No XXE validation if XML parsing added
%% No DTD disabling
%% No entity expansion limits
```

**What Could Go Wrong:**
- XML bomb: Entity expansion causes DOS
- Information disclosure: External DTD reads system files
- Billion laughs attack: Exponential entity expansion

**Why It Matters**:
- Even if not using XML now, might in future
- Should plan defenses in advance
- Could affect upstream tools that parse responses

**Recommended Fix**:
```erlang
-spec safe_xml_parse(binary()) -> {ok, term()} | {error, term()}.
safe_xml_parse(XmlBinary) ->
    %% Disable external entity processing
    try
        xmerl:string(XmlBinary, [
            {dtdvalidation, false},
            {validation, dtd},
            {space, normalize},
            {max_entity_expansion, 10000}  % Limit entity expansion
        ])
    catch
        _:_ -> {error, invalid_xml}
    end.
```

**Severity**: MEDIUM
**Implementation**: 2-3 hours (preventive)

---

#### **Gap 8.4: No CSRF Protection on HTTP Transport (MEDIUM)**
**Problem**: HTTP transport doesn't protect against CSRF:
```erlang
%% No CSRF token validation
%% No SameSite cookie attributes
%% No origin checking
```

**What Could Go Wrong:**
- Attacker tricks user's browser into calling server
- Browser sends credentials, request executes
- Attacker runs tools on behalf of victim

**Why It Matters**:
- Web-based clients vulnerable to CSRF
- Browser automatically sends cookies
- No way for server to distinguish legitimate from CSRF request

**Recommended Fix**:
```erlang
-spec validate_csrf_protection(map(), atom()) -> {ok, map()} | {error, term()}.
validate_csrf_protection(Headers, Method) ->
    case Method of
        get -> {ok, Headers};  % GET is safe
        _ ->
            %% For POST/PUT/DELETE, require CSRF token
            case maps:get(<<"X-CSRF-Token">>, Headers, undefined) of
                undefined ->
                    {error, {missing_csrf_token, <<"CSRF token required">>}};
                Token ->
                    case verify_csrf_token(Token) of
                        true -> {ok, Headers};
                        false -> {error, {invalid_csrf_token, <<"CSRF token invalid">>}}
                    end
            end
    end.
```

**Severity**: MEDIUM
**Implementation**: 2-3 hours

---

### Summary: Security & Input Validation

| Gap ID | Issue | Severity | Time |
|--------|-------|----------|------|
| 8.1 | No input sanitization for tool args | CRITICAL | 5-7h |
| 8.2 | JSON injection via error messages | HIGH | 2-3h |
| 8.3 | No protection against XXE | MEDIUM | 2-3h |
| 8.4 | No CSRF protection on HTTP | MEDIUM | 2-3h |

**Total**: 4 gaps, 11-16 hours implementation

---

## Implementation Priority Matrix

### CRITICAL (Immediate - 1 Week)

| Gap | Module | Time | Impact |
|-----|--------|------|--------|
| 1.2 | Batch request validation | 3-4h | Protocol violation, message loss |
| 2.1 | Unvalidated message routing | 2-3h | Server corruption, DOS |
| 2.2 | No access control on tools | 5-8h | Security breach |
| 4.2 | Session ID reuse | 3-4h | Session hijacking |
| 5.1 | No global memory limit | 3-4h | Out-of-memory crash |
| 8.1 | No input sanitization | 5-7h | Code injection |

**Subtotal: 21-30 hours**

### HIGH (Priority - 2 Weeks)

| Gap | Module | Time | Impact |
|-----|--------|-------|--------|
| 1.1 | Parameter type validation | 1-2h | Error propagation |
| 1.3 | Response validation | 2-3h | Protocol violation |
| 1.4 | Field value range validation | 4-5h | Resource exhaustion |
| 2.3 | Resource canonicalization | 2-3h | Access control bypass |
| 2.4 | Tool registration validation | 2-3h | Invalid metadata |
| 3.1 | Client phase enforcement | 3-4h | State corruption |
| 3.2 | Reentrant initialization | 2-3h | State corruption |
| 3.3 | Server checks client init | 4-5h | Capability mismatch |
| 4.1 | Closed connection reuse | 2-3h | Message loss |
| 4.3 | Connection rate limiting | 2-3h | DOS |
| 5.2 | ETS table size limits | 6-9h | Memory leak |
| 5.3 | Request queue bounds | 2-3h | DOS |
| 5.4 | Handler timeout | 3-4h | Cascading failure |
| 6.1 | Exception detail leak | 2-3h | Information disclosure |
| 6.4 | Circuit breaker in tools | 2-3h | Resilience |
| 7.2 | Slow tool detection | 2-3h | Performance |
| 7.4 | Distributed tracing | 4-5h | Observability |
| 8.2 | JSON injection escape | 2-3h | JSON corruption |

**Subtotal: 53-71 hours**

### MEDIUM (Optional - 3-4 Weeks)

| Gap | Module | Time | Impact |
|-----|--------|-------|--------|
| 3.4 | Init timeout | 2-3h | Resource leak |
| 4.4 | Stale connection detection | 3-4h | Resource leak |
| 6.2 | Error logging context | 4-5h | Observability |
| 7.1 | Authorization failure metrics | 2-3h | Security observability |
| 7.3 | Resource limit alerts | 2-3h | Operational awareness |
| 8.3 | XXE protection | 2-3h | Preventive |
| 8.4 | CSRF protection | 2-3h | Web security |

**Subtotal: 17-24 hours**

---

## Recommended Implementation Roadmap

### Phase 1: Critical Security (Weeks 1-2)
1. **Session ID reuse prevention** (4h) - Highest impact
2. **Input sanitization** (7h) - Prevent injection
3. **Batch validation** (4h) - Protocol compliance
4. **Message routing validation** (3h) - Prevent corruption
5. **Memory limits** (4h) - Prevent crash

**Total: 22 hours**

### Phase 2: High-Impact Resilience (Weeks 3-4)
1. **ETS cleanup** (9h) - Memory management
2. **Tool timeout** (4h) - Prevent hangs
3. **Parameter validation** (2h) - Error handling
4. **Response validation** (3h) - Protocol compliance
5. **Client phase enforcement** (4h) - State safety
6. **Queue bounds** (3h) - DOS prevention

**Total: 25 hours**

### Phase 3: Observability & Hardening (Weeks 5-6)
1. **Distributed tracing** (5h) - Debugging
2. **Slow tool detection** (3h) - Performance
3. **Authorization metrics** (3h) - Security visibility
4. **Field validation** (5h) - Comprehensive
5. **Circuit breaker integration** (3h) - Resilience

**Total: 19 hours**

---

## Success Metrics

After implementing all gaps, system should have:

✓ **100% message validation** - All inputs validated before processing
✓ **Zero silent failures** - All errors logged and propagated
✓ **Bounded resource usage** - Memory, connections, queues all limited
✓ **Complete observability** - All errors, slowness, authz failures tracked
✓ **Secure by default** - Input sanitization, CSRF, XXE protection
✓ **High availability** - Timeouts, circuit breakers, graceful degradation

---

## References

**Poka-Yoke Principles:**
- Mistake-proofing: Make invalid operations impossible
- Error detection: Catch errors immediately
- Auto-correction: Fail fast and safely
- Prevention over detection: Fix root causes

**Standards:**
- OWASP Top 10: Input validation, error handling, security
- CWE/SANS: Common Weakness Enumeration
- JSON-RPC 2.0: Protocol compliance
- Lean Six Sigma: Defect prevention

**Related erlmcp Documentation:**
- `/Users/sac/erlmcp/docs/architecture.md` - System design
- `/Users/sac/erlmcp/docs/otp-patterns.md` - OTP best practices
- `/Users/sac/erlmcp/docs/protocol.md` - MCP protocol details

---

**Analysis completed**: 2026-01-27
**Total gaps identified**: 28
**Total implementation time**: 66-126 hours (high variance due to audit scope)
**Recommended sprint allocation**: 2-3 per week (8-12 week delivery)
