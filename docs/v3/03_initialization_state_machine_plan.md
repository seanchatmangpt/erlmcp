# Initialization State Machine Implementation Plan (v3)

**Status**: DRAFT (2026-01-31)
**MCP Specification**: 2025-11-25
**Priority**: P0 (Protocol Compliance)

## Executive Summary

The MCP 2025-11-25 specification mandates a strict initialization state machine:

```
pre_initialization → initialize → initialized → ready
                         ↓             ↓
                      (response)   (notification)
```

**Current State Analysis**:
- Server: Emits `notifications/initialized` after initialize response (line 820)
- Client: Receives and handles `notifications/initialized` (line 1035-1039)
- Gap: Both sides need explicit state transition enforcement and error recovery

## Protocol Requirements

### MCP 2025-11-25 Initialization Flow

**Step 1**: Client sends `initialize` request
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "initialize",
  "params": {
    "protocolVersion": "2025-11-25",
    "capabilities": {...},
    "clientInfo": {...}
  }
}
```

**Step 2**: Server responds with capabilities
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": {
    "protocolVersion": "2025-11-25",
    "capabilities": {...},
    "serverInfo": {...}
  }
}
```

**Step 3**: Server MUST send `notifications/initialized`
```json
{
  "jsonrpc": "2.0",
  "method": "notifications/initialized",
  "params": {}
}
```

**Step 4**: Client transitions to `initialized` phase
- Only after receiving `notifications/initialized`
- All other requests blocked until this phase

### Phase Definitions

| Phase | Server State | Client State | Allowed Operations |
|-------|-------------|--------------|-------------------|
| `pre_initialization` | Not applicable | Transport connected | `initialize` only |
| `initializing` | Received `initialize` | Sent `initialize`, awaiting `initialized` | Server: send response + notification; Client: await notification |
| `initialized` | Sent `initialized` | Received `initialized` | All MCP methods |
| `error` | Initialization failed | Initialization failed | Cleanup, reconnection |
| `closed` | Connection terminated | Connection terminated | None |

## Server-Side Implementation

### Current State (erlmcp_server.erl)

**Lines 818-820**: Server ALREADY emits `notifications/initialized`
```erlang
%% Send notifications/initialized after successful initialize
%% This is REQUIRED by MCP 2025-11-25 specification (Gap P0-1)
send_notification_via_registry(State, ?MCP_METHOD_INITIALIZED, #{})
```

**Line 827**: Server transitions to `initialized` phase
```erlang
phase = ?MCP_PHASE_INITIALIZED
```

### Required Server Enhancements

#### 1. Phase Transition Enforcement

**Location**: `erlmcp_server.erl:handle_request/5`

**Current Code** (line 774):
```erlang
handle_request(Id, ?MCP_METHOD_INITIALIZE, Params, TransportId,
               #state{initialized = false} = State) ->
```

**Required Enhancement**:
```erlang
%% Initialize request - ONLY allowed in initialization phase
handle_request(Id, ?MCP_METHOD_INITIALIZE, Params, TransportId,
               #state{phase = ?MCP_PHASE_INITIALIZATION, initialized = false} = State) ->
    ...existing code...

handle_request(Id, ?MCP_METHOD_INITIALIZE, _Params, _TransportId,
               #state{phase = CurrentPhase} = State) when CurrentPhase =/= ?MCP_PHASE_INITIALIZATION ->
    logger:error("Initialize request rejected in phase ~p", [CurrentPhase]),
    send_error_via_registry(State, TransportId, Id, ?JSONRPC_INVALID_REQUEST,
                           <<"Initialize must be called in initialization phase">>),
    {noreply, State}.
```

#### 2. Request Blocking Before Initialization

**Location**: `erlmcp_server.erl:handle_request/5` (add at top of function)

```erlang
%% STRICT INITIALIZATION ENFORCEMENT (P0 Security)
%% All non-initialize requests rejected before initialization completes
handle_request(_Id, _Method, _Params, _TransportId,
               #state{phase = Phase} = State) when Phase =/= ?MCP_PHASE_INITIALIZED ->
    logger:warning("Request rejected - server not initialized (phase: ~p)", [Phase]),
    send_error_via_registry(State, TransportId, _Id, ?JSONRPC_INVALID_REQUEST,
                           ?MCP_MSG_PHASE_VIOLATION),
    {noreply, State};
```

#### 3. Timeout Handling

**Location**: `erlmcp_server.erl:handle_info/2` (add new clause)

```erlang
%% Handle initialization timeout
handle_info({initialization_timeout, TransportId},
            #state{phase = ?MCP_PHASE_INITIALIZATION} = State) ->
    logger:error("Initialization timeout for transport ~p - client did not complete initialization",
                 [TransportId]),
    %% Emit telemetry event
    telemetry:execute(
        [erlmcp, server, initialization, timeout],
        #{count => 1},
        #{transport_id => TransportId, server_id => State#state.server_id}
    ),
    {noreply, State#state{phase = ?MCP_PHASE_ERROR}};
```

**Initialization**: Set timeout after receiving `initialize` request
```erlang
%% In handle_request for initialize
TimeoutRef = erlang:send_after(State#state.init_timeout_ms, self(),
                               {initialization_timeout, TransportId}),
NewState = State#state{init_timeout_ref = TimeoutRef},
```

**Cleanup**: Cancel timeout on successful initialization
```erlang
%% After sending notifications/initialized
erlang:cancel_timer(NewState#state.init_timeout_ref),
```

## Client-Side Implementation

### Current State (erlmcp_client.erl)

**Lines 1035-1039**: Client ALREADY handles `notifications/initialized`
```erlang
handle_notification(<<"notifications/initialized">> = Method, _Params,
                   #state{phase = initializing} = State) ->
    logger:info("Client received initialized notification, transitioning to initialized phase"),
    {noreply, State#state{phase = initialized, initialized = true}};
```

**Lines 561-569**: Client has initialization timeout
```erlang
handle_info(initialization_timeout, #state{phase = initializing} = State) ->
    logger:error("Initialization timeout - server did not send notifications/initialized within ~p ms",
                 [State#state.timeout]),
    {stop, initialization_timeout, State};
```

### Required Client Enhancements

#### 1. Phase State Type Specification

**Location**: `erlmcp_client.erl` (line 46)

**Current**:
```erlang
-type client_phase() :: pre_initialization | initializing | initialized | error | closed.
```

**Status**: Already correct - no changes needed

#### 2. Initialize Phase Enforcement

**Location**: `erlmcp_client.erl:handle_call/3`

**Current Code** (line 236-246): Already enforces `pre_initialization` check
```erlang
handle_call({initialize, Capabilities, _Options}, From,
           #state{phase = pre_initialization} = State) ->
    ...initialization code...

handle_call({initialize, _Capabilities, _Options}, From,
           #state{phase = Phase} = State) ->
    gen_server:reply(From, {error, {invalid_phase, Phase,
        <<"Initialize must be called in pre_initialization phase">>}}),
    {noreply, State};
```

**Status**: Already correct - no changes needed

#### 3. Request Blocking Before Initialized

**Location**: All `handle_call` clauses for MCP methods

**Current Code** (example line 255-266):
```erlang
handle_call(list_resources, From, #state{phase = initialized} = State) ->
    ...handle request...

handle_call(list_resources, From, #state{phase = Phase} = State) ->
    gen_server:reply(From, {error, {not_initialized, Phase,
        <<"Server must complete initialization first">>}}),
    {noreply, State};
```

**Status**: Already correct - all methods enforce `initialized` phase

#### 4. Enhanced Timeout Recovery

**Location**: `erlmcp_client.erl:handle_info/2`

**Current Code** (line 561-569): Timeout stops the client

**Enhancement**: Add reconnection capability
```erlang
handle_info(initialization_timeout, #state{phase = initializing} = State) ->
    logger:error("Initialization timeout - server did not send notifications/initialized within ~p ms",
                 [State#state.timeout]),

    %% Emit telemetry event
    telemetry:execute(
        [erlmcp, client, initialization, timeout],
        #{count => 1},
        #{timeout => State#state.timeout, transport => State#state.transport}
    ),

    case State#state.auto_reconnect of
        true ->
            logger:info("Attempting reconnection after initialization timeout"),
            %% Trigger reconnection logic
            {noreply, State#state{phase = error}};
        false ->
            {stop, initialization_timeout, State}
    end;
```

## State Transition Diagram

```
                    ┌─────────────────────────────────────────┐
                    │         INITIALIZATION STATE MACHINE        │
                    └─────────────────────────────────────────┘

SERVER SIDE:                    CLIENT SIDE:
-------------                    -------------

[initialization]
    │
    │ receives 'initialize'
    ├──────────────────────────────────────────────┐
    │                                              │
    ▼                                              │
[initializing]                                      │
    │                                              │
    │ validates capabilities                        │ sends 'initialize' ────► [pre_initialization]
    │                                              │                              │
    │ sends response                                │                              │
    ├─────────────────────────────────────────────────────────────────────────────┤
    │                                              │                              │
    │ sends notifications/initialized ─────────────┼──────────────────────────────┘
    │                                              │
    ▼                                              ▼
[initialized] ◄─────────────────────────────── [initialized]
    │                                              │
    │ accepts all requests                         │ accepts all requests
    │                                              │
    └──────────────────────────────────────────────┘

ERROR PATHS:
  - Protocol version mismatch
  - Invalid client info
  - Initialization timeout (30s default)
  - Missing notifications/initialized
```

## Test Specifications

### Unit Tests (erlmcp_server_tests.erl)

```erlang
%% Test 1: Server emits initialized notification
server_emits_initialized_notification_test() ->
    {ok, Server} = erlmcp_server:start_link(test_server, #mcp_server_capabilities{}),

    %% Send initialize request
    InitializeReq = #{
        <<"protocolVersion">> => <<"2025-11-25">>,
        <<"capabilities">> => #{},
        <<"clientInfo">> => #{<<"name">> => <<"test">>, <<"version">> => <<"1.0.0">>}
    },

    %% Verify response received
    ?assertMatch({ok, _}, erlmcp_server:handle_request(1, <<"initialize">>,
                                                       InitializeReq, test_transport, State)),

    %% Verify initialized notification sent
    ?assertReceived(#{<<"jsonrpc">> => <<"2.0">>,
                      <<"method">> => <<"notifications/initialized">>}),

    %% Verify state transition
    ?assertEqual(?MCP_PHASE_INITIALIZED, State#state.phase),
    ?assertEqual(true, State#state.initialized).

%% Test 2: Server rejects requests before initialization
server_rejects_requests_before_initialization_test() ->
    {ok, Server} = erlmcp_server:start_link(test_server, #mcp_server_capabilities{}),

    %% Try to call tools/list before initialization
    ?assertMatch({error, ?JSONRPC_INVALID_REQUEST},
                 erlmcp_server:handle_request(1, <<"tools/list">>, #{},
                                             test_transport, State)),

    %% Verify phase unchanged
    ?assertEqual(?MCP_PHASE_INITIALIZATION, State#state.phase).

%% Test 3: Server rejects duplicate initialize requests
server_rejects_duplicate_initialize_test() ->
    {ok, Server} = erlmcp_server:start_link(test_server, #mcp_server_capabilities{}),

    %% First initialize succeeds
    ?assertMatch({ok, _}, erlmcp_server:handle_request(1, <<"initialize">>,
                                                       InitParams, test_transport, State)),

    %% Second initialize fails
    ?assertMatch({error, ?JSONRPC_INVALID_REQUEST},
                 erlmcp_server:handle_request(2, <<"initialize">>,
                                             InitParams, test_transport, State2)).

%% Test 4: Initialization timeout triggers error phase
server_initialization_timeout_test() ->
    {ok, Server} = erlmcp_server:start_link(test_server, #mcp_server_capabilities{}),
    State = State#state{init_timeout_ms = 100},  % Short timeout for testing

    %% Trigger timeout
    timer:sleep(150),

    %% Verify phase transition to error
    ?assertEqual(?MCP_PHASE_ERROR, State#state.phase).
```

### Unit Tests (erlmcp_client_tests.erl)

```erlang
%% Test 1: Client transitions to initialized after notification
client_transitions_to_initialized_test() ->
    {ok, Client} = erlmcp_client:start_link({stdio, []}, #{}),

    %% Send initialize request
    {ok, ServerCaps} = erlmcp_client:initialize(Client, #mcp_client_capabilities{}),

    %% Verify client in initializing phase
    ?assertEqual(initializing, get_phase(Client)),

    %% Simulate server sending initialized notification
    send_notification(Client, #{<<"jsonrpc">> => <<"2.0">>,
                               <<"method">> => <<"notifications/initialized">>,

                               <<"params">> => #{}}),

    %% Verify client in initialized phase
    ?assertEqual(initialized, get_phase(Client)),
    ?assertEqual(true, get_initialized_flag(Client)).

%% Test 2: Client rejects requests before initialized
client_rejects_requests_before_initialized_test() ->
    {ok, Client} = erlmcp_client:start_link({stdio, []}, #{}),

    %% Try to call list_resources before initialization
    ?assertMatch({error, {not_initialized, pre_initialization, _}},
                 erlmcp_client:list_resources(Client)).

%% Test 3: Client handles initialization timeout
client_initialization_timeout_test() ->
    {ok, Client} = erlmcp_client:start_link({stdio, []}, #{timeout => 100}),

    %% Send initialize request
    erlmcp_client:initialize(Client, #mcp_client_capabilities{}),

    %% Wait for timeout
    timer:sleep(150),

    %% Verify client stopped or in error phase
    ?assert(not is_process_alive(Client) orelse get_phase(Client) =:= error).

%% Test 4: Client rejects duplicate initialize
client_rejects_duplicate_initialize_test() ->
    {ok, Client} = erlmcp_client:start_link({stdio, []}, #{}),

    %% First initialize
    {ok, _} = erlmcp_client:initialize(Client, #mcp_client_capabilities{}),

    %% Receive initialized notification
    send_initialized_notification(Client),

    %% Second initialize should fail
    ?assertMatch({error, {invalid_phase, initialized, _}},
                 erlmcp_client:initialize(Client, #mcp_client_capabilities{})).
```

### Integration Tests

```erlang
%% Test 1: End-to-end initialization flow
e2e_initialization_flow_test() ->
    %% Start server and client
    {ok, Server} = erlmcp_server:start_link(test_server, #mcp_server_capabilities{}),
    {ok, Client} = erlmcp_client:start_link({stdio, []}, #{}),

    %% Client sends initialize
    {ok, ServerCaps} = erlmcp_client:initialize(Client, #mcp_client_capabilities{}),

    %% Verify server received initialize
    ?assertEqual(true, received_initialize(Server)),

    %% Verify client received initialized notification
    ?assertEqual(initialized, get_phase(Client)),

    %% Verify both sides ready for requests
    ?assertMatch({ok, _}, erlmcp_client:list_tools(Client)),
    ?assertEqual(true, can_handle_requests(Server)).

%% Test 2: Server violation - missing initialized notification
server_missing_initialized_notification_test() ->
    %% Mock server that doesn't send initialized notification
    {ok, BadServer} = start_broken_server(omit_initialized_notification),

    {ok, Client} = erlmcp_client:start_link({stdio, []}, #{timeout => 1000}),

    %% Client sends initialize
    erlmcp_client:initialize(Client, #mcp_client_capabilities{}),

    %% Verify client times out and errors
    timer:sleep(1100),
    ?assert(not is_process_alive(Client) orelse get_phase(Client) =:= error).

%% Test 3: Client violation - requests before initialization
client_requests_before_initialization_test() ->
    {ok, Server} = erlmcp_server:start_link(test_server, #mcp_server_capabilities{}),
    {ok, Client} = erlmcp_client:start_link({stdio, []}, #{}),

    %% Try to call tool without initializing
    ?assertMatch({error, {not_initialized, pre_initialization, _}},
                 erlmcp_client:call_tool(Client, <<"test">>, #{})).

%% Test 4: Phase enforcement on both sides
phase_enforcement_test() ->
    %% Server initialized, client not
    {ok, Server} = erlmcp_server:start_link(test_server, #mcp_server_capabilities{}),
    initialize_server(Server),

    {ok, Client} = erlmcp_client:start_link({stdio, []}, #{}),

    %% Client tries to call server method
    ?assertMatch({error, {not_initialized, pre_initialization, _}},
                 erlmcp_client:list_tools(Client)).

%% Test 5: Recovery from initialization failure
initialization_failure_recovery_test() ->
    {ok, Server} = erlmcp_server:start_link(test_server, #mcp_server_capabilities{}),

    %% Client with auto-reconnect enabled
    {ok, Client} = erlmcp_client:start_link({stdio, []},
                                            #{timeout => 100,
                                              auto_reconnect => true}),

    %% Trigger timeout by not sending initialized notification
    erlmcp_client:initialize(Client, #mcp_client_capabilities{}),
    timer:sleep(150),

    %% Verify client in error phase (not stopped)
    ?assertEqual(error, get_phase(Client)),

    %% Fix server to send initialized notification
    fix_server(Server),

    %% Trigger reconnection
    erlmcp_client:reconnect(Client),

    %% Verify successful initialization
    ?assertEqual(initialized, get_phase(Client)).
```

## Error Cases and Recovery

### Error Case 1: Protocol Version Mismatch

**Server Detection** (line 800-835):
```erlang
case erlmcp_capabilities:validate_protocol_version(ProtocolVersion) of
    ok ->
        ...proceed with initialization...
    {error, ErrorMsg} ->
        send_error_via_registry(State, TransportId, Id,
                               ?MCP_ERROR_UNSUPPORTED_PROTOCOL_VERSION, ErrorMsg),
        {noreply, State#state{phase = ?MCP_PHASE_ERROR}}
end
```

**Client Recovery**:
- Log protocol version mismatch
- Stop client with `{error, unsupported_protocol_version}`
- User must upgrade/downgrade to compatible version

### Error Case 2: Missing Client Info

**Server Detection** (line 786-793):
```erlang
case validate_client_info(Params) of
    ok ->
        ...proceed...
    {error, ClientInfoError} ->
        send_error_via_registry(State, TransportId, Id,
                               ?JSONRPC_INVALID_PARAMS, ClientInfoError),
        throw({client_info_error, ClientInfoError})
end
```

**Client Recovery**:
- Fix initialize request to include `clientInfo`
- Retry initialize

### Error Case 3: Initialization Timeout

**Server Timeout**:
```erlang
handle_info({initialization_timeout, TransportId},
            #state{phase = ?MCP_PHASE_INITIALIZATION} = State) ->
    logger:error("Initialization timeout for transport ~p", [TransportId]),
    {noreply, State#state{phase = ?MCP_PHASE_ERROR}}
```

**Client Timeout** (line 561-569):
```erlang
handle_info(initialization_timeout, #state{phase = initializing} = State) ->
    logger:error("Initialization timeout"),
    case State#state.auto_reconnect of
        true -> {noreply, State#state{phase = error}};
        false -> {stop, initialization_timeout, State}
    end
```

**Recovery**:
- Server: Close connection, wait for reconnect
- Client: Reconnect with backoff (if `auto_reconnect = true`)
- If `auto_reconnect = false`, stop client

### Error Case 4: Duplicate Initialize Requests

**Server Detection**:
```erlang
handle_request(_Id, ?MCP_METHOD_INITIALIZE, _Params, _TransportId,
               #state{phase = Phase} = State) when Phase =/= ?MCP_PHASE_INITIALIZATION ->
    logger:error("Duplicate initialize request in phase ~p", [Phase]),
    send_error_via_registry(State, TransportId, _Id, ?JSONRPC_INVALID_REQUEST,
                           <<"Initialize already completed">>),
    {noreply, State}
```

**Client Detection** (line 244-246):
```erlang
handle_call({initialize, _Capabilities, _Options}, From,
           #state{phase = Phase} = State) ->
    gen_server:reply(From, {error, {invalid_phase, Phase,
        <<"Initialize must be called in pre_initialization phase">>}}),
    {noreply, State}
```

### Error Case 5: Requests Before Initialization

**Server Detection**:
```erlang
handle_request(_Id, _Method, _Params, _TransportId,
               #state{phase = Phase} = State) when Phase =/= ?MCP_PHASE_INITIALIZED ->
    logger:warning("Request rejected - server not initialized (phase: ~p)", [Phase]),
    send_error_via_registry(State, TransportId, _Id, ?JSONRPC_INVALID_REQUEST,
                           ?MCP_MSG_PHASE_VIOLATION),
    {noreply, State}
```

**Client Detection** (line 265-266):
```erlang
handle_call(list_resources, From, #state{phase = Phase} = State) ->
    gen_server:reply(From, {error, {not_initialized, Phase,
        <<"Server must complete initialization first">>}}),
    {noreply, State}
```

## Implementation Checklist

### Server-Side (erlmcp_server.erl)

- [x] **DONE**: Emit `notifications/initialized` after initialize response (line 820)
- [x] **DONE**: Transition to `initialized` phase (line 827)
- [ ] **TODO**: Add phase enforcement at top of `handle_request/5`
- [ ] **TODO**: Add timeout initialization on `initialize` request
- [ ] **TODO**: Cancel timeout on successful initialization
- [ ] **TODO**: Add timeout handler in `handle_info/2`
- [ ] **TODO**: Add telemetry events for phase transitions

### Client-Side (erlmcp_client.erl)

- [x] **DONE**: Handle `notifications/initialized` notification (line 1035-1039)
- [x] **DONE**: Transition to `initialized` phase (line 1039)
- [x] **DONE**: Initialize phase enforcement (line 236-246)
- [x] **DONE**: Request blocking before initialized (line 255-266)
- [x] **DONE**: Initialization timeout handler (line 561-569)
- [ ] **TODO**: Add auto-reconnect logic on timeout
- [ ] **TODO**: Add telemetry events for phase transitions

### Tests (erlmcp_*_tests.erl)

- [ ] **TODO**: `server_emits_initialized_notification_test/0`
- [ ] **TODO**: `server_rejects_requests_before_initialization_test/0`
- [ ] **TODO**: `server_rejects_duplicate_initialize_test/0`
- [ ] **TODO**: `server_initialization_timeout_test/0`
- [ ] **TODO**: `client_transitions_to_initialized_test/0`
- [ ] **TODO**: `client_rejects_requests_before_initialized_test/0`
- [ ] **TODO**: `client_initialization_timeout_test/0`
- [ ] **TODO**: `client_rejects_duplicate_initialize_test/0`
- [ ] **TODO**: `e2e_initialization_flow_test/0`
- [ ] **TODO**: `server_missing_initialized_notification_test/0`
- [ ] **TODO**: `client_requests_before_initialization_test/0`
- [ ] **TODO**: `phase_enforcement_test/0`
- [ ] **TODO**: `initialization_failure_recovery_test/0`

## Telemetry Events

### Server Events

```erlang
%% Initialization started
telemetry:execute(
    [erlmcp, server, initialization, start],
    #{count => 1},
    #{server_id => ServerId, transport_id => TransportId}
).

%% Initialization completed
telemetry:execute(
    [erlmcp, server, initialization, complete],
    #{count => 1, duration_us => Duration},
    #{server_id => ServerId, protocol_version => ProtocolVersion}
).

%% Initialization failed
telemetry:execute(
    [erlmcp, server, initialization, failed],
    #{count => 1},
    #{server_id => ServerId, reason => Reason, phase => Phase}
).

%% Initialization timeout
telemetry:execute(
    [erlmcp, server, initialization, timeout],
    #{count => 1},
    #{server_id => ServerId, transport_id => TransportId, timeout_ms => Timeout}
).

%% Phase transition
telemetry:execute(
    [erlmcp, server, phase, transition],
    #{count => 1},
    #{server_id => ServerId, from => OldPhase, to => NewPhase}
).
```

### Client Events

```erlang
%% Initialization started
telemetry:execute(
    [erlmcp, client, initialization, start],
    #{count => 1},
    #{client => Client, transport => Transport}
).

%% Initialization completed
telemetry:execute(
    [erlmcp, client, initialization, complete],
    #{count => 1, duration_us => Duration},
    #{client => Client, server_capabilities => Caps}
).

%% Initialization failed
telemetry:execute(
    [erlmcp, client, initialization, failed],
    #{count => 1},
    #{client => Client, reason => Reason, phase => Phase}
).

%% Initialization timeout
telemetry:execute(
    [erlmcp, client, initialization, timeout],
    #{count => 1},
    #{client => Client, transport => Transport, timeout_ms => Timeout}
).

%% Phase transition
telemetry:execute(
    [erlmcp, client, phase, transition],
    #{count => 1},
    #{client => Client, from => OldPhase, to => NewPhase}
).
```

## Compliance Matrix

| Requirement | Server Status | Client Status | Test Coverage |
|-------------|---------------|---------------|---------------|
| Emit `notifications/initialized` | ✅ Implemented | N/A | ⚠️ Pending |
| Receive `notifications/initialized` | N/A | ✅ Implemented | ⚠️ Pending |
| Phase transition on init | ✅ Implemented | ✅ Implemented | ⚠️ Pending |
| Block requests before init | ⚠️ Partial | ✅ Implemented | ⚠️ Pending |
| Reject duplicate init | ⚠️ Partial | ✅ Implemented | ⚠️ Pending |
| Timeout handling | ❌ Missing | ✅ Implemented | ⚠️ Pending |
| Error recovery | ⚠️ Partial | ⚠️ Partial | ⚠️ Pending |
| Telemetry events | ❌ Missing | ❌ Missing | ❌ Missing |

**Legend**:
- ✅ Complete
- ⚠️ Partial/Needs Enhancement
- ❌ Missing

## References

- **MCP Specification**: 2025-11-25
- **Server Module**: `/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_server.erl`
- **Client Module**: `/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_client.erl`
- **Header File**: `/Users/sac/erlmcp/apps/erlmcp_core/include/erlmcp.hrl`
- **OTP Patterns**: `/Users/sac/erlmcp/docs/otp-patterns.md`

## Appendix: State Transition Table

### Server Phase Transitions

| Current Phase | Event | Next Phase | Action |
|---------------|-------|------------|--------|
| `initialization` | Receive `initialize` (valid) | `initializing` | Validate capabilities, send response |
| `initializing` | Send `notifications/initialized` | `initialized` | Set `initialized = true`, cancel timeout |
| `initializing` | Timeout (30s) | `error` | Log error, close connection |
| `initialized` | Any request | `initialized` | Process request |
| `any` | Transport disconnect | `closed` | Cleanup resources |
| `any` | Invalid phase request | `unchanged` | Return error, reject request |

### Client Phase Transitions

| Current Phase | Event | Next Phase | Action |
|---------------|-------|------------|--------|
| `pre_initialization` | Send `initialize` | `initializing` | Start timeout timer |
| `initializing` | Receive `notifications/initialized` | `initialized` | Cancel timeout, ready for requests |
| `initializing` | Timeout (default 5000ms) | `error` | Trigger reconnection or stop |
| `initialized` | Any request | `initialized` | Process request |
| `any` | Transport disconnect | `closed` | Cleanup resources |
| `any` | Invalid phase request | `unchanged` | Return error, reject request |

---

**Document Version**: 1.0
**Last Updated**: 2026-01-31
**Author**: Erlang OTP Developer Agent
**Review Status**: Pending Review
