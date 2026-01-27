# MCP Initialization Phase Enforcement

## Overview

This document describes the initialization phase enforcement system implemented in erlmcp to prevent early requests during the MCP protocol handshake and ensure proper state management.

## Problem Statement

The MCP protocol requires a strict handshake sequence:

1. Client sends `initialize` request
2. Server responds with capabilities
3. Client sends `initialized` notification
4. Server and client transition to operation phase
5. Both sides can now exchange capability requests

Without enforcement, early requests (before step 3) could cause protocol violations, state inconsistencies, and undefined behavior.

## Solution Architecture

### Lifecycle Phases

Both client and server track their lifecycle phase:

```erlang
%% Server phases
-type lifecycle_phase() :: initialization | operation | shutdown.

%% Client phases
-type client_phase() :: pre_initialization | initializing | initialized | error | closed.
```

### Server State

Added to `erlmcp_server.erl`:

```erlang
-record(state, {
    server_id :: server_id(),
    phase = initialization :: lifecycle_phase(),
    capabilities :: #mcp_server_capabilities{},
    %% ... other fields
    initialized = false :: boolean()
}).
```

### Client State

Added to `erlmcp_client.erl`:

```erlang
-record(state, {
    transport :: module(),
    transport_state :: term(),
    phase = pre_initialization :: client_phase(),
    capabilities :: #mcp_server_capabilities{} | undefined,
    %% ... other fields
    initialized = false :: boolean()
}).
```

## Phase Enforcement

### Server-Side Enforcement

#### 1. Initialization Phase

During `phase = initialization`:

**Allowed:**
- `initialize` request - triggers response with server capabilities
- `ping` - always allowed for monitoring
- `initialized` notification - transitions to operation phase

**Blocked:**
- All capability requests (resources/*, tools/*, prompts/*)
- All other requests

**Response:** `{error, {initialization_not_complete, "Server initialization in progress"}}`

#### 2. Operation Phase

During `phase = operation`:

**Allowed:**
- All negotiated capability requests
- Ping

**Blocked:**
- `initialize` request - returns error (too late)
- Requests for non-negotiated capabilities

**Response:** Proper error codes (e.g., `capability_not_supported`)

#### 3. Shutdown Phase

During `phase = shutdown`:

**Blocked:**
- All requests

**Response:** `{error, shutting_down}`

### Client-Side Enforcement

#### 1. Pre-Initialization Phase

During `phase = pre_initialization`:

**Allowed:**
- `initialize` request - transitions to `initializing` phase

**Blocked:**
- All capability requests

**Response:** `{error, {invalid_phase, pre_initialization, "Initialize must be called in pre_initialization phase"}}`

#### 2. Initializing Phase

During `phase = initializing`:

**Waiting for:**
- `initialize` response - stays in `initializing`
- `initialized` notification - transitions to `initialized`

**Blocked:**
- All capability requests

**Response:** `{error, {not_initialized, initializing, "Server must complete initialization first"}}`

#### 3. Initialized Phase

During `phase = initialized`:

**Allowed:**
- All capability requests
- Subsequent initialize calls are rejected

#### 4. Error/Closed Phases

**Blocked:**
- All requests

## Implementation Details

### Server Implementation

**Phase Transitions:**

```erlang
%% In handle_call - enforce initialization phase
handle_call(Request, From, #state{phase = initialization} = State) ->
    case Request of
        {initialize, _, _} ->
            handle_initialize_request(Request, From, State);
        ping ->
            {reply, pong, State};
        _ ->
            {reply, {error, {initialization_not_complete, ...}}, State}
    end;

handle_call(Request, From, #state{phase = shutdown} = State) ->
    {reply, {error, {shutting_down, ...}}, State};

handle_call(Request, From, #state{phase = operation} = State) ->
    handle_operation_request(Request, From, State).
```

**Initialize Response Handler:**

```erlang
%% In handle_request - initialize request during initialization phase
handle_request(Id, ?MCP_METHOD_INITIALIZE, _Params, TransportId,
               #state{phase = initialization} = State) ->
    Response = build_initialize_response(State#state.capabilities),
    send_response_via_registry(State, TransportId, Id, Response),
    {noreply, State#state{initialized = true}};

%% Reject initialize during operation phase
handle_request(Id, ?MCP_METHOD_INITIALIZE, _Params, TransportId,
               #state{phase = operation} = State) ->
    send_error_via_registry(State, TransportId, Id, ?MCP_ERROR_NOT_INITIALIZED,
        <<"Initialize must be called first during initialization phase">>),
    {noreply, State}.
```

**Phase Transition on Initialized Notification:**

```erlang
%% In handle_notification - transitions to operation on initialized
handle_notification(?MCP_METHOD_INITIALIZED, _Params,
                    #state{phase = initialization} = State) ->
    logger:info("Server ~p transitioning to operation phase", [State#state.server_id]),
    {noreply, State#state{phase = operation}}.
```

**All Capability Requests Now Check Phase:**

```erlang
%% Example: resources/list
handle_request(Id, ?MCP_METHOD_RESOURCES_LIST, _Params, TransportId,
               #state{phase = initialization} = State) ->
    send_error_via_registry(State, TransportId, Id, ?MCP_ERROR_NOT_INITIALIZED,
        <<"Cannot list resources before initialization completes">>),
    {noreply, State};

handle_request(Id, ?MCP_METHOD_RESOURCES_LIST, _Params, TransportId,
               #state{phase = operation} = State) ->
    Resources = list_all_resources(State),
    send_response_via_registry(State, TransportId, Id,
        #{?MCP_PARAM_RESOURCES => Resources}),
    {noreply, State};

handle_request(Id, ?MCP_METHOD_RESOURCES_LIST, _Params, TransportId,
               #state{phase = shutdown} = State) ->
    send_error_via_registry(State, TransportId, Id, ?JSONRPC_INTERNAL_ERROR,
        <<"Server shutting down">>),
    {noreply, State}.
```

### Client Implementation

**Phase Enforcement in handle_call:**

```erlang
%% Initialize must be called during pre_initialization phase
handle_call({initialize, Capabilities, _Options}, From,
            #state{phase = pre_initialization} = State) ->
    Request = build_initialize_request(Capabilities),
    NewState = State#state{phase = initializing},
    send_request(NewState, <<"initialize">>, Request, {initialize, From});

%% Initialize not allowed in other phases
handle_call({initialize, _Capabilities, _Options}, From,
            #state{phase = Phase} = State) ->
    gen_server:reply(From, {error, {invalid_phase, Phase,
        <<"Initialize must be called in pre_initialization phase">>}}),
    {noreply, State}.
```

**Capability Requests Require Initialized Phase:**

```erlang
%% List resources requires initialized phase
handle_call(list_resources, From, #state{phase = initialized} = State) ->
    case ?CHECK_CAPABILITY(State, resources) of
        do_request ->
            send_request(State, <<"resources/list">>, #{}, {list_resources, From});
        {error, _} = ErrorTuple ->
            {reply, ErrorTuple, State}
    end;

handle_call(list_resources, From, #state{phase = Phase} = State) ->
    gen_server:reply(From, {error, {not_initialized, Phase,
        <<"Client not initialized">>}}),
    {noreply, State}.
```

**Initialize Response Handler:**

```erlang
handle_response(Id, Result, State) ->
    case maps:take(Id, State#state.pending_requests) of
        {{initialize, From}, NewPending} ->
            gen_server:reply(From, Result),
            case Result of
                {ok, InitResult} ->
                    ServerCapabilities = extract_server_capabilities(InitResult),
                    %% Stay in initializing phase until initialized notification
                    NewState = State#state{
                        pending_requests = NewPending,
                        capabilities = ServerCapabilities,
                        initialized = true,
                        phase = initializing
                    },
                    {noreply, NewState};
                {error, _} ->
                    %% Go to error phase on initialization failure
                    NewState = State#state{
                        pending_requests = NewPending,
                        phase = error
                    },
                    {noreply, NewState}
            end;
        %% ... handle other responses
    end.
```

**Phase Transition on Initialized Notification:**

```erlang
%% INITIALIZED notification transitions to initialized phase
handle_notification(<<"notifications/initialized">> = _Method, _Params,
                    #state{phase = initializing} = State) ->
    logger:info("Client received initialized notification, transitioning to initialized phase"),
    {noreply, State#state{phase = initialized}}.
```

## Error Codes

### Standard JSON-RPC Errors

- `-32601` (METHOD_NOT_FOUND) - Invalid request method
- `-32602` (INVALID_PARAMS) - Missing required parameters
- `-32603` (INTERNAL_ERROR) - Server error

### MCP-Specific Errors

- `-32005` (?MCP_ERROR_NOT_INITIALIZED) - Server/client not initialized
- `-32004` (?MCP_ERROR_CAPABILITY_NOT_SUPPORTED) - Feature not negotiated
- `-32001` (?MCP_ERROR_RESOURCE_NOT_FOUND) - Resource doesn't exist
- `-32002` (?MCP_ERROR_TOOL_NOT_FOUND) - Tool doesn't exist
- `-32003` (?MCP_ERROR_PROMPT_NOT_FOUND) - Prompt doesn't exist

## Testing

Comprehensive test suite in `test/erlmcp_lifecycle_tests.erl` covers:

1. **Cannot list resources before initialize** - Requests blocked during initialization
2. **Cannot call tool before initialize** - Tool calls blocked during initialization
3. **Cannot get prompt before initialize** - Prompt requests blocked during initialization
4. **Ping allowed during initialization** - Monitoring always allowed
5. **Initialize response is allowed** - Initialize request succeeds
6. **Transitions to operation on initialized notification** - Phase transitions correctly
7. **All requests allowed during operation** - All capabilities work after initialization
8. **Shutdown rejects all requests** - Shutdown phase blocks everything
9. **Initialize during operation rejected** - Can't re-initialize
10. **Capability check before operation** - Non-negotiated features blocked
11. **Proper error codes during initialization** - Correct error responses

## Sequence Diagrams

### Successful Initialization

```
Client                          Server
  |                              |
  |-------- initialize --------->|
  |                   (phase: initialization)
  |<------- initialize response -|
  |                   (phase: initialized=true, still initialization)
  |---- initialized notify ----->|
  |                   (phase: operation)
  |                   (all requests now allowed)
  |-------- resources/list ----->|
  |<------ resources response ----|
```

### Failed Initialization (Early Request)

```
Client                          Server
  |                              |
  |---- resources/list early --->|
  |                   (phase: initialization)
  |<-- error: not_initialized ---|
  |                              |
  |---- initialize request ----->|
  |<--- initialize response ------|
  |---- initialized notify ----->|
  |---- resources/list --------->|
  |<--- resources response ------|
```

## Benefits

1. **Protocol Compliance** - Enforces MCP spec handshake sequence
2. **State Safety** - Prevents undefined state transitions
3. **Error Prevention** - Catches protocol violations early
4. **Debugging** - Clear error messages indicate what's wrong
5. **Monitoring** - Ping allowed anytime for health checks
6. **Clean Shutdown** - Graceful rejection during shutdown

## Configuration

No special configuration required. Phase enforcement is automatic:

- Server starts in `initialization` phase
- Client starts in `pre_initialization` phase
- Transitions happen automatically based on messages

## Backward Compatibility

This implementation maintains backward compatibility:

- Existing code that follows the MCP spec continues to work
- Code that violates the spec gets clear error messages
- No breaking changes to public APIs

## Future Enhancements

1. **Timeout Enforcement** - Add timeouts for slow initializations
2. **Graceful Degradation** - Allow partial initialization with warnings
3. **Recovery Paths** - Support re-initialization on connection loss
4. **Metrics** - Track initialization failures and latency

## Summary

The initialization phase enforcement system ensures erlmcp strictly follows the MCP protocol, preventing early requests and maintaining proper state throughout the client-server lifecycle. This implementation provides strong guarantees about protocol conformance while maintaining backward compatibility and clear error messages for debugging.
