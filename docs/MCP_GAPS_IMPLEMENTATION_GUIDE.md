# MCP 2025-11-25 Compliance Gaps - Implementation Guide

**Purpose**: Detailed implementation guidance for fixing each identified gap
**Status**: Ready for development
**Maintenance**: Update as gaps are completed

---

## How to Use This Guide

1. Start with **Gap #1-10** (Critical path)
2. Follow the **Implementation Steps** for each gap
3. Add the **Recommended Tests** for quality validation
4. Update progress in task tracker
5. Reference **Code Samples** for implementation details

---

## Gap #1: Capability Negotiation Structure

### Problem Statement
Client cannot determine which features server supports. Server doesn't validate client capabilities.

### Why This Matters
- MCP spec requires capability-based operation filtering
- Clients need to know what features are available
- Prevents invalid requests to unsupported features

### Implementation Steps

**Step 1: Update erlmcp.hrl**
```erlang
%% Add to include/erlmcp.hrl
-record(mcp_capabilities, {
    resources :: #{
        subscribe => boolean(),
        listChanged => boolean()
    } | undefined,
    tools :: #{
        listChanged => boolean()
    } | undefined,
    prompts :: #{
        listChanged => boolean()
    } | undefined,
    logging :: #{} | undefined,
    sampling :: #{} | undefined,
    roots :: #{} | undefined,
    experimental :: map() | undefined
}).
```

**Step 2: Update erlmcp_server.erl**
- Add capabilities field to server state (already exists)
- Implement capability negotiation in initialize response
- Add capability validation before allowing operations

**Step 3: Update erlmcp_client.erl**
- Store received capabilities in client state
- Validate capabilities before sending requests
- Return error if required capability not present

### Code Sample

```erlang
%% In erlmcp_server.erl handle_initialize/3
handle_initialize(Params, From, State) ->
    RequestedVersion = maps:get(<<"protocolVersion">>, Params),
    ClientCapabilities = maps:get(<<"capabilities">>, Params),

    ServerCapabilities = #{
        <<"logging">> => #{},
        <<"prompts">> => #{<<"listChanged">> => true},
        <<"resources">> => #{
            <<"subscribe">> => true,
            <<"listChanged">> => true
        },
        <<"tools">> => #{<<"listChanged">> => true},
        <<"sampling">> => #{},
        <<"roots">> => #{}
    },

    Response = #{
        <<"protocolVersion">> => <<"2025-11-25">>,
        <<"capabilities">> => ServerCapabilities,
        <<"serverInfo">> => #{
            <<"name">> => <<"erlmcp">>,
            <<"version">> => <<"0.6.0">>
        }
    },

    NewState = State#state{
        client_capabilities = ClientCapabilities,
        protocol_version = RequestedVersion,
        capabilities = ServerCapabilities,
        initialized = true
    },

    {reply, Response, NewState}.
```

### Recommended Tests
1. Test capability response includes all required fields
2. Test client receives capabilities correctly
3. Test client rejects requests for unsupported capabilities
4. Test server enforces capability-based filtering
5. Test experimental capabilities are handled

### Acceptance Criteria
- [ ] Initialize response includes capabilities JSON
- [ ] All capability types included (logging, prompts, resources, tools, etc.)
- [ ] Client validates capabilities before operation
- [ ] Server returns error for unsupported capabilities
- [ ] Tests pass (min 25 tests)

### Estimated Time: 8-10 hours

---

## Gap #2: HTTP Session Management

### Problem Statement
Clients cannot identify or resume sessions. No session ID tracking.

### Why This Matters
- Clients need to identify their session
- Recovery from network failures requires session continuity
- Server-side session management enables proper cleanup
- Required for SSE stream resumption

### Implementation Steps

**Step 1: Create erlmcp_session_manager.erl**
```erlang
-module(erlmcp_session_manager).
-behaviour(gen_server).

-export([start_link/0, create_session/1, validate_session/1,
         terminate_session/1, get_session_info/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SESSION_TIMEOUT, 300000).  % 5 minutes

-record(session, {
    id :: binary(),
    client_id :: binary(),
    created :: integer(),
    last_activity :: integer(),
    status :: active | closed
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create_session(ClientId) ->
    gen_server:call(?MODULE, {create, ClientId}).

validate_session(SessionId) ->
    gen_server:call(?MODULE, {validate, SessionId}).

get_session_info(SessionId) ->
    gen_server:call(?MODULE, {info, SessionId}).

terminate_session(SessionId) ->
    gen_server:call(?MODULE, {terminate, SessionId}).

init([]) ->
    %% Use ETS for session storage
    ets:new(erlmcp_sessions, [
        set, named_table, public,
        {heir, self(), []}
    ]),
    {ok, #{}, init_cleanup_timer()}.

init_cleanup_timer() ->
    {ok, _} = timer:send_interval(60000, cleanup_expired),
    ok.

handle_call({create, ClientId}, _From, State) ->
    SessionId = generate_session_id(),
    Now = erlang:monotonic_time(),
    Session = #session{
        id = SessionId,
        client_id = ClientId,
        created = Now,
        last_activity = Now,
        status = active
    },
    ets:insert(erlmcp_sessions, {SessionId, Session}),
    {reply, {ok, SessionId}, State};

handle_call({validate, SessionId}, _From, State) ->
    case ets:lookup(erlmcp_sessions, SessionId) of
        [{_, Session}] ->
            case is_expired(Session) of
                false ->
                    %% Update last activity
                    UpdatedSession = Session#session{
                        last_activity = erlang:monotonic_time()
                    },
                    ets:insert(erlmcp_sessions, {SessionId, UpdatedSession}),
                    {reply, {ok, active}, State};
                true ->
                    ets:delete(erlmcp_sessions, SessionId),
                    {reply, {error, expired}, State}
            end;
        [] ->
            {reply, {error, not_found}, State}
    end;

handle_call({terminate, SessionId}, _From, State) ->
    ets:delete(erlmcp_sessions, SessionId),
    {reply, ok, State};

handle_info(cleanup_expired, State) ->
    %% Remove expired sessions
    ets:match_delete(erlmcp_sessions, {
        '_',
        #session{
            status = '$1',
            last_activity = '$2',
            _ = '_'
        }
    }),
    {noreply, State}.

generate_session_id() ->
    Base64 = base64:encode(crypto:strong_rand_bytes(32)),
    erlang:binary_to_list(Base64).

is_expired(Session) ->
    Now = erlang:monotonic_time(),
    (Now - Session#session.last_activity) > ?SESSION_TIMEOUT.
```

**Step 2: Update erlmcp_transport_sse.erl**
- Call session manager to create session on first request
- Include session ID in response headers
- Validate session ID on subsequent requests

**Step 3: Update erlmcp_transport_http_server.erl**
- Apply same session management pattern

### Code Sample (SSE Handler)

```erlang
handle_sse_stream(Req, TransportId, State) ->
    %% Create new session
    case erlmcp_session_manager:create_session(TransportId) of
        {ok, SessionId} ->
            Headers = #{
                <<"content-type">> => <<"text/event-stream">>,
                <<"cache-control">> => <<"no-cache">>,
                <<"connection">> => <<"keep-alive">>,
                <<"mcp-session-id">> => SessionId,
                <<"mcp-protocol-version">> => <<"2025-11-25">>
            },
            Req2 = cowboy_req:reply(200, Headers, Req),

            %% Continue with stream
            sse_event_loop(Req2, #{
                session_id => SessionId,
                transport_id => TransportId
            });
        {error, Reason} ->
            cowboy_req:reply(500, #{}, <<"Failed to create session">>, Req)
    end.
```

### Recommended Tests
1. Session creation returns unique IDs
2. Session validation accepts valid IDs
3. Session validation rejects expired IDs
4. Session cleanup removes old sessions
5. Concurrent session handling
6. Session persistence across requests
7. HTTP response includes MCP-Session-Id header

### Acceptance Criteria
- [ ] Session manager module created and working
- [ ] Sessions created with unique IDs
- [ ] Sessions included in HTTP responses
- [ ] Session validation working
- [ ] Session expiration handled
- [ ] Tests pass (min 30 tests)

### Estimated Time: 10-12 hours

---

## Gap #3: Origin Validation (DNS Rebinding Protection)

### Problem Statement
HTTP server doesn't validate Origin header, vulnerable to DNS rebinding attacks.

### Why This Matters
- DNS rebinding attack allows remote websites to access local MCP server
- Attackers can steal resources, execute tools, read prompts
- Simple to implement, critical security fix

### Implementation Steps

**Step 1: Update sys.config**
```erlang
{erlmcp, [
    {http_bind_address, "127.0.0.1"},  % ONLY localhost by default
    {allowed_origins, [
        <<"http://127.0.0.1:8080">>,
        <<"http://localhost:8080">>,
        <<"https://my-app.example.com">>  % Add trusted origins only
    ]},
    {require_https, true}
]}.
```

**Step 2: Create origin validation function**

```erlang
%% In erlmcp_transport_http_server.erl or new module erlmcp_origin_validator.erl
-spec validate_origin(Req :: term()) -> ok | {error, forbidden}.
validate_origin(Req) ->
    Origin = cowboy_req:header(<<"origin">>, Req, undefined),
    case Origin of
        undefined ->
            %% Origin header not present - OK (same-origin request)
            ok;
        _ ->
            %% Origin header present - validate against whitelist
            AllowedOrigins = application:get_env(erlmcp, allowed_origins, []),
            case lists:member(Origin, AllowedOrigins) of
                true ->
                    ok;
                false ->
                    logger:warning("Rejected request from invalid origin: ~p", [Origin]),
                    {error, forbidden}
            end
    end.
```

**Step 3: Add validation to HTTP handlers**

```erlang
handle_request(Req, State) ->
    case validate_origin(Req) of
        ok ->
            handle_authenticated_request(Req, State);
        {error, forbidden} ->
            logger:warning("HTTP 403: Invalid origin"),
            cowboy_req:reply(403, #{<<"content-type">> => <<"application/json">>},
                           jsx:encode(#{
                               <<"error">> => <<"Forbidden">>,
                               <<"message">> => <<"Origin not allowed">>
                           }), Req)
    end.
```

**Step 4: Update documentation**
- Document origin validation requirements
- Add security best practices section
- Explain why 0.0.0.0 is dangerous
- Recommend HTTPS in production

### Recommended Tests
1. Valid origin accepted
2. Invalid origin rejected with 403
3. Missing origin header accepted
4. Multiple allowed origins work
5. Case sensitivity of origin check
6. Localhost binding enforced
7. Configuration validation

### Acceptance Criteria
- [ ] Origin validation implemented
- [ ] HTTP 403 returned for invalid origins
- [ ] Server binds to 127.0.0.1 by default
- [ ] Configuration allows whitelist
- [ ] Documentation updated with security section
- [ ] Tests pass (min 15 tests)

### Estimated Time: 4-6 hours

---

## Gap #4: Initialization Phase State Machine

### Problem Statement
No enforcement of initialization phases. Clients can send requests before initialized.

### Why This Matters
- MCP spec requires three phases: pre-init, init, operation
- Without phase enforcement, unpredictable behavior
- Must timeout if initialization doesn't complete
- Prevents protocol violations

### Implementation Steps

**Step 1: Define phase type in header**

```erlang
%% In include/erlmcp.hrl
-type server_phase() :: initialization | operation | shutdown.
```

**Step 2: Update server state**

```erlang
%% In erlmcp_server.erl
-record(state, {
    server_id :: server_id(),
    phase = initialization :: server_phase(),  % ADD THIS
    capabilities :: #mcp_server_capabilities{},
    client_capabilities :: #mcp_client_capabilities{} | undefined,
    protocol_version :: binary() | undefined,
    init_timer :: reference() | undefined,  % ADD THIS
    %% ... rest of fields
    initialized = false :: boolean()
}).
```

**Step 3: Implement phase enforcement**

```erlang
handle_call(Request, From, State) ->
    case State#state.phase of
        initialization ->
            handle_initialization_request(Request, From, State);
        operation ->
            handle_operation_request(Request, From, State);
        shutdown ->
            {reply, {error, -32000, <<"Server shutting down">>}, State}
    end.

handle_initialization_request({initialize, Capabilities, _Options}, From, State) ->
    %% Validate protocol version
    RequestedVersion = maps:get(<<"protocolVersion">>, Capabilities),
    case validate_protocol_version(RequestedVersion) of
        ok ->
            Response = #{
                <<"protocolVersion">> => <<"2025-11-25">>,
                <<"capabilities">> => server_capabilities(),
                <<"serverInfo">> => #{
                    <<"name">> => <<"erlmcp">>,
                    <<"version">> => <<"0.6.0">>
                }
            },
            %% Cancel init timer
            case State#state.init_timer of
                undefined -> ok;
                Ref -> timer:cancel(Ref)
            end,
            %% Set init timeout for next phase
            {ok, NewRef} = timer:send_after(5000, init_timeout),
            NewState = State#state{
                init_timer = NewRef
            },
            {reply, Response, NewState};
        {error, Reason} ->
            {reply, {error, -32602, <<"Invalid protocol version">>, Reason}, State}
    end;

handle_initialization_request({initialized, _}, _From, State) ->
    %% Move to operation phase
    %% Cancel init timeout
    case State#state.init_timer of
        undefined -> ok;
        Ref -> timer:cancel(Ref)
    end,
    NewState = State#state{
        phase = operation,
        init_timer = undefined
    },
    {reply, ok, NewState};

handle_initialization_request({ping, _}, From, State) ->
    %% Ping allowed during init
    {reply, pong, State};

handle_initialization_request(_Request, From, State) ->
    %% Any other request blocked
    {reply, {error, -32005, <<"Not initialized">>}, State}.

handle_operation_request(Request, From, State) ->
    %% Handle normal requests
    handle_operation_call(Request, From, State).

%% Handle init timeout
handle_info(init_timeout, State) ->
    logger:error("Initialization timeout"),
    {stop, init_timeout, State}.

validate_protocol_version(Version) ->
    SupportedVersions = [<<"2025-11-25">>, <<"2024-11-05">>],
    case lists:member(Version, SupportedVersions) of
        true -> ok;
        false ->
            {error, #{
                <<"supported">> => SupportedVersions,
                <<"requested">> => Version
            }}
    end.
```

**Step 4: Update client similarly**

Apply same phase machine to client in erlmcp_client.erl.

### Recommended Tests
1. Server starts in initialization phase
2. Only initialize request accepted during init
3. Ping allowed during init
4. Other requests rejected with -32005
5. Move to operation after initialized notification
6. Timeout if initialization doesn't complete
7. Protocol version validation
8. Version error includes supported versions
9. Cannot return to initialization phase

### Acceptance Criteria
- [ ] Phase machine implemented in server
- [ ] Phase machine implemented in client
- [ ] Initialization timeout working
- [ ] Only valid requests allowed in each phase
- [ ] Protocol version negotiation working
- [ ] Tests pass (min 35 tests)

### Estimated Time: 12-15 hours

---

## Gap #5: Error Response Structure

### Problem Statement
Error responses missing data field and structured context.

### Why This Matters
- Clients need details to debug issues
- Data field helps identify specific problem
- Spec requires data field for rich errors

### Implementation Steps

**Step 1: Update erlmcp_json_rpc.erl**

```erlang
-spec encode_error_response(json_rpc_id(), integer(), binary()) -> binary().
encode_error_response(Id, Code, Message) ->
    encode_error_response(Id, Code, Message, undefined).

-spec encode_error_response(json_rpc_id(), integer(), binary(), map() | undefined) -> binary().
encode_error_response(Id, Code, Message, Data) when is_integer(Code), is_binary(Message) ->
    case validate_error_code(Code) of
        true ->
            Error = build_error_object(Code, Message, Data),
            Response = #json_rpc_response{
                id = Id,
                error = Error
            },
            encode_message(Response);
        false ->
            logger:warning("Invalid error code: ~p", [Code]),
            Error = build_error_object(-32603, <<"Internal error">>, undefined),
            Response = #json_rpc_response{
                id = Id,
                error = Error
            },
            encode_message(Response)
    end.

-spec build_error_object(integer(), binary(), map() | undefined) -> map().
build_error_object(Code, Message, undefined) ->
    #{
        ?JSONRPC_ERROR_FIELD_CODE => Code,
        ?JSONRPC_ERROR_FIELD_MESSAGE => Message
    };
build_error_object(Code, Message, Data) when is_map(Data) ->
    #{
        ?JSONRPC_ERROR_FIELD_CODE => Code,
        ?JSONRPC_ERROR_FIELD_MESSAGE => Message,
        ?JSONRPC_ERROR_FIELD_DATA => Data
    };
build_error_object(Code, Message, _) ->
    %% Invalid data type, skip it
    #{
        ?JSONRPC_ERROR_FIELD_CODE => Code,
        ?JSONRPC_ERROR_FIELD_MESSAGE => Message
    }.

-spec validate_error_code(integer()) -> boolean().
validate_error_code(Code) ->
    ValidCodes = [
        -32700,  % Parse error
        -32600,  % Invalid Request
        -32601,  % Method not found
        -32602,  % Invalid params
        -32603,  % Internal error
        -32001,  % Resource not found
        -32002,  % Tool not found
        -32003,  % Prompt not found
        -32004,  % Capability not supported
        -32005,  % Not initialized
        -32006   % Subscription failed
    ],
    lists:member(Code, ValidCodes).
```

**Step 2: Add helper functions**

```erlang
%% Helper functions for common errors
-spec method_not_found(json_rpc_id(), binary()) -> binary().
method_not_found(Id, Method) ->
    Data = #{<<"method">> => Method},
    encode_error_response(Id, -32601, <<"Method not found">>, Data).

-spec invalid_params(json_rpc_id(), string() | binary()) -> binary().
invalid_params(Id, Details) when is_list(Details) ->
    invalid_params(Id, erlang:list_to_binary(Details));
invalid_params(Id, Details) when is_binary(Details) ->
    Data = #{<<"details">> => Details},
    encode_error_response(Id, -32602, <<"Invalid params">>, Data).

-spec resource_not_found(json_rpc_id(), binary()) -> binary().
resource_not_found(Id, Uri) ->
    Data = #{<<"uri">> => Uri},
    encode_error_response(Id, -32001, <<"Resource not found">>, Data).

-spec tool_not_found(json_rpc_id(), binary()) -> binary().
tool_not_found(Id, Name) ->
    Data = #{<<"name">> => Name},
    encode_error_response(Id, -32002, <<"Tool not found">>, Data).

-spec capability_not_supported(json_rpc_id(), binary()) -> binary().
capability_not_supported(Id, Capability) ->
    Data = #{<<"capability">> => Capability},
    encode_error_response(Id, -32004, <<"Capability not supported">>, Data).
```

**Step 3: Update handlers to use helpers**

Replace all error responses with helper functions.

### Recommended Tests
1. Error code validation
2. Data field included when provided
3. Data field omitted when undefined
4. Error code enum enforcement
5. Each error type tested
6. JSON structure validation
7. Invalid data types handled gracefully

### Acceptance Criteria
- [ ] Error responses include data field
- [ ] Helper functions created for common errors
- [ ] Error codes validated
- [ ] JSON structure correct
- [ ] All handlers use error helpers
- [ ] Tests pass (min 20 tests)

### Estimated Time: 4-6 hours

---

## Gaps #6-10: Implementation Guide Summary

Due to length constraints, gaps #6-10 follow similar patterns:

### Gap #6: List Change Notifications
- **File**: `src/erlmcp_server.erl`
- **Steps**: Implement notification system, wire to add/remove handlers
- **Key Functions**: `notify_list_changed/2`, `broadcast_notification/2`
- **Tests**: 30 tests for notification emission, subscription, etc.

### Gap #7: Resource Subscriptions
- **File**: `src/erlmcp_server.erl`
- **Steps**: Add subscribe/unsubscribe handlers, notification sending
- **Key Functions**: `subscribe_resource/2`, `unsubscribe_resource/2`
- **Tests**: 30 tests for subscription lifecycle

### Gap #8: HTTP Header Validation
- **Files**: `src/erlmcp_transport_sse.erl`, `src/erlmcp_transport_http_server.erl`
- **Steps**: Validate Accept, Content-Type, MCP-Protocol-Version
- **Key Functions**: `validate_headers/1`, `add_response_headers/1`
- **Tests**: 20 tests for header validation

### Gap #9: WebSocket Improvements
- **File**: `src/erlmcp_transport_ws.erl`
- **Steps**: Add newline delimiter, UTF-8 validation, size limits, close handling
- **Key Functions**: `process_messages/2`, `validate_utf8/1`
- **Tests**: 25 tests for message parsing and frame handling

### Gap #10: Tool Progress Tokens
- **File**: `src/erlmcp_server.erl`
- **Steps**: Generate tokens, send progress notifications, cleanup
- **Key Functions**: `generate_progress_token/0`, `send_progress/3`
- **Tests**: 25 tests for progress tracking

---

## Post-Implementation Checklist

After implementing each gap:

- [ ] Code written and compiles cleanly
- [ ] Tests written and passing (100% pass rate)
- [ ] Code reviewed for quality
- [ ] Dialyzer warnings resolved
- [ ] Ruff linting passes
- [ ] Documentation updated
- [ ] Git commit created
- [ ] PR created if team-based
- [ ] Integration tested with other gaps

---

## Common Patterns Used

### Pattern 1: Feature Manager (Gaps #1, #2, #6, #7)
Create a gen_server module to manage feature state.

### Pattern 2: Validator Middleware (Gaps #3, #8)
Add validation function called early in request handling.

### Pattern 3: State Machine (Gap #4)
Use phase/state field to enforce operation order.

### Pattern 4: Notification Broadcaster (Gaps #6, #7, #10)
Send notifications to registered subscribers.

### Pattern 5: Token Generator (Gap #10)
Generate unique identifiers and track lifecycle.

---

## Testing Strategy

### Unit Tests
- Test individual functions in isolation
- Mock dependencies
- Test error cases and edge cases

### Integration Tests
- Test interactions between modules
- Test full request/response cycles
- Test notification delivery

### Property Tests
- Generate random valid/invalid inputs
- Verify invariants hold
- Check error handling

### Security Tests
- Test origin validation blocks invalid origins
- Test session validation rejects expired sessions
- Test error messages don't leak sensitive info

---

## Code Quality Standards

All implementations must meet:
- ✅ 100% type hints
- ✅ Comprehensive docstrings
- ✅ No compiler warnings
- ✅ No ruff lint issues
- ✅ Test coverage >= 80%
- ✅ Proper error handling
- ✅ Logging at appropriate levels
- ✅ OTP behavior compliance

---

## Questions & Troubleshooting

### Q: How do I test the phase machine?
A: Create a test client, send requests in wrong order, verify rejection with -32005.

### Q: How do I handle concurrent sessions?
A: Use ETS with named_table and public access for thread-safe operations.

### Q: What if origin header is missing?
A: Allow it (same-origin requests don't include Origin header).

### Q: How do I validate error codes?
A: Use a whitelist (see Gap #5 validate_error_code/1).

### Q: What's the max message size for WebSocket?
A: 64KB by default (see Gap #9 MAX_MESSAGE_SIZE).

---

## Timeline Summary

- **Phase 1 (Critical)**: 40-50 hours, 5-7 days
- **Phase 2 (High)**: 40-50 hours, 5-7 days
- **Phase 3 (Medium)**: 30-40 hours, 4-5 days

**Total**: 110-140 hours (3-4 weeks with full-time developer)

---

**Implementation Guide Version**: 1.0
**Last Updated**: 2026-01-27
**Status**: Ready for development
