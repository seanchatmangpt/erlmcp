# Transport Layer Compliance Fixes

**Purpose**: Provide concrete code changes to achieve MCP 2025-11-25 compliance

---

## Issue 1: Connection Phase Enforcement (ALL TRANSPORTS - CRITICAL)

### Current State
All transports accept messages immediately without enforcing the initialize→initialized lifecycle.

### Root Cause
- No phase tracking in transport state
- `get_info/1` callback returns status but doesn't enforce transitions
- No state machine to validate message types per phase

### Impact
- Protocol violation: uninitialized requests accepted
- Security: capabilities not negotiated before access
- Interoperability: clients expect strict phase enforcement

### Fix Implementation

#### Step 1: Update Transport Behavior Interface

**File**: `/home/user/erlmcp/apps/erlmcp_transports/src/erlmcp_transport_behavior.erl`

Add new callback for phase validation:

```erlang
%% BEFORE: Only 4 required callbacks
-callback init(Config :: map()) -> {ok, State} | {error, Reason}.
-callback send(State :: term(), Data :: binary()) -> ok | {error, Reason}.
-callback close(State :: term()) -> ok.
-callback get_info(State :: term()) -> #{atom() => term()}.

%% AFTER: Add phase enforcement callback
-callback validate_initialization(State :: term(), Message :: map()) ->
    ok | {error, not_initialized}.

-optional_callbacks([
    get_info/1,
    handle_transport_call/2,
    validate_initialization/2
]).
```

Add type definition for phases:

```erlang
-type connection_phase() :: initialization | operating | shutdown.

-export_type([
    ...
    connection_phase/0
]).
```

#### Step 2: Update Transport Behavior Helper

Add default implementation:

```erlang
%% @doc Default phase validation - enforce strict ordering
-spec validate_initialization(term(), map()) -> ok | {error, not_initialized}.
validate_initialization(State, Message) when is_map(Message) ->
    Phase = extract_phase(State),
    Method = maps:get(<<"method">>, Message, undefined),

    case Phase of
        initialization ->
            case is_initialization_method(Method) of
                true -> ok;
                false -> {error, not_initialized}
            end;
        operating ->
            ok;  % All methods allowed in operating phase
        _ ->
            {error, invalid_phase}
    end.

-spec is_initialization_method(binary()) -> boolean().
is_initialization_method(Method) ->
    lists:member(Method, [
        <<"initialize">>,
        <<"ping">>,
        <<"logging/setLevel">>
    ]).

-spec extract_phase(term()) -> connection_phase().
extract_phase(State) when is_tuple(State), tuple_size(State) > 1 ->
    % Try record with phase field
    try element(find_phase_index(State), State) of
        Phase when is_atom(Phase) -> Phase
    catch
        _ -> operating
    end;
extract_phase(#{connection_phase := Phase}) when is_atom(Phase) ->
    Phase;
extract_phase(_) ->
    operating.  % Default to operating for backward compatibility
```

#### Step 3: Update Each Transport State Record

**For Stdio** (`erlmcp_transport_stdio.erl`):

```erlang
-record(state, {
    owner :: pid(),
    owner_monitor :: reference() | undefined,
    reader :: pid() | undefined,
    buffer = <<>> :: binary(),
    test_mode = false :: boolean(),
    max_message_size :: pos_integer(),
    transport_id :: atom() | binary() | undefined,
    %% NEW: Phase tracking
    connection_phase = initialization :: initialization | operating | shutdown,
    protocol_version :: binary() | undefined,
    capabilities :: map() | undefined
}).
```

**For TCP** (`erlmcp_transport_tcp.erl`):

```erlang
-record(state, {
    mode :: server | client,
    transport_id :: atom() | undefined,
    server_id :: atom() | undefined,
    socket :: gen_tcp:socket() | undefined,
    ranch_ref :: ranch:ref() | undefined,
    owner :: pid() | undefined,
    connected :: boolean(),
    buffer :: binary(),
    options :: list(),
    idle_timer :: reference() | undefined,
    resource_monitor_timer :: reference() | undefined,
    last_activity :: non_neg_integer(),
    max_message_size :: pos_integer(),
    initialized :: boolean(),
    %% NEW: Phase tracking
    connection_phase = initialization :: initialization | operating | shutdown,
    protocol_version :: binary() | undefined,
    capabilities :: map() | undefined,
    bytes_sent = 0 :: non_neg_integer(),
    bytes_received = 0 :: non_neg_integer()
}).
```

#### Step 4: Implement Phase Transitions

**For TCP Transport** (`erlmcp_transport_tcp.erl`):

Add handler for initialize request:

```erlang
handle_info({tcp, Socket, Data}, #state{socket = Socket, buffer = Buffer,
            connection_phase = initialization, max_message_size = MaxMessageSize} = State) ->
    %% In initialization phase, ONLY process initialize method
    DataSize = byte_size(Data),
    NewBufferSize = byte_size(Buffer) + DataSize,

    case NewBufferSize > MaxMessageSize of
        true ->
            ErrorMsg = erlmcp_json_rpc:error_message_too_large(null, MaxMessageSize),
            catch gen_tcp:send(Socket, [ErrorMsg, <<"\n">>]),
            gen_tcp:close(Socket),
            {stop, {message_too_large, NewBufferSize}, State};
        false ->
            NewBuffer = <<Buffer/binary, Data/binary>>,
            {Messages, RemainingBuffer} = extract_messages(NewBuffer),

            %% Process only initialize messages in initialization phase
            FinalState = process_initialization_messages(Messages, State#state{buffer = RemainingBuffer}),
            {noreply, FinalState}
    end;

%% Transition to operating phase after initialized notification
handle_info({transition_to_operating, _}, State) ->
    {noreply, State#state{connection_phase = operating}};

handle_call(get_info, _From, State) ->
    Info = #{
        transport_id => State#state.transport_id,
        type => tcp,
        status => connection_status(State),
        phase => State#state.connection_phase,
        protocol_version => State#state.protocol_version,
        capabilities => State#state.capabilities
    },
    {reply, {ok, Info}, State}.

-spec connection_status(#state{}) -> atom().
connection_status(#state{connected = true, connection_phase = operating}) ->
    ready;
connection_status(#state{connected = true, connection_phase = initialization}) ->
    initializing;
connection_status(#state{connected = false}) ->
    disconnected;
connection_status(_) ->
    unknown.

-spec process_initialization_messages([map()], #state{}) -> #state{}.
process_initialization_messages([], State) ->
    State;
process_initialization_messages([Msg | Rest], State) ->
    Method = maps:get(<<"method">>, Msg, undefined),
    case Method of
        <<"initialize">> ->
            %% Extract capabilities from client and store
            Params = maps:get(<<"params">>, Msg, #{}),
            ProtoVersion = maps:get(<<"protocolVersion">>, Params, <<"2025-11-25">>),
            ClientCaps = maps:get(<<"capabilities">>, Params, #{}),

            %% Store for later negotiation
            UpdatedState = State#state{
                protocol_version = ProtoVersion,
                capabilities = ClientCaps
            },
            process_initialization_messages(Rest, UpdatedState);
        <<"ping">> ->
            %% Ping allowed during initialization
            process_initialization_messages(Rest, State);
        _ ->
            %% Reject non-initialization methods
            logger:warning("Rejecting ~p during initialization phase", [Method]),
            process_initialization_messages(Rest, State)
    end.
```

---

## Issue 2: HTTP Origin Validation (HTTP TRANSPORT - CRITICAL SECURITY)

### Current State
HTTP transport doesn't validate Origin header, allowing DNS rebinding attacks.

### Root Cause
- No Origin header parsing in HTTP handlers
- No whitelist validation logic
- Server binds to all interfaces (0.0.0.0) by default

### Impact
- CRITICAL SECURITY: Remote websites can attack local MCP servers via DNS rebinding
- Local services compromised through browser-based clients
- CSRF attacks possible

### Fix Implementation

**File**: `/home/user/erlmcp/apps/erlmcp_transports/src/erlmcp_transport_http_server.erl`

#### Step 1: Add Origin Validation Module

Create new file: `/home/user/erlmcp/apps/erlmcp_transports/src/erlmcp_http_origin_validator.erl`

```erlang
-module(erlmcp_http_origin_validator).

-export([
    validate/2,
    get_allowed_origins/0,
    parse_origin/1
]).

-define(DEFAULT_ALLOWED_ORIGINS, [
    <<"http://localhost">>,
    <<"http://127.0.0.1">>,
    <<"http://localhost:*">>,
    <<"http://127.0.0.1:*">>
]).

%% @doc Validate Origin header against whitelist
-spec validate(binary() | undefined, list()) -> ok | {error, forbidden}.
validate(undefined, _Whitelist) ->
    %% No Origin header - this is OK for some requests (e.g., direct HTTP)
    ok;
validate(Origin, Whitelist) when is_binary(Origin) ->
    case check_origin(Origin, Whitelist) of
        true -> ok;
        false -> {error, forbidden}
    end;
validate(_, _) ->
    {error, invalid_origin}.

-spec check_origin(binary(), list()) -> boolean().
check_origin(Origin, Whitelist) ->
    lists:any(fun(AllowedPattern) ->
        match_origin_pattern(Origin, AllowedPattern)
    end, Whitelist).

-spec match_origin_pattern(binary(), binary()) -> boolean().
match_origin_pattern(Origin, Pattern) ->
    case string:split(Pattern, <<"*">>) of
        [Prefix] ->
            Origin =:= Prefix;
        [Prefix, Suffix] ->
            case binary:match(Origin, Prefix) of
                {0, _} ->
                    case binary:match(Origin, Suffix) of
                        {_, _} -> true;
                        nomatch ->
                            case Suffix of
                                <<>> -> true;  % Wildcard at end
                                _ -> false
                            end
                    end;
                nomatch -> false
            end;
        _ -> false
    end.

-spec parse_origin(binary()) -> {Scheme :: binary(), Host :: binary(), Port :: binary()}.
parse_origin(Origin) ->
    case binary:split(Origin, <<"://">>) of
        [Scheme, Rest] ->
            [HostPort | _] = binary:split(Rest, <<"/">>, []),
            case binary:split(HostPort, <<":">>) of
                [Host, Port] -> {Scheme, Host, Port};
                [Host] -> {Scheme, Host, default_port(Scheme)}
            end;
        _ ->
            {undefined, undefined, undefined}
    end.

-spec default_port(binary()) -> binary().
default_port(<<"https">>) -> <<"443">>;
default_port(<<"http">>) -> <<"80">>;
default_port(_) -> undefined.

-spec get_allowed_origins() -> list().
get_allowed_origins() ->
    case application:get_env(erlmcp_transports, allowed_origins) of
        {ok, Origins} -> Origins;
        undefined -> ?DEFAULT_ALLOWED_ORIGINS
    end.
```

#### Step 2: Integrate Into HTTP Handler

**File**: `/home/user/erlmcp/apps/erlmcp_transports/src/erlmcp_transport_http_server.erl`

Add to `init/1`:

```erlang
-spec init(http_opts()) -> {ok, state()} | {error, term()}.
init(Opts) when is_map(Opts) ->
    process_flag(trap_exit, true),

    %% Ensure required applications are started
    ensure_apps_started(),

    %% Monitor owner
    Owner = maps:get(owner, Opts),
    monitor(process, Owner),

    %% Build state
    State = build_initial_state(Opts),

    %% NEW: Validate origin configuration
    AllowedOrigins = erlmcp_http_origin_validator:get_allowed_origins(),

    %% Connect to server
    case connect(State) of
        {ok, NewState} ->
            {ok, NewState#state{allowed_origins = AllowedOrigins}};
        {error, Reason} ->
            {error, Reason}
    end.
```

Add to state record:

```erlang
-record(state, {
    url :: string(),
    owner :: pid(),
    method :: get | post,
    headers :: [{binary(), binary()}],
    timeout :: timeout(),
    connect_timeout :: timeout(),
    max_retries :: non_neg_integer(),
    retry_delay :: pos_integer(),
    gun_pid :: pid() | undefined,
    gun_monitor :: reference() | undefined,
    gun_stream_ref :: reference() | undefined,
    pending_requests = #{} :: #{reference() => {pid(), reference(), binary(), non_neg_integer()}},
    message_queue :: queue:queue({binary(), {pid(), reference()}}),
    pool_size :: non_neg_integer(),
    active_requests :: non_neg_integer(),
    ssl_options :: [ssl:tls_client_option()],
    host :: string(),
    port :: pos_integer(),
    path :: string(),
    scheme :: http | https,
    %% NEW: Origin validation
    allowed_origins :: list()
}).
```

Add handler for origin validation:

```erlang
%% Validate Origin on incoming requests
validate_request(Req, State) ->
    Origin = cowboy_req:header(<<"origin">>, Req, undefined),
    AllowedOrigins = State#state.allowed_origins,

    case erlmcp_http_origin_validator:validate(Origin, AllowedOrigins) of
        ok ->
            {ok, Req};
        {error, forbidden} ->
            logger:warning("Rejecting request with invalid Origin: ~p", [Origin]),
            {error, cowboy_req:reply(403, #{
                <<"content-type">> => <<"application/json">>
            },
            jsx:encode(#{
                <<"error">> => <<"Forbidden: Invalid Origin">>
            }), Req)}
    end.
```

#### Step 3: Configuration

**File**: `/home/user/erlmcp/apps/erlmcp_transports/config/sys.config`

Add allowed origins configuration:

```erlang
{erlmcp_transports, [
    {allowed_origins, [
        <<"http://localhost">>,
        <<"http://127.0.0.1">>,
        <<"https://localhost">>,
        <<"https://127.0.0.1">>
    ]},
    {http_bind_address, {127, 0, 0, 1}},  % Bind to localhost only
    {http_port, 8080}
]}.
```

---

## Issue 3: HTTP Session Management (HTTP TRANSPORT - CRITICAL)

### Current State
HTTP transport has no session tracking with MCP-Session-Id headers.

### Root Cause
- No session ID generation
- No session state storage
- No session expiration handling

### Impact
- Cannot implement resumable HTTP transport
- Client can't maintain state across requests
- Protocol violation

### Fix Implementation

**File**: `/home/user/erlmcp/apps/erlmcp_transports/src/erlmcp_http_session_manager.erl`

```erlang
-module(erlmcp_http_session_manager).

-behavior(gen_server).

%% API
-export([
    start_link/0,
    create_session/1,
    get_session/1,
    update_session/2,
    expire_session/1,
    cleanup_expired/0
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(SESSION_TIMEOUT, 3600000).  % 1 hour
-define(CLEANUP_INTERVAL, 600000).  % 10 minutes

-record(session, {
    session_id :: binary(),
    transport_id :: binary(),
    created_at :: non_neg_integer(),
    last_activity :: non_neg_integer(),
    metadata :: map()
}).

-record(state, {
    sessions = #{} :: #{binary() => #session{}},
    cleanup_timer :: reference() | undefined
}).

%% @doc Start session manager
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Create new session
-spec create_session(binary()) -> {ok, binary()} | {error, term()}.
create_session(TransportId) ->
    gen_server:call(?MODULE, {create_session, TransportId}).

%% @doc Get session info
-spec get_session(binary()) -> {ok, #session{}} | {error, not_found | expired}.
get_session(SessionId) ->
    gen_server:call(?MODULE, {get_session, SessionId}).

%% @doc Update session activity
-spec update_session(binary(), map()) -> ok | {error, term()}.
update_session(SessionId, Metadata) ->
    gen_server:cast(?MODULE, {update_session, SessionId, Metadata}).

%% @doc Expire session
-spec expire_session(binary()) -> ok.
expire_session(SessionId) ->
    gen_server:cast(?MODULE, {expire_session, SessionId}).

%% @doc Cleanup expired sessions
-spec cleanup_expired() -> ok.
cleanup_expired() ->
    gen_server:cast(?MODULE, cleanup_expired).

%% gen_server callbacks

-spec init([]) -> {ok, #state{}}.
init([]) ->
    process_flag(trap_exit, true),
    Timer = erlang:send_after(?CLEANUP_INTERVAL, self(), cleanup_expired),
    {ok, #state{cleanup_timer = Timer}}.

-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {reply, term(), #state{}} | {noreply, #state{}}.
handle_call({create_session, TransportId}, _From, State) ->
    SessionId = generate_session_id(),
    Now = erlang:system_time(millisecond),
    Session = #session{
        session_id = SessionId,
        transport_id = TransportId,
        created_at = Now,
        last_activity = Now,
        metadata = #{}
    },
    NewSessions = maps:put(SessionId, Session, State#state.sessions),
    {reply, {ok, SessionId}, State#state{sessions = NewSessions}};

handle_call({get_session, SessionId}, _From, State) ->
    case maps:get(SessionId, State#state.sessions, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        Session ->
            Now = erlang:system_time(millisecond),
            case Now - Session#session.last_activity > ?SESSION_TIMEOUT of
                true ->
                    {reply, {error, expired}, State};
                false ->
                    {reply, {ok, Session}, State}
            end
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast({update_session, SessionId, Metadata}, State) ->
    case maps:get(SessionId, State#state.sessions, undefined) of
        undefined ->
            {noreply, State};
        Session ->
            Now = erlang:system_time(millisecond),
            UpdatedSession = Session#session{
                last_activity = Now,
                metadata = Metadata
            },
            NewSessions = maps:put(SessionId, UpdatedSession, State#state.sessions),
            {noreply, State#state{sessions = NewSessions}}
    end;

handle_cast({expire_session, SessionId}, State) ->
    NewSessions = maps:remove(SessionId, State#state.sessions),
    {noreply, State#state{sessions = NewSessions}};

handle_cast(cleanup_expired, State) ->
    Now = erlang:system_time(millisecond),
    Expired = maps:filter(fun(_Id, Session) ->
        Now - Session#session.last_activity > ?SESSION_TIMEOUT
    end, State#state.sessions),
    NewSessions = maps:without(maps:keys(Expired), State#state.sessions),
    {noreply, State#state{sessions = NewSessions}};

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info(cleanup_expired, State) ->
    Timer = erlang:send_after(?CLEANUP_INTERVAL, self(), cleanup_expired),
    {noreply, State#state{cleanup_timer = Timer}};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, State) ->
    case State#state.cleanup_timer of
        undefined -> ok;
        Timer -> erlang:cancel_timer(Timer), ok
    end.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Private functions

-spec generate_session_id() -> binary().
generate_session_id() ->
    crypto:strong_rand_bytes(32).
```

Use in HTTP handlers:

```erlang
handle_http_post(Req, State) ->
    %% Extract or create session
    SessionId = cowboy_req:header(<<"mcp-session-id">>, Req, undefined),
    FinalSessionId = case SessionId of
        undefined ->
            case erlmcp_http_session_manager:create_session(State#state.transport_id) of
                {ok, NewSessionId} -> NewSessionId;
                {error, _} -> undefined
            end;
        Existing -> Existing
    end,

    %% Validate session exists and is active
    case erlmcp_http_session_manager:get_session(FinalSessionId) of
        {ok, _Session} ->
            %% Update last activity
            erlmcp_http_session_manager:update_session(FinalSessionId, #{}),

            %% Return response with session ID in header
            Resp = handle_json_rpc_request(Req, State),
            Headers = maps:put(<<"mcp-session-id">>, FinalSessionId, Resp),
            {ok, cowboy_req:reply(200, Headers, Resp)};
        {error, expired} ->
            {ok, cowboy_req:reply(401, #{}, jsx:encode(#{
                <<"error">> => <<"Session expired">>
            }))};
        {error, not_found} ->
            {ok, cowboy_req:reply(400, #{}, jsx:encode(#{
                <<"error">> => <<"Invalid or missing session">>
            }))}
    end.
```

---

## Issue 4: Capability Negotiation (ALL TRANSPORTS - CRITICAL)

### Current State
Transports don't exchange capabilities with clients.

### Root Cause
- No capability data structure in state
- `transport_connected` message doesn't include capabilities
- No capability negotiation protocol

### Impact
- Clients can't determine supported features
- Protocol violation
- Feature detection impossible

### Fix Implementation

**File**: `/home/user/erlmcp/apps/erlmcp_transports/src/erlmcp_transport_capabilities.erl`

```erlang
-module(erlmcp_transport_capabilities).

-export([
    get_server_capabilities/0,
    negotiate_capabilities/2,
    validate_operation_capability/2
]).

%% @doc Get default server capabilities
-spec get_server_capabilities() -> map().
get_server_capabilities() ->
    #{
        logging => #{},
        resources => #{
            subscribe => true,
            listChanged => true
        },
        tools => #{
            listChanged => true
        },
        prompts => #{
            listChanged => true
        },
        completions => #{},
        tasks => #{
            list => true,
            cancel => true,
            requests => #{
                tools => #{call => true}
            }
        },
        experimental => #{}
    }.

%% @doc Negotiate capabilities between client and server
-spec negotiate_capabilities(map(), map()) -> map().
negotiate_capabilities(ClientCaps, ServerCaps) ->
    %% Return intersection of capabilities
    maps:fold(fun(Key, ServerVal, Acc) ->
        case maps:get(Key, ClientCaps, undefined) of
            undefined ->
                %% Client doesn't support, exclude from negotiated set
                Acc;
            _ ->
                %% Both support, include in negotiated set
                maps:put(Key, ServerVal, Acc)
        end
    end, #{}, ServerCaps).

%% @doc Validate that operation uses negotiated capability
-spec validate_operation_capability(binary(), map()) -> ok | {error, capability_not_supported}.
validate_operation_capability(Method, NegotiatedCaps) ->
    CapabilityName = method_to_capability(Method),
    case maps:get(CapabilityName, NegotiatedCaps, undefined) of
        undefined -> {error, capability_not_supported};
        _ -> ok
    end.

-spec method_to_capability(binary()) -> atom().
method_to_capability(<<"resources/", _/binary>>) -> resources;
method_to_capability(<<"tools/", _/binary>>) -> tools;
method_to_capability(<<"prompts/", _/binary>>) -> prompts;
method_to_capability(<<"logging/", _/binary>>) -> logging;
method_to_capability(<<"tasks/", _/binary>>) -> tasks;
method_to_capability(<<"completions/", _/binary>>) -> completions;
method_to_capability(_) -> undefined.
```

Use in TCP transport handler:

```erlang
handle_info({tcp, Socket, Data}, #state{socket = Socket, buffer = Buffer,
            connection_phase = initialization, max_message_size = MaxMessageSize} = State) ->
    NewBuffer = <<Buffer/binary, Data/binary>>,
    {Messages, RemainingBuffer} = extract_messages(NewBuffer),

    %% Process only initialize messages
    FinalState = process_initialization_messages(Messages, State#state{buffer = RemainingBuffer}),
    {noreply, FinalState};

handle_info(send_initialize_response, #state{owner = Owner} = State) ->
    %% Send initialize response with negotiated capabilities
    ServerCaps = erlmcp_transport_capabilities:get_server_capabilities(),
    ClientCaps = State#state.capabilities,
    NegotiatedCaps = erlmcp_transport_capabilities:negotiate_capabilities(ClientCaps, ServerCaps),

    %% Send to owner
    Owner ! {transport_initialized, self(), #{
        capabilities => NegotiatedCaps,
        protocol_version => <<"2025-11-25">>,
        server_info => #{
            name => <<"erlmcp">>,
            version => get_version()
        }
    }},

    {noreply, State#state{
        capabilities = NegotiatedCaps,
        connection_phase = operating
    }}.
```

---

## Summary of Changes by Priority

| Priority | Component | Changes | Files |
|----------|-----------|---------|-------|
| P0-CRITICAL | All Transports | Add phase enforcement | behavior, stdio, tcp, http, ws, sse |
| P0-CRITICAL | HTTP | Origin validation | http_server, new module |
| P0-CRITICAL | HTTP | Session management | http_server, new module |
| P0-CRITICAL | All | Capability negotiation | new module, behavior |
| P1-HIGH | HTTP | GET handler for SSE | http_server |
| P1-HIGH | HTTP | Status code mapping | http_server |
| P2-MEDIUM | All | Error response data | json_rpc |
| P2-MEDIUM | Stdio | JSON validation | stdio |
| P2-MEDIUM | SSE | Last-Event-ID support | sse |

---

**Next Steps**: Implement fixes in order of priority, with comprehensive testing at each level.
