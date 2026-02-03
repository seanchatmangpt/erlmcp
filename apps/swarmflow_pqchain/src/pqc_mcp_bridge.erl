%%% @doc PQC-enabled MCP Bridge
%%%
%%% Bridges the post-quantum blockchain with Model Context Protocol (MCP) for
%%% tool/resource invocation with quantum-safe authentication.
%%%
%%% Key Features:
%%% - All MCP tool invocations are signed with ML-DSA PQC signatures
%%% - Authorization via on-chain policies
%%% - Encrypted sessions using ML-KEM key encapsulation
%%% - Audit trail: Tool invocations can be anchored on-chain
%%% - Capability verification with PQC keys
%%%
%%% Architecture:
%%% - gen_server per bridge instance
%%% - ETS table for session management
%%% - gproc for registry and routing
%%% - Request-ID correlation for async operations
%%%
%%% @end
-module(pqc_mcp_bridge).

-behaviour(gen_server).

-include("pqchain.hrl").
-include_lib("erlmcp_core/include/erlmcp_messages.hrl").

%% API exports
-export([
    start_link/1,
    call_tool/4,
    verify_tool_call/1,
    authorize_tool/3,
    list_resources/1,
    read_resource/2,
    invoke_prompt/3,
    register_capability/2,
    verify_capability/1,
    create_session/2,
    anchor_invocation/2
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

%% ETS table for sessions
-define(SESSION_TABLE, pqc_mcp_sessions).

%% Default timeouts
-define(DEFAULT_CALL_TIMEOUT, 5000).
-define(SESSION_TIMEOUT_MS, 3600000). % 1 hour
-define(REKEY_INTERVAL_MS, 1800000).  % 30 minutes

%%% ============================================================================
%%% Records
%%% ============================================================================

%% MCP request with PQC signature
-record(pqc_mcp_request, {
    id :: binary(),                      % ULID request ID
    method :: binary(),                  % MCP method (e.g., <<"tools/call">>)
    params :: map(),                     % MCP parameters
    caller :: binary(),                  % Caller's blockchain address
    signature :: #pqc_signature{},       % PQC signature over request
    session_id :: binary() | undefined,  % Optional session ID for encrypted transport
    timestamp :: non_neg_integer(),      % Unix timestamp (milliseconds)
    nonce :: non_neg_integer()          % Replay protection nonce
}).

%% MCP response with PQC authentication
-record(pqc_mcp_response, {
    id :: binary(),                      % Matches request ID
    result :: term() | undefined,        % Success result
    error :: map() | undefined,          % Error object
    signature :: #pqc_signature{},       % Bridge's signature over response
    timestamp :: non_neg_integer()       % Unix timestamp (milliseconds)
}).

%% Session with ML-KEM encrypted channel
-record(pqc_mcp_session, {
    id :: binary(),                      % ULID session ID
    caller :: binary(),                  % Caller's blockchain address
    channel :: #secure_channel{},        % ML-KEM secure channel
    created_at :: non_neg_integer(),     % Creation timestamp
    last_used :: non_neg_integer(),      % Last activity timestamp
    expires_at :: non_neg_integer(),     % Expiration timestamp
    request_count :: non_neg_integer(),  % Total requests in session
    capabilities :: [binary()]           % Granted capabilities
}).

%% Capability registration
-record(pqc_capability, {
    id :: binary(),                      % ULID capability ID
    name :: binary(),                    % Capability name
    holder :: binary(),                  % Holder's blockchain address
    public_key :: binary(),              % PQC public key for this capability
    signature :: #pqc_signature{},       % Self-signed registration
    issued_at :: non_neg_integer(),      % Issue timestamp
    expires_at :: non_neg_integer() | undefined,  % Optional expiry
    metadata :: map()                    % Additional metadata
}).

%% State record following erlmcp patterns
-record(state, {
    bridge_id :: binary(),               % Bridge instance ID
    keypair :: #pqc_keypair{},           % Bridge's signing keypair
    kem_keypair :: #pqc_keypair{},       % Bridge's KEM keypair for sessions
    policies :: map(),                   % On-chain policy cache: Address -> Policies
    capabilities :: #{binary() => #pqc_capability{}},  % Registered capabilities
    pending_requests :: #{binary() => {pid(), reference()}},  % Request correlation
    session_table :: ets:tid(),          % ETS table for sessions
    mcp_server :: pid() | undefined,     % Optional MCP server PID
    anchoring_enabled :: boolean(),      % Whether to anchor invocations on-chain
    request_id = 1 :: non_neg_integer(), % Auto-incrementing request ID
    stats :: map()                       % Statistics (requests, verifications, etc.)
}).

-type state() :: #state{}.

%%% ============================================================================
%%% Type Exports
%%% ============================================================================

-export_type([
    pqc_mcp_request/0,
    pqc_mcp_response/0,
    pqc_mcp_session/0,
    pqc_capability/0
]).

-type pqc_mcp_request() :: #pqc_mcp_request{}.
-type pqc_mcp_response() :: #pqc_mcp_response{}.
-type pqc_mcp_session() :: #pqc_mcp_session{}.
-type pqc_capability() :: #pqc_capability{}.

%%% ============================================================================
%%% API Functions
%%% ============================================================================

%% @doc Start PQC-MCP bridge with configuration
%% Config must contain:
%%   - keypair: #pqc_keypair{} (signing key)
%%   - kem_keypair: #pqc_keypair{} (optional, for sessions)
%%   - policies: map() (optional, on-chain policy cache)
%%   - mcp_server: pid() (optional, MCP server to forward to)
%%   - anchoring_enabled: boolean() (optional, default false)
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) when is_map(Config) ->
    gen_server:start_link(?MODULE, [Config], []).

%% @doc Call MCP tool with PQC-signed request
%% Method: tool name (binary)
%% Params: tool parameters (map)
%% Keypair: caller's signing keypair
%% Options: #{session_id => binary(), timeout => pos_integer()}
-spec call_tool(pid(), binary(), map(), #pqc_keypair{}) ->
    {ok, term()} | {error, term()}.
call_tool(Bridge, Method, Params, CallerKeypair) ->
    call_tool(Bridge, Method, Params, CallerKeypair, #{}).

call_tool(Bridge, Method, Params, CallerKeypair, Options)
    when is_binary(Method), is_map(Params), is_record(CallerKeypair, pqc_keypair), is_map(Options) ->
    Timeout = maps:get(timeout, Options, ?DEFAULT_CALL_TIMEOUT),
    gen_server:call(Bridge, {call_tool, Method, Params, CallerKeypair, Options}, Timeout).

%% @doc Verify that a tool call has a valid PQC signature
-spec verify_tool_call(pqc_mcp_request()) -> {ok, verified} | {error, term()}.
verify_tool_call(#pqc_mcp_request{} = Request) ->
    try
        %% Extract signature and reconstruct message
        #pqc_mcp_request{
            id = Id,
            method = Method,
            params = Params,
            caller = Caller,
            signature = Signature,
            timestamp = Timestamp,
            nonce = Nonce
        } = Request,

        %% Reconstruct signed message (canonical encoding)
        Message = sign_message_data(Id, Method, Params, Caller, Timestamp, Nonce),

        %% Verify signature
        PublicKey = Signature#pqc_signature.public_key_hash,
        case pqc_crypto:verify(Message, Signature, PublicKey) of
            {ok, true} -> {ok, verified};
            {ok, false} -> {error, invalid_signature};
            {error, Reason} -> {error, {verification_failed, Reason}}
        end
    catch
        error:Reason:Stack ->
            {error, {verification_error, Reason, Stack}}
    end.

%% @doc Check if caller is authorized to use tool (via on-chain policy)
-spec authorize_tool(pid(), binary(), binary()) -> {ok, authorized} | {error, term()}.
authorize_tool(Bridge, CallerAddress, ToolName)
    when is_binary(CallerAddress), is_binary(ToolName) ->
    gen_server:call(Bridge, {authorize_tool, CallerAddress, ToolName}, ?DEFAULT_CALL_TIMEOUT).

%% @doc List MCP resources with PQC-signed response
-spec list_resources(pid()) -> {ok, [map()]} | {error, term()}.
list_resources(Bridge) ->
    gen_server:call(Bridge, list_resources, ?DEFAULT_CALL_TIMEOUT).

%% @doc Read MCP resource with PQC authentication
-spec read_resource(pid(), binary()) -> {ok, map()} | {error, term()}.
read_resource(Bridge, ResourceUri) when is_binary(ResourceUri) ->
    gen_server:call(Bridge, {read_resource, ResourceUri}, ?DEFAULT_CALL_TIMEOUT).

%% @doc Invoke MCP prompt with PQC signature
-spec invoke_prompt(pid(), binary(), map()) -> {ok, term()} | {error, term()}.
invoke_prompt(Bridge, PromptName, Arguments)
    when is_binary(PromptName), is_map(Arguments) ->
    gen_server:call(Bridge, {invoke_prompt, PromptName, Arguments}, ?DEFAULT_CALL_TIMEOUT).

%% @doc Register MCP capability with PQC key
-spec register_capability(pid(), #pqc_capability{}) -> ok | {error, term()}.
register_capability(Bridge, #pqc_capability{} = Capability) ->
    gen_server:call(Bridge, {register_capability, Capability}, ?DEFAULT_CALL_TIMEOUT).

%% @doc Verify capability registration signature
-spec verify_capability(#pqc_capability{}) -> {ok, verified} | {error, term()}.
verify_capability(#pqc_capability{} = Capability) ->
    try
        #pqc_capability{
            id = Id,
            name = Name,
            holder = Holder,
            public_key = PubKey,
            signature = Signature,
            issued_at = IssuedAt
        } = Capability,

        %% Reconstruct signed message
        Message = capability_message_data(Id, Name, Holder, PubKey, IssuedAt),

        %% Verify self-signature
        case pqc_crypto:verify(Message, Signature, Signature#pqc_signature.public_key_hash) of
            {ok, true} -> {ok, verified};
            {ok, false} -> {error, invalid_capability_signature};
            {error, Reason} -> {error, {verification_failed, Reason}}
        end
    catch
        error:Reason:Stack ->
            {error, {verification_error, Reason, Stack}}
    end.

%% @doc Create MCP session with ML-KEM encrypted channel
-spec create_session(pid(), #pqc_keypair{}) -> {ok, binary(), #pqc_mcp_session{}} | {error, term()}.
create_session(Bridge, CallerKeypair) when is_record(CallerKeypair, pqc_keypair) ->
    gen_server:call(Bridge, {create_session, CallerKeypair}, ?DEFAULT_CALL_TIMEOUT).

%% @doc Anchor tool invocation on-chain for audit trail
-spec anchor_invocation(pid(), pqc_mcp_request()) -> {ok, binary()} | {error, term()}.
anchor_invocation(Bridge, #pqc_mcp_request{} = Request) ->
    gen_server:call(Bridge, {anchor_invocation, Request}, ?DEFAULT_CALL_TIMEOUT).

%%% ============================================================================
%%% gen_server Callbacks
%%% ============================================================================

%% @doc Initialize bridge - async setup pattern
init([Config]) ->
    process_flag(trap_exit, true),

    %% Validate required config
    case validate_config(Config) of
        {ok, ValidatedConfig} ->
            %% Create ETS table for sessions
            SessionTable = ets:new(?SESSION_TABLE, [
                set,
                public,
                {read_concurrency, true},
                {write_concurrency, true},
                {keypos, #pqc_mcp_session.id}
            ]),

            %% Extract configuration
            BridgeId = generate_ulid(),
            Keypair = maps:get(keypair, ValidatedConfig),
            KemKeypair = maps:get(kem_keypair, ValidatedConfig, undefined),
            Policies = maps:get(policies, ValidatedConfig, #{}),
            McpServer = maps:get(mcp_server, ValidatedConfig, undefined),
            AnchoringEnabled = maps:get(anchoring_enabled, ValidatedConfig, false),

            %% Register with gproc
            register_with_gproc(BridgeId),

            %% Initialize state
            State = #state{
                bridge_id = BridgeId,
                keypair = Keypair,
                kem_keypair = KemKeypair,
                policies = Policies,
                capabilities = #{},
                pending_requests = #{},
                session_table = SessionTable,
                mcp_server = McpServer,
                anchoring_enabled = AnchoringEnabled,
                stats = #{
                    total_requests => 0,
                    verified_requests => 0,
                    failed_verifications => 0,
                    active_sessions => 0
                }
            },

            %% Schedule session cleanup
            schedule_session_cleanup(),

            {ok, State};
        {error, Reason} ->
            {stop, {invalid_config, Reason}}
    end.

%% @doc Handle synchronous calls
handle_call({call_tool, Method, Params, CallerKeypair, Options}, From, State) ->
    %% Create signed request
    case create_signed_request(Method, Params, CallerKeypair, Options, State) of
        {ok, Request} ->
            %% Verify signature
            case verify_tool_call(Request) of
                {ok, verified} ->
                    %% Check authorization
                    CallerAddress = pqc_crypto:derive_address(CallerKeypair#pqc_keypair.public_key),
                    case check_authorization(CallerAddress, Method, State) of
                        {ok, authorized} ->
                            %% Forward to MCP server or handle locally
                            handle_tool_call(Request, From, State);
                        {error, _} = Error ->
                            {reply, Error, State}
                    end;
                {error, _} = Error ->
                    NewStats = increment_stat(failed_verifications, State#state.stats),
                    {reply, Error, State#state{stats = NewStats}}
            end;
        {error, _} = Error ->
            {reply, Error, State}
    end;

handle_call({authorize_tool, CallerAddress, ToolName}, _From, State) ->
    Result = check_authorization(CallerAddress, ToolName, State),
    {reply, Result, State};

handle_call(list_resources, _From, State) ->
    %% Query MCP server for resources
    case State#state.mcp_server of
        undefined ->
            {reply, {error, no_mcp_server}, State};
        McpServer ->
            %% Forward to MCP server
            Result = forward_to_mcp_server(McpServer, list_resources, #{}),
            {reply, Result, State}
    end;

handle_call({read_resource, ResourceUri}, _From, State) ->
    case State#state.mcp_server of
        undefined ->
            {reply, {error, no_mcp_server}, State};
        McpServer ->
            Result = forward_to_mcp_server(McpServer, read_resource, #{uri => ResourceUri}),
            {reply, Result, State}
    end;

handle_call({invoke_prompt, PromptName, Arguments}, _From, State) ->
    case State#state.mcp_server of
        undefined ->
            {reply, {error, no_mcp_server}, State};
        McpServer ->
            Result = forward_to_mcp_server(McpServer, invoke_prompt, #{
                name => PromptName,
                arguments => Arguments
            }),
            {reply, Result, State}
    end;

handle_call({register_capability, Capability}, _From, State) ->
    %% Verify capability signature
    case verify_capability(Capability) of
        {ok, verified} ->
            %% Store capability
            CapId = Capability#pqc_capability.id,
            NewCapabilities = maps:put(CapId, Capability, State#state.capabilities),
            {reply, ok, State#state{capabilities = NewCapabilities}};
        {error, _} = Error ->
            {reply, Error, State}
    end;

handle_call({create_session, CallerKeypair}, _From, State) ->
    case State#state.kem_keypair of
        undefined ->
            {reply, {error, kem_not_configured}, State};
        KemKeypair ->
            %% Perform ML-KEM key encapsulation
            CallerAddress = pqc_crypto:derive_address(CallerKeypair#pqc_keypair.public_key),
            case create_secure_session(CallerAddress, CallerKeypair, KemKeypair, State) of
                {ok, SessionId, Session, NewState} ->
                    %% Store session in ETS
                    ets:insert(State#state.session_table, Session),
                    {reply, {ok, SessionId, Session}, NewState};
                {error, _} = Error ->
                    {reply, Error, State}
            end
    end;

handle_call({anchor_invocation, Request}, From, State) ->
    case State#state.anchoring_enabled of
        false ->
            {reply, {error, anchoring_disabled}, State};
        true ->
            %% Anchor on-chain asynchronously
            spawn_link(fun() ->
                TxId = anchor_request_on_chain(Request, State),
                gen_server:reply(From, {ok, TxId})
            end),
            {noreply, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @doc Handle asynchronous casts
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Handle info messages
handle_info(cleanup_sessions, State) ->
    %% Remove expired sessions
    Now = erlang:system_time(millisecond),
    ets:select_delete(State#state.session_table, [
        {#pqc_mcp_session{expires_at = '$1', _ = '_'},
         [{'<', '$1', Now}],
         [true]}
    ]),

    %% Reschedule
    schedule_session_cleanup(),
    {noreply, State};

handle_info({'DOWN', _Ref, process, _Pid, _Reason}, State) ->
    %% Handle monitored process death (future: track MCP server)
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%% @doc Cleanup on termination
terminate(_Reason, State) ->
    %% Delete ETS table
    catch ets:delete(State#state.session_table),

    %% Unregister from gproc
    catch gproc:unreg({n, l, {pqc_mcp_bridge, State#state.bridge_id}}),
    ok.

%% @doc Handle code upgrades
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% ============================================================================
%%% Internal Functions
%%% ============================================================================

%% @doc Validate configuration map
validate_config(Config) ->
    try
        %% Require keypair
        case maps:get(keypair, Config, undefined) of
            #pqc_keypair{} = Keypair ->
                ok;
            _ ->
                throw({missing_required, keypair})
        end,

        %% Validate optional KEM keypair
        case maps:get(kem_keypair, Config, undefined) of
            undefined -> ok;
            #pqc_keypair{} -> ok;
            _ -> throw({invalid_type, kem_keypair})
        end,

        %% Validate optional policies
        case maps:get(policies, Config, #{}) of
            Policies when is_map(Policies) -> ok;
            _ -> throw({invalid_type, policies})
        end,

        {ok, Config}
    catch
        throw:Error ->
            {error, Error}
    end.

%% @doc Register with gproc for service discovery
register_with_gproc(BridgeId) ->
    gproc:reg({n, l, {pqc_mcp_bridge, BridgeId}}),
    gproc:reg({p, l, pqc_mcp_bridge}),
    ok.

%% @doc Schedule periodic session cleanup
schedule_session_cleanup() ->
    %% Run cleanup every 5 minutes
    erlang:send_after(300000, self(), cleanup_sessions).

%% @doc Create signed request from parameters
create_signed_request(Method, Params, CallerKeypair, Options, State) ->
    try
        RequestId = generate_ulid(),
        Timestamp = erlang:system_time(millisecond),
        Nonce = State#state.request_id,
        CallerAddress = pqc_crypto:derive_address(CallerKeypair#pqc_keypair.public_key),
        SessionId = maps:get(session_id, Options, undefined),

        %% Sign request
        Message = sign_message_data(RequestId, Method, Params, CallerAddress, Timestamp, Nonce),
        case pqc_crypto:sign(Message, CallerKeypair, CallerKeypair#pqc_keypair.algorithm) of
            {ok, Signature} ->
                Request = #pqc_mcp_request{
                    id = RequestId,
                    method = Method,
                    params = Params,
                    caller = CallerAddress,
                    signature = Signature,
                    session_id = SessionId,
                    timestamp = Timestamp,
                    nonce = Nonce
                },
                {ok, Request};
            {error, Reason} ->
                {error, {signing_failed, Reason}}
        end
    catch
        error:Reason:Stack ->
            {error, {request_creation_failed, Reason, Stack}}
    end.

%% @doc Reconstruct canonical message for signing/verification
sign_message_data(Id, Method, Params, Caller, Timestamp, Nonce) ->
    %% Use deterministic encoding (term_to_binary with sorted maps)
    Data = #{
        id => Id,
        method => Method,
        params => Params,
        caller => Caller,
        timestamp => Timestamp,
        nonce => Nonce
    },
    term_to_binary(Data, [deterministic]).

%% @doc Reconstruct capability message for signing/verification
capability_message_data(Id, Name, Holder, PubKey, IssuedAt) ->
    Data = #{
        id => Id,
        name => Name,
        holder => Holder,
        public_key => PubKey,
        issued_at => IssuedAt
    },
    term_to_binary(Data, [deterministic]).

%% @doc Check if caller is authorized to use tool
check_authorization(CallerAddress, ToolName, State) ->
    %% Check on-chain policies
    case maps:get(CallerAddress, State#state.policies, undefined) of
        undefined ->
            %% No policy found - deny by default
            {error, unauthorized};
        Policy ->
            %% Check if tool is allowed in policy
            case is_tool_allowed(ToolName, Policy) of
                true -> {ok, authorized};
                false -> {error, unauthorized}
            end
    end.

%% @doc Check if tool is allowed by policy
is_tool_allowed(ToolName, Policy) when is_map(Policy) ->
    %% Simple policy check - can be extended with on-chain validation
    AllowedTools = maps:get(allowed_tools, Policy, []),
    lists:member(ToolName, AllowedTools) orelse AllowedTools =:= <<"*">>.

%% @doc Handle tool call - forward to MCP server
handle_tool_call(Request, From, State) ->
    case State#state.mcp_server of
        undefined ->
            {reply, {error, no_mcp_server}, State};
        McpServer ->
            %% Forward request asynchronously
            spawn_link(fun() ->
                Result = forward_to_mcp_server(McpServer, call_tool, #{
                    name => Request#pqc_mcp_request.method,
                    arguments => Request#pqc_mcp_request.params
                }),

                %% Sign response
                Response = sign_response(Request#pqc_mcp_request.id, Result, State),

                %% Anchor if enabled
                case State#state.anchoring_enabled of
                    true -> spawn(fun() -> anchor_request_on_chain(Request, State) end);
                    false -> ok
                end,

                gen_server:reply(From, {ok, Response})
            end),

            %% Update stats
            NewStats = increment_stat(total_requests, State#state.stats),
            NewStats2 = increment_stat(verified_requests, NewStats),
            {noreply, State#state{stats = NewStats2, request_id = State#state.request_id + 1}}
    end.

%% @doc Sign MCP response with bridge's key
sign_response(RequestId, Result, State) ->
    Timestamp = erlang:system_time(millisecond),
    Message = term_to_binary(#{id => RequestId, result => Result, timestamp => Timestamp}, [deterministic]),

    {ok, Signature} = pqc_crypto:sign(Message, State#state.keypair, State#state.keypair#pqc_keypair.algorithm),

    #pqc_mcp_response{
        id = RequestId,
        result = Result,
        error = undefined,
        signature = Signature,
        timestamp = Timestamp
    }.

%% @doc Forward request to MCP server (placeholder - integrate with erlmcp_server)
forward_to_mcp_server(_McpServer, _Method, _Params) ->
    %% TODO: Integrate with actual erlmcp_server API
    {ok, <<"placeholder_result">>}.

%% @doc Create secure session with ML-KEM
create_secure_session(CallerAddress, CallerKeypair, BridgeKemKeypair, State) ->
    try
        SessionId = generate_ulid(),
        Now = erlang:system_time(millisecond),

        %% Perform ML-KEM encapsulation
        case pqc_crypto:kem_encapsulate(CallerKeypair#pqc_keypair.public_key,
                                        CallerKeypair#pqc_keypair.algorithm) of
            {ok, Encapsulation} ->
                %% Create secure channel record
                Channel = #secure_channel{
                    id = SessionId,
                    local_peer = pqc_crypto:derive_address(BridgeKemKeypair#pqc_keypair.public_key),
                    remote_peer = CallerAddress,
                    kem_algorithm = CallerKeypair#pqc_keypair.algorithm,
                    shared_secret = Encapsulation#pqc_encapsulation.shared_secret,
                    session_keys = derive_session_keys(Encapsulation#pqc_encapsulation.shared_secret),
                    established_at = Now,
                    last_rekey = Now,
                    rekey_interval_ms = ?REKEY_INTERVAL_MS,
                    messages_sent = 0,
                    messages_received = 0
                },

                %% Create session record
                Session = #pqc_mcp_session{
                    id = SessionId,
                    caller = CallerAddress,
                    channel = Channel,
                    created_at = Now,
                    last_used = Now,
                    expires_at = Now + ?SESSION_TIMEOUT_MS,
                    request_count = 0,
                    capabilities = []
                },

                %% Update stats
                NewStats = increment_stat(active_sessions, State#state.stats),
                {ok, SessionId, Session, State#state{stats = NewStats}};
            {error, Reason} ->
                {error, {kem_failed, Reason}}
        end
    catch
        error:Reason:Stack ->
            {error, {session_creation_failed, Reason, Stack}}
    end.

%% @doc Derive session keys from shared secret
derive_session_keys(SharedSecret) ->
    %% Use HKDF-like derivation (simplified for now)
    SendKey = pqc_crypto:hash(<<SharedSecret/binary, "send">>, ?HASH_SHA3_256),
    ReceiveKey = pqc_crypto:hash(<<SharedSecret/binary, "receive">>, ?HASH_SHA3_256),
    ChainKey = pqc_crypto:hash(<<SharedSecret/binary, "chain">>, ?HASH_SHA3_256),

    #session_keys{
        send_key = SendKey,
        receive_key = ReceiveKey,
        send_nonce = 0,
        receive_nonce = 0,
        chain_key = ChainKey
    }.

%% @doc Anchor request on blockchain (placeholder)
anchor_request_on_chain(Request, _State) ->
    %% TODO: Create transaction and submit to blockchain
    %% For now, just return a placeholder transaction ID
    TxId = generate_ulid(),
    logger:info("Anchored request ~s on-chain with tx ~s",
                [Request#pqc_mcp_request.id, TxId]),
    TxId.

%% @doc Increment statistic counter
increment_stat(Key, Stats) ->
    Current = maps:get(Key, Stats, 0),
    maps:put(Key, Current + 1, Stats).

%% @doc Generate ULID (sortable unique ID)
generate_ulid() ->
    %% Simple implementation - timestamp + random
    Timestamp = erlang:system_time(millisecond),
    Random = crypto:strong_rand_bytes(10),
    <<Timestamp:48, Random/binary>>.
