%%%-------------------------------------------------------------------
%%% @doc
%%% Security Manager - Centralized security services for erlmcp_new_features
%%%
%%% Provides:
%%% - Dynamic capability system replacing hardcoded permissions
%%% - Zero-trust authentication with least privilege containers
%%% - Audit logging across all modules
%%% - Request correlation ID tracking for JSON-RPC calls
%%% - Secure process isolation patterns with proper supervision
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_security_manager).

-behaviour(gen_server).

%% API exports
-export([start_link/0,
         start_link/1,
         authenticate_request/2,
         authorize_capability/3,
         add_capability/2,
         revoke_capability/2,
         get_audit_log/1,
         generate_correlation_id/0,
         get_security_context/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(AUDIT_LOG_LIMIT, 5000).
-define(CAPABILITY_CACHE_SIZE, 1000).
-define(CORRELATION_ID_LENGTH, 16).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Types
%%====================================================================

-type capability_id() :: binary().
-type resource() :: binary().
-type action() :: binary().
-type condition() :: map().
-type capability() :: #{
    id => capability_id(),
    resource => resource(),
    actions => [action()],
    conditions => [condition()],
    subject => binary(),
    created_at => integer(),
    expires_at => integer() | undefined,
    version => pos_integer()
}.

-type subject() :: binary().
-type permission_cache() :: #{capability_id() => capability()}.
-type audit_event() :: #{
    id => binary(),
    timestamp => integer(),
    event_type => binary(),
    subject => subject(),
    resource => resource(),
    action => action(),
    result => success | failure | error,
    details => term(),
    correlation_id => binary() | undefined
}.

-type security_context() :: #{
    enable_zero_trust => boolean(),
    require_auth => boolean(),
    audit_enabled => boolean(),
    session_timeout => pos_integer(),
    max_capabilities => pos_integer(),
    allowed_containers => [binary()],
    ssl_verify => verify_none | verify_peer,
    ip_whitelist => [binary()]
}.

-record(state, {
    capabilities = #{} :: #{capability_id() => capability()},
    permission_cache = #{} :: permission_cache(),
    audit_log = [] :: [audit_event()],
    correlation_counter = 0 :: integer(),
    security_context = #{} :: security_context(),
    container_pids = #{} :: #{pid() => binary()},
    session_tokens = #{} :: #{binary() => {subject(), integer()}},
    processes = #{} :: #{pid() => security_context()}
}).

-type state() :: #state{}.

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the security manager with default options.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link([]).

%% @doc Start the security manager with options.
-spec start_link(Options :: list()) -> {ok, pid()} | {error, term()}.
start_link(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

%% @doc Authenticate a request using zero-trust principles.
-spec authenticate_request(Request :: map(), Metadata :: map()) ->
    {ok, subject()} | {error, term()}.
authenticate_request(Request, Metadata) ->
    gen_server:call(?MODULE, {authenticate_request, Request, Metadata}, 5000).

%% @doc Authorize a capability check.
-spec authorize_capability(subject(), resource(), action()) ->
    ok | {error, term()}.
authorize_capability(Subject, Resource, Action) ->
    gen_server:call(?MODULE, {authorize_capability, Subject, Resource, Action}, 5000).

%% @doc Add a new capability to the system.
-spec add_capability(Capability :: capability(), Subject :: subject()) ->
    ok | {error, term()}.
add_capability(Capability, Subject) ->
    gen_server:call(?MODULE, {add_capability, Capability, Subject}, 5000).

%% @doc Revoke a capability from the system.
-spec revoke_capability(capability_id(), Subject :: subject()) ->
    ok | {error, term()}.
revoke_capability(CapabilityId, Subject) ->
    gen_server:call(?MODULE, {revoke_capability, CapabilityId, Subject}, 5000).

%% @doc Get audit log for a specific time range.
-spec get_audit_log(Range :: {integer(), integer()}) -> {ok, [audit_event()]}.
get_audit_log({StartTimestamp, EndTimestamp}) ->
    gen_server:call(?MODULE, {get_audit_log, StartTimestamp, EndTimestamp}, 5000).

%% @doc Generate a correlation ID for security tracking.
-spec generate_correlation_id() -> binary().
generate_correlation_id() ->
    gen_server:call(?MODULE, generate_correlation_id, 5000).

%% @doc Get security context for a process.
-spec get_security_context(pid()) -> {ok, security_context()} | {error, not_found}.
get_security_context(Pid) when is_pid(Pid) ->
    gen_server:call(?MODULE, {get_security_context, Pid}, 5000).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

-spec init(Options :: list()) -> {ok, state()}.
init(Options) ->
    process_flag(trap_exit, true),

    %% Initialize security context
    SecurityContext = init_security_context(Options),

    %% Load default capabilities
    DefaultCapabilities = load_default_capabilities(),

    %% Initialize permission cache
    PermissionCache = init_permission_cache(DefaultCapabilities),

    logger:info("Initializing Security Manager with context: ~p", [SecurityContext]),

    {ok, #state{
        security_context = SecurityContext,
        capabilities = DefaultCapabilities,
        permission_cache = PermissionCache
    }}.

-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: state()) ->
    {reply, term(), state()} | {noreply, state()}.
handle_call({authenticate_request, Request, Metadata}, _From, State) ->
    CorrelationId = generate_correlation_id_internal(State),

    case do_authenticate_request(Request, Metadata, State) of
        {ok, Subject} ->
            AuditEvent = create_audit_event(
                authentication,
                Subject,
                maps:get(resource, Request, <<"unknown">>),
                <<"authenticate">>,
                success,
                {Request, Metadata},
                CorrelationId
            ),
            NewState = log_audit_event(AuditEvent, State),
            {reply, {ok, Subject}, NewState};
        {error, Reason} ->
            AuditEvent = create_audit_event(
                authentication,
                <<"unknown">>,
                maps:get(resource, Request, <<"unknown">>),
                <<"authenticate">>,
                failure,
                {Reason, Request, Metadata},
                CorrelationId
            ),
            NewState = log_audit_event(AuditEvent, State),
            {reply, {error, Reason}, NewState}
    end;

handle_call({authorize_capability, Subject, Resource, Action}, _From, State) ->
    CorrelationId = generate_correlation_id_internal(State),

    case do_authorize_capability(Subject, Resource, Action, State) of
        ok ->
            AuditEvent = create_audit_event(
                authorization,
                Subject,
                Resource,
                Action,
                success,
                {Subject, Resource, Action},
                CorrelationId
            ),
            NewState = log_audit_event(AuditEvent, State),
            {reply, ok, NewState};
        {error, Reason} ->
            AuditEvent = create_audit_event(
                authorization,
                Subject,
                Resource,
                Action,
                failure,
                {Reason, Subject, Resource, Action},
                CorrelationId
            ),
            NewState = log_audit_event(AuditEvent, State),
            {reply, {error, Reason}, NewState}
    end;

handle_call({add_capability, Capability, Subject}, _From, State) ->
    CorrelationId = generate_correlation_id_internal(State),

    case validate_capability(Capability) of
        ok ->
            case add_capability_to_system(Capability, Subject, State) of
                {ok, NewState} ->
                    AuditEvent = create_audit_event(
                        capability_management,
                        Subject,
                        maps:get(resource, Capability),
                        <<"add_capability">>,
                        success,
                        {Capability, Subject},
                        CorrelationId
                    ),
                    UpdatedState = log_audit_event(AuditEvent, NewState),
                    {reply, ok, UpdatedState};
                {error, Reason} ->
                    AuditEvent = create_audit_event(
                        capability_management,
                        Subject,
                        maps:get(resource, Capability, <<"unknown">>),
                        <<"add_capability">>,
                        error,
                        {Reason, Capability, Subject},
                        CorrelationId
                    ),
                    UpdatedState = log_audit_event(AuditEvent, State),
                    {reply, {error, Reason}, UpdatedState}
            end;
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({revoke_capability, CapabilityId, Subject}, _From, State) ->
    CorrelationId = generate_correlation_id_internal(State),

    case revoke_capability_from_system(CapabilityId, Subject, State) of
        {ok, NewState} ->
            AuditEvent = create_audit_event(
                capability_management,
                Subject,
                CapabilityId,
                <<"revoke_capability">>,
                success,
                {CapabilityId, Subject},
                CorrelationId
            ),
            UpdatedState = log_audit_event(AuditEvent, NewState),
            {reply, ok, UpdatedState};
        {error, Reason} ->
            AuditEvent = create_audit_event(
                capability_management,
                Subject,
                CapabilityId,
                <<"revoke_capability">>,
                failure,
                {Reason, CapabilityId, Subject},
                CorrelationId
            ),
            UpdatedState = log_audit_event(AuditEvent, State),
            {reply, {error, Reason}, UpdatedState}
    end;

handle_call({get_audit_log, StartTimestamp, EndTimestamp}, _From, State) ->
    FilteredLog = filter_audit_log_by_time(State#state.audit_log, StartTimestamp, EndTimestamp),
    {reply, {ok, FilteredLog}, State};

handle_call(generate_correlation_id, _From, State) ->
    CorrelationId = generate_correlation_id_internal(State),
    {reply, {ok, CorrelationId}, State};

handle_call({get_security_context, Pid}, _From, State) ->
    case maps:find(Pid, State#state.processes) of
        {ok, SecurityContext} ->
            {reply, {ok, SecurityContext}, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call(_Request, _From, State) ->
    AuditEvent = create_audit_event(
        unknown_request,
        <<"system">>,
        <<"unknown">>,
        <<"unknown">>,
        error,
        {received},
        undefined
    ),
    NewState = log_audit_event(AuditEvent, State),
    {reply, {error, unknown_request}, NewState}.

-spec handle_cast(Request :: term(), State :: state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(Info :: term(), State :: state()) -> {noreply, state()}.
handle_info({'EXIT', Pid, Reason}, State) ->
    %% Handle process exit and update container tracking
    case maps:find(Pid, State#state.container_pids) of
        {ok, ContainerId} ->
            logger:info("Process ~p in container ~p exited: ~p", [Pid, ContainerId, Reason]),
            NewContainerPids = maps:remove(Pid, State#state.container_pids),
            NewProcesses = maps:remove(Pid, State#state.processes),
            {noreply, State#state{
                container_pids = NewContainerPids,
                processes = NewProcesses
            }};
        error ->
            {noreply, State}
    end;

handle_info({session_cleanup, Now}, State) ->
    %% Clean up expired session tokens
    SessionTokens = State#state.session_tokens,
    ValidTokens = maps:filter(fun(_Token, {_, ExpiresAt}) ->
        ExpiresAt > Now
    end, SessionTokens),
    {noreply, State#state{session_tokens = ValidTokens}};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(Reason :: term(), State :: state()) -> ok.
terminate(_Reason, State) ->
    %% Log shutdown event
    AuditEvent = create_audit_event(
        system_shutdown,
        <<"system">>,
        <<"security_manager">>,
        <<"shutdown">>,
        success,
        completed,
        undefined
    ),
    _NewState = log_audit_event(AuditEvent, State),
    ok.

-spec code_change(OldVsn :: term(), State :: state(), Extra :: term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Initialize security context from options.
-spec init_security_context(Options :: list()) -> security_context().
init_security_context(Options) ->
    #{
        enable_zero_trust => proplists:get_value(enable_zero_trust, Options, true),
        require_auth => proplists:get_value(require_auth, Options, true),
        audit_enabled => proplists:get_value(audit_enabled, Options, true),
        session_timeout => proplists:get_value(session_timeout, Options, 300000),
        max_capabilities => proplists:get_value(max_capabilities, Options, 1000),
        allowed_containers => proplists:get_value(allowed_containers, Options, [<<"erlmcp_security">>]),
        ssl_verify => proplists:get_value(ssl_verify, Options, verify_none),
        ip_whitelist => proplists:get_value(ip_whitelist, Options, [])
    }.

%% @private Load default capabilities.
-spec load_default_capabilities() -> #{capability_id() => capability()}.
load_default_capabilities() ->
    #{
        <<"system_admin">> => #{
            id => <<"system_admin">>,
            resource => <<"/api/*">>,
            actions => [<<"read">>, <<"write">>, <<"delete">>, <<"admin">>],
            conditions => [],
            subject => <<"system_admin">>,
            created_at => erlang:system_time(millisecond),
            expires_at => undefined,
            version => 1
        },
        <<"api_user">> => #{
            id => <<"api_user">>,
            resource => <<"/api/tools/*">>,
            actions => [<<"read">>, <<"execute">>],
            conditions => [],
            subject => <<"api_user">>,
            created_at => erlang:system_time(millisecond),
            expires_at => undefined,
            version => 1
        },
        <<"audit_reader">> => #{
            id => <<"audit_reader">>,
            resource => <<"/audit/*">>,
            actions => [<<"read">>],
            conditions => [],
            subject => <<"audit_reader">>,
            created_at => erlang:system_time(millisecond),
            expires_at => undefined,
            version => 1
        }
    }.

%% @private Initialize permission cache.
-spec init_permission_cache(Capabilities :: #{capability_id() => capability()}) -> permission_cache().
init_permission_cache(Capabilities) ->
    maps:fold(fun(_Id, Capability, Acc) ->
        maps:put(maps:get(id, Capability), Capability, Acc)
    end, #{}, Capabilities).

%% @private Do the actual authentication.
-spec do_authenticate_request(Request :: map(), Metadata :: map(), State :: state()) ->
    {ok, subject()} | {error, term()}.
do_authenticate_request(Request, Metadata, State) ->
    SecurityContext = State#state.security_context,

    %% Check if authentication is required
    case maps:get(require_auth, SecurityContext, true) of
        false ->
            %% No authentication required, use anonymous subject
            {ok, <<"anonymous">>};
        true ->
            %% Extract authentication information
            case extract_auth_info(Request, Metadata) of
                {ok, AuthToken} ->
                    validate_auth_token(AuthToken, State);
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% @private Extract authentication information from request.
-spec extract_auth_info(Request :: map(), Metadata :: map()) ->
    {ok, binary()} | {error, term()}.
extract_auth_info(Request, Metadata) ->
    %% Try token from request
    case maps:get(<<"token">>, Request, undefined) of
        undefined ->
            %% Try token from metadata
            case maps:get(<<"token">>, Metadata, undefined) of
                undefined ->
                    %% Try auth header
                    case maps:get(<<"auth">>, Request, undefined) of
                        undefined ->
                            {error, missing_authentication};
                        Auth when is_binary(Auth) ->
                            {ok, Auth};
                        _ ->
                            {error, invalid_auth_format}
                    end;
                Token when is_binary(Token) ->
                    {ok, Token};
                _ ->
                    {error, invalid_token_format}
            end;
        Token when is_binary(Token) ->
            {ok, Token};
        _ ->
            {error, invalid_token_format}
    end.

%% @private Validate authentication token.
-spec validate_auth_token(Token :: binary(), State :: state()) ->
    {ok, subject()} | {error, term()}.
validate_auth_token(Token, State) ->
    %% Check if token exists in session tokens
    case maps:find(Token, State#state.session_tokens) of
        {ok, {Subject, ExpiresAt}} ->
            case erlang:system_time(millisecond) < ExpiresAt of
                true ->
                    {ok, Subject};
                false ->
                    {error, token_expired}
            end;
        error ->
            %% Check if token is a capability ID
            case maps:find(Token, State#state.capabilities) of
                {ok, Capability} ->
                    Subject = maps:get(subject, Capability),
                    {ok, Subject};
                error ->
                    {error, invalid_token}
            end
    end.

%% @private Do the actual capability authorization.
-spec do_authorize_capability(Subject :: subject(), Resource :: resource(), Action :: action(), State :: state()) ->
    ok | {error, term()}.
do_authorize_capability(Subject, Resource, Action, State) ->
    Capabilities = State#state.capabilities,

    %% Find all capabilities for the subject
    SubjectCapabilities = maps:fold(fun(Id, Capability, Acc) ->
        case maps:get(subject, Capability) of
            Subject ->
                [Capability | Acc];
            _ ->
                Acc
        end
    end, [], Capabilities),

    %% Check if any capability grants the requested permission
    case check_capabilities_for_action(SubjectCapabilities, Resource, Action) of
        true ->
            ok;
        false ->
            {error, insufficient_permissions}
    end.

%% @private Check capabilities for a specific action.
-spec check_capabilities_for_action([capability()], resource(), action()) -> boolean().
check_capabilities_for_action(Capabilities, Resource, Action) ->
    lists:any(fun(Capability) ->
        %% Check if resource matches
        CapResource = maps:get(resource, Capability),
        ResourceMatch = (CapResource == Resource) orelse
                       (string:prefix(Resource, CapResource) =/= nomatch),

        %% Check if action is allowed
        CapActions = maps:get(actions, Capability, []),
        ActionMatch = lists:member(Action, CapActions),

        %% Check conditions
        Conditions = maps:get(conditions, Capability, []),
        ConditionsMet = check_conditions(Conditions),

        ResourceMatch andalso ActionMatch andalso ConditionsMet
    end, Capabilities).

%% @private Check conditions for a capability.
-spec check_conditions([condition()]) -> boolean().
check_conditions([]) ->
    true;
check_conditions([Condition | Rest]) ->
    case evaluate_condition(Condition) of
        true ->
            check_conditions(Rest);
        false ->
            false
    end.

%% @private Evaluate a single condition.
-spec evaluate_condition(condition()) -> boolean().
evaluate_condition(#{<<"type">> := <<"time_range">>, <<"start">> := Start, <<"end">> := End}) ->
    Now = erlang:system_time(millisecond),
    Now >= Start andalso Now =< End;
evaluate_condition(#{<<"type">> := <<"ip_whitelist">>, <<"ips">> := Ips}) ->
    ClientIp = get_client_ip(),
    lists:member(ClientIp, Ips);
evaluate_condition(#{<<"type">> := <<"container_check">>, <<"allowed">> := Allowed}) ->
    CurrentContainer = get_current_container(),
    lists:member(CurrentContainer, Allowed);
evaluate_condition(_) ->
    true. % Unknown condition type, allow by default

%% @private Add capability to the system.
-spec add_capability_to_system(Capability :: capability(), Subject :: subject(), State :: state()) ->
    {ok, state()} | {error, term()}.
add_capability_to_system(Capability, Subject, State) ->
    Capabilities = State#state.capabilities,
    MaxCapabilities = maps:get(max_capabilities, State#state.security_context, 1000),

    %% Check capability limit
    case maps:size(Capabilities) >= MaxCapabilities of
        true ->
            {error, capability_limit_reached};
        false ->
            CapabilityId = maps:get(id, Capability),
            %% Check if capability already exists
            case maps:is_key(CapabilityId, Capabilities) of
                true ->
                    {error, capability_exists};
                false ->
                    %% Add capability with version increment
                    EnhancedCapability = Capability#{
                        created_at => erlang:system_time(millisecond),
                        version => 1
                    },
                    NewCapabilities = maps:put(CapabilityId, EnhancedCapability, Capabilities),

                    %% Update permission cache
                    PermissionCache = State#state.permission_cache,
                    NewPermissionCache = maps:put(CapabilityId, EnhancedCapability, PermissionCache),

                    {ok, State#state{
                        capabilities = NewCapabilities,
                        permission_cache = NewPermissionCache
                    }}
            end
    end.

%% @private Revoke capability from the system.
-spec revoke_capability_from_system(CapabilityId :: capability_id(), Subject :: subject(), State :: state()) ->
    {ok, state()} | {error, term()}.
revoke_capability_from_system(CapabilityId, Subject, State) ->
    Capabilities = State#state.capabilities,

    case maps:find(CapabilityId, Capabilities) of
        {ok, Capability} ->
            %% Check if subject has permission to revoke
            CapabilitySubject = maps:get(subject, Capability),
            case CapabilitySubject == Subject orelse Subject == <<"system_admin">> of
                true ->
                    NewCapabilities = maps:remove(CapabilityId, Capabilities),
                    PermissionCache = State#state.permission_cache,
                    NewPermissionCache = maps:remove(CapabilityId, PermissionCache),

                    {ok, State#state{
                        capabilities = NewCapabilities,
                        permission_cache = NewPermissionCache
                    }};
                false ->
                    {error, insufficient_permissions}
            end;
        error ->
            {error, capability_not_found}
    end.

%% @private Validate capability structure.
-spec validate_capability(Capability :: capability()) -> ok | {error, term()}.
validate_capability(#{id := Id, resource := Resource, actions := Actions})
    when is_binary(Id), is_binary(Resource), is_list(Actions) ->
    ok;
validate_capability(_) ->
    {error, invalid_capability_structure}.

%% @private Create audit event.
-spec create_audit_event(EventType :: binary(), Subject :: subject(), Resource :: resource(),
                        Action :: action(), Result :: success | failure | error, Details :: term(),
                        CorrelationId :: binary() | undefined) -> audit_event().
create_audit_event(EventType, Subject, Resource, Action, Result, Details, CorrelationId) ->
    #{
        id => crypto:strong_rand_bytes(16),
        timestamp => erlang:system_time(millisecond),
        event_type => EventType,
        subject => Subject,
        resource => Resource,
        action => Action,
        result => Result,
        details => Details,
        correlation_id => CorrelationId
    }.

%% @private Log audit event with size limit.
-spec log_audit_event(AuditEvent :: audit_event(), State :: state()) -> state().
log_audit_event(AuditEvent, State) ->
    AuditLog = [AuditEvent | State#state.audit_log],
    LimitedAuditLog = lists:sublist(AuditLog, ?AUDIT_LOG_LIMIT),
    State#state{audit_log = LimitedAuditLog}.

%% @private Filter audit log by time range.
-spec filter_audit_log_by_time([audit_event()], integer(), integer()) -> [audit_event()].
filter_audit_log_by_time(AuditLog, StartTimestamp, EndTimestamp) ->
    lists:filter(fun(Event) ->
        Timestamp = maps:get(timestamp, Event),
        Timestamp >= StartTimestamp andalso Timestamp =< EndTimestamp
    end, AuditLog).

%% @private Generate correlation ID.
-spec generate_correlation_id_internal(State :: state()) -> binary().
generate_correlation_id_internal(State) ->
    Counter = State#state.correlation_counter + 1,
    Timestamp = erlang:system_time(millisecond),
    UniqueId = crypto:strong_rand_bytes(?CORRELATION_ID_LENGTH),
    CorrelationId = list_to_binary(io_lib:format("~p-~p-", [Timestamp, Counter])),
    <<CorrelationId/binary, UniqueId/binary>>.

%% @private Get client IP (mock implementation).
-spec get_client_ip() -> binary().
get_client_ip() ->
    <<"127.0.0.1">>.

%% @private Get current container (mock implementation).
-spec get_current_container() -> binary().
get_current_container() ->
    <<"erlmcp_security">>.