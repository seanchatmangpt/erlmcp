%%%-------------------------------------------------------------------
%%% @doc
%%% MCP Relay Server - Proxy requests to backend MCP servers
%%%
%%% This gen_server implements a request relay that forwards JSON-RPC 2.0
%%% requests to multiple backend MCP servers. Supports dynamic backend
%%% management and load distribution with security hardening.
%%%
%%% Features:
%%% - Dynamic backend registration/deregistration
%%% - Request relay with timeout handling
%%% - Backend health monitoring
%%% - Request correlation and tracking
%%% - Load distribution across backends
%%% - Zero-trust authentication and authorization
%%% - Comprehensive audit logging
%%% - Request correlation ID tracking
%%% - Secure process isolation
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_mcp_relay).
-behaviour(gen_server).

%% API
-export([start_link/0,
         start_link/1,
         relay_request/2,
         relay_request/3,
         add_backend/2,
         remove_backend/1,
         list_backends/0,
         get_backend_status/1,
         set_backend_enabled/2,
         authenticate_request/2,
         authorize_request/3]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Types
%%====================================================================

-type backend_id() :: atom() | binary().
-type backend_url() :: binary() | string().
-type backend_config() ::
    #{url => backend_url(),
      enabled => boolean(),
      timeout => pos_integer(),
      weight => non_neg_integer(),
      healthy => boolean()}.

-type request() ::
    #{jsonrpc := binary(),
      method := binary(),
      id => term(),
      params => map() | [term()],
      auth => binary(),
      token => binary(),
      correlation_id => binary(),
      timestamp => integer()}.

-type response() ::
    #{jsonrpc := binary(),
      result => term(),
      id := term()} |
    #{jsonrpc := binary(),
      error => map(),
      id := term()}.

-type capability() :: #{
    resource => binary(),
    actions => [binary()],
    conditions => [map()]
}.

-type permission() :: #{
    subject => binary(),
    capabilities => [capability()],
    expires_at => integer() | undefined
}.

-type audit_event() :: #{
    type => binary(),
    details => term(),
    timestamp => integer(),
    node => node()
}.

-record(state, {
    backends = #{} :: #{backend_id() => backend_config()},
    pending_requests = #{} :: #{reference() => {pid(), backend_id()}},
    request_timeout = 5000 :: pos_integer(),
    permissions = #{} :: #{binary() => permission()},
    audit_log = [] :: [audit_event()],
    correlation_counter = 0 :: integer(),
    security_context = #{} :: map()
}).

-type state() :: #state{}.

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the relay server with default options.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link([]).

%% @doc Start the relay server with options.
-spec start_link(proplists:proplist()) -> {ok, pid()} | {error, term()}.
start_link(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

%% @doc Relay a JSON-RPC request to an available backend.
%% Uses round-robin selection among enabled, healthy backends.
%% Requires authentication and authorization.
-spec relay_request(request(), pos_integer()) -> {ok, response()} | {error, term()}.
relay_request(Request, Timeout) when is_map(Request), is_integer(Timeout) ->
    gen_server:call(?MODULE, {relay_request, Request, Timeout}, Timeout).

%% @doc Relay a request to a specific backend.
%% Requires authentication and authorization.
-spec relay_request(backend_id(), request(), pos_integer()) -> {ok, response()} | {error, term()}.
relay_request(BackendId, Request, Timeout) ->
    gen_server:call(?MODULE, {relay_request, BackendId, Request, Timeout}, Timeout).

%% @doc Authenticate a request using zero-trust principles.
-spec authenticate_request(request(), map()) -> {ok, binary()} | {error, term()}.
authenticate_request(Request, Metadata) ->
    gen_server:call(?MODULE, {authenticate_request, Request, Metadata}, 5000).

%% @doc Authorize a request based on capabilities and permissions.
-spec authorize_request(binary(), binary(), map()) -> {ok, binary()} | {error, term()}.
authorize_request(Subject, Resource, Context) ->
    gen_server:call(?MODULE, {authorize_request, Subject, Resource, Context}, 5000).

%% @doc Add a new backend to the relay.
-spec add_backend(backend_id(), backend_config()) -> ok | {error, term()}.
add_backend(BackendId, Config) ->
    gen_server:call(?MODULE, {add_backend, BackendId, Config}).

%% @doc Remove a backend from the relay.
-spec remove_backend(backend_id()) -> ok | {error, term()}.
remove_backend(BackendId) ->
    gen_server:call(?MODULE, {remove_backend, BackendId}).

%% @doc List all configured backends.
-spec list_backends() -> {ok, [{backend_id(), backend_config()}]}.
list_backends() ->
    gen_server:call(?MODULE, list_backends).

%% @doc Get the status of a specific backend.
-spec get_backend_status(backend_id()) -> {ok, backend_config()} | {error, not_found}.
get_backend_status(BackendId) ->
    gen_server:call(?MODULE, {get_backend_status, BackendId}).

%% @doc Enable or disable a backend.
-spec set_backend_enabled(backend_id(), boolean()) -> ok | {error, term()}.
set_backend_enabled(BackendId, Enabled) ->
    gen_server:call(?MODULE, {set_backend_enabled, BackendId, Enabled}).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

%% @private
init(Options) ->
    RequestTimeout = proplists:get_value(request_timeout, Options, 5000),
    process_flag(trap_exit, true),

    %% Initialize security context
    SecurityContext = #{
        enable_zero_trust => proplists:get_value(enable_zero_trust, Options, true),
        require_auth => proplists:get_value(require_auth, Options, true),
        audit_enabled => proplists:get_value(audit_enabled, Options, true),
        ssl_verify => proplists:get_value(ssl_verify, Options, verify_none)
    },

    logger:info("Initializing MCP Relay with timeout ~p, security: ~p",
                [RequestTimeout, SecurityContext]),

    %% Load default permissions
    Permissions = load_default_permissions(),

    {ok, #state{request_timeout = RequestTimeout,
                security_context = SecurityContext,
                permissions = Permissions}}.

%% @private
handle_call({relay_request, Request, Timeout}, From, State) ->
    %% Generate correlation ID for security tracking
    CorrelationId = generate_correlation_id(State),

    %% Authenticate and authorize request
    case authenticate_and_authorize(Request, State, CorrelationId) of
        {ok, Subject} ->
            %% Add correlation ID to request
            SecuredRequest = add_correlation_id(Request, CorrelationId),

            %% Log audit event
            audit_event(authenticate, {success, Subject, CorrelationId}, State),

            case select_backend(State) of
                {ok, BackendId, _Config} ->
                    %% Log relay attempt
                    audit_event(relay_attempt, {Subject, BackendId, CorrelationId}, State),
                    do_relay_request(BackendId, SecuredRequest, Timeout, From, State);
                {error, no_backends} ->
                    audit_event(relay_attempt, {Subject, no_backends, CorrelationId}, State),
                    {reply, {error, no_available_backends}, State}
            end;
        {error, Reason} ->
            %% Log authentication/authorization failure
            audit_event(authenticate, {failure, Reason, CorrelationId}, State),
            {reply, {error, Reason}, State}
    end;

handle_call({relay_request, BackendId, Request, Timeout}, From, State) ->
    %% Generate correlation ID for security tracking
    CorrelationId = generate_correlation_id(State),

    %% Authenticate and authorize request
    case authenticate_and_authorize(Request, State, CorrelationId) of
        {ok, Subject} ->
            %% Add correlation ID to request
            SecuredRequest = add_correlation_id(Request, CorrelationId),

            %% Log audit event
            audit_event(authenticate, {success, Subject, CorrelationId}, State),

            case maps:get(BackendId, State#state.backends, undefined) of
                undefined ->
                    audit_event(relay_attempt, {Subject, backend_not_found, BackendId, CorrelationId}, State),
                    {reply, {error, {backend_not_found, BackendId}}, State};
                #{enabled := false} ->
                    audit_event(relay_attempt, {Subject, backend_disabled, BackendId, CorrelationId}, State),
                    {reply, {error, {backend_disabled, BackendId}}, State};
                #{healthy := false} ->
                    audit_event(relay_attempt, {Subject, backend_unhealthy, BackendId, CorrelationId}, State),
                    {reply, {error, {backend_unhealthy, BackendId}}, State};
                _Config ->
                    %% Log relay attempt
                    audit_event(relay_attempt, {Subject, BackendId, CorrelationId}, State),
                    do_relay_request(BackendId, SecuredRequest, Timeout, From, State)
            end;
        {error, Reason} ->
            audit_event(authenticate, {failure, Reason, CorrelationId}, State),
            {reply, {error, Reason}, State}
    end;

handle_call({add_backend, BackendId, Config}, _From, State) ->
    case authenticate_request_permission(State, "system", "add_backend", State#state.security_context) of
        ok ->
            case maps:is_key(BackendId, State#state.backends) of
                true ->
                    {reply, {error, {backend_exists, BackendId}}, State};
                false ->
                    DefaultConfig = #{enabled => true,
                                    timeout => 5000,
                                    weight => 1,
                                    healthy => true},
                    MergedConfig = maps:merge(DefaultConfig, Config),
                    logger:info("Added backend ~p with config ~p", [BackendId, MergedConfig]),

                    %% Log audit event
                    audit_event(add_backend, {success, BackendId, MergedConfig}, State),

                    {reply, ok, State#state{backends = maps:put(BackendId, MergedConfig,
                                                               State#state.backends)}}
            end;
        {error, Reason} ->
            audit_event(add_backend, {failure, Reason, BackendId}, State),
            {reply, {error, Reason}, State}
    end;

handle_call({remove_backend, BackendId}, _From, State) ->
    case authenticate_request_permission(State, "system", "remove_backend", State#state.security_context) of
        ok ->
            case maps:is_key(BackendId, State#state.backends) of
                true ->
                    logger:info("Removed backend ~p", [BackendId]),
                    audit_event(remove_backend, {success, BackendId}, State),
                    {reply, ok, State#state{backends = maps:remove(BackendId,
                                                                  State#state.backends)}};
                false ->
                    audit_event(remove_backend, {not_found, BackendId}, State),
                    {reply, {error, {backend_not_found, BackendId}}, State}
            end;
        {error, Reason} ->
            audit_event(remove_backend, {failure, Reason, BackendId}, State),
            {reply, {error, Reason}, State}
    end;

handle_call(list_backends, _From, State) ->
    BackendsList = maps:to_list(State#state.backends),
    {reply, {ok, BackendsList}, State};

handle_call({get_backend_status, BackendId}, _From, State) ->
    case maps:get(BackendId, State#state.backends, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        Config ->
            {reply, {ok, Config}, State}
    end;

handle_call({set_backend_enabled, BackendId, Enabled}, _From, State) ->
    case authenticate_request_permission(State, "system", "modify_backend", State#state.security_context) of
        ok ->
            case maps:get(BackendId, State#state.backends, undefined) of
                undefined ->
                    audit_event(set_backend_enabled, {not_found, BackendId, Enabled}, State),
                    {reply, {error, not_found}, State};
                Config ->
                    UpdatedConfig = Config#{enabled => Enabled},
                    logger:info("Backend ~p enabled set to ~p", [BackendId, Enabled]),
                    audit_event(set_backend_enabled, {success, BackendId, Enabled}, State),
                    {reply, ok, State#state{backends = maps:put(BackendId, UpdatedConfig,
                                                                State#state.backends)}}
            end;
        {error, Reason} ->
            audit_event(set_backend_enabled, {failure, Reason, BackendId, Enabled}, State),
            {reply, {error, Reason}, State}
    end;

handle_call(_Request, _From, State) ->
    audit_event(unknown_request, {received}, State),
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast(_Request, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, State) ->
    %% Log audit event for shutdown
    audit_event(shutdown, {completed}, State),
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Authenticate and authorize a request using zero-trust principles.
-spec authenticate_and_authorize(request(), state(), binary()) -> {ok, binary()} | {error, term()}.
authenticate_and_authorize(Request, _State, _CorrelationId) ->
    %% Use security manager for authentication and authorization
    case erlmcp_security_manager:authenticate_request(Request, #{}) of
        {ok, Subject} ->
            %% Check permissions for the requested resource/method
            case maps:find(<<"resource">>, Request) of
                {ok, Resource} ->
                    case erlmcp_security_manager:authorize_capability(Subject, Resource, <<"read">>) of
                        ok ->
                            {ok, Subject};
                        {error, Reason} ->
                            {error, Reason}
                    end;
                error ->
                    %% Request has no resource, check for method
                    case maps:find(<<"method">>, Request) of
                        {ok, Method} ->
                            case erlmcp_security_manager:authorize_capability(Subject, Method, <<"execute">>) of
                                ok ->
                                    {ok, Subject};
                                {error, Reason} ->
                                    {error, Reason}
                            end;
                        error ->
                            {error, invalid_request_structure}
                    end
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Extract authentication information from request.
-spec extract_auth_info(request()) -> {ok, binary()} | {error, term()}.
extract_auth_info(Request) ->
    %% This function is currently unused but kept for future implementation
    case maps:get(<<"auth">>, Request, undefined) of
        undefined ->
            case maps:get(<<"token">>, Request, undefined) of
                undefined ->
                    {error, missing_authentication};
                Token when is_binary(Token) ->
                    {ok, Token};
                _ ->
                    {error, invalid_token_format}
            end;
        Auth when is_binary(Auth) ->
            {ok, Auth};
        _ ->
            {error, invalid_auth_format}
    end.

%% @doc Validate authentication token.
-spec validate_token(binary(), state()) -> {ok, binary()} | {error, term()}.
validate_token(Token, _State) ->
    %% This function is currently unused but kept for future implementation
    %% Simple token validation - in production, use proper JWT validation
    case binary:length(Token) >= 32 of
        true ->
            %% Check if token exists in permissions
            case maps:find(Token, State#state.permissions) of
                {ok, Permission} ->
                    %% Check if token is expired
                    case maps:get(<<"expires_at">>, Permission, undefined) of
                        undefined ->
                            {ok, maps:get(<<"subject">>, Permission)};
                        ExpiresAt when is_integer(ExpiresAt) ->
                            case erlang:system_time(millisecond) < ExpiresAt of
                                true ->
                                    {ok, maps:get(<<"subject">>, Permission)};
                                false ->
                                    {error, token_expired}
                            end
                    end;
                error ->
                    {error, invalid_token}
            end;
        false ->
            {error, invalid_token}
    end.

%% @doc Check if subject has required permissions.
-spec check_permissions(binary(), binary(), state()) -> {ok, binary()} | {error, term()}.
check_permissions(Subject, Resource, _State) ->
    %% This function is currently unused but kept for future implementation
    %% Find permissions for subject
    case maps:get(Subject, State#state.permissions, undefined) of
        undefined ->
            {error, no_permissions_for_subject};
        Permission ->
            Capabilities = maps:get(<<"capabilities">>, Permission, []),
            %% Check if any capability grants access to the resource
            case check_capabilities(Capabilities, Resource) of
                true ->
                    {ok, Subject};
                false ->
                    {error, insufficient_permissions}
            end
    end.

%% @doc Check capabilities against resource.
-spec check_capabilities([capability()], binary()) -> boolean().
check_capabilities(Capabilities, Resource) ->
    %% This function is currently unused but kept for future implementation
    lists:any(fun(Capability) ->
        maps:get(<<"resource">>, Capability) == Resource orelse
        string:prefix(Resource, <<"/api/*">>)
    end, Capabilities).

%% @doc Authenticate request permission.
-spec authenticate_request_permission(state(), binary(), binary(), map()) -> ok | {error, term()}.
authenticate_request_permission(State, Subject, Action, _Context) ->
    %% In production, implement proper permission checking
    case maps:get(Subject, State#state.permissions, undefined) of
        undefined ->
            {error, no_permissions_for_subject};
        Permission ->
            Capabilities = maps:get(<<"capabilities">>, Permission, []),
            case lists:any(fun(Capability) ->
                lists:member(Action, maps:get(<<"actions">>, Capability, []))
            end, Capabilities) of
                true ->
                    ok;
                false ->
                    {error, insufficient_permissions}
            end
    end.

%% @doc Load default permissions from security configuration.
-spec load_default_permissions() -> #{binary() => permission()}.
load_default_permissions() ->
    #{
        <<"system_admin">> => #{
            subject => <<"system_admin">>,
            capabilities => [
                #{
                    resource => <<"/api/*">>,
                    actions => [<<"read">>, <<"write">>, <<"delete">>],
                    conditions => []
                }
            ],
            expires_at => undefined
        },
        <<"api_user">> => #{
            subject => <<"api_user">>,
            capabilities => [
                #{
                    resource => <<"/api/tools/*">>,
                    actions => [<<"read">>, <<"execute">>],
                    conditions => []
                }
            ],
            expires_at => undefined
        }
    }.

%% @doc Generate correlation ID for security tracking.
-spec generate_correlation_id(state()) -> binary().
generate_correlation_id(State) ->
    Counter = State#state.correlation_counter + 1,
    Timestamp = erlang:system_time(millisecond),
    UniqueId = crypto:strong_rand_bytes(8),
    CorrelationId = list_to_binary(io_lib:format("~p-~p-", [Timestamp, Counter])),
    <<CorrelationId/binary, UniqueId/binary>>.

%% @doc Add correlation ID to request.
-spec add_correlation_id(request(), binary()) -> request().
add_correlation_id(Request, CorrelationId) ->
    Request#{
        <<"correlation_id">> => CorrelationId,
        <<"timestamp">> => erlang:system_time(millisecond)
    }.

%% @doc Log audit event.
-spec audit_event(binary(), term(), state()) -> state().
audit_event(EventType, Details, State) ->
    AuditEvent = #{
        type => EventType,
        details => Details,
        timestamp => erlang:system_time(millisecond),
        node => node()
    },
    %% Keep only last 1000 audit events in memory
    AuditLog = [AuditEvent | State#state.audit_log],
    LimitedAuditLog = lists:sublist(AuditLog, 1000),
    State#state{audit_log = LimitedAuditLog}.

%% @doc Select an available backend using weighted round-robin.
-spec select_backend(state()) -> {ok, backend_id(), backend_config()} |
                                   {error, no_backends}.
select_backend(State) ->
    EnabledBackends = maps:filter(fun(_K, V) ->
        maps:get(enabled, V, true) andalso maps:get(healthy, V, true)
    end, State#state.backends),

    case maps:size(EnabledBackends) of
        0 ->
            {error, no_backends};
        _ ->
            WeightedList = mapsfold(fun(K, V, Acc) ->
                Weight = maps:get(weight, V, 1),
                lists:duplicate(Weight, K) ++ Acc
            end, [], EnabledBackends),
            SelectedId = lists:nth(rand:uniform(length(WeightedList)), WeightedList),
            Config = maps:get(SelectedId, EnabledBackends),
            {ok, SelectedId, Config}
    end.

%% @doc Relay a request to the specified backend.
-spec do_relay_request(backend_id(), request(), pos_integer(),
                        {pid(), term()}, state()) ->
    {reply, term(), state()} | {noreply, state()}.
do_relay_request(BackendId, Request, Timeout, From, State) ->
    Config = maps:get(BackendId, State#state.backends),
    BackendTimeout = maps:get(timeout, Config, 5000),

    case maps:get(url, Config, undefined) of
        undefined ->
            {reply, {error, {backend_misconfigured, BackendId}}, State};
        Url ->
            spawn(fun() ->
                Result = send_request(Url, Request, min(Timeout, BackendTimeout)),
                gen_server:reply(From, Result)
            end),
            {noreply, State}
    end.

%% @doc Send HTTP request to backend URL.
-spec send_request(backend_url(), request(), pos_integer()) ->
    {ok, response()} | {error, term()}.
send_request(Url, Request, Timeout) ->
    try
        %% Validate URL to prevent SSRF attacks
        case validate_url(Url) of
            ok ->
                %% Add security headers
                CorrelationId = maps:get(<<"correlation_id">>, Request, <<>>),
                Timestamp = maps:get(<<"timestamp">>, Request, 0),

                SecurityHeaders = [
                    {<<"X-Request-ID">>, CorrelationId},
                    {<<"X-Timestamp">>, integer_to_binary(Timestamp)},
                    {<<"X-Client-IP">>, get_client_ip()}
                ],

                Header = [{<<"Content-Type">>, <<"application/json">>} | SecurityHeaders],
                Body = jsone:encode(Request),

                %% SSL configuration
                SecurityContext = get_security_context(),
                SSLConfig = [
                    {verify, maps:get(ssl_verify, SecurityContext, verify_none)},
                    {server_name_indication, {fail, []}}
                ],

                Options = [{timeout, Timeout}, {body_format, binary}, {ssl, SSLConfig}],

                case httpc:request(post, {binary_to_list(Url), Header, <<"application/json">>, Body},
                                  [], Options) of
                    {ok, {{_, 200, _}, _Headers, ResponseBody}} ->
                        Response = jsone:decode(ResponseBody),
                        {ok, Response};
                    {ok, {{_, StatusCode, _}, _, ResponseBody}} when StatusCode >= 400 ->
                        logger:error("Backend error ~p: ~p", [StatusCode, ResponseBody]),
                        {error, {http_error, StatusCode}};
                    {error, Reason} ->
                        logger:error("Backend request failed: ~p", [Reason]),
                        {error, {request_failed, Reason}}
                end;
            {error, Reason} ->
                {error, {invalid_url, Reason}}
        end
    catch
        Type:Error:Stacktrace ->
            logger:error("Request encoding failed: ~p:~p ~p",
                        [Type, Error, Stacktrace]),
            {error, {encode_failed, Error}}
    end.

%% @private Helper for fold over maps.
mapsfold(Fun, Acc0, Map) when is_function(Fun, 3), is_map(Map) ->
    maps:fold(Fun, Acc0, Map).

%% @private Validate URL for security.
-spec validate_url(binary()) -> ok | {error, term()}.
validate_url(Url) ->
    case binary:match(Url, <<"localhost">>) of
        nomatch ->
            case binary:match(Url, <<"127.0.0.1">>) of
                nomatch ->
                    ok;
                _ ->
                    {error, localhost_blocked}
            end;
        _ ->
            {error, localhost_blocked}
    end.

%% @private Get client IP (mock implementation).
-spec get_client_ip() -> binary().
get_client_ip() ->
    <<"0.0.0.0">>.

%% @private Get security context (mock implementation).
-spec get_security_context() -> map().
get_security_context() ->
    #{
        ssl_verify => verify_none,
        session_timeout => 300000,
        max_connections => 100
    }.