%%%-------------------------------------------------------------------
%%% @doc erlmcp_auth - Authentication and Authorization Module
%%% Implements multiple auth methods: API key, JWT, OAuth2, mTLS
%%% with RBAC (Role-Based Access Control).
%%%
%%% Design:
%%% - gen_server for session management
%%% - ETS for fast permission lookups
%%% - JWT validation with jose library
%%% - OAuth2 client credentials flow
%%% - mTLS certificate verification
%%% - RBAC with roles and permissions
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_auth).
-behaviour(gen_server).

%% API exports
-export([
    start_link/0,
    start_link/1,
    authenticate/2,
    validate_jwt/1,
    validate_api_key/1,
    validate_oauth2_token/1,
    validate_mtls/1,
    check_permission/3,
    create_session/2,
    destroy_session/1,
    rotate_token/1,
    revoke_token/1,
    get_user_roles/1,
    get_role_permissions/1,
    add_role/2,
    add_permission/3,
    remove_permission/3,
    stop/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Types
-type auth_method() :: api_key | jwt | oauth2 | mtls.
-type auth_token() :: binary().
-type user_id() :: binary().
-type session_id() :: binary().
-type role() :: binary().  % <<"admin">>, <<"user">>, <<"guest">>
-type permission() :: binary().  % <<"read">>, <<"write">>, <<"execute">>, <<"delete">>
-type resource() :: binary().

-export_type([auth_method/0, auth_token/0, user_id/0, session_id/0, role/0, permission/0, resource/0]).

%% State record
-record(state, {
    sessions :: ets:tid(),           % session_id -> session_data
    api_keys :: ets:tid(),           % api_key -> user_id
    jwt_keys :: ets:tid(),           % kid -> public_key
    oauth2_config :: map(),
    mtls_config :: map(),
    rbac_roles :: ets:tid(),         % role -> [permissions]
    user_roles :: ets:tid(),         % user_id -> [roles]
    acls :: ets:tid(),               % {resource, action} -> [roles]
    revoked_tokens :: ets:tid()      % token -> revoked_at
}).

-type state() :: #state{}.

%% Session record
-record(session, {
    session_id :: session_id(),
    user_id :: user_id(),
    roles :: [role()],
    permissions :: [permission()],
    auth_method :: auth_method(),
    created_at :: integer(),
    expires_at :: integer(),
    metadata :: map()
}).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Config], []).

%% @doc Authenticate user with given method and credentials.
-spec authenticate(auth_method(), map()) ->
    {ok, session_id()} | {error, term()}.
authenticate(Method, Credentials) ->
    gen_server:call(?MODULE, {authenticate, Method, Credentials}).

%% @doc Validate JWT token and extract claims.
-spec validate_jwt(auth_token()) -> {ok, map()} | {error, term()}.
validate_jwt(Token) ->
    gen_server:call(?MODULE, {validate_jwt, Token}).

%% @doc Validate API key.
-spec validate_api_key(auth_token()) -> {ok, user_id()} | {error, term()}.
validate_api_key(ApiKey) ->
    gen_server:call(?MODULE, {validate_api_key, ApiKey}).

%% @doc Validate OAuth2 access token.
-spec validate_oauth2_token(auth_token()) -> {ok, map()} | {error, term()}.
validate_oauth2_token(Token) ->
    gen_server:call(?MODULE, {validate_oauth2_token, Token}).

%% @doc Validate mTLS certificate.
-spec validate_mtls(map()) -> {ok, user_id()} | {error, term()}.
validate_mtls(CertInfo) ->
    gen_server:call(?MODULE, {validate_mtls, CertInfo}).

%% @doc Check if user has permission for resource action.
-spec check_permission(session_id(), resource(), permission()) ->
    ok | {error, forbidden}.
check_permission(SessionId, Resource, Permission) ->
    gen_server:call(?MODULE, {check_permission, SessionId, Resource, Permission}).

%% @doc Create authenticated session.
-spec create_session(user_id(), map()) -> {ok, session_id()}.
create_session(UserId, Metadata) ->
    gen_server:call(?MODULE, {create_session, UserId, Metadata}).

%% @doc Destroy session.
-spec destroy_session(session_id()) -> ok.
destroy_session(SessionId) ->
    gen_server:call(?MODULE, {destroy_session, SessionId}).

%% @doc Rotate token for session.
-spec rotate_token(session_id()) -> {ok, auth_token()} | {error, term()}.
rotate_token(SessionId) ->
    gen_server:call(?MODULE, {rotate_token, SessionId}).

%% @doc Revoke authentication token.
-spec revoke_token(auth_token()) -> ok.
revoke_token(Token) ->
    gen_server:call(?MODULE, {revoke_token, Token}).

%% @doc Get user roles.
-spec get_user_roles(user_id()) -> {ok, [role()]} | {error, not_found}.
get_user_roles(UserId) ->
    gen_server:call(?MODULE, {get_user_roles, UserId}).

%% @doc Get role permissions.
-spec get_role_permissions(role()) -> {ok, [permission()]} | {error, not_found}.
get_role_permissions(Role) ->
    gen_server:call(?MODULE, {get_role_permissions, Role}).

%% @doc Add role to user.
-spec add_role(user_id(), role()) -> ok.
add_role(UserId, Role) ->
    gen_server:call(?MODULE, {add_role, UserId, Role}).

%% @doc Add permission to resource for roles.
-spec add_permission(resource(), permission(), [role()]) -> ok.
add_permission(Resource, Permission, Roles) ->
    gen_server:call(?MODULE, {add_permission, Resource, Permission, Roles}).

%% @doc Remove permission from resource for roles.
-spec remove_permission(resource(), permission(), [role()]) -> ok.
remove_permission(Resource, Permission, Roles) ->
    gen_server:call(?MODULE, {remove_permission, Resource, Permission, Roles}).

%% @doc Stop auth server.
-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([map()]) -> {ok, state()}.
init([Config]) ->
    process_flag(trap_exit, true),

    State = #state{
        sessions = ets:new(auth_sessions, [set, protected]),
        api_keys = ets:new(auth_api_keys, [set, protected]),
        jwt_keys = ets:new(auth_jwt_keys, [set, protected]),
        oauth2_config = maps:get(oauth2, Config, #{}),
        mtls_config = maps:get(mtls, Config, #{}),
        rbac_roles = ets:new(auth_rbac_roles, [set, protected]),
        user_roles = ets:new(auth_user_roles, [set, protected]),
        acls = ets:new(auth_acls, [bag, protected]),  % bag for multiple roles per resource
        revoked_tokens = ets:new(auth_revoked_tokens, [set, protected])
    },

    % Initialize default roles
    init_default_roles(State),

    % Load API keys from config
    init_api_keys(State, Config),

    % Load JWT public keys
    init_jwt_keys(State, Config),

    % Start cleanup timer
    erlang:send_after(60000, self(), cleanup_expired),

    logger:info("Auth server started with config: ~p", [maps:keys(Config)]),
    {ok, State}.

-spec handle_call(term(), {pid(), term()}, state()) ->
    {reply, term(), state()} | {noreply, state()}.
handle_call({authenticate, Method, Credentials}, _From, State) ->
    Result = do_authenticate(Method, Credentials, State),
    {reply, Result, State};

handle_call({validate_jwt, Token}, _From, State) ->
    Result = do_validate_jwt(Token, State),
    {reply, Result, State};

handle_call({validate_api_key, ApiKey}, _From, State) ->
    Result = do_validate_api_key(ApiKey, State),
    {reply, Result, State};

handle_call({validate_oauth2_token, Token}, _From, State) ->
    Result = do_validate_oauth2_token(Token, State),
    {reply, Result, State};

handle_call({validate_mtls, CertInfo}, _From, State) ->
    Result = do_validate_mtls(CertInfo, State),
    {reply, Result, State};

handle_call({check_permission, SessionId, Resource, Permission}, _From, State) ->
    Result = do_check_permission(SessionId, Resource, Permission, State),
    {reply, Result, State};

handle_call({create_session, UserId, Metadata}, _From, State) ->
    Result = do_create_session(UserId, Metadata, State),
    {reply, Result, State};

handle_call({destroy_session, SessionId}, _From, State) ->
    ets:delete(State#state.sessions, SessionId),
    {reply, ok, State};

handle_call({rotate_token, SessionId}, _From, State) ->
    Result = do_rotate_token(SessionId, State),
    {reply, Result, State};

handle_call({revoke_token, Token}, _From, State) ->
    ets:insert(State#state.revoked_tokens, {Token, erlang:system_time(second)}),
    logger:warning("Token revoked: ~p", [Token]),
    {reply, ok, State};

handle_call({get_user_roles, UserId}, _From, State) ->
    Result = case ets:lookup(State#state.user_roles, UserId) of
        [{_, Roles}] -> {ok, Roles};
        [] -> {error, not_found}
    end,
    {reply, Result, State};

handle_call({get_role_permissions, Role}, _From, State) ->
    Result = case ets:lookup(State#state.rbac_roles, Role) of
        [{_, Permissions}] -> {ok, Permissions};
        [] -> {error, not_found}
    end,
    {reply, Result, State};

handle_call({add_role, UserId, Role}, _From, State) ->
    Roles = case ets:lookup(State#state.user_roles, UserId) of
        [{_, ExistingRoles}] -> lists:usort([Role | ExistingRoles]);
        [] -> [Role]
    end,
    ets:insert(State#state.user_roles, {UserId, Roles}),
    {reply, ok, State};

handle_call({add_permission, Resource, Permission, Roles}, _From, State) ->
    lists:foreach(fun(Role) ->
        ets:insert(State#state.acls, {{Resource, Permission}, Role})
    end, Roles),
    {reply, ok, State};

handle_call({remove_permission, Resource, Permission, Roles}, _From, State) ->
    lists:foreach(fun(Role) ->
        ets:delete_object(State#state.acls, {{Resource, Permission}, Role})
    end, Roles),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Request, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(cleanup_expired, State) ->
    Now = erlang:system_time(second),
    cleanup_expired_sessions(State, Now),
    cleanup_revoked_tokens(State, Now),
    erlang:send_after(60000, self(), cleanup_expired),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, State) ->
    ets:delete(State#state.sessions),
    ets:delete(State#state.api_keys),
    ets:delete(State#state.jwt_keys),
    ets:delete(State#state.rbac_roles),
    ets:delete(State#state.user_roles),
    ets:delete(State#state.acls),
    ets:delete(State#state.revoked_tokens),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

%% @private Initialize default RBAC roles.
init_default_roles(State) ->
    % Admin role - full permissions
    ets:insert(State#state.rbac_roles, {<<"admin">>, [<<"read">>, <<"write">>, <<"execute">>, <<"delete">>]}),
    % User role - read, write
    ets:insert(State#state.rbac_roles, {<<"user">>, [<<"read">>, <<"write">>]}),
    % Guest role - read only
    ets:insert(State#state.rbac_roles, {<<"guest">>, [<<"read">>]}),
    ok.

%% @private Load API keys from config.
init_api_keys(State, Config) ->
    ApiKeys = maps:get(api_keys, Config, #{}),
    maps:foreach(fun(Key, UserId) ->
        ets:insert(State#state.api_keys, {Key, UserId})
    end, ApiKeys),
    ok.

%% @private Load JWT public keys from config.
init_jwt_keys(State, Config) ->
    JwtKeys = maps:get(jwt_keys, Config, #{}),
    maps:foreach(fun(Kid, PublicKey) ->
        ets:insert(State#state.jwt_keys, {Kid, PublicKey})
    end, JwtKeys),
    ok.

%% @private Authenticate with given method.
do_authenticate(api_key, #{api_key := ApiKey}, State) ->
    case do_validate_api_key(ApiKey, State) of
        {ok, UserId} -> do_create_session(UserId, #{auth_method => api_key}, State);
        Error -> Error
    end;
do_authenticate(jwt, #{token := Token}, State) ->
    case do_validate_jwt(Token, State) of
        {ok, Claims} ->
            UserId = maps:get(<<"sub">>, Claims, <<"unknown">>),
            do_create_session(UserId, #{auth_method => jwt, claims => Claims}, State);
        Error -> Error
    end;
do_authenticate(oauth2, #{token := Token}, State) ->
    case do_validate_oauth2_token(Token, State) of
        {ok, TokenInfo} ->
            UserId = maps:get(<<"user_id">>, TokenInfo, <<"unknown">>),
            do_create_session(UserId, #{auth_method => oauth2, token_info => TokenInfo}, State);
        Error -> Error
    end;
do_authenticate(mtls, CertInfo, State) ->
    case do_validate_mtls(CertInfo, State) of
        {ok, UserId} -> do_create_session(UserId, #{auth_method => mtls, cert => CertInfo}, State);
        Error -> Error
    end;
do_authenticate(_Method, _Credentials, _State) ->
    {error, unsupported_auth_method}.

%% @private Validate JWT token.
do_validate_jwt(Token, State) ->
    % Check if token is revoked
    case ets:lookup(State#state.revoked_tokens, Token) of
        [{_, _}] -> {error, token_revoked};
        [] ->
            % TODO: Implement full JWT validation with jose library
            % For now, basic structure validation
            case binary:split(Token, <<".">>, [global]) of
                [_Header, Payload, _Signature] ->
                    try
                        ClaimsJson = base64:decode(Payload),
                        Claims = jsx:decode(ClaimsJson, [return_maps]),
                        validate_jwt_claims(Claims)
                    catch
                        _:_ -> {error, invalid_jwt}
                    end;
                _ -> {error, invalid_jwt_format}
            end
    end.

%% @private Validate JWT claims (expiration, issuer, etc.).
validate_jwt_claims(Claims) ->
    Now = erlang:system_time(second),
    case maps:get(<<"exp">>, Claims, undefined) of
        undefined -> {error, missing_expiration};
        Exp when Exp > Now -> {ok, Claims};
        _ -> {error, token_expired}
    end.

%% @private Validate API key.
do_validate_api_key(ApiKey, State) ->
    case ets:lookup(State#state.api_keys, ApiKey) of
        [{_, UserId}] -> {ok, UserId};
        [] -> {error, invalid_api_key}
    end.

%% @private Validate OAuth2 access token.
do_validate_oauth2_token(Token, State) ->
    % TODO: Implement OAuth2 token introspection
    % For now, return placeholder
    Config = State#state.oauth2_config,
    case maps:get(enabled, Config, false) of
        true ->
            % Mock validation - in production, call introspection endpoint
            {ok, #{<<"user_id">> => <<"oauth2_user">>, <<"scope">> => <<"read write">>}};
        false ->
            {error, oauth2_not_configured}
    end.

%% @private Validate mTLS certificate.
do_validate_mtls(CertInfo, State) ->
    % TODO: Implement mTLS certificate validation
    % Extract CN from certificate subject
    Config = State#state.mtls_config,
    case maps:get(enabled, Config, false) of
        true ->
            Subject = maps:get(subject, CertInfo, #{}),
            CN = maps:get(cn, Subject, <<"unknown">>),
            {ok, CN};
        false ->
            {error, mtls_not_configured}
    end.

%% @private Check permission for session.
do_check_permission(SessionId, Resource, Permission, State) ->
    case ets:lookup(State#state.sessions, SessionId) of
        [{_, Session}] ->
            check_user_permission(Session#session.user_id, Resource, Permission, State);
        [] ->
            {error, invalid_session}
    end.

%% @private Check if user has permission for resource.
check_user_permission(UserId, Resource, Permission, State) ->
    case ets:lookup(State#state.user_roles, UserId) of
        [{_, Roles}] ->
            % Check if any user role has permission for resource
            AllowedRoles = ets:lookup(State#state.acls, {Resource, Permission}),
            AllowedRolesList = [Role || {_, Role} <- AllowedRoles],
            case lists:any(fun(Role) -> lists:member(Role, AllowedRolesList) end, Roles) of
                true -> ok;
                false -> {error, forbidden}
            end;
        [] ->
            {error, user_not_found}
    end.

%% @private Create new session.
do_create_session(UserId, Metadata, State) ->
    SessionId = generate_session_id(),
    Now = erlang:system_time(second),
    ExpiresAt = Now + 3600,  % 1 hour TTL

    % Get user roles
    Roles = case ets:lookup(State#state.user_roles, UserId) of
        [{_, UserRoles}] -> UserRoles;
        [] -> [<<"guest">>]  % Default guest role
    end,

    % Aggregate permissions from all roles
    Permissions = lists:usort(lists:flatmap(fun(Role) ->
        case ets:lookup(State#state.rbac_roles, Role) of
            [{_, Perms}] -> Perms;
            [] -> []
        end
    end, Roles)),

    Session = #session{
        session_id = SessionId,
        user_id = UserId,
        roles = Roles,
        permissions = Permissions,
        auth_method = maps:get(auth_method, Metadata, api_key),
        created_at = Now,
        expires_at = ExpiresAt,
        metadata = Metadata
    },

    ets:insert(State#state.sessions, {SessionId, Session}),
    logger:info("Session created for user ~p: ~p", [UserId, SessionId]),
    {ok, SessionId}.

%% @private Rotate token for session.
do_rotate_token(SessionId, State) ->
    case ets:lookup(State#state.sessions, SessionId) of
        [{_, Session}] ->
            % Generate new session ID
            NewSessionId = generate_session_id(),
            NewSession = Session#session{
                session_id = NewSessionId,
                created_at = erlang:system_time(second)
            },
            ets:insert(State#state.sessions, {NewSessionId, NewSession}),
            ets:delete(State#state.sessions, SessionId),
            {ok, NewSessionId};
        [] ->
            {error, invalid_session}
    end.

%% @private Generate random session ID.
generate_session_id() ->
    Rand = crypto:strong_rand_bytes(32),
    base64:encode(Rand).

%% @private Cleanup expired sessions.
cleanup_expired_sessions(State, Now) ->
    ets:foldl(fun({SessionId, Session}, Acc) ->
        case Session#session.expires_at < Now of
            true ->
                ets:delete(State#state.sessions, SessionId),
                logger:debug("Session expired: ~p", [SessionId]);
            false ->
                ok
        end,
        Acc
    end, ok, State#state.sessions).

%% @private Cleanup old revoked tokens (older than 7 days).
cleanup_revoked_tokens(State, Now) ->
    Threshold = Now - (7 * 24 * 3600),
    ets:foldl(fun({Token, RevokedAt}, Acc) ->
        case RevokedAt < Threshold of
            true -> ets:delete(State#state.revoked_tokens, Token);
            false -> ok
        end,
        Acc
    end, ok, State#state.revoked_tokens).
