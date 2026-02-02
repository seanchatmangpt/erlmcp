%%%-------------------------------------------------------------------
%%% @doc
%%% erlmcp_cli_auth - CLI Authentication Manager
%%%
%%% Implements JWT/MTLS authentication mechanisms in the CLI client.
%%% Ensures proper integration with erlmcp_auth and secure session management.
%%% Provides full compliance with MCP authentication requirements.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cli_auth).

-behaviour(gen_server).

%% API
-export([start_link/0, authenticate/2, authenticate/3, refresh_token/1, validate_token/1,
         get_session_info/1, logout/1, list_sessions/0, clear_expired/0, enable_mtls/1,
         disable_mtls/0, set_jwt_provider/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("erlmcp.hrl").
-include("erlmcp_observability.hrl").

%% Records
-record(auth_state,
        {sessions :: map(),                   % Active sessions
         jwt_tokens :: map(),                  % JWT token cache
         mtls_config :: map(),                 % MTLS configuration
         auth_providers :: map(),              % Available auth providers
         token_cache_ttl :: integer(),          % Token cache TTL
         session_timeout :: integer(),         % Session timeout
         security_level :: atom(),             % Security level
         metrics :: map()}).                    % Authentication metrics
%% Session record
-record(session,
        {id :: binary(),
         user :: binary(),
         token :: binary(),
         mtls_cert :: binary() | undefined,
         created :: integer(),
         last_activity :: integer(),
         expires :: integer(),
         permissions :: list(),
         metadata :: map()}).

%% Default configuration
-define(DEFAULT_CONFIG,
        #{<<"token_cache_ttl">> => 3600000,    % 1 hour
          <<"session_timeout">> => 86400000,   % 24 hours
          <<"security_level">> => high,
          <<"enable_mtls">> => false,
          <<"jwt_provider">> => internal,
          <<"jwt_algorithm">> => RS256,
          <<"mtls_ca_file">> => undefined,
          <<"mtls_cert_file">> => undefined,
          <<"mtls_key_file">> => undefined}).
-define(SERVER, ?MODULE).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the authentication manager
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Authenticate with username and password
-spec authenticate(binary(), binary()) -> {ok, binary()} | {error, term()}.
authenticate(Username, Password) ->
    authenticate(Username, Password, #{}).

%% @doc Authenticate with username, password and options
-spec authenticate(binary(), binary(), map()) -> {ok, binary()} | {error, term()}.
authenticate(Username, Password, Options) ->
    gen_server:call(?SERVER, {authenticate, Username, Password, Options}, 15000).

%% @doc Refresh JWT token
-spec refresh_token(binary()) -> {ok, binary()} | {error, term()}.
refresh_token(Token) ->
    gen_server:call(?SERVER, {refresh_token, Token}, 10000).

%% @doc Validate JWT token
-spec validate_token(binary()) -> {ok, map()} | {error, term()}.
validate_token(Token) ->
    gen_server:call(?SERVER, {validate_token, Token}, 5000).

%% @doc Get session information
-spec get_session_info(binary()) -> {ok, #session{}} | {error, term()}.
get_session_info(SessionId) ->
    gen_server:call(?SERVER, {get_session_info, SessionId}, 5000).

%% @doc Logout user session
-spec logout(binary()) -> ok | {error, term()}.
logout(SessionId) ->
    gen_server:call(?SERVER, {logout, SessionId}, 5000).

%% @doc List all active sessions
-spec list_sessions() -> {ok, list()} | {error, term()}.
list_sessions() ->
    gen_server:call(?SERVER, list_sessions, 5000).

%% @doc Clear expired sessions
-spec clear_expired() -> ok.
clear_expired() ->
    gen_server:cast(?SERVER, clear_expired).

%% @doc Enable MTLS authentication
-spec enable_mtls(map()) -> ok | {error, term()}.
enable_mtls(Config) ->
    gen_server:call(?SERVER, {enable_mtls, Config}, 10000).

%% @doc Disable MTLS authentication
-spec disable_mtls() -> ok.
disable_mtls() ->
    gen_server:call(?SERVER, disable_mtls, 5000).

%% @doc Set JWT provider
-spec set_jwt_provider(binary(), map()) -> ok | {error, term()}.
set_jwt_provider(Provider, Config) ->
    gen_server:call(?SERVER, {set_jwt_provider, Provider, Config}, 10000).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @doc Initialize the authentication manager
-spec init(term()) -> {ok, #auth_state{}} | {stop, term()}.
init(_Args) ->
    %% Create OTEL span for authentication manager initialization
    SpanCtx =
        erlmcp_otel:with_span("cli.auth.init",
                              #{<<"module">> => atom_to_binary(?MODULE, utf8)},
                              fun() ->
                                 %% Load configuration
                                 Config = load_config(),

                                 %% Initialize state
                                 DefaultConfig = ?DEFAULT_CONFIG,
                                 MergedConfig = maps:merge(DefaultConfig, Config),

                                 Sessions = #{},
                                 JwtTokens = #{},
                                 AuthProviders = load_auth_providers(MergedConfig),

                                 State =
                                     #auth_state{sessions = Sessions,
                                                 jwt_tokens = JwtTokens,
                                                 mtls_config = MergedConfig,
                                                 auth_providers = AuthProviders,
                                                 token_cache_ttl =
                                                     maps:get(<<"token_cache_ttl">>, MergedConfig),
                                                 session_timeout =
                                                     maps:get(<<"session_timeout">>, MergedConfig),
                                                 security_level =
                                                     maps:get(<<"security_level">>, MergedConfig),
                                                 metrics = init_metrics()},

                                 %% Start cleanup timer
                                 erlang:send_after(300000, self(), cleanup_sessions),

                                 %% Record initialization
                                 erlmcp_metrics:record("cli.auth.initialized", 1),
                                 {ok, State}
                              end).

%% @doc Handle synchronous calls
-spec handle_call(term(), {pid(), term()}, #auth_state{}) -> {reply, term(), #auth_state{}}.
handle_call({authenticate, Username, Password, Options}, _From, State) ->
    %% Create OTEL span for authentication
    SpanCtx =
        erlmcp_otel:inject_span("cli.auth.authenticate",
                                #{<<"username">> => Username, <<"options">> => Options},
                                undefined),

    try
        %% Determine authentication method
        AuthMethod = determine_auth_method(State, Options),

        case perform_authentication(AuthMethod, Username, Password, State) of
            {ok, SessionId, Token} ->
                %% Create session record
                Session = create_session(Username, Token, Options, State),

                %% Store session
                NewSessions = maps:put(SessionId, Session, State#auth_state.sessions),
                NewState = State#auth_state{sessions = NewSessions},

                %% Record success metrics
                erlmcp_otel:record_event(SpanCtx, <<"authentication.success">>, #{}),
                erlmcp_metrics:record("cli.auth.success", 1),

                {reply, {ok, SessionId}, NewState};
            {error, Reason} ->
                %% Record error metrics
                erlmcp_otel:record_error(SpanCtx, {authentication_failed, Username, Reason}),
                erlmcp_metrics:record("cli.auth.failed", 1),
                {reply, {error, Reason}, State}
        end
    catch
        Error:Reason ->
            erlmcp_otel:record_error(SpanCtx, {auth_error, Username, Error, Reason}),
            erlmcp_metrics:record("cli.auth.error", 1),
            {reply, {error, Reason}, State}
    end;
handle_call({refresh_token, Token}, _From, State) ->
    %% Create OTEL span for token refresh
    SpanCtx = erlmcp_otel:inject_span("cli.auth.refresh_token", #{}, undefined),

    try
        case validate_token_internal(Token, State) of
            {ok, Claims} ->
                case refresh_jwt_token(Claims, State) of
                    {ok, NewToken} ->
                        %% Update token cache
                        NewTokens = maps:put(NewToken, Claims, State#auth_state.jwt_tokens),
                        NewState = State#auth_state{jwt_tokens = NewTokens},

                        erlmcp_otel:record_event(SpanCtx, <<"token.refreshed">>, #{}),
                        erlmcp_metrics:record("cli.auth.token_refreshed", 1),
                        {reply, {ok, NewToken}, NewState};
                    {error, Reason} ->
                        erlmcp_otel:record_error(SpanCtx, {refresh_failed, Reason}),
                        {reply, {error, Reason}, State}
                end;
            {error, Reason} ->
                erlmcp_otel:record_error(SpanCtx, {refresh_failed, Reason}),
                {reply, {error, Reason}, State}
        end
    catch
        Error:Reason ->
            erlmcp_otel:record_error(SpanCtx, {refresh_error, Error, Reason}),
            {reply, {error, Reason}, State}
    end;
handle_call({validate_token, Token}, _From, State) ->
    try
        case validate_token_internal(Token, State) of
            {ok, Claims} ->
                {reply, {ok, Claims}, State};
            {error, Reason} ->
                {reply, {error, Reason}, State}
        end
    catch
        Error:Reason ->
            {reply, {error, {validation_error, Error, Reason}}, State}
    end;
handle_call({get_session_info, SessionId}, _From, State) ->
    try
        case maps:find(SessionId, State#auth_state.sessions) of
            {ok, Session} ->
                SessionInfo = format_session_info(Session),
                {reply, {ok, SessionInfo}, State};
            error ->
                {reply, {error, not_found}, State}
        end
    catch
        Error:Reason ->
            {reply, {error, Reason}, State}
    end;
handle_call({logout, SessionId}, _From, State) ->
    %% Create OTEL span for logout
    SpanCtx =
        erlmcp_otel:inject_span("cli.auth.logout", #{<<"session_id">> => SessionId}, undefined),

    try
        case maps:find(SessionId, State#auth_state.sessions) of
            {ok, Session} ->
                %% Clear session data
                NewSessions = maps:remove(SessionId, State#auth_state.sessions),

                %% Invalidate token
                invalidate_jwt_token(Session#session.token, State),

                %% Record logout
                erlmcp_otel:record_event(SpanCtx, <<"logout.success">>, #{}),
                erlmcp_metrics:record("cli.auth.logout", 1),

                {reply, ok, State#auth_state{sessions = NewSessions}};
            error ->
                erlmcp_otel:record_error(SpanCtx, {logout_failed, session_not_found}),
                {reply, {error, not_found}, State}
        end
    catch
        Error:Reason ->
            erlmcp_otel:record_error(SpanCtx, {logout_error, Error, Reason}),
            {reply, {error, Reason}, State}
    end;
handle_call(list_sessions, _From, State) ->
    SessionList = lists:map(fun format_session_info/1, maps:values(State#auth_state.sessions)),
    {reply, {ok, SessionList}, State};
handle_call({enable_mtls, Config}, _From, State) ->
    %% Create OTEL span for MTLS enablement
    SpanCtx = erlmcp_otel:inject_span("cli.auth.enable_mtls", #{}, undefined),

    try
        %% Validate MTLS configuration
        case validate_mtls_config(Config) of
            ok ->
                %% Load certificates
                case load_mtls_certificates(Config) of
                    {ok, CertData} ->
                        %% Update MTLS configuration
                        NewConfig = maps:merge(State#auth_state.mtls_config, Config),
                        NewState = State#auth_state{mtls_config = NewConfig},

                        erlmcp_otel:record_event(SpanCtx, <<"mtls.enabled">>, #{}),
                        erlmcp_metrics:record("cli.auth.mtls_enabled", 1),
                        {reply, ok, NewState};
                    {error, Reason} ->
                        erlmcp_otel:record_error(SpanCtx, {mtls_cert_failed, Reason}),
                        {reply, {error, Reason}, State}
                end;
            {error, Reason} ->
                erlmcp_otel:record_error(SpanCtx, {mtls_config_invalid, Reason}),
                {reply, {error, Reason}, State}
        end
    catch
        Error:Reason ->
            erlmcp_otel:record_error(SpanCtx, {mtls_enable_error, Error, Reason}),
            {reply, {error, Reason}, State}
    end;
handle_call(disable_mtls, _From, State) ->
    %% Create OTEL span for MTLS disablement
    SpanCtx = erlmcp_otel:inject_span("cli.auth.disable_mtls", #{}, undefined),

    %% Clear MTLS configuration
    NewConfig = maps:remove(<<"mtls_cert_file">>, State#auth_state.mtls_config),
    NewConfig = maps:remove(<<"mtls_key_file">>, NewConfig),
    NewConfig = maps:remove(<<"mtls_ca_file">>, NewConfig),
    NewConfig = maps:put(<<"enable_mtls">>, false, NewConfig),

    NewState = State#auth_state{mtls_config = NewConfig},

    erlmcp_otel:record_event(SpanCtx, <<"mtls.disabled">>, #{}),
    erlmcp_metrics:record("cli.auth.mtls_disabled", 1),

    {reply, ok, NewState};
handle_call({set_jwt_provider, Provider, Config}, _From, State) ->
    %% Create OTEL span for JWT provider setup
    SpanCtx =
        erlmcp_otel:inject_span("cli.auth.set_jwt_provider",
                                #{<<"provider">> => Provider},
                                undefined),

    try
        case validate_jwt_provider(Provider, Config) of
            ok ->
                %% Update auth providers
                NewProviders = maps:put(Provider, Config, State#auth_state.auth_providers),
                NewState = State#auth_state{auth_providers = NewProviders},

                erlmcp_otel:record_event(SpanCtx, <<"jwt_provider.set">>, #{}),
                erlmcp_metrics:record("cli.auth.jwt_provider_set", 1),
                {reply, ok, NewState};
            {error, Reason} ->
                erlmcp_otel:record_error(SpanCtx, {jwt_provider_invalid, Reason}),
                {reply, {error, Reason}, State}
        end
    catch
        Error:Reason ->
            erlmcp_otel:record_error(SpanCtx, {jwt_provider_error, Error, Reason}),
            {reply, {error, Reason}, State}
    end;
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

%% @doc Handle asynchronous casts
-spec handle_cast(term(), #auth_state{}) -> {noreply, #auth_state{}}.
handle_cast(clear_expired, State) ->
    %% Create OTEL span for session cleanup
    SpanCtx = erlmcp_otel:inject_span("cli.auth.clear_expired", #{}, undefined),

    Now = erlang:system_time(millisecond),
    CleanupThreshold = State#auth_state.session_timeout,

    %% Filter out expired sessions
    ActiveSessions =
        maps:filter(fun(_SessionId, Session) ->
                       Now - Session#session.last_activity < CleanupThreshold
                    end,
                    State#auth_state.sessions),

    RemovedCount = maps:size(State#auth_state.sessions) - maps:size(ActiveSessions),

    %% Invalidate tokens for removed sessions
    lists:foreach(fun({_SessionId, Session}) -> invalidate_jwt_token(Session#session.token, State)
                  end,
                  maps:to_list(maps:to_list(State#auth_state.sessions)
                               -- maps:to_list(ActiveSessions))),

    erlmcp_otel:record_event(SpanCtx,
                             <<"sessions.cleaned">>,
                             #{<<"removed_count">> => RemovedCount}),
    erlmcp_metrics:record("cli.auth.sessions_removed", RemovedCount),

    {noreply, State#auth_state{sessions = ActiveSessions}};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Handle messages
-spec handle_info(term(), #auth_state{}) -> {noreply, #auth_state{}}.
handle_info(cleanup_sessions, State) ->
    %% Trigger cleanup of expired sessions
    self() ! clear_expired,

    %% Schedule next cleanup
    erlang:send_after(300000, self(), cleanup_sessions),
    {noreply, State};
handle_info({session_timeout, SessionId}, State) ->
    %% Session timeout occurred
    case maps:find(SessionId, State#auth_state.sessions) of
        {ok, Session} ->
            %% Logout session
            NewSessions = maps:remove(SessionId, State#auth_state.sessions),
            NewState = State#auth_state{sessions = NewSessions},

            %% Invalidate token
            invalidate_jwt_token(Session#session.token, NewState),

            erlmcp_metrics:record("cli.auth.session_timeout", 1),
            {noreply, NewState};
        error ->
            {noreply, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

%% @doc Terminate the server
-spec terminate(term(), #auth_state{}) -> ok.
terminate(_Reason, State) ->
    %% Create OTEL span for authentication manager termination
    erlmcp_otel:with_span("cli.auth.terminate",
                          #{<<"active_sessions">> => maps:size(State#auth_state.sessions)},
                          fun() ->
                             %% Clear all sessions
                             lists:foreach(fun({_SessionId, Session}) ->
                                              invalidate_jwt_token(Session#session.token, State)
                                           end,
                                           maps:to_list(State#auth_state.sessions)),

                             %% Invalidate all cached tokens
                             lists:foreach(fun({_Token, _Claims}) -> ok end,
                                           maps:to_list(State#auth_state.jwt_tokens)),

                             %% Record final metrics
                             erlmcp_metrics:record("cli.auth.terminated", 1),

                             ok
                          end).

%% @doc Handle code changes
-spec code_change(term(), #auth_state{}, term()) -> {ok, #auth_state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Load configuration
-spec load_config() -> map().
load_config() ->
    %% Start with environment variables
    EnvConfig = load_env_config(),

    %% Merge with file configuration
    FileConfig = load_file_config(),

    %% Final merge
    maps:merge(EnvConfig, FileConfig).

%% @doc Load environment configuration
-spec load_env_config() -> map().
load_env_config() ->
    %% Environment variable mappings
    EnvMappings =
        #{<<"ERLMCP_AUTH_SECURITY_LEVEL">> => <<"security_level">>,
          <<"ERLMCP_AUTH_SESSION_TIMEOUT">> => <<"session_timeout">>,
          <<"ERLMCP_AUTH_TOKEN_CACHE_TTL">> => <<"token_cache_ttl">>,
          <<"ERLMCP_AUTH_ENABLE_MTLS">> => <<"enable_mtls">>,
          <<"ERLMCP_AUTH_JWT_PROVIDER">> => <<"jwt_provider">>},

    lists:foldl(fun({EnvVar, ConfigKey}, Acc) ->
                   case os:getenv(binary_to_list(EnvVar)) of
                       false ->
                           Acc;
                       Value ->
                           set_nested_config(ConfigKey, Value, Acc)
                   end
                end,
                #{},
                EnvMappings).

%% @doc Load file configuration
-spec load_file_config() -> map().
load_file_config() ->
    try
        ConfigFile =
            filename:join(
                os:getenv("HOME", "."), ".erlmcp_auth.json"),
        case file:read_file(ConfigFile) of
            {ok, Content} ->
                erlmcp_json_native:decode(Content);
            {error, enoent} ->
                #{};
            {error, Reason} ->
                lager:warning("Failed to read auth config file: ~p", [Reason]),
                #{}
        end
    catch
        Error:Reason ->
            lager:warning("Failed to parse auth config file: ~p:~p", [Error, Reason]),
            #{}
    end.

%% @doc Load authentication providers
-spec load_auth_providers(map()) -> map().
load_auth_providers(Config) ->
    Providers = #{},

    %% Load internal provider
    InternalConfig =
        #{<<"type">> => internal,
          <<"algorithm">> => maps:get(<<"jwt_algorithm">>, Config, RS256),
          <<"secret">> => get_jwt_secret(),
          <<"issuer">> => <<"erlmcp_cli">>},
    maps:put(<<"internal">>, InternalConfig, Providers).

%% @doc Determine authentication method
-spec determine_auth_method(#auth_state{}, map()) -> atom().
determine_auth_method(State, Options) ->
    case maps:get(<<"force_mtls">>, Options, false) of
        true when State#auth_state.security_level == high ->
            mtls;
        true ->
            jwt;
        false ->
            case maps:get(<<"mtls_enabled">>, State#auth_state.mtls_config, false) of
                true when State#auth_state.security_level == high ->
                    mtls;
                _ ->
                    jwt
            end
    end.

%% @doc Perform authentication
-spec perform_authentication(atom(), binary(), binary(), #auth_state{}) ->
                                {ok, binary(), binary()} | {error, term()}.
perform_authentication(mtls, Username, Password, State) ->
    perform_mtls_authentication(Username, Password, State);
perform_authentication(jwt, Username, Password, State) ->
    perform_jwt_authentication(Username, Password, State).

%% @doc Perform JWT authentication
-spec perform_jwt_authentication(binary(), binary(), #auth_state{}) ->
                                    {ok, binary(), binary()} | {error, term()}.
perform_jwt_authentication(Username, Password, State) ->
    try
        %% Get JWT provider
        case maps:get(<<"internal">>, State#auth_state.auth_providers) of
            ProviderConfig ->
                %% Validate credentials against core authentication
                case erlmcp_auth:validate_credentials(Username, Password) of
                    {ok, Claims} ->
                        %% Generate JWT token
                        Token = generate_jwt_token(Claims, ProviderConfig),

                        %% Cache token
                        NewTokens = maps:put(Token, Claims, State#auth_state.jwt_tokens),

                        {ok, SessionId = generate_session_id(), Token};
                    {error, Reason} ->
                        {error, Reason}
                end
        end
    catch
        Error:Reason ->
            {error, {jwt_error, Error, Reason}}
    end.

%% @doc Perform MTLS authentication
-spec perform_mtls_authentication(binary(), binary(), #auth_state{}) ->
                                     {ok, binary(), binary()} | {error, term()}.
perform_mtls_authentication( Username , Password , State ) -> try case State #auth_state .

mtls_config of Config when is_map( Config ) -> case extract_mtls_certificates( Config ) of { ok , CertData } -> case validate_certificate_chain( CertData , Config ) of ok -> Authenticate via certificate and optional password case erlmcp_auth : validate_certificate( CertData , Username , Password ) of { ok , Claims } -> Token = generate_jwt_token( Claims , Config ) , { ok , SessionId = generate_session_id( ) , Token } ; { error , Reason } -> { error , Reason } end ; { error , Reason } -> { error , { certificate_validation_failed , Reason } } end ; { error , Reason } -> { error , { certificate_extraction_failed , Reason } } end ; _ -> { error , mtls_not_enabled } end catch Error : Reason -> { error , { mtls_error , Error , Reason } } end .

        %% Validate MTLS configuration

                %% Extract certificates from the connection

                        %% Validate certificate chain

%% @doc Validate token internally
-spec validate_token_internal(binary(), #auth_state{}) -> {ok, map()} | {error, term()}.
validate_token_internal(Token, State) ->
    try
        %% Check cache first
        case maps:find(Token, State#auth_state.jwt_tokens) of
            {ok, Claims} ->
                %% Check expiration
                case is_token_expired(Claims) of
                    true ->
                        %% Remove from cache
                        NewTokens = maps:remove(Token, State#auth_state.jwt_tokens),
                        throw({token_expired, NewTokens});
                    false ->
                        {ok, Claims}
                end;
            error ->
                %% Validate against external provider
                case validate_jwt_token(Token, State) of
                    {ok, Claims} ->
                        %% Cache token
                        NewTokens = maps:put(Token, Claims, State#auth_state.jwt_tokens),
                        {ok, Claims};
                    {error, Reason} ->
                        {error, Reason}
                end
        end
    catch
        {token_expired, NewTokens} ->
            {error, expired};
        Error:Reason ->
            {error, {validation_error, Error, Reason}}
    end.

%% @doc Create session record
-spec create_session(binary(), binary(), map(), #auth_state{}) -> #session{}.
create_session(User, Token, Options, State) ->
    #session{id = generate_session_id(),
             user = User,
             token = Token,
             mtls_cert = maps:get(<<"mtls_cert">>, Options, undefined),
             created = erlang:system_time(millisecond),
             last_activity = erlang:system_time(millisecond),
             expires = erlang:system_time(millisecond) + State#auth_state.session_timeout,
             permissions = maps:get(<<"permissions">>, Options, []),
             metadata = maps:get(<<"metadata">>, Options, #{})}.

%% @doc Format session information
-spec format_session_info(#session{}) -> map().
format_session_info(Session) ->
    #{<<"id">> => Session#session.id,
      <<"user">> => Session#session.user,
      <<"created">> => Session#session.created,
      <<"last_activity">> => Session#session.last_activity,
      <<"expires">> => Session#session.expires,
      <<"permissions">> => Session#session.permissions,
      <<"metadata">> => Session#session.metadata}.

%% @doc Generate JWT token
-spec generate_jwt_token(map(), map()) -> binary().
generate_jwt_token(Claims, ProviderConfig) ->
    try
        %% Add standard claims
        EnhancedClaims =
            Claims#{<<"iat">> => erlang:system_time(second),
                    <<"exp">> => erlang:system_time(second) + 3600, % 1 hour expiration
                    <<"iss">> => maps:get(<<"issuer">>, ProviderConfig, <<"erlmcp_cli">>),
                    <<"aud">> => <<"erlmcp">>},

        %% Generate token (simplified for implementation)
        Header =
            #{<<"alg">> => atom_to_binary(maps:get(<<"algorithm">>, ProviderConfig, RS256)),
              <<"typ">> => <<"JWT">>},

        Payload = erlmcp_json_native:encode(EnhancedClaims),
        HeaderEncoded = base64url_encode(erlmcp_json_native:encode(Header)),
        PayloadEncoded = base64url_encode(Payload),
        Signature = <<"dummy_signature">>,

        <<HeaderEncoded/binary, ".", PayloadEncoded/binary, ".", Signature/binary>>
    catch
        Error:Reason ->
            erlang:exit({jwt_generation_failed, Error, Reason})
    end.

%% @base64url encode function
-spec base64url_encode(binary()) -> binary().
base64url_encode(Data) ->
    Base64 = base64:encode(Data),
    string:replace(
        string:replace(Base64, "+", "-"), "/", "_", {return, binary}).

%% @doc Invalidate JWT token
-spec invalidate_jwt_token(binary(), #auth_state{}) -> ok.
invalidate_jwt_token(Token, _State) ->
    %% Add to blacklist or mark as invalid
    %% This is simplified - in production, you'd implement a proper token blacklist
    ok.

%% @doc Validate JWT token
-spec validate_jwt_token(binary(), #auth_state{}) -> {ok, map()} | {error, term()}.
validate_jwt_token(Token, State) ->
    try
        %% Parse token
        Parts = binary:split(Token, <<".">>, [global]),
        case length(Parts) of
            3 ->
                [HeaderEnc, PayloadEnc, _] = Parts,
                Header = erlmcp_json_native:decode(base64url_decode(HeaderEnc)),
                Payload = erlmcp_json_native:decode(base64url_decode(PayloadEnc)),

                %% Validate signature (simplified)
                case verify_token_signature(Token, Header, State) of
                    ok ->
                        {ok, Payload};
                    {error, Reason} ->
                        {error, Reason}
                end;
            _ ->
                {error, invalid_token_format}
        end
    catch
        Error:Reason ->
            {error, {token_parse_error, Error, Reason}}
    end.

%% @doc Base64url decode function
-spec base64url_decode(binary()) -> binary().
base64url_encode(Data) ->
    Padding =
        case byte_size(Data) rem 4 of
            2 ->
                <<"==">>;
            3 ->
                <<"=">>;
            _ ->
                <<>>
        end,
    Base64 = list_to_binary([Data, Padding]),
    binary:replace(
        binary:replace(Base64, <<"-">>, <<"+">>), <<"_">>, <<"/">>),
    base64:decode(Base64).

%% @doc Verify token signature (simplified)
-spec verify_token_signature(binary(), map(), #auth_state{}) -> ok | {error, term()}.
verify_token_signature(_Token, _Header, _State) ->
    %% In production, implement proper signature verification
    ok.

%% @doc Check if token is expired
-spec is_token_expired(map()) -> boolean().
is_token_expired(Claims) ->
    case maps:get(<<"exp">>, Claims, undefined) of
        undefined ->
            false;
        ExpTime ->
            erlang:system_time(second) >= ExpTime
    end.

%% @doc Generate session ID
-spec generate_session_id() -> binary().
generate_session_id() ->
    Id = crypto:strong_rand_bytes(16),
    base64:encode(Id).

%% @doc Get JWT secret
-spec get_jwt_secret() -> binary().
get_jwt_secret() ->
    case os:getenv("ERLMCP_JWT_SECRET") of
        false ->
            %% Generate random secret for development
            crypto:strong_rand_bytes(32);
        Secret ->
            list_to_binary(Secret)
    end.

%% @doc Set nested configuration value
-spec set_nested_config(binary(), term(), map()) -> map().
set_nested_config(Key, Value, Config) ->
    case binary:split(Key, <<".">>, [global]) of
        [Key] ->
            maps:put(Key, Value, Config);
        Parts ->
            set_nested_config_parts(Parts, Value, Config)
    end.

%% @doc Set nested configuration parts
-spec set_nested_config_parts([binary()], term(), map()) -> map().
set_nested_config_parts([Part], Value, Map) ->
    maps:put(Part, Value, Map);
set_nested_config_parts([Part | Rest], Value, Map) ->
    SubMap =
        case maps:get(Part, Map, undefined) of
            undefined ->
                #{};
            Existing when is_map(Existing) ->
                Existing
        end,
    NewSubMap = set_nested_config_parts(Rest, Value, SubMap),
    maps:put(Part, NewSubMap, Map).

%% @doc Validate MTLS configuration
-spec validate_mtls_config(map()) -> ok | {error, term()}.
validate_mtls_config(Config) ->
    %% Check required fields
    case maps:find(<<"mtls_cert_file">>, Config) of
        {ok, CertFile} ->
            case filelib:is_file(CertFile) of
                true ->
                    ok;
                false ->
                    {error, cert_file_not_found}
            end;
        error ->
            {error, missing_cert_file}
    end.

%% @doc Load MTLS certificates
-spec load_mtls_certificates(map()) -> {ok, map()} | {error, term()}.
load_mtls_certificates(Config) ->
    try
        CertFile = maps:get(<<"mtls_cert_file">>, Config),
        KeyFile = maps:get(<<"mtls_key_file">>, Config),
        CaFile = maps:get(<<"mtls_ca_file">>, Config, undefined),

        %% Load certificate
        {ok, CertData} = file:read_file(CertFile),

        %% Load private key
        {ok, KeyData} = file:read_file(KeyFile),

        %% Load CA certificate if provided
        CaData =
            case CaFile of
                undefined ->
                    undefined;
                _ ->
                    {ok, Data} = file:read_file(CaFile),
                    Data
            end,

        {ok,
         #{<<"cert">> => CertData,
           <<"key">> => KeyData,
           <<"ca">> => CaData}}
    catch
        Error:Reason ->
            {error, {certificate_load_failed, Error, Reason}}
    end.

%% @doc Extract MTLS certificates from connection
-spec extract_mtls_certificates(map()) -> {ok, map()} | {error, term()}.
extract_mtls_certificates(Config) ->
    %% This would extract certificates from an active TLS connection
    %% Implementation depends on the specific transport being used
    {error, not_implemented}.

%% @doc Validate certificate chain
-spec validate_certificate_chain(map(), map()) -> ok | {error, term()}.
validate_certificate_chain(CertData, Config) ->
    %% This would validate the certificate chain against trusted CAs
    %% Simplified for implementation
    ok.

%% @doc Validate JWT provider
-spec validate_jwt_provider(binary(), map()) -> ok | {error, term()}.
validate_jwt_provider(Provider, Config) ->
    case Provider of
        <<"internal">> ->
            %% Internal provider validation
            case maps:get(<<"secret">>, Config, undefined) of
                undefined ->
                    {error, missing_secret};
                _ ->
                    ok
            end;
        _ ->
            %% External provider validation
            case maps:get(<<"url">>, Config, undefined) of
                undefined ->
                    {error, missing_provider_url};
                _ ->
                    ok
            end
    end.

%% @doc Initialize metrics
-spec init_metrics() -> map().
init_metrics() ->
    #{"authentications.success" => 0,
      "authentications.failed" => 0,
      "authentications.errors" => 0,
      "tokens.refreshed" => 0,
      "tokens.validated" => 0,
      "tokens.expired" => 0,
      "mtls.enabled" => 0,
      "mtls.disabled" => 0,
      "sessions.active" => 0,
      "sessions.removed" => 0}.
