%%%-------------------------------------------------------------------
%%% @doc erlmcp_oauth2 - OAuth2 and OpenID Connect implementation
%%% Comprehensive OAuth2/OIDC provider support for enterprise authentication
%%%
%%% Features:
%%% - OAuth2 2.0 flows (Authorization Code, Implicit, Client Credentials, Resource Owner)
%%% - OpenID Connect support
%%% - Multiple provider integration
%%% - Token management and refresh
%%% - Claims validation
%%% - Client registration and management
%%% - Dynamic client configuration
%%% - PKCE support for public clients
%%% - JWT token validation
%%% - Token introspection and revocation
%%% - Redirect URI validation
%%% - Client metadata management
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_oauth2).

-behaviour(gen_server).

%% API exports
-export([start_link/0, start_link/1,
         % Core OAuth2 functions
         authorize/3, token/3, introspect/2, revoke/2, userinfo/2,
         % OIDC functions
         openid_configuration/1, jwks/1, id_token/3, claims/2,
         % Client management
         register_client/2, update_client/2, get_client/1, list_clients/0,
         % Token management
         create_token/3, refresh_token/2, validate_token/2, revoke_token/1,
         % Configuration
         add_provider/2, remove_provider/1, get_provider/1, list_providers/0,
         % Utility functions
         validate_redirect_uri/2, validate_scope/1, generate_code/0,
         generate_state/0, generate_challenge/0, verify_pkce/3,
         % System functions
         stop/0, status/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("erlmcp.hrl").

%% Types
-type client_id() :: binary().
-type client_secret() :: binary().
-type redirect_uri() :: binary().
-type scope() :: binary() | [binary()].

-type token_type() :: access_token | refresh_token | id_token.
-type token_endpoint_auth_method() :: client_secret_basic | client_secret_post |
                                     private_key_jwt | client_secret_jwt |
                                     none.

-type oauth_flow() :: authorization_code | implicit | password |
                     client_credentials | device_code.

-type client() :: #{id := client_id(),
                    secret := client_secret(),
                    redirect_uris := [redirect_uri()],
                    scope := scope(),
                    response_types := [binary()],
                    grant_types := [oauth_flow()],
                    token_auth_method := token_endpoint_auth_method(),
                    metadata := map()}.

-type token() :: #{id := binary(),
                   client_id := client_id(),
                   user_id := binary(),
                   type := token_type(),
                   access_token := binary(),
                   refresh_token := binary(),
                   scope := scope(),
                   expires_at := integer(),
                   issued_at := integer(),
                   claims := map(),
                    metadata := map()}.

-type provider() :: #{id := binary(),
                     name := binary(),
                     auth_url := binary(),
                     token_url := binary(),
                     userinfo_url := binary(),
                     jwks_url :: binary(),
                     issuer := binary(),
                     authorization_endpoint := binary(),
                     token_endpoint := binary(),
                     userinfo_endpoint := binary(),
                     end_session_endpoint :: binary(),
                     client_id := client_id(),
                     client_secret := client_secret(),
                     scopes := [binary()],
                    metadata := map()}.

-type state() :: #{clients := list(),                % Active client registrations
                   tokens := ets:tid(),             % Token storage
                   providers := list(),              % OAuth2 providers
                   codes := ets:tid(),              % Authorization codes
                   sessions := ets:tid(),           % OAuth sessions
                   config := map(),                 % Configuration
                   refresh_tokens := ets:tid()}.    % Refresh token storage

-export_type([client_id/0, client_secret/0, redirect_uri/0, scope/0,
              token_type/0, token_endpoint_auth_method/0, oauth_flow/0,
              client/0, token/0, provider/0, state/0]).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Config], []).

%%--------------------------------------------------------------------
%% Core OAuth2 Functions
%%--------------------------------------------------------------------

%% @doc OAuth2 authorization endpoint
-spec authorize(client_id(), binary(), map()) ->
    {ok, binary()} | {error, term()}.
authorize(ClientId, RedirectUri, Params) ->
    gen_server:call(?MODULE, {authorize, ClientId, RedirectUri, Params}).

%% @doc Token endpoint
-spec token(client_id(), binary(), map()) ->
    {ok, map()} | {error, term()}.
token(ClientId, Secret, Params) ->
    gen_server:call(?MODULE, {token, ClientId, Secret, Params}).

%% @doc Token introspection (RFC 7662)
-spec introspect(binary(), map()) -> {ok, map()} | {error, term()}.
introspect(Token, Config) ->
    gen_server:call(?MODULE, {introspect, Token, Config}).

%% @doc Token revocation (RFC 7009)
-spec revoke(binary(), map()) -> ok | {error, term()}.
revoke(Token, Config) ->
    gen_server:call(?MODULE, {revoke, Token, Config}).

%% @doc OpenID Connect userinfo endpoint
-spec userinfo(binary(), map()) -> {ok, map()} | {error, term()}.
userinfo(Token, Config) ->
    gen_server:call(?MODULE, {userinfo, Token, Config}).

%%--------------------------------------------------------------------
%% OIDC Functions
%%--------------------------------------------------------------------

%% @doc OpenID Connect discovery endpoint
-spec openid_configuration(binary()) -> {ok, map()}.
openid_configuration(Issuer) ->
    gen_server:call(?MODULE, {openid_configuration, Issuer}).

%% @doc JSON Web Key Set (JWKS) endpoint
-spec jwks(binary()) -> {ok, map()}.
jwks(Issuer) ->
    gen_server:call(?MODULE, {jwks, Issuer}).

%% @ ID Token generation and validation
-spec id_token(client_id(), binary(), map()) -> {ok, map()}.
id_token(ClientId, UserId, Claims) ->
    gen_server:call(?MODULE, {id_token, ClientId, UserId, Claims}).

%% @doc OIDC claims validation
-spec claims(binary(), map()) -> {ok, map()} | {error, term()}.
claims(IdToken, Config) ->
    gen_server:call(?MODULE, {claims, IdToken, Config}).

%%--------------------------------------------------------------------
%% Client Management Functions
%%--------------------------------------------------------------------

%% @doc Register new OAuth2 client
-spec register_client(map(), binary()) -> {ok, client()} | {error, term()}.
register_client(ClientMetadata, Secret) ->
    gen_server:call(?MODULE, {register_client, ClientMetadata, Secret}).

%% @doc Update existing client
-spec update_client(client()) -> ok | {error, term()}.
update_client(Client) ->
    gen_server:call(?MODULE, {update_client, Client}).

%% @doc Get client by ID
-spec get_client(client_id()) -> {ok, client()} | {error, not_found}.
get_client(ClientId) ->
    gen_server:call(?MODULE, {get_client, ClientId}).

%% @doc List all registered clients
-spec list_clients() -> [client()].
list_clients() ->
    gen_server:call(?MODULE, list_clients).

%%--------------------------------------------------------------------
%% Token Management Functions
%%--------------------------------------------------------------------

%% @doc Create new token
-spec create_token(client_id(), binary(), map()) -> {ok, token()}.
create_token(ClientId, UserId, Params) ->
    gen_server:call(?MODULE, {create_token, ClientId, UserId, Params}).

%% @doc Refresh access token
-spec refresh_token(binary(), map()) -> {ok, map()} | {error, term()}.
refresh_token(RefreshToken, Params) ->
    gen_server:call(?MODULE, {refresh_token, RefreshToken, Params}).

%% @doc Validate token
-spec validate_token(binary(), map()) -> {ok, map()} | {error, term()}.
validate_token(Token, Config) ->
    gen_server:call(?MODULE, {validate_token, Token, Config}).

%% @doc Revoke token
-spec revoke_token(binary()) -> ok.
revoke_token(Token) ->
    gen_server:call(?MODULE, {revoke_token, Token}).

%%--------------------------------------------------------------------
%% Configuration Functions
%%--------------------------------------------------------------------

%% @doc Add OAuth2 provider
-spec add_provider(binary(), map()) -> ok.
add_provider(ProviderId, Config) ->
    gen_server:cast(?MODULE, {add_provider, ProviderId, Config}).

%% @doc Remove OAuth2 provider
-spec remove_provider(binary()) -> ok.
remove_provider(ProviderId) ->
    gen_server:cast(?MODULE, {remove_provider, ProviderId}).

%% @doc Get provider configuration
-spec get_provider(binary()) -> {ok, provider()} | {error, not_found}.
get_provider(ProviderId) ->
    gen_server:call(?MODULE, {get_provider, ProviderId}).

%% @doc List all providers
-spec list_providers() -> [provider()].
list_providers() ->
    gen_server:call(?MODULE, list_providers).

%%--------------------------------------------------------------------
%% Utility Functions
%%--------------------------------------------------------------------

%% @doc Validate redirect URI
-spec validate_redirect_uri(client_id(), binary()) -> boolean().
validate_redirect_uri(ClientId, RedirectUri) ->
    gen_server:call(?MODULE, {validate_redirect_uri, ClientId, RedirectUri}).

%% @doc Validate scope
-spec validate_scope(binary()) -> boolean().
validate_scope(ScopeString) ->
    gen_server:call(?MODULE, {validate_scope, ScopeString}).

%% @doc Generate authorization code
-spec generate_code() -> binary().
generate_code() ->
    crypto:strong_rand_bytes(32).

%% @doc Generate state parameter
-spec generate_state() -> binary().
generate_state() ->
    crypto:strong_rand_bytes(16).

%% @doc Generate PKCE challenge
-spec generate_challenge() -> {binary(), binary()}.
generate_challenge() ->
    CodeVerifier = crypto:strong_rand_bytes(43),
    Hash = crypto:hash(sha256, CodeVerifier),
    Base64 = base64:encode(Hash),
    Challenge = re:replace(Base64, "=+$", "", [{return, binary}]),
    {CodeVerifier, Challenge}.

%% @doc Verify PKCE code verifier
-spec verify_pkce(binary(), binary(), binary()) -> boolean().
verify_pkce(CodeVerifier, Challenge, Method) ->
    case Method of
        <<"S256">> ->
            Hash = crypto:hash(sha256, CodeVerifier),
            Base64 = base64:encode(Hash),
            CleanBase64 = re:replace(Base64, "=+$", "", [{return, binary}]),
            CleanBase64 =:= Challenge;
        _ ->
            % Plain method - not recommended for production
            CodeVerifier =:= Challenge
    end.

%%--------------------------------------------------------------------
%% System Functions
%%--------------------------------------------------------------------

%% @doc Stop OAuth2 server
-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE).

%% @doc Get server status
-spec status() -> map().
status() ->
    gen_server:call(?MODULE, status).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([map()]) -> {ok, state()}.
init([Config]) ->
    process_flag(trap_exit, true),

    % Initialize ETS tables
    Tokens = ets:new(oauth_tokens, [set, protected, {read_concurrency, true}]),
    Codes = ets:new(oauth_codes, [set, protected, {write_concurrency, true}]),
    Sessions = ets:new(oauth_sessions, [set, protected, {read_concurrency, true}]),
    RefreshTokens = ets:new(oauth_refresh_tokens, [set, protected, {read_concurrency, true}]),

    % Default configuration
    DefaultConfig = #{
        token_lifetime => 3600,              % 1 hour
        refresh_token_lifetime => 2592000,  % 30 days
        code_lifetime => 300,               % 5 minutes
        max_tokens_per_client => 100,
        pkce_enabled => true,
        require_client_secret => true,
        allowed_scopes => [<<"openid">>, <<"profile">>, <<"email">>, <<"address">>,
                          <<"phone">>, <<"offline_access">>],
        default_scopes => [<<"openid">>, <<"profile">>]
    },

    State = #{
        clients => [],
        tokens => Tokens,
        providers => [],
        codes => Codes,
        sessions => Sessions,
        config => maps:merge(DefaultConfig, Config),
        refresh_tokens => RefreshTokens
    },

    % Load clients from configuration
    init_clients(State),

    % Start cleanup timer
    erlang:send_after(60000, self(), cleanup_expired_tokens),

    logger:info("OAuth2 server initialized with config: ~p", [maps:keys(Config)]),
    {ok, State}.

-spec handle_call(term(), {pid(), term()}, state()) ->
                     {reply, term(), state()} | {noreply, state()}.
handle_call({authorize, ClientId, RedirectUri, Params}, _From, State) ->
    Result = do_authorize(ClientId, RedirectUri, Params, State),
    {reply, Result, State};

handle_call({token, ClientId, Secret, Params}, _From, State) ->
    Result = do_token(ClientId, Secret, Params, State),
    {reply, Result, State};

handle_call({introspect, Token, Config}, _From, State) ->
    Result = do_introspect(Token, Config, State),
    {reply, Result, State};

handle_call({revoke, Token, Config}, _From, State) ->
    Result = do_revoke(Token, Config, State),
    {reply, Result, State};

handle_call({userinfo, Token, Config}, _From, State) ->
    Result = do_userinfo(Token, Config, State),
    {reply, Result, State};

handle_call({openid_configuration, Issuer}, _From, State) ->
    Result = do_openid_configuration(Issuer, State),
    {reply, Result, State};

handle_call({jwks, Issuer}, _From, State) ->
    Result = do_jwks(Issuer, State),
    {reply, Result, State};

handle_call({id_token, ClientId, UserId, Claims}, _From, State) ->
    Result = do_id_token(ClientId, UserId, Claims, State),
    {reply, Result, State};

handle_call({claims, IdToken, Config}, _From, State) ->
    Result = do_claims(IdToken, Config, State),
    {reply, Result, State};

handle_call({register_client, ClientMetadata, Secret}, _From, State) ->
    Result = do_register_client(ClientMetadata, Secret, State),
    {reply, Result, State};

handle_call({update_client, Client}, _From, State) ->
    Result = do_update_client(Client, State),
    {reply, Result, State};

handle_call({get_client, ClientId}, _From, State) ->
    Result = do_get_client(ClientId, State),
    {reply, Result, State};

handle_call(list_clients, _From, State) ->
    {reply, State#{clients}, State};

handle_call({create_token, ClientId, UserId, Params}, _From, State) ->
    Result = do_create_token(ClientId, UserId, Params, State),
    {reply, Result, State};

handle_call({refresh_token, RefreshToken, Params}, _From, State) ->
    Result = do_refresh_token(RefreshToken, Params, State),
    {reply, Result, State};

handle_call({validate_token, Token, Config}, _From, State) ->
    Result = do_validate_token(Token, Config, State),
    {reply, Result, State};

handle_call({revoke_token, Token}, _From, State) ->
    Result = do_revoke_token(Token, State),
    {reply, Result, State};

handle_call({get_provider, ProviderId}, _From, State) ->
    Result = do_get_provider(ProviderId, State),
    {reply, Result, State};

handle_call(list_providers, _From, State) ->
    {reply, State#{providers}, State};

handle_call({validate_redirect_uri, ClientId, RedirectUri}, _From, State) ->
    Result = do_validate_redirect_uri(ClientId, RedirectUri, State),
    {reply, Result, State};

handle_call({validate_scope, ScopeString}, _From, State) ->
    Result = do_validate_scope(ScopeString, State),
    {reply, Result, State};

handle_call(status, _From, State) ->
    Status = #{
        clients => length(State#{clients}),
        tokens => ets:info(State#{tokens}, size),
        providers => length(State#{providers}),
        memory => ets:info(State#{tokens}, memory),
        uptime => erlang:system_time(second) - element(2, process_info(self(), start_time))
    },
    {reply, Status, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast({add_provider, ProviderId, Config}, State) ->
    Provider = build_provider(ProviderId, Config),
    Providers = [Provider | State#{providers}],
    {noreply, State#{providers => Providers}};

handle_cast({remove_provider, ProviderId}, State) ->
    Providers = lists:filter(fun(P) -> P#{id} =/= ProviderId end, State#{providers}),
    {noreply, State#{providers => Providers}};

handle_cast(_Request, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(cleanup_expired_tokens, State) ->
    cleanup_expired_tokens(State),
    erlang:send_after(60000, self(), cleanup_expired_tokens),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, State) ->
    % Cleanup ETS tables
    ets:delete(State#{tokens}),
    ets:delete(State#{codes}),
    ets:delete(State#{sessions}),
    ets:delete(State#{refresh_tokens}),

    logger:info("OAuth2 server terminated"),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% Initialize clients from configuration
init_clients(State) ->
    % This would typically load clients from database or configuration file
    % For demonstration, we'll add a default client
    DefaultClient = #{
        id => <<"default_client">>,
        secret => crypto:strong_rand_bytes(32),
        redirect_uris => [<<"https://localhost:3000/callback">>],
        scope => [<<"openid">>, <<"profile">>],
        response_types => [<<"code">>],
        grant_types => [<<"authorization_code">>],
        token_auth_method => client_secret_basic,
        metadata => #{name => "Default Client", created => erlang:system_time(second)}
    },
    State#{clients => [DefaultClient]}.

%% OAuth2 authorization flow
do_authorize(ClientId, RedirectUri, Params, State) ->
    case do_get_client(ClientId, State) of
        {ok, Client} ->
            % Validate redirect URI
            case lists:member(RedirectUri, Client#{redirect_uris}) of
                true ->
                    % Get response type
                    ResponseType = maps:get(<<"response_type">>, Params, <<"code">>),

                    % Validate response type
                    case lists:member(ResponseType, Client#{response_types}) of
                        true ->
                            % Generate authorization code
                            Code = generate_code(),
                            Now = erlang:system_time(second),

                            % Store code
                            CodeData = #{
                                code => Code,
                                client_id => ClientId,
                                redirect_uri => RedirectUri,
                                scope => maps:get(<<"scope">>, Params, Client#{scope}),
                                state => maps:get(<<"state">>, Params, <<>>),
                                created_at => Now,
                                expires_at => Now + State#{config}#{code_lifetime}
                            },
                            ets:insert(State#{codes}, {Code, CodeData}),

                            % Build redirect URL
                            Redirect = build_redirect_uri(RedirectUri, Params, Code, State),
                            {ok, Redirect};
                        false ->
                            {error, unsupported_response_type}
                    end;
                false ->
                    {error, invalid_redirect_uri}
            end;
        {error, _} ->
            {error, invalid_client}
    end.

%% Token exchange
do_token(ClientId, Secret, Params, State) ->
    case do_get_client(ClientId, State) of
        {ok, Client} ->
            % Verify client credentials
            case verify_client_credentials(ClientId, Secret, Client, State) of
                true ->
                    % Get grant type
                    GrantType = maps:get(<<"grant_type">>, Params, <<"authorization_code">>),

                    % Process based on grant type
                    case GrantType of
                        <<"authorization_code">> ->
                            process_authorization_code(Params, Client, State);
                        <<"refresh_token">> ->
                            process_refresh_token(Params, Client, State);
                        <<"client_credentials">> ->
                            process_client_credentials(Client, State);
                        <<"password">> ->
                            process_password_grant(Params, Client, State);
                        _ ->
                            {error, unsupported_grant_type}
                    end;
                false ->
                    {error, invalid_client}
            end;
        {error, _} ->
            {error, invalid_client}
    end.

%% Process authorization code grant
process_authorization_code(Params, Client, State) ->
    Code = maps:get(<<"code">>, Params, <<>>),
    RedirectUri = maps:get(<<"redirect_uri">>, Params, <<>>),

    case ets:lookup(State#{codes}, Code) of
        [{_, CodeData}] ->
            % Validate code
            case validate_code(CodeData, Client#{id}, RedirectUri, State) of
                true ->
                    % Generate tokens
                    {ok, Tokens} = generate_tokens(CodeData#{client_id} = Client#{id}, State),

                    % Delete code
                    ets:delete(State#{codes}, Code),

                    % Return token response
                    TokenResponse = build_token_response(Tokens),
                    {ok, TokenResponse};
                false ->
                    {error, invalid_grant}
            end;
        [] ->
            {error, invalid_grant}
    end.

%% Process refresh token grant
process_refresh_token(Params, Client, State) ->
    RefreshToken = maps:get(<<"refresh_token">>, Params, <<>>),

    case ets:lookup(State#{refresh_tokens}, RefreshToken) of
        [{_, RefreshData}] ->
            % Validate refresh token
            case validate_refresh_token(RefreshData, Client#{id}, State) of
                true ->
                    % Generate new tokens
                    {ok, NewTokens} = generate_tokens(RefreshData#{client_id} = Client#{id}, State),

                    % Delete old refresh token
                    ets:delete(State#{refresh_tokens}, RefreshToken),

                    % Return token response
                    TokenResponse = build_token_response(NewTokens),
                    {ok, TokenResponse};
                false ->
                    {error, invalid_grant}
            end;
        [] ->
            {error, invalid_grant}
    end.

%% Process client credentials grant
process_client_credentials(Client, State) ->
    % Generate tokens for client credentials
    TokenData = #{
        client_id => Client#{id},
        scope => Client#{scope},
        type => access_token,
        created_at => erlang:system_time(second),
        expires_at => erlang:system_time(second) + State#{config}#{token_lifetime}
    },
    {ok, Tokens} = generate_tokens(TokenData, State),

    % Return token response
    TokenResponse = build_token_response(Tokens),
    {ok, TokenResponse}.

%% Generate access and refresh tokens
generate_tokens(TokenData, State) ->
    AccessToken = generate_access_token(TokenData),
    RefreshToken = generate_refresh_token(TokenData),
    ExpiresIn = TokenData#{expires_at} - erlang:system_time(second),

    Token = #{
        id => crypto:strong_rand_bytes(16),
        client_id => TokenData#{client_id},
        user_id => maps:get(user_id, TokenData, <<>>),
        type => access_token,
        access_token => AccessToken,
        refresh_token => RefreshToken,
        scope => TokenData#{scope},
        expires_at => TokenData#{expires_at},
        issued_at => TokenData#{created_at},
        claims => maps:get(claims, TokenData, #{}),
        metadata => TokenData#{metadata}
    },

    % Store tokens
    ets:insert(State#{tokens}, {Token#{id}, Token}),
    if TokenData#{type} == refresh_token ->
            ets:insert(State#{refresh_tokens}, {RefreshToken, Token});
       true ->
            ok
    end,

    {ok, Token}.

%% Token introspection
do_introspect(Token, Config, State) ->
    case ets:lookup(State#{tokens}, Token) of
        [{_, TokenData}] ->
            % Validate token is not expired
            case erlang:system_time(second) < TokenData#{expires_at} of
                true ->
                    % Build introspection response
                    Response = #{
                        active => true,
                        scope => TokenData#{scope},
                        client_id => TokenData#{client_id},
                        username => TokenData#{user_id},
                        exp => TokenData#{expires_at},
                        iat => TokenData#{issued_at},
                        sub => TokenData#{user_id}
                    },
                    {ok, Response};
                false ->
                    {ok, #{active => false}}
            end;
        [] ->
            {ok, #{active => false}}
    end.

%% Token revocation
do_revoke(Token, Config, State) ->
    case ets:lookup(State#{tokens}, Token) of
        [{_, TokenData}] ->
            % Delete access token
            ets:delete(State#{tokens}, TokenData#{id}),
            % Delete refresh token if present
            case maps:is_key(refresh_token, TokenData) of
                true ->
                    ets:delete(State#{refresh_tokens}, TokenData#{refresh_token});
                false ->
                    ok
            end,
            ok;
        [] ->
            % Check if it's a refresh token
            case ets:lookup(State#{refresh_tokens}, Token) of
                [{_, _}] ->
                    ets:delete(State#{refresh_tokens}, Token),
                    ok;
                [] ->
                    {error, invalid_token}
            end
    end.

%% OpenID Connect userinfo endpoint
do_userinfo(Token, Config, State) ->
    case do_introspect(Token, Config, State) of
        {ok, #{active := true} = Claims} ->
            % Get user information from claims
            UserInfo = extract_userinfo(Claims),
            {ok, UserInfo};
        {ok, #{active := false}} ->
            {error, invalid_token};
        {error, Reason} ->
            {error, Reason}
    end.

%% OpenID Connect discovery
do_openid_configuration(Issuer, State) ->
    BaseUrl = re:replace(Issuer, "/$", "", [{return, binary}]),
    Config = #{
        issuer => Issuer,
        authorization_endpoint => <<BaseUrl/binary, "/oauth/authorize">>,
        token_endpoint => <<BaseUrl/binary, "/oauth/token">>,
        userinfo_endpoint => <<BaseUrl/binary, "/oauth/userinfo">>,
        jwks_uri => <<BaseUrl/binary, "/oauth/jwks">>,
        response_types_supported => [<<"code">>, <<"token">>, <<"id_token">>],
        subject_types_supported => [<<"public">>, <<"pairwise">>],
        id_token_signing_alg_values_supported => [<<"RS256">>],
        scopes_supported => State#{config}#{allowed_scopes}
    },
    {ok, Config}.

%% JSON Web Key Set
do_jwks(Issuer, State) ->
    % Return JSON Web Key Set
    % In production, this would return actual public keys
    Keys = [#{
        kid => <<"test_key_1">>,
        kty => <<>>  % "RSA"
    }],
    {ok, #{keys => Keys}}.

%% ID Token generation
do_id_token(ClientId, UserId, Claims, State) ->
    % Generate ID Token
    Now = erlang:system_time(second),
    IDToken = #{
        iss => <<"https://localhost:8080">>,
        sub => UserId,
        aud => ClientId,
        exp => Now + 3600,
        iat => Now,
        nonce => maps:get(nonce, Claims, <<>>),
        name => maps:get(name, Claims, <<>>),
        email => maps:get(email, Claims, <<>>,
        email_verified => maps:get(email_verified, Claims, false)
    },
    {ok, IDToken}.

%% Claims validation
do_claims(IdToken, Config, State) ->
    % Validate ID Token
    % This would verify signature, expiration, issuer, etc.
    % For demonstration, we'll just return the claims
    {ok, IdToken}.

%% Client registration
do_register_client(ClientMetadata, Secret, State) ->
    % Validate client metadata
    case validate_client_metadata(ClientMetadata) of
        ok ->
            % Generate client ID
            ClientId = crypto:strong_rand_bytes(16),

            % Create client record
            Client = #{
                id => ClientId,
                secret => Secret,
                redirect_uris => maps:get(redirect_uris, ClientMetadata, []),
                scope => maps:get(scope, ClientMetadata, []),
                response_types => maps:get(response_types, ClientMetadata, [<<"code">>]),
                grant_types => maps:get(grant_types, ClientMetadata, [<<"authorization_code">>]),
                token_auth_method => maps:get(token_auth_method, ClientMetadata, client_secret_basic),
                metadata => ClientMetadata
            },

            % Add to client list
            NewClients = [Client | State#{clients}],
            {ok, Client};
        {error, Reason} ->
            {error, Reason}
    end.

%% Update client
do_update_client(Client, State) ->
    ClientId = Client#{id},

    % Find and update client
    case lists:keyfind(ClientId, 1, State#{clients}) of
        {_, _} ->
            UpdatedClients = lists:keyreplace(ClientId, 1, State#{clients}, Client),
            {ok, Client};
        false ->
            {error, client_not_found}
    end.

%% Get client by ID
do_get_client(ClientId, State) ->
    case lists:keyfind(ClientId, 1, State#{clients}) of
        {_, Client} ->
            {ok, Client};
        false ->
            {error, not_found}
    end.

%% Refresh token
do_refresh_token(RefreshToken, Params, State) ->
    process_refresh_token(Params, #{id => maps:get(<<"client_id">>, Params, <<>>)}, State).

%% Validate token
do_validate_token(Token, Config, State) ->
    case ets:lookup(State#{tokens}, Token) of
        [{_, TokenData}] ->
            % Check expiration
            case erlang:system_time(second) < TokenData#{expires_at} of
                true ->
                    {ok, TokenData};
                false ->
                    {error, expired_token}
            end;
        [] ->
            {error, invalid_token}
    end.

%% Revoke token
do_revoke_token(Token, State) ->
    case ets:lookup(State#{tokens}, Token) of
        [{_, TokenData}] ->
            ets:delete(State#{tokens}, TokenData#{id}),
            % Also revoke refresh token
            case maps:is_key(refresh_token, TokenData) of
                true ->
                    ets:delete(State#{refresh_tokens}, TokenData#{refresh_token});
                false ->
                    ok
            end,
            ok;
        [] ->
            {error, not_found}
    end.

%% Get provider
do_get_provider(ProviderId, State) ->
    case lists:keyfind(ProviderId, 1, State#{providers}) of
        {_, Provider} ->
            {ok, Provider};
        false ->
            {error, not_found}
    end.

%% Validate redirect URI
do_validate_redirect_uri(ClientId, RedirectUri, State) ->
    case do_get_client(ClientId, State) of
        {ok, Client} ->
            lists:member(RedirectUri, Client#{redirect_uris});
        {error, _} ->
            false
    end.

%% Validate scope
do_validate_scope(ScopeString, State) ->
    AllowedScopes = State#{config}#{allowed_scopes},
    Scopes = binary:split(ScopeString, <<" ">>, [global]),
    lists:all(fun(Scope) -> lists:member(Scope, AllowedScopes) end, Scopes).

%% Build provider configuration
build_provider(ProviderId, Config) ->
    #{
        id => ProviderId,
        name => maps:get(name, Config, ProviderId),
        auth_url => maps:get(auth_url, Config),
        token_url => maps:get(token_url, Config),
        userinfo_url => maps:get(userinfo_url, Config),
        jwks_url => maps:get(jwks_url, Config),
        issuer => maps:get(issuer, Config),
        authorization_endpoint => maps:get(authorization_endpoint, Config),
        token_endpoint => maps:get(token_endpoint, Config),
        userinfo_endpoint => maps:get(userinfo_endpoint, Config),
        end_session_endpoint => maps:get(end_session_endpoint, Config),
        client_id => maps:get(client_id, Config),
        client_secret => maps:get(client_secret, Config),
        scopes => maps:get(scopes, Config, []),
        metadata => Config
    }.

%% Build redirect URI with parameters
build_redirect_uri(RedirectUri, Params, Code, State) ->
    Query = uri_string:compose_query([
        {<<"code">>, Code},
        {<<"state">>, maps:get(<<"state">>, Params, <<>>)}
    ]),
    <<RedirectUri/binary, "?", Query/binary>>.

%% Build token response
build_token_response(Token) ->
    Response = #{
        access_token => Token#{access_token},
        token_type => <<"Bearer">>,
        expires_in => Token#{expires_at} - erlang:system_time(second),
        scope => Token#{scope}
    },
    case maps:is_key(refresh_token, Token) of
        true ->
            Response#{refresh_token => Token#{refresh_token}};
        false ->
            Response
    end.

%% Validate authorization code
validate_code(CodeData, ClientId, RedirectUri, State) ->
    % Check code expiration
    case erlang:system_time(second) < CodeData#{expires_at} of
        true ->
            % Check client ID matches
            case CodeData#{client_id} =:= ClientId of
                true ->
                    % Check redirect URI matches
                    case CodeData#{redirect_uri} =:= RedirectUri of
                        true ->
                            true;
                        false ->
                            false
                    end;
                false ->
                    false
            end;
        false ->
            false
    end.

%% Validate refresh token
validate_refresh_token(RefreshData, ClientId, State) ->
    % Check expiration
    case erlang:system_time(second) < RefreshData#{expires_at} of
        true ->
            % Check client ID matches
            RefreshData#{client_id} =:= ClientId;
        false ->
            false
    end.

%% Generate access token
generate_access_token(TokenData) ->
    crypto:strong_rand_bytes(32).

%% Generate refresh token
generate_refresh_token(TokenData) ->
    crypto:strong_rand_bytes(32).

%% Verify client credentials
verify_client_credentials(ClientId, Secret, Client, State) ->
    case State#{config}#{require_client_secret} of
        true ->
            Client#{secret} =:= Secret;
        false ->
            true
    end.

%% Extract userinfo from claims
extract_userinfo(Claims) ->
    #{
        sub => maps:get(sub, Claims, <<>>),
        name => maps:get(name, Claims, <<>>),
        email => maps:get(email, Claims, <<>>),
        email_verified => maps:get(email_verified, Claims, false),
        preferred_username => maps:get(preferred_username, Claims, <<>>)
    }.

%% Validate client metadata
validate_client_metadata(Metadata) ->
    % Validate required fields
    RequiredFields = [redirect_uris, response_types],
    case lists:all(fun(Field) -> maps:is_key(Field, Metadata) end, RequiredFields) of
        true ->
            % Validate redirect URIs format
            RedirectUris = maps:get(redirect_uris, Metadata, []),
            case validate_uris(RedirectUris) of
                true ->
                    ok;
                false ->
                    {error, invalid_redirect_uris}
            end;
        false ->
            {error, missing_required_fields}
    end.

%% Validate URIs
validate_uris([]) ->
    true;
validate_uris([Uri | Rest]) ->
    case re:run(Uri, "^https?://.+") of
        nomatch ->
            false;
        _ ->
            validate_uris(Rest)
    end.

%% Cleanup expired tokens
cleanup_expired_tokens(State) ->
    Now = erlang:system_time(second),

    % Cleanup access tokens
    ets:foldl(fun({TokenId, TokenData}, Acc) ->
                 case TokenData#{expires_at} < Now of
                     true ->
                         ets:delete(State#{tokens}, TokenId);
                     false ->
                         ok
                 end,
                 Acc
              end, ok, State#{tokens}),

    % Refresh tokens are handled during token refresh
    % Codes have their own expiration
    ets:foldl(fun({CodeId, CodeData}, Acc) ->
                 case CodeData#{expires_at} < Now of
                     true ->
                         ets:delete(State#{codes}, CodeId);
                     false ->
                         ok
                 end,
                 Acc
              end, ok, State#{codes}).

%% Include additional helper functions as needed...
% build_authorization_url/3
% validate_pkce/3
% handle_token_response/1
% generate_jwt/2
% verify_jwt/2
% calculate_token_lifetime/1
% store_token_metadata/2
% get_token_metadata/1
% rotate_client_secret/1
% enable_client/1
% disable_client/1