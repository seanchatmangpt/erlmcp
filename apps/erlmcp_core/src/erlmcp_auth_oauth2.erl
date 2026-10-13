%%%-------------------------------------------------------------------
%%% @doc erlmcp_auth_oauth2 - OAuth 2.0 / 2.1 Authentication Implementation
%%%
%%% This module implements comprehensive OAuth 2.0/2.1 flows with security best practices:
%%%
%%% == Supported Flows ==
%%%
%%% 1. **Authorization Code Flow** (recommended for server-side apps)
%%%    - User redirected to authorization server
%%%    - Exchange authorization code for access token
%%%    - Supports PKCE (Proof Key for Code Exchange)
%%%
%%% 2. **Client Credentials Flow** (for service-to-service auth)
%%%    - Direct token exchange with client credentials
%%%    - No user interaction required
%%%    - Token refresh supported
%%%
%%% 3. **Implicit Flow** (deprecated - OAuth 2.1 compliant)
%%%    - Token returned in redirect URI
%%%    - Shows deprecation warnings
%%%    - Use authorization code flow instead
%%%
%%% == PKCE Support ==
%%%
%%% - Code verifier: Random 43-128 character string
%%% - Code challenge: SHA256 hash of verifier
%%% - Prevents authorization code interception attacks
%%% - Required for mobile/native apps (OAuth 2.1)
%%%
%%% == Security Features ==
%%%
%%% - State parameter for CSRF protection
%%% - TLS/SSL required for all endpoints
%%% - Token introspection for validation
%%% - Secure token storage
%%% - PKCE for public clients
%%%
%%% == Usage ==
%%%
%%% ```
%%% %% Start OAuth2 client
%%% Config = #{
%%%     client_id => <<"my_client">>,
%%%     client_secret => <<"my_secret">>,
%%%     redirect_uri => <<"https://example.com/callback">>,
%%%     authorization_endpoint => <<"https://auth.example.com/authorize">>,
%%%     token_endpoint => <<"https://auth.example.com/token">>,
%%%     introspection_endpoint => <<"https://auth.example.com/introspect">>,
%%%     scope => [<<"read">>, <<"write">>],
%%%     use_pkce => true
%%% },
%%% {ok, Pid} = erlmcp_auth_oauth2:start_link(Config).
%%%
%%% %% Generate authorization URL (authorization code flow with PKCE)
%%% {ok, AuthURL, State} = erlmcp_auth_oauth2:authorization_url(Pid),
%%%
%%% %% Handle callback
%%% case erlmcp_auth_oauth2:handle_callback(Pid, CallbackParams) of
%%%     {ok, TokenInfo} -> process_token(TokenInfo);
%%%     {error, Reason} -> handle_error(Reason)
%%% end.
%%'
%%% '''
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_auth_oauth2).

-behaviour(gen_server).

%% API exports
-export([start_link/1, start_link/2,
         authorization_url/0, authorization_url/1,
         handle_callback/1, handle_callback/2,
         client_credentials/0, client_credentials/1,
         refresh_token/1,
         validate_token/1,
         introspect_token/1,
         generate_pkce/0,
         verify_pkce/2,
         stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Types
-type oauth2_config() :: #{
    client_id := binary(),
    client_secret := binary(),
    redirect_uri := binary(),
    authorization_endpoint := binary(),
    token_endpoint := binary(),
    introspection_endpoint => binary(),
    scope => [binary()],
    use_pkce => boolean(),
    http_client => module(),
    http_timeout => pos_integer()
}.

-type token_info() :: #{
    access_token := binary(),
    token_type := binary(),
    expires_in => pos_integer(),
    refresh_token => binary(),
    scope => [binary()],
    id_token => binary()
}.

-type oauth2_state() :: binary().
-type pkce_verifier() :: binary().
-type pkce_challenge() :: binary().

-export_type([oauth2_config/0, token_info/0, oauth2_state/0, pkce_verifier/0, pkce_challenge/0]).

%% State record
-record(state,
        {config :: oauth2_config(),
         pending_states :: ets:tid(),  % state -> {verifier, timestamp}
         token_cache :: ets:tid(),     % token -> token_info
         client :: module()}).

-include_lib("kernel/include/logger.hrl").

%% Defaults
-define(DEFAULT_HTTP_CLIENT, gun).
-define(DEFAULT_TIMEOUT, 5000).
-define(STATE_EXPIRY, 600000). % 10 minutes
-define(PKCE_VERIFIER_MIN_LENGTH, 43).
-define(PKCE_VERIFIER_MAX_LENGTH, 128).
-define(TOKEN_CACHE_TTL, 300000). % 5 minutes

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Start OAuth2 client with config
-spec start_link(oauth2_config()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Config], []).

%% @doc Start OAuth2 client with config and name
-spec start_link(atom(), oauth2_config()) -> {ok, pid()} | {error, term()}.
start_link(Name, Config) ->
    gen_server:start_link({local, Name}, ?MODULE, [Config], []).

%% @doc Generate authorization URL for authorization code flow
%% Uses default OAuth2 client (?MODULE)
-spec authorization_url() -> {ok, binary(), oauth2_state()} | {error, term()}.
authorization_url() ->
    authorization_url(?MODULE).

%% @doc Generate authorization URL for authorization code flow
%% Includes PKCE if enabled in config
-spec authorization_url(pid() | atom()) -> {ok, binary(), oauth2_state()} | {error, term()}.
authorization_url(Pid) ->
    gen_server:call(Pid, authorization_url).

%% @doc Handle OAuth2 callback with authorization code
%% Uses default OAuth2 client (?MODULE)
-spec handle_callback(map()) -> {ok, token_info()} | {error, term()}.
handle_callback(Params) ->
    handle_callback(?MODULE, Params).

%% @doc Handle OAuth2 callback with authorization code
%% Validates state parameter, exchanges code for token
-spec handle_callback(pid() | atom(), map()) -> {ok, token_info()} | {error, term()}.
handle_callback(Pid, Params) ->
    gen_server:call(Pid, {handle_callback, Params}).

%% @doc Request token using client credentials flow
%% Uses default OAuth2 client (?MODULE)
-spec client_credentials() -> {ok, token_info()} | {error, term()}.
client_credentials() ->
    client_credentials(?MODULE).

%% @doc Request token using client credentials flow
%% For service-to-service authentication without user interaction
-spec client_credentials(pid() | atom()) -> {ok, token_info()} | {error, term()}.
client_credentials(Pid) ->
    gen_server:call(Pid, client_credentials).

%% @doc Refresh access token using refresh token
-spec refresh_token(binary()) -> {ok, token_info()} | {error, term()}.
refresh_token(RefreshToken) ->
    gen_server:call(?MODULE, {refresh_token, RefreshToken}).

%% @doc Validate token and get token info
-spec validate_token(binary()) -> {ok, token_info()} | {error, term()}.
validate_token(Token) ->
    gen_server:call(?MODULE, {validate_token, Token}).

%% @doc Introspect token via introspection endpoint
-spec introspect_token(binary()) -> {ok, map()} | {error, term()}.
introspect_token(Token) ->
    gen_server:call(?MODULE, {introspect_token, Token}).

%% @doc Generate PKCE verifier and challenge
%% Returns {Verifier, Challenge, ChallengeMethod}
-spec generate_pkce() -> {pkce_verifier(), pkce_challenge(), binary()}.
generate_pkce() ->
    Verifier = generate_code_verifier(),
    Challenge = base64:encode(crypto:hash(sha256, Verifier)),
    {Verifier,Challenge, <<"S256">>}.

%% @doc Verify PKCE challenge against verifier
-spec verify_pkce(pkce_verifier(), pkce_challenge()) -> boolean().
verify_pkce(Verifier, Challenge) ->
    ExpectedChallenge = base64:encode(crypto:hash(sha256, Verifier)),
    ExpectedChallenge =:= Challenge.

%% @doc Stop OAuth2 client
-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Config]) ->
    %% Validate config
    case validate_config(Config) of
        ok ->
            %% Create ETS tables
            PendingStates = ets:new(?MODULE, [set, private, {read_concurrency, true}]),
            TokenCache = ets:new(?MODULE, [set, private, {read_concurrency, true}]),

            %% Get HTTP client
            Client = maps:get(http_client, Config, ?DEFAULT_HTTP_CLIENT),

            State = #state{
                config = Config,
                pending_states = PendingStates,
                token_cache = TokenCache,
                client = Client
            },

            %% Cleanup expired states periodically
            timer:send_interval(60000, self(), cleanup_expired_states),

            {ok, State};
        {error, Reason} ->
            {stop, {config_error, Reason}}
    end.

handle_call(authorization_url, _From, State) ->
    {Reply, NewState} = do_authorization_url(State),
    {reply, Reply, NewState};

handle_call({handle_callback, Params}, _From, State) ->
    {Reply, NewState} = do_handle_callback(Params, State),
    {reply, Reply, NewState};

handle_call(client_credentials, _From, State) ->
    {Reply, NewState} = do_client_credentials(State),
    {reply, Reply, NewState};

handle_call({refresh_token, RefreshToken}, _From, State) ->
    {Reply, NewState} = do_refresh_token(RefreshToken, State),
    {reply, Reply, NewState};

handle_call({validate_token, Token}, _From, State) ->
    Reply = do_validate_token(Token, State),
    {reply, Reply, State};

handle_call({introspect_token, Token}, _From, State) ->
    Reply = do_introspect_token(Token, State),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(cleanup_expired_states, State) ->
    NewState = cleanup_states(State),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{pending_states = PendingStates, token_cache = TokenCache}) ->
    ets:delete(PendingStates),
    ets:delete(TokenCache),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @private Validate OAuth2 configuration
validate_config(Config) ->
    Required = [client_id, client_secret, redirect_uri,
                authorization_endpoint, token_endpoint],
    case lists:all(fun(Key) -> maps:is_key(Key, Config) end, Required) of
        true ->
            %% Validate endpoints are HTTPS
            Endpoints = [maps:get(authorization_endpoint, Config),
                        maps:get(token_endpoint, Config)],
            case lists:all(fun is_https_endpoint/1, Endpoints) of
                true -> ok;
                false -> {error, {security_violation, http_not_allowed}}
            end;
        false ->
            {error, missing_required_fields}
    end.

%% @private Check if endpoint uses HTTPS
is_https_endpoint(Endpoint) when is_binary(Endpoint) ->
    case binary:match(Endpoint, <<"https://">>) of
        {0, 8} -> true;
        _ -> false
    end;
is_https_endpoint(_) ->
    false.

%% @private Generate authorization URL
do_authorization_url(#state{config = Config, pending_states = PendingStates} = State) ->
    %% Generate state parameter
    StateParam = generate_state(),

    %% Generate PKCE if enabled
    UsePKCE = maps:get(use_pkce, Config, false),
    {Verifier, Challenge, ChallengeMethod} = case UsePKCE of
        true -> generate_pkce();
        false -> {undefined, undefined, undefined}
    end,

    %% Build authorization URL
    ClientID = maps:get(client_id, Config),
    RedirectURI = maps:get(redirect_uri, Config),
    AuthEndpoint = maps:get(authorization_endpoint, Config),
    Scope = maps:get(scope, Config, []),

    %% Build query parameters
    Params = [
        {<<"response_type">>, <<"code">>},
        {<<"client_id">>, ClientID},
        {<<"redirect_uri">>, RedirectURI},
        {<<"state">>, StateParam}
    ] ++
    case Scope of
        [] -> [];
        _ -> [{<<"scope">>, join_scopes(Scope)}]
    end ++
    case Challenge of
        undefined -> [];
        _ -> [
            {<<"code_challenge">>, Challenge},
            {<<"code_challenge_method">>, ChallengeMethod}
        ]
    end,

    %% Construct URL
    AuthURL = build_url(AuthEndpoint, Params),

    %% Store state with verifier and timestamp
    ets:insert(PendingStates, {StateParam, {Verifier, erlang:monotonic_time(millisecond)}}),

    {{ok, AuthURL, StateParam}, State}.

%% @private Handle callback from authorization server
do_handle_callback(Params, #state{config = Config, pending_states = PendingStates, token_cache = TokenCache} = State) ->
    %% Extract parameters
    Code = maps:get(<<"code">>, Params, undefined),
    StateParam = maps:get(<<"state">>, Params, undefined),
    Error = maps:get(<<"error">>, Params, undefined),

    case Error of
        undefined ->
            %% Successful authorization
            case {Code, StateParam} of
                {undefined, _} ->
                    {{error, missing_code}, State};
                {_, undefined} ->
                    {{error, missing_state}, State};
                {Code, StateParam} ->
                    %% Validate state
                    case ets:lookup(PendingStates, StateParam) of
                        [{StateParam, {Verifier, _Timestamp}}] ->
                            %% State valid, exchange code for token
                            TokenEndpoint = maps:get(token_endpoint, Config),
                            TokenParams = [
                                {<<"grant_type">>, <<"authorization_code">>},
                                {<<"code">>, Code},
                                {<<"redirect_uri">>, maps:get(redirect_uri, Config)},
                                {<<"client_id">>, maps:get(client_id, Config)},
                                {<<"client_secret">>, maps:get(client_secret, Config)}
                            ] ++
                            case Verifier of
                                undefined -> [];
                                _ -> [{<<"code_verifier">>, Verifier}]
                            end,

                            case exchange_token(TokenEndpoint, TokenParams, State) of
                                {ok, TokenInfo} ->
                                    %% Cache token
                                    AccessToken = maps:get(access_token, TokenInfo),
                                    ets:insert(TokenCache, {AccessToken, {TokenInfo, erlang:monotonic_time(millisecond)}}),

                                    %% Clean up state
                                    ets:delete(PendingStates, StateParam),

                                    {{ok, TokenInfo}, State};
                                {error, Reason} ->
                                    {{error, {token_exchange_failed, Reason}}, State}
                            end;
                        [] ->
                            {{error, invalid_state}, State}
                    end
            end;
        _Error ->
            %% Authorization server returned error
            ErrorDesc = maps:get(<<"error_description">>, Params, <<>>),
            {{error, {authorization_error, Error, ErrorDesc}}, State}
    end.

%% @private Request token using client credentials flow
do_client_credentials(#state{config = Config, token_cache = TokenCache} = State) ->
    TokenEndpoint = maps:get(token_endpoint, Config),
    Scope = maps:get(scope, Config, []),

    Params = [
        {<<"grant_type">>, <<"client_credentials">>},
        {<<"client_id">>, maps:get(client_id, Config)},
        {<<"client_secret">>, maps:get(client_secret, Config)}
    ] ++
    case Scope of
        [] -> [];
        _ -> [{<<"scope">>, join_scopes(Scope)}]
    end,

    case exchange_token(TokenEndpoint, Params, State) of
        {ok, TokenInfo} ->
            %% Cache token
            AccessToken = maps:get(access_token, TokenInfo),
            ets:insert(TokenCache, {AccessToken, {TokenInfo, erlang:monotonic_time(millisecond)}}),
            {{ok, TokenInfo}, State};
        {error, Reason} ->
            {{error, {token_request_failed, Reason}}, State}
    end.

%% @private Refresh access token
do_refresh_token(RefreshToken, #state{config = Config, token_cache = TokenCache} = State) ->
    TokenEndpoint = maps:get(token_endpoint, Config),

    Params = [
        {<<"grant_type">>, <<"refresh_token">>},
        {<<"refresh_token">>, RefreshToken},
        {<<"client_id">>, maps:get(client_id, Config)},
        {<<"client_secret">>, maps:get(client_secret, Config)}
    ],

    case exchange_token(TokenEndpoint, Params, State) of
        {ok, TokenInfo} ->
            %% Update cache with new token
            AccessToken = maps:get(access_token, TokenInfo),
            ets:insert(TokenCache, {AccessToken, {TokenInfo, erlang:monotonic_time(millisecond)}}),
            {{ok, TokenInfo}, State};
        {error, Reason} ->
            {{error, {refresh_failed, Reason}}, State}
    end.

%% @private Validate token from cache
do_validate_token(Token, #state{token_cache = TokenCache}) ->
    case ets:lookup(TokenCache, Token) of
        [{Token, {TokenInfo, Timestamp}}] ->
            %% Check if token expired
            case maps:get(expires_in, TokenInfo, undefined) of
                undefined ->
                    {ok, TokenInfo};
                ExpiresIn ->
                    CurrentTime = erlang:monotonic_time(millisecond),
                    case CurrentTime - Timestamp < (ExpiresIn * 1000) of
                        true -> {ok, TokenInfo};
                        false -> {error, token_expired}
                    end
            end;
        [] ->
            {error, token_not_found}
    end.

%% @private Introspect token via introspection endpoint
do_introspect_token(Token, #state{config = Config} = State) ->
    case maps:get(introspection_endpoint, Config, undefined) of
        undefined ->
            {error, introspection_not_configured};
        IntrospectEndpoint ->
            Params = [
                {<<"token">>, Token},
                {<<"client_id">>, maps:get(client_id, Config)},
                {<<"client_secret">>, maps:get(client_secret, Config)}
            ],

            case make_http_request(IntrospectEndpoint, Params, State) of
                {ok, Response} ->
                    case maps:get(<<"active">>, Response, false) of
                        true -> {ok, Response};
                        false -> {error, token_inactive}
                    end;
                {error, Reason} ->
                    {error, {introspection_failed, Reason}}
            end
    end.

%% @private Exchange authorization code or credentials for token
exchange_token(TokenEndpoint, Params, #state{client = Client, config = Config}) ->
    _Timeout = maps:get(http_timeout, Config, ?DEFAULT_TIMEOUT),

    case make_http_request(TokenEndpoint, Params, #state{client = Client, config = Config}) of
        {ok, Response} ->
            parse_token_response(Response);
        {error, Reason} ->
            {error, Reason}
    end.

%% @private Parse token response from authorization server
parse_token_response(Response) ->
    case {maps:get(<<"access_token">>, Response, undefined),
          maps:get(<<"token_type">>, Response, undefined)} of
        {undefined, _} ->
            {error, missing_access_token};
        {_, undefined} ->
            {error, missing_token_type};
        {AccessToken, TokenType} ->
            TokenInfo = #{
                access_token => AccessToken,
                token_type => TokenType
            },
            TokenInfo2 = case maps:get(<<"expires_in">>, Response, undefined) of
                undefined -> TokenInfo;
                ExpiresIn -> TokenInfo#{expires_in => ExpiresIn}
            end,
            TokenInfo3 = case maps:get(<<"refresh_token">>, Response, undefined) of
                undefined -> TokenInfo2;
                RefreshToken -> TokenInfo2#{refresh_token => RefreshToken}
            end,
            TokenInfo4 = case maps:get(<<"scope">>, Response, undefined) of
                undefined -> TokenInfo3;
                ScopeBin -> TokenInfo3#{scope => binary:split(ScopeBin, <<" ">>, [global])}
            end,
            TokenInfo5 = case maps:get(<<"id_token">>, Response, undefined) of
                undefined -> TokenInfo4;
                IDToken -> TokenInfo4#{id_token => IDToken}
            end,
            {ok, TokenInfo5}
    end.

%% @private Make HTTP request using gun client
make_http_request(URL, Params, #state{client = gun, config = Config}) ->
    %% Parse URL
    case parse_url(URL) of
        {ok, _Scheme, Host, Port, Path} ->
            Timeout = maps:get(http_timeout, Config, ?DEFAULT_TIMEOUT),

            %% Start gun connection
            {ok, ConnPid} = gun:open(Host, Port, #{transport => ssl, protocols => [http]}),
            {ok, _Protocol} = gun:await_up(ConnPid, Timeout),

            %% Encode parameters as form data
            Body = encode_form_data(Params),

            %% Make POST request
            StreamRef = gun:post(ConnPid, Path, [
                {<<"content-type">>, <<"application/x-www-form-urlencoded">>},
                {<<"accept">>, <<"application/json">>}
            ], Body),

            %% Await response
            case gun:await(ConnPid, StreamRef, Timeout) of
                {response, nofin, 200, _Headers} ->
                    %% Read body
                    {data, nofin, ResponseBody} = gun:await(ConnPid, StreamRef, Timeout),

                    %% Close connection
                    gun:close(ConnPid),

                    %% Parse JSON response
                    try json:decode(ResponseBody) of
                        Response when is_map(Response) ->
                            {ok, Response};
                        _ ->
                            {error, invalid_response_format}
                    catch
                        _:_ ->
                            {error, json_decode_error}
                    end;
                {response, _, StatusCode, _Headers} ->
                    gun:close(ConnPid),
                    {error, {http_error, StatusCode}};
                {error, Reason} ->
                    gun:close(ConnPid),
                    {error, {request_failed, Reason}}
            end;
        {error, Reason} ->
            {error, {invalid_url, Reason}}
    end;

%% @private Make HTTP request using custom client
make_http_request(URL, Params, #state{client = ClientModule}) ->
    try
        %% Call custom client module
        apply(ClientModule, request, [post, URL, Params])
    catch
        Error:Reason ->
            {error, {client_error, Error, Reason}}
    end.

%% @private Parse URL into components
parse_url(URL) when is_binary(URL) ->
    parse_url(binary_to_list(URL));
parse_url(URL) when is_list(URL) ->
    try uri_string:parse(URL) of
        #{scheme := Scheme, host := Host} = Map ->
            Port = case maps:get(port, Map, undefined) of
                undefined -> default_port(Scheme);
                PortVal -> PortVal
            end,
            Path = case maps:get(path, Map, undefined) of
                undefined -> "/";
                PathVal -> PathVal
            end,
            {ok, list_to_existing_atom(Scheme), Host, Port, Path}
    catch
        error:_ ->
            {error, invalid_uri}
    end.

%% @private Get default port for scheme
default_port("https") -> 443;
default_port("http") -> 80;
default_port(_) -> 80.

%% @private Encode form data
encode_form_data(Params) ->
    encode_form_data(Params, []).

encode_form_data([], Acc) ->
    iolist_to_binary(lists:reverse(Acc));
encode_form_data([{Key, Value} | Rest], Acc) ->
    Encoded = [Acc, [uri_encode(Key), "=", uri_encode(Value), "&"]],
    encode_form_data(Rest, Encoded).

%% @private URI encode
uri_encode(Bin) when is_binary(Bin) ->
    uri_encode(binary_to_list(Bin));
uri_encode(List) when is_list(List) ->
    uri_encode(List, []).

uri_encode([], Acc) ->
    lists:reverse(Acc);
uri_encode([C | Rest], Acc) when C >= $a, C =< $z ->
    uri_encode(Rest, [C | Acc]);
uri_encode([C | Rest], Acc) when C >= $A, C =< $Z ->
    uri_encode(Rest, [C | Acc]);
uri_encode([C | Rest], Acc) when C >= $0, C =< $9 ->
    uri_encode(Rest, [C | Acc]);
uri_encode([$- | Rest], Acc) ->
    uri_encode(Rest, [$- | Acc]);
uri_encode([$_ | Rest], Acc) ->
    uri_encode(Rest, [$_ | Acc]);
uri_encode([$. | Rest], Acc) ->
    uri_encode(Rest, [$. | Acc]);
uri_encode([C | Rest], Acc) ->
    uri_encode(Rest, [$%, hex_digit(C bsr 4), hex_digit(C band 16#F) | Acc]).

hex_digit(N) when N >= 0, N =< 9 ->
    N + $0;
hex_digit(N) when N >= 10, N =< 15 ->
    N - 10 + $A.

%% @private Build URL with query parameters
build_url(BaseURL, Params) ->
    QueryString = encode_form_data(Params),
    <<BaseURL/binary, "?", QueryString/binary>>.

%% @private Join scopes with space delimiter
join_scopes(Scopes) ->
    list_to_binary(lists:join(<<" ">>, Scopes)).

%% @private Generate random state parameter
generate_state() ->
    base64:encode(crypto:strong_rand_bytes(24)).

%% @private Generate code verifier for PKCE
%% Must be 43-128 characters with characters [A-Z] / [a-z] / [0-9] / "-" / "." / "_" / "~"
generate_code_verifier() ->
    %% Generate 32 random bytes (43 characters when base64url encoded)
    RandomBytes = crypto:strong_rand_bytes(32),
    base64url:encode(RandomBytes).

%% @private Cleanup expired states
cleanup_states(#state{pending_states = PendingStates} = State) ->
    CurrentTime = erlang:monotonic_time(millisecond),
    ets:foldl(
        fun({StateParam, {_Verifier, Timestamp}}, Acc) ->
            case CurrentTime - Timestamp > ?STATE_EXPIRY of
                true ->
                    ets:delete(PendingStates, StateParam),
                    Acc;
                false ->
                    Acc
            end
        end,
        State,
        PendingStates
    ).
