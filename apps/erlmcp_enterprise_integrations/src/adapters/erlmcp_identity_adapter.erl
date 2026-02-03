%% @doc Enterprise Identity Adapter
%% Integrates with enterprise identity providers (Okta, Azure AD, ADFS)
-module(erlmcp_identity_adapter).

-behaviour(gen_server).

-export([start_link/0, authenticate/2, authorize/3, get_user_info/1,
         get_groups/1, provision_user/2, deprovision_user/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Type Definitions
%%====================================================================

-type provider() :: okta | azure | adfs.
-type auth_token() :: binary().
-type user_id() :: binary().
-type group_id() :: binary().
-type user_info() :: map().
-type group_info() :: map().

-record(state, {
    provider :: provider(),
    config :: map(),
    connection :: pid() | undefined,
    cache :: map(),
    metrics :: map()
}).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Authenticate user with identity provider
-spec authenticate(binary(), map()) -> {ok, user_info()} | {error, term()}.
authenticate(Credentials, Context) ->
    gen_server:call(?MODULE, {authenticate, Credentials, Context}).

%% Authorize user for specific resource
-spec authorize(user_id(), binary(), map()) -> {ok, boolean()} | {error, term()}.
authorize(UserId, Resource, Context) ->
    gen_server:call(?MODULE, {authorize, UserId, Resource, Context}).

%% Get user information
-spec get_user_info(user_id()) -> {ok, user_info()} | {error, term()}.
get_user_info(UserId) ->
    gen_server:call(?MODULE, {get_user_info, UserId}).

%% Get user groups
-spec get_groups(user_id()) -> {ok, [group_info()]} | {error, term()}.
get_groups(UserId) ->
    gen_server:call(?MODULE, {get_groups, UserId}).

%% Provision a new user
-spec provision_user(user_info(), map()) -> {ok, user_id()} | {error, term()}.
provision_user(UserInfo, Context) ->
    gen_server:call(?MODULE, {provision_user, UserInfo, Context}).

%% Deprovision a user
-spec deprovision_user(user_id()) -> ok | {error, term()}.
deprovision_user(UserId) ->
    gen_server:call(?MODULE, {deprovision_user, UserId}).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

init([]) ->
    %% Initialize state with configuration
    Config = load_config(),
    Provider = maps:get(provider, Config, undefined),

    State = #state{
        provider = Provider,
        config = Config,
        connection = undefined,
        cache = #{},
        metrics = #{}
    },

    %% Initialize provider-specific connection
    case Provider of
        okta ->
            okta_connection:start(State);
        azure ->
            azure_connection:start(State);
        adfs ->
            adfs_connection:start(State);
        _ ->
            throw({invalid_provider, Provider})
    end,

    {ok, State}.

handle_call({authenticate, Credentials, Context}, _From, State) ->
    try
        %% Authenticate based on provider
        case State#state.provider of
            okta -> authenticate_okta(Credentials, Context, State);
            azure -> authenticate_azure(Credentials, Context, State);
            adfs -> authenticate_adfs(Credentials, Context, State)
        end
    catch
        Error:Reason ->
            ?LOG_ERROR("Authentication failed: ~p:~p", [Error, Reason]),
            {reply, {error, Reason}, State}
    end;

handle_call({authorize, UserId, Resource, Context}, _From, State) ->
    try
        %% Authorize user for resource
        {ok, UserGroups} = get_groups(UserId, State),
        Authorized = check_authorization(UserGroups, Resource, Context),

        %% Update metrics
        NewMetrics = update_metrics(State#state.metrics, authorize, 1),

        {reply, {ok, Authorized}, State#state{metrics = NewMetrics}}
    catch
        Error:Reason ->
            ?LOG_ERROR("Authorization check failed: ~p:~p", [Error, Reason]),
            {reply, {error, Reason}, State}
    end;

handle_call({get_user_info, UserId}, _From, State) ->
    try
        %% Check cache first
        case maps:get(UserId, State#state.cache, not_found) of
            not_found ->
                %% Fetch from provider
                UserInfo = fetch_user_info(UserId, State),
                Cache = maps:put(UserId, UserInfo, State#state.cache),

                %% Update metrics
                NewMetrics = update_metrics(State#state.metrics, get_user_info, 1),

                {reply, {ok, UserInfo}, State#state{cache = Cache, metrics = NewMetrics}};
            UserInfo ->
                {reply, {ok, UserInfo}, State}
        end
    catch
        Error:Reason ->
            ?LOG_ERROR("Failed to get user info: ~p:~p", [Error, Reason]),
            {reply, {error, Reason}, State}
    end;

handle_call({get_groups, UserId}, _From, State) ->
    try
        %% Get user groups from provider
        Groups = fetch_user_groups(UserId, State),

        %% Update metrics
        NewMetrics = update_metrics(State#state.metrics, get_groups, 1),

        {reply, {ok, Groups}, State#state{metrics = NewMetrics}}
    catch
        Error:Reason ->
            ?LOG_ERROR("Failed to get groups: ~p:~p", [Error, Reason]),
            {reply, {error, Reason}, State}
    end;

handle_call({provision_user, UserInfo, Context}, _From, State) ->
    try
        %% Provision user in identity provider
        UserId = provision_user_to_provider(UserInfo, Context, State),

        %% Update cache
        Cache = maps:put(UserId, UserInfo, State#state.cache),

        %% Update metrics
        NewMetrics = update_metrics(State#state.metrics, provision_user, 1),

        {reply, {ok, UserId}, State#state{cache = Cache, metrics = NewMetrics}}
    catch
        Error:Reason ->
            ?LOG_ERROR("User provisioning failed: ~p:~p", [Error, Reason]),
            {reply, {error, Reason}, State}
    end;

handle_call({deprovision_user, UserId}, _From, State) ->
    try
        %% Deprovision user from provider
        deprovision_user_from_provider(UserId, State),

        %% Remove from cache
        Cache = maps:remove(UserId, State#state.cache),

        %% Update metrics
        NewMetrics = update_metrics(State#state.metrics, deprovision_user, 1),

        {reply, ok, State#state{cache = Cache, metrics = NewMetrics}}
    catch
        Error:Reason ->
            ?LOG_ERROR("User deprovisioning failed: ~p:~p", [Error, Reason]),
            {reply, {error, Reason}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Cast, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    %% Clean up resources
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

load_config() ->
    %% Load identity provider configuration
    case application:get_env(erlmcp_enterprise_integrations, identity_config) of
        undefined -> default_identity_config();
        {ok, Config} -> Config
    end.

default_identity_config() ->
    #{
        provider => okta,
        endpoint => "https://your-org.okta.com",
        api_key => undefined,
        client_id => undefined,
        client_secret => undefined,
        auth_flow => client_credentials,
        cache_ttl => 300000, % 5 minutes
        timeout => 30000
    }.

authenticate_okta(Credentials, Context, State) ->
    %% Authenticate with Okta using OAuth2
    Endpoint = maps:get(endpoint, State#state.config) ++ "/api/v1/authn",
    Headers = okta_headers(State),
    Body = okta_auth_body(Credentials, Context),

    case httpc:request(post, {Endpoint, Headers, "application/json", Body},
                     [{timeout, maps:get(timeout, State#state.config)}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            AuthData = jsx:decode(ResponseBody, [{labels, binary}]),
            {ok, parse_okta_auth_response(AuthData)};
        {ok, {{_, 401, _}, _, _}} ->
            {error, invalid_credentials};
        {ok, {{_, 429, _}, _, _}} ->
            {error, rate_limit_exceeded};
        {ok, {{_, Code, _}, _, _}} ->
            {error, {authentication_failed, Code}};
        {error, Reason} ->
            {error, {network_error, Reason}}
    end.

authenticate_azure(Credentials, Context, State) ->
    %% Authenticate with Azure AD using OAuth2
    Endpoint = maps:get(endpoint, State#state.config) ++ "/oauth2/token",
    Headers = azure_headers(State),
    Body = azure_auth_body(Credentials, Context),

    case httpc:request(post, {Endpoint, Headers, "application/x-www-form-urlencoded", Body},
                     [{timeout, maps:get(timeout, State#state.config)}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            AuthData = jsx:decode(ResponseBody, [{labels, binary}]),
            {ok, parse_azure_auth_response(AuthData)};
        {ok, {{_, 401, _}, _, _}} ->
            {error, invalid_credentials};
        {ok, {{_, 429, _}, _, _}} ->
            {error, rate_limit_exceeded};
        {ok, {{_, Code, _}, _, _}} ->
            {error, {authentication_failed, Code}};
        {error, Reason} ->
            {error, {network_error, Reason}}
    end.

authenticate_adfs(Credentials, Context, State) ->
    %% Authenticate with ADFS using SAML
    Endpoint = maps:get(endpoint, State#state.config) ++ "/adfs/ls/idpinitiatedsignon.aspx",
    Headers = adfs_headers(State),
    Body = adfs_auth_body(Credentials, Context),

    case httpc:request(post, {Endpoint, Headers, "application/x-www-form-urlencoded", Body},
                     [{timeout, maps:get(timeout, State#state.config)}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            {ok, parse_adfs_auth_response(ResponseBody)};
        {ok, {{_, 401, _}, _, _}} ->
            {error, invalid_credentials};
        {ok, {{_, 429, _}, _, _}} ->
            {error, rate_limit_exceeded};
        {ok, {{_, Code, _}, _, _}} ->
            {error, {authentication_failed, Code}};
        {error, Reason} ->
            {error, {network_error, Reason}}
    end.

okta_headers(State) ->
    #{
        <<"Authorization">> => <<"Bearer ", (maps:get(api_key, State#state.config))/binary>>,
        <<"Accept">> => <<"application/json">>,
        <<"Content-Type">> => <<"application/json">>
    }.

okta_auth_body(Credentials, Context) ->
    %% Build Okta authentication request body
    #{<<"username">> => Credentials,
      <<"password">> => <<"your-password">>,
      <<"options">> => #{<<"warnBeforePasswordExpired">> => true}}.

azure_headers(State) ->
    #{
        <<"Content-Type">> => <<"application/x-www-form-urlencoded">>
    }.

azure_auth_body(Credentials, Context) ->
    %% Build Azure AD authentication request body
    ClientId = maps:get(client_id, State#state.config),
    ClientSecret = maps:get(client_secret, State#state.config),
    Scope = <<"openid profile offline_access">>,
    GrantType = <<"password">>,
    Resource = <<"https://graph.microsoft.com">>,

    <<(iolist_to_binary(["client_id=", ClientId, "&client_secret=", ClientSecret,
                        "&scope=", Scope, "&grant_type=", GrantType, "&resource=", Resource,
                        "&username=", Credentials, "&password=your-password"]))/binary>>.

adfs_headers(State) ->
    #{
        <<"Content-Type">> => <<"application/x-www-form-urlencoded">>
    }.

adfs_auth_body(Credentials, Context) ->
    %% Build ADFS authentication request body
    <<(iolist_to_binary(["username=", Credentials, "&password=your-password",
                        "&AuthMethod=FormsAuthentication"]))/binary>>.

parse_okta_auth_response(Response) ->
    %% Parse Okta authentication response
    #{<<"user">> => UserInfo} = Response,
    UserInfo.

parse_azure_auth_response(Response) ->
    %% Parse Azure AD authentication response
    #{<<"access_token">> => AccessToken, <<"expires_in">> => ExpiresIn,
      <<"refresh_token">> => RefreshToken, <<"token_type">> => TokenType} = Response,
    #{
        access_token => AccessToken,
        expires_in => ExpiresIn,
        refresh_token => RefreshToken,
        token_type => TokenType
    }.

parse_adfs_auth_response(Response) ->
    %% Parse ADFS authentication response
    %% Extract SAML assertion from response
    SAMLAssertion = extract_saml_assertion(Response),
    #{
        saml_assertion => SAMLAssertion
    }.

extract_saml_assertion(Response) ->
    %% Extract SAML assertion from HTML response
    %% This is a simplified implementation
    case binary:match(Response, <<"<input type='hidden' name='SAMLResponse' value='">>) of
        nomatch -> undefined;
        {Start, _} ->
            End = binary:find(Response, <<"'">>, Start + 1),
            binary:part(Response, Start + 42, End - Start - 42)
    end.

fetch_user_info(UserId, State) ->
    %% Fetch user information from provider
    case State#state.provider of
        okta -> fetch_okta_user_info(UserId, State);
        azure -> fetch_azure_user_info(UserId, State);
        adfs -> fetch_adfs_user_info(UserId, State)
    end.

fetch_okta_user_info(UserId, State) ->
    Endpoint = maps:get(endpoint, State#state.config) ++ "/api/v1/users/" ++ UserId,
    Headers = okta_headers(State),

    case httpc:request(get, {Endpoint, Headers},
                     [{timeout, maps:get(timeout, State#state.config)}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            jsx:decode(ResponseBody, [{labels, binary}];
        {ok, {{_, Code, _}, _, _}} ->
            throw({failed_to_fetch_user, Code});
        {error, Reason} ->
            throw({network_error, Reason})
    end.

fetch_azure_user_info(UserId, State) ->
    Endpoint = maps:get(endpoint, State#state.config) ++ "/v1.0/users/" ++ UserId,
    Headers = azure_headers(State),

    case httpc:request(get, {Endpoint, Headers},
                     [{timeout, maps:get(timeout, State#state.config)}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            jsx:decode(ResponseBody, [{labels, binary}];
        {ok, {{_, Code, _}, _, _}} ->
            throw({failed_to_fetch_user, Code});
        {error, Reason} ->
            throw({network_error, Reason})
    end.

fetch_adfs_user_info(UserId, State) ->
    %% ADFS doesn't have a direct user info endpoint
    %% Return minimal user information
    #{<<"id">> => UserId, <<"display_name">> => UserId}.

fetch_user_groups(UserId, State) ->
    %% Fetch user groups from provider
    case State#state.provider of
        okta -> fetch_okta_user_groups(UserId, State);
        azure -> fetch_azure_user_groups(UserId, State);
        adfs -> fetch_adfs_user_groups(UserId, State)
    end.

fetch_okta_user_groups(UserId, State) ->
    Endpoint = maps:get(endpoint, State#state.config) ++ "/api/v1/users/" ++ UserId ++ "/groups",
    Headers = okta_headers(State),

    case httpc:request(get, {Endpoint, Headers},
                     [{timeout, maps:get(timeout, State#state.config)}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            Groups = jsx:decode(ResponseBody, [{labels, binary}]),
            proplists:get_value(<<"groups">>, Groups, []);
        {ok, {{_, Code, _}, _, _}} ->
            throw({failed_to_fetch_groups, Code});
        {error, Reason} ->
            throw({network_error, Reason})
    end.

fetch_azure_user_groups(UserId, State) ->
    Endpoint = maps:get(endpoint, State#state.config) ++ "/v1.0/users/" ++ UserId ++ "/memberOf",
    Headers = azure_headers(State),

    case httpc:request(get, {Endpoint, Headers},
                     [{timeout, maps:get(timeout, State#state.config)}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            Groups = jsx:decode(ResponseBody, [{labels, binary}]),
            proplists:get_value(<<"value">>, Groups, []);
        {ok, {{_, Code, _}, _, _}} ->
            throw({failed_to_fetch_groups, Code});
        {error, Reason} ->
            throw({network_error, Reason})
    end.

fetch_adfs_user_groups(UserId, State) ->
    %% ADFS groups are typically managed through Windows groups
    %% Return empty list for now
    [].

check_authorization(UserGroups, Resource, Context) ->
    %% Check if user has authorization for resource
    %% This is a simplified implementation
    ResourceGroups = proplists:get_value(groups, Context, []),
    lists:any(fun(Group) -> lists:member(Group, ResourceGroups) end, UserGroups).

provision_user_to_provider(UserInfo, Context, State) ->
    %% Provision user to identity provider
    case State#state.provider of
        okta -> provision_okta_user(UserInfo, Context, State);
        azure -> provision_azure_user(UserInfo, Context, State);
        adfs -> provision_adfs_user(UserInfo, Context, State)
    end.

provision_okta_user(UserInfo, Context, State) ->
    Endpoint = maps:get(endpoint, State#state.config) ++ "/api/v1/users",
    Headers = okta_headers(State),
    Body = jsx:encode(UserInfo),

    case httpc:request(post, {Endpoint, Headers, "application/json", Body},
                     [{timeout, maps:get(timeout, State#state.config)}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            jsx:decode(ResponseBody, [{labels, binary}]);
        {ok, {{_, Code, _}, _, _}} ->
            throw({user_provision_failed, Code});
        {error, Reason} ->
            throw({network_error, Reason})
    end.

provision_azure_user(UserInfo, Context, State) ->
    Endpoint = maps:get(endpoint, State#state.config) ++ "/v1.0/users",
    Headers = azure_headers(State),
    Body = jsx:encode(UserInfo),

    case httpc:request(post, {Endpoint, Headers, "application/json", Body},
                     [{timeout, maps:get(timeout, State#state.config)}], []) of
        {ok, {{_, 201, _}, _, ResponseBody}} ->
            jsx:decode(ResponseBody, [{labels, binary}];
        {ok, {{_, Code, _}, _, _}} ->
            throw({user_provision_failed, Code});
        {error, Reason} ->
            throw({network_error, Reason})
    end.

provision_adfs_user(UserInfo, Context, State) ->
    %% ADFS provisioning typically requires Windows tools
    %% Return dummy ID for now
    UserId = proplists:get_value(id, UserInfo),
    UserId.

deprovision_user_from_provider(UserId, State) ->
    %% Deprovision user from identity provider
    case State#state.provider of
        okta -> deprovision_okta_user(UserId, State);
        azure -> deprovision_azure_user(UserId, State);
        adfs -> deprovision_adfs_user(UserId, State)
    end.

deprovision_okta_user(UserId, State) ->
    Endpoint = maps:get(endpoint, State#state.config) ++ "/api/v1/users/" ++ UserId,
    Headers = okta_headers(State),

    case httpc:request(delete, {Endpoint, Headers},
                     [{timeout, maps:get(timeout, State#state.config)}], []) of
        {ok, {{_, 200, _}, _, _}} ->
            ok;
        {ok, {{_, Code, _}, _, _}} ->
            throw({user_deprovision_failed, Code});
        {error, Reason} ->
            throw({network_error, Reason})
    end.

deprovision_azure_user(UserId, State) ->
    Endpoint = maps:get(endpoint, State#state.config) ++ "/v1.0/users/" ++ UserId,
    Headers = azure_headers(State),

    case httpc:request(delete, {Endpoint, Headers},
                     [{timeout, maps:get(timeout, State#state.config)}], []) of
        {ok, {{_, 204, _}, _, _}} ->
            ok;
        {ok, {{_, Code, _}, _, _}} ->
            throw({user_deprovision_failed, Code});
        {error, Reason} ->
            throw({network_error, Reason})
    end.

deprovision_adfs_user(UserId, State) ->
    %% ADFS deprovisioning typically requires Windows tools
    %% Return success for now
    ok.

update_metrics(Metrics, Type, Inc) ->
    %% Update performance metrics
    Key = atom_to_binary(Type, utf8),
    maps:update_with(Key, fun(V) -> V + Inc end, Inc, Metrics).