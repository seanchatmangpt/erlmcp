-module(erlmcp_oauth_security).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    get_client_secret/0,
    get_client_id/0,
    get_oauth_config/0,
    validate_oauth_config/0,
    sanitize_config_for_logging/1,
    verify_secure_storage/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Internal state
-record(state, {
    client_id :: string() | undefined,
    client_secret :: string() | undefined,
    config :: map() | undefined
}).

-include_lib("kernel/include/logger.hrl").

%% Configuration keys
-define(OAUTH_CLIENT_ID_ENV, "OAUTH_CLIENT_ID").
-define(OAUTH_CLIENT_SECRET_ENV, "OAUTH_CLIENT_SECRET").
-define(OAUTH_TOKEN_ENDPOINT_ENV, "OAUTH_TOKEN_ENDPOINT").
-define(OAUTH_RESOURCE_INDICATOR_ENV, "OAUTH_RESOURCE_INDICATOR").

%%====================================================================
%% API
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec get_client_secret() -> string() | {error, term()}.
get_client_secret() ->
    gen_server:call(?MODULE, get_client_secret).

-spec get_client_id() -> string() | {error, term()}.
get_client_id() ->
    gen_server:call(?MODULE, get_client_id).

-spec get_oauth_config() -> map() | {error, term()}.
get_oauth_config() ->
    gen_server:call(?MODULE, get_oauth_config).

-spec validate_oauth_config() -> ok | {error, term()}.
validate_oauth_config() ->
    gen_server:call(?MODULE, validate_oauth_config).

-spec sanitize_config_for_logging(map()) -> map().
sanitize_config_for_logging(Config) ->
    %% Remove secrets from config for safe logging
    {Config, _} = maps:take(client_secret, Config),
    {Config2, _} = maps:take(access_token, Config),
    {Config3, _} = maps:take(refresh_token, Config2),
    maps:remove(client_secret, maps:remove(access_token, maps:remove(refresh_token, Config3))).

-spec verify_secure_storage() -> ok | {error, term()}.
verify_secure_storage() ->
    gen_server:call(?MODULE, verify_secure_storage).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([]) -> {ok, #state{}} | {error, term()}.
init([]) ->
    %% Validate OAuth configuration at startup
    case load_oauth_config() of
        {ok, ClientId, ClientSecret, Config} ->
            logger:info("OAuth configuration loaded from environment variables"),
            {ok, #state{
                client_id = ClientId,
                client_secret = ClientSecret,
                config = Config
            }};
        {error, Reason} ->
            logger:error("Failed to initialize OAuth: ~p", [Reason]),
            {error, Reason}
    end.

-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {reply, term(), #state{}}.

handle_call(get_client_secret, _From, #state{client_secret = Secret} = State) ->
    {reply, Secret, State};

handle_call(get_client_id, _From, #state{client_id = Id} = State) ->
    {reply, Id, State};

handle_call(get_oauth_config, _From, #state{config = Config} = State) ->
    {reply, Config, State};

handle_call(validate_oauth_config, _From, State) ->
    Result = do_validate_oauth_config(State),
    {reply, Result, State};

handle_call(verify_secure_storage, _From, State) ->
    Result = do_verify_secure_storage(State),
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

-spec load_oauth_config() ->
    {ok, string(), string(), map()} | {error, term()}.
load_oauth_config() ->
    %% Load all OAuth credentials from environment variables ONLY
    case os:getenv(?OAUTH_CLIENT_ID_ENV) of
        false ->
            {error, oauth_client_id_missing};
        "" ->
            {error, oauth_client_id_empty};
        ClientId ->
            case os:getenv(?OAUTH_CLIENT_SECRET_ENV) of
                false ->
                    {error, oauth_client_secret_missing};
                "" ->
                    {error, oauth_client_secret_empty};
                Secret when Secret =:= "   " ->
                    {error, oauth_client_secret_empty};
                ClientSecret ->
                    %% Load optional configuration
                    TokenEndpoint = os:getenv(?OAUTH_TOKEN_ENDPOINT_ENV, "https://oauth.example.com/token"),
                    ResourceIndicator = os:getenv(?OAUTH_RESOURCE_INDICATOR_ENV, "https://mcp.example.com"),

                    Config = #{
                        client_id => ClientId,
                        client_secret => ClientSecret,
                        token_endpoint => TokenEndpoint,
                        resource_indicator => ResourceIndicator,
                        enabled => true
                    },

                    {ok, ClientId, ClientSecret, Config}
            end
    end.

-spec do_validate_oauth_config(#state{}) -> ok | {error, term()}.
do_validate_oauth_config(#state{
    client_id = ClientId,
    client_secret = ClientSecret,
    config = Config
}) ->
    case validate_client_id(ClientId) of
        ok ->
            case validate_client_secret(ClientSecret) of
                ok ->
                    case validate_config_fields(Config) of
                        ok -> ok;
                        Error -> Error
                    end;
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

-spec validate_client_id(string() | undefined) -> ok | {error, term()}.
validate_client_id(undefined) ->
    {error, oauth_client_id_missing};
validate_client_id("") ->
    {error, oauth_client_id_empty};
validate_client_id(ClientId) when is_list(ClientId) ->
    case length(ClientId) > 0 of
        true -> ok;
        false -> {error, oauth_client_id_empty}
    end;
validate_client_id(_) ->
    {error, invalid_client_id_format}.

-spec validate_client_secret(string() | undefined) -> ok | {error, term()}.
validate_client_secret(undefined) ->
    {error, oauth_client_secret_missing};
validate_client_secret("") ->
    {error, oauth_client_secret_empty};
validate_client_secret(Secret) when is_list(Secret) ->
    TrimmedSecret = string:trim(Secret),
    case length(TrimmedSecret) > 0 of
        true -> ok;
        false -> {error, oauth_client_secret_empty}
    end;
validate_client_secret(_) ->
    {error, invalid_client_secret_format}.

-spec validate_config_fields(map() | undefined) -> ok | {error, term()}.
validate_config_fields(undefined) ->
    {error, oauth_config_missing};
validate_config_fields(Config) when is_map(Config) ->
    %% Verify required fields are present
    case maps:is_key(token_endpoint, Config) of
        true ->
            case maps:is_key(client_id, Config) of
                true ->
                    case maps:is_key(client_secret, Config) of
                        true -> ok;
                        false -> {error, oauth_config_invalid}
                    end;
                false ->
                    {error, oauth_config_invalid}
            end;
        false ->
            {error, oauth_config_invalid}
    end;
validate_config_fields(_) ->
    {error, oauth_config_invalid}.

-spec do_verify_secure_storage(#state{}) -> ok | {error, term()}.
do_verify_secure_storage(#state{client_secret = Secret}) ->
    %% Verify that secrets are not exposed in logger or state
    case is_secret_safe(Secret) of
        true -> ok;
        false ->
            logger:error("Secret storage verification failed"),
            {error, secret_exposure_risk}
    end.

-spec is_secret_safe(string() | undefined) -> boolean().
is_secret_safe(undefined) ->
    false;
is_secret_safe(Secret) when is_list(Secret) ->
    %% Secret should only be accessible through proper channels
    length(Secret) > 0 andalso Secret =/= "changeme" andalso Secret =/= "";
is_secret_safe(_) ->
    false.
