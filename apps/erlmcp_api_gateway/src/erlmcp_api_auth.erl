-module(erlmcp_api_auth).

-behaviour(gen_server).

%% API exports
-export([start_link/0, authenticate/2, authorize/3, generate_token/1, validate_token/1,
         create_consumer/1, get_consumer_by_id/1, get_consumer_by_key/1, revoke_token/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("erlmcp_api_gateway.hrl").

%%====================================================================
%% API Functions
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

authenticate(ConsumerId, Credentials) ->
    gen_server:call(?MODULE, {authenticate, ConsumerId, Credentials}).

authorize(ConsumerId, Resource, Action) ->
    gen_server:call(?MODULE, {authorize, ConsumerId, Resource, Action}).

generate_token(ConsumerId) ->
    gen_server:call(?MODULE, {generate_token, ConsumerId}).

validate_token(Token) ->
    gen_server:call(?MODULE, {validate_token, Token}).

create_consumer(ConsumerData) ->
    gen_server:call(?MODULE, {create_consumer, ConsumerData}).

get_consumer_by_id(ConsumerId) ->
    gen_server:call(?MODULE, {get_consumer_by_id, ConsumerId}).

get_consumer_by_key(ApiKey) ->
    gen_server:call(?MODULE, {get_consumer_by_key, ApiKey}).

revoke_token(Token) ->
    gen_server:call(?MODULE, {revoke_token, Token}).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

init([]) ->
    %% Initialize authentication storage
    ets:new(consumers, [
        named_table,
        public,
        set,
        {keypos, 2}
    ]),

    ets:new(api_keys, [
        named_table,
        public,
        set,
        {keypos, 2}
    ]),

    ets:new(tokens, [
        named_table,
        public,
        set,
        {keypos, 2}
    ]),

    ets:new(auth_logs, [
        named_table,
        public,
        set,
        {keypos, 2}
    ]),

    %% Load configuration
    Config = load_auth_config(),

    %% Start token cleanup process
    erlang:send_after(3600000, self(), cleanup_expired_tokens),

    State = #{
        config => Config,
        algorithms => load_algorithms(),
        secret_key => get_secret_key()
    },

    {ok, State}.

handle_call({authenticate, ConsumerId, Credentials}, _From, State) ->
    case get_consumer_by_id_from_storage(ConsumerId) of
        {ok, Consumer} ->
            %% Authenticate credentials
            case verify_credentials(Credentials, Consumer) of
                true ->
                    %% Log authentication attempt
                    log_auth_attempt(ConsumerId, success),

                    %% Generate session token
                    {ok, Token} = generate_token_for_consumer(ConsumerId, State),

                    {reply, {ok, Token}, State};
                false ->
                    %% Log failed attempt
                    log_auth_attempt(ConsumerId, failed),

                    {reply, {error, invalid_credentials}, State}
            end;
        {error, not_found} ->
            log_auth_attempt(ConsumerId, not_found),
            {reply, {error, consumer_not_found}, State}
    end;

handle_call({authorize, ConsumerId, Resource, Action}, _From, State) ->
    case get_consumer_by_id_from_storage(ConsumerId) of
        {ok, Consumer} ->
            %% Check consumer permissions
            case check_permissions(Consumer, Resource, Action) of
                true ->
                    {reply, {ok, authorized}, State};
                false ->
                    {reply, {error, unauthorized}, State}
            end;
        {error, not_found} ->
            {reply, {error, consumer_not_found}, State}
    end;

handle_call({generate_token, ConsumerId}, _From, State) ->
    case generate_token_for_consumer(ConsumerId, State) of
        {ok, Token} ->
            {reply, {ok, Token}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({validate_token, Token}, _From, State) ->
    case ets:lookup(tokens, Token) of
        [{Token, ConsumerId, Expires, _}] ->
            case Expires > erlang:system_time(millisecond) of
                true ->
                    {reply, {ok, ConsumerId}, State};
                false ->
                    {reply, {error, expired}, State}
            end;
        [] ->
            {reply, {error, invalid}, State}
    end;

handle_call({create_consumer, ConsumerData}, _From, State) ->
    %% Validate consumer data
    case validate_consumer_data(ConsumerData) of
        ok ->
            ConsumerId = generate_consumer_id(),

            %% Create consumer
            Consumer = #{
                id => ConsumerId,
                name => maps:get(name, ConsumerData),
                email => maps:get(email, ConsumerData),
                status => maps:get(status, ConsumerData, active),
                created_at => erlang:system_time(millisecond),
                updated_at => erlang:system_time(millisecond),
                permissions => maps:get(permissions, ConsumerData, []),
                rate_limit => maps:get(rate_limit, ConsumerData, #{}),
                metadata => maps:get(metadata, ConsumerData, #{})
            },

            %% Store consumer
            true = ets:insert(consumers, {ConsumerId, Consumer}),

            %% Generate API key
            ApiKey = generate_api_key(),
            true = ets:insert(api_keys, {ApiKey, ConsumerId}),

            {reply, {ok, ConsumerId, ApiKey}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({get_consumer_by_id, ConsumerId}, _From, State) ->
    case get_consumer_by_id_from_storage(ConsumerId) of
        {ok, Consumer} ->
            {reply, {ok, Consumer}, State};
        {error, not_found} ->
            {reply, {error, not_found}, State}
    end;

handle_call({get_consumer_by_key, ApiKey}, _From, State) ->
    case ets:lookup(api_keys, ApiKey) of
        [{ApiKey, ConsumerId}] ->
            case get_consumer_by_id_from_storage(ConsumerId) of
                {ok, Consumer} ->
                    {reply, {ok, Consumer}, State};
                {error, not_found} ->
                    {reply, {error, not_found}, State}
            end;
        [] ->
            {reply, {error, not_found}, State}
    end;

handle_call({revoke_token, Token}, _From, State) ->
    case ets:lookup(tokens, Token) of
        [{Token, _, _, _}] ->
            true = ets:delete(tokens, Token),
            {reply, ok, State};
        [] ->
            {reply, {error, not_found}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(cleanup_expired_tokens, State) ->
    %% Clean up expired tokens
    Now = erlang:system_time(millisecond),

    %% Find expired tokens
    ExpiredTokens = ets:foldl(fun({Token, _, Expires, _}, Acc) ->
        if
            Expires < Now ->
                [Token | Acc];
            true ->
                Acc
        end
    end, [], tokens),

    %% Delete expired tokens
    lists:foreach(fun(Token) ->
        true = ets:delete(tokens, Token)
    end, ExpiredTokens),

    %% Schedule next cleanup
    erlang:send_after(3600000, self(), cleanup_expired_tokens),

    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

load_auth_config() ->
    %% Load authentication configuration
    #{
        token_ttl => 3600000, %% 1 hour
        token_refresh => true,
        hash_algorithm => sha256,
        iterations => 10000,
        key_length => 32
    }.

load_algorithms() ->
    %% Available authentication algorithms
    [hs256, hs384, hs512, rs256].

get_secret_key() ->
    %% Get or generate secret key
    case application:get_env(erlmcp_api_gateway, secret_key) of
        undefined ->
            Secret = crypto:strong_rand_bytes(32),
            application:set_env(erlmcp_api_gateway, secret_key, Secret),
            Secret;
        {ok, Key} ->
            Key
    end.

generate_consumer_id() ->
    %% Generate unique consumer ID
    binary_to_list(base64:encode(crypto:strong_rand_bytes(16))).

generate_api_key() ->
    %% Generate API key
    <<"erlmcp_", (binary:replace(base64:encode(crypto:strong_rand_bytes(32)), <<"/">>, <<"-">>))/binary>>.

generate_token_for_consumer(ConsumerId, State) ->
    %% Generate JWT-like token
    Now = erlang:system_time(millisecond),
    Expires = Now + maps:get(token_ttl, State#{token_ttl => 3600000}),

    Token = generate_token(ConsumerId, Expires, State),

    true = ets:insert(tokens, {Token, ConsumerId, Expires, Now}),

    {ok, Token}.

generate_token(ConsumerId, Expires, State) ->
    %% Create token payload
    Payload = #{
        consumer_id => ConsumerId,
        iat => erlang:system_time(millisecond),
        exp => Expires,
        jti => generate_id()
    },

    %% Encode payload
    PayloadJson = jsx:encode(Payload),

    %% Sign with secret key
    SecretKey = maps:get(secret_key, State),
    Token = base64:encode(PayloadJson) ++ "." ++ base64:encode(crypto:hash(sha256, PayloadJson ++ SecretKey)),

    Token.

verify_credentials(Credentials, Consumer) ->
    %% Verify credentials based on auth method
    case maps:get(auth_method, Credentials, api_key) of
        api_key ->
            %% API key authentication
            ApiKey = maps:get(api_key, Credentials),
            case ets:lookup(api_keys, ApiKey) of
                [{ApiKey, ConsumerId}] ->
                    ConsumerId == Consumer#consumer.id;
                _ ->
                    false
            end;
        basic ->
            %% Basic authentication
            Username = maps:get(username, Credentials),
            Password = maps:get(password, Credentials),

            StoredUsername = maps:get(username, Consumer),
            StoredPassword = maps:get(password, Consumer),

            case Username == StoredUsername of
                true ->
                    verify_password(Password, StoredPassword);
                false ->
                    false
            end;
        oauth2 ->
            %% OAuth2 authentication
            AccessToken = maps:get(access_token, Credentials),
            validate_oauth_token(AccessToken);
        _ ->
            false
    end.

verify_password(Password, StoredHash) ->
    %% Verify password hash
    case crypto:hash(sha256, Password) == StoredHash of
        true ->
            true;
        false ->
            false
    end.

validate_oauth_token(AccessToken) ->
    %% Validate OAuth2 token
    %% This would typically involve calling OAuth provider
    false. %% Simplified for demonstration

check_permissions(Consumer, Resource, Action) ->
    %% Check if consumer has permission for resource/action
    Permissions = maps:get(permissions, Consumer, []),
    lists:member({Resource, Action}, Permissions).

validate_consumer_data(ConsumerData) ->
    %% Validate consumer data
    RequiredFields = [name, email],

    case check_required_fields(RequiredFields, ConsumerData) of
        ok ->
            case validate_email(maps:get(email, ConsumerData)) of
                true ->
                    ok;
                false ->
                    {error, invalid_email}
            end;
        {error, Field} ->
            {error, {missing_field, Field}}
    end.

check_required_fields(Fields, Map) ->
    check_required_fields(Fields, Map, ok).

check_required_fields([], _Map, Result) ->
    Result;

check_required_fields([Field|Rest], Map, Result) ->
    case Result of
        ok ->
            case maps:is_key(Field, Map) of
                true ->
                    check_required_fields(Rest, Map, ok);
                false ->
                    check_required_fields(Rest, Map, {error, Field})
            end;
        _ ->
            check_required_fields(Rest, Map, Result)
    end.

validate_email(Email) when is_binary(Email) ->
    %% Simple email validation
    case re:run(Email, "^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$") of
        match ->
            true;
        nomatch ->
            false
    end;

validate_email(_) ->
    false.

get_consumer_by_id_from_storage(ConsumerId) ->
    case ets:lookup(consumers, ConsumerId) of
        [{ConsumerId, Consumer}] ->
            {ok, Consumer};
        [] ->
            {error, not_found}
    end.

log_auth_attempt(ConsumerId, Result) ->
    %% Log authentication attempt
    LogEntry = #{
        consumer_id => ConsumerId,
        timestamp => erlang:system_time(millisecond),
        result => Result,
        ip => get_remote_ip()
    },
    true = ets:insert(auth_logs, {generate_id(), LogEntry}).

get_remote_ip() ->
    %% Get remote IP address
    {ok, {_Address, Port}} = inet:peername(whereis(?MODULE)),
    Address.