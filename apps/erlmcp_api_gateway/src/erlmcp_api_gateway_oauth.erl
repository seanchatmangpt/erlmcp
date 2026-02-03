-module(erlmcp_api_gateway_oauth).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([
    authorize/3, token/2, introspect/1, revoke/1,
    create_client/1, validate_client/2
]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #{clients => #{}, tokens => #{}}}.

authorize(ClientId, RedirectUri, Scope) ->
    gen_server:call(?MODULE, {authorize, ClientId, RedirectUri, Scope}).

token(ClientId, ClientSecret) ->
    gen_server:call(?MODULE, {token, ClientId, ClientSecret}).

introspect(Token) ->
    gen_server:call(?MODULE, {introspect, Token}).

revoke(Token) ->
    gen_server:call(?MODULE, {revoke, Token}).

create_client(ClientSpec) ->
    gen_server:call(?MODULE, {create_client, ClientSpec}).

validate_client(ClientId, ClientSecret) ->
    gen_server:call(?MODULE, {validate_client, ClientId, ClientSecret}).

handle_call({authorize, ClientId, RedirectUri, Scope}, _From, State) ->
    case validate_client(ClientId, <<>>) of
        {ok, Client} ->
            AuthCode = generate_auth_code(),
            Tokens = State#{tokens},
            NewTokens = maps:put(AuthCode, #{
                client_id => ClientId,
                redirect_uri => RedirectUri,
                scope => Scope,
                created_at => erlang:system_time(millisecond)
            }, Tokens),
            {reply, {ok, AuthCode}, State#{tokens => NewTokens}};
        {error, _} ->
            {reply, {error, invalid_client}, State}
    end;

handle_call({token, ClientId, ClientSecret}, _From, State) ->
    case validate_client(ClientId, ClientSecret) of
        {ok, Client} ->
            AccessToken = generate_access_token(),
            RefreshToken = generate_refresh_token(),
            Tokens = State#{tokens},
            NewTokens = maps:put(AccessToken, #{
                client_id => ClientId,
                type => access,
                created_at => erlang:system_time(millisecond),
                expires_in => 3600
            }, Tokens),
            NewTokens2 = maps:put(RefreshToken, #{
                client_id => ClientId,
                type => refresh,
                created_at => erlang:system_time(millisecond)
            }, NewTokens),
            {reply, {ok, #{
                access_token => AccessToken,
                refresh_token => RefreshToken,
                token_type => <<"Bearer">>,
                expires_in => 3600
            }}, State#{tokens => NewTokens2}};
        {error, _} ->
            {reply, {error, invalid_client}, State}
    end;

handle_call({introspect, Token}, _From, State) ->
    case maps:find(Token, State#{tokens}) of
        {ok, TokenData} ->
            {reply, {ok, TokenData}, State};
        error ->
            {reply, {error, invalid_token}, State}
    end;

handle_call({revoke, Token}, _From, State) ->
    NewTokens = maps:remove(Token, State#{tokens}),
    {reply, ok, State#{tokens => NewTokens}};

handle_call({create_client, ClientSpec}, _From, State) ->
    ClientId = generate_client_id(),
    ClientSecret = generate_client_secret(),
    Client = #{
        id => ClientId,
        secret => ClientSecret,
        redirect_uris => maps:get(<<"redirect_uris">>, ClientSpec, []),
        scope => maps:get(<<"scope">>, ClientSpec, <<>>),
        created_at => erlang:system_time(millisecond)
    },
    Clients = State#{clients},
    NewClients = maps:put(ClientId, Client, Clients),
    {reply, {ok, Client}, State#{clients => NewClients}};

handle_call({validate_client, ClientId, ClientSecret}, _From, State) ->
    case maps:find(ClientId, State#{clients}) of
        {ok, Client} ->
            case maps:get(secret, Client) =:= ClientSecret of
                true ->
                    {ok, Client};
                false ->
                    {error, invalid_client}
            end;
        error ->
            {error, invalid_client}
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

generate_auth_code() ->
    crypto:strong_rand_bytes(32).

generate_access_token() ->
    crypto:strong_rand_bytes(32).

generate_refresh_token() ->
    crypto:strong_rand_bytes(32).

generate_client_id() ->
    crypto:strong_rand_bytes(16).

generate_client_secret() ->
    crypto:strong_rand_bytes(32).