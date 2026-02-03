-module(erlmcp_api_gateway_auth).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([authenticate/2, authorize/3, generate_token/2, validate_token/1, revoke_token/1]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #{}}.

authenticate(Req, AuthHeader) ->
    gen_server:call(?MODULE, {authenticate, Req, AuthHeader}).

authorize(ApiId, ConsumerId, Token) ->
    gen_server:call(?MODULE, {authorize, ApiId, ConsumerId, Token}).

generate_consumer(ConsumerSpec) ->
    gen_server:call(?MODULE, {generate_consumer, ConsumerSpec}).

validate_token(Token) ->
    gen_server:call(?MODULE, {validate_token, Token}).

revoke_token(Token) ->
    gen_server:call(?MODULE, {revoke_token, Token}).

handle_call({authenticate, _Req, AuthHeader}, _From, State) ->
    case AuthHeader of
        <<"Bearer ", Token/binary>> ->
            case validate_token(Token) of
                {ok, Claims} ->
                    {reply, {ok, Claims}, State};
                {error, _} ->
                    {reply, {error, unauthorized}, State}
            end;
        _ ->
            {reply, {error, invalid_auth_scheme}, State}
    end;

handle_call({authorize, ApiId, ConsumerId, Token}, _From, State) ->
    case validate_token(Token) of
        {ok, Claims} ->
            case maps:get(<<"consumer_id">>, Claims) =:= ConsumerId andalso
                 maps:get(<<"api_id">>, Claims) =:= ApiId andalso
                 maps:get(<<"exp">>, Claims) > erlang:system_time(second) of
                true ->
                    {reply, {ok, authorized}, State};
                false ->
                    {reply, {error, forbidden}, State}
            end;
        {error, _} ->
            {reply, {error, invalid_token}, State}
    end;

handle_call({generate_consumer, ConsumerSpec}, _From, State) ->
    ConsumerId = uuid:uuid_to_list(uuid:uuid4()),
    ApiKey = generate_api_key(),
    Secret = generate_secret(),

    Consumer = #{
        id => ConsumerId,
        api_key => ApiKey,
        secret => Secret,
        custom_id => maps:get(<<"custom_id">>, ConsumerSpec, <<>>),
        tags => maps:get(<<"tags">>, ConsumerSpec, []),
        created_at => erlang:system_time(millisecond)
    },

    {reply, {ok, Consumer}, State};

handle_call({validate_token, Token}, _From, State) ->
    case jwt:decode(Token) of
        {ok, Claims} ->
            case maps:get(<<"exp">>, Claims) > erlang:system_time(second) of
                true ->
                    {reply, {ok, Claims}, State};
                false ->
                    {reply, {error, expired}, State}
            end;
        {error, _} ->
            {reply, {error, invalid}, State}
    end;

handle_call({revoke_token, Token}, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

generate_api_key() ->
    crypto:strong_rand_bytes(32).

generate_secret() ->
    crypto:strong_rand_bytes(64).