-module(erlmcp_api_gateway_rate_limiter).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([check_rate/3, reset_rate/2, get_stats/1]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    erlang:send_after(1000, self(), cleanup),
    {ok, #{}}.

check_rate(ApiId, ConsumerId, Window) ->
    gen_server:call(?MODULE, {check_rate, ApiId, ConsumerId, Window}).

reset_rate(ApiId, ConsumerId) ->
    gen_server:call(?MODULE, {reset_rate, ApiId, ConsumerId}).

get_stats(ApiId) ->
    gen_server:call(?MODULE, {get_stats, ApiId}).

handle_call({check_rate, ApiId, ConsumerId, Window}, _From, State) ->
    Now = erlang:system_time(millisecond),
    Key = rate_key(ApiId, ConsumerId),

    case redis_command([<<"hget">>, Key, <<"tokens">>]) of
        {ok, Tokens} when is_binary(Tokens) ->
            TokensInt = binary_to_integer(Tokens),
            case TokensInt > 0 of
                true ->
                    NewTokens = TokensInt - 1,
                    redis_command([<<"hset">>, Key, <<"tokens">>, integer_to_binary(NewTokens)]),
                    {reply, {ok, allowed}, State};
                false ->
                    {reply, {error, rate_limited}, State}
            end;
        {ok, undefined} ->
            RateLimit = get_rate_limit(ApiId, ConsumerId),
            MaxRequests = maps:get(max_requests, RateLimit, 1000),
            redis_command([<<"hset">>, Key, <<"tokens>>, integer_to_binary(MaxRequests)]),
            {reply, {ok, allowed}, State};
        {error, _} ->
            {reply, {error, redis_error}, State}
    end.

handle_call({reset_rate, ApiId, ConsumerId}, _From, State) ->
    Key = rate_key(ApiId, ConsumerId),
    redis_command([<<"del">>, Key]),
    {reply, ok, State}.

handle_call({get_stats, ApiId}, _From, State) ->
    Stats = calculate_stats(ApiId),
    {reply, {ok, Stats}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(cleanup, State) ->
    Now = erlang:system_time(millisecond),
    Keys = redis_command([<<"keys">>, <<"rate:*">>]),
    case Keys of
        {ok, AllKeys} when is_list(AllKeys) ->
            CleanKeys = lists:filter(fun(K) ->
                {ok, Timestamp} = redis_command([<<"hget">>, K, <<"timestamp">>]),
                if
                    is_binary(Timestamp) ->
                        TS = binary_to_integer(Timestamp),
                        Now - TS > 60000;
                    true ->
                        true
                end
            end, AllKeys),
            lists:foreach(fun(K) ->
                redis_command([<<"del">>, K])
            end, CleanKeys);
        _ ->
            ok
    end,
    erlang:send_after(60000, self(), cleanup),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

rate_key(ApiId, ConsumerId) ->
    list_to_binary(["rate:", ApiId, ":", ConsumerId]).

get_rate_limit(ApiId, ConsumerId) ->
    case redis_command([<<"hget">>, <<"rate_limits">>, ApiId]) of
        {ok, undefined} ->
            #{max_requests => 1000, window_ms => 60000};
        {ok, Config} ->
            jsx:decode(Config, [{return_maps, true}])
    end.

calculate_stats(ApiId) ->
    case redis_command([<<"keys">>, <<"rate:", ApiId, ":*">>]) of
        {ok, Keys} when is_list(Keys) ->
            #{active_consumers => length(Keys),
              timestamp => erlang:system_time(millisecond)};
        _ ->
            #{active_consumers => 0,
              timestamp => erlang:system_time(millisecond)}
    end.

redis_command(Command) ->
    erlmcp_api_gateway_redis:execute(Command).