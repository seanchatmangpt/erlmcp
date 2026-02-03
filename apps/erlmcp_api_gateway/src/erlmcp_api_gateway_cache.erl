-module(erlmcp_api_gateway_cache).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([
    get/1, set/2, delete/1, exists/1, clear/0,
    get_with_ttl/1, set_with_ttl/2, expire/2
]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #{cache => #{}, timers => #{}}}.

get(Key) ->
    gen_server:call(?MODULE, {get, Key}).

set(Key, Value) ->
    gen_server:call(?MODULE, {set, Key, Value}).

set_with_ttl(Key, {Value, TTL}) ->
    gen_server:call(?MODULE, {set_with_ttl, Key, Value, TTL}).

delete(Key) ->
    gen_server:call(?MODULE, {delete, Key}).

exists(Key) ->
    gen_server:call(?MODULE, {exists, Key}).

clear() ->
    gen_server:call(?MODULE, clear).

get_with_ttl(Key) ->
    gen_server:call(?MODULE, {get_with_ttl, Key}).

expire(Key, TTL) ->
    gen_server:call(?MODULE, {expire, Key, TTL}).

handle_call({get, Key}, _From, State) ->
    case maps:find(Key, State#{cache}) of
        {ok, Value} ->
            {reply, {ok, Value}, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({set, Key, Value}, _From, State) ->
    NewCache = maps:put(Key, Value, State#{cache}),
    {reply, ok, State#{cache => NewCache}};

handle_call({set_with_ttl, Key, Value, TTL}, _From, State) ->
    Now = erlang:system_time(millisecond),
    Expiry = Now + TTL,
    NewCache = maps:put(Key, {Value, Expiry}, State#{cache}),
    {reply, ok, State#{cache => NewCache}};

handle_call({get_with_ttl, Key}, _From, State) ->
    Now = erlang:system_time(millisecond),
    case maps:find(Key, State#{cache}) of
        {ok, {Value, Expiry}} when Expiry > Now ->
            {reply, {ok, {Value, Expiry - Now}}, State};
        {ok, {_Value, Expiry}} when Expiry =< Now ->
            {reply, {error, expired}, State#{cache => maps:remove(Key, State#{cache})}};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({delete, Key}, _From, State) ->
    NewCache = maps:remove(Key, State#{cache}),
    {reply, ok, State#{cache => NewCache}};

handle_call({exists, Key}, _From, State) ->
    case maps:is_key(Key, State#{cache}) of
        true ->
            {reply, true, State};
        false ->
            {reply, false, State}
    end;

handle_call(clear, _From, State) ->
    Timers = maps:get(timers, State),
    lists:foreach(fun erlang:cancel_timer/1, Timers),
    {reply, ok, #{cache => #{}, timers => #{}}}.

handle_cast({expire, Key, TTL}, State) ->
    Now = erlang:system_time(millisecond),
    Ref = erlang:send_after(TTL, self(), {expire_key, Key}),
    Timers = maps:put(Key, Ref, State#{timers}),
    {noreply, State#{cache => State#{cache}, timers => Timers}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({expire_key, Key}, State) ->
    NewCache = maps:remove(Key, State#{cache}),
    Timers = maps:remove(Key, State#{timers}),
    {noreply, State#{cache => NewCache, timers => Timers}}.

terminate(_Reason, State) ->
    Timers = maps:get(timers, State),
    lists:foreach(fun erlang:cancel_timer/1, Timers),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.