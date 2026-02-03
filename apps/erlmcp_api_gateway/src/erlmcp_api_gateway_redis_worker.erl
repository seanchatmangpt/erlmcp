-module(erlmcp_api_gateway_redis_worker).
-behaviour(gen_server).

-export([start_link/0]).
-export([execute/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

execute(Command) ->
    gen_server:call(?MODULE, {execute, Command}).

init([]) ->
    case redix:start_link([{host, "localhost"}, {port, 6379}, {database, 0}]) of
        {ok, Pid} ->
            {ok, #{redis => Pid}};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call({execute, Command}, _From, State) ->
    RedisPid = maps:get(redis, State),
    case redix:command(RedisPid, Command) of
        {ok, Result} ->
            {reply, {ok, Result}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    RedisPid = maps:get(redis, State),
    redix:stop(RedisPid),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.