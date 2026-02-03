-module(erlmcp_api_gateway_redis_client).
-behaviour(gen_server).

-export([start_link/0, execute/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

execute(Cmd, Args) ->
    gen_server:call(?MODULE, {execute, Cmd, Args}).

init([]) ->
    {ok, #{}}.

handle_call({execute, Cmd, Args}, _From, State) ->
    try
        Result = redix:command(Cmd, Args),
        {reply, {ok, Result}, State}
    catch
        _:Reason -> {reply, {error, Reason}, State}
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.