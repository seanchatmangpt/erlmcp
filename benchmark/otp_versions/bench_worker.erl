%% Simple worker process for supervisor benchmarks

-module(bench_worker).
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2]).

start_link() ->
    proc_lib:start_link(?MODULE, init, []).

init([]) ->
    {ok, state}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.