%% Worker for supervisor restart benchmarks

-module(bench_restart_worker).
-export([start_link/0, init/1, terminate/2]).

start_link() ->
    proc_lib:start_link(?MODULE, init, []).

init([]) ->
    {ok, state}.

terminate(_Reason, _State) ->
    %% Clean up resources
    ok.