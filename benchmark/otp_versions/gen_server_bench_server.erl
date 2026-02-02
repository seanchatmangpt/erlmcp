%% Benchmark gen_server implementation
%% Used for testing process creation overhead

-module(gen_server_bench_server).
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2]).
-behaviour(gen_server).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    %% Initialize with some memory allocation to simulate real usage
    _ = lists:map(fun(_) -> crypto:strong_rand_bytes(1024) end, lists:seq(1, 10)),
    {ok, state}.

handle_call(_Request, _From, State) ->
    %% Simulate some work
    _ = crypto:strong_rand_bytes(512),
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    %% Simulate some work
    _ = crypto:strong_rand_bytes(256),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.