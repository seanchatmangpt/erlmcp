%%%-------------------------------------------------------------------
%%% @doc Benchmark Pool Worker
%%%
%%% Simple worker for connection pool benchmarking.
%%% Maintains a mock connection and responds to pings.
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_bench_pool_worker).
-behaviour(gen_server).

%% poolboy worker API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%====================================================================
%% API
%%====================================================================

start_link(WorkerArgs) ->
    gen_server:start_link(?MODULE, WorkerArgs, []).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

init(WorkerArgs) ->
    process_flag(trap_exit, true),
    {ok, #{args => WorkerArgs}}.

handle_call(ping, _From, State) ->
    {reply, pong, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
