%%%-------------------------------------------------------------------
%%% @doc Test Pool Worker for Chicago School TDD
%%%
%%% Real erlmcp process for pool manager testing
%%% Follows the pool worker contract: start_link/1 -> {ok, Pid}
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_test_pool_worker).
-behaviour(gen_server).

%% Pool worker API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    owner :: pid()
}).

%%====================================================================
%% Pool Worker API
%%====================================================================

%% @doc Start pool worker
%% Required by pool manager contract
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) when is_map(Opts) ->
    Owner = maps:get(owner, Opts, self()),
    gen_server:start_link(?MODULE, [Owner], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Owner]) when is_pid(Owner) ->
    %% Monitor owner for cleanup
    monitor(process, Owner),
    {ok, #state{owner = Owner}}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, process, Owner, _Reason}, #state{owner = Owner} = State) ->
    %% Owner died, terminate
    {stop, normal, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
