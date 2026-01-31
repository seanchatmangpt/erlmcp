%%%====================================================================
%%% @doc Chaos Worker Supervisor
%%%
%%% Supervises chaos experiment workers using simple_one_for_one strategy.
%%% Each chaos experiment gets its own supervised worker process.
%%%
%%% Supervision Strategy: simple_one_for_one
%%% - Transient workers (stop after experiment completes)
%%% - One worker per chaos experiment
%%% - Automatic cleanup after completion
%%%
%%% @end
%%%====================================================================
-module(erlmcp_chaos_worker_sup).
-behaviour(supervisor).

%% API
-export([start_link/0, start_worker/4]).

%% Supervisor callbacks
-export([init/1]).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Start a chaos experiment worker under supervision
-spec start_worker(pid(), term(), atom(), map()) -> {ok, pid()} | {error, term()}.
start_worker(Parent, ExperimentId, Type, Config) ->
    supervisor:start_child(?MODULE, [Parent, ExperimentId, Type, Config]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 10,
        period => 60
    },

    ChildSpec = #{
        id => erlmcp_chaos_worker,
        start => {erlmcp_chaos_worker, start_link, []},
        restart => transient,  % Don't restart after normal completion
        shutdown => 5000,
        type => worker,
        modules => [erlmcp_chaos_worker]
    },

    {ok, {SupFlags, [ChildSpec]}}.
