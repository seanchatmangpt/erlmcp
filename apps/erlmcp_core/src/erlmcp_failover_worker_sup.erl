%%%====================================================================
%%% @doc Failover Worker Supervisor
%%%
%%% Supervises session failover workers using simple_one_for_one strategy.
%%% Each failover operation (replication/notification) gets its own supervised worker.
%%%
%%% Supervision Strategy: simple_one_for_one
%%% - Transient workers (stop after completing work)
%%% - One worker per failover operation
%%% - Automatic cleanup after completion
%%%
%%% @end
%%%====================================================================
-module(erlmcp_failover_worker_sup).
-behaviour(supervisor).

%% API
-export([start_link/0, start_worker/1]).

%% Supervisor callbacks
-export([init/1]).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Start a failover worker under supervision
%% Work types:
%%   {replicate, SessionId, FailoverState, BackupNode}
%%   {notify, SessionId, NewPrimary, BackupNode}
-spec start_worker(term()) -> {ok, pid()} | {error, term()}.
start_worker(Work) ->
    supervisor:start_child(?MODULE, [Work]).

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
        id => erlmcp_failover_worker,
        start => {erlmcp_failover_worker, start_link, []},
        restart => transient,  % Don't restart after normal completion
        shutdown => 5000,
        type => worker,
        modules => [erlmcp_failover_worker]
    },

    {ok, {SupFlags, [ChildSpec]}}.
