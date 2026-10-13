%%%-------------------------------------------------------------------
%%% @doc PQChain Mempool Supervisor
%%%
%%% Supervisor for mempool management processes.
%%% Currently uses one_for_one with a single mempool worker.
%%% Can be extended to simple_one_for_one for per-sender partitions.
%%%
%%% Architecture:
%%% - Uses one_for_one for single mempool worker
%%% - Can evolve to simple_one_for_one for sharded mempool
%%% - Mempool worker is permanent - always restarted
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(pqc_mempool_sup).

-behaviour(supervisor).

-include("pqchain.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% Restart intensity limits
-define(MAX_RESTARTS, 10).
-define(RESTART_PERIOD, 60).  % seconds

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the mempool supervisor
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor Callbacks
%%====================================================================

%% @private
%% @doc Initialize the supervisor with one_for_one strategy
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => ?MAX_RESTARTS,
        period => ?RESTART_PERIOD
    },

    %% Main mempool worker
    %% Manages pending transactions, priority ordering, gas tracking
    Mempool = #{
        id => pqc_mempool,
        start => {pqc_mempool, start_link, []},
        restart => permanent,
        shutdown => 10000,  % Allow time to persist pending txs
        type => worker,
        modules => [pqc_mempool]
    },

    {ok, {SupFlags, [Mempool]}}.
