%%%====================================================================
%%% @doc Cache Warmer Supervisor
%%%
%%% Supervises cache warming workers using simple_one_for_one strategy.
%%% Each warming operation gets its own supervised worker process.
%%%
%%% Supervision Strategy: simple_one_for_one
%%% - Transient workers (stop after completing work)
%%% - One worker per cache warming task
%%% - Automatic cleanup after completion
%%%
%%% @end
%%%====================================================================
-module(erlmcp_cache_warmer_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_warmer/3]).
%% Supervisor callbacks
-export([init/1]).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Start a cache warming worker under supervision
-spec start_warmer(term(), fun(() -> term()), pos_integer()) -> {ok, pid()} | {error, term()}.
start_warmer(Key, ValueFun, TTLSeconds) ->
    supervisor:start_child(?MODULE, [Key, ValueFun, TTLSeconds]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    SupFlags =
        #{strategy => simple_one_for_one,
          intensity => 10,
          period => 60},

    ChildSpec =
        #{id => erlmcp_cache_warmer,
          start => {erlmcp_cache_warmer, start_link, []},
          restart => transient,  % Don't restart after normal completion
          shutdown => 5000,
          type => worker,
          modules => [erlmcp_cache_warmer]},

    {ok, {SupFlags, [ChildSpec]}}.
