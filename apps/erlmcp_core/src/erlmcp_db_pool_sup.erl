%%%-------------------------------------------------------------------
%%% @doc
%%% Database Pool Supervisor
%%%
%%% Top-level supervisor for database connection pools.
%%% Manages the pool manager and all child pool workers.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_db_pool_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

%%====================================================================
%%% API functions
%%====================================================================

%% @doc Start the database pool supervisor
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%====================================================================
%%% Supervisor callbacks
%%====================================================================

%% @private
-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags =
        #{strategy => one_for_one,
          intensity => 10,
          period => 60},

    ChildSpecs =
        [#{id => erlmcp_db_pool,
           start => {erlmcp_db_pool, start_link, []},
           restart => permanent,
           shutdown => 5000,
           type => worker,
           modules => [erlmcp_db_pool]}],

    {ok, {SupFlags, ChildSpecs}}.
