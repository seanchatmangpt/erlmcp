%%%-------------------------------------------------------------------
%%% @doc erlmcp_temp_worker_sup - Temporary Worker Supervisor
%%%
%%% Simple one_for_one supervisor for temporary worker processes.
%%% Used by performance validator to safely spawn supervised workers.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_temp_worker_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Start the supervisor
-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60
    },
    {ok, {SupFlags, []}}.
