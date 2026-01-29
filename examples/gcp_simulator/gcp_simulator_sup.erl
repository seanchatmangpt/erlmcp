-module(gcp_simulator_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Starts the supervisor
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
%% @doc Initialize the supervisor with one_for_one strategy
%%
%% Strategy: one_for_one
%% Rationale: If gcp_simulator_server fails, only that child should restart.
%% This is the appropriate strategy for independent workers where one
%% child's failure shouldn't affect others.
-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60
    },

    ChildSpecs = [
        #{
            id => gcp_simulator_server,
            start => {gcp_simulator_server, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [gcp_simulator_server]
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.
