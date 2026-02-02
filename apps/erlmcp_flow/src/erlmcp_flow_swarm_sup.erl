%%%-------------------------------------------------------------------
%%% @doc erlmcp-flow Swarm Supervisor (TIER 3)
%%% Dynamic swarm coordinator instances (simple_one_for_one)
%%% Each swarm manages a group of agents for collaborative tasks
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_flow_swarm_sup).

-behaviour(supervisor).

-export([start_link/0, start_child/3]).
-export([init/1]).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Start a swarm coordinator with given configuration
-spec start_child(SwarmId, Topology, Config) -> {ok, pid()} | {error, term()}
    when SwarmId :: binary(),
         Topology :: mesh | hierarchical | ring | star,
         Config :: map().
start_child(SwarmId, Topology, Config) ->
    supervisor:start_child(?MODULE, [SwarmId, Topology, Config]).

%%%===================================================================
%%% Supervisor Callbacks
%%%===================================================================

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    %% ================================================================
    %% TIER 3: Swarm Supervisor (simple_one_for_one)
    %% Strategy: Dynamic worker pool for swarm coordinators
    %% - Each swarm coordinates multiple agents
    %% - Swarms are spawned on-demand per task/workflow
    %% - Restart: transient (restart on abnormal exit, not normal shutdown)
    %% ================================================================
    SupFlags = #{
        strategy => simple_one_for_one,  % Dynamic swarm instances
        intensity => 5,                   % Max 5 restarts per swarm
        period => 60                      % Within 60 seconds
    },

    %% Template child spec for swarm coordinator instances
    %% Args (SwarmId, Topology, Config) are appended by start_child/3
    ChildSpecs = [
        #{id => erlmcp_flow_swarm,
          start => {erlmcp_flow_swarm, start_link, []},
          restart => transient,  % Restart on abnormal exit (crash), not normal shutdown
          shutdown => 5000,       % 5s graceful shutdown (flush pending messages)
          type => worker,
          modules => [erlmcp_flow_swarm]}
    ],

    {ok, {SupFlags, ChildSpecs}}.
