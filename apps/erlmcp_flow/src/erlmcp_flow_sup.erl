%%%-------------------------------------------------------------------
%%% @doc erlmcp-flow Top-Level Supervisor (TIER 1)
%%% 3-Tier Supervision Tree: Root supervisor with one_for_all strategy
%%% Critical components restart together (registry + consensus + core)
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_flow_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor Callbacks
%%%===================================================================

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    %% ================================================================
    %% TIER 1: Root Supervisor (one_for_all)
    %% Strategy: All components depend on each other
    %% - Registry failure requires consensus restart
    %% - Consensus failure requires swarm reconfiguration
    %% Recovery Time: ~500-1000ms for full subsystem restart
    %% ================================================================
    SupFlags = #{
        strategy => one_for_all,    % Critical: registry + consensus + core restart together
        intensity => 3,              % Conservative: max 3 restarts
        period => 60                 % Within 60 seconds
    },

    ChildSpecs = [
        %% ================================================================
        %% REGISTRY: Agent routing and process discovery (gproc-based)
        %% Critical: Must start FIRST - all components depend on it
        %% Failure Impact: All agent lookups fail → triggers one_for_all restart
        %% Recovery: ~200ms registry initialization
        %% ================================================================
        #{id => erlmcp_flow_registry,
          start => {erlmcp_flow_registry, start_link, []},
          restart => permanent,      % Always restart
          shutdown => 5000,           % 5s graceful shutdown
          type => worker,
          modules => [erlmcp_flow_registry]},

        %% ================================================================
        %% RAFT CONSENSUS: Leader election and log replication (CFT)
        %% Critical: Starts SECOND - depends on registry for coordination
        %% Failure Impact: Leader election required → operations pause
        %% Recovery: ~150-300ms randomized election timeout
        %% ================================================================
        #{id => erlmcp_flow_raft,
          start => {erlmcp_flow_raft, start_link, [#{
              node_id => <<"flow_node_1">>,
              peers => [],
              election_timeout => 5000
          }]},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [erlmcp_flow_raft]},

        %% ================================================================
        %% CORE SUPERVISOR: Swarms, agents, and services (TIER 2)
        %% Strategy: one_for_one (isolated failures)
        %% Critical: Starts LAST - depends on registry + consensus
        %% Failure Impact: Individual subsystem failures (no cascade)
        %% Recovery: Per-component restart (swarm: <200ms, agent: <50ms)
        %% ================================================================
        #{id => erlmcp_flow_core_sup,
          start => {erlmcp_flow_core_sup, start_link, []},
          restart => permanent,
          shutdown => infinity,       % Supervisor: wait for all children
          type => supervisor,
          modules => [erlmcp_flow_core_sup]}
    ],

    {ok, {SupFlags, ChildSpecs}}.
