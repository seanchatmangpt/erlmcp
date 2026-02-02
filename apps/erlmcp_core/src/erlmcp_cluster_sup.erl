-module(erlmcp_cluster_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

%%====================================================================
%% Cluster Management Supervisor
%%
%% Manages distributed cluster components:
%% - Node monitor: Tracks node up/down events
%% - Heartbeat: Periodic connectivity checks
%% - Split-brain detector: Detects and resolves network partitions
%%
%% Strategy: one_for_one - each component fails independently
%% Intensity: 5 restarts per 60 seconds (OTP standard)
%%
%% Zombie Prevention FIX v1.0.0:
%% - Returns {ok, Pid} unconditionally to prevent zombie processes
%% - Dynamically adjusts child list based on cluster_enabled flag
%% - Always starts supervisor with proper intensity tracking
%% - Empty child list when clustering disabled prevents crashes
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    process_flag(trap_exit, true),

    %% Read cluster configuration
    ClusterEnabled = application:get_env(erlmcp_core, cluster_enabled, false),

    SupFlags =
        #{strategy => one_for_one,
          intensity => 5,
          period => 60},

    %% Dynamically build child specs based on cluster_enabled flag
    ChildSpecs =
        case ClusterEnabled of
            true ->
                logger:info("Starting cluster supervisor with distributed components"),
                [%% Distributed registry (global gproc)
                 #{id => erlmcp_registry_dist,
                   start => {erlmcp_registry_dist, start_link, []},
                   restart => permanent,
                   shutdown => 5000,
                   type => worker,
                   modules => [erlmcp_registry_dist]},
                 %% Node monitor (tracks nodeup/nodedown)
                 #{id => erlmcp_node_monitor,
                   start => {erlmcp_node_monitor, start_link, []},
                   restart => permanent,
                   shutdown => 5000,
                   type => worker,
                   modules => [erlmcp_node_monitor]},
                 %% Split-brain detector
                 #{id => erlmcp_split_brain_detector,
                   start => {erlmcp_split_brain_detector, start_link, []},
                   restart => permanent,
                   shutdown => 5000,
                   type => worker,
                   modules => [erlmcp_split_brain_detector]},
                 %% OTP 26-28 Cluster management
                 #{id => erlmcp_cluster,
                   start => {erlmcp_cluster, start_link, []},
                   restart => permanent,
                   shutdown => 5000,
                   type => worker,
                   modules => [erlmcp_cluster]},
                 %% Session affinity for distributed routing
                 #{id => erlmcp_session_affinity,
                   start => {erlmcp_session_affinity, start_link, []},
                   restart => permanent,
                   shutdown => 5000,
                   type => worker,
                   modules => [erlmcp_session_affinity]},
                 %% Distributed tracing for cluster correlation
                 #{id => erlmcp_distributed_tracer,
                   start => {erlmcp_distributed_tracer, start_link, []},
                   restart => permanent,
                   shutdown => 5000,
                   type => worker,
                   modules => [erlmcp_distributed_tracer]},
                 %% Cluster health monitoring
                 #{id => erlmcp_cluster_monitor,
                   start => {erlmcp_cluster_monitor, start_link, []},
                   restart => permanent,
                   shutdown => 5000,
                   type => worker,
                   modules => [erlmcp_cluster_monitor]}];
            false ->
                logger:info("Starting cluster supervisor in local mode (no distributed components)"),
                %% Empty child list when clustering disabled
                []
        end,

    {ok, {SupFlags, ChildSpecs}}.
