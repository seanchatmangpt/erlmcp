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
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    %% Only start if clustering is enabled
    ClusterEnabled = application:get_env(erlmcp_core, cluster_enabled, false),

    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 60
    },

    ChildSpecs = case ClusterEnabled of
        true ->
            [
                %% Distributed registry (global gproc)
                #{
                    id => erlmcp_registry_dist,
                    start => {erlmcp_registry_dist, start_link, []},
                    restart => permanent,
                    shutdown => 5000,
                    type => worker,
                    modules => [erlmcp_registry_dist]
                },

                %% Node monitor (tracks nodeup/nodedown)
                #{
                    id => erlmcp_node_monitor,
                    start => {erlmcp_node_monitor, start_link, []},
                    restart => permanent,
                    shutdown => 5000,
                    type => worker,
                    modules => [erlmcp_node_monitor]
                },

                %% Split-brain detector
                #{
                    id => erlmcp_split_brain_detector,
                    start => {erlmcp_split_brain_detector, start_link, []},
                    restart => permanent,
                    shutdown => 5000,
                    type => worker,
                    modules => [erlmcp_split_brain_detector]
                }
            ];
        false ->
            %% No cluster components when disabled
            []
    end,

    {ok, {SupFlags, ChildSpecs}}.
