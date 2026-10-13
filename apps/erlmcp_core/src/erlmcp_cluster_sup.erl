%%%-------------------------------------------------------------------
%%% @doc erlmcp_cluster_sup - Cluster Supervisor
%%%
%%% Supervises all cluster-related processes:
%%% - Cluster discovery
%%% - Cluster membership
%%% - Cluster RPC layer
%%% - Distributed locks
%%% - Cluster health monitor
%%% - Cluster shutdown coordinator
%%% - Existing cluster and distributed registry
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cluster_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%====================================================================
%% supervisor callbacks
%%====================================================================

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 60
    },

    % Get configuration
    Nodes = application:get_env(erlmcp_core, cluster_nodes, []),
    DiscoveryMode = application:get_env(erlmcp_core, discovery_mode, epmd),
    HeartbeatInterval = application:get_env(erlmcp_core, cluster_heartbeat_interval, 5000),
    SyncInterval = application:get_env(erlmcp_core, cluster_sync_interval, 10000),
    DiscoveryInterval = application:get_env(erlmcp_core, discovery_interval, 30000),
    CleanupInterval = application:get_env(erlmcp_core, health_cleanup_interval, 60000),

    ChildSpecs = [
        %% ================================================================
        %% CLUSTER DISCOVERY: Node discovery via EPMD/static/DNS
        %% ================================================================
        #{
            id => erlmcp_cluster_discovery,
            start => {erlmcp_cluster_discovery, start_link, [
                #{
                    discovery_mode => DiscoveryMode,
                    static_nodes => Nodes,
                    discovery_interval => DiscoveryInterval,
                    name_prefix => application:get_env(erlmcp_core, cluster_name_prefix, "erlmcp"),
                    dns_domain => application:get_env(erlmcp_core, cluster_dns_domain, undefined)
                }
            ]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_cluster_discovery]
        },

        %% ================================================================
        %% CLUSTER MEMBERSHIP: Gossip-based membership protocol
        %% ================================================================
        #{
            id => erlmcp_cluster_membership,
            start => {erlmcp_cluster_membership, start_link, [
                #{
                    heartbeat_interval => HeartbeatInterval,
                    failure_timeout => HeartbeatInterval * 3,
                    suspect_timeout => HeartbeatInterval * 6,
                    sync_interval => SyncInterval
                }
            ]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_cluster_membership]
        },

        %% ================================================================
        %% CLUSTER RPC: Inter-node communication layer
        %% ================================================================
        #{
            id => erlmcp_cluster_rpc,
            start => {erlmcp_cluster_rpc, start_link, [
                #{
                    timeout => 10000,
                    cleanup_interval => 60000
                }
            ]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_cluster_rpc]
        },

        %% ================================================================
        %% DISTRIBUTED LOCKS: Global locking service
        %% ================================================================
        #{
            id => erlmcp_distributed_lock,
            start => {erlmcp_distributed_lock, start_link, [
                #{
                    default_timeout => 30000,
                    cleanup_interval => 30000
                }
            ]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_distributed_lock]
        },

        %% ================================================================
        %% CLUSTER HEALTH: Health monitoring and alerts
        %% ================================================================
        #{
            id => erlmcp_cluster_health,
            start => {erlmcp_cluster_health, start_link, [
                #{
                    check_interval => CleanupInterval,
                    history_size => 100,
                    cpu_threshold => 80,
                    memory_threshold => 80,
                    process_threshold => 100000
                }
            ]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_cluster_health]
        },

        %% ================================================================
        %% CLUSTER SHUTDOWN: Graceful shutdown coordinator
        %% ================================================================
        #{
            id => erlmcp_cluster_shutdown,
            start => {erlmcp_cluster_shutdown, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_cluster_shutdown]
        },

        %% ================================================================
        %% DISTRIBUTED SESSION: Session replication
        %% ================================================================
        #{
            id => erlmcp_distributed_session,
            start => {erlmcp_distributed_session, start_link, [
                #{
                    replication_factor => application:get_env(erlmcp_core, replication_factor, 2),
                    sync_interval => 5000
                }
            ]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_distributed_session]
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.
