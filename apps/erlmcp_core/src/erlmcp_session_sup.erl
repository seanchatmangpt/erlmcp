-module(erlmcp_session_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

%%====================================================================
%% Session Supervisor
%%
%% Supervises session management infrastructure:
%% - Session manager
%% - Session replication
%% - Session failover
%% - Failover worker supervisor
%% - Cluster supervisor (conditional)
%%
%% Strategy: one_for_one - independent session components
%% ====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 60
    },

    %% Build child specs dynamically based on configuration
    ClusterEnabled = application:get_env(erlmcp_core, cluster_enabled, false),

    BaseChildSpecs = [
        %% ================================================================
        %% SESSION MANAGER: Core session lifecycle management
        %% ================================================================
        #{
            id => erlmcp_session_manager,
            start => {erlmcp_session_manager, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_session_manager]
        },

        %% ================================================================
        %% SESSION REPLICATOR: Replicate sessions across cluster nodes
        %% ================================================================
        #{
            id => erlmcp_session_replicator,
            start => {erlmcp_session_replicator, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_session_replicator]
        },

        %% ================================================================
        %% SESSION FAILOVER: Handle node failures and session migration
        %% ================================================================
        #{
            id => erlmcp_session_failover,
            start => {erlmcp_session_failover, start_link, [node()]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_session_failover]
        },

        %% ================================================================
        %% FAILOVER WORKER SUPERVISOR: Supervises async failover operations
        %% ================================================================
        #{
            id => erlmcp_failover_worker_sup,
            start => {erlmcp_failover_worker_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [erlmcp_failover_worker_sup]
        }
    ],

    %% Conditionally include cluster supervisor
    ChildSpecs = case ClusterEnabled of
        true ->
            [
                %% ================================================================
                %% CLUSTER: Distributed registry and cluster management
                %% ================================================================
                #{
                    id => erlmcp_cluster_sup,
                    start => {erlmcp_cluster_sup, start_link, []},
                    restart => permanent,
                    shutdown => 5000,
                    type => supervisor,
                    modules => [erlmcp_cluster_sup]
                }
                | BaseChildSpecs
            ];
        false ->
            %% Skip cluster supervisor when disabled to prevent zombie process
            BaseChildSpecs
    end,

    {ok, {SupFlags, ChildSpecs}}.
