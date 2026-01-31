-module(erlmcp_core_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

%%====================================================================
%% v1.4.0: Core Infrastructure Supervisor (TIER 1)
%%
%% Consolidates registry and infrastructure into single core supervisor.
%% This is the foundation - no external dependencies.
%%
%% Strategy: one_for_one - each component fails independently
%% Impact: Individual service failures don't cascade
%% ====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags = #{
        strategy => one_for_one,  % Each core component fails independently
        intensity => 5,
        period => 60
    },

    %% Build child specs dynamically based on configuration
    ClusterEnabled = application:get_env(erlmcp_core, cluster_enabled, false),

    BaseChildSpecs = [
        %% ================================================================
        %% REGISTRY: Message routing using gproc
        %% ================================================================
        #{
            id => erlmcp_registry,
            start => {erlmcp_registry, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_registry]
        },

        %% ================================================================
        %% HEALTH CHECKS: Simple health check aggregator for orchestration
        %% Follows Joe Armstrong's principle: "Health checks are for orchestration"
        %% ================================================================
        #{
            id => erlmcp_health,
            start => {erlmcp_health, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_health]
        },

        %% ================================================================
        %% INFRASTRUCTURE: Hot reload, sessions, tasks, resources
        %% ================================================================
        #{
            id => erlmcp_reload_sup,
            start => {erlmcp_reload_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [erlmcp_reload_sup]
        },

        #{
            id => erlmcp_session_manager,
            start => {erlmcp_session_manager, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_session_manager]
        },

        %% NOTE: erlmcp_task_manager removed - module was never implemented
        %% Replaced by erlmcp_hooks for Claude Code integration
        #{
            id => erlmcp_hooks,
            start => {erlmcp_hooks, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_hooks]
        },

        #{
            id => erlmcp_resource_subscriptions,
            start => {erlmcp_resource_subscriptions, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_resource_subscriptions]
        },

        #{
            id => erlmcp_sse_event_store,
            start => {erlmcp_sse_event_store, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_sse_event_store]
        },

        #{
            id => erlmcp_icon_cache,
            start => {erlmcp_icon_cache, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_icon_cache]
        },

        %% ================================================================
        %% CACHE: Multi-level intelligent caching (L1: ETS, L2: Mnesia, L3: External)
        %% ================================================================
        #{
            id => erlmcp_cache,
            start => {erlmcp_cache, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_cache]
        },

        #{
            id => erlmcp_session_replicator,
            start => {erlmcp_session_replicator, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_session_replicator]
        },

        #{
            id => erlmcp_session_failover,
            start => {erlmcp_session_failover, start_link, [node()]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_session_failover]
        },

        %% ================================================================
        %% CONNECTION LIMITING: Prevent FD exhaustion at 10K connections
        %% ================================================================
        #{
            id => erlmcp_connection_limiter,
            start => {erlmcp_connection_limiter, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_connection_limiter]
        },

        %% ================================================================
        %% CONNECTION MONITORING: Detect and prevent FD leaks
        %% ================================================================
        #{
            id => erlmcp_connection_monitor,
            start => {erlmcp_connection_monitor, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_connection_monitor]
        },

        %% ================================================================
        %% MEMORY MONITORING: Binary garbage collection to prevent heap exhaustion
        %% NOTE: Temporarily disabled due to syntax errors - needs review
        %% #{
        %%     id => erlmcp_memory_monitor,
        %%     start => {erlmcp_memory_monitor, start_link, []},
        %%     restart => permanent,
        %%     shutdown => 5000,
        %%     type => worker,
        %%     modules => [erlmcp_memory_monitor]
        %% },

        %% ================================================================
        %% CPU QUOTA MANAGEMENT: Prevent CPU-intensive DoS attacks (TASK #107)
        %% ================================================================
        #{
            id => erlmcp_cpu_quota,
            start => {erlmcp_cpu_quota, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_cpu_quota]
        },

        %% ================================================================
        %% CANCELLATION: Request cancellation for long-running operations (TASK #142)
        %% ================================================================
        #{
            id => erlmcp_cancellation,
            start => {erlmcp_cancellation, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_cancellation]
        },

        %% ================================================================
        %% PAGINATION: Cursor-based pagination for list operations (TASK #146)
        %% ================================================================
        #{
            id => erlmcp_pagination,
            start => {erlmcp_pagination, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_pagination]
        },

        %% ================================================================
        %% NOTIFICATION HANDLERS: Supervised notification processing (RPN 168)
        %% ================================================================
        #{
            id => erlmcp_notification_handler_sup,
            start => {erlmcp_notification_handler_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [erlmcp_notification_handler_sup]
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
