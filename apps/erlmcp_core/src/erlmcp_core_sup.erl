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

    ChildSpecs = [
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
        },

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

        #{
            id => erlmcp_registry_health_check,
            start => {erlmcp_registry_health_check, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_registry_health_check]
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

        #{
            id => erlmcp_task_manager,
            start => {erlmcp_task_manager, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_task_manager]
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
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.
