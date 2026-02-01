-module(erlmcp_core_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

%%====================================================================
%% v2.0.0: Core Infrastructure Supervisor (TIER 1) - Supervisor of Supervisors
%%
%% Refactored from 28+ direct children to 5 intermediate supervisors.
%% Follows OTP best practice: supervisor should have ≤10 direct children.
%%
%% Architecture:
%%   erlmcp_core_sup (one_for_one)
%%   ├── erlmcp_registry_sup         (Registry + gproc)
%%   ├── erlmcp_resource_sup         (MCP resources, protocol features)
%%   ├── erlmcp_session_sup          (Session management, replication, clustering)
%%   ├── erlmcp_resilience_sup       (Circuit breakers, rate limiters, guards)
%%   └── erlmcp_infrastructure_sup   (Cache, hooks, health, clients, plugins)
%%
%% Strategy: one_for_one - each subsystem fails independently
%% Impact: Individual subsystem failures don't cascade
%% ====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags = #{
        strategy => one_for_one,  % Each intermediate supervisor fails independently
        intensity => 5,
        period => 60
    },

    ChildSpecs = [
        %% ================================================================
        %% REGISTRY SUPERVISOR: Message routing infrastructure
        %% Supervises: erlmcp_registry (gproc-based routing)
        %% ================================================================
        #{
            id => erlmcp_registry_sup,
            start => {erlmcp_registry_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [erlmcp_registry_sup]
        },

        %% ================================================================
        %% RESOURCE SUPERVISOR: MCP resources and protocol features
        %% Supervises: resource_subscriptions, sse_event_store, completion,
        %%             elicitation, roots_server, apps_server, notification handlers
        %% ================================================================
        #{
            id => erlmcp_resource_sup,
            start => {erlmcp_resource_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [erlmcp_resource_sup]
        },

        %% ================================================================
        %% SESSION SUPERVISOR: Session management and clustering
        %% Supervises: session_manager, session_replicator, session_failover,
        %%             failover_worker_sup, cluster_sup (conditional)
        %% ================================================================
        #{
            id => erlmcp_session_sup,
            start => {erlmcp_session_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [erlmcp_session_sup]
        },

        %% ================================================================
        %% RESILIENCE SUPERVISOR: DoS protection and resource guards
        %% Supervises: circuit_breaker, rate_limiter, connection_limiter,
        %%             connection_monitor, memory_monitor, cpu_quota
        %% ================================================================
        #{
            id => erlmcp_resilience_sup,
            start => {erlmcp_resilience_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [erlmcp_resilience_sup]
        },

        %% ================================================================
        %% INFRASTRUCTURE SUPERVISOR: Core services and dynamic subsystems
        %% Supervises: health, reload_sup, hooks, caches, cancellation,
        %%             pagination, client_sup, plugin_sup
        %% ================================================================
        #{
            id => erlmcp_infrastructure_sup,
            start => {erlmcp_infrastructure_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [erlmcp_infrastructure_sup]
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.
