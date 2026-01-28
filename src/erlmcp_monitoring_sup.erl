-module(erlmcp_monitoring_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

%%====================================================================
%% v1.3.0: Monitoring/Observability Subsystem Supervisor
%%
%% Supervises all monitoring, health checks, metrics, and dashboards.
%% This is TIER 5 - independent subsystem that can fail without affecting core.
%%
%% Strategy: one_for_one - monitoring failures do not propagate
%% Impact: Lost observability but no protocol layer impact
%% ====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags = #{
        strategy => one_for_one,  % Each monitoring component fails independently
        intensity => 5,
        period => 60
    },

    ChildSpecs = [
        %% Health monitor - system health monitoring
        #{
            id => erlmcp_health_monitor,
            start => {erlmcp_health_monitor, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_health_monitor]
        },

        %% Recovery manager - failure recovery coordination
        #{
            id => erlmcp_recovery_manager,
            start => {erlmcp_recovery_manager, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_recovery_manager]
        },

        %% Metrics server - collects and aggregates system metrics
        #{
            id => erlmcp_metrics_server,
            start => {erlmcp_metrics_server, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_metrics_server]
        },

        %% Metrics HTTP supervisor - HTTP server for dashboard
        #{
            id => erlmcp_metrics_http_sup,
            start => {erlmcp_metrics_http_sup, start_link, [8088]},
            restart => transient,
            shutdown => 5000,
            type => supervisor,
            modules => [erlmcp_metrics_http_sup]
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.
