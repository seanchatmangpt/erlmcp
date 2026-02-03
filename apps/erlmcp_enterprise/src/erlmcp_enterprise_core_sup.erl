%% @doc Enterprise Core Services Supervisor
%% Manages critical enterprise integration services with one_for_all strategy
-module(erlmcp_enterprise_core_sup).
-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

%%====================================================================
%% API functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

-spec init(Args :: term()) -> {ok, {SupervisorFlags :: supervisor:flags(),
                                     [ChildSpec :: supervisor:child_spec()]}}.
init([]) ->
    %% Enterprise Registry
    Registry = #{
        id => erlmcp_enterprise_registry,
        start => {erlmcp_enterprise_registry, start_link, []},
        type => worker,
        modules => [erlmcp_enterprise_registry],
        restart => permanent,
        shutdown => 2000,
        significance => critical
    },

    %% Enterprise Configuration Manager
    ConfigManager = #{
        id => erlmcp_enterprise_config,
        start => {erlmcp_enterprise_config, start_link, []},
        type => worker,
        modules => [erlmcp_enterprise_config],
        restart => permanent,
        shutdown => 2000,
        significance => critical
    },

    %% Enterprise Connection Pool
    ConnectionPool = #{
        id => erlmcp_enterprise_pool,
        start => {erlmcp_enterprise_pool_sup, start_link, []},
        type => supervisor,
        modules => [erlmcp_enterprise_pool_sup],
        restart => permanent,
        shutdown => infinity,
        significance => critical
    },

    %% Enterprise Event Bus
    EventBus = #{
        id => erlmcp_enterprise_event,
        start => {erlmcp_enterprise_event, start_link, []},
        type => worker,
        modules => [erlmcp_enterprise_event],
        restart => permanent,
        shutdown => 2000,
        significance => critical
    },

    %% Enterprise Metrics Collector
    MetricsCollector = #{
        id => erlmcp_enterprise_metrics,
        start => {erlmcp_enterprise_metrics, start_link, []},
        type => worker,
        modules => [erlmcp_enterprise_metrics],
        restart => permanent,
        shutdown => 2000,
        significance => critical
    },

    %% Enterprise Health Monitor
    HealthMonitor = #{
        id => erlmcp_enterprise_health,
        start => {erlmcp_enterprise_health, start_link, []},
        type => worker,
        modules => [erlmcp_enterprise_health],
        restart => permanent,
        shutdown => 2000,
        significance => critical
    },

    %% Enterprise Audit Logger
    AuditLogger = #{
        id => erlmcp_enterprise_audit,
        start => {erlmcp_enterprise_audit, start_link, []},
        type => worker,
        modules => [erlmcp_enterprise_audit],
        restart => permanent,
        shutdown => 2000,
        significance => critical
    },

    Children = [
        Registry,
        ConfigManager,
        ConnectionPool,
        EventBus,
        MetricsCollector,
        HealthMonitor,
        AuditLogger
    ],

    {ok, {{one_for_all, 5, 3600}, Children}}.