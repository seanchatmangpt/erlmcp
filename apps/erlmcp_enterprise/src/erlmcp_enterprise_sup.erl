%% @doc erlmcp_enterprise supervisor
%% 3-tier supervision for enterprise integrations
-module(erlmcp_enterprise_sup).
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
    %% Tier 1: Core Enterprise Services (one_for_all)
    EnterpriseCore = #{
        id => erlmcp_enterprise_core_sup,
        start => {erlmcp_enterprise_core_sup, start_link, []},
        type => supervisor,
        modules => [erlmcp_enterprise_core_sup],
        restart => permanent,
        shutdown => infinity,
        significance => critical
    },

    %% Tier 2: Integration Adapters (simple_one_for_one)
    IntegrationAdapters = #{
        id => erlmcp_enterprise_adapter_sup,
        start => {erlmcp_enterprise_adapter, start_link, []},
        type => supervisor,
        modules => [erlmcp_enterprise_adapter],
        restart => transient,
        shutdown => 5000,
        significant => true
    },

    %% Tier 3: Platform Connectors (isolated)
    PlatformConnectors = #{
        id => erlmcp_enterprise_connector_sup,
        start => {erlmcp_enterprise_connector_sup, start_link, []},
        type => supervisor,
        modules => [erlmcp_enterprise_connector_sup],
        restart => permanent,
        shutdown => infinity,
        significance => critical
    },

    %% Tier 4: Monitoring & Observability (isolated)
    Monitoring = #{
        id => erlmcp_enterprise_monitor_sup,
        start => {erlmcp_enterprise_monitor_sup, start_link, []},
        type => supervisor,
        modules => [erlmcp_enterprise_monitor_sup],
        restart => permanent,
        shutdown => infinity,
        significance => critical
    },

    %% Tier 5: Security Services (isolated)
    Security = #{
        id => erlmcp_enterprise_security_sup,
        start => {erlmcp_enterprise_security_sup, start_link, []},
        type => supervisor,
        modules => [erlmcp_enterprise_security_sup],
        restart => permanent,
        shutdown => infinity,
        significance => critical
    },

    Children = [
        EnterpriseCore,
        IntegrationAdapters,
        PlatformConnectors,
        Monitoring,
        Security
    ],

    {ok, {{one_for_all, 10, 3600}, Children}}.