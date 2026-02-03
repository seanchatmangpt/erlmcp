-module(erlmcp_zero_trust_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    %% Zero-trust supervision tree
    Children = [
        %% Identity Management
        {erlmcp_identity_manager,
            {erlmcp_identity_manager, start_link, []},
            permanent, 5000, worker, [erlmcp_identity_manager]},

        %% Access Control
        {erlmcp_access_control,
            {erlmcp_access_control, start_link, []},
            permanent, 5000, worker, [erlmcp_access_control]},

        %% Network Isolation
        {erlmcp_network_isolation,
            {erlmcp_network_isolation, start_link, []},
            permanent, 5000, worker, [erlmcp_network_isolation]},

        %% Data Protection
        {erlmcp_data_protection,
            {erlmcp_data_protection, start_link, []},
            permanent, 5000, worker, [erlmcp_data_protection]},

        %% Security Monitoring
        {erlmcp_security_monitor,
            {erlmcp_security_monitor, start_link, []},
            permanent, 5000, worker, [erlmcp_security_monitor]},

        %% Threat Detection
        {erlmcp_threat_detection,
            {erlmcp_threat_detection, start_link, []},
            permanent, 5000, worker, [erlmcp_threat_detection]},

        %% Supply Chain Security
        {erlmcp_supply_chain_security,
            {erlmcp_supply_chain_security, start_link, []},
            permanent, 5000, worker, [erlmcp_supply_chain_security]},

        %% Application Security
        {erlmcp_application_security,
            {erlmcp_application_security, start_link, []},
            permanent, 5000, worker, [erlmcp_application_security]},

        %% Compliance Automation
        {erlmcp_compliance_automation,
            {erlmcp_compliance_automation, start_link, []},
            permanent, 5000, worker, [erlmcp_compliance_automation]},

        %% Zero Trust Integration
        {erlmcp_zero_trust_integration,
            {erlmcp_zero_trust_integration, start_link, []},
            permanent, 5000, worker, [erlmcp_zero_trust_integration]}
    ],

    {ok, {{one_for_all, 10, 3600}, Children}}.