-module(erlmcp_security_monitoring_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec init(term()) -> {ok, {supervisor:supervisor_specs(), [supervisor:child_spec()]}} | ignore.
init(_Args) ->
    %% Define child specifications for security monitoring components

    Children = [
        %% SIEM Integration Supervisor
        {siem_supervisor,
         {erlmcp_siem_supervisor, start_link, []},
         permanent, 5000, supervisor,
         [erlmcp_siem_supervisor]},

        %% Threat Detection Engine
        {threat_detection_engine,
         {erlmcp_threat_detection_engine, start_link, []},
         permanent, 5000, worker,
         [erlmcp_threat_detection_engine]},

        %% Security Analytics Service
        {security_analytics,
         {erlmcp_security_analytics, start_link, []},
         permanent, 5000, worker,
         [erlmcp_security_analytics]},

        %% Intrusion Detection System
        {intrusion_detection,
         {erlmcp_intrusion_detection, start_link, []},
         permanent, 5000, worker,
         [erlmcp_intrusion_detection]},

        %% Security Posture Manager
        {security_posture_manager,
         {erlmcp_security_posture_manager, start_link, []},
         permanent, 5000, worker,
         [erlmcp_security_posture_manager]},

        %% Compliance Monitor
        {compliance_monitor,
         {erlmcp_compliance_monitor, start_link, []},
         permanent, 5000, worker,
         [erlmcp_compliance_monitor]},

        %% Incident Response Orchestrator
        {incident_response_orchestrator,
         {erlmcp_incident_response_orchestrator, start_link, []},
         permanent, 5000, worker,
         [erlmcp_incident_response_orchestrator]},

        %% Threat Intelligence Service
        {threat_intelligence_service,
         {erlmcp_threat_intelligence_service, start_link, []},
         permanent, 5000, worker,
         [erlmcp_threat_intelligence_service]},

        %% Vulnerability Management System
        {vulnerability_management,
         {erlmcp_vulnerability_management, start_link, []},
         permanent, 5000, worker,
         [erlmcp_vulnerability_management]},

        %% Security Automation Engine
        {security_automation_engine,
         {erlmcp_security_automation_engine, start_link, []},
         permanent, 5000, worker,
         [erlmcp_security_automation_engine]},

        %% Security Metrics Collector
        {security_metrics_collector,
         {erlmcp_security_metrics_collector, start_link, []},
         permanent, 5000, worker,
         [erlmcp_security_metrics_collector]}
    ],

    {ok, {{one_for_all, 10, 3600}, Children}}.