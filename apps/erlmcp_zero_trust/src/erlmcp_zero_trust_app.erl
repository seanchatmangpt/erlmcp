-module(erlmcp_zero_trust_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, prep_stop/1]).

%% Public API functions
-export([get_security_posture/1, enable_continuous_verification/1, generate_compliance_report/2]).
-export([create_security_policy/2, run_threat_simulation/1, export_audit_logs/2]).

start(_Type, _Args) ->
    {ok, Pid} = erlmcp_zero_trust_sup:start_link(),
    erlmcp_zero_trust_app:register_events(),
    {ok, Pid}.

stop(_State) ->
    ok.

prep_stop(State) ->
    logger:info("Preparing erlmcp_zero_trust for graceful shutdown"),
    %% Unregister security handlers gracefully
    try
        erlmcp_security_monitor:unregister_handlers(),
        erlmcp_compliance_automation:stop_continuous_monitoring()
    catch
        _:_ -> ok
    end,
    State.

register_events() ->
    %% Register security events for monitoring
    erlmcp_security_monitor:register_handlers(),
    erlmcp_threat_detection:initialize_intelligence_feeds(),
    erlmcp_compliance_automation:start_continuous_monitoring(),
    erlmcp_integration:enable_cross_component_communication().

get_security_posture(Application) ->
    erlmcp_zero_trust_integration:assess_security_posture(Application).

enable_continuous_verification(Application) ->
    erlmcp_identity_manager:enable_continuous_verification(Application).

generate_compliance_report(Framework, Scope) ->
    erlmcp_compliance_automation:generate_report(Framework, Scope).

create_security_policy(Application, PolicyData) ->
    erlmcp_application_security:create_security_profile(Application, PolicyData).

run_threat_simulation(Scenario) ->
    erlmcp_threat_detection:simulate_threat_scenario(Scenario).

export_audit_logs(Application, Format) ->
    erlmcp_zero_trust_integration:export_audit_logs(Application, Format).