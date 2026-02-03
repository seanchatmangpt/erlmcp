%% @doc erlmcp Compliance Module
%% Public API for the compliance framework
%% Provides functions to access compliance functionality
-module(erlmcp_compliance).

-export([
    %% Policy Management
    get_policies/1,
    get_policy/2,
    create_policy/4,
    update_policy/3,
    delete_policy/2,

    %% Compliance Monitoring
    monitor_compliance/1,
    check_control_status/2,
    get_violations/1,
    generate_alert/5,

    %% Audit Logging
    log_event/4,
    log_event/5,
    get_audit_trail/2,
    search_audit_events/3,

    %% Reporting
    generate_compliance_report/2,
    generate_evidence_report/3,
    generate_framework_summary/1,
    export_compliance_data/2,

    %% Enforcement
    enforce_policy/3,
    check_access/3,
    validate_data_handling/2,
    enforce_encryption/2,
    enforce_retention/3,

    %% Analysis
    analyze_compliance_trends/1,
    calculate_compliance_score/1,
    identify_gaps/2,
    perform_risk_assessment/2,
    benchmark_against_industry/1,

    %% Utilities
    get_compliance_dashboard/0,
    start_continuous_monitoring/0,
    get_all_frameworks/0
]).

%%====================================================================
%% Public API
%%====================================================================

%% Policy Management
get_policies(Framework) ->
    erlmcp_compliance_policy_manager:get_policies_by_framework(Framework).

get_policy(Framework, PolicyId) ->
    erlmcp_compliance_policy_manager:get_policy(Framework, PolicyId).

create_policy(Framework, Name, Category, Controls) ->
    erlmcp_compliance_policy_manager:create_policy(Framework, Name, Category, Controls).

update_policy(Framework, PolicyId, Updates) ->
    erlmcp_compliance_policy_manager:update_policy(Framework, PolicyId, Updates).

delete_policy(Framework, PolicyId) ->
    erlmcp_compliance_policy_manager:delete_policy(Framework, PolicyId).

%% Compliance Monitoring
monitor_compliance(Framework) ->
    erlmcp_compliance_monitor:monitor_compliance(Framework).

check_control_status(Framework, ControlId) ->
    erlmcp_compliance_monitor:check_control_status(Framework, ControlId).

get_violations(Framework) ->
    erlmcp_compliance_monitor:get_violations(Framework).

generate_alert(Framework, Severity, Message, ControlId, Details) ->
    erlmcp_compliance_monitor:generate_alert(Framework, Severity, Message, ControlId, Details).

%% Audit Logging
log_event(Framework, EventType, Severity, UserId, Action) ->
    erlmcp_compliance_audit_logger:log_event(Framework, EventType, Severity, UserId, Action, #{}).

log_event(Framework, EventType, Severity, UserId, Action, Details) ->
    erlmcp_compliance_audit_logger:log_event(Framework, EventType, Severity, UserId, Action, Details).

get_audit_trail(Framework, StartTime) ->
    erlmcp_compliance_audit_logger:get_audit_trail(Framework, StartTime).

get_audit_trail(Framework, StartTime, EndTime) ->
    erlmcp_compliance_audit_logger:get_audit_trail(Framework, StartTime, EndTime).

search_audit_events(Framework, Filters, Limit) ->
    erlmcp_compliance_audit_logger:search_audit_events(Framework, Filters, Limit).

%% Reporting
generate_compliance_report(Framework, Options) ->
    erlmcp_compliance_reporter:generate_compliance_report(Framework, Options).

generate_evidence_report(Framework, ControlId, Filters) ->
    erlmcp_compliance_reporter:generate_evidence_report(Framework, ControlId, Filters).

generate_framework_summary(Framework) ->
    erlmcp_compliance_reporter:generate_framework_summary(Framework).

export_compliance_data(Framework, Format) ->
    erlmcp_compliance_reporter:export_compliance_data(Framework, Format).

%% Enforcement
enforce_policy(Framework, Action, Context) ->
    erlmcp_compliance_enforcer:enforce_policy(Framework, Action, Context).

check_access(UserId, Resource, Action) ->
    erlmcp_compliance_enforcer:check_access(UserId, Resource, Action).

validate_data_handling(DataType, ProcessingPurpose) ->
    erlmcp_compliance_enforcer:validate_data_handling(DataType, ProcessingPurpose).

enforce_encryption(Data, Context) ->
    erlmcp_compliance_enforcer:enforce_encryption(Data, Context).

enforce_retention(DataType, RetentionPeriod, Purpose) ->
    erlmcp_compliance_enforcer:enforce_retention(DataType, RetentionPeriod, Purpose).

%% Analysis
analyze_compliance_trends(Framework) ->
    erlmcp_compliance_analyzer:analyze_compliance_trends(Framework).

calculate_compliance_score(Framework) ->
    erlmcp_compliance_analyzer:calculate_compliance_score(Framework).

identify_gaps(Framework, ReferenceModel) ->
    erlmcp_compliance_analyzer:identify_gaps(Framework, ReferenceModel).

perform_risk_assessment(Framework, Scope) ->
    erlmcp_compliance_analyzer:perform_risk_assessment(Framework, Scope).

benchmark_against_industry(Framework) ->
    erlmcp_compliance_analyzer:benchmark_against_industry(Framework).

%% Utilities
get_compliance_dashboard() ->
    erlmcp_compliance_monitor:get_compliance_dashboard().

start_continuous_monitoring() ->
    erlmcp_compliance_monitor:start_continuous_check().

get_all_frameworks() ->
    [soc2, hipaa, gdpr, iso27001].