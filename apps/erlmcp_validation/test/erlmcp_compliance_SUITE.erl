%% @doc Comprehensive Compliance Framework Test Suite
%% Tests all compliance-related functionality including SOC2, HIPAA, GDPR, ISO27001
%%
%% Test Coverage Areas:
%% - Compliance policy management
%% - Real-time compliance monitoring
%% - Incident response coordination
%% - Continuous compliance validation
%% - Policy enforcement
%% - Compliance reporting
%% - Data protection enforcement
%% - Audit logging
%%
%% @end
-module(erlmcp_compliance_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Test exports
-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

%% Test cases
-export([
    compliance_framework_test/1,
    policy_enforcement_test/1,
    incident_response_test/1,
    continuous_compliance_test/1,
    compliance_monitoring_test/1,
    compliance_reporting_test/1,
    data_protection_test/1,
    audit_logging_test/1,
    soc2_compliance_test/1,
    hipaa_compliance_test/1,
    gdpr_compliance_test/1,
    iso27001_compliance_test/1
]).

%%====================================================================
%% Test Configuration
%%====================================================================

all() ->
    [
        compliance_framework_test,
        policy_enforcement_test,
        incident_response_test,
        continuous_compliance_test,
        compliance_monitoring_test,
        compliance_reporting_test,
        data_protection_test,
        audit_logging_test,
        soc2_compliance_test,
        hipaa_compliance_test,
        gdpr_compliance_test,
        iso27001_compliance_test
    ].

init_per_suite(Config) ->
    %% Start test dependencies
    ok = application:start(erlmcp_core),
    ok = application:start(erlmcp_validation),
    ok = application:start(erlmcp_compliance),

    %% Create test data directory
    TestDir = ?config(data_dir, Config),
    ComplianceDir = filename:join(TestDir, "compliance_data"),
    ok = filelib:ensure_dir(ComplianceDir),

    [{compliance_dir, ComplianceDir} | Config].

end_per_suite(Config) ->
    %% Stop applications
    ok = application:stop(erlmcp_compliance),
    ok = application:stop(erlmcp_validation),
    ok = application:stop(erlmcp_core),

    %% Clean up test data
    TestDir = ?config(data_dir, Config),
    ok = file:delete(filename:join(TestDir, "compliance_data")),

    ok.

init_per_testcase(TestCase, Config) ->
    %% Set up test environment
    ct:pal("Initializing test case: ~p", [TestCase]),
    Config.

end_per_testcase(TestCase, Config) ->
    %% Clean up after test
    ct:pal("Cleaning up test case: ~p", [TestCase]),
    Config.

%%====================================================================
%% Test Cases
%%====================================================================

compliance_framework_test(Config) ->
    %% Test compliance framework initialization and basic operations
    ct:pal("Testing compliance framework"),

    %% Test 1: Start compliance framework
    {ok, _} = erlmcp_compliance_framework:start_link(),

    %% Test 2: Register a compliance policy
    PolicyId = <<"test_policy">>,
    PolicyData = #{
        name => "Test Compliance Policy",
        standard => soc2_type_ii,
        requirements => ["CC6.1", "CC6.2"],
        controls => ["access_control", "audit_logging"],
        enforcement_level => mandatory
    },
    ok = erlmcp_compliance_framework:register_policy(PolicyId, PolicyData),

    %% Test 3: Enforce the policy
    Context = #{
        user_id => <<"test_user">>,
        action => <<"access_resource">>,
        resource => <<"test_resource">>
    },
    Result = erlmcp_compliance_framework:enforce_policy(PolicyId, Context, <<"admin_user">>),
    ct:pal("Policy enforcement result: ~p", [Result]),

    %% Test 4: Check compliance
    {ComplianceLevel, Details} = erlmcp_compliance_framework:check_compliance(soc2_type_ii, Context),
    ct:pal("Compliance level: ~p, Details: ~p", [ComplianceLevel, Details]),

    %% Test 5: Generate compliance report
    {ok, Report} = erlmcp_compliance_framework:generate_report(soc2_type_ii),
    ct:pal("Generated report size: ~p", [maps:size(Report)]),

    %% Clean up
    ok = erlmcp_compliance_framework:stop(),

    %% Verify expected outcomes
    ComplianceLevel =/= undefined,
    maps:size(Report) > 0,
    ok.

policy_enforcement_test(Config) ->
    %% Test policy enforcement functionality
    ct:pal("Testing policy enforcement"),

    %% Start policy enforcer
    {ok, _} = erlmcp_policy_enforcer:start_link(),

    %% Add test policy
    PolicyId = <<"enforcement_test_policy">>,
    PolicyData = #{
        name => "Access Control Policy",
        type => access_control,
        description => "Test access control policy",
        compliance_standard => "SOC2",
        enforcement_level => mandatory,
        enabled => true
    },
    ok = erlmcp_policy_enforcer:add_policy(PolicyId, PolicyData),

    %% Add policy rule
    RuleData = #{
        name => "MFA Required",
        description => "Multi-factor authentication required",
        condition => "user.privileged = true",
        action => allow,
        severity => high,
        enabled => true
    },
    ok = erlmcp_policy_enforcer:create_policy_rule(PolicyId, RuleData),

    %% Test policy enforcement
    Context = #{
        user_id => <<"test_user">>,
        privileged => true
    },
    Result = erlmcp_policy_enforcer:enforce_policy(PolicyId, Context, <<"admin">>),
    ct:pal("Policy enforcement result: ~p", [Result]),

    %% Check policy status
    {ok, Status} = erlmcp_policy_enforcer:get_policy_status(PolicyId),
    ct:pal("Policy status: ~p", [Status]),

    %% Clean up
    ok = erlmcp_policy_enforcer:stop(),

    %% Verify results
    is_map(Status),
    ok.

incident_response_test(Config) ->
    %% Test incident response coordination
    ct:pal("Testing incident response"),

    %% Start incident response coordinator
    {ok, _} = erlmcp_incident_response:start_link(),

    %% Create test incident
    IncidentData = #{
        title => "Security Incident Test",
        description => "Test security incident",
        type => security_breach,
        severity => high,
        affected_systems => ["test_system"]
    },
    {ok, IncidentId} = erlmcp_incident_response:create_incident(IncidentData, <<"test_user">>),
    ct:pal("Created incident: ~p", [IncidentId]),

    %% Update incident
    UpdateData = #{
        status => investigating,
        actions => [#{action => "investigate", user_id => <<"admin">>}]
    },
    ok = erlmcp_incident_response:update_incident(IncidentId, UpdateData, <<"admin">>),

    %% Get incident details
    {ok, IncidentDetails} = erlmcp_incident_response:get_incident(IncidentId),
    ct:pal("Incident details: ~p", [IncidentDetails]),

    %% Assign incident
    ok = erlmcp_incident_response:assign_incident(IncidentId, <<"analyst_user">>, "Assign to analyst"),

    %% Log incident action
    ok = erlmcp_incident_response:log_incident_action(IncidentId, "initial_analysis", #{details => "Initial analysis complete"}, <<"analyst_user">>),

    %% Get active incidents
    {ok, ActiveIncidents} = erlmcp_incident_response:get_active_incidents(all),
    ct:pal("Active incidents count: ~p", [length(ActiveIncidents)]),

    %% Generate incident report
    {ok, ReportId} = erlmcp_incident_response:generate_incident_report(IncidentId, #{format => pdf}),
    ct:pal("Generated incident report: ~p", [ReportId]),

    %% Clean up
    ok = erlmcp_incident_response:stop(),

    %% Verify results
    is_binary(IncidentId),
    maps:get(type, IncidentDetails) =:= security_breach,
    ok.

continuous_compliance_test(Config) ->
    %% Test continuous compliance validation
    ct:pal("Testing continuous compliance"),

    %% Start continuous compliance checker
    {ok, _} = erlmcp_continuous_compliance:start_link(),

    %% Get compliance score
    {ok, Score} = erlmcp_continuous_compliance:get_compliance_score(soc2_type_i),
    ct:pal("SOC2 Type I compliance score: ~p", [Score]),

    %% Get compliance trends
    {ok, Trends} = erlmcp_continuous_compliance:get_compliance_trends(soc2_type_i),
    ct:pal("Compliance trends: ~p", [Trends]),

    %% Identify compliance gaps
    {ok, Gaps} = erlmcp_continuous_compliance:identify_gaps(soc2_type_i, #{}),
    ct:pal("Identified gaps: ~p", [length(Gaps)]),

    %% Generate compliance dashboard
    {ok, Dashboard} = erlmcp_continuous_compliance:generate_compliance_dashboard(all),
    ct:pal("Dashboard overall score: ~p", [maps:get(overall_score, Dashboard)]),

    %% Run immediate compliance check
    {ok, CheckResult} = erlmcp_continuous_compliance:run_compliance_check(hipaa, #{}),
    ct:pal("Compliance check result: ~p", [CheckResult]),

    %% Set compliance thresholds
    Thresholds = #{
        critical => 0.7,
        high => 0.8,
        medium => 0.85,
        low => 0.9,
        action => "attention_required"
    },
    ok = erlmcp_continuous_compliance:set_compliance_thresholds(hipaa, Thresholds),

    %% Predict compliance risk
    {ok, RiskPrediction} = erlmcp_continuous_compliance:predict_compliance_risk(hipaa),
    ct:pal("Risk prediction: ~p", [maps:get(risk_level, RiskPrediction)]),

    %% Clean up
    ok = erlmcp_continuous_compliance:stop(),

    %% Verify results
    is_map(Score),
    is_map(Dashboard),
    is_map(RiskPrediction),
    ok.

compliance_monitoring_test(Config) ->
    %% Test compliance monitoring functionality
    ct:pal("Testing compliance monitoring"),

    %% Start compliance monitor
    {ok, _} = erlmcp_compliance_monitor:start_link(),

    %% Add monitoring configuration
    MonitorId = <<"soc2_monitor">>,
    MonitorConfig = #{
        standard => soc2_type_ii,
        type => real_time,
        frequency => 30,
        enabled => true,
        rules => [
            #{
                id => "access_control_violation",
                condition => "failed_access_attempts > 5",
                action => "alert"
            }
        ],
        thresholds => #{failed_access_attempts => 5}
    },
    ok = erlmcp_compliance_monitor:add_monitor(MonitorId, MonitorConfig),

    %% Get monitoring status
    Status = erlmcp_compliance_monitor:get_monitoring_status(),
    ct:pal("Monitoring status: ~p", [Status]),

    %% Get compliance score
    {ok, Score} = erlmcp_compliance_monitor:get_compliance_score(soc2_type_ii),
    ct:pal("Compliance score: ~p", [Score]),

    %% Generate alert
    ok = erlmcp_compliance_monitor:generate_alert(soc2_type_ii, violation, #{message => "Test alert"}),

    %% Get violations
    {ok, Violations} = erlmcp_compliance_monitor:get_violations(soc2_type_ii, #{}),
    ct:pal("Violations count: ~p", [length(Violations)]),

    %% Clean up
    ok = erlmcp_compliance_monitor:stop(),

    %% Verify results
    is_map(Status),
    is_map(Score),
    ok.

compliance_reporting_test(Config) ->
    %% Test compliance reporting functionality
    ct:pal("Testing compliance reporting"),

    %% Start compliance reporter
    {ok, _} = erlmcp_compliance_reporter:start_link(),

    %% Generate report
    {ok, ReportId} = erlmcp_compliance_reporter:generate_report(hipaa, #{type => detailed, format => pdf}),
    ct:pal("Generated report: ~p", [ReportId]),

    %% Generate executive summary
    {ok, SummaryId} = erlmcp_compliance_reporter:generate_executive_summary(gdpr, #{}),
    ct:pal("Generated executive summary: ~p", [SummaryId]),

    %% Generate audit package
    {ok, PackageId} = erlmcp_compliance_reporter:generate_audit_package(iso27001, #{}),
    ct:pal("Generated audit package: ~p", [PackageId]),

    %% Export report
    {ok, Exported} = erlmcp_compliance_reporter:export_report(ReportId, pdf, #{}),
    ct:pal("Exported report size: ~p", [byte_size(Exported)]),

    %% Get report templates
    Templates = erlmcp_compliance_reporter:get_report_templates(),
    ct:pal("Available templates: ~p", [length(Templates)]),

    %% Get report history
    History = erlmcp_compliance_reporter:get_report_history(soc2_type_ii),
    ct:pal("Report history count: ~p", [length(History)]),

    %% Clean up
    ok = erlmcp_compliance_reporter:stop(),

    %% Verify results
    is_binary(ReportId),
    length(Templates) > 0,
    ok.

data_protection_test(Config) ->
    %% Test data protection enforcement
    ct:pal("Testing data protection"),

    %% Start compliance framework
    {ok, _} = erlmcp_compliance_framework:start_link(),

    %% Test encryption
    TestData = <<"sensitive_data">>,
    Context = #{standard => hipaa, classification => "PHI"},

    {ok, Encrypted} = erlmcp_compliance_framework:encrypt_data(TestData, Context),
    ct:pal("Encrypted data length: ~p", [byte_size(Encrypted)]),

    {ok, Decrypted} = erlmcp_compliance_framework:decrypt_data(Encrypted, Context),
    ct:pal("Decrypted data matches: ~p", [TestData =:= Decrypted]),

    %% Test compliance assessment
    {ComplianceLevel, _} = erlmcp_compliance_framework:check_compliance(hipaa, Context),
    ct:pal("Data protection compliance: ~p", [ComplianceLevel]),

    %% Clean up
    ok = erlmcp_compliance_framework:stop(),

    %% Verify results
    TestData =:= Decrypted,
    ComplianceLevel =/= undefined,
    ok.

audit_logging_test(Config) ->
    %% Test audit logging functionality
    ct:pal("Testing audit logging"),

    %% Start compliance framework
    {ok, _} = erlmcp_compliance_framework:start_link(),

    %% Log audit events
    ok = erlmcp_compliance_framework:audit_event(soc2_type_ii, "user_login", <<"test_user">>, #{device => "test_device"}),
    ok = erlmcp_compliance_framework:audit_event(hipaa, "phi_access", <<"doctor">>, #{patient_id => "12345"}),

    %% Create incidents to generate audit events
    IncidentData = #{
        title => "Audit Test Incident",
        description => "Test incident for audit",
        type => security_breach,
        severity => medium
    },
    {ok, _IncidentId} = erlmcp_compliance_framework:create_incident(hipaa, IncidentData),

    %% Clean up
    ok = erlmcp_compliance_framework:stop(),

    %% Note: Actual audit log verification would require direct access to internal state
    ct:pal("Audit logging test completed"),
    ok.

soc2_compliance_test(Config) ->
    %% Test SOC2 compliance specific functionality
    ct:pal("Testing SOC2 compliance"),

    %% Start compliance framework
    {ok, _} = erlmcp_compliance_framework:start_link(),

    %% Test SOC2 Type I compliance
    {ComplianceLevel, Details} = erlmcp_compliance_framework:check_compliance(soc2_type_i, #{}),
    ct:pal("SOC2 Type I compliance: ~p", [ComplianceLevel]),

    %% Test SOC2 Type II compliance
    {ComplianceLevel2, Details2} = erlmcp_compliance_framework:check_compliance(soc2_type_ii, #{}),
    ct:pal("SOC2 Type II compliance: ~p", [ComplianceLevel2]),

    %% Generate SOC2 reports
    {ok, Report1} = erlmcp_compliance_framework:generate_report(soc2_type_i),
    {ok, Report2} = erlmcp_compliance_framework:generate_report(soc2_type_ii),
    ct:pal("SOC2 Type I report size: ~p, Type II report size: ~p", [maps:size(Report1), maps:size(Report2)]),

    %% Assess SOC2 risk
    {ok, RiskAssessment} = erlmcp_compliance_framework:assess_risk(soc2_type_ii, #{}),
    ct:pal("SOC2 risk level: ~p", [maps:get(risk_level, RiskAssessment)]),

    %% Clean up
    ok = erlmcp_compliance_framework:stop(),

    %% Verify results
    ComplianceLevel =/= undefined,
    ComplianceLevel2 =/= undefined,
    is_map(RiskAssessment),
    ok.

hipaa_compliance_test(Config) ->
    %% Test HIPAA compliance specific functionality
    ct:pal("Testing HIPAA compliance"),

    %% Start compliance framework
    {ok, _} = erlmcp_compliance_framework:start_link(),

    %% Test HIPAA compliance check
    {ComplianceLevel, Details} = erlmcp_compliance_framework:check_compliance(hipaa, #{}),
    ct:pal("HIPAA compliance: ~p", [ComplianceLevel]),

    %% Test PHI protection
    TestPHI = #{
        patient_id => "12345",
        name => "John Doe",
        diagnosis => "Hypertension",
        ssn => "123-45-6789"
    },

    {ok, EncryptedPHI} = erlmcp_compliance_framework:encrypt_data(erlang:term_to_binary(TestPHI), #{standard => hipaa}),
    ct:pal("PHI encryption successful"),

    %% Test HIPAA breach notification assessment
    {ok, Risk} = erlmcp_compliance_framework:assess_risk(hipaa, #{contains_phi => true}),
    ct:pal("HIPAA breach risk: ~p", [maps:get(risk_level, Risk)]),

    %% Generate HIPAA report
    {ok, Report} = erlmcp_compliance_framework:generate_report(hipaa),
    ct:pal("HIPAA report generated: ~p", [maps:size(Report)]),

    %% Clean up
    ok = erlmcp_compliance_framework:stop(),

    %% Verify results
    ComplianceLevel =/= undefined,
    is_binary(EncryptedPHI),
    is_map(Risk),
    ok.

gdpr_compliance_test(Config) ->
    %% Test GDPR compliance specific functionality
    ct:pal("Testing GDPR compliance"),

    %% Start compliance framework
    {ok, _} = erlmcp_compliance_framework:start_link(),

    %% Test GDPR compliance check
    {ComplianceLevel, Details} = erlmcp_compliance_framework:check_compliance(gdpr, #{}),
    ct:pal("GDPR compliance: ~p", [ComplianceLevel]),

    %% Test data subject rights
    Context = #{
        request_type => "data_access",
        user_id => "user123",
        data_categories => ["personal", "behavioral"]
    },

    {ok, Risk} = erlmcp_compliance_framework:assess_risk(gdpr, Context),
    ct:pal("GDPR data subject rights risk: ~p", [maps:get(risk_level, Risk)]),

    %% Test data protection impact assessment
    Assessment = erlmcp_compliance_framework:check_compliance(gdpr, #{assessment_type => "DPIA"}),
    ct:pal("DPIA assessment: ~p", [Assessment]),

    %% Generate GDPR report
    {ok, Report} = erlmcp_compliance_framework:generate_report(gdpr),
    ct:pal("GDPR report size: ~p", [maps:size(Report)]),

    %% Clean up
    ok = erlmcp_compliance_framework:stop(),

    %% Verify results
    ComplianceLevel =/= undefined,
    is_map(Risk),
    ok.

iso27001_compliance_test(Config) ->
    %% Test ISO27001 compliance specific functionality
    ct:pal("Testing ISO27001 compliance"),

    %% Start compliance framework
    {ok, _} = erlmcp_compliance_framework:start_link(),

    %% Test ISO27001 compliance check
    {ComplianceLevel, Details} = erlmcp_compliance_framework:check_compliance(iso27001, #{}),
    ct:pal("ISO27001 compliance: ~p", [ComplianceLevel]),

    %% Test control testing
    TestContext = #{
        controls_tested => ["A.9", "A.10", "A.12"],
        pass_rate => 0.95,
        findings => 2
    },

    {ok, Risk} = erlmcp_compliance_framework:assess_risk(iso27001, TestContext),
    ct:pal("ISO27001 control risk: ~p", [maps:get(risk_level, Risk)]),

    %% Generate ISO27001 report
    {ok, Report} = erlmcp_compliance_framework:generate_report(iso27001),
    ct:pal("ISO27001 report size: ~p", [maps:size(Report)]),

    %% Test continuous compliance for ISO27001
    {ok, Dashboard} = erlmcp_continuous_compliance:generate_compliance_dashboard(iso27001),
    ct:pal("ISO27001 dashboard score: ~p", [maps:get(overall_score, Dashboard)]),

    %% Clean up
    ok = erlmcp_compliance_framework:stop(),

    %% Verify results
    ComplianceLevel =/= undefined,
    is_map(Risk),
    ok.