%% @doc Comprehensive Test Suite for erlmcp Compliance Framework
%% Tests all compliance frameworks: SOC2, HIPAA, GDPR, ISO27001
-module(erlmcp_compliance_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Export all test functions
-export([
         all/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2
        ]).

%% Test groups
-export([
         compliance_frameworks/0,
         policy_management/0,
         audit_logging/0,
         monitoring/0,
         reporting/0,
         enforcement/0,
         analysis/0
        ]).

%% Test cases
-export([
         test_soc2_compliance/1,
         test_hipaa_compliance/1,
         test_gdpr_compliance/1,
         test_iso27001_compliance/1,
         test_policy_lifecycle/1,
         test_policy_approval/1,
         test_audit_trail/1,
         test_realtime_monitoring/1,
         test_compliance_report/1,
         test_evidence_collection/1,
         test_policy_enforcement/1,
         test_access_control/1,
         test_data_handling/1,
         test_compliance_scoring/1,
         test_risk_assessment/1,
         test_benchmarking/1,
         test_compliance_forecast/1,
         test_integration_with_core/1
        ]).

%%====================================================================
%% Test configuration
%%====================================================================

all() ->
    [
        {group, compliance_frameworks},
        {group, policy_management},
        {group, audit_logging},
        {group, monitoring},
        {group, reporting},
        {group, enforcement},
        {group, analysis}
    ].

compliance_frameworks() ->
    [test_soc2_compliance, test_hipaa_compliance, test_gdpr_compliance, test_iso27001_compliance].

policy_management() ->
    [test_policy_lifecycle, test_policy_approval].

audit_logging() ->
    [test_audit_trail, test_evidence_collection].

monitoring() ->
    [test_realtime_monitoring, test_compliance_scoring].

reporting() ->
    [test_compliance_report, test_evidence_collection].

enforcement() ->
    [test_policy_enforcement, test_access_control, test_data_handling].

analysis() ->
    [test_risk_assessment, test_benchmarking, test_compliance_forecast].

%%====================================================================
%% Setup functions
%%====================================================================

init_per_suite(Config) ->
    %% Start compliance application
    {ok, _} = application:ensure_all_started(erlmcp_compliance),

    %% Create test data
    TestData = create_test_data(),
    [{test_data, TestData} | Config].

end_per_suite(_Config) ->
    %% Stop compliance application
    ok = application:stop(erlmcp_compliance).

init_per_testcase(_TestCase, Config) ->
    %% Reset state before each test
    reset_compliance_state(),
    Config.

end_per_testcase(_TestCase, _Config) ->
    %% Clean up after test
    cleanup_test_data(),
    ok.

%%====================================================================
%% Test cases - Compliance Frameworks
%%====================================================================

test_soc2_compliance(Config) ->
    ct:comment("Testing SOC2 compliance implementation"),
    TestData = ?config(test_data, Config),

    %% Test SOC2 specific controls
    PolicyId = erlmcp_compliance:create_policy(soc2, "Access Control", "Security",
                                             ["MFA", "RBAC", "Session Timeout"]),

    %% Check policy creation
    ?assert(is_binary(PolicyId)),

    %% Monitor compliance
    ComplianceStatus = erlmcp_compliance:monitor_compliance(soc2),
    ?assert(is_map(ComplianceStatus)),

    %% Check control status
    ControlStatus = erlmcp_compliance:check_control_status(soc2, PolicyId),
    ?assert(is_record(ControlStatus, control_status)),

    ok.

test_hipaa_compliance(Config) ->
    ct:comment("Testing HIPAA compliance implementation"),
    TestData = ?config(test_data, Config),

    %% Test HIPAA specific safeguards
    SafeguardId = erlmcp_compliance:create_policy(hipaa, "Technical Safeguards", "Security",
                                                  ["Access Control", "Audit Controls", "Integrity"]),

    ?assert(is_binary(SafeguardId)),

    %% Validate PHI handling
    ValidationResult = erlmcp_compliance:validate_data_handling("phi", "treatment"),
    ?assert(is_map(ValidationResult)),

    ok.

test_gdpr_compliance(Config) ->
    ct:comment("Testing GDPR compliance implementation"),
    TestData = ?config(test_data, Config),

    %% Test GDPR requirements
    DpoPolicy = erlmcp_compliance:create_policy(gdpr, "Data Subject Rights", "Privacy",
                                               ["Access", "Erasure", "Portability"]),

    ?assert(is_binary(DpoPolicy)),

    %% Test data subject requests
    DsrResult = erlmcp_compliance:handle_data_subject_request("access", "user123"),
    ?assert(is_map(DsrResult)),

    ok.

test_iso27001_compliance(Config) ->
    ct:comment("Testing ISO27001 compliance implementation"),
    TestData = ?config(test_data, Config),

    %% Test ISO27001 controls
    IsoControl = erlmcp_compliance:create_policy(iso27001, "Information Security Management", "ISMS",
                                                ["Risk Assessment", "Access Control", "Incident Management"]),

    ?assert(is_binary(IsoControl)),

    %% Perform risk assessment
    RiskAssessment = erlmcp_compliance:perform_risk_assessment(iso27001, full),
    ?assert(is_map(RiskAssessment)),

    ok.

%%====================================================================
%% Test cases - Policy Management
%%====================================================================

test_policy_lifecycle(Config) ->
    ct:comment("Testing policy lifecycle management"),

    %% Create policy
    PolicyId = erlmcp_compliance:create_policy(soc2, "Test Policy", "Category", ["Control1"]),
    ?assert(is_binary(PolicyId)),

    %% Get policy
    Policy = erlmcp_compliance:get_policy(soc2, PolicyId),
    ?assert(is_record(Policy, policy)),

    %% Update policy
    UpdateResult = erlmcp_compliance:update_policy(soc2, PolicyId, #{implemented => true}),
    ?assert(is_record(UpdateResult, policy)),

    %% Delete policy
    DeleteResult = erlmcp_compliance:delete_policy(soc2, PolicyId),
    ?assert(DeleteResult =:= {ok, deleted}),

    ok.

test_policy_approval(Config) ->
    ct:comment("Testing policy approval workflow"),

    %% Create policy requiring approval
    PolicyId = erlmcp_compliance:create_policy(soc2, "Approved Policy", "Category", ["Control1"]),

    %% Simulate approval process
    ApprovalResult = erlmcp_compliance:approve_policy(soc2, PolicyId, "approver123"),
    ?assert(ApprovalResult =:= approved),

    %% Check policy status
    UpdatedPolicy = erlmcp_compliance:get_policy(soc2, PolicyId),
    ?assert(UpdatedPolicy#policy.implemented =:= true),

    ok.

%%====================================================================
%% Test cases - Audit Logging
%%====================================================================

test_audit_trail(Config) ->
    ct:comment("Testing audit trail functionality"),

    %% Log audit event
    EventId = erlmcp_compliance:log_event(soc2, policy_change, high, "user123",
                                         "Updated policy", #{policy_id => "test-policy"}),
    ?assert(is_binary(EventId)),

    %% Get audit trail
    AuditTrail = erlmcp_compliance:get_audit_trail(soc2, {{2024,1,1},{0,0,0}}),
    ?assert(is_list(AuditTrail)),

    %% Search events
    SearchResults = erlmcp_compliance:search_audit_events(soc2, #{event_type => policy_change}, 10),
    ?assert(is_list(SearchResults)),

    ok.

test_evidence_collection(Config) ->
    ct:comment("Testing evidence collection for compliance"),

    %% Create evidence report
    EvidenceReport = erlmcp_compliance:generate_evidence_report(soc2, "control-123", #{evidence_type => all}),
    ?assert(is_map(EvidenceReport)),

    %% Verify evidence
    Evidence = EvidenceReport#{evidence},
    ?assert(is_list(Evidence)),

    ok.

%%====================================================================
%% Test cases - Monitoring
%%====================================================================

test_realtime_monitoring(Config) ->
    ct:comment("Testing real-time compliance monitoring"),

    %% Start continuous monitoring
    erlmcp_compliance:start_continuous_monitoring(),

    %% Get violations
    Violations = erlmcp_compliance:get_violations(soc2),
    ?assert(is_list(Violations)),

    %% Test monitoring dashboard
    Dashboard = erlmcp_compliance:get_compliance_dashboard(),
    ?assert(is_map(Dashboard)),

    ok.

test_compliance_scoring(Config) ->
    ct:comment("Testing compliance scoring calculation"),

    %% Calculate compliance score
    Score = erlmcp_compliance:calculate_compliance_score(soc2),
    ?assert(is_float(Score)),
    ?assert(Score >= 0.0 andalso Score =< 1.0),

    %% Test framework summary
    Summary = erlmcp_compliance:generate_framework_summary(soc2),
    ?assert(is_map(Summary)),

    ok.

%%====================================================================
%% Test cases - Reporting
%%====================================================================

test_compliance_report(Config) ->
    ct:comment("Testing compliance report generation"),

    %% Generate compliance report
    Report = erlmcp_compliance:generate_compliance_report(soc2, #{period => default}),
    ?assert(is_map(Report)),

    %% Verify report structure
    ?assert(is_binary(Report#{id})),
    ?assert(is_map(Report#{summary})),

    %% Export report
    Export = erlmcp_compliance:export_compliance_data(soc2, json),
    ?assert(is_binary(Export)),

    ok.

%%====================================================================
%% Test cases - Enforcement
%%====================================================================

test_policy_enforcement(Config) ->
    ct:comment("Testing policy enforcement"),

    %% Test policy enforcement
    Result = erlmcp_compliance:enforce_policy(soc2, "access", #{user_id => "user123", resource => "system"}),
    ?assert(is_map(Result)),

    %% Check decision
    Decision = Result#{decision},
    ?assert(Decision =:= allowed orelse Decision =:= denied),

    ok.

test_access_control(Config) ->
    ct:comment("Testing access control enforcement"),

    %% Test access control
    AccessResult = erlmcp_compliance:check_access("user123", "resource", "action"),
    ?assert(is_map(AccessResult)),

    %% Verify decision
    Decision = AccessResult#{decision},
    ?assert(Decision =:= allowed orelse Decision =:= denied),

    ok.

test_data_handling(Config) ->
    ct:comment("Testing data handling validation"),

    %% Test data handling policies
    DataResult = erlmcp_compliance:validate_data_handling("sensitive_data", "business"),
    ?assert(is_map(DataResult)),

    %% Test encryption requirements
    EncryptionResult = erlmcp_compliance:enforce_encryption("data", #{type => "sensitive"}),
    ?assert(is_map(EncryptionResult)),

    %% Test retention policies
    RetentionResult = erlmcp_compliance:enforce_retention("financial_data", 7, "compliance"),
    ?assert(is_map(RetentionResult)),

    ok.

%%====================================================================
%% Test cases - Analysis
%%====================================================================

test_risk_assessment(Config) ->
    ct:comment("Testing risk assessment functionality"),

    %% Perform risk assessment
    RiskAssessment = erlmcp_compliance:perform_risk_assessment(soc2, full),
    ?assert(is_map(RiskAssessment)),

    %% Check risk level
    RiskLevel = RiskAssessment#{risk_level},
    ?assert(RiskLevel =:= low orelse RiskLevel =:= medium orelse RiskLevel =:= high),

    ok.

test_benchmarking(Config) ->
    ct:comment("Testing industry benchmarking"),

    %% Benchmark against industry
    Benchmark = erlmcp_compliance:benchmark_against_industry(soc2),
    ?assert(is_map(Benchmark)),

    %% Verify benchmark data
    ?assert(is_float(Benchmark#{benchmark_score})),
    ?assert(is_float(Benchmark#{industry_average})),

    ok.

test_compliance_forecast(Config) ->
    ct:comment("Testing compliance forecasting"),

    %% Generate forecast
    ForecastId = erlmcp_compliance:generate_compliance_forecast(soc2, #{}, 12),
    ?assert(is_binary(ForecastId)),

    %% Analyze trends
    Trends = erlmcp_compliance:analyze_compliance_trends(soc2),
    ?assert(is_list(Trends)),

    ok.

%%====================================================================
%% Test cases - Integration
%%====================================================================

test_integration_with_core(Config) ->
    ct:comment("Testing integration with erlmcp core"),

    %% Test MCP resource functionality
    ResourceResult = erlmcp_compliance_app_resource:handle_resource_call(
                     "policy", "create",
                     #{framework => soc2, name => "Integration Test", category => "Test", controls => []},
                     #{frameworks => [soc2]}),
    ?assert(ResourceResult =:= {ok, _, _}),

    ok.

%%====================================================================
%% Helper functions
%%====================================================================

create_test_data() ->
    %% Create test data for all frameworks
    TestData = #{
        soc2 => [
            #{id => "test-cc6-1", name => "Access Control", controls => ["MFA", "RBAC"]},
            #{id => "test-cc6-6", name => "Security Logging", controls => ["Audit Trails"]}
        ],
        hipaa => [
            #{id => "test-164-312", name => "Technical Safeguards", controls => ["Encryption"]}
        ],
        gdpr => [
            #{id => "test-art-16", name => "Data Subject Rights", controls => ["Access", "Erasure"]}
        ],
        iso27001 => [
            #{id => "test-a9", name => "Access Control", controls => ["Authentication"]}
        ]
    },
    TestData.

reset_compliance_state() ->
    %% Reset compliance state for testing
    %% This would clear policies, audit logs, etc.
    ok.

cleanup_test_data() ->
    %% Clean up test data
    ok.

%%====================================================================
%% Convenience functions
%%====================================================================

handle_data_subject_request(RequestType, UserId) ->
    %% Mock implementation for testing
    #{
        request_type => RequestType,
        user_id => UserId,
        status => "processing",
        estimated_completion => "2024-12-31"
    }.

approve_policy(Framework, PolicyId, Approver) ->
    %% Mock approval implementation
    approved.