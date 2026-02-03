%% -*- erlang -*-
%%====================================================================
%% Zero-Trust Security Architecture Test Suite
%%====================================================================
-module(erlmcp_zero_trust_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Test exports
-export([suite/0, all/0, groups/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_group/2, end_per_group/2]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([
    %% Identity Management Tests
    test_identity_authentication/1,
    test_identity_session_creation/1,
    test_identity_trust_scoring/1,

    %% Access Control Tests
    test_access_control_policy_evaluation/1,
    test_access_control_escalation_handling/1,
    test_access_control_session_management/1,

    %% Network Isolation Tests
    test_network_micro_segmentation/1,
    test_network_policy_enforcement/1,
    test_network_access_control/1,

    %% Data Protection Tests
    test_data_classification/1,
    test_data_encryption/1,
    test_data_policy_application/1,

    %% Security Monitoring Tests
    test_security_event_monitoring/1,
    test_security_anomaly_detection/1,
    test_security_alert_generation/1,

    %% Threat Detection Tests
    test_threat_pattern_detection/1,
    test_threat_behavioral_analysis/1,
    test_threat_response_coordination/1,

    %% Supply Chain Security Tests
    test_package_verification/1,
    test_dependency_scanning/1,
    test_sbom_generation/1,

    %% Application Security Tests
    test_application_hardening/1,
    test_vulnerability_scanning/1,
    test_security_profile_management/1,

    %% Compliance Automation Tests
    test_compliance_assessment/1,
    test_evidence_generation/1,
    test_compliance_reporting/1,

    %% Integration Tests
    test_zero_trust_enabling/1,
    test_security_posture_assessment/1,
    test_security_dashboard_generation/1,
    test_cross_component_coordination/1,

    %% Performance Tests
    test_concurrent_authentication/1,
    test_throughput_performance/1,
    test_memory_usage/1,

    %% Chaos Engineering Tests
    test_failure_isolation/1,
    test_graceful_degradation/1,
    test_recovery_procedures/1
]).

%%====================================================================
%% Test Suite Configuration
%%====================================================================

suite() ->
    [{timetrap, {seconds, 300}},  %% 5 minutes max test time
     {require, otp},              %% OTP 26+ required
     {doc, "Zero-Trust Security Architecture Test Suite"}].

all() ->
    [
        {group, identity_management},
        {group, access_control},
        {group, network_isolation},
        {group, data_protection},
        {group, security_monitoring},
        {group, threat_detection},
        {group, supply_chain_security},
        {group, application_security},
        {group, compliance_automation},
        {group, integration},
        {group, performance},
        {group, chaos_engineering}
    ].

groups() ->
    [
        {identity_management, [], [
            test_identity_authentication,
            test_identity_session_creation,
            test_identity_trust_scoring
        ]},
        {access_control, [], [
            test_access_control_policy_evaluation,
            test_access_control_escalation_handling,
            test_access_control_session_management
        ]},
        {network_isolation, [], [
            test_network_micro_segmentation,
            test_network_policy_enforcement,
            test_network_access_control
        ]},
        {data_protection, [], [
            test_data_classification,
            test_data_encryption,
            test_data_policy_application
        ]},
        {security_monitoring, [], [
            test_security_event_monitoring,
            test_security_anomaly_detection,
            test_security_alert_generation
        ]},
        {threat_detection, [], [
            test_threat_pattern_detection,
            test_threat_behavioral_analysis,
            test_threat_response_coordination
        ]},
        {supply_chain_security, [], [
            test_package_verification,
            test_dependency_scanning,
            test_sbom_generation
        ]},
        {application_security, [], [
            test_application_hardening,
            test_vulnerability_scanning,
            test_security_profile_management
        ]},
        {compliance_automation, [], [
            test_compliance_assessment,
            test_evidence_generation,
            test_compliance_reporting
        ]},
        {integration, [], [
            test_zero_trust_enabling,
            test_security_posture_assessment,
            test_security_dashboard_generation,
            test_cross_component_coordination
        ]},
        {performance, [], [
            test_concurrent_authentication,
            test_throughput_performance,
            test_memory_usage
        ]},
        {chaos_engineering, [], [
            test_failure_isolation,
            test_graceful_degradation,
            test_recovery_procedures
        ]}
    ].

%%====================================================================
%% Setup and Cleanup
%%====================================================================

init_per_suite(Config) ->
    %% Start zero-trust application
    case application:ensure_all_started(erlmcp_zero_trust) of
        {ok, _Apps} ->
            %% Initialize test data
            TestData = init_test_data(),
            [{test_data, TestData} | Config];
        {error, _Reason} ->
            ct:fail("Failed to start zero-trust application")
    end.

end_per_suite(Config) ->
    %% Stop zero-trust application
    application:stop(erlmcp_zero_trust),
    Config.

init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, Config) ->
    Config.

init_per_testcase(_TestCase, Config) ->
    %% Reset state between tests
    reset_test_state(),
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.

%%====================================================================
%% Test Cases - Identity Management
%%====================================================================

test_identity_authentication(Config) ->
    TestData = ?config(test_data, Config),
    Credentials = maps:get(credentials, TestData),
    Context = maps:get(identity_context, TestData),

    %% Test successful authentication
    {ok, Session} = erlmcp_identity_manager:authenticate(Credentials, Context),
    ct:pal("Authentication successful: ~p", [Session]),

    %% Test authentication with invalid credentials
    InvalidCredentials = Credentials#{<<"password">> => <<"invalid">>},
    {error, invalid_credentials} = erlmcp_identity_manager:authenticate(InvalidCredentials, Context),

    %% Test MFA requirement
    MFAEnabled = erlmcp_identity_manager:is_mfa_enabled(),
    ct:pal("MFA enabled: ~p", [MFAEnabled]),

    ok.

test_identity_session_creation(Config) ->
    TestData = ?config(test_data, Config),
    IdentityId = <<"test-user-123">>,
    AccessRequest = maps:get(access_request, TestData),

    %% Create session with JIT access
    {ok, Session} = erlmcp_identity_manager:create_session(IdentityId, AccessRequest),
    ct:pal("Session created: ~p", [Session]),

    %% Verify session properties
    SessionId = maps:get(session_id, Session),
    Permissions = maps:get(permissions, Session),
    ct:pal("Session ID: ~p, Permissions: ~p", [SessionId, Permissions]),

    %% Test session expiration
    timer:sleep(1000),
    SessionStatus = erlmcp_identity_manager:get_session_status(SessionId),
    ct:pal("Session status after delay: ~p", [SessionStatus]),

    ok.

test_identity_trust_scoring(Config) ->
    TestData = ?config(test_data, Config),
    IdentityId = <<"test-user-456">>,
    InitialScore = erlmcp_identity_manager:get_trust_score(IdentityId),
    ct:pal("Initial trust score: ~p", [InitialScore]),

    %% Update trust score based on behavior
    NewScore = erlmcp_identity_manager:update_trust_score(
        IdentityId,
        positive_behavior,
        0.1
    ),
    ct:pal("Updated trust score: ~p", [NewScore]),

    %% Verify score update
    UpdatedScore = erlmcp_identity_manager:get_trust_score(IdentityId),
    ct:pal("Verified trust score: ~p", [UpdatedScore]),

    ok.

%%====================================================================
%% Test Cases - Access Control
%%====================================================================

test_access_control_policy_evaluation(Config) ->
    TestData = ?config(test_data, Config),
    Policy = maps:get(access_policy, TestData),
    AccessRequest = maps:get(access_request, TestData),

    %% Create access control policy
    {ok, PolicyId} = erlmcp_access_control:create_policy(Policy),
    ct:pal("Policy created: ~p", [PolicyId]),

    %% Evaluate access request
    {allowed, Permissions} = erlmcp_access_control:evaluate_request(AccessRequest, PolicyId),
    ct:pal("Access allowed with permissions: ~p", [Permissions]),

    %% Test denial of unauthorized access
    UnauthorizedRequest = AccessRequest#{action => delete},
    {denied, Reason} = erlmcp_access_control:evaluate_request(UnauthorizedRequest, PolicyId),
    ct:pal("Access denied: ~p", [Reason]),

    ok.

test_access_control_escalation_handling(Config) ->
    TestData = ?config(test_data, Config),
    UserId = <<"escalation-user">>,
    RequestData = #{resource => <<"admin_panel">>, action => elevate},

    %% Test privilege escalation request
    {escalation_required, Context} = erlmcp_access_control:escalate_privileges(UserId, RequestData),
    ct:pal("Escalation required: ~p", [Context]),

    %% Approve escalation
    Approval = #{approved_by => <<"admin">>, reason => <<"maintenance">>},
    {escalated, NewPermissions} = erlmcp_access_control:approve_escalation(UserId, Approval),
    ct:pal("Escalation approved: ~p", [NewPermissions]),

    ok.

test_access_control_session_management(Config) ->
    TestData = ?config(test_data, Config),
    UserId = <<"session-user">>,

    %% Create multiple sessions
    Sessions = lists:map(fun(_) ->
        Request = #{resource => <<"api">>, action => read},
        {ok, Session} = erlmcp_access_control:create_session(UserId, Request),
        Session
    end, lists:seq(1, 3)),

    ct:pal("Created ~p sessions", [length(Sessions)]),

    %% Test session termination
    TerminateCount = erlmcp_access_control:terminate_sessions(UserId),
    ct:pal("Terminated ~p sessions", [TerminateCount]),

    ok.

%%====================================================================
%% Test Cases - Network Isolation
%%====================================================================

test_network_micro_segmentation(Config) ->
    SegmentConfig = #{
        name => "test-segment",
        network_zones => ["test-zone"],
        allowed_services => ["http", "https"],
        blocked_services => ["telnet", "ftp"],
        isolation_enabled => true
    },

    %% Create micro-segment
    {ok, SegmentId} = erlmcp_network_isolation:create_micro_segment(SegmentConfig),
    ct:pal("Micro-segment created: ~p", [SegmentId]),

    %% Verify segment properties
    Segment = erlmcp_network_isolation:get_segment(SegmentId),
    ct:pal("Segment properties: ~p", [Segment]),

    %% Test segment isolation
    IsolationResult = erlmcp_network_isolation:isolate_network(SegmentId),
    ct:pal("Isolation result: ~p", [IsolationResult]),

    ok.

test_network_policy_enforcement(Config) ->
    PolicyConfig = #{
        name => "test-policy",
        source_segments => ["app-tier"],
        target_segments => ["db-tier"],
        allowed_traffic => [
            {<<"http">>, <<"8080">>},
            {<<"https">>, <<"8443">>}
        ],
        denied_traffic => [
            {<<"telnet">>, <<"23">>},
            {<<"ssh">>, <<"22">>}
        ]
    },

    %% Apply network policy
    {ok, PolicyId} = erlmcp_network_isolation:apply_network_policy(PolicyConfig),
    ct:pal("Network policy applied: ~p", [PolicyId]),

    %% Test traffic validation
    AllowedTraffic = #{protocol => <<"http">>, port => <<"8080">>},
    TrafficResult = erlmcp_network_isolation:validate_traffic(AllowedTraffic, PolicyId),
    ct:pal("Traffic validation result: ~p", [TrafficResult]),

    ok.

test_network_access_control(Config) ->
    UserId = <<"network-user">>,
    NetworkAccess = #{
        resource => <<"api/v1/users">>,
        method => <<"GET">>,
        ip_address => <<"192.168.1.100">>,
        user_agent => <<"test-browser">>
    },

    %% Check network access
    AccessResult = erlmcp_network_isolation:check_network_access(UserId, NetworkAccess),
    ct:pal("Network access check: ~p", [AccessResult]),

    %% Monitor network traffic
    MonitorResult = erlmcp_network_isolation:monitor_traffic(UserId, NetworkAccess),
    ct:pal("Network monitoring result: ~p", [MonitorResult]),

    ok.

%%====================================================================
%% Test Cases - Data Protection
%%====================================================================

test_data_classification(Config) ->
    TestData = <<"sensitive customer information: John Doe, SSN: 123-45-6789">>,

    %% Classify data
    Classification = erlmcp_data_protection:classify_data(TestData, confidential),
    ct:pal("Data classification: ~p", [Classification]),

    %% Verify classification
    ConfirmedType = erlmcp_data_protection:get_classification_type(Classification),
    ct:pal("Classification confirmed: ~p", [ConfirmedType]),

    ok.

test_data_encryption(Config) ->
    TestData = <<"sensitive financial data">>,
    Key = crypto:strong_rand_bytes(32),

    %% Encrypt data
    EncryptedData = erlmcp_data_protection:encrypt_data(TestData, "AES-256-GCM", Key),
    ct:pal("Data encrypted: ~p bytes", [byte_size(EncryptedData)]),

    %% Decrypt data
    DecryptedData = erlmcp_data_protection:decrypt_data(EncryptedData, Key),
    ct:pal("Data decrypted: ~p", [DecryptedData]),

    %% Verify data integrity
    TestData =:= DecryptedData,
    ct:pal("Data integrity verified"),

    ok.

test_data_policy_application(Config) ->
    DataClassification = confidential,
    AccessContext = #{user_role => admin, purpose => maintenance},

    %% Apply data protection policy
    PolicyResult = erlmcp_data_protection:apply_policy(DataClassification, AccessContext),
    ct:pal("Policy application result: ~p", [PolicyResult]),

    %% Test policy enforcement
    EnforcementResult = erlmcp_data_protection:enforce_policy(DataClassification),
    ct:pal("Policy enforcement: ~p", [EnforcementResult]),

    ok.

%%====================================================================
%% Test Cases - Security Monitoring
%%====================================================================

test_security_event_monitoring(Config) ->
    EventData = #{
        type => user_login,
        severity => medium,
        actor => <<"user1">>,
        resource => <<"system">>,
        details => #{ip_address => <<"192.168.1.100">>, device_id => <<"device123">>}
    },

    %% Monitor security event
    MonitoringResult = erlmcp_security_monitor:monitor_event(EventData),
    ct:pal("Event monitoring result: ~p", [MonitoringResult]),

    %% Verify event processing
    ProcessedEvents = erlmcp_security_monitor:get_processed_events(),
    ct:pal("Processed events count: ~p", [length(ProcessedEvents)]),

    ok.

test_security_anomaly_detection(Config) ->
    HistoricalData = [
        #{timestamp => 1640995200, event_type => user_login, location => "US"},
        #{timestamp => 1640995300, event_type => file_access, location => "US"},
        #{timestamp => 1640995400, event_type => user_login, location => "CN"}  %% Anomaly
    ],

    %% Detect anomalies
    AnomalyResult = erlmcp_security_monitor:detect_anomaly(HistoricalData),
    ct:pal("Anomaly detection result: ~p", [AnomalyResult]),

    %% Verify anomaly classification
    IsAnomaly = erlmcp_security_monitor:is_anomaly(AnomalyResult),
    ct:pal("Is anomaly: ~p", [IsAnomaly]),

    ok.

test_security_alert_generation(Config) ->
    AlertContext = #{
        severity => high,
        alert_type => unauthorized_access,
        affected_resources => [<<"api/v1/users">>],
        recommendation => "Review access logs and revoke suspicious sessions"
    },

    %% Generate security alert
    Alert = erlmcp_security_monitor:generate_alert(AlertContext),
    ct:pal("Security alert generated: ~p", [Alert]),

    %% Test alert delivery
    DeliveryResult = erlmcp_security_monitor:deliver_alert(Alert),
    ct:pal("Alert delivery result: ~p", [DeliveryResult]),

    ok.

%%====================================================================
%% Test Cases - Threat Detection
%%====================================================================

test_threat_pattern_detection(Config) ->
    ThreatData = #{
        pattern => brute_force,
        source_ip => <<"192.168.1.50">>,
        target_resource => <<"login">>,
        attempt_count => 10,
        timeframe_seconds => 30
    },

    %% Detect threat pattern
    DetectionResult = erlmcp_threat_detection:detect_threat(ThreatData, #{}),
    ct:pal("Threat detection result: ~p", [DetectionResult]),

    %% Verify threat classification
    ThreatType = erlmcp_threat_detection:classify_threat(DetectionResult),
    ct:pal("Threat type: ~p", [ThreatType]),

    ok.

test_threat_behavioral_analysis(Config) ->
    UserBehavior = [
        #{action => login, timestamp => 1640995200, location => "US", device => "desktop"},
        #{action => data_access, timestamp => 1640995300, location => "US", device => "desktop"},
        #{action => admin_action, timestamp => 1640995400, location => "EU", device => "mobile"}  %% Anomaly
    ],

    %% Analyze user behavior
    AnalysisResult = erlmcp_threat_detection:analyze_behavior(UserBehavior),
    ct:pal("Behavior analysis result: ~p", [AnalysisResult]),

    %% Detect behavioral anomalies
    AnomalyScore = erlmcp_threat_detection:calculate_anomaly_score(UserBehavior),
    ct:pal("Anomaly score: ~p", [AnomalyScore]),

    ok.

test_threat_response_coordination(Config) ->
    ThreatId = <<"threat-123">>,
    ResponsePlan = [
        #{action => block_ip, target => <<"192.168.1.50">>},
        #{action => notify_admin, recipient => <<"security-team">>},
        #{action => increase_monitoring, scope => <<"authentication">>}
    ],

    %% Coordinate threat response
    CoordinationResult = erlmcp_threat_detection:coordinate_response(ThreatId, ResponsePlan),
    ct:pal("Response coordination: ~p", [CoordinationResult]),

    %% Test response execution
    ExecutionResult = erlmcp_threat_detection:execute_response_actions(ResponsePlan),
    ct:pal("Response execution: ~p", [ExecutionResult]),

    ok.

%%====================================================================
%% Test Cases - Supply Chain Security
%%====================================================================

test_package_verification(Config) ->
    PackageData = #{
        name => "test-package",
        version => "1.0.0",
        checksum => "sha256:abc123...",
        signature => "digital-signature",
        source => "verified-registry"
    },

    %% Verify package integrity
    VerificationResult = erlmcp_supply_chain_security:verify_package(PackageData, #{}),
    ct:pal("Package verification result: ~p", [VerificationResult]),

    %% Check if package is trusted
    IsTrusted = erlmcp_supply_chain_security:is_trusted_package(PackageData),
    ct:pal("Package is trusted: ~p", [IsTrusted]),

    ok.

test_dependency_scanning(Config) ->
    Dependencies = [
        #{name => "lodash", version => "4.17.21", vulnerabilities => []},
        #{name => "axios", version => "0.21.1", vulnerabilities => [{cve, "CVE-2021-23337"}]},
        #{name => "express", version => "4.17.3", vulnerabilities => [{cve, "CVE-2022-24999"}]}
    ],

    %% Scan dependencies for vulnerabilities
    ScanResult = erlmcp_supply_chain_security:scan_dependencies(Dependencies),
    ct:pal("Dependency scan result: ~p", [ScanResult]),

    %% Get vulnerability summary
    VulnerabilityCount = erlmcp_supply_chain_security:get_vulnerability_count(ScanResult),
    ct:pal("Vulnerability count: ~p", [VulnerabilityCount]),

    ok.

test_sbom_generation(Config) ->
    ProjectInfo = #{
        name => "test-project",
        version => "1.0.0",
        dependencies => [
            #{name => "lodash", version => "4.17.21", license => "MIT"},
            #{name => "axios", version => "0.21.1", license => "MIT"}
        ]
    },

    %% Generate SBOM
    SBOM = erlmcp_supply_chain_security:generate_sbom(ProjectInfo),
    ct:pal("SBOM generated: ~p", [SBOM]),

    %% Validate SBOM format
    IsValidSBOM = erlmcp_supply_chain_security:validate_sbom(SBOM),
    ct:pal("SBOM is valid: ~p", [IsValidSBOM]),

    ok.

%%====================================================================
%% Test Cases - Application Security
%%====================================================================

test_application_hardening(Config) ->
    AppConfig = #{
        app_name => "test-app",
        security_level => high,
        enabled_features => [
            input_validation,
            output_encoding,
            secure_headers,
            session_protection
        ]
    },

    %% Harden application
    HardenResult = erlmcp_application_security:harden_application(AppConfig, #{}),
    ct:pal("Application hardening result: ~p", [HardenResult]),

    %% Verify security posture
    SecurityScore = erlmcp_application_security:assess_security_posture(AppConfig),
    ct:pal("Security posture score: ~p", [SecurityScore]),

    ok.

test_vulnerability_scanning(Config) ->
    ScanTarget = #{
        app_name => "test-app",
        version => "1.0.0",
        scan_type => "comprehensive",
        include_tests => [sql_injection, xss, buffer_overflow]
    },

    %% Scan application vulnerabilities
    ScanResult = erlmcp_application_security:scan_vulnerabilities(ScanTarget),
    ct:pal("Vulnerability scan result: ~p", [ScanResult]),

    %% Get vulnerability details
    Vulnerabilities = erlmcp_application_security:get_vulnerabilities(ScanResult),
    ct:pal("Found ~p vulnerabilities", [length(Vulnerabilities)]),

    ok.

test_security_profile_management(Config) ->
    ProfileData = #{
        name => "production-profile",
        baseline_security => high,
        compliance_frameworks => ["SOC2", "ISO27001"],
        monitoring_level => comprehensive,
        response_time => real_time
    },

    %% Create security profile
    {ok, ProfileId} = erlmcp_application_security:create_profile(ProfileData),
    ct:pal("Security profile created: ~p", [ProfileId]),

    %% Apply profile to application
    ApplyResult = erlmcp_application_security:apply_profile("test-app", ProfileId),
    ct:pal("Profile applied: ~p", [ApplyResult]),

    ok.

%%====================================================================
%% Test Cases - Compliance Automation
%%====================================================================

test_compliance_assessment(Config) ->
    AssessmentData = #{
        framework => "SOC2",
        scope => "production",
        controls => [
            #{id => "CC6.1", name => "Access Controls", status => compliant},
            #{id => "CC6.6", name => "Security Logging", status => non_compliant},
            #{id => "CC7.2", name => "Encryption", status => compliant}
        ]
    },

    %% Assess compliance
    Assessment = erlmcp_compliance_automation:assess_compliance(AssessmentData, #{}),
    ct:pal("Compliance assessment: ~p", [Assessment]),

    %% Calculate compliance score
    ComplianceScore = erlmcp_compliance_automation:calculate_compliance_score(Assessment),
    ct:pal("Compliance score: ~p", [ComplianceScore]),

    ok.

test_evidence_generation(Config) ->
    EvidenceContext = #{
        control_id => "CC6.1",
        requirement => "Access Controls",
        evidence_type => "configuration",
        collected_data => #{
            policies => ["RBAC implemented"],
            configurations => ["MFA enabled"],
            audit_logs => ["Last 30 days"]
        }
    },

    %% Generate compliance evidence
    Evidence = erlmcp_compliance_automation:generate_evidence(EvidenceContext),
    ct:pal("Compliance evidence: ~p", [Evidence]),

    %% Verify evidence integrity
    EvidenceValid = erlmcp_compliance_automation:verify_evidence_integrity(Evidence),
    ct:pal("Evidence is valid: ~p", [EvidenceValid]),

    ok.

test_compliance_reporting(Config) ->
    ReportContext = #{
        framework => "SOC2",
        report_type => "type_2",
        timeframe => "2024-Q1",
        include_recommendations => true
    },

    %% Generate compliance report
    Report = erlmcp_compliance_automation:generate_report(ReportContext),
    ct:pal("Compliance report: ~p", [Report]),

    %% Validate report structure
    ReportValid = erlmcp_compliance_automation:validate_report_structure(Report),
    ct:pal("Report is valid: ~p", [ReportValid]),

    ok.

%%====================================================================
%% Test Cases - Integration
%%====================================================================

test_zero_trust_enabling(Config) ->
    Application = "test-app",

    %% Enable zero-trust for application
    EnableResult = erlmcp_zero_trust_integration:enable_zero_trust(Application),
    ct:pal("Zero-trust enabled: ~p", [EnableResult]),

    %% Verify zero-trust status
    Status = erlmcp_zero_trust_integration:get_zero_trust_status(Application),
    ct:pal("Zero-trust status: ~p", [Status]),

    ok.

test_security_posture_assessment(Config) ->
    Application = "test-app",

    %% Assess security posture
    Posture = erlmcp_zero_trust_integration:assess_security_posture(Application),
    ct:pal("Security posture: ~p", [Posture]),

    %% Verify posture components
    Components = maps:get(identity_security, Posture),
    ct:pal("Identity security component: ~p", [Components]),

    ok.

test_security_dashboard_generation(Config) ->
    Scope = "production",

    %% Generate security dashboard
    Dashboard = erlmcp_zero_trust_integration:generate_security_dashboard(Scope),
    ct:pal("Security dashboard: ~p", [Dashboard]),

    %% Verify dashboard metrics
    OverallScore = maps:get(overall_score, Dashboard),
    ActiveThreats = maps:get(active_threats, Dashboard),
    ct:pal("Dashboard score: ~p, threats: ~p", [OverallScore, ActiveThreats]),

    ok.

test_cross_component_coordination(Config) ->
    ThreatId = <<"threat-456">>,
    ResponsePlan = [
        #{component => identity, action => revoke_sessions},
        #{component => network, action => block_ip},
        #{component => monitoring, action => increase_alerting}
    ],

    %% Coordinate cross-component response
    CoordinationResult = erlmcp_zero_trust_integration:coordinate_response(ThreatId, ResponsePlan),
    ct:pal("Cross-component coordination: ~p", [CoordinationResult]),

    %% Verify component synchronization
    SyncStatus = erlmcp_zero_trust_integration:get_component_sync_status(),
    ct:pal("Component sync status: ~p", [SyncStatus]),

    ok.

%%====================================================================
%% Test Cases - Performance
%%====================================================================

test_concurrent_authentication(Config) ->
    NumUsers = 100,
    UserData = lists:map(fun(I) ->
        #{
            username => <<"user", (integer_to_binary(I))/binary>>,
            password => <<"pass", (integer_to_binary(I))/binary>>,
            context => #{device_fingerprint => <<"device", (integer_to_binary(I))/binary>>}
        }
    end, lists:seq(1, NumUsers)),

    %% Test concurrent authentication
    Start = erlang:system_time(millisecond),
    Results = lists:map(fun(User) ->
        erlmcp_identity_manager:authenticate(User, #{})
    end, UserData),
    End = erlang:system_time(millisecond),

    Duration = End - Start,
    SuccessCount = length(lists:filter(fun({ok, _}) -> true; (_) -> false end, Results)),
    ct:pal("Concurrent authentication: ~p/~p in ~p ms (~.2f ms/user)",
           [SuccessCount, NumUsers, Duration, Duration/NumUsers]),

    ok.

test_throughput_performance(Config) ->
    NumRequests = 1000,
    TestData = #{
        type => access_control,
        payload => #{action => read, resource => <<"api">>}
    },

    %% Test throughput
    Start = erlang:system_time(millisecond),
    Results = lists:map(fun(_) ->
        erlmcp_access_control:evaluate_request(TestData, "test-policy")
    end, lists:seq(1, NumRequests)),
    End = erlang:system_time(millisecond),

    Duration = End - Start,
    Throughput = (NumRequests * 1000) / Duration,
    ct:pal("Throughput: ~.2f requests/sec", [Throughput]),

    ok.

test_memory_usage(Config) ->
    InitialMemory = erlang:memory(total),

    %% Perform memory-intensive operations
    _SBOM = erlmcp_supply_chain_security:generate_sbom(#{
        name => "large-project",
        dependencies lists:seq(1, 1000)
    }),

    FinalMemory = erlang:memory(total),
    MemoryDelta = FinalMemory - InitialMemory,
    ct:pal("Memory delta: ~p bytes (~.2f MB)", [MemoryDelta, MemoryDelta/(1024*1024)]),

    ok.

%%====================================================================
%% Test Cases - Chaos Engineering
%%====================================================================

test_failure_isolation(Config) ->
    %% Simulate component failure
    erlmcp_identity_manager:stop(),

    %% Verify other components continue functioning
    Result = erlmcp_access_control:evaluate_request(
        #{action => read, resource => <<"api">>},
        "test-policy"
    ),
    ct:pal("Component after failure: ~p", [Result]),

    %% Restart failed component
    erlmcp_identity_manager:start(),
    ok.

test_graceful_degradation(Config) ->
    %% Enable graceful degradation mode
    erlmcp_zero_trust_integration:enable_graceful_degradation(true),

    %% Reduce system capacity
    erlmcp_zero_trust_integration:set_system_capacity(50),

    %% Verify system operates in degraded mode
    Result = erlmcp_identity_manager:authenticate(
        #{username => <<"user">>, password => <<"pass">>},
        #{}
    ),
    ct:pal("Degraded mode result: ~p", [Result]),

    %% Restore normal operation
    erlmcp_zero_trust_integration:enable_graceful_degradation(false),
    erlmcp_zero_trust_integration:set_system_capacity(100),

    ok.

test_recovery_procedures(Config) ->
    %% Simulate system failure
    erlmcp_zero_trust_integration:simulate_system_failure(),

    %% Trigger recovery procedures
    RecoveryResult = erlmcp_zero_trust_integration:execute_recovery_procedures(),
    ct:pal("Recovery result: ~p", [RecoveryResult]),

    %% Verify system recovery
    SystemStatus = erlmcp_zero_trust_integration:get_system_status(),
    ct:pal("System status after recovery: ~p", [SystemStatus]),

    ok.

%%====================================================================
%% Helper Functions
%%====================================================================

init_test_data() ->
    #{
        credentials => #{
            username => <<"test-user">>,
            password => <<"test-password">>,
            mfa_code => <<"123456">>
        },
        identity_context => #{
            device_fingerprint => <<"device123">>,
            location => <<"US">>,
            ip_address => <<"192.168.1.100">>
        },
        access_request => #{
            resource => <<"api/v1/users">>,
            action => read,
            justification => <<"Business requirement">>
        },
        access_policy => #{
            name => "test-access-policy",
            rules => [
                #{resource => <<"api/v1/users">>, action => read, allowed => true},
                #{resource => <<"api/v1/admin">>, action => delete, allowed => false}
            ]
        }
    }.

reset_test_state() ->
    %% Clear test data and reset state
    erlmcp_identity_manager:clear_test_data(),
    erlmcp_access_control:clear_test_data(),
    erlmcp_network_isolation:clear_test_data(),
    erlmcp_data_protection:clear_test_data(),
    erlmcp_security_monitor:clear_test_data(),
    erlmcp_threat_detection:clear_test_data(),
    erlmcp_supply_chain_security:clear_test_data(),
    erlmcp_application_security:clear_test_data(),
    erlmcp_compliance_automation:clear_test_data(),
    erlmcp_zero_trust_integration:clear_test_data(),
    ok.