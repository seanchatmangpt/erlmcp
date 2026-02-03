-module(erlmcp_compliance_monitor).

-behaviour(gen_server).

-export([start_link/0, check_compliance/1, get_compliance_status/0, generate_report/1, update_framework/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(COMPLIANCE_INTERVAL, 3600000). % 1 hour
-define(REPORT_DIR, "/var/log/erlmcp/compliance").
-define(MAX_AUDIT_LOGS, 10000).

-record(compliance_framework, {
    id :: binary(),
    name :: binary(),
    version :: binary(),
    requirements :: list(),
    controls :: list(),
    last_updated :: integer()
}).

-record(control_requirement, {
    id :: binary(),
    title :: binary(),
    description :: binary(),
    category :: binary(),
    severity :: binary(),
    evidence_required :: list(),
    automation_level :: binary()
}).

-record(compliance_check, {
    id :: binary(),
    framework :: binary(),
    control_id :: binary(),
    status :: binary(),
    evidence :: list(),
    findings :: list(),
    next_review :: integer(),
    last_checked :: integer()
}).

-record(compliance_state, {
    framework_id :: binary(),
    overall_status :: binary(),
    compliance_percentage :: float(),
    total_controls :: integer(),
    passed_controls :: integer(),
    failed_controls :: integer(),
    pending_controls :: integer(),
    last_assessment :: integer(),
    next_review :: integer(),
    audit_trail :: list()
}).

-record(state, {
    frameworks :: ets:tid(),
    checks :: ets:tid(),
    audit_trail :: ets:tid(),
    requirements :: ets:tid(),
    current_checks :: list(),
    report_templates :: map(),
    compliance_threshold :: float()
}).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec check_compliance(binary()) -> {ok, map()} | {error, term()}.
check_compliance(FrameworkId) when is_binary(FrameworkId) ->
    gen_server:call(?SERVER, {check_compliance, FrameworkId}).

-spec get_compliance_status() -> list().
get_compliance_status() ->
    gen_server:call(?SERVER, get_compliance_status).

-spec generate_report(binary()) -> {ok, binary()} | {error, term()}.
generate_report(FrameworkId) when is_binary(FrameworkId) ->
    gen_server:call(?SERVER, {generate_report, FrameworkId}).

-spec update_framework(binary(), binary()) -> ok | {error, term()}.
update_framework(FrameworkId, FrameworkData) when is_binary(FrameworkId), is_binary(FrameworkData) ->
    gen_server:call(?SERVER, {update_framework, FrameworkId, FrameworkData}).

%%====================================================================
%% Gen Server Callbacks
%%====================================================================

-spec init(term()) -> {ok, #state{}} | {stop, term()}.
init(_Args) ->
    process_flag(trap_exit, true),

    %% Initialize compliance frameworks registry
    Frameworks = ets:new(compliance_frameworks, [
        set, private, {keypos, 1},
        {write_concurrency, true}, {read_concurrency, true}
    ]),

    %% Initialize compliance checks registry
    Checks = ets:new(compliance_checks, [
        set, private, {keypos, 1},
        {write_concurrency, true}, {read_concurrency, true}
    ]),

    %% Initialize audit trail
    AuditTrail = ets:new(compliance_audit, [
        set, private, {keypos, 1},
        {write_concurrency, true}, {read_concurrency, true}
    ]),

    %% Initialize requirements registry
    Requirements = ets:new(compliance_requirements, [
        set, private, {keypos, 1},
        {write_concurrency, true}, {read_concurrency, true}
    ]),

    %% Load compliance frameworks
    load_frameworks(Frameworks, Requirements),

    %% Initialize state
    State = #state{
        frameworks = Frameworks,
        checks = Checks,
        audit_trail = AuditTrail,
        requirements = Requirements,
        current_checks = [],
        report_templates = load_report_templates(),
        compliance_threshold = application:get_env(erlmcp_security_monitoring, compliance_threshold, 90.0)
    },

    %% Start periodic compliance checks
    erlang:send_after(?COMPLIANCE_INTERVAL, self(), periodic_compliance_check),

    %% Start audit cleanup timer
    erlang:send_after(86400000, self(), cleanup_audit_logs), % 24 hours

    {ok, State}.

-spec handle_call(term(), {pid(), term()}, #state{}) -> {reply, term(), #state{}} | {noreply, #state{}}.
handle_call({check_compliance, FrameworkId}, _From, State) ->
    %% Perform compliance check for specified framework
    case ets:lookup(State#state.frameworks, FrameworkId) of
        [{_, Framework}] ->
            ComplianceState = perform_compliance_check(Framework, State),

            %% Update compliance state
            update_compliance_state(ComplianceState, State),

            %% Log compliance check
            log_compliance_check(FrameworkId, ComplianceState, State),

            {reply, {ok, ComplianceState}, State};
        [] ->
            {reply, {error, framework_not_found}, State}
    end;

handle_call(get_compliance_status, _From, State) ->
    %% Get compliance status for all frameworks
    Status = lists:foldl(fun({FrameworkId, _Framework}, Acc) ->
        case ets:lookup(State#state.checks, FrameworkId) of
            [{_, ComplianceState}] ->
                maps:put(FrameworkId, ComplianceState, Acc);
            [] ->
                maps:put(FrameworkId, undefined, Acc)
        end
    end, #{}, ets:tab2list(State#state.frameworks)),

    {reply, Status, State};

handle_call({generate_report, FrameworkId}, _From, State) ->
    %% Generate compliance report
    case ets:lookup(State#state.frameworks, FrameworkId) of
        [{_, Framework}] ->
            Report = generate_compliance_report(Framework, State),
            ReportId = generate_report_id(),
            save_report(ReportId, Report, State),
            {reply, {ok, ReportId}, State};
        [] ->
            {reply, {error, framework_not_found}, State}
    end;

handle_call({update_framework, FrameworkId, FrameworkData}, _From, State) ->
    %% Update compliance framework
    case parse_framework_data(FrameworkData) of
        {ok, UpdatedFramework} ->
            ets:insert(State#state.frameworks, UpdatedFramework),
            ets:insert(State#state.requirements, UpdatedFramework#compliance_framework.requirements),
            log_framework_update(FrameworkId, UpdatedFramework, State),
            {reply, ok, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info(periodic_compliance_check, State) ->
    %% Perform periodic compliance checks for all frameworks
    lists:foreach(fun({FrameworkId, Framework}) ->
        ComplianceState = perform_compliance_check(Framework, State),
        update_compliance_state(ComplianceState, State),
        log_compliance_check(FrameworkId, ComplianceState, State)
    end, ets:tab2list(State#state.frameworks)),

    %% Check for compliance violations
    check_compliance_violations(State),

    %% Continue periodic checks
    erlang:send_after(?COMPLIANCE_INTERVAL, self(), periodic_compliance_check),

    {noreply, State};

handle_info(cleanup_audit_logs, State) ->
    %% Clean up old audit logs
    cleanup_old_audit_logs(State),

    %% Continue cleanup
    erlang:send_after(86400000, self(), cleanup_audit_logs),

    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

load_frameworks(FrameworksTable, RequirementsTable) ->
    %% Load compliance frameworks
    DefaultFrameworks = [
        #compliance_framework{
            id => <<"NIST_SP_800_53">>,
            name => "NIST Special Publication 800-53",
            version => "4.0",
            requirements = load_nist_requirements(),
            controls = load_nist_controls(),
            last_updated = erlang:system_time(second)
        },
        #compliance_framework{
            id => <<"ISO_27001">>,
            name => "ISO/IEC 27001:2013",
            version => "2013",
            requirements = load_iso_requirements(),
            controls = load_iso_controls(),
            last_updated = erlang:system_time(second)
        },
        #compliance_framework{
            id => <<"SOC2">>,
            name => "Service Organization Control 2",
            version => "2017",
            requirements = load_soc2_requirements(),
            controls = load_soc2_controls(),
            last_updated = erlang:system_time(second)
        },
        #compliance_framework{
            id => <<"GDPR">>,
            name => "General Data Protection Regulation",
            version => "2018",
            requirements = load_gdpr_requirements(),
            controls = load_gdpr_controls(),
            last_updated = erlang:system_time(second)
        },
        #compliance_framework{
            id => <<"HIPAA">>,
            name => "Health Insurance Portability and Accountability Act",
            version => "2013",
            requirements = load_hipaa_requirements(),
            controls = load_hipaa_controls(),
            last_updated = erlang:system_time(second)
        },
        #compliance_framework{
            id => <<"PCI_DSS">>,
            name => "Payment Card Industry Data Security Standard",
            version => "3.2.1",
            requirements = load_pci_requirements(),
            controls = load_pci_controls(),
            last_updated = erlang:system_time(second)
        }
    ],

    %% Insert frameworks and requirements
    lists:foreach(fun(Framework) ->
        ets:insert(FrameworksTable, Framework),
        lists:foreach(fun(Req) ->
            ets:insert(RequirementsTable, Req)
        end, Framework#compliance_framework.requirements)
    end, DefaultFrameworks).

load_nist_requirements() ->
    %% Load NIST SP 800-53 requirements
    [
        #control_requirement{
            id => <<"AC-1">>,
            title => "Access Control Policy and Procedures",
            description => "The organization develops, documents, and disseminates to [Assignment: organization-defined personnel or roles]:
                a. An access control policy that addresses purpose, scope, roles, responsibilities, management commitment, coordination among organizational entities, and compliance; and
                b. Procedures to facilitate the implementation of the access control policy and associated access controls.",
            category => "Access Control",
            severity => "high",
            evidence_required = ["policy_document", "procedures_document", "implementation_guide"],
            automation_level = "partial"
        },
        #control_requirement{
            id => <<"AU-1">>,
            title => "Audit and Accountability Policy and Procedures",
            description => "The organization:
                a. Develops, documents, and disseminates to [Assignment: organization-defined personnel or roles]:
                    1. An audit and accountability policy that addresses purpose, scope, roles, responsibilities, management commitment, coordination among organizational entities, and compliance; and
                    2. Procedures to facilitate the implementation of the audit and accountability policy and associated audit and accountability controls.",
            category = "Audit and Accountability",
            severity = "high",
            evidence_required = ["audit_policy", "procedures", "implementation_evidence"],
            automation_level = "partial"
        },
        %% Additional NIST requirements...
        #control_requirement{
            id => <<"SC-13">>,
            title => "Cryptographic Protection",
            description => "The organization employs cryptographic mechanisms to protect the confidentiality and integrity of information in storage [(a) and (b) in SUPP-PORT] and [(c) and (d) in SUPP-PORT].",
            category = "System and Communications Protection",
            severity => "high",
            evidence_required = ["crypto_policy", "implementation", "key_management"],
            automation_level = "full"
        }
    ].

load_iso_requirements() ->
    %% Load ISO 27001 requirements
    [
        #control_requirement{
            id => <<"A.5.1.1">>,
            title => "Information security policies",
            description => "An information security policy shall be defined, approved by management, and communicated to employees.",
            category => "Information Security Policies",
            severity => "high",
            evidence_required = ["policy_document", "approval_document", "communication_records"],
            automation_level = "manual"
        },
        #control_requirement{
            id => <<"A.6.1.2">>,
            title => "Competence",
            description => "Competent personnel shall be ensured.",
            category => "Human Resource Security",
            severity => "medium",
            evidence_required = ["competency_records", "training_records", "performance_reviews"],
            automation_level = "partial"
        }
    ].

load_soc2_requirements() ->
    %% Load SOC 2 requirements
    [
        #control_requirement{
            id => <<"CC6.1">>,
            title => "Logical access controls",
            description => "The service organization restricts access to systems and data to authorized users.",
            category => "Access Controls",
            severity => "high",
            evidence_required = ["access_policy", "user_accounts", "access_reviews"],
            automation_level = "partial"
        },
        #control_requirement{
            id => <<"CC6.6">>,
            title => "Security event logging",
            description => "The service organization maintains security event logs.",
            category => "Operations",
            severity => "medium",
            evidence_required = ["log_configuration", "retention_policy", "log_analysis"],
            automation_level = "partial"
        }
    ].

load_gdpr_requirements() ->
    %% Load GDPR requirements
    [
        #control_requirement{
            id => <<"GDPR_Article_32">>,
            title => "Security of processing",
            description => "The controller and the processor shall implement appropriate technical and organisational measures to ensure a level of security appropriate to the risk.",
            category => "Security of Processing",
            severity => "critical",
            evidence_required = ["risk_assessment", "security_measures", "documentation"],
            automation_level = "partial"
        },
        #control_requirement{
            id => <<"GDPR_Article_33">>,
            title => "Notification of a personal data breach to the supervisory authority",
            description => "Where a personal data breach is likely to result in a high risk to the rights and freedoms of natural persons, the controller shall notify the personal data breach to the supervisory authority.",
            category => "Breach Notification",
            severity => "high",
            evidence_required = ["breach_procedures", "notification_templates", "test_records"],
            automation_level = "partial"
        }
    ].

load_hipaa_requirements() ->
    %% Load HIPAA requirements
    [
        #control_requirement{
            id => <<"HIPAA_164.306">>,
            title => "Security standards: General rules",
            description => "The covered entity must implement policies and procedures to prevent, detect, contain, and correct security violations.",
            category => "Security Standards",
            severity = "high",
            evidence_required = ["security_policies", "procedures", "implementation_evidence"],
            automation_level = "partial"
        },
        #control_requirement{
            id => <<"HIPAA_164.312">>,
            title => "Technical safeguards",
            description => "The covered entity must implement technical policies and procedures for electronic protected health information.",
            category = "Technical Safeguards",
            severity = "high",
            evidence_required = ["technical_controls", "access_controls", "audit_logs"],
            automation_level = "full"
        }
    ].

load_pci_requirements() ->
    %% Load PCI DSS requirements
    [
        #control_requirement{
            id => <<"PCI_Req_1">>,
            title => "Install and maintain a firewall configuration to protect cardholder data",
            description => "Establish and maintain a configuration standard for all system components.",
            category = "Network Security",
            severity = "critical",
            evidence_required = ["firewall_config", "change_control", "testing_records"],
            automation_level = "partial"
        },
        #control_requirement{
            id => <<"PCI_Req_2">>,
            title => "Do not use vendor-supplied defaults for system passwords and other security parameters",
            description => "Always change vendor-supplied defaults before installing a system on the network.",
            category = "Access Control",
            severity = "high",
            evidence_required = ["password_policy", "change_records", "validation"],
            automation_level = "full"
        }
    ].

load_nist_controls() ->
    %% Load NIST control implementations
    [].

load_iso_controls() ->
    %% Load ISO control implementations
    [].

load_soc2_controls() ->
    %% Load SOC2 control implementations
    [].

load_gdpr_controls() ->
    %% Load GDPR control implementations
    [].

load_hipaa_controls() ->
    %% Load HIPAA control implementations
    [].

load_pci_controls() ->
    %% Load PCI DSS control implementations
    [].

load_report_templates() ->
    %% Load compliance report templates
    #{
        executive => template_executive(),
        detailed => template_detailed(),
        summary => template_summary(),
        remediation => template_remediation()
    }.

template_executive() ->
    #{
        title => "Executive Compliance Report",
        sections => [
            {"Executive Summary", "executive_summary"},
            {"Overall Compliance", "overall_compliance"},
            {"Key Findings", "key_findings"},
            {"Risk Assessment", "risk_assessment"},
            {"Recommendations", "recommendations"},
            {"Next Steps", "next_steps"}
        ],
        format => "pdf"
    }.

template_detailed() ->
    #{
        title => "Detailed Compliance Report",
        sections => [
            {"Introduction", "introduction"},
            {"Methodology", "methodology"},
            {"Control Assessments", "control_assessments"},
            {"Detailed Findings", "detailed_findings"},
            {"Evidence", "evidence"},
            {"Appendices", "appendices"}
        ],
        format => "pdf"
    }.

template_summary() ->
    #{
        title => "Compliance Summary",
        sections => [
            {"Framework Overview", "overview"},
            {"Compliance Score", "score"},
            {"Status Summary", "status"},
            {"Top Issues", "top_issues"}
        ],
        format => "json"
    }.

template_remediation() ->
    #{
        title => "Remediation Plan",
        sections => [
            {"Issue Description", "issues"},
            {"Root Cause", "root_cause"},
            {"Remediation Steps", "steps"},
            {"Timeline", "timeline"},
            {"Resources Required", "resources"}
        ],
        format => "xlsx"
    }.

perform_compliance_check(Framework, State) ->
    %% Perform comprehensive compliance check
    Timestamp = erlang:system_time(second),

    %% Check all controls
    ControlResults = check_all_controls(Framework, State),

    %% Calculate compliance metrics
    TotalControls = length(Framework#compliance_framework.controls),
    PassedControls = length([R || R <- ControlResults, R#compliance_check.status == "passed"]),
    FailedControls = length([R || R <- ControlResults, R#compliance_check.status == "failed"]),
    PendingControls = length([R || R <- ControlResults, R#compliance_check.status == "pending"]),
    CompliancePercentage = (PassedControls / TotalControls) * 100,

    %% Determine overall status
    OverallStatus = determine_overall_status(CompliancePercentage, State#state.compliance_threshold),

    %% Create compliance state
    #compliance_state{
        framework_id = Framework#compliance_framework.id,
        overall_status = OverallStatus,
        compliance_percentage = CompliancePercentage,
        total_controls = TotalControls,
        passed_controls = PassedControls,
        failed_controls = FailedControls,
        pending_controls = PendingControls,
        last_assessment = Timestamp,
        next_review = calculate_next_review(Timestamp),
        audit_trail = []
    }.

check_all_controls(Framework, State) ->
    %% Check all controls for a framework
    lists:foldl(fun(Control, Acc) ->
        ControlId = Control#control_requirement.id,
        Check = perform_control_check(ControlId, Framework#compliance_framework.id, State),
        [Check | Acc]
    end, [], Framework#compliance_framework.requirements).

perform_control_check(ControlId, FrameworkId, State) ->
    %% Perform individual control check
    Timestamp = erlang:system_time(second),

    %% Check control implementation
    Implementation = check_control_implementation(ControlId, State),

    %% Check evidence collection
    Evidence = collect_control_evidence(ControlId, State),

    %% Evaluate findings
    Findings = evaluate_findings(Implementation, Evidence, ControlId),

    %% Determine status
    Status = determine_control_status(Findings, ControlId, State),

    #compliance_check{
        id = generate_check_id(ControlId, FrameworkId),
        framework = FrameworkId,
        control_id = ControlId,
        status = Status,
        evidence = Evidence,
        findings = Findings,
        next_review = calculate_next_review(Timestamp),
        last_checked = Timestamp
    }.

check_control_implementation(ControlId, State) ->
    %% Check if control is implemented
    case ControlId of
        <<"AC-1">> -> check_ac1_implementation();
        <<"AU-1">> -> check_au1_implementation();
        <<"SC-13">> -> check_sc13_implementation();
        _ -> check_generic_implementation(ControlId)
    end.

check_ac1_implementation() ->
    %% Check AC-1 (Access Control Policy) implementation
    #{
        policy_exists => check_policy_exists(),
        procedures_documented => check_procedures_documented(),
        roles_defined => check_roles_defined(),
        disseminated => check_disseminated()
    }.

check_au1_implementation() ->
    %% Check AU-1 (Audit) implementation
    #{
        policy_exists => check_audit_policy_exists(),
        procedures_defined => check_audit_procedures(),
        implemented => check_audit_implemented()
    }.

check_sc13_implementation() ->
    %% Check SC-13 (Cryptographic Protection) implementation
    #{
        encryption_enabled => check_encryption_enabled(),
        proper_algorithms => check_crypto_algorithms(),
        key_management => check_key_management()
    }.

check_generic_implementation(ControlId) ->
    %% Generic control implementation check
    #{
        implemented => false,
        details => "Generic implementation check for " ++ binary_to_list(ControlId)
    }.

collect_control_evidence(ControlId, State) ->
    %% Collect evidence for control
    Evidence = [],

    %% Collect configuration evidence
    ConfigEvidence = collect_configuration_evidence(ControlId),
    Evidence ++ ConfigEvidence,

    %% Collect log evidence
    LogEvidence = collect_log_evidence(ControlId),
    Evidence ++ LogEvidence,

    %% Collect policy evidence
    PolicyEvidence = collect_policy_evidence(ControlId),
    Evidence ++ PolicyEvidence,

    Evidence.

collect_configuration_evidence(ControlId) ->
    %% Collect configuration-related evidence
    [].

collect_log_evidence(ControlId) ->
    %% Collect log-related evidence
    [].

collect_policy_evidence(ControlId) ->
    %% Collect policy-related evidence
    [].

evaluate_findings(Implementation, Evidence, ControlId) ->
    %% Evaluate findings from implementation and evidence
    Findings = [],

    %% Check implementation findings
    case maps:get(implemented, Implementation, false) of
        false ->
            Findings ++ [#{
                type => "implementation",
                severity => "high",
                description => "Control not implemented: " ++ binary_to_list(ControlId),
                recommendation => "Implement control"
            }];
        true ->
            Findings
    end,

    %% Check evidence findings
    case length(Evidence) == 0 of
        true ->
            Findings ++ [#{
                type => "evidence",
                severity => "medium",
                description => "No evidence collected for control: " ++ binary_to_list(ControlId),
                recommendation => "Document evidence"
            }];
        false ->
            Findings
    end,

    Findings.

determine_control_status(Findings, ControlId, State) ->
    %% Determine control status based on findings
    case Findings of
        [] -> "passed";
        _ ->
            CriticalFindings = [F || F <- Findings, F#finding.severity == "critical"],
            HighFindings = [F || F <- Findings, F#finding.severity == "high"],

            if
                length(CriticalFindings) > 0 -> "failed";
                length(HighFindings) > 2 -> "failed";
                length(HighFindings) > 0 -> "pending";
                true -> "pending"
            end
    end.

determine_overall_status(CompliancePercentage, Threshold) ->
    %% Determine overall compliance status
    if
        CompliancePercentage >= Threshold -> "compliant";
        CompliancePercentage >= Threshold - 10 -> "partially_compliant";
        CompliancePercentage >= Threshold - 20 -> "non_compliant";
        true -> "critical"
    end.

update_compliance_state(ComplianceState, State) ->
    %% Update compliance state in registry
    FrameworkId = ComplianceState#compliance_state.framework_id,
    ets:insert(State#state.checks, {FrameworkId, ComplianceState}).

log_compliance_check(FrameworkId, ComplianceState, State) ->
    %% Log compliance check
    AuditEntry = #{
        timestamp => erlang:timestamp(),
        type => "compliance_check",
        framework_id => FrameworkId,
        compliance_percentage => ComplianceState#compliance_state.compliance_percentage,
        overall_status => ComplianceState#compliance_state.overall_status,
        checks_performed => ComplianceState#compliance_state.total_controls,
        failed_controls => ComplianceState#compliance_state.failed_controls
    },

    ets:insert(State#state.audit_trail, AuditEntry).

check_compliance_violations(State) ->
    %% Check for compliance violations
    ComplianceStates = ets:tab2list(State#state.checks),

    lists:foreach(fun({_, ComplianceState}) ->
        case ComplianceState#compliance_state.overall_status of
            "critical" ->
                generate_violation_alert(ComplianceState, State);
            "non_compliant" ->
                generate_violation_alert(ComplianceState, State);
            _ -> ok
        end
    end, ComplianceStates).

generate_violation_alert(ComplianceState, State) ->
    %% Generate compliance violation alert
    Alert = #{
        timestamp => erlang:timestamp(),
        alert_id => generate_alert_id(),
        type => "compliance_violation",
        framework_id => ComplianceState#compliance_state.framework_id,
        compliance_percentage => ComplianceState#compliance_state.compliance_percentage,
        overall_status => ComplianceState#compliance_state.overall_status,
        failed_controls => ComplianceState#compliance_state.failed_controls,
        message => "Compliance violation detected: " ++
                   binary_to_list(ComplianceState#compliance_state.framework_id) ++
                   " status: " ++
                   binary_to_list(ComplianceState#compliance_state.overall_status),
        severity => "critical",
        source => "erlmcp_compliance_monitor",
        recommendations => generate_compliance_recommendations(ComplianceState)
    },

    %% Send to SIEM
    erlmcp_siem_generic:send_event(Alert),

    %% Send to incident response
    erlmcp_incident_response_orchestrator:new_incident(Alert).

generate_compliance_recommendations(ComplianceState) ->
    %% Generate recommendations for compliance violations
    [#{
        priority => "high",
        recommendation => "Address compliance issues in " ++
                         binary_to_list(ComplianceState#compliance_state.framework_id),
        impact => "High",
        effort => "High",
        timeline => "30 days"
    }].

generate_compliance_report(Framework, State) ->
    %% Generate detailed compliance report
    ComplianceState = case ets:lookup(State#state.checks, Framework#compliance_framework.id) of
        [{_, CS}] -> CS;
        _ -> undefined
    end,

    #{
        report_id => generate_report_id(),
        framework => #{
            id => Framework#compliance_framework.id,
            name => Framework#compliance_framework.name,
            version => Framework#compliance_framework.version
        },
        generated_at => erlang:timestamp(),
        overall_status => ComplianceState#compliance_state.overall_status,
        compliance_percentage => ComplianceState#compliance_state.compliance_percentage,
        control_summary => #{
            total => ComplianceState#compliance_state.total_controls,
            passed => ComplianceState#compliance_state.passed_controls,
            failed => ComplianceState#compliance_state.failed_controls,
            pending => ComplianceState#compliance_state.pending_controls
        },
        detailed_findings => get_detailed_findings(Framework, State),
        evidence_summary => collect_evidence_summary(Framework, State),
        recommendations => generate_report_recommendations(ComplianceState),
        next_review => ComplianceState#compliance_state.next_review
    }.

get_detailed_findings(Framework, State) ->
    %% Get detailed findings for each control
    ControlResults = check_all_controls(Framework, State),

    lists:foldl(fun(ControlCheck, Acc) ->
        #{
            control_id => ControlCheck#compliance_check.control_id,
            status => ControlCheck#compliance_check.status,
            findings => ControlCheck#compliance_check.findings,
            evidence => ControlCheck#compliance_check.evidence
        } | Acc
    end, [], ControlResults).

collect_evidence_summary(Framework, State) ->
    %% Collect summary of evidence
    [].

generate_report_recommendations(ComplianceState) ->
    %% Generate recommendations for the report
    [].

save_report(ReportId, Report, State) ->
    %% Save report to file system
    ReportData = jsx:encode(Report),
    ReportFile = ?REPORT_DIR ++ "/" ++ binary_to_list(ReportId) ++ ".json",

    filelib:ensure_dir(?REPORT_DIR),
    file:write_file(ReportFile, ReportData).

cleanup_old_audit_logs(State) ->
    %% Clean up old audit logs
    Now = erlang:system_time(second),
    Threshold = Now - 2592000, % 30 days

    %% Delete old audit entries
    ets:foldl(fun({_Key, Entry}, Acc) ->
        case maps:get(timestamp, Entry) < Threshold of
            true ->
                ets:delete(State#state.audit_trail, maps:get(id, Entry));
            false ->
                Acc
        end
    end, [], State#state.audit_trail).

log_framework_update(FrameworkId, Framework, State) ->
    %% Log framework update
    AuditEntry = #{
        timestamp => erlang:timestamp(),
        type => "framework_update",
        framework_id => FrameworkId,
        action => "updated",
        details => #{
            name => Framework#compliance_framework.name,
            version => Framework#compliance_framework.version,
            last_updated => Framework#compliance_framework.last_updated
        }
    },

    ets:insert(State#state.audit_trail, AuditEntry).

parse_framework_data(Binary) ->
    %% Parse framework data from binary
    try
        {ok, #{}}
    catch
        _: _ -> {error, invalid_format}
    end.

calculate_next_review(Timestamp) ->
    %% Calculate next review date
    Timestamp + 2592000. % 30 days

generate_check_id(ControlId, FrameworkId) ->
    %% Generate unique check ID
    Hash = crypto:hash(sha256, <<FrameworkId/binary, ControlId/binary>>),
    binary:part(Hash, 0, 16).

generate_report_id() ->
    %% Generate unique report ID
    integer_to_binary(erlang:system_time(nanosecond)).

generate_alert_id() ->
    %% Generate unique alert ID
    integer_to_binary(erlang:system_time(nanosecond)).