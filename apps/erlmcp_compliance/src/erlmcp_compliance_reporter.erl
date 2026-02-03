%% @doc erlmcp Compliance Reporter
%% Generates compliance reports for SOC2, HIPAA, GDPR, ISO27001 frameworks
%% Provides automated report generation and compliance evidence collection
-module(erlmcp_compliance_reporter).

-behaviour(gen_server).

%% API
-export([start_link/0, generate_compliance_report/2, generate_evidence_report/3,
         generate_audit_pack/4, export_compliance_data/2, get_report_templates/1,
         schedule_report_generation/3, generate_framework_summary/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(evidence, {
    id :: binary(),
    framework :: soc2 | hipaa | gdpr | iso27001,
    control_id :: binary(),
    control_name :: binary(),
    evidence_type :: configuration | log | screenshot | report | test_result | questionnaire,
    collected_at :: erlang:timestamp(),
    collected_by :: binary(),
    file_path :: binary(),
    file_hash :: binary(),
    status :: collected | pending | failed,
    metadata :: map()
}).

-report_template(
    name: binary(),
    framework: soc2 | hipaa | gdpr | iso27001,
    format: pdf | html | json | csv,
    sections: list(),
    required_controls: list(),
    custom_sections: list()
).

-record(state, {
    reports :: list(),
    evidence :: list(),
    templates :: list(),
    scheduled_reports :: list(),
    export_history :: list(),
    compliance_metrics :: #{}
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

generate_compliance_report(Framework, Options) ->
    gen_server:call(?SERVER, {generate_compliance_report, Framework, Options}).

generate_evidence_report(Framework, ControlId, Filters) ->
    gen_server:call(?SERVER, {generate_evidence_report, Framework, ControlId, Filters}).

generate_audit_pack(Framework, Scope, Format, Options) ->
    gen_server:call(?SERVER, {generate_audit_pack, Framework, Scope, Format, Options}).

export_compliance_data(Framework, Format) ->
    gen_server:call(?SERVER, {export_compliance_data, Framework, Format}).

get_report_templates(Framework) ->
    gen_server:call(?SERVER, {get_report_templates, Framework}).

schedule_report_generation(Framework, Schedule, Options) ->
    gen_server:call(?SERVER, {schedule_report_generation, Framework, Schedule, Options}).

generate_framework_summary(Framework) ->
    gen_server:call(?SERVER, {generate_framework_summary, Framework}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    Reports0 = [],
    Evidence0 = [],
    Templates0 = init_report_templates(),
    ScheduledReports0 = [],
    ExportHistory0 = [],
    ComplianceMetrics0 = init_compliance_metrics(),
    State0 = #state{
        reports = Reports0,
        evidence = Evidence0,
        templates = Templates0,
        scheduled_reports = ScheduledReports0,
        export_history = ExportHistory0,
        compliance_metrics = ComplianceMetrics0
    },
    erlmcp_telemetry:counter("compliance.reporter.initialized", 1,
                           #{component => "compliance"}),
    {ok, State0}.

handle_call({generate_compliance_report, Framework, Options}, _From, State) ->
    ReportId = generate_report_id(),
    Evidence = collect_evidence_for_framework(Framework, State),
    Summary = generate_framework_summary_for_report(Framework, State),
    Report = #{
        id => ReportId,
        framework => Framework,
        title => <<>>,
        generated_at => erlang:timestamp(),
        period => get_report_period(Options),
        summary => Summary,
        evidence => Evidence,
        controls => generate_control_assessment(Framework, State),
        recommendations => generate_recommendations(Framework, State),
        status => draft,
        reviewed_by => null,
        approved_by => null
    },

    Reports1 = [Report | State#state.reports],

    erlmcp_telemetry:counter("compliance.report.generated", 1,
                           #{framework => atom_to_list(Framework)}),

    {reply, {ok, ReportId}, State#state{reports = Reports1}};

handle_call({generate_evidence_report, Framework, ControlId, Filters}, _From, State) ->
    Evidence = get_evidence_for_control(Framework, ControlId, State, Filters),
    Report = #{
        id => generate_report_id(),
        framework => Framework,
        control_id => ControlId,
        title => "Evidence Report for Control: " ++ binary_to_list(ControlId),
        generated_at => erlang:timestamp(),
        evidence => Evidence,
        filters => Filters,
        status => complete
    },

    {reply, {ok, Report}, State};

handle_call({generate_audit_pack, Framework, Scope, Format, Options}, _From, State) ->
    PackId = generate_pack_id(),
    Evidence = collect_audit_evidence(Framework, Scope, State),
    Report = #{
        id => PackId,
        framework => Framework,
        scope => Scope,
        format => Format,
        generated_at => erlang:timestamp(),
        evidence => Evidence,
        report => generate_framework_summary(Framework, State),
        checklist => generate_audit_checklist(Framework, Scope),
        documentation => generate_framework_docs(Framework),
        options => Options,
        status => assembled
    },

    Packs1 = [Pack | State#state.reports],

    erlmcp_telemetry:counter("compliance.audit_pack.generated", 1,
                           #{framework => atom_to_list(Framework),
                             format => atom_to_list(Format)}),

    {reply, {ok, PackId}, State#state{reports = Packs1}};

handle_call({export_compliance_data, Framework, Format}, _From, State) ->
    Data = extract_compliance_data(Framework, State),
    ExportId = generate_export_id(),
    Export = #{
        id => ExportId,
        framework => Framework,
        format => Format,
        data => Data,
        exported_at => erlang:timestamp(),
        status => ready
    },

    ExportHistory1 = [Export | State#state.export_history],

    {reply, {ok, ExportId}, State#state{export_history = ExportHistory1}};

handle_call({get_report_templates, Framework}, _From, State) ->
    Templates = lists:filter(
        fun(Template) ->
            Template#report_template.framework =:= Framework
        end, State#state.templates),
    {reply, {ok, Templates}, State};

handle_call({schedule_report_generation, Framework, Schedule, Options}, _From, State) ->
    ScheduleId = generate_schedule_id(),
    ScheduledReport = #{
        id => ScheduleId,
        framework => Framework,
        schedule => Schedule,
        options => Options,
        created_at => erlang:timestamp(),
        next_run => calculate_next_run(Schedule),
        status => active,
        last_run => null,
        results => []
    },

    ScheduledReports1 = [ScheduledReport | State#state.scheduled_reports],

    {reply, {ok, ScheduleId}, State#state{scheduled_reports = ScheduledReports1}};

handle_call({generate_framework_summary, Framework}, _From, State) ->
    Summary = generate_framework_summary_content(Framework, State),
    {reply, {ok, Summary}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

init_report_templates() ->
    %% SOC2 Templates
    [
        #report_template{
            name = "SOC2 Type I Report",
            framework = soc2,
            format = pdf,
            sections = ["Executive Summary", "Scope", "Control Environment",
                       "Access Control", "System Operations", "Change Management"],
            required_controls = ["CC6.1", "CC6.2", "CC6.6"],
            custom_sections = []
        },
        #report_template{
            name = "SOC2 Type II Report",
            framework = soc2,
            format = pdf,
            sections = ["Executive Summary", "Scope", "Control Environment",
                       "Access Control", "System Operations", "Change Management",
                       "Control Testing", "Results", "Recommendations"],
            required_controls = ["CC6.1", "CC6.2", "CC6.6", "CC1.1", "CC1.2"],
            custom_sections = []
        }
    ].

generate_report_id() ->
    iolist_to_binary(io_lib:format("report-~s-~s-~w", [
        integer_to_list(erlang:system_time(second)),
        crypto:strong_rand_bytes(8) |> binary:encode_hex(),
        erlang:unique_integer()
    ])).

generate_pack_id() ->
    iolist_to_binary(io_lib:format("pack-~s-~s", [
        integer_to_list(erlang:system_time(second)),
        crypto:strong_rand_bytes(6) |> binary:encode_hex()
    ])).

generate_export_id() ->
    iolist_to_binary(io_lib:format("export-~s-~s", [
        integer_to_list(erlang:system_time(second)),
        crypto:strong_rand_bytes(6) |> binary:encode_hex()
    ])).

generate_schedule_id() ->
    iolist_to_binary(io_lib:format("sched-~s-~s", [
        integer_to_list(erlang:system_time(second)),
        crypto:strong_rand_bytes(6) |> binary:encode_hex()
    ])).

collect_evidence_for_framework(Framework, State) ->
    % This would collect all evidence for the framework
    % For now, return empty list
    [].

get_report_period(Options) ->
    case maps:get(period, Options, default) of
        default -> {calendar:local_time(), calendar:local_time()};
        {start, End} -> {start, End};
        {start, End, _} -> {start, End}
    end.

generate_framework_summary_for_report(Framework, State) ->
    % Generate comprehensive framework summary
    #{
        framework => Framework,
        total_controls => get_total_controls(Framework),
        implemented_controls => get_implemented_controls(Framework, State),
        compliance_score => calculate_compliance_score(Framework, State),
        last_audit_date => get_last_audit_date(Framework, State),
        next_audit_date => calculate_next_audit_date(Framework),
        key_findings => get_key_findings(Framework, State),
        risk_assessment => get_risk_assessment(Framework, State)
    }.

get_total_controls(Framework) ->
    case Framework of
        soc2 -> 93;  % Number of SOC2 controls
        hipaa -> 60;  % Number of HIPAA controls
        gdpr -> 99;   % Number of GDPR requirements
        iso27001 -> 114 % Number of ISO27001 controls
    end.

get_implemented_controls(Framework, State) ->
    % This would query the policy manager
    % For now, return placeholder
    50.

calculate_compliance_score(Framework, State) ->
    % This would calculate based on actual implementation
    0.75.

get_last_audit_date(Framework, State) ->
    % This would query the audit logger
    calendar:local_time().

calculate_next_audit_date(Framework) ->
    % Calculate based on framework requirements
    case Framework of
        soc2 -> add_months(calendar:local_time(), 12);
        hipaa -> add_months(calendar:local_time(), 6);
        gdpr -> add_months(calendar:local_time(), 12);
        iso27001 -> add_months(calendar:local_time(), 12)
    end.

get_key_findings(Framework, State) ->
    % This would analyze audit events and violations
    ["System access controls need enhancement", "Logging improvements required"].

get_risk_assessment(Framework, State) ->
    #{
        risk_level => medium,
        key_risks => ["Unauthorized access", "Data breach", "Non-compliance"],
        mitigation_status => "In progress"
    }.

generate_control_assessment(Framework, State) ->
    % Generate detailed control assessment
    Controls = get_controls_for_framework(Framework),
    lists:map(fun(Control) ->
        #{
            id => Control#control_status.id,
            name => Control#control_status.name,
            status => Control#control_status.status,
            effectiveness => Control#control_status.effectiveness,
            last_reviewed => Control#control_status.last_check,
            next_review => Control#control_status.next_review,
            violations => Control#control_status.violations,
            recommendations => generate_control_recommendations(Control)
        }
    end, Controls).

generate_control_recommendations(Control) ->
    % Generate specific recommendations for each control
    case Control#control_status.status of
        non_compliant -> ["Immediate remediation required", "Implement missing controls"];
        partial_compliance -> ["Enhance existing controls", "Address partial implementation"];
        not_implemented -> ["Implement control", "Define implementation plan"];
        _ -> ["Maintain current controls", "Regular monitoring"]
    end.

generate_recommendations(Framework, State) ->
    % Generate high-level recommendations
    [
        "Enhance access control mechanisms",
        "Improve monitoring and alerting",
        "Implement regular training programs",
        "Update security policies",
        "Conduct regular assessments"
    ].

get_evidence_for_control(Framework, ControlId, State, Filters) ->
    % Query evidence for specific control
    FilteredEvidence = lists:filter(fun(Evidence) ->
        Evidence#evidence.control_id =:= ControlId
    end, State#state.evidence),

    % Apply additional filters
    case maps:get(evidence_type, Filters, all) of
        all -> FilteredEvidence;
        Type -> lists:filter(fun(E) -> E#evidence.evidence_type =:= Type end, FilteredEvidence)
    end.

collect_audit_evidence(Framework, Scope, State) ->
    % Collect comprehensive evidence for audit
    Evidence = State#state.evidence,
    EvidenceTypes = configuration, log, report, test_result,
    lists:filter(fun(E) ->
        lists:member(E#evidence.evidence_type, EvidenceTypes)
    end, Evidence).

generate_framework_summary(Framework, State) ->
    #{
        framework => Framework,
        compliance_status => get_compliance_status(Framework, State),
        key_metrics => get_key_metrics(Framework, State),
        current_controls => get_current_controls(Framework, State),
        outstanding_issues => get_outstanding_issues(Framework, State),
        recent_audits => get_recent_audits(Framework, State)
    }.

get_compliance_status(Framework, State) ->
    % This would determine compliance status
    compliant.

get_key_metrics(Framework, State) ->
    % Framework-specific metrics
    #{
        controls_implemented => 75,
        effectiveness => 0.85,
        violations => 0,
        last_check => calendar:local_time()
    }.

get_current_controls(Framework, State) ->
    % List current controls
    ["Access Control", "Security Monitoring", "Change Management"].

get_outstanding_issues(Framework, State) ->
    % List outstanding issues
    ["Review access permissions", "Update security policies"].

get_recent_audits(Framework, State) ->
    % List recent audits
    [].

extract_compliance_data(Framework, State) ->
    % Extract data for export
    #{
        framework => Framework,
        policies => extract_policies(Framework, State),
        evidence => extract_evidence(Framework, State),
        metrics => extract_metrics(Framework, State),
        audit_trail => extract_audit_trail(Framework, State)
    }.

extract_policies(Framework, State) ->
    % Extract policies for framework
    [].

extract_evidence(Framework, State) ->
    % Extract evidence for framework
    [].

extract_metrics(Framework, State) ->
    % Extract metrics for framework
    #{
        compliance_score => 0.75,
        controls_implemented => 50,
        last_audit => calendar:local_time()
    }.

extract_audit_trail(Framework, State) ->
    % Extract audit trail for framework
    [].

init_compliance_metrics() ->
    #{
        soc2 => #{compliance_score => 0.0, controls_implemented => 0},
        hipaa => #{compliance_score => 0.0, controls_implemented => 0},
        gdpr => #{compliance_score => 0.0, controls_implemented => 0},
        iso27001 => #{compliance_score => 0.0, controls_implemented => 0}
    }.

generate_audit_checklist(Framework, Scope) ->
    % Generate comprehensive audit checklist
    Checklists = #{
        soc2 => [
            {"Review security policies", []},
            {"Test access controls", []},
            {"Verify backup procedures", []},
            {"Check change management", []},
            {"Assess incident response", []}
        ],
        hipaa => [
            {"Review administrative safeguards", []},
            {"Test technical safeguards", []},
            {"Verify PHI handling procedures", []},
            {"Check breach notification", []},
            {"Assess staff training", []}
        ],
        gdpr => [
            {"Review consent management", []},
            {"Test data subject rights", []},
            {"Verify data processing agreements", []},
            {"Check data breach procedures", []},
            {"Assess DPO responsibilities", []}
        ],
        iso27001 => [
            {"Review ISMS documentation", []},
            {"Test information security controls", []},
            {"Verify risk assessment", []},
            {"Check incident management", []},
            {"Assess business continuity", []}
        ]
    },
    maps:get(Framework, Checklists, []).

generate_framework_docs(Framework) ->
    % Generate framework-specific documentation
    Docs = #{
        soc2 => "SOC2 Type I & II compliance documentation",
        hipaa => "HIPAA compliance documentation",
        gdpr => "GDPR compliance documentation",
        iso27001 => "ISO27001 compliance documentation"
    },
    maps:get(Framework, Docs, "").

calculate_next_run(Schedule) ->
    % Calculate next scheduled run
    case Schedule of
        #{interval := Days} ->
            add_days(calendar:local_time(), Days);
        _ ->
            calendar:local_time()
    end.

add_days(Date, Days) ->
    calendar:gregorian_days_to_date(calendar:gregorian_days_date(Date) + Days).

add_months(Date, Months) ->
    % Add months to date (simplified)
    calendar:gregorian_days_to_date(
        calendar:gregorian_days_date(Date) + Months * 30  % Approximation
    ).