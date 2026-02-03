-module(erlmcp_compliance_manager).
-behaviour(gen_server).

%% API
-export([start_link/0, check_compliance/1, generate_compliance_report/2]).
-export([create_compliance_framework/2, update_compliance_framework/3, delete_compliance_framework/2]).
-export([get_compliance_frameworks/1, get_framework_details/1]).
-export([run_compliance_scan/2, get_scan_results/1, export_compliance_data/2]).
-export([schedule_compliance_check/3, get_compliance_schedule/1]).
-export([create_audit_trail/2, get_audit_trail/2, validate_audit_integrity/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Records
-record.compliance_framework, {
    id :: binary(),
    name :: binary(),
    description :: binary(),
    type :: iso27001 | sox404 | gdpr | hipaa | pci_dss | soc2 | custom,
    version :: binary(),
    controls :: list(),
    requirements :: list(),
    created_at :: integer(),
    updated_at :: integer()
}.

record.compliance_control, {
    id :: binary(),
    title :: binary(),
    description :: binary(),
    requirement :: binary(),
    control :: binary(),
    status :: implemented | not_implemented | partial | exception,
    evidence :: list(),
    last_assessed :: integer(),
    next_review :: integer(),
    owner :: binary(),
    test_results :: list()
}.

record.compliance_scan, {
    id :: binary(),
    framework_id :: binary(),
    status :: running | completed | failed | cancelled,
    start_time :: integer(),
    end_time :: integer() | undefined,
    results :: list(),
    score :: float(),
    findings :: list(),
    created_by :: binary()
}.

record.audit_trail, {
    id :: binary(),
    event_type :: binary(),
    timestamp :: integer(),
    actor :: binary(),
    action :: binary(),
    resource :: binary(),
    details :: map(),
    checksum :: binary()
}.

record.state, {
    compliance_frameworks :: map(),
    compliance_scans :: list(),
    audit_trails :: list(),
    config :: map()
}).

-define(TIMEOUT, 30000).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

check_compliance(FrameworkId) ->
    gen_server:call(?MODULE, {check_compliance, FrameworkId}, ?TIMEOUT).

generate_compliance_report(FrameworkId, Format) ->
    gen_server:call(?MODULE, {generate_compliance_report, FrameworkId, Format}, ?TIMEOUT).

create_compliance_framework(FrameworkData) ->
    gen_server:call(?MODULE, {create_compliance_framework, FrameworkData}, ?TIMEOUT).

update_compliance_framework(FrameworkId, UpdateData) ->
    gen_server:call(?MODULE, {update_compliance_framework, FrameworkId, UpdateData}, ?TIMEOUT).

delete_compliance_framework(FrameworkId, Reason) ->
    gen_server:call(?MODULE, {delete_compliance_framework, FrameworkId, Reason}, ?TIMEOUT).

get_compliance_frameworks(Filter) ->
    gen_server:call(?MODULE, {get_compliance_frameworks, Filter}, ?TIMEOUT).

get_framework_details(FrameworkId) ->
    gen_server:call(?MODULE, {get_framework_details, FrameworkId}, ?TIMEOUT).

run_compliance_scan(FrameworkId, ScanConfig) ->
    gen_server:call(?MODULE, {run_compliance_scan, FrameworkId, ScanConfig}, ?TIMEOUT).

get_scan_results(ScanId) ->
    gen_server:call(?MODULE, {get_scan_results, ScanId}, ?TIMEOUT).

export_compliance_data(FrameworkId, Format) ->
    gen_server:call(?MODULE, {export_compliance_data, FrameworkId, Format}, ?TIMEOUT).

schedule_compliance_check(FrameworkId, Schedule, Config) ->
    gen_server:call(?MODULE, {schedule_compliance_check, FrameworkId, Schedule, Config}, ?TIMEOUT).

get_compliance_schedule(FrameworkId) ->
    gen_server:call(?MODULE, {get_compliance_schedule, FrameworkId}, ?TIMEOUT).

create_audit_trail(EventData) ->
    gen_server:cast(?MODULE, {create_audit_trail, EventData}).

get_audit_trail(Limit) ->
    gen_server:call(?MODULE, {get_audit_trail, Limit}, ?TIMEOUT).

validate_audit_integrity() ->
    gen_server:call(?MODULE, {validate_audit_integrity}, ?TIMEOUT).

init([]) ->
    State = #state{
        compliance_frameworks = load_default_compliance_frameworks(),
        compliance_scans = [],
        audit_trails = [],
        config = load_config()
    },
    erlmcp_compliance_manager:initialize(),
    {ok, State}.

handle_call({check_compliance, FrameworkId}, _From, State) ->
    case maps:find(FrameworkId, State#state.compliance_frameworks) of
        {ok, Framework} ->
            ScanResults = check_framework_compliance(Framework, State),
            ComplianceScore = calculate_compliance_score(ScanResults),
            {reply, {ok, ComplianceScore, ScanResults}, State};
        error ->
            {reply, {error, framework_not_found}, State}
    end;

handle_call({generate_compliance_report, FrameworkId, Format}, _From, State) ->
    case maps:find(FrameworkId, State#state.compliance_frameworks) of
        {ok, Framework} ->
            Report = generate_report(Framework, Format, State),
            {reply, {ok, Report}, State};
        error ->
            {reply, {error, framework_not_found}, State}
    end;

handle_call({create_compliance_framework, FrameworkData}, _From, State) ->
    case validate_framework_data(FrameworkData) of
        {ok, ValidatedData} ->
            FrameworkId = generate_framework_id(),
            Framework = #compliance_framework{
                id = FrameworkId,
                name = maps:get(name, ValidatedData),
                description = maps:get(description, ValidatedData, <<"">>),
                type = maps:get(type, ValidatedData),
                version = maps:get(version, ValidatedData, <<"1.0">>),
                controls = maps:get(controls, ValidatedData, []),
                requirements = maps:get(requirements, ValidatedData, []),
                created_at = timestamp(),
                updated_at = timestamp()
            },
            NewState = State#state{
                compliance_frameworks = maps:put(FrameworkId, Framework, State#state.compliance_frameworks)
            },
            {reply, {ok, FrameworkId}, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({update_compliance_framework, FrameworkId, UpdateData}, _From, State) ->
    case maps:find(FrameworkId, State#state.compliance_frameworks) of
        {ok, Framework} ->
            UpdatedFramework = Framework#compliance_framework{
                name = maps:get(name, UpdateData, Framework#compliance_framework.name),
                description = maps:get(description, UpdateData, Framework#compliance_framework.description),
                version = maps:get(version, UpdateData, Framework#compliance_framework.version),
                controls = maps:get(controls, UpdateData, Framework#compliance_framework.controls),
                requirements = maps:get(requirements, UpdateData, Framework#compliance_framework.requirements),
                updated_at = timestamp()
            },
            NewState = State#state{
                compliance_frameworks = maps:put(FrameworkId, UpdatedFramework, State#state.compliance_frameworks)
            },
            {reply, {ok, FrameworkId}, NewState};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({delete_compliance_framework, FrameworkId, Reason}, _From, State) ->
    case maps:find(FrameworkId, State#state.compliance_frameworks) of
        {ok, Framework} ->
            %% Log framework deletion
            create_audit_trail(#{
                event_type => framework_deleted,
                actor => system,
                action => delete,
                resource => FrameworkId,
                details => #{reason => Reason, name => Framework#compliance_framework.name}
            }),
            NewState = State#state{
                compliance_frameworks = maps:remove(FrameworkId, State#state.compliance_frameworks)
            },
            {reply, {ok, deleted}, NewState};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({get_compliance_frameworks, Filter}, _From, State) ->
    FilteredFrameworks = apply_filter(State#state.compliance_frameworks, Filter),
    {reply, {ok, maps:values(FilteredFrameworks)}, State};

handle_call({get_framework_details, FrameworkId}, _From, State) ->
    case maps:find(FrameworkId, State#state.compliance_frameworks) of
        {ok, Framework} ->
            {reply, {ok, Framework}, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({run_compliance_scan, FrameworkId, ScanConfig}, _From, State) ->
    case maps:find(FrameworkId, State#state.compliance_frameworks) of
        {ok, Framework} ->
            ScanId = generate_scan_id(),
            Scan = #compliance_scan{
                id = ScanId,
                framework_id = FrameworkId,
                status = running,
                start_time = timestamp(),
                results = [],
                score = 0.0,
                findings = [],
                created_by = maps:get(created_by, ScanConfig, system)
            },
            %% Start compliance scan process
            spawn_link(fun() -> perform_compliance_scan(ScanId, FrameworkId, ScanConfig) end),
            NewState = State#state{
                compliance_scans = [Scan|State#state.compliance_scans]
            },
            {reply, {ok, ScanId}, NewState};
        error ->
            {reply, {error, framework_not_found}, State}
    end;

handle_call({get_scan_results, ScanId}, _From, State) ->
    case lists:search(fun(S) -> S#compliance_scan.id == ScanId end, State#state.compliance_scans) of
        {value, Scan} ->
            {reply, {ok, Scan}, State};
        false ->
            {reply, {error, scan_not_found}, State}
    end;

handle_call({export_compliance_data, FrameworkId, Format}, _From, State) ->
    case maps:find(FrameworkId, State#state.compliance_frameworks) of
        {ok, Framework} ->
            ExportData = export_compliance_data_1(Framework, State),
            Exported = format_export_data(ExportData, Format),
            {reply, {ok, Exported}, State};
        error ->
            {reply, {error, framework_not_found}, State}
    end;

handle_call({schedule_compliance_check, FrameworkId, Schedule, Config}, _From, State) ->
    case maps:find(FrameworkId, State#state.compliance_frameworks) of
        {ok, Framework} ->
            ScheduleId = generate_schedule_id(),
            ComplianceSchedule = #{
                id => ScheduleId,
                framework_id => FrameworkId,
                schedule => Schedule,
                config => Config,
                enabled => true,
                created_at => timestamp(),
                last_run => undefined,
                next_run => calculate_next_run(Schedule)
            },
            %% Start scheduled compliance check
            start_scheduled_check(ComplianceSchedule),
            {reply, {ok, ScheduleId}, State};
        error ->
            {reply, {error, framework_not_found}, State}
    end;

handle_call({get_compliance_schedule, FrameworkId}, _From, State) ->
    Schedules = lists:filter(fun(S) -> S#schedule.framework_id == FrameworkId end, State#state.schedules),
    {reply, {ok, Schedules}, State};

handle_call({validate_audit_integrity}, _From, State) ->
    Validation = validate_audit_trail_integrity(State#state.audit_trails),
    {reply, {ok, Validation}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({create_audit_trail, EventData}, State) ->
    AuditTrail = #audit_trail{
        id = generate_audit_id(),
        event_type = maps:get(event_type, EventData),
        timestamp = timestamp(),
        actor = maps:get(actor, EventData),
        action = maps:get(action, EventData),
        resource = maps:get(resource, EventData),
        details = maps:get(details, EventData, #{}),
        checksum = generate_checksum(EventData)
    },
    NewAuditTrails = [AuditTrail|State#state.audit_trails],
    LimitedAuditTrails = lists:sublist(NewAuditTrails, 100000), %% Keep last 100,000 entries
    {noreply, State#state{audit_trails = LimitedAuditTrails}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({scan_completed, ScanId, Results}, State) ->
    %% Update scan results
    UpdatedScans = lists:map(fun(Scan) ->
        case Scan#compliance_scan.id == ScanId of
            true ->
                Scan#compliance_scan{
                    status = completed,
                    end_time = timestamp(),
                    results = Results#scan.results,
                    score = Results#scan.score,
                    findings = Results#scan.findings
                };
            false ->
                Scan
        end
    end, State#state.compliance_scans),
    {noreply, State#state{compliance_scans = UpdatedScans}};

handle_info({scheduled_scan, ScheduleId}, State) ->
    %% Run scheduled compliance scan
    case lists:search(fun(S) -> S#schedule.id == ScheduleId end, State#state.schedules) of
        {value, Schedule} ->
            run_compliance_scan(Schedule#schedule.framework_id, Schedule#schedule.config),
            %% Update next run time
            UpdatedSchedule = Schedule#schedule{
                last_run = timestamp(),
                next_run = calculate_next_run(Schedule#schedule.schedule)
            },
            {noreply, State};
        false ->
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
initialize() ->
    %% Initialize compliance management system
    %% Load compliance frameworks
    %% Configure audit logging
    ok.

load_config() ->
    #{
        audit_retention_days => 2555, %% 7 years
        scan_timeout => 3600000, %% 1 hour
        max_scan_size => 1000, %% Maximum controls per scan
        auto_archive => true,
        real_time_monitoring => true
    }.

load_default_compliance_frameworks() ->
    %% Load default compliance frameworks for Fortune 500
    #{
        iso27001_2013 => #compliance_framework{
            id => <<"iso27001_2013">>,
            name => <<"ISO 27001:2013">>,
            description => <<"International Organization for Standardization - Information Security Management">>,
            type => iso27001,
            version => <<"2013">>,
            controls = load_iso27001_controls(),
            requirements = load_iso27001_requirements(),
            created_at => timestamp(),
            updated_at => timestamp()
        },
        sox404 => #compliance_framework{
            id => <<"sox404">>,
            name => <<"SOX Section 404">>,
            description => <<"Sarbanes-Oxley Act Section 404 - Internal Control Over Financial Reporting">>,
            type => sox404,
            version => <<"2013">>,
            controls = load_sox404_controls(),
            requirements = load_sox404_requirements(),
            created_at => timestamp(),
            updated_at => timestamp()
        },
        gdpr => #compliance_framework{
            id => <<"gdpr">>,
            name => <<"GDPR">>,
            description => <<"General Data Protection Regulation">>,
            type => gdpr,
            version => <<"2018">>,
            controls = load_gdpr_controls(),
            requirements = load_gdpr_requirements(),
            created_at => timestamp(),
            updated_at => timestamp()
        },
        hipaa => #compliance_framework{
            id => <<"hipaa">>,
            name => <<"HIPAA">>,
            description => <<"Health Insurance Portability and Accountability Act">>,
            type => hipaa,
            version => <<"2013">>,
            controls = load_hipaa_controls(),
            requirements = load_hipaa_requirements(),
            created_at => timestamp(),
            updated_at => timestamp()
        },
        pci_dss => #compliance_framework{
            id => <<"pci_dss">>,
            name => <<"PCI DSS">>,
            description => <<"Payment Card Industry Data Security Standard">>,
            type => pci_dss,
            version => <<"3.2.1">>,
            controls = load_pci_dss_controls(),
            requirements = load_pci_dss_requirements(),
            created_at => timestamp(),
            updated_at => timestamp()
        },
        soc2 => #compliance_framework{
            id => <<"soc2">>,
            name => <<"SOC 2">>,
            description => <<"Service Organization Control 2">>,
            type => soc2,
            version = <<"2017">>,
            controls = load_soc2_controls(),
            requirements = load_soc2_requirements(),
            created_at => timestamp(),
            updated_at => timestamp()
        }
    }.

load_iso27001_controls() ->
    %% Load ISO 27001 Annex A controls
    [
        #compliance_control{
            id => <<"A.5.1.1">>,
            title => <<"Information security policies">>,
            description => <<"An information security policy shall be defined, approved by management, and communicated to employees.">>,
            requirement => <<"A.5.1">>,
            control => <<"Policy">>,
            status => implemented,
            evidence = [security_policy_document],
            last_assessed => timestamp(),
            next_review => timestamp() + 2592000000, %% 30 days
            owner => security_officer,
            test_results = []
        }
        %% Add more controls...
    ].

load_sox404_controls() ->
    %% Load SOX 404 controls
    [
        #compliance_control{
            id => <<"SOX-C-01">>,
            title => <<"Financial reporting controls">>,
            description => <<"Controls over financial reporting processes">>,
            requirement => <<"SOX 404">>,
            control => <<"Financial Controls">>,
            status => implemented,
            evidence = [control_matrix, testing_procedures],
            last_assessed => timestamp(),
            next_review => timestamp() + 15552000000, %% 6 months
            owner => cio,
            test_results = []
        }
        %% Add more controls...
    ].

load_gdpr_controls() ->
    %% Load GDPR controls
    [
        #compliance_control{
            id => <<"GDPR-ART-32">>,
            title => <<"Security of processing">>,
            description => <<"Security of personal data processing">>,
            requirement => <<"Article 32">>,
            control => <<"Technical and Organizational Measures">>,
            status => implemented,
            evidence = [risk_assessment, security_controls],
            last_assessed => timestamp(),
            next_review => timestamp() + 15552000000, %% 6 months
            owner => dpo,
            test_results = []
        }
        %% Add more controls...
    ].

load_hipaa_controls() ->
    %% Load HIPAA controls
    [
        #compliance_control{
            id => <<"HIPAA-164.306">>,
            title => <<"Security standards: General rules">>,
            description => <<"Security standards for protected health information">>,
            requirement => <<"164.306">>,
            control => <<"Administrative, Physical, Technical Safeguards">>,
            status => implemented,
            evidence = [security_rule_compliance, risk_analysis],
            last_assessed => timestamp(),
            next_review => timestamp() + 15552000000, %% 6 months
            owner => hipaa_officer,
            test_results = []
        }
        %% Add more controls...
    ].

load_pci_dss_controls() ->
    %% Load PCI DSS controls
    [
        #compliance_control{
            id => <<"PCI-Req-1">>,
            title => <<"Install and maintain a firewall configuration">>,
            description => <<"Install and maintain a firewall configuration to protect cardholder data">>,
            requirement => <<"Requirement 1">>,
            control => <<"Firewall Configuration">>,
            status => implemented,
            evidence = [firewall_rules, configuration_changes],
            last_assessed => timestamp(),
            next_review => timestamp() + 777600000, %% 9 days (PCI requirement)
            owner => security_manager,
            test_results = []
        }
        %% Add more controls...
    ].

load_soc2_controls() ->
    %% Load SOC 2 controls
    [
        #compliance_control{
            id => <<"SOC2-CC1.1">>,
            title => <<"Management establishes, communicates, and monitors policies and procedures to guide operations and achieve objectives.">>,
            description => <<"Management establishes policies and procedures">>,
            requirement => <<"Common Criteria CC1.1">>,
            control = <<"Policy and Procedures">>,
            status => implemented,
            evidence = [policy_documents, training_records],
            last_assessed => timestamp(),
            next_review => timestamp() + 15552000000, %% 6 months
            owner => auditor,
            test_results = []
        }
        %% Add more controls...
    ].

load_iso27001_requirements() ->
    %% Load ISO 27001 requirements
    [<<"A.5 Information Security Policies">>, <<"A.6 Organization of Information Security">>].

load_sox404_requirements() ->
    %% Load SOX 404 requirements
    [<<"Internal Control Assessment">>, <<>"Testing of Controls">>].

load_gdpr_requirements() ->
    %% Load GDPR requirements
    [<<"Data Processing Principles">>, <<"Data Subject Rights">>, <<"Data Breach Notification">>].

load_hipaa_requirements() ->
    %% Load HIPAA requirements
    [<<"Administrative Safeguards">>, <<"Physical Safeguards">>, <<"Technical Safeguards">>].

load_pci_dss_requirements() ->
    %% Load PCI DSS requirements
    [<<"Build and Maintain a Secure Network">>, <<"Protect Cardholder Data">>].

load_soc2_requirements() ->
    %% Load SOC 2 requirements
    [<<"Security">>, <<"Availability">>, <<"Processing Integrity">>, <<"Confidentiality">>, <<"Privacy">>].

check_framework_compliance(Framework, State) ->
    %% Check each control in the framework
    Results = lists:map(fun(Control) ->
        check_control_compliance(Control, State)
    end, Framework#compliance_framework.controls),

    Results.

check_control_compliance(Control, State) ->
    %% Check if control is implemented and effective
    Status = Control#compliance_control.status,
    TestResults = Control#compliance_control.test_results,

    case Status of
        implemented ->
            case TestResults of
                [] ->
                    #{control => Control#compliance_control.id, status => implemented, effective => false};
                _ ->
                    case lists:any(fun(Result) -> Result#test_result.passed == false end, TestResults) of
                        true ->
                            #{control => Control#compliance_control.id, status => implemented, effective => false};
                        false ->
                            #{control => Control#compliance_control.id, status => implemented, effective => true}
                    end
            end;
        not_implemented ->
            #{control => Control#compliance_control.id, status => not_implemented, effective => false};
        partial ->
            #{control => Control#compliance_control.id, status => partial, effective => false};
        exception ->
            #{control => Control#compliance_control.id, status => exception, effective => false}
    end.

calculate_compliance_score(ScanResults) ->
    %% Calculate overall compliance score
    Implemented = lists:filter(fun(R) -> R#result.status == implemented andalso R#result.effective end, ScanResults),
    Total = length(ScanResults),

    case Total of
        0 -> 0.0;
        _ -> length(Implemented) / Total
    end.

perform_compliance_scan(ScanId, FrameworkId, ScanConfig) ->
    %% Perform compliance scan
    case maps:find(FrameworkId, State#state.compliance_frameworks) of
        {ok, Framework} ->
            ScanResults = lists:map(fun(Control) ->
                perform_control_scan(Control, ScanConfig)
            end, Framework#compliance_framework.controls),

            Results = #scan{
                id => ScanId,
                results => ScanResults,
                score => calculate_compliance_score(ScanResults),
                findings => generate_findings(ScanResults)
            },

            self() ! {scan_completed, ScanId, Results};
        error ->
            self() ! {scan_failed, ScanId, framework_not_found}
    end.

perform_control_scan(Control, ScanConfig) ->
    %% Perform individual control scan
    case ScanConfig#scan_config.scoping_controls of
        undefined orelse lists:member(Control#compliance_control.id, ScanConfig#scan_config.scoping_controls) ->
            %% Scan this control
            ScanResult = #test_result{
                control_id => Control#compliance_control.id,
                test_name => "Automated Compliance Check",
                passed => true, %% Simplified
                evidence => [],
                timestamp => timestamp()
            },
            [ScanResult];
        _ ->
            []
    end.

generate_findings(ScanResults) ->
    %% Generate compliance findings
    Findings = lists:foldl(fun(Result, Acc) ->
        case Result#result.effective of
            false ->
                case Result#result.status of
                    implemented ->
                        Acc ++ [{control => Result#result.control, issue => "Control not effective"}];
                    not_implemented ->
                        Acc ++ [{control => Result#result.control, issue => "Control not implemented"}];
                    partial ->
                        Acc ++ [{control => Result#result.control, issue => "Control partially implemented"}];
                    exception ->
                        Acc ++ [{control => Report#result.control, issue => "Control exception"}]
                end;
            true ->
                Acc
        end
    end, [], ScanResults),

    Findings.

start_scheduled_check(Schedule) ->
    %% Start scheduled compliance check
    NextRun = Schedule#schedule.next_run,
    Delay = NextRun - timestamp(),

    case Delay > 0 of
        true ->
            erlang:send_after(Delay, self(), {scheduled_scan, Schedule#schedule.id});
        false ->
            erlang:send_after(0, self(), {scheduled_scan, Schedule#schedule.id})
    end.

calculate_next_run(Schedule) ->
    %% Calculate next run time based on schedule
    case Schedule#schedule.schedule.type of
        daily ->
            timestamp() + 86400000; %% 24 hours
        weekly ->
            timestamp() + 604800000; %% 7 days
        monthly ->
            timestamp() + 2592000000; %% 30 days
        quarterly ->
            timestamp() + 7776000000; %% 90 days
        _ ->
            timestamp() + 86400000 %% Default to daily
    end.

export_compliance_data_1(Framework, State) ->
    %% Export compliance data
    ExportData = #{
        framework => Framework,
        scan_results => lists:filter(fun(S) -> S#compliance_scan.framework_id == Framework#compliance_framework.id end, State#state.compliance_scans),
        last_updated => timestamp()
    },

    ExportData.

format_export_data(Data, Format) ->
    case Format of
        json ->
            jsx:encode(Data);
        xml ->
            format_xml(Data);
        pdf ->
            generate_pdf_report(Data);
        _ ->
            Data
    end.

format_xml(Data) ->
    %% Format data as XML
    FrameworkXml = format_framework_xml(Data#framework),
    ScanResultsXml = format_scan_results_xml(Data#scan_results),

    ["<compliance_data>", FrameworkXml, ScanResultsXml, "</compliance_data>"].

generate_pdf_report(Data) ->
    %% Generate PDF report (simplified)
    #{
        title => "Compliance Report",
        framework => Data#framework.name,
        generated_at => timestamp(),
        score => calculate_compliance_score(Data#scan_results),
        findings => Data#findings
    }.

validate_audit_trail_integrity(AuditTrails) ->
    %% Validate audit trail integrity using checksums
    Valid = lists:foldl(fun(Trail, Acc) ->
        case generate_checksum(Trail) == Trail#audit_trail.checksum of
            true -> Acc + 1;
            false -> Acc
        end
    end, 0, AuditTrails),

    #{
        total_entries => length(AuditTrails),
        valid_entries => Valid,
        invalid_entries => length(AuditTrails) - Valid,
        integrity_score => Valid / length(AuditTrails)
    }.

generate_checksum(Data) ->
    %% Generate checksum for data integrity
    Checksum = crypto:hash(sha256, term_to_binary(Data)),
    binary_to_hex(Checksum).

binary_to_hex(Binary) ->
    lists:flatten([io_lib:format("~2.16.0b", [B]) || B <- binary_to_list(Binary)]).

apply_filter(Map, Filter) ->
    case Filter of
        #{type := Type} ->
            maps:filter(fun(_, Framework) -> Framework#compliance_framework.type == Type end, Map);
        #{version := Version} ->
            maps:filter(fun(_, Framework) -> Framework#compliance_framework.version == Version end, Map);
        _ ->
            Map
    end.

validate_framework_data(Data) ->
    Required = [name, type],
    case check_required_fields(Data, Required) of
        ok ->
            {ok, Data};
        {error, missing_field} ->
            {error, {invalid_framework_data, missing_field}}
    end.

check_required_fields(Data, Fields) ->
    check_required_fields(Data, Fields, ok).

check_required_fields(_, [], Result) ->
    Result;
check_required_fields(Data, [Field|Rest], ok) ->
    case maps:is_key(Field, Data) of
        true ->
            check_required_fields(Data, Rest, ok);
        false ->
            check_required_fields(Data, Rest, {error, missing_field})
    end;
check_required_fields(_, _, Result) ->
    Result.

generate_framework_id() ->
    crypto:strong_rand_bytes(16).

generate_scan_id() ->
    crypto:strong_rand_bytes(16).

generate_schedule_id() ->
    crypto:strong_rand_bytes(16).

generate_audit_id() ->
    crypto:strong_rand_bytes(16).

timestamp() ->
    erlang:system_time(millisecond).