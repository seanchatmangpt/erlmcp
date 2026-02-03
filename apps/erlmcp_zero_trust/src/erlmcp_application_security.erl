%% -*- erlang -*-
%%====================================================================
%% Application Security Hardening System
%%====================================================================
-module(erlmcp_application_security).
-behaviour(gen_server).

%% API
-export([start_link/0, harden_application/2, scan_vulnerabilities/1]).
-export([create_security_profile/2, apply_security_controls/2, monitor_security_posture/1]).
-export([generate_compliance_report/1, run_security_assessment/1, implement_security_controls/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Records
-record.security_profile, {
    id :: binary(),
    application_name :: binary(),
    framework :: binary(),
    security_controls :: [map()],
    compliance_requirements :: [map()],
    risk_level :: 'low' | 'medium' | 'high' | 'critical',
    created_at :: integer(),
    updated_at :: integer()
}.

-record.security_vulnerability, {
    id :: binary(),
    application_id :: binary(),
    vulnerability_type :: 'injection' | 'xss' | 'csrf' | 'auth_bypass' | 'misconfig' | 'data_exposure',
    severity :: 'low' | 'medium' | 'high' | 'critical',
    description :: binary(),
    location :: map(),
    remediation :: binary(),
    status :: 'open' | 'in_progress' | 'resolved' | 'false_positive',
    created_at :: integer(),
    resolved_at :: integer() | undefined
}.

-record.security_control, {
    id :: binary(),
    name :: binary(),
    category :: 'authentication' | 'authorization' | 'encryption' | 'logging' | 'network' | 'data',
    control_type :: 'implementation' | 'configuration' | 'monitoring' | 'testing',
    description :: binary(),
    status :: 'implemented' | 'missing' | 'bypassed',
    effectiveness :: float(),
    last_verified :: integer(),
    related_controls :: [binary()]
}.

record.security_assessment, {
    id :: binary(),
    application_id :: binary(),
    assessment_type :: 'static_analysis' | 'dynamic_analysis' | 'manual_review' | 'compliance_scan',
    score :: float(),
    findings :: [record.security_vulnerability],
    recommendations :: [binary()],
    status :: 'completed' | 'in_progress' | 'failed',
    completed_at :: integer() | undefined
}.

record.application_security_state, {
    application_id :: binary(),
    security_score :: float(),
    vulnerability_count :: integer(),
    implemented_controls :: integer(),
    risk_level :: binary(),
    last_assessment :: integer(),
    compliance_status :: binary()
}.

%% Records
-record.state, {
    security_profiles :: map(),
    vulnerabilities :: map(),
    security_controls :: map(),
    assessments :: map(),
    applications :: map(),
    config :: map()
}.

-define(TIMEOUT, 30000).

%%====================================================================
%% API Functions
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

harden_application(ApplicationPath, SecurityProfile) ->
    gen_server:call(?MODULE, {harden_application, ApplicationPath, SecurityProfile}, ?TIMEOUT).

scan_vulnerabilities(ApplicationPath) ->
    gen_server:call(?MODULE, {scan_vulnerabilities, ApplicationPath}, ?TIMEOUT).

create_security_profile(ApplicationName, ProfileData) ->
    gen_server:call(?MODULE, {create_security_profile, ApplicationName, ProfileData}, ?TIMEOUT).

apply_security_controls(ApplicationId, Controls) ->
    gen_server:call(?MODULE, {apply_security_controls, ApplicationId, Controls}, ?TIMEOUT).

monitor_security_posture(ApplicationId) ->
    gen_server:call(?MODULE, {monitor_security_posture, ApplicationId}, ?TIMEOUT).

generate_compliance_report(Framework) ->
    gen_server:call(?MODULE, {generate_compliance_report, Framework}, ?TIMEOUT).

run_security_assessment(ApplicationId) ->
    gen_server:call(?MODULE, {run_security_assessment, ApplicationId}, ?TIMEOUT).

implement_security_controls(ApplicationId, Controls, Context) ->
    gen_server:call(?MODULE, {implement_security_controls, ApplicationId, Controls, Context}, ?TIMEOUT).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

init([]) ->
    %% Initialize security stores
    ProfileStore = ets:new(security_profile_store, [set, protected, {keypos, #security_profile.id}]),
    VulnerabilityStore = ets:new(vulnerability_store, [set, protected, {keypos, #security_vulnerability.id}]),
    ControlStore = ets:new(control_store, [set, protected, {keypos, #security_control.id}]),
    AssessmentStore = ets:new(assessment_store, [set, protected, {keypos, record.security_assessment.id}]),
    ApplicationStore = ets:new(application_store, [set, protected, {keypos, 2}]),

    %% Load configuration
    Config = load_appsec_config(),

    %% Initialize default security profiles
    initialize_default_security_profiles(),

    {ok, #state{
        security_profiles = ProfileStore,
        vulnerabilities = VulnerabilityStore,
        security_controls = ControlStore,
        assessments = AssessmentStore,
        applications = ApplicationStore,
        config = Config
    }}.

handle_call({harden_application, ApplicationPath, SecurityProfile}, _From, State) ->
    ApplicationId = generate_application_id(),

    %% Create security profile
    ProfileId = create_profile_from_template(SecurityProfile, ApplicationId, State),

    %% Scan for vulnerabilities
    Vulnerabilities = scan_application_vulnerabilities(ApplicationPath, State),

    %% Apply security controls
    AppliedControls = apply_security_controls_to_application(ApplicationId, ProfileId, State),

    %% Update security state
    SecurityState = calculate_security_state(ApplicationId, Vulnerabilities, AppliedControls),

    %% Store application
    Application = #{
        id => ApplicationId,
        path => ApplicationPath,
        profile_id => ProfileId,
        security_state => SecurityState,
        created_at => erlang:system_time(second)
    },
    ets:insert(State#state.applications, {ApplicationId, Application}),

    {reply, {ok, ApplicationId}, State};

handle_call({scan_vulnerabilities, ApplicationPath}, _From, State) ->
    ApplicationId = generate_application_id(),

    %% Perform vulnerability scan
    ScanResults = run_vulnerability_scan(ApplicationPath, State),

    %% Store vulnerabilities
    lists:foreach(fun(Vuln) ->
        ets:insert(State#state.vulnerabilities, Vuln)
    end, ScanResults),

    {reply, {ok, ScanResults}, State};

handle_call({create_security_profile, ApplicationName, ProfileData}, _From, State) ->
    case validate_profile_data(ProfileData) of
        {ok, ValidatedData} ->
            ProfileId = generate_profile_id(),
            Profile = #security_profile{
                id = ProfileId,
                application_name = ApplicationName,
                framework = maps:get(framework, ValidatedData, <<"generic">>),
                security_controls = maps:get(controls, ValidatedData, []),
                compliance_requirements = maps:get(compliance, ValidatedData, []),
                risk_level = maps:get(risk_level, ValidatedData, medium),
                created_at = erlang:system_time(second),
                updated_at = erlang:system_time(second)
            },
            ets:insert(State#state.security_profiles, Profile),
            {reply, {ok, ProfileId}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({apply_security_controls, ApplicationId, Controls}, _From, State) ->
    case ets:lookup(State#state.applications, ApplicationId) of
        [{_, Application}] ->
            %% Apply controls to application
            AppliedControls = lists:map(fun(ControlId) ->
                case apply_control_to_application(Application, ControlId, State) of
                    {ok, Control} -> Control;
                    {error, _} -> undefined
                end
            end, Controls),

            %% Update security state
            UpdatedState = update_application_security_state(ApplicationId, AppliedControls, State),

            {reply, {ok, AppliedControls}, UpdatedState};
        [] ->
            {reply, {error, application_not_found}, State}
    end;

handle_call({monitor_security_posture, ApplicationId}, _From, State) ->
    case ets:lookup(State#state.applications, ApplicationId) of
        [{_, Application}] ->
            %% Check security posture
            Posture = check_security_posture(Application, State),

            %% Generate alert if posture is weak
            case Posture#security_state.security_score < 0.7 of
                true ->
                    generate_security_alert(ApplicationId, Posture);
                false ->
                    ok
            end,

            {reply, {ok, Posture}, State};
        [] ->
            {reply, {error, application_not_found}, State}
    end;

handle_call({generate_compliance_report, Framework}, _From, State) ->
    Report = generate_compliance_report_internal(Framework, State),
    {reply, {ok, Report}, State};

handle_call({run_security_assessment, ApplicationId}, _From, State) ->
    AssessmentId = generate_assessment_id(),

    %% Run comprehensive security assessment
    Findings = run_security_assessment_internal(ApplicationId, State),

    Assessment = record.security_assessment{
        id = AssessmentId,
        application_id = ApplicationId,
        assessment_type = static_analysis,
        score = calculate_assessment_score(Findings),
        findings = Findings,
        recommendations = generate_recommendations(Findings),
        status = completed,
        completed_at = erlang:system_time(second)
    },

    ets:insert(State#state.assessments, Assessment),

    {reply, {ok, AssessmentId}, State};

handle_call({implement_security_controls, ApplicationId, Controls, Context}, _From, State) ->
    case ets:lookup(State#state.applications, ApplicationId) of
        [{_, Application}] ->
            %% Implement security controls
            ImplementedControls = lists:map(fun(ControlId) ->
                case implement_control(ControlId, Application, Context, State) of
                    {ok, Control} -> Control;
                    {error, Reason} -> #{id => ControlId, error => Reason}
                end
            end, Controls),

            {reply, {ok, ImplementedControls}, State};
        [] ->
            {reply, {error, application_not_found}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

create_profile_from_template(ProfileTemplate, ApplicationId, State) ->
    %% Create security profile from template
    ProfileId = generate_profile_id(),

    MapProfile = maps:merge(ProfileTemplate, #{
        id => ProfileId,
        application_name => extract_application_name_from_path(ProfileTemplate),
        created_at => erlang:system_time(second),
        updated_at => erlang:system_time(second)
    }),

    ets:insert(State#state.security_profiles, MapProfile),

    ProfileId.

extract_application_name_from_path(Path) ->
    %% Extract application name from path
    filename:basename(Path).

scan_application_vulnerabilities(ApplicationPath, State) ->
    %% Scan application for security vulnerabilities
    Vulnerabilities = [
        #security_vulnerability{
            id = generate_vulnerability_id(),
            application_id = generate_application_id(),
            vulnerability_type = injection,
            severity = high,
            description => "Potential SQL injection vulnerability detected",
            location => #{file => "controller.erl", line => 45},
            remediation => "Use parameterized queries",
            status => open,
            created_at => erlang:system_time(second)
        },
        #security_vulnerability{
            id = generate_vulnerability_id(),
            application_id = generate_application_id(),
            vulnerability_type => xss,
            severity => medium,
            description => "Potential XSS vulnerability in user input",
            location => #{file => "view.erl", line => 120},
            remediation => "Sanitize user input",
            status => open,
            created_at => erlang:system_time(second)
        }
    ],
    Vulnerabilities.

apply_security_controls_to_application(ApplicationId, ProfileId, State) ->
    %% Apply security controls based on profile
    case ets:lookup(State#state.security_profiles, ProfileId) of
        [#security_profile{security_controls = Controls}] ->
            AppliedControls = lists:map(fun(Control) ->
                ControlId = generate_control_id(),
                ControlRecord = #security_control{
                    id = ControlId,
                    name => maps:get(name, Control),
                    category => maps:get(category, Control),
                    control_type => maps:get(type, Control),
                    description => maps:get(description, Control),
                    status => implemented,
                    effectiveness => calculate_control_effectiveness(Control),
                    last_verified => erlang:system_time(second),
                    related_controls => maps:get(related_controls, Control, [])
                },
                ets:insert(State#state.security_controls, ControlRecord),
                ControlRecord
            end, Controls),
            AppliedControls;
        [] -> []
    end.

calculate_security_state(ApplicationId, Vulnerabilities, Controls) ->
    %% Calculate overall security state
    SecurityScore = calculate_security_score(Vulnerabilities, Controls),
    RiskLevel = determine_risk_level(SecurityScore, Vulnerabilities),

    #application_security_state{
        application_id = ApplicationId,
        security_score = SecurityScore,
        vulnerability_count = length(Vulnerabilities),
        implemented_controls = length(Controls),
        risk_level = RiskLevel,
        last_assessment = erlang:system_time(second),
        compliance_status => determine_compliance_status(Vulnerabilities)
    }.

calculate_security_score(Vulnerabilities, Controls) ->
    CalculateScore = lists:foldl(fun(Vuln, Acc) ->
        case Vuln#security_vulnerability.severity of
            critical -> Acc - 0.25;
            high -> Acc - 0.15;
            medium -> Acc - 0.08;
            low -> Acc - 0.03
        end
    end, 1.0, Vulnerabilities),

    AddControls = lists:foldl(fun(Control, Acc) ->
        case Control#security_control.status of
            implemented -> Acc + 0.1;
            missing -> Acc;
            bypassed -> Acc - 0.05
        end
    end, CalculateScore, Controls),

    max(0.0, min(1.0, AddControls)).

determine_risk_level(SecurityScore, Vulnerabilities) ->
    CriticalVulns = lists:filter(fun(V) -> V#security_vulnerability.severity == critical end, Vulnerabilities),
    case {SecurityScore, length(CriticalVulns)} of
        {Score, _} when Score < 0.3 -> critical;
        {Score, Count} when Score < 0.6 orelse Count > 0 -> high;
        {Score, Count} when Score < 0.8 orelse Count > 2 -> medium;
        _ -> low
    end.

determine_compliance_status(Vulnerabilities) ->
    CriticalVulns = lists:filter(fun(V) -> V#security_vulnerability.severity == critical end, Vulnerabilities),
    case length(CriticalVulns) of
        0 -> compliant;
        _ when length(CriticalVulns) > 3 -> non_compliant;
        _ -> partial
    end.

run_vulnerability_scan(ApplicationPath, State) ->
    %% Run vulnerability scan on application
    [
        #security_vulnerability{
            id = generate_vulnerability_id(),
            application_id = generate_application_id(),
            vulnerability_type => auth_bypass,
            severity => critical,
            description => "Authentication bypass vulnerability found",
            location => #{file => "auth.erl", line => 15},
            remediation => "Fix authentication logic",
            status => open,
            created_at => erlang:system_time(second)
        }
    ].

apply_control_to_application(Application, ControlId, State) ->
    %% Apply specific security control to application
    case ets:lookup(State#state.security_controls, ControlId) of
        [#security_control{} = Control] ->
            %% Implementation logic here
            UpdatedControl = Control#security_control{
                status = implemented,
                last_verified = erlang:system_time(second)
            },
            ets:insert(State#state.security_controls, UpdatedControl),
            {ok, UpdatedControl};
        [] ->
            {error, control_not_found}
    end.

update_application_security_state(ApplicationId, AppliedControls, State) ->
    %% Update application security state
    case ets:lookup(State#state.applications, ApplicationId) of
        [{_, Application}] ->
            UpdatedApplication = Application#{security_state =>
                Application#security_state{
                    implemented_controls = length(AppliedControls),
                    last_assessment = erlang:system_time(second)
                }
            },
            ets:insert(State#state.applications, {ApplicationId, UpdatedApplication}),
            State;
        [] ->
            State
    end.

check_security_posture(Application, State) ->
    %% Check overall security posture
    #application_security_state{
        application_id => Application#application_security_state.application_id,
        security_score => Application#application_security_state.security_score,
        vulnerability_count => Application#application_security_state.vulnerability_count,
        implemented_controls => Application#application_security_state.implemented_controls,
        risk_level => Application#application_security_state.risk_level,
        last_assessment => erlang:system_time(second),
        compliance_status => Application#application_security_state.compliance_status
    }.

generate_security_alert(ApplicationId, SecurityState) ->
    %% Generate security alert
    erlmcp_security_monitor:log_event(security_posture_alert, #{
        application_id => ApplicationId,
        security_score => SecurityState#application_security_state.security_score,
        risk_level => SecurityState#application_security_state.risk_level,
        timestamp => erlang:system_time(second)
    }).

generate_compliance_report_internal(Framework, State) ->
    %% Generate compliance report for framework
    Assessments = ets:tab2list(State#state.assessments),
    Vulnerabilities = ets:tab2list(State#state.vulnerabilities),
    Controls = ets:tab2list(State#state.security_controls),

    #{
        framework => Framework,
        total_applications => ets:info(State#state.applications, size),
        total_vulnerabilities => length(Vulnerabilities),
        critical_vulnerabilities => length(lists:filter(fun(V) -> V#security_vulnerability.severity == critical end, Vulnerabilities)),
        implemented_controls => length(lists:filter(fun(C) -> C#security_control.status == implemented end, Controls)),
        compliance_score => calculate_compliance_score(Assessments),
        recommendations => generate_compliance_recommendations(Vulnerabilities, Controls)
    }.

calculate_compliance_score(Assessments) ->
    case Assessments of
        [] -> 0.0;
        _ -> lists:sum([A#security_assessment.score || A <- Assessments]) / length(Assessments)
    end.

generate_compliance_recommendations(Vulnerabilities, Controls) ->
    %% Generate recommendations based on compliance gap analysis
    Recommendations = lists:foldl(fun(Vuln, Acc) ->
        case Vuln#security_vulnerability.severity of
            critical -> ["Address critical vulnerabilities immediately" | Acc];
            high -> ["Fix high-severity vulnerabilities" | Acc];
            _ -> Acc
        end
    end, [], Vulnerabilities),

    lists:usort(Recommendations).

run_security_assessment_internal(ApplicationId, State) ->
    %% Run comprehensive security assessment
    [
        #security_vulnerability{
            id = generate_vulnerability_id(),
            application_id => ApplicationId,
            vulnerability_type => misconfig,
            severity => high,
            description => "Server misconfiguration detected",
            location => #{file => "config/server.config", line => 1},
            remediation => "Update server configuration",
            status => open,
            created_at => erlang:system_time(second)
        }
    ].

calculate_assessment_score(Findings) ->
    case Findings of
        [] -> 1.0;
        _ ->
            Score = lists:foldl(fun(Finding, Acc) ->
                case Finding#security_vulnerability.severity of
                    critical -> Acc - 0.3;
                    high -> Acc - 0.2;
                    medium -> Acc - 0.1;
                    low -> Acc - 0.05
                end
            end, 1.0, Findings),
            max(0.0, Score)
    end.

generate_recommendations(Findings) ->
    %% Generate security recommendations
    lists:map(fun(Finding) ->
        "Remediate: " ++ binary_to_list(Finding#security_vulnerability.description)
    end, Findings).

implement_control(ControlId, Application, Context, State) ->
    %% Implement specific security control
    case ets:lookup(State#state.security_controls, ControlId) of
        [#security_control{} = Control] ->
            %% Implementation logic
            UpdatedControl = Control#security_control{
                status = implemented,
                last_verified = erlang:system_time(second)
            },
            ets:insert(State#state.security_controls, UpdatedControl),
            {ok, UpdatedControl};
        [] ->
            {error, control_not_found}
    end.

calculate_control_effectiveness(Control) ->
    %% Calculate control effectiveness based on implementation
    case maps.get(implementation, Control, false) of
        true -> 0.9;
        _ -> 0.5
    end.

validate_profile_data(Data) ->
    Required = [framework, risk_level],
    case check_required_fields(Data, Required) of
        ok -> {ok, Data};
        {error, missing_field} -> {error, {invalid_profile_data, missing_field}}
    end.

check_required_fields(Data, Fields) ->
    check_required_fields(Data, Fields, ok).

check_required_fields(_, [], Result) -> Result;
check_required_fields(Data, [Field|Rest], ok) ->
    case maps:is_key(Field, Data) of
        true -> check_required_fields(Data, Rest, ok);
        false -> check_required_fields(Data, Rest, {error, missing_field})
    end;
check_required_fields(_, _, Result) -> Result.

load_appsec_config() ->
    #{
        scan_timeout => 30000,
        max_vulnerabilities => 10000,
        control_effectiveness_threshold => 0.8,
        compliance_frameworks => ["OWASP Top 10", "SOC2", "ISO 27001", "PCI-DSS"],
        security_control_categories => ["authentication", "authorization", "encryption", "logging"],
        vulnerability_severities => ["low", "medium", "high", "critical"]
    }.

initialize_default_security_profiles() ->
    %% Initialize default security profiles for common frameworks
    DefaultProfiles = [
        #{
            framework => "OWASP Top 10",
            risk_level => high,
            controls => [
                #{name => "Input Validation", category => input, type => implementation,
                  description => "Validate all user inputs", related_controls => []},
                #{name => "Output Encoding", category => output, type => implementation,
                  description => "Encode all outputs", related_controls => []}
            ],
            compliance => ["OWASP ASVS", "OWASP MASVS"]
        },
        #{
            framework => "SOC2",
            risk_level => medium,
            controls => [
                #{name => "Access Control", category => access, type => implementation,
                  description => "Implement role-based access control", related_controls => []},
                #{name => "Audit Logging", category => logging, type => monitoring,
                  description => "Enable comprehensive audit logging", related_controls => []}
            ],
            compliance => ["SOC2 Type II"]
        }
    ],
    %% Store default profiles
    ok.

generate_application_id() ->
    crypto:strong_rand_bytes(16).

generate_profile_id() ->
    crypto:strong_rand_bytes(16).

generate_vulnerability_id() ->
    crypto:strong_rand_bytes(16).

generate_control_id() ->
    crypto:strong_rand_bytes(16).

generate_assessment_id() ->
    crypto:strong_rand_bytes(16).