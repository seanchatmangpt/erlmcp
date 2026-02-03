-module(erlmcp_security_posture_manager).

-behaviour(gen_server).

-export([start_link/0, assess_posture/1, get_security_score/0, get_compliance_status/0, generate_dashboard/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(ASSESSMENT_INTERVAL, 300000). % 5 minutes
-define(MAX_HISTORY, 1000).

-record(control, {
    id :: binary(),
    name :: binary(),
    category :: binary(),
    requirement :: binary(),
    enabled :: boolean(),
    configuration :: map(),
    compliance :: binary(),
    last_assessed :: integer()
}).

-record(asset, {
    id :: binary(),
    name :: binary(),
    type :: binary(),
    criticality :: binary(),
    security_controls :: list(),
    compliance_status :: binary(),
    risk_score :: float(),
    last_assessed :: integer()
}).

-record(posture_assessment, {
    timestamp :: integer(),
    overall_score :: float(),
    control_compliance :: map(),
    asset_risk_distribution :: map(),
    vulnerabilities :: list(),
    recommendations :: list(),
    compliance_framework :: binary()
}).

-record(state, {
    assets :: ets:tid(),
    controls :: ets:tid(),
    assessment_history :: list(),
    compliance_frameworks :: list(),
    thresholds :: map(),
    metrics :: map()
}).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec assess_posture(binary()) -> {ok, map()} | {error, term()}.
assess_posture(Framework) when is_binary(Framework) ->
    gen_server:call(?SERVER, {assess_posture, Framework}).

-spec get_security_score() -> float().
get_security_score() ->
    gen_server:call(?SERVER, get_security_score).

-spec get_compliance_status() -> map().
get_compliance_status() ->
    gen_server:call(?SERVER, get_compliance_status).

-spec generate_dashboard() -> map().
generate_dashboard() ->
    gen_server:call(?SERVER, generate_dashboard).

%%====================================================================
%% Gen Server Callbacks
%%====================================================================

-spec init(term()) -> {ok, #state{}} | {stop, term()}.
init(_Args) ->
    process_flag(trap_exit, true),

    %% Initialize assets registry
    Assets = ets:new(assets, [
        set, private, {keypos, 1},
        {write_concurrency, true}, {read_concurrency, true}
    ]),

    %% Initialize controls registry
    Controls = ets:new(controls, [
        set, private, {keypos, 1},
        {write_concurrency, true}, {read_concurrency, true}
    ]),

    %% Load compliance frameworks
    Frameworks = load_compliance_frameworks(),

    %% Load controls
    load_controls(Controls),

    %% Load assets
    load_assets(Assets),

    %% Initialize state
    State = #state{
        assets = Assets,
        controls = Controls,
        assessment_history = [],
        compliance_frameworks = Frameworks,
        thresholds = load_thresholds(),
        metrics = #{
            total_assets => 0,
            compliant_assets => 0,
            non_compliant_assets => 0,
            total_controls => 0,
            implemented_controls => 0,
            average_score => 0.0,
            last_assessment => undefined
        }
    },

    %% Start periodic assessment
    erlang:send_after(?ASSESSMENT_INTERVAL, self(), periodic_assessment),

    {ok, State}.

-spec handle_call(term(), {pid(), term()}, #state{}) -> {reply, term(), #state{}} | {noreply, #state{}}.
handle_call({assess_posture, Framework}, _From, State) ->
    %% Perform posture assessment for specified framework
    Assessment = perform_assessment(Framework, State),

    %% Add to history
    NewHistory = [Assessment | State#state.assessment_history],
    TrimmedHistory = case length(NewHistory) > ?MAX_HISTORY of
        true -> lists:sublist(NewHistory, ?MAX_HISTORY);
        false -> NewHistory
    end,

    %% Update metrics
    NewMetrics = update_metrics(State#state.metrics, Assessment),

    {reply, {ok, Assessment}, State#state{assessment_history = TrimmedHistory, metrics = NewMetrics}};

handle_call(get_security_score, _From, State) ->
    %% Get current security posture score
    LatestAssessment = get_latest_assessment(State),
    Score = case LatestAssessment of
        undefined -> 0.0;
        _ -> LatestAssessment#posture_assessment.overall_score
    end,
    {reply, Score, State};

handle_call(get_compliance_status, _From, State) ->
    %% Get compliance status
    LatestAssessment = get_latest_assessment(State),
    Status = case LatestAssessment of
        undefined -> #{};
        _ -> LatestAssessment#posture_assessment.control_compliance
    end,
    {reply, Status, State};

handle_call(generate_dashboard, _From, State) ->
    %% Generate security posture dashboard
    Dashboard = generate_security_dashboard(State),
    {reply, Dashboard, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info(periodic_assessment, State) ->
    %% Perform posture assessment for all frameworks
    lists:foreach(fun(Framework) ->
        Assessment = perform_assessment(Framework, State),
        NewHistory = [Assessment | State#state.assessment_history],
        TrimmedHistory = case length(NewHistory) > ?MAX_HISTORY of
            true -> lists:sublist(NewHistory, ?MAX_HISTORY);
            false -> NewHistory
        end,

        %% Update metrics
        NewMetrics = update_metrics(State#state.metrics, Assessment),
        State2 = State#state{assessment_history = TrimmedHistory, metrics = NewMetrics},

        %% Check for critical issues
        case Assessment#posture_assessment.overall_score < 60.0 of
            true ->
                notify_critical_issues(Assessment, State2);
            false ->
                ok
        end
    end, State#state.compliance_frameworks),

    %% Continue periodic assessment
    erlang:send_after(?ASSESSMENT_INTERVAL, self(), periodic_assessment),

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

load_compliance_frameworks() ->
    %% Load supported compliance frameworks
    [
        {<<"NIST_SP_800_53">>, "NIST Special Publication 800-53"},
        {<<"ISO_27001">>, "ISO/IEC 27001 Information Security Management"},
        {<<"SOC2">>, "Service Organization Control 2"},
        {<<"GDPR">>, "General Data Protection Regulation"},
        {<<"HIPAA">>, "Health Insurance Portability and Accountability Act"},
        {<<"PCI_DSS">>, "Payment Card Industry Data Security Standard"}
    ].

load_controls(ControlsTable) ->
    %% Load security controls
    DefaultControls = [
        #control{
            id => <<"access_control">>,
            name => "Access Control",
            category => "technical",
            requirement => "AC-1",
            enabled = true,
            configuration => #{
                multi_factor => true,
                least_privilege => true,
                password_policy => "strong",
                session_timeout => 3600
            },
            compliance => "compliant",
            last_assessed => erlang:system_time(second)
        },
        #control{
            id => <<"audit_logging">>,
            name => "Audit Logging",
            category => "technical",
            requirement => "AU-1",
            enabled = true,
            configuration => #{
                log_level => "info",
                retention_period => 365,
                log_rotation => true,
                centralized_logging => true
            },
            compliance => "compliant",
            last_assessed => erlang:system_time(second)
        },
        #control{
            id => <<"data_encryption">>,
            name => "Data Encryption",
            category => "technical",
            requirement => "SC-13",
            enabled = true,
            configuration => #{
                encryption_at_rest => true,
                encryption_in_transit => true,
                key_rotation => 90,
                algorithm => "AES-256"
            },
            compliance => "compliant",
            last_assessed => erlang:system_time(second)
        },
        #control{
            id => <<"network_security">>,
            name => "Network Security",
            category => "technical",
            requirement => "SC-7",
            enabled = true,
            configuration => #{
                firewall => true,
                intrusion_detection => true,
                network_segmentation => true,
                vulnerability_scanning => true
            },
            compliance => "non_compliant",
            last_assessed => erlang:system_time(second)
        },
        #control{
            id => <<"incident_response">>,
            name => "Incident Response",
            category => "operational",
            requirement => "IR-1",
            enabled = true,
            configuration => #{
                response_plan => true,
                communication_plan => true,
                incident_tracking => true,
                recovery_plan => true
            },
            compliance => "compliant",
            last_assessed => erlang:system_time(second)
        }
    ],

    lists:foreach(fun(Control) ->
        ets:insert(ControlsTable, Control)
    end, DefaultControls).

load_assets(AssetsTable) ->
    %% Load asset inventory
    DefaultAssets = [
        #asset{
            id => <<"app_server_1">>,
            name => "Application Server 1",
            type => "server",
            criticality => "high",
            security_controls => [<<"access_control">>, <<"audit_logging">>, <<"data_encryption">>],
            compliance_status => "compliant",
            risk_score => 3.2,
            last_assessed => erlang:system_time(second)
        },
        #asset{
            id => <<"db_server_1">>,
            name => "Database Server 1",
            type => "database",
            criticality => "critical",
            security_controls => [<<"access_control">>, <<"data_encryption">>, <<"audit_logging">>],
            compliance_status => "compliant",
            risk_score => 4.5,
            last_assessed => erlang:system_time(second)
        },
        #asset{
            id => <<"api_gateway">>,
            name => "API Gateway",
            type => "network",
            criticality => "high",
            security_controls => [<<"network_security">>, <<"access_control">>],
            compliance_status => "non_compliant",
            risk_score => 5.8,
            last_assessed => erlang:system_time(second)
        },
        #asset{
            id => <<"storage_system">>,
            name => "Storage System",
            type => "storage",
            criticality => "high",
            security_controls => [<<"data_encryption">>, <<"audit_logging">>],
            compliance_status => "compliant",
            risk_score => 2.8,
            last_assessed => erlang:system_time(second)
        }
    ],

    lists:foreach(fun(Asset) ->
        ets:insert(AssetsTable, Asset)
    end, DefaultAssets),

    %% Update metrics
    NumAssets = length(DefaultAssets),
    NumCompliant = length([A || A <- DefaultAssets, A#asset.compliance_status == "compliant"]),
    NumNonCompliant = NumAssets - NumCompliant,

    %% Update control metrics
    Controls = ets:tab2list(ets:info(AssetsTable, name)),
    NumControls = length(Controls),
    NumImplemented = length([C || C <- Controls, C#control.enabled]).

load_thresholds() ->
    %% Define assessment thresholds
    #{
        risk => #{
            low => 2.0,
            medium => 3.5,
            high => 5.0,
            critical => 7.0
        },
        compliance => #{
            excellent => 95,
            good => 85,
            fair => 75,
            poor => 65,
            critical => 50
        },
        control => #{
            implemented => true,
            tested => true,
            audited => true
        }
    }.

perform_assessment(Framework, State) ->
    %% Perform comprehensive security posture assessment
    Timestamp = erlang:system_time(second),

    %% Assess controls compliance
    ControlCompliance = assess_controls_compliance(State),

    %% Assess asset risk
    AssetRisk = assess_asset_risk(State),

    %% Calculate overall score
    OverallScore = calculate_overall_score(ControlCompliance, AssetRisk),

    %% Identify vulnerabilities
    Vulnerabilities = identify_vulnerabilities(State),

    %% Generate recommendations
    Recommendations = generate_recommendations(ControlCompliance, AssetRisk, Vulnerabilities),

    %% Create assessment record
    #posture_assessment{
        timestamp = Timestamp,
        overall_score = OverallScore,
        control_compliance = ControlCompliance,
        asset_risk_distribution = AssetRisk,
        vulnerabilities = Vulnerabilities,
        recommendations = Recommendations,
        compliance_framework = Framework
    }.

assess_controls_compliance(State) ->
    %% Assess compliance of security controls
    Controls = ets:tab2list(State#state.controls),
    ComplianceMap = #{},

    lists:foldl(fun(Control, Acc) ->
        ControlId = Control#control.id,
        ComplianceScore = calculate_control_compliance(Control, State),
        Status = determine_compliance_status(ComplianceScore, State),

        maps:put(ControlId, #{
            compliance_score => ComplianceScore,
            status => Status,
            enabled => Control#control.enabled,
            category => Control#control.category,
            last_assessed => Control#control.last_assessed
        }, Acc)
    end, ComplianceMap, Controls).

calculate_control_compliance(Control, State) ->
    %% Calculate compliance score for a control
    BaseScore = case Control#control.enabled of
        true -> 100;
        false -> 0
    end,

    Configuration = Control#control.configuration,
    ConfigurationScore = calculate_configuration_score(Configuration, Control#control.id),

    Metrics = get_control_metrics(Control#control.id, State),
    MetricsScore = calculate_metrics_score(Metrics, Control#control.id),

    %% Weighted average
    (BaseScore * 0.4) + (ConfigurationScore * 0.3) + (MetricsScore * 0.3).

calculate_configuration_score(Configuration, ControlId) ->
    %% Calculate configuration score based on requirements
    case ControlId of
        <<"access_control">> ->
            case {maps:get(multi_factor, Configuration, false),
                  maps:get(least_privilege, Configuration, false)} of
                {true, true} -> 100;
                {true, false} -> 70;
                {false, true} -> 70;
                {false, false} -> 0
            end;
        <<"audit_logging">> ->
            case {maps:get(log_level, Configuration, undefined),
                  maps:get(centralized_logging, Configuration, false)} of
                {_, true} when is_binary(maps:get(log_level, Configuration)) -> 100;
                {_, true} -> 80;
                {_, false} -> 0
            end;
        _ ->
            100 % Default to compliant if no specific rules
    end.

calculate_metrics_score(Metrics, ControlId) ->
    %% Calculate score based on operational metrics
    case ControlId of
        <<"access_control">> ->
            case Metrics of
                #{failures := F} when F < 10 -> 100;
                #{failures := F} when F < 50 -> 80;
                #{failures := F} when F < 100 -> 60;
                #{failures := _} -> 0
            end;
        _ ->
            100 % Default
    end.

determine_compliance_status(Score, State) ->
    %% Determine compliance status based on score
    Thresholds = State#state.thresholds.compliance,

    if
        Score >= Thresholds.excellent -> "excellent";
        Score >= Thresholds.good -> "good";
        Score >= Thresholds.fair -> "fair";
        Score >= Thresholds.poor -> "poor";
        true -> "critical"
    end.

assess_asset_risk(State) ->
    %% Assess risk across all assets
    Assets = ets:tab2list(State#state.assets),
    RiskDistribution = #{},

    lists:foldl(fun(Asset, Acc) ->
        RiskScore = Asset#asset.risk_score,
        RiskLevel = determine_risk_level(RiskScore, State),

        maps:update(RiskLevel, maps:get(RiskLevel, Acc, 0) + 1, Acc)
    end, RiskDistribution, Assets).

determine_risk_level(Score, State) ->
    %% Determine risk level based on score
    Thresholds = State#state.thresholds.risk,

    if
        Score < Thresholds.low -> "low";
        Score < Thresholds.medium -> "medium";
        Score < Thresholds.high -> "high";
        Score < Thresholds.critical -> "critical";
        true -> "extreme"
    end.

calculate_overall_score(ControlCompliance, AssetRisk) ->
    %% Calculate overall security posture score
    ControlScores = maps:values(ControlCompliance),
    AverageControlScore = lists:sum(ControlScores) / length(ControlScores),

    %% Weighted average of control compliance and asset risk
    (AverageControlScore * 0.7) + (calculate_risk_score(AssetRisk) * 0.3).

calculate_risk_score(RiskDistribution) ->
    %% Calculate risk score from distribution
    Total = maps:values(RiskDistribution),
    if
        Total == [] -> 0;
        true ->
            WeightedScore = lists:foldl(fun({Risk, Count}, Acc) ->
                RiskWeight = case Risk of
                    "low" -> 1;
                    "medium" -> 2;
                    "high" -> 3;
                    "critical" -> 4;
                    "extreme" -> 5
                end,
                Acc + (RiskWeight * Count)
            end, 0, maps:to_list(RiskDistribution)),

            (WeightedScore / lists:sum(Total)) * 20 % Normalize to 0-100 scale
    end.

identify_vulnerabilities(State) ->
    %% Identify security vulnerabilities
    Vulnerabilities = [],

    %% Check for unpatched systems
    Unpatched = check_unpatched_systems(State),
    case Unpatched of
        [] -> ok;
        _ ->
            lists:append(Vulnerabilities, [
                #{
                    id => <<"unpatched_systems">>,
                    severity => "high",
                    description => "Systems with known vulnerabilities",
                    affected_assets => Unpatched,
                    recommendation => "Apply security patches"
                }
            ])
    end,

    %% Check for weak configurations
    WeakConfigs = check_weak_configurations(State),
    case WeakConfigs of
        [] -> ok;
        _ ->
            lists:append(Vulnerabilities, [
                #{
                    id => <<"weak_configurations">>,
                    severity => "medium",
                    description => "Weak security configurations detected",
                    affected_controls => WeakConfigs,
                    recommendation => "Implement stronger configuration"
                }
            ])
    end,

    %% Check for misconfigurations
    Misconfigs = check_misconfigurations(State),
    case Misconfigs of
        [] -> ok;
        _ ->
            lists:append(Vulnerabilities, [
                #{
                    id => <<"security_misconfigurations">>,
                    severity => "high",
                    description => "Security misconfigurations found",
                    affected_assets => Misconfigs,
                    recommendation => "Fix misconfigurations"
                }
            ])
    end,

    Vulnerabilities.

check_unpatched_systems(State) ->
    %% Check for systems with unpatched vulnerabilities
    %% Placeholder implementation
    [].

check_weak_configurations(State) ->
    %% Check for weak security configurations
    %% Placeholder implementation
    [].

check_misconfigurations(State) ->
    %% Check for security misconfigurations
    %% Placeholder implementation
    [].

generate_recommendations(ControlCompliance, AssetRisk, Vulnerabilities) ->
    %% Generate actionable recommendations
    Recommendations = [],

    %% Generate control recommendations
    ControlRecs = generate_control_recommendations(ControlCompliance),
    Recommendations ++ ControlRecs,

    %% Generate risk recommendations
    RiskRecs = generate_risk_recommendations(AssetRisk),
    Recommendations ++ RiskRecs,

    %% Generate vulnerability recommendations
    VulnRecs = generate_vulnerability_recommendations(Vulnerabilities),
    Recommendations ++ VulnRecs,

    Recommendations.

generate_control_recommendations(ControlCompliance) ->
    %% Generate recommendations for controls
    lists:foldl(fun({ControlId, Compliance}, Acc) ->
        case Compliance#status of
            "poor" ->
                [#{
                    priority => "high",
                    control => ControlId,
                    recommendation => "Improve implementation of " ++ binary_to_list(ControlId),
                    impact => "High",
                    effort => "Medium"
                } | Acc];
            "fair" ->
                [#{
                    priority => "medium",
                    control => ControlId,
                    recommendation => "Enhance configuration of " ++ binary_to_list(ControlId),
                    impact => "Medium",
                    effort => "Low"
                } | Acc];
            _ -> Acc
        end
    end, [], maps:to_list(ControlCompliance)).

generate_risk_recommendations(AssetRisk) ->
    %% Generate recommendations for asset risks
    [].

generate_vulnerability_recommendations(Vulnerabilities) ->
    %% Generate recommendations for vulnerabilities
    lists:foldl(fun(Vulnerability, Acc) ->
        Rec = #{
            priority => case Vulnerability#severity of
                "critical" -> "critical";
                "high" -> "high";
                _ -> "medium"
            end,
            vulnerability => Vulnerability#id,
            recommendation => Vulnerability#recommendation,
            impact => "High",
            effort => case Vulnerability#id of
                <<"unpatched_systems">> -> "High";
                _ -> "Medium"
            end
        },
        [Rec | Acc]
    end, [], Vulnerabilities).

get_control_metrics(ControlId, State) ->
    %% Get operational metrics for a control
    case ControlId of
        <<"access_control">> -> #{failures => 0};
        _ -> #{}
    end.

update_metrics(Metrics, Assessment) ->
    %% Update security metrics with new assessment
    ControlCompliance = Assessment#posture_assessment.control_compliance,
    ControlScores = maps:values(ControlCompliance),
    AverageControlScore = lists:sum(ControlScores) / length(ControlScores),

    Metrics#{
        last_assessment => Assessment#posture_assessment.timestamp,
        average_score => AverageControlScore
    }.

get_latest_assessment(State) ->
    %% Get the latest posture assessment
    case State#state.assessment_history of
        [Latest | _] -> Latest;
        _ -> undefined
    end.

generate_security_dashboard(State) ->
    %% Generate comprehensive security posture dashboard
    LatestAssessment = get_latest_assessment(State),
    Metrics = State#state.metrics,

    #{
        overall_score => case LatestAssessment of
            undefined -> 0.0;
            _ -> LatestAssessment#posture_assessment.overall_score
        end,
        control_compliance => LatestAssessment#posture_assessment.control_compliance,
        asset_risk_distribution => LatestAssessment#posture_assessment.asset_risk_distribution,
        key_metrics => #{
            total_assets => ets:info(State#state.assets, size),
            compliant_assets => count_compliant_assets(State),
            non_compliant_assets => count_non_compliant_assets(State),
            total_controls => ets:info(State#state.controls, size),
            implemented_controls => count_implemented_controls(State),
            vulnerabilities => length(LatestAssessment#posture_assessment.vulnerabilities),
            recommendations => length(LatestAssessment#posture_assessment.recommendations)
        },
        compliance_status => assess_compliance_state(State),
        trends => analyze_trends(State),
        alerts => get_active_alerts(State),
        timestamp => erlang:timestamp()
    }.

count_compliant_assets(State) ->
    %% Count compliant assets
    length([A || A <- ets:tab2list(State#state.assets), A#asset.compliance_status == "compliant"]).

count_non_compliant_assets(State) ->
    %% Count non-compliant assets
    length([A || A <- ets:tab2list(State#state.assets), A#asset.compliance_status /= "compliant"]).

count_implemented_controls(State) ->
    %% Count implemented controls
    length([C || C <- ets:tab2list(State#state.controls), C#control.enabled]).

assess_compliance_state(State) ->
    %% Overall compliance state assessment
    LatestAssessment = get_latest_assessment(State),
    case LatestAssessment of
        undefined -> "unknown";
        _ ->
            OverallScore = LatestAssessment#posture_assessment.overall_score,
            if
                OverallScore >= 90 -> "excellent";
                OverallScore >= 80 -> "good";
                OverallScore >= 70 -> "fair";
                OverallScore >= 60 -> "poor";
                true -> "critical"
            end
    end.

analyze_trends(State) ->
    %% Analyze security posture trends
    Assessments = lists:sublist(State#state.assessment_history, 10),
    if
        length(Assessments) < 2 ->
            #{trend => "insufficient_data"};
        true ->
            Scores = [A#posture_assessment.overall_score || A <- Assessments],
            FirstScore = lists:nth(1, Scores),
            LastScore = lists:last(Scores),
            Change = LastScore - FirstScore,
            #{
                trend => if Change > 5 -> "improving";
                           Change < -5 -> "declining";
                           true -> "stable"
                        end,
                change_percentage => (Change / FirstScore) * 100,
                period => length(Assessments) * ?ASSESSMENT_INTERVAL / 1000 / 60 % minutes
            }
    end.

get_active_alerts(State) ->
    %% Get active security alerts
    [].

notify_critical_issues(Assessment, State) ->
    %% Notify about critical security issues
    Alert = #{
        timestamp => erlang:timestamp(),
        alert_id => generate_alert_id(),
        type => "critical_security_issue",
        message => "Security posture score dropped below threshold: " ++
                   float_to_list(Assessment#posture_assessment.overall_score) ++
                   " for framework " ++
                   binary_to_list(Assessment#posture_assessment.compliance_framework),
        severity => "critical",
        source => "erlmcp_security_posture_manager",
        recommendations => Assessment#posture_assessment.recommendations
    },

    %% Send to SIEM
    erlmcp_siem_generic:send_event(Alert),

    %% Send to incident response
    erlmcp_incident_response_orchestrator:new_incident(Alert).

generate_alert_id() ->
    %% Generate unique alert ID
    integer_to_binary(erlang:system_time(nanosecond)).