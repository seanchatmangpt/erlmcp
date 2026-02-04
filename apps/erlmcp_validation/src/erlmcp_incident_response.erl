%% @doc Incident Response Coordinator
%% Manages security incidents and compliance breaches
%%
%% Responsibilities:
%% - Incident detection and classification
%% - Incident response coordination
%% - Communication management during incidents
%% - Compliance breach handling
%% - Root cause analysis
%% - Incident reporting and documentation
%% - Continuous improvement of incident response
%%
%% @end
-module(erlmcp_incident_response).

-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Incident Response API
-export([
    create_incident/2,
    update_incident/3,
    resolve_incident/2,
    escalate_incident/2,
    assign_incident/3,
    get_incident/1,
    get_active_incidents/1,
    get_incident_history/2,
    log_incident_action/4,
    generate_incident_report/2,
    execute_incident_plan/2
]).

%% Types
-type incident_type() :: security_breach | policy_violation | data_loss | system_failure | compliance_breach | phishing | malware | unauthorized_access.
-type incident_severity() :: low | medium | high | critical.
-type incident_status() :: new | in_progress | investigating | resolved | closed | escalated.
-type incident_phase() :: detection | analysis | containment | eradication | recovery | lessons_learned.

-record(incident, {
    id :: binary(),
    title :: binary(),
    description :: binary(),
    type :: incident_type(),
    severity :: incident_severity(),
    status :: incident_status(),
    phase :: incident_phase(),
    detected_at :: erlang:timestamp(),
    created_by :: binary(),
    assigned_to :: binary() | undefined,
    affected_systems :: [binary()],
    impact :: map(),
    root_cause :: binary() | undefined,
    remediation :: binary() | undefined,
    resolution :: binary() | undefined,
    actions :: [map()],
    communications :: [map()],
    compliance_impact :: map(),
    business_impact :: map(),
    notifications :: [map()],
    metadata :: map()
}).

-record(communication_log, {
    id :: binary(),
    incident_id :: binary(),
    sent_to :: binary(),
    sent_by :: binary(),
    message :: binary(),
    timestamp :: erlang:timestamp(),
    channel :: binary(),
    response :: binary() | undefined
}).

-record(incident_plan, {
    id :: binary(),
    name :: binary(),
    incident_type :: incident_type(),
    severity_min :: incident_severity(),
    severity_max :: incident_severity(),
    steps :: [map()],
    triggers :: [binary()],
    escalation_rules :: [map()],
    communications :: [map()]
}).

-record(state, {
    incidents :: [#incident{}],
    incident_plans :: [#incident_plan{}],
    communications :: [#communication_log{}],
    active_incidents :: [binary()],
    metrics :: map(),
    escalation_rules :: [map()],
    notification_channels :: [map()],
    response_thresholds :: map()
}).

%% Constants
-define(SERVER, ?MODULE).
-define(INCIDENT_MAX_AGE_DAYS, 365).
-define(ACTIVE_INCIDENTS_LIMIT, 100).
-define(COMMUNICATION_RETENTION_DAYS, 90).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the incident response coordinator
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Stop the incident response coordinator
-spec stop() -> ok.
stop() ->
    gen_server:call(?SERVER, stop).

%% @doc Create a new incident
-spec create_incident(map(), binary()) -> {ok, binary()} | {error, term()}.
create_incident(IncidentData, UserId) ->
    gen_server:call(?SERVER, {create_incident, IncidentData, UserId}).

%% @doc Update an incident
-spec update_incident(binary(), map(), binary()) -> ok | {error, term()}.
update_incident(IncidentId, UpdateData, UserId) ->
    gen_server:call(?SERVER, {update_incident, IncidentId, UpdateData, UserId}).

%% @doc Resolve an incident
-spec resolve_incident(binary(), binary()) -> ok | {error, term()}.
resolve_incident(IncidentId, Resolution) ->
    gen_server:call(?SERVER, {resolve_incident, IncidentId, Resolution}).

%% @doc Escalate an incident
-spec escalate_incident(binary(), binary()) -> ok | {error, term()}.
escalate_incident(IncidentId, EscalationReason) ->
    gen_server:call(?SERVER, {escalate_incident, IncidentId, EscalationReason}).

%% @doc Assign an incident to a user
-spec assign_incident(binary(), binary(), binary()) -> ok | {error, term()}.
assign_incident(IncidentId, UserId, AssignmentNotes) ->
    gen_server:call(?SERVER, {assign_incident, IncidentId, UserId, AssignmentNotes}).

%% @doc Get incident details
-spec get_incident(binary()) -> {ok, map()} | {error, term()}.
get_incident(IncidentId) ->
    gen_server:call(?SERVER, {get_incident, IncidentId}).

%% @doc Get active incidents
-spec get_active_incidents(binary()) -> {ok, [map()]} | {error, term()}.
get_active_incidents(Severity) ->
    gen_server:call(?SERVER, {get_active_incidents, Severity}).

%% @doc Get incident history
-spec get_incident_history(binary(), map()) -> {ok, [map()]} | {error, term()}.
get_incident_history(UserId, Filter) ->
    gen_server:call(?SERVER, {get_incident_history, UserId, Filter}).

%% @doc Log an incident action
-spec log_incident_action(binary(), binary(), map(), binary()) -> ok.
log_incident_action(IncidentId, Action, Details, UserId) ->
    gen_server:cast(?SERVER, {log_incident_action, IncidentId, Action, Details, UserId}).

%% @doc Generate incident report
-spec generate_incident_report(binary(), map()) -> {ok, binary()} | {error, term()}.
generate_incident_report(IncidentId, Options) ->
    gen_server:call(?SERVER, {generate_incident_report, IncidentId, Options}).

%% @doc Execute incident response plan
-spec execute_incident_plan(binary(), map()) -> ok | {error, term()}.
execute_incident_plan(IncidentId, Context) ->
    gen_server:call(?SERVER, {execute_incident_plan, IncidentId, Context}).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

init([]) ->
    %% Initialize with default incident plans and rules
    IncidentPlans = init_incident_plans(),
    EscalationRules = init_escalation_rules(),
    NotificationChannels = init_notification_channels(),
    ResponseThresholds = init_response_thresholds(),
    State = #state{
        incidents = [],
        incident_plans = IncidentPlans,
        communications = [],
        active_incidents = [],
        metrics = init_metrics(),
        escalation_rules = EscalationRules,
        notification_channels = NotificationChannels,
        response_thresholds = ResponseThresholds
    },

    %% Start incident monitoring
    {ok, State, 0}.

handle_call({create_incident, IncidentData, UserId}, _From, State) ->
    IncidentId = generate_incident_id(),
    Incident = create_incident_record(IncidentId, IncidentData, UserId),
    UpdatedIncidents = [Incident | State#state.incidents],
    UpdatedActiveIncidents = [IncidentId | State#state.active_incidents],

    %% Check if incident requires immediate escalation
    case requires_immediate_escalation(Incident) of
        true ->
            execute_escalation(Incident);
        false ->
            ok
    end,

    %% Notify stakeholders
    notify_stakeholders(Incident),

    %% Update metrics
    UpdatedMetrics = update_incident_metrics(State#state.metrics, Incident),

    {reply, {ok, IncidentId}, State#state{
        incidents = UpdatedIncidents,
        active_incidents = UpdatedActiveIncidents,
        metrics = UpdatedMetrics
    }};

handle_call({update_incident, IncidentId, UpdateData, UserId}, _From, State) ->
    case find_incident(IncidentId, State#state.incidents) of
        {ok, Incident} ->
            UpdatedIncident = update_incident_record(Incident, UpdateData, UserId),
            UpdatedIncidents = lists:keyreplace(IncidentId, #incident.id, State#state.incidents, UpdatedIncident),

            %% Check for escalation triggers
            case should_trigger_escalation(UpdatedIncident) of
                true -> execute_escalation(UpdatedIncident);
                false -> ok
            end,

            {reply, ok, State#state{incidents = UpdatedIncidents}};
        {error, not_found} ->
            {reply, {error, incident_not_found}, State}
    end;

handle_call({resolve_incident, IncidentId, Resolution}, _From, State) ->
    case find_incident(IncidentId, State#state.incidents) of
        {ok, Incident} ->
            ResolvedIncident = Incident#incident{
                status = resolved,
                resolution = Resolution,
                actions = [#{action => "resolve", resolution => Resolution, timestamp => erlang:timestamp()} | Incident#incident.actions],
                metadata = maps:put(resolved_at, erlang:timestamp(), Incident#incident.metadata)
            },
            UpdatedIncidents = lists:keyreplace(IncidentId, #incident.id, State#state.incidents, ResolvedIncident),
            UpdatedActiveIncidents = lists:delete(IncidentId, State#state.active_incidents),

            %% Generate lessons learned
            generate_lessons_learned(ResolvedIncident),

            %% Update metrics
            UpdatedMetrics = update_resolved_metrics(State#state.metrics, ResolvedIncident),

            {reply, ok, State#state{
                incidents = UpdatedIncidents,
                active_incidents = UpdatedActiveIncidents,
                metrics = UpdatedMetrics
            }};
        {error, not_found} ->
            {reply, {error, incident_not_found}, State}
    end;

handle_call({escalate_incident, IncidentId, EscalationReason}, _From, State) ->
    case find_incident(IncidentId, State#state.incidents) of
        {ok, Incident} ->
            EscalatedIncident = Incident#incident{
                status = escalated,
                actions = [#{action => "escalate", reason => EscalationReason, timestamp => erlang:timestamp()} | Incident#incident.actions]
            },
            UpdatedIncidents = lists:keyreplace(IncidentId, #incident.id, State#state.incidents, EscalatedIncident),

            ExecuteEscalation = execute_escalation(EscalatedIncident),

            {reply, ok, State#state{incidents = UpdatedIncidents}};
        {error, not_found} ->
            {reply, {error, incident_not_found}, State}
    end;

handle_call({assign_incident, IncidentId, UserId, AssignmentNotes}, _From, State) ->
    case find_incident(IncidentId, State#state.incidents) of
        {ok, Incident} ->
            AssignedIncident = Incident#incident{
                assigned_to = UserId,
                actions = [#{action => "assign", assigned_to => UserId, notes => AssignmentNotes, timestamp => erlang:timestamp()} | Incident#incident.actions]
            },
            UpdatedIncidents = lists:keyreplace(IncidentId, #incident.id, State#state.incidents, AssignedIncident),

            {reply, ok, State#state{incidents = UpdatedIncidents}};
        {error, not_found} ->
            {reply, {error, incident_not_found}, State}
    end;

handle_call({get_incident, IncidentId}, _From, State) ->
    case find_incident(IncidentId, State#state.incidents) of
        {ok, Incident} ->
            IncidentMap = incident_to_map(Incident),
            {reply, {ok, IncidentMap}, State};
        {error, not_found} ->
            {reply, {error, incident_not_found}, State}
    end;

handle_call({get_active_incidents, Severity}, _From, State) ->
    FilteredIncidents = case Severity of
        all -> State#state.incidents;
        _ -> lists:filter(fun(I) -> I#incident.severity =:= Severity end, State#state.incidents)
    end,
    ActiveIncidents = [incident_to_map(I) || I <- FilteredIncidents, lists:member(I#incident.id, State#state.active_incidents)],
    {reply, {ok, ActiveIncidents}, State};

handle_call({get_incident_history, UserId, Filter}, _From, State) ->
    UserIncidents = lists:filter(fun(I) ->
        case I#incident.created_by of
            UserId -> true;
            _ -> false
        end
    end, State#state.incidents),

    FilteredIncidents = filter_incidents(UserIncidents, Filter),
    {reply, {ok, [incident_to_map(I) || I <- FilteredIncidents]}, State};

handle_call({generate_incident_report, IncidentId, Options}, _From, State) ->
    case find_incident(IncidentId, State#state.incidents) of
        {ok, Incident} ->
            Report = generate_incident_report_content(Incident, Options),
            ReportId = generate_report_id(),
            {reply, {ok, ReportId}, State};
        {error, not_found} ->
            {reply, {error, incident_not_found}, State}
    end;

handle_call({execute_incident_plan, IncidentId, Context}, _From, State) ->
    case find_incident(IncidentId, State#state.incidents) of
        {ok, Incident} ->
            case find_incident_plan(Incident#incident.type, Incident#incident.severity, State#state.incident_plans) of
                {ok, Plan} ->
                    execute_plan_steps(Plan, Incident, Context),
                    {reply, ok, State};
                {error, not_found} ->
                    {reply, {error, incident_plan_not_found}, State}
            end;
        {error, not_found} ->
            {reply, {error, incident_not_found}, State}
    end;

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast({log_incident_action, IncidentId, Action, Details, UserId}, State) ->
    Communication = #communication_log{
        id = generate_communication_id(),
        incident_id = IncidentId,
        sent_by = UserId,
        message = Action,
        timestamp = erlang:timestamp(),
        channel = <<"action_log">>
    },
    UpdatedCommunications = [Communication | State#state.communications],

    ActionRecord = #{
        action => Action,
        details => Details,
        user_id => UserId,
        timestamp => erlang:timestamp()
    },

    UpdatedIncidents = case find_incident(IncidentId, State#state.incidents) of
        {ok, Incident} ->
            UpdatedIncident = Incident#incident{
                actions = [ActionRecord | Incident#incident.actions]
            },
            lists:keyreplace(IncidentId, #incident.id, State#state.incidents, UpdatedIncident);
        {error, _} -> State#state.incidents
    end,

    {noreply, State#state{
        communications = UpdatedCommunications,
        incidents = UpdatedIncidents
    }};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, State) ->
    %% Clean up old incidents and communications
    NewState = cleanup_expired_records(State),

    %% Check for active incidents requiring attention
    check_active_incidents(NewState),

    {noreply, NewState, 60000};  % Check every minute

handle_info(active_incident_check, State) ->
    %% Check active incidents
    check_active_incidents(State),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

init_incident_plans() ->
    %% Initialize standard incident response plans
    [
        #{
            id => "security_breach_response",
            name => "Security Breach Response Plan",
            incident_type => security_breach,
            severity_min => low,
            severity_max => critical,
            steps => [
                #{
                    step => 1,
                    action => "Immediate containment",
                    description => "Isolate affected systems",
                    estimated_time => "30 minutes",
                    required_resources => ["Security team", "Network team"]
                },
                #{
                    step => 2,
                    action => "Assessment",
                    description => "Determine scope and impact",
                    estimated_time => "2 hours",
                    required_resources => ["Forensics team", "Security team"]
                },
                #{
                    step => 3,
                    action => "Notification",
                    description => "Notify stakeholders and authorities",
                    estimated_time => "1 hour",
                    required_resources => ["Legal", "PR", "Management"]
                }
            ],
            triggers => ["unauthorized_access", "data_exfiltration", "malware_detection"],
            escalation_rules => [
                #{
                    condition => "severity >= critical",
                    action => "notify_executive",
                    recipients => ["CEO", "CISO", "Board"]
                }
            ],
            communications => [
                #{
                    recipient => "customers",
                    message => "Security incident notification",
                    template => "security_breach_customer_notification"
                }
            ]
        },
        #{
            id => "compliance_breach_response",
            name => "Compliance Breach Response Plan",
            incident_type => compliance_breach,
            severity_min => medium,
            severity_max => critical,
            steps => [
                #{
                    step => 1,
                    action => "Documentation",
                    description => "Document breach details",
                    estimated_time => "1 hour",
                    required_resources => ["Compliance officer", "Legal"]
                },
                #{
                    step => 2,
                    action => "Assessment",
                    description => "Determine regulatory impact",
                    estimated_time => "4 hours",
                    required_resources => ["Compliance team", "Legal"]
                },
                #{
                    step => 3,
                    action => "Reporting",
                    description => "File required reports",
                    estimated_time => "24 hours",
                    required_resources => ["Compliance officer"]
                }
            ],
            triggers => ["policy_violation", "data_breach", "audit_findings"],
            escalation_rules => [
                #{
                    condition => "standard = hipaa AND severity >= high",
                    action => "notify_hhs",
                    recipients => ["Compliance team", "Legal"]
                }
            ],
            communications => [
                #{
                    recipient => "regulatory_bodies",
                    message => "Compliance breach notification",
                    template => "compliance_breach_regulatory"
                }
            ]
        }
    ].

init_escalation_rules() ->
    %% Initialize escalation rules
    [
        #{
            condition => "severity = critical",
            action => "immediate_notification",
            recipients => ["CEO", "CISO", "COO"],
            time_limit => 15  % minutes
        },
        #{
            condition => "severity = high AND status = investigating",
            action => "management_notification",
            recipients => ["Department Heads"],
            time_limit => 60  % minutes
        },
        #{
            condition => "escalation_count > 2",
            action => "executive_review",
            recipients => ["Executive Team"],
            time_limit => 30  % minutes
        }
    ].

init_notification_channels() ->
    %% Initialize notification channels
    [
        #{
            id => "email",
            type => "email",
            enabled => true,
            configuration => #{
                smtp_server => "smtp.erlmcp.com",
                from => "incidents@erlmcp.com"
            }
        },
        #{
            id => "sms",
            type => "sms",
            enabled => true,
            configuration => #{
                provider => "twilio",
                from_number => "+1234567890"
            }
        },
        #{
            id => "slack",
            type => "slack",
            enabled => true,
            configuration => #{
                webhook_url => "https://hooks.slack.com/services/xxx",
                channel => "#incidents"
            }
        },
        #{
            id => "pagerduty",
            type => "pagerduty",
            enabled => true,
            configuration => #{
                service_key => "xxx",
                escalation_policy => "critical_incidents"
            }
        }
    ].

init_response_thresholds() ->
    %% Initialize response time thresholds
    #{
        low => #{
            detection => "4 hours",
            containment => "8 hours",
            resolution => "72 hours"
        },
        medium => #{
            detection => "2 hours",
            containment => "4 hours",
            resolution => "48 hours"
        },
        high => #{
            detection => "1 hour",
            containment => "2 hours",
            resolution => "24 hours"
        },
        critical => #{
            detection => "15 minutes",
            containment => "30 minutes",
            resolution => "8 hours"
        }
    }.

init_metrics() ->
    #{
        total_incidents => 0,
        active_incidents => 0,
        resolved_incidents => 0,
        average_resolution_time => 0,
        escalation_count => 0,
        notifications_sent => 0,
        last_incident => undefined
    }.

generate_incident_id() ->
    integer_to_binary(erlang:system_time(nanosecond)).

generate_communication_id() ->
    integer_to_binary(erlang:system_time(nanosecond)).

create_incident_record(IncidentId, IncidentData, UserId) ->
    Type = maps:get(type, IncidentData),
    Severity = maps:get(severity, IncidentData, medium),

    #incident{
        id = IncidentId,
        title = maps:get(title, IncidentData),
        description = maps:get(description, IncidentData),
        type = Type,
        severity = Severity,
        status = new,
        phase = detection,
        detected_at = erlang:timestamp(),
        created_by = UserId,
        affected_systems = maps:get(affected_systems, IncidentData, []),
        impact = maps:get(impact, IncidentData, #{}),
        actions = [
            #{
                action => "create",
                user_id => UserId,
                timestamp => erlang:timestamp()
            }
        ],
        compliance_impact = maps:get(compliance_impact, IncidentData, #{}),
        business_impact = maps:get(business_impact, IncidentData, #{}),
        notifications = [],
        metadata = maps:get(metadata, IncidentData, #{})
    }.

update_incident_record(Incident, UpdateData, UserId) ->
    UpdatedStatus = case maps:get(status, UpdateData, undefined) of
        undefined -> Incident#incident.status;
        NewStatus -> NewStatus
    end,

    UpdatedPhase = case maps:get(phase, UpdateData, undefined) of
        undefined -> Incident#incident.phase;
        NewPhase -> NewPhase
    end,

    Incident#incident{
        title = maps:get(title, UpdateData, Incident#incident.title),
        description = maps:get(description, UpdateData, Incident#incident.description),
        severity = maps:get(severity, UpdateData, Incident#incident.severity),
        status = UpdatedStatus,
        phase = UpdatedPhase,
        affected_systems = maps:get(affected_systems, UpdateData, Incident#incident.affected_systems),
        impact = maps:get(impact, UpdateData, Incident#incident.impact),
        root_cause = maps:get(root_cause, UpdateData, Incident#incident.root_cause),
        remediation = maps:get(remediation, UpdateData, Incident#incident.remediation),
        compliance_impact = maps:get(compliance_impact, UpdateData, Incident#incident.compliance_impact),
        business_impact = maps:get(business_impact, UpdateData, Incident#incident.business_impact),
        metadata = maps:merge(Incident#incident.metadata, maps:get(metadata, UpdateData, #{}))
    }.

find_incident(IncidentId, Incidents) ->
    case lists:keyfind(IncidentId, #incident.id, Incidents) of
        false -> {error, not_found};
        Incident -> {ok, Incident}
    end.

find_incident_plan(Type, Severity, Plans) ->
    case lists:filter(fun(P) ->
        P#incident_plan.incident_type =:= Type andalso
        Severity >= P#incident_plan.severity_min andalso
        Severity =< P#incident_plan.severity_max
    end, Plans) of
        [] -> {error, not_found};
        [Plan | _] -> {ok, Plan}
    end.

requires_immediate_escalation(#incident{severity = critical}) -> true;
requires_immediate_escalation(#incident{type = security_breach, severity = high}) -> true;
requires_immediate_escalation(#incident{type = compliance_breach, severity = high}) -> true;
requires_immediate_escalation(_) -> false.

should_trigger_escalation(#incident{severity = Severity, status = investigating}) ->
    case Severity of
        high -> true;
        critical -> true;
        _ -> false
    end;
should_trigger_escalation(_) -> false.

execute_escalation(Incident) ->
    %% Execute escalation procedures
    %% This would typically involve notifying specific teams
    %% and potentially escalating to higher management
    ok.

notify_stakeholders(Incident) ->
    %% Notify stakeholders based on incident severity and type
    ok.

execute_plan_steps(Plan, Incident, Context) ->
    %% Execute each step of the incident response plan
    lists:foreach(fun(Step) ->
        execute_plan_step(Step, Incident, Context)
    end, Plan#incident_plan.steps).

execute_plan_step(Step, Incident, Context) ->
    %% Execute a single step of the incident response plan
    case maps:get(action, Step) of
        "Immediate containment" ->
            execute_immediate_containment(Incident, Context);
        "Assessment" ->
            execute_assessment(Incident, Context);
        "Notification" ->
            execute_notification(Incident, Context);
        _ ->
            ok
    end.

execute_immediate_containment(Incident, _Context) ->
    %% Isolate affected systems
    ok.

execute_assessment(Incident, _Context) ->
    %% Determine scope and impact
    ok.

execute_notification(Incident, _Context) ->
    %% Notify stakeholders
    ok.

generate_lessons_learned(Incident) ->
    %% Generate lessons learned from resolved incident
    ok.

filter_incidents(Incidents, Filter) ->
    %% Filter incidents based on criteria
    lists:filter(fun(I) ->
        true
    end, Incidents).

incident_to_map(Incident) ->
    #{
        id => Incident#incident.id,
        title => Incident#incident.title,
        description => Incident#incident.description,
        type => Incident#incident.type,
        severity => Incident#incident.severity,
        status => Incident#incident.status,
        phase => Incident#incident.phase,
        detected_at => Incident#incident.detected_at,
        created_by => Incident#incident.created_by,
        assigned_to => Incident#incident.assigned_to,
        affected_systems => Incident#incident.affected_systems,
        impact => Incident#incident.impact,
        root_cause => Incident#incident.root_cause,
        remediation => Incident#incident.remediation,
        resolution => Incident#incident.resolution,
        actions => Incident#incident.actions,
        communications => Incident#incident.communications,
        compliance_impact => Incident#incident.compliance_impact,
        business_impact => Incident#incident.business_impact,
        notifications => Incident#incident.notifications,
        metadata => Incident#incident.metadata
    }.

generate_incident_report_content(Incident, _Options) ->
    %% Generate detailed incident report
    #{
        incident => incident_to_map(Incident),
        timeline => generate_incident_timeline(Incident),
        evidence => collect_evidence(Incident),
        recommendations => generate_recommendations(Incident),
        attachments => generate_incident_attachments(Incident)
    }.

generate_incident_timeline(Incident) ->
    %% Generate timeline of events
    lists:foldl(fun(Action, Acc) ->
        [
            #{
                time => maps:get(timestamp, Action),
                action => maps:get(action, Action),
                details => maps:get(details, Action, #{}),
                user_id => maps:get(user_id, Action)
            }
            | Acc
        ]
    end, [], lists:reverse(Incident#incident.actions)).

collect_evidence(Incident) ->
    %% Collect evidence related to incident
    [].

generate_recommendations(Incident) ->
    %% Generate recommendations for future incidents
    case Incident#incident.type of
        security_breach ->
            ["Implement additional monitoring", "Enhance access controls"];
        compliance_breach ->
            ["Update compliance policies", "Implement regular audits"];
        _ -> []
    end.

generate_incident_attachments(Incident) ->
    %% Generate incident report attachments
    [].

update_incident_metrics(Metrics, Incident) ->
    TotalIncidents = maps:get(total_incidents, Metrics, 0),
    ActiveIncidents = maps:get(active_incidents, Metrics, 0),
    Metrics#{
        total_incidents => TotalIncidents + 1,
        active_incidents => ActiveIncidents + 1,
        last_incident => erlang:timestamp()
    }.

update_resolved_metrics(Metrics, Incident) ->
    ResolutionTime = calculate_resolution_time(Incident),
    ActiveIncidents = maps:get(active_incidents, Metrics, 0),
    ResolvedIncidents = maps:get(resolved_incidents, Metrics, 0),

    Metrics#{
        active_incidents => ActiveIncidents - 1,
        resolved_incidents => ResolvedIncidents + 1,
        average_resolution_time => calculate_average_resolution_time(Metrics, ResolutionTime)
    }.

calculate_resolution_time(#incident{detected_at = Detected, resolution = Resolution}) ->
    case Resolution of
        undefined -> 0;
        _ -> erlang:time_diff(Detected, Resolution)
    end.

calculate_average_resolution_time(Metrics, NewTime) ->
    AverageResolutionTime = maps:get(average_resolution_time, Metrics, 0),
    case AverageResolutionTime of
        0 -> NewTime;
        Current -> (Current + NewTime) div 2
    end.

cleanup_expired_records(State) ->
    %% Clean up old incidents and communications
    CurrentTime = erlang:timestamp(),

    %% Remove old incidents
    CleanedIncidents = lists:filter(fun(I) ->
        time_diff(I#incident.detected_at, CurrentTime) < ?INCIDENT_MAX_AGE_DAYS * 24 * 60 * 60 * 1000
    end, State#state.incidents),

    %% Remove old communications
    CleanedCommunications = lists:filter(fun(C) ->
        time_diff(C#communication_log.timestamp, CurrentTime) < ?COMMUNICATION_RETENTION_DAYS * 24 * 60 * 60 * 1000
    end, State#state.communications),

    %% Update active incidents list
    UpdatedActiveIncidents = lists:filter(fun(I) ->
        lists:member(I#incident.id, State#state.active_incidents) andalso
        time_diff(I#incident.detected_at, CurrentTime) < ?INCIDENT_MAX_AGE_DAYS * 24 * 60 * 60 * 1000
    end, State#state.active_incidents),

    State#state{
        incidents = CleanedIncidents,
        communications = CleanedCommunications,
        active_incidents = UpdatedActiveIncidents
    }.

time_diff(Timestamp1, Timestamp2) ->
    %% Calculate time difference in milliseconds
    erlang:time_diff(Timestamp2, Timestamp1).

check_active_incidents(State) ->
    %% Check active incidents requiring attention
    lists:foreach(fun(I) ->
        case requires_attention(I) of
            true -> handle_needing_attention(I);
            false -> ok
        end
    end, State#state.incidents).

requires_attention(#incident{status = investigating, severity = Severity}) ->
    case Severity of
        high -> true;
        critical -> true;
        _ -> false
    end;
requires_attention(_) -> false.

handle_needing_attention(Incident) ->
    %% Handle incident requiring attention
    ok.

format_date(Date) ->
    %% Format date for display
    io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B", tuple_to_list(Date)).

generate_report_id() ->
    integer_to_binary(erlang:system_time(nanosecond)).

%%====================================================================
%% End of File
%%====================================================================