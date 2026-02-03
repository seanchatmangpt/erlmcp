-module(erlmcp_incident_response_orchestrator).

-behaviour(gen_server).

-export([start_link/0, new_incident/1, update_incident/2, resolve_incident/1, get_incidents/1, get_incident_dashboard/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(INCIDENT_TYPES, [
    {"security_breach", "Security Breach", "critical"},
    {"data_exfiltration", "Data Exfiltration", "critical"},
    {"malware_detection", "Malware Detection", "high"},
    {"unauthorized_access", "Unauthorized Access", "high"},
    {"suspicious_activity", "Suspicious Activity", "medium"},
    {"policy_violation", "Policy Violation", "medium"},
    {"compliance_violation", "Compliance Violation", "low"},
    {"false_positive", "False Positive", "low"}
]).
-define(MAX_INCIDENTS, 10000).
-define(ESCALATION_THRESHOLD, 60). % minutes

-record(incident, {
    id :: binary(),
    type :: binary(),
    severity :: binary(),
    title :: binary(),
    description :: binary(),
    source :: binary(),
    created_at :: integer(),
    status :: binary(),
    assigned_to :: binary() | undefined,
    priority :: integer(),
    affected_assets :: list(),
    root_cause :: binary() | undefined,
    containment_actions :: list(),
    eradication_actions :: list(),
    recovery_actions :: list(),
    lessons_learned :: binary() | undefined,
    metadata :: map(),
    timeline :: list()
}).

-record(state, {
    incidents :: ets:tid(),
    incident_timeline :: ets:tid(),
    playbooks :: ets:tid(),
    notifications :: queue:queue(),
    response_playbooks :: map(),
    escalation_rules :: list(),
    metrics :: map()
}).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec new_incident(map()) -> {ok, binary()} | {error, term()}.
new_incident(IncidentData) when is_map(IncidentData) ->
    gen_server:call(?SERVER, {new_incident, IncidentData}).

-spec update_incident(binary(), map()) -> ok | {error, term()}.
update_incident(IncidentId, UpdateData) when is_binary(IncidentId), is_map(UpdateData) ->
    gen_server:call(?SERVER, {update_incident, IncidentId, UpdateData}).

-spec resolve_incident(binary()) -> ok | {error, term()}.
resolve_incident(IncidentId) when is_binary(IncidentId) ->
    gen_server:call(?SERVER, {resolve_incident, IncidentId}).

-spec get_incidents(binary()) -> list().
get_incidents(Status) when is_binary(Status) ->
    gen_server:call(?SERVER, {get_incidents, Status}).

-spec get_incident_dashboard() -> map().
get_incident_dashboard() ->
    gen_server:call(?SERVER, get_incident_dashboard).

%%====================================================================
%% Gen Server Callbacks
%%====================================================================

-spec init(term()) -> {ok, #state{}} | {stop, term()}.
init(_Args) ->
    process_flag(trap_exit, true),

    %% Initialize incidents registry
    Incidents = ets:new(incidents, [
        set, private, {keypos, 1},
        {write_concurrency, true}, {read_concurrency, true}
    ]),

    %% Initialize incident timeline
    Timeline = ets:new(incident_timeline, [
        set, private, {keypos, 1},
        {write_concurrency, true}, {read_concurrency, true}
    ]),

    %% Initialize playbooks registry
    Playbooks = ets:new(playbooks, [
        set, private, {keypos, 1},
        {write_concurrency, true}, {read_concurrency, true}
    ]),

    %% Load response playbooks
    ResponsePlaybooks = load_response_playbooks(),

    %% Load escalation rules
    EscalationRules = load_escalation_rules(),

    %% Initialize state
    State = #state{
        incidents = Incidents,
        incident_timeline = Timeline,
        playbooks = Playbooks,
        notifications = queue:new(),
        response_playbooks = ResponsePlaybooks,
        escalation_rules = EscalationRules,
        metrics = initialize_metrics()
    },

    %% Start incident monitoring
    erlang:send_after(30000, self(), monitor_incidents), % 30 seconds

    %% Start playbook execution
    erlang:send_after(10000, self(), execute_playbooks), % 10 seconds

    {ok, State}.

-spec handle_call(term(), {pid(), term()}, #state{}) -> {reply, term(), #state{}} | {noreply, #state{}}.
handle_call({new_incident, IncidentData}, _From, State) ->
    %% Create new incident
    IncidentId = generate_incident_id(),
    Incident = create_incident(IncidentId, IncidentData),

    %% Insert incident
    ets:insert(State#state.incidents, Incident),
    ets:insert(State#state.incident_timeline, {IncidentId, [], erlang:timestamp()}),

    %% Add to timeline
    add_timeline_entry(IncidentId, "created", "Incident created", State),

    %% Update metrics
    NewMetrics = update_incident_metrics(State#state.metrics, "created", Incident),

    %% Trigger response playbook
    trigger_response_playbook(IncidentId, State),

    %% Notify stakeholders
    notify_stakeholders(IncidentId, State),

    %% Check for escalation
    check_escalation(IncidentId, State),

    {reply, {ok, IncidentId}, State#state{metrics = NewMetrics}};

handle_call({update_incident, IncidentId, UpdateData}, _From, State) ->
    %% Update existing incident
    case ets:lookup(State#state.incidents, IncidentId) of
        [{_, Incident}] ->
            UpdatedIncident = update_incident_details(Incident, UpdateData),
            ets:insert(State#state.incidents, UpdatedIncident),

            %% Add timeline entry
            add_timeline_entry(IncidentId, "updated", "Incident updated", State),

            %% Check for status change
            check_status_change(IncidentId, UpdatedIncident, State),

            {reply, ok, State};
        [] ->
            {reply, {error, incident_not_found}, State}
    end;

handle_call({resolve_incident, IncidentId}, _From, State) ->
    %% Resolve incident
    case ets:lookup(State#state.incidents, IncidentId) of
        [{_, Incident}] ->
            ResolvedIncident = Incident#incident{
                status = "resolved",
                resolved_at = erlang:system_time(second)
            },
            ets:insert(State#state.incidents, ResolvedIncident),

            %% Add timeline entry
            add_timeline_entry(IncidentId, "resolved", "Incident resolved", State),

            %% Generate lessons learned
            Lessons = generate_lessons_learned(IncidentId, State),
            UpdatedIncident = ResolvedIncident#incident{
                lessons_learned = Lessons
            },
            ets:insert(State#state.incidents, UpdatedIncident),

            %% Update metrics
            NewMetrics = update_incident_metrics(State#state.metrics, "resolved", UpdatedIncident),

            {reply, ok, State#state{metrics = NewMetrics}};
        [] ->
            {reply, {error, incident_not_found}, State}
    end;

handle_call({get_incidents, Status}, _From, State) ->
    %% Get incidents by status
    Incidents = ets:tab2list(State#state.incidents),
    Filtered = case Status of
        <<"all">> -> Incidents;
        _ -> [I || I <- Incidents, I#incident.status == Status]
    end,
    {reply, Filtered, State};

handle_call(get_incident_dashboard, _From, State) ->
    %% Get incident response dashboard
    Dashboard = generate_incident_dashboard(State),
    {reply, Dashboard, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info(monitor_incidents, State) ->
    %% Monitor active incidents
    ActiveIncidents = get_active_incidents(State),

    %% Check for pending actions
    lists:foreach(fun(Incident) ->
        check_pending_actions(Incident, State)
    end, ActiveIncidents),

    %% Check for SLA violations
    check_sla_violations(State),

    %% Continue monitoring
    erlang:send_after(30000, self(), monitor_incidents),

    {noreply, State};

handle_info(execute_playbooks, State) ->
    %% Execute response playbooks
    ActiveIncidents = get_active_incidents(State),

    lists:foreach(fun(Incident) ->
        execute_response_playbook(Incident, State)
    end, ActiveIncidents),

    %% Continue playbook execution
    erlang:send_after(10000, self(), execute_playbooks),

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

create_incident(IncidentId, IncidentData) ->
    %% Create incident record
    #incident{
        id = IncidentId,
        type = maps:get(type, IncidentData, <<"unknown">>),
        severity = maps:get(severity, IncidentData, <<"medium">>),
        title = maps:get(title, IncidentData, <<"Untitled Incident">>),
        description = maps:get(description, IncidentData, <<"No description">>),
        source = maps:get(source, IncidentData, <<"unknown">>),
        created_at = erlang:system_time(second),
        status = "new",
        priority = calculate_priority(IncidentData),
        affected_assets = maps:get(affected_assets, IncidentData, []),
        metadata = maps:get(metadata, IncidentData, #{}),
        timeline = []
    }.

calculate_priority(IncidentData) ->
    %% Calculate incident priority
    Severity = maps:get(severity, IncidentData, <<"medium">>),
    AffectedCount = length(maps:get(affected_assets, IncidentData, [])),

    case Severity of
        <<"critical">> -> 1;
        <<"high">> ->
            if AffectedCount > 10 -> 1;
               true -> 2
            end;
        <<"medium">> -> 3;
        <<"low">> -> 4;
        _ -> 5
    end.

generate_incident_id() ->
    %% Generate unique incident ID
    integer_to_binary(erlang:system_time(nanosecond)).

add_timeline_entry(IncidentId, Action, Description, State) ->
    %% Add entry to incident timeline
    case ets:lookup(State#state.incident_timeline, IncidentId) of
        [{_, Timeline, _Timestamp}] ->
            Entry = #{
                timestamp => erlang:timestamp(),
                action => Action,
                description => Description,
                user => <<"system">>
            },
            NewTimeline = [Entry | Timeline],
            ets:insert(State#state.incident_timeline, {IncidentId, NewTimeline, erlang:timestamp()});
        [] ->
            ok
    end.

update_incident_details(Incident, UpdateData) ->
    %% Update incident details
    UpdatedIncident = Incident#incident{
        title = maps:get(title, UpdateData, Incident#incident.title),
        description = maps:get(description, UpdateData, Incident#incident.description),
        status = maps:get(status, UpdateData, Incident#incident.status),
        assigned_to = maps:get(assigned_to, UpdateData, Incident#incident.assigned_to),
        root_cause = maps:get(root_cause, UpdateData, Incident#incident.root_cause),
        metadata = maps:merge(Incident#incident.metadata, maps:get(metadata, UpdateData, #{}))
    },

    %% Update affected assets if provided
    case maps:get(affected_assets, UpdateData, undefined) of
        undefined -> UpdatedIncident;
        Assets -> UpdatedIncident#incident{affected_assets = Assets}
    end.

trigger_response_playbook(IncidentId, State) ->
    %% Trigger appropriate response playbook
    case ets:lookup(State#state.incidents, IncidentId) of
        [{_, Incident}] ->
            Playbook = find_response_playbook(Incident, State),
            case Playbook of
                undefined -> ok;
                _ -> execute_playbook(Playbook, IncidentId, State)
            end;
        [] -> ok
    end.

find_response_playbook(Incident, State) ->
    %% Find appropriate response playbook
    Playbooks = ets:tab2list(State#state.playbooks),
    lists:foldl(fun(Playbook, Acc) ->
        case Playbook#playbook.type == Incident#incident.type andalso
             Playbook#playbook.severity == Incident#incident.severity of
            true -> Playbook;
            false -> Acc
        end
    end, undefined, Playbooks).

execute_playbook(Playbook, IncidentId, State) ->
    %% Execute response playbook
    Tasks = Playbook#playbook.tasks,
    lists:foreach(fun(Task) ->
        execute_task(Task, IncidentId, State)
    end, Tasks).

execute_task(Task, IncidentId, State) ->
    %% Execute individual playbook task
    case Task#task.type of
        notification ->
            send_notification(Task, IncidentId, State);
        containment ->
            execute_containment_action(Task, IncidentId, State);
        eradication ->
            execute_eradication_action(Task, IncidentId, State);
        recovery ->
            execute_recovery_action(Task, IncidentId, State);
        investigation ->
            execute_investigation_action(Task, IncidentId, State);
        _ ->
            log_task_status(Task, IncidentId, "skipped", "Unsupported task type", State)
    end.

send_notification(Task, IncidentId, State) ->
    %% Send notification as part of playbook
    Message = Task#task.message,
    Recipients = Task#task.recipients,

    Notification = #{
        incident_id => IncidentId,
        type => "notification",
        message => Message,
        recipients => Recipients,
        sent_at => erlang:timestamp(),
        status => "pending"
    },

    %% Add to notification queue
    NewQueue = queue:in(Notification, State#state.notifications),
    State#state{notifications = NewQueue}.

execute_containment_action(Task, IncidentId, State) ->
    %% Execute containment action
    Action = Task#task.action,

    %% Add to incident containment actions
    case ets:lookup(State#state.incidents, IncidentId) of
        [{_, Incident}] ->
            ContainedAction = #{
                action => Action,
                executed_by => <<"system">>,
                executed_at => erlang:timestamp(),
                status => "completed"
            },
            UpdatedIncident = Incident#incident{
                containment_actions = [ContainedAction | Incident#incident.containment_actions]
            },
            ets:insert(State#state.incidents, UpdatedIncident),
            log_task_status(Task, IncidentId, "completed", "Containment action executed", State);
        [] -> ok
    end.

execute_eradication_action(Task, IncidentId, State) ->
    %% Execute eradication action
    Action = Task#task.action,

    %% Add to incident eradication actions
    case ets:lookup(State#state.incidents, IncidentId) of
        [{_, Incident}] ->
            EradicatedAction = #{
                action => Action,
                executed_by => <<"system">>,
                executed_at => erlang:timestamp(),
                status => "pending"
            },
            UpdatedIncident = Incident#incident{
                eradication_actions = [EradicatedAction | Incident#incident.eradication_actions]
            },
            ets:insert(State#state.incidents, UpdatedIncident),
            log_task_status(Task, IncidentId, "pending", "Eradication action queued", State);
        [] -> ok
    end.

execute_recovery_action(Task, IncidentId, State) ->
    %% Execute recovery action
    Action = Task#task.action,

    %% Add to incident recovery actions
    case ets:lookup(State#state.incidents, IncidentId) of
        [{_, Incident}] ->
            RecoveredAction = #{
                action => Action,
                executed_by => <<"system">>,
                executed_at => erlang:timestamp(),
                status => "completed"
            },
            UpdatedIncident = Incident#incident{
                recovery_actions = [RecoveredAction | Incident#incident.recovery_actions]
            },
            ets:insert(State#state.incidents, UpdatedIncident),
            log_task_status(Task, IncidentId, "completed", "Recovery action executed", State);
        [] -> ok
    end.

execute_investigation_action(Task, IncidentId, State) ->
    %% Execute investigation action
    Investigation = #{
        task => Task#task.description,
        assigned_to => Task#task.assigned_to,
        started_at => erlang:timestamp(),
        status => "in_progress"
    },

    %% Update incident with investigation
    case ets:lookup(State#state.incidents, IncidentId) of
        [{_, Incident}] ->
            UpdatedIncident = Incident#incident{
                metadata = maps:put(<<"investigation">>, Investigation, Incident#incident.metadata)
            },
            ets:insert(State#state.incidents, UpdatedIncident),
            log_task_status(Task, IncidentId, "in_progress", "Investigation started", State);
        [] -> ok
    end.

log_task_status(Task, IncidentId, Status, Message, State) ->
    %% Log task execution status
    LogEntry = #{
        task_id => Task#task.id,
        incident_id => IncidentId,
        status => Status,
        message => Message,
        timestamp => erlang:timestamp()
    },

    %% Add to timeline
    add_timeline_entry(IncidentId, "task_" ++ binary_to_list(Status), Message, State).

get_active_incidents(State) ->
    %% Get all active incidents
    ets:tab2list(State#state.incidents),
    lists:filter(fun(Incident) ->
        lists:member(Incident#incident.status, ["new", "investigating", "containment", "recovery"])
    end, _).

check_status_change(IncidentId, UpdatedIncident, State) ->
    %% Check for status changes that require action
    case UpdatedIncident#incident.status of
        "investigating" ->
            start_investigation(IncidentId, State);
        "containment" ->
            start_containment(IncidentId, State);
        "recovery" ->
            start_recovery(IncidentId, State);
        _ -> ok
    end.

start_investigation(IncidentId, State) ->
    %% Start investigation phase
    Investigation = #{
        phase => "investigation",
        started_at => erlang:timestamp(),
        assigned_to => assign_investigator(IncidentId, State),
        evidence_collected => [],
        hypotheses => []
    },

    %% Update incident
    case ets:lookup(State#state.incidents, IncidentId) of
        [{_, Incident}] ->
            UpdatedIncident = Incident#incident{
                assigned_to => Investigation#{"assigned_to"},
                metadata = maps:put(<<"investigation">>, Investigation, Incident#incident.metadata)
            },
            ets:insert(State#state.incidents, UpdatedIncident),
            add_timeline_entry(IncidentId, "investigation_started", "Investigation phase started", State);
        [] -> ok
    end.

start_containment(IncidentId, State) ->
    %% Start containment phase
    ContainmentPlan = #{
        phase => "containment",
        started_at => erlang:timestamp(),
        actions => [],
        completed => false
    },

    %% Update incident
    case ets:lookup(State#state.incidents, IncidentId) of
        [{_, Incident}] ->
            UpdatedIncident = Incident#incident{
                metadata = maps:put(<<"containment">>, ContainmentPlan, Incident#incident.metadata)
            },
            ets:insert(State#state.incidents, UpdatedIncident),
            add_timeline_entry(IncidentId, "containment_started", "Containment phase started", State);
        [] -> ok
    end.

start_recovery(IncidentId, State) ->
    %% Start recovery phase
    RecoveryPlan = #{
        phase => "recovery",
        started_at => erlang:timestamp(),
        actions => [],
        systems_restored => []
    },

    %% Update incident
    case ets:lookup(State#state.incidents, IncidentId) of
        [{_, Incident}] ->
            UpdatedIncident = Incident#incident{
                metadata = maps:put(<<"recovery">>, RecoveryPlan, Incident#incident.metadata)
            },
            ets:insert(State#state.incidents, UpdatedIncident),
            add_timeline_entry(IncidentId, "recovery_started", "Recovery phase started", State);
        [] -> ok
    end.

notify_stakeholders(IncidentId, State) ->
    %% Notify stakeholders about new incident
    case ets:lookup(State#state.incidents, IncidentId) of
        [{_, Incident}] ->
            %% Create notification
            Notification = #{
                incident_id => IncidentId,
                type => "incident_created",
                message => "New incident created: " ++ binary_to_list(Incident#incident.title),
                severity => Incident#incident.severity,
                recipients => get_stakeholders_for_severity(Incident#incident.severity, State),
                sent_at => erlang:timestamp(),
                status => "pending"
            },

            %% Add to notification queue
            NewQueue = queue:in(Notification, State#state.notifications),
            State#state{notifications = NewQueue};
        [] -> ok
    end.

get_stakeholders_for_severity(Severity, State) ->
    %% Get stakeholders based on severity
    case Severity of
        <<"critical">> -> [<<"security_director">>, <<"cto">>, <<"cio">>];
        <<"high">> -> [<<"security_manager">>, <<"it_director">>];
        <<"medium">> -> [<<"security_team">>, <<"it_team">>];
        _ -> [<<"security_analyst">>]
    end.

check_escalation(IncidentId, State) ->
    %% Check if incident needs escalation
    case ets:lookup(State#state.incidents, IncidentId) of
        [{_, Incident}] ->
            case Incident#incident.severity of
                <<"critical">> ->
                    escalate_incident(IncidentId, Incident, State);
                _ -> ok
            end;
        [] -> ok
    end.

escalate_incident(IncidentId, Incident, State) ->
    %% Escalate critical incident
    EscalationNotice = #{
        incident_id => IncidentId,
        escalation_level => 1,
        escalated_to => [<<"security_director">>, <<"cto">>],
        escalated_at => erlang:timestamp(),
        reason => "Critical incident detected"
    },

    %% Add to timeline
    add_timeline_entry(IncidentId, "escalated", "Incident escalated to leadership", State),

    %% Update incident
    UpdatedIncident = Incident#incident{
        metadata = maps:put(<<"escalation">>, EscalationNotice, Incident#incident.metadata)
    },
    ets:insert(State#state.incidents, UpdatedIncident).

check_pending_actions(Incident, State) ->
    %% Check for pending actions that need attention
    case Incident#incident.status of
        "containment" ->
            check_containment_actions(Incident, State);
        "recovery" ->
            check_recovery_actions(Incident, State);
        _ -> ok
    end.

check_containment_actions(Incident, State) ->
    %% Check containment action status
    ContainmentActions = Incident#incident.containment_actions,
    PendingActions = [A || A <- ContainmentActions, A#{"status"} == "pending"],

    case length(PendingActions) of
        0 -> ok;
        _ ->
            %% Escalate if too many pending actions
            if length(PendingActions) > 5 ->
                   escalate_containment_issue(Incident, State);
               true ->
                   ok
            end
    end.

check_recovery_actions(Incident, State) ->
    %% Check recovery action status
    RecoveryActions = Incident#incident.recovery_actions,
    PendingActions = [A || A <- RecoveryActions, A#{"status"} == "pending"],

    case length(PendingActions) of
        0 -> ok;
        _ ->
            log_recovery_progress(Incident, PendingActions, State)
    end.

check_sla_violations(State) ->
    %% Check for SLA violations
    ActiveIncidents = get_active_incidents(State),

    lists:foreach(fun(Incident) ->
        case check_incident_sla(Incident) of
            {sla_violated, SLA} ->
                generate_sla_violation_alert(Incident, SLA, State);
            _ -> ok
        end
    end, ActiveIncidents).

check_incident_sla(Incident) ->
    %% Check if incident violates SLA
    CreatedAt = Incident#incident.created_at,
    Now = erlang:system_time(second),
    Elapsed = (Now - CreatedAt) / 60, % minutes

    case Incident#incident.severity of
        <<"critical">> when Elapsed > 15 -> {sla_violated, "critical_slb"};
        <<"high">> when Elapsed > 30 -> {sla_violated, "high_slb"};
        <<"medium">> when Elapsed > 60 -> {sla_violated, "medium_slb"};
        _ -> no_violation
    end.

generate_sla_violation_alert(Incident, SLA, State) ->
    %% Generate SLA violation alert
    Alert = #{
        timestamp => erlang:timestamp(),
        alert_id => generate_alert_id(),
        type => "sla_violation",
        incident_id => Incident#incident.id,
        sla_violated => SLA,
        message => "SLA violation detected for incident " ++ binary_to_list(Incident#incident.id),
        severity => "high",
        source => "erlmcp_incident_response_orchestrator"
    },

    %% Send to SIEM
    erlmcp_siem_generic:send_event(Alert).

generate_lessons_learned(IncidentId, State) ->
    %% Generate lessons learned from incident
    Incident = case ets:lookup(State#state.incidents, IncidentId) of
        [{_, I}] -> I;
        _ -> undefined
    end,

    case Incident of
        undefined -> <<"No lessons learned">>;
        _ ->
            Timeline = get_incident_timeline(IncidentId, State),
            ResponseTime = calculate_response_time(Timeline),
            Effectiveness = calculate_effectiveness(Incident),
            Prevention = generate_prevention_recommendations(Incident),

            #{
                response_time => ResponseTime,
                effectiveness => Effectiveness,
                prevention => Prevention,
                generated_at => erlang:timestamp()
            }
    end.

get_incident_timeline(IncidentId, State) ->
    %% Get timeline for incident
    case ets:lookup(State#state.incident_timeline, IncidentId) of
        [{_, Timeline, _}] -> Timeline;
        [] -> []
    end.

calculate_response_time(Timeline) ->
    %% Calculate response time from timeline
    Created = find_timeline_action("created", Timeline),
    Contained = find_timeline_action("containment_started", Timeline),
    Investigated = find_timeline_action("investigation_started", Timeline),

    case {Created, Contained, Investigated} of
        {_, {time, CreatedTime}, {time, ContainmentTime}} ->
            (ContainmentTime - CreatedTime) / 1000000; % Convert to seconds
        _ -> undefined
    end.

find_timeline_action(Action, Timeline) ->
    %% Find timeline action by type
    lists:foldl(fun(Entry, Acc) ->
        case maps:get(action, Entry) == Action of
            true -> {time, maps:get(timestamp, Entry)};
            false -> Acc
        end
    end, undefined, Timeline).

generate_incident_dashboard(State) ->
    %% Generate incident response dashboard
    AllIncidents = ets:tab2list(State#state.incidents),

    #{
        summary => #{
            total => length(AllIncidents),
            new => length([I || I <- AllIncidents, I#incident.status == "new"]),
            investigating => length([I || I <- AllIncidents, I#incident.status == "investigating"]),
            containment => length([I || I <- AllIncidents, I#incident.status == "containment"]),
            recovery => length([I || I <- AllIncidents, I#incident.status == "recovery"]),
            resolved => length([I || I <- AllIncidents, I#incident.status == "resolved"])
        },
        severity_distribution => #{
            critical => length([I || I <- AllIncidents, I#incident.severity == "critical"]),
            high => length([I || I <- AllIncidents, I#incident.severity == "high"]),
            medium => length([I || I <- AllIncidents, I#incident.severity == "medium"]),
            low => length([I || I <- AllIncidents, I#incident.severity == "low"])
        },
        recent_incidents => lists:sublist(lists:sort(fun(I1, I2) ->
            I1#incident.created_at > I2#incident.created_at
        end, AllIncidents), 10),
        metrics => State#state.metrics,
        pending_notifications => queue:len(State#state.notifications),
        active_playbooks => count_active_playbooks(State)
    }.

initialize_metrics() ->
    %% Initialize incident response metrics
    #{
        total_incidents => 0,
        resolved_incidents => 0,
        average_response_time => 0,
        average_resolution_time => 0,
        sla_compliance => 100,
        playbook_success_rate => 0,
        last_updated => erlang:timestamp()
    }.

update_incident_metrics(Metrics, Action, Incident) ->
    %% Update incident response metrics
    case Action of
        "created" ->
            Metrics#{
                total_incidents => Metrics#{"total_incidents"} + 1,
                last_updated => erlang:timestamp()
            };
        "resolved" ->
            Resolved = Metrics#{"resolved_incidents"} + 1,
            Total = Metrics#{"total_incidents"},
            ResolutionRate = (Resolved / Total) * 100,
            Metrics#{
                resolved_incidents => Resolved,
                resolution_rate => ResolutionRate,
                last_updated => erlang:timestamp()
            };
        _ -> Metrics
    end.

assign_investigator(IncidentId, State) ->
    %% Assign investigator to incident
    case Incident#incident.severity of
        <<"critical">> -> <<"lead_security_analyst">>;
        <<"high">> -> <<"senior_security_analyst">>;
        _ -> <<"security_analyst">>
    end.

escalate_containment_issue(Incident, State) ->
    %% Escalate containment issues
    Escalation = #{
        issue => "containment_delay",
        severity => "medium",
        escalated_to => [<<"security_director">>],
        escalated_at => erlang:timestamp()
    },

    add_timeline_entry(Incident#incident.id, "escalation", "Containment actions delayed", State).

log_recovery_progress(Incident, PendingActions, State) ->
    %% Log recovery progress
    Progress = #{
        completed => length(Incident#incident.recovery_actions) - length(PendingActions),
        total => length(Incident#incident.recovery_actions),
        percentage => (length(Incident#incident.recovery_actions) - length(PendingActions)) / length(Incident#incident.recovery_actions) * 100
    },

    add_timeline_entry(Incident#incident.id, "recovery_progress",
                      "Recovery progress: " ++ float_to_list(Progress#{"percentage"}) ++ "% complete", State).

execute_response_playbook(Incident, State) ->
    %% Execute response playbook for incident
    Playbook = find_response_playbook(Incident, State),
    case Playbook of
        undefined -> ok;
        _ -> execute_playbook(Playbook, Incident#incident.id, State)
    end.

count_active_playbooks(State) ->
    %% Count active playbooks
    ets:foldl(fun({_Id, Playbook}, Acc) ->
        case Playbook#playbook.status of
            "active" -> Acc + 1;
            _ -> Acc
        end
    end, 0, State#state.playbooks).

load_response_playbooks() ->
    %% Load response playbooks
    #{
        security_breach => #playbook{
            id => <<"security_breach">>,
            name => "Security Breach Response",
            type => "security_breach",
            severity => "critical",
            description => "Response playbook for security breaches",
            version => "1.0",
            created_at => erlang:system_time(second),
            updated_at => erlang:system_time(second),
            status => "active",
            tasks => [
                #task{
                    id => <<"notify_security_team">>,
                    name => "Notify Security Team",
                    type => "notification",
                    description => "Notify security team of incident",
                    recipients => [<<"security_team">>, <<"security_manager">>],
                    message => "Security breach detected - immediate attention required"
                },
                #task{
                    id => <<"isolate_system">>,
                    name => "Isolate System",
                    type => "containment",
                    description => "Isolate affected system",
                    action => "isolate_system"
                },
                #task{
                    id => <<"gather_evidence">>,
                    name => "Gather Evidence",
                    type => "investigation",
                    description => "Collect forensic evidence",
                    assigned_to => <<"forensics_team">>
                },
                #task{
                    id => <<"contain_threat">>,
                    name => "Contain Threat",
                    type => "containment",
                    description => "Contain the threat",
                    action => "contain_threat"
                }
            ]
        }
    }.

load_escalation_rules() ->
    %% Load escalation rules
    [
        #{
            id => <<"critical_incident_escalation">>,
            condition => "severity == 'critical'",
            action => "escalate_to_security_director",
            timeframe => 5 % minutes
        },
        #{
            id => <<"sla_violation_escalation">>,
            condition => "sla_violated == true",
            action => "escalate_to_cio",
            timeframe => 15 % minutes
        }
    ].

generate_alert_id() ->
    %% Generate unique alert ID
    integer_to_binary(erlang:system_time(nanosecond)).

%% Record definitions
-record(playbook, {
    id :: binary(),
    name :: binary(),
    type :: binary(),
    severity :: binary(),
    description :: binary(),
    version :: binary(),
    created_at :: integer(),
    updated_at :: integer(),
    status :: binary(),
    tasks :: list()
}).

-record(task, {
    id :: binary(),
    name :: binary(),
    type :: binary(),
    description :: binary(),
    action :: binary(),
    message :: binary(),
    recipients :: list(),
    assigned_to :: binary()
}).

-record(finding, {
    type :: binary(),
    severity :: binary(),
    description :: binary(),
    recommendation :: binary()
}).