-module(erlmcp_threat_detection_engine).

-behaviour(gen_server).

-export([start_link/0, analyze_event/1, get_detection_rules/0, add_rule/1, remove_rule/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_RULESET, "enterprise_fortune500").
-define(MAX_EVENTS_ANALYSIS, 10000).

-record(state, {
    detection_rules :: list(),
    event_buffer :: list(),
    threat_patterns :: map(),
    alert_threshold :: integer(),
    time_window :: integer(),
    analysis_stats :: map()
}).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec analyze_event(map()) -> ok | {error, term()}.
analyze_event(Event) when is_map(Event) ->
    gen_server:cast(?SERVER, {analyze_event, Event}).

-spec get_detection_rules() -> list().
get_detection_rules() ->
    gen_server:call(?SERVER, get_detection_rules).

-spec add_rule(map()) -> ok | {error, term()}.
add_rule(Rule) when is_map(Rule) ->
    gen_server:call(?SERVER, {add_rule, Rule}).

-spec remove_rule(binary()) -> ok | {error, term()}.
remove_rule(RuleId) when is_binary(RuleId) ->
    gen_server:call(?SERVER, {remove_rule, RuleId}).

%%====================================================================
%% Gen Server Callbacks
%%====================================================================

-spec init(term()) -> {ok, #state{}} | {stop, term()}.
init(_Args) ->
    process_flag(trap_exit, true),

    %% Initialize detection rules
    Rules = load_detection_rules(),

    %% Initialize threat patterns
    Patterns = load_threat_patterns(),

    %% Initialize state
    State = #state{
        detection_rules = Rules,
        event_buffer = [],
        threat_patterns = Patterns,
        alert_threshold = application:get_env(erlmcp_security_monitoring, alert_threshold, 5),
        time_window = application:get_env(erlmcp_security_monitoring, time_window, 300000), % 5 minutes
        analysis_stats = #{total_events => 0, threats_detected => 0, false_positives => 0}
    },

    %% Start periodic analysis
    erlang:send_after(60000, self(), periodic_analysis), % 1 minute

    {ok, State}.

-spec handle_call(term(), {pid(), term()}, #state{}) -> {reply, term(), #state{}} | {noreply, #state{}}.
handle_call(get_detection_rules, _From, State) ->
    {reply, State#state.detection_rules, State};

handle_call({add_rule, Rule}, _From, State) ->
    %% Validate rule
    case validate_rule(Rule) of
        true ->
            NewRules = [Rule | State#state.detection_rules],
            {reply, ok, State#state{detection_rules = NewRules}};
        false ->
            {reply, {error, invalid_rule}, State}
    end;

handle_call({remove_rule, RuleId}, _From, State) ->
    NewRules = lists:filter(fun(R) -> maps:get(<<"id">>, R) /= RuleId end,
                          State#state.detection_rules),
    {reply, ok, State#state{detection_rules = NewRules}};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast({analyze_event, Event}, State) ->
    %% Add event to buffer
    NewBuffer = [Event | State#state.event_buffer],

    %% Limit buffer size
    TrimmedBuffer = case length(NewBuffer) > ?MAX_EVENTS_ANALYSIS of
        true -> lists:sublist(NewBuffer, ?MAX_EVENTS_ANALYSIS);
        false -> NewBuffer
    end,

    %% Perform real-time analysis
    case analyze_real_time(Event, State) of
        threat_detected ->
            generate_alert(Event, State);
        _ ->
            ok
    end,

    %% Update stats
    NewStats = maps:update(total_events,
                          maps:get(total_events, State#state.analysis_stats) + 1,
                          State#state.analysis_stats),

    {noreply, State#state{event_buffer = TrimmedBuffer, analysis_stats = NewStats}};

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info(periodic_analysis, State) ->
    %% Perform comprehensive analysis of buffered events
    case analyze_comprehensive(State) of
        [] -> ok;
        Threats ->
            lists:foreach(fun(Threat) ->
                generate_alert(Threat, State)
            end, Threats)
    end,

    %% Clean old events from buffer
    CleanBuffer = clean_old_events(State#state.event_buffer, State#state.time_window),

    %% Continue periodic analysis
    erlang:send_after(60000, self(), periodic_analysis),

    {noreply, State#state{event_buffer = CleanBuffer}};

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

load_detection_rules() ->
    %% Load enterprise detection rules
    DefaultRules = [
        %% Brute Force Detection
        #{
            id => <<"brute_force_login">>,
            name => "Brute Force Login Attack",
            description => "Multiple failed login attempts from same source",
            severity => <<"high">>,
            condition => fun(Event, State) -> check_brute_force(Event, State) end,
            action => fun(Event) -> lock_account(Event), notify_security_team(Event) end,
            enabled => true
        },

        %% Data Exfiltration
        #{
            id => <<"data_exfiltration">>,
            name => "Unusual Data Transfer",
            description => "Large data transfer outside normal business hours",
            severity => <<"critical">>,
            condition => fun(Event, State) -> check_data_exfiltration(Event, State) end,
            action => fun(Event) -> block_transfer(Event), escalate_incident(Event) end,
            enabled => true
        },

        %% Anomalous Access Pattern
        #{
            id => <<"anomalous_access">>,
            name => "Anomalous Access Pattern",
            description => "User access pattern deviates from baseline",
            severity => <<"medium">>,
            condition => fun(Event, State) -> check_anomalous_access(Event, State) end,
            action => fun(Event) -> require_multi_factor(Event), monitor_behavior(Event) end,
            enabled => true
        },

        %% Privilege Escalation
        #{
            id => <<"privilege_escalation">>,
            name => "Privilege Escalation Attempt",
            description => "Unauthorized elevation of privileges",
            severity => <<"high">>,
            condition => fun(Event, State) -> check_privilege_escalation(Event, State) end,
            action => fun(Event) -> revoke_access(Event), initiate_investigation(Event) end,
            enabled => true
        },

        %% Malware Indicators
        #{
            id => <<"malware_indicators">>,
            name => "Malware Activity Detected",
            description => "Suspicious file patterns or network behavior",
            severity => <<"high">>,
            condition => fun(Event, State) -> check_malware_indicators(Event, State) end,
            action => fun(Event) -> isolate_system(Event), run_antivirus(Event) end,
            enabled => true
        }
    ],

    %% Load from configuration if available
    ConfigRules = application:get_env(erlmcp_security_monitoring, detection_rules, []),
    case ConfigRules of
        [] -> DefaultRules;
        Rules -> Rules ++ DefaultRules
    end.

load_threat_patterns() ->
    %% Load known threat patterns for correlation
    #{
        common_attack_patterns => [
            {ddos_indicators, [<<"syn_flood">>, <<"udp_flood">>, <<"http_flood">>]},
            {malware_hashes, [<<"e2ac723a8f5...">>, <<"d5a9...">>]}, % Example hashes
            {phishing_indicators, [<<"urgent_verify">>, <<"suspended_account">>]}
        ],
        baseline_behaviors => load_baseline_behaviors()
    }.

load_baseline_behaviors() ->
    %% Load normal operational patterns
    #{
        login_frequency => {avg, 10, max, 50}, % per hour
        data_transfer => {avg, 100, max, 1000}, % MB per hour
        access_patterns => load_access_patterns()
    }.

load_access_patterns() ->
    %% Define normal access patterns by role
    #{
        admin => {hours => [8, 9, 10, 11, 12, 13, 14, 15, 16],
                  locations => ["office_network", "vpn"]},
        user => {hours => [9, 10, 11, 12, 13, 14, 15, 16, 17],
                 locations => ["office_network", "vpn", "home_wifi"]},
        guest => {hours => [9, 10, 11, 12, 13, 14],
                 locations => ["lobby_wifi", "meeting_rooms"]}
    }.

validate_rule(Rule) ->
    RequiredFields = [<<"id">>, <<"name">>, <<"severity">>, <<"condition">>],
    lists:all(fun(Field) -> maps:is_key(Field, Rule) end, RequiredFields).

analyze_real_time(Event, State) ->
    %% Check against all enabled rules
    lists:foreach(fun(Rule) ->
        case maps:get(<<"enabled">>, Rule, true) of
            true ->
                try
                    case (maps:get(<<"condition">>, Rule))(Event, State) of
                        true ->
                            (maps:get(<<"action">>, Rule))(Event);
                        _ ->
                            ok
                    end
                catch
                    Error:Reason ->
                        error_logger:error_msg("Error in threat detection rule ~p: ~p:~p",
                                             [maps:get(<<"id">>, Rule), Error, Reason])
                end;
            false ->
                ok
        end
    end, State#state.detection_rules),

    threat_detected.

analyze_comprehensive(State) ->
    %% Analyze event buffer for complex patterns
    Buffer = State#state.event_buffer,
    Threats = [],

    %% Cross-rule correlation
    CorrelatedThreats = correlate_threats(Buffer, State),

    %% Time-series analysis
    TrendAnalysis = analyze_trends(Buffer, State),

    %% Statistical analysis
    StatisticalAnomalies = detect_statistical_anomalies(Buffer, State),

    CorrelatedThreats ++ TrendAnalysis ++ StatisticalAnomalies.

correlate_threats(Events, State) ->
    %% Look for patterns across multiple events
    Threats = [],

    %% Geographic correlation
    GeoThreats = correlate_geographic_events(Events),

    TemporalThreats = correlate_temporal_events(Events),

    EntityThreats = correlate_entity_events(Events),

    GeoThreats ++ TemporalThreats ++ EntityThreats.

correlate_geographic_events(Events) ->
    %% Group events by location and detect suspicious patterns
    Groups = lists:foldl(fun(Event, Acc) ->
        Location = maps:get(ip_address, Event, undefined),
        case Location of
            undefined -> Acc;
            _ -> maps:update(Location, [Event | maps:get(Location, Acc, [])], Acc)
        end
    end, #{}, Events),

    %% Check for distributed attacks
    lists:filter(fun({_Location, EventList}) ->
        length(EventList) > 5 % Threshold for distributed attack
    end, Groups).

correlate_temporal_events(Events) ->
    %% Group events by time intervals
    Groups = lists:foldl(fun(Event, Acc) ->
        Timestamp = maps:get(timestamp, Event, erlang:timestamp()),
        TimeBin = bin_timestamp(Timestamp),
        maps:update(TimeBin, [Event | maps:get(TimeBin, Acc, [])], Acc)
    end, #{}, Events),

    %% Check for burst activity
    lists:filter(fun({_TimeBin, EventList}) ->
        length(EventList) > 20 % Burst threshold
    end, Groups).

correlate_entity_events(Events) ->
    %% Group events by user/system
    Groups = lists:foldl(fun(Event, Acc) ->
        Entity = maps:get(user, Event, maps:get(source, Event, undefined)),
        case Entity of
            undefined -> Acc;
            _ -> maps:update(Entity, [Event | maps:get(Entity, Acc, [])], Acc)
        end
    end, #{}, Events),

    %% Check for suspicious activity by entity
    lists:filter(fun({_Entity, EventList}) ->
        has_suspicious_pattern(EventList)
    end, Groups).

bin_timestamp(Timestamp) when is_integer(Timestamp) ->
    %% Bin to 5-minute intervals
    trunc(Timestamp / 300) * 300;
bin_timestamp({_, _, Micro} = Ts) ->
    %% Erlang timestamp
    Sec = calendar:datetime_to_gregorian_seconds(Ts) - 719528 * 86400,
    trunc(Sec / 300) * 300.

has_suspicious_pattern(Events) ->
    %% Check for patterns indicating malicious activity
    FailedLogins = lists:filter(fun(E) ->
        maps:get(event_type, E, undefined) == <<"failed_login">>
    end, Events),

    length(FailedLogins) > 3.

analyze_trends(Events, State) ->
    %% Analyze trends in the event stream
    Trends = [],

    %% Login trend analysis
    LoginTrend = analyze_login_trend(Events),
    case LoginTrend of
        suspicious -> [generate_trend_alert(<<"login_spike">>, Events)];
        _ -> []
    end,

    %% Data transfer trend
    DataTrend = analyze_data_transfer_trend(Events),
    case DataTrend of
        suspicious -> [generate_trend_alert(<<"data_transfer_spike">>, Events)];
        _ -> []
    end,

    Trends.

detect_statistical_anomalies(Events, State) ->
    %% Use statistical methods to detect anomalies
    Anomalies = [],

    %% Frequency analysis
    FrequencyAnomalies = detect_frequency_anomalies(Events),

    %% Value-based anomalies
    ValueAnomalies = detect_value_anomalies(Events),

    FrequencyAnomalies ++ ValueAnomalies.

detect_frequency_anomalies(Events) ->
    %% Check for unusual frequencies
    EventCounts = count_event_types(Events),
    Baseline = get_baseline_frequencies(),

    lists:foldl(fun({EventType, Count}, Acc) ->
        BaselineCount = maps:get(EventType, Baseline, 0),
        if
            Count > BaselineCount * 3 -> % 3x baseline
                [generate_anomaly_alert(<<"frequency_anomaly">>, EventType, Count, BaselineCount) | Acc];
            true -> Acc
        end
    end, [], EventCounts).

detect_value_anomalies(Events) ->
    %% Check for unusual values in events
    lists:filter(fun(Event) ->
        case maps:get(value, Event, undefined) of
            undefined -> false;
            Value -> is_value_anomalous(Value)
        end
    end, Events).

is_value_anomalous(Value) when is_integer(Value) ->
    %% Simple Z-score calculation
    Mean = 100, % Placeholder
    StdDev = 30, % Placeholder
    Z = abs(Value - Mean) / StdDev,
    Z > 3; % 3-sigma threshold
is_value_anomalous(_) ->
    false.

check_brute_force(Event, State) ->
    %% Check for multiple failed logins from same source
    FailedLogins = lists:filter(fun(E) ->
        maps:get(event_type, E, undefined) == <<"failed_login">> andalso
        maps:get(ip_address, E) == maps:get(ip_address, Event)
    end, State#state.event_buffer),

    length(FailedLogins) > State#state.alert_threshold.

check_data_exfiltration(Event, State) ->
    %% Check for large data transfers
    Size = maps:get(data_size, Event, 0),

    %% Check time
    Hour = calendar:hour_of_day(erlang:timestamp()),

    Size > 1000000000 andalso % 1GB
    Hour < 6 or Hour > 22. % Outside business hours

check_anomalous_access(Event, State) ->
    %% Check if access pattern deviates from baseline
    User = maps:get(user, Event, undefined),
    case User of
        undefined -> false;
        _ ->
            UserEvents = lists:filter(fun(E) ->
                maps:get(user, E) == User
            end, State#state.event_buffer),
            analyze_user_pattern(UserEvents, User)
    end.

check_privilege_escalation(Event, State) ->
    %% Check for privilege escalation attempts
    EventType = maps:get(event_type, Event, undefined),
    SensitiveActions = [<<"sudo_access">>, <<"admin_console">>, <<"system_config">>],

    lists:member(EventType, SensitiveActions).

check_malware_indicators(Event, State) ->
    %% Check for malware behavior indicators
    Actions = [<<"exec_script">>, <<"/tmp/">>, <<"/var/tmp/">>],
    EventType = maps:get(event_type, Event, undefined),

    lists:member(EventType, Actions).

analyze_user_pattern(Events, User) ->
    %% Analyze user access patterns
    AccessTimes = lists:map(fun(E) ->
        calendar:hour_of_day(maps:get(timestamp, E))
    end, Events),

    % Simple deviation check
    Mean = lists:sum(AccessTimes) / length(AccessTimes),
    Deviation = lists:foldl(fun(Time, Acc) ->
        Acc + math:pow(Time - Mean, 2)
    end, 0, AccessTimes) / length(AccessTimes),

    math:sqrt(Deviation) > 4. % High deviation

clean_old_events(Events, TimeWindow) ->
    %% Remove events older than time window
    Now = erlang:timestamp(),
    lists:filter(fun(Event) ->
        Timestamp = maps:get(timestamp, Event, Now),
        TimeDiff = timer:now_diff(Now, Timestamp),
        TimeDiff < TimeWindow
    end, Events).

count_event_types(Events) ->
    lists:foldl(fun(Event, Acc) ->
        Type = maps:get(event_type, Event, <<"unknown">>),
        maps:update(Type, 1 + maps:get(Type, Acc, 0), Acc)
    end, #{}, Events).

get_baseline_frequencies() ->
    %% Return baseline frequencies for comparison
    #{
        <<"login">> => 100,
        <<"logout">> => 100,
        <<"data_access">> => 50,
        <<"file_download">> => 25
    }.

generate_alert(Event, State) ->
    %% Generate security alert
    Alert = #{
        timestamp => erlang:timestamp(),
        event_id => generate_event_id(),
        severity => maps:get(severity, Event, <<"medium">>),
        source => <<"erlmcp_threat_detection">>,
        event => Event,
        type => <<"threat_detected">>
    },

    %% Send to SIEM
    erlmcp_siem_generic:send_event(Alert),

    %% Send to incident response
    erlmcp_incident_response_orchestrator:new_incident(Alert),

    %% Update stats
    NewStats = maps:update(threats_detected,
                          maps:get(threats_detected, State#state.analysis_stats) + 1,
                          State#state.analysis_stats),

    State#state{analysis_stats = NewStats}.

generate_trend_alert(Type, Events) ->
    #{
        timestamp => erlang:timestamp(),
        event_id => generate_event_id(),
        severity => <<"medium">>,
        source => <<"erlmcp_trend_analysis">>,
        trend_type => Type,
        events_count => length(Events),
        type => <<"trend_alert">>
    }.

generate_anomaly_alert(Type, EventType, Current, Baseline) ->
    #{
        timestamp => erlang:timestamp(),
        event_id => generate_event_id(),
        severity => <<"high">>,
        source => <<"erlmcp_statistical_analysis">>,
        anomaly_type => Type,
        event_type => EventType,
        current_value => Current,
        baseline_value => Baseline,
        deviation => (Current - Baseline) / Baseline,
        type => <<"anomaly_alert">>
    }.

generate_event_id() ->
    integer_to_binary(erlang:system_time(second)).