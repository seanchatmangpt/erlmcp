-module(erlmcp_intrusion_detection).

-behaviour(gen_server).

-export([start_link/0, monitor_connection/1, detect_intrusion/1, get_attack_signatures/0, update_signatures/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(SIGNATURES_DIR, "/etc/erlmcp/security/signatures").
-define(ATTACK_PATTERNS_DIR, "/etc/erlmcp/security/patterns").

-record(state, {
    attack_signatures :: map(),
    traffic_patterns :: map(),
    connection_monitor :: ets:tid(),
    alert_queue :: queue:queue(),
    detection_threshold :: float(),
    block_list :: list(),
    whitelisted_sources :: list(),
    analysis_window :: integer()
}).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec monitor_connection(map()) -> ok | {error, term()}.
monitor_connection(ConnectionData) when is_map(ConnectionData) ->
    gen_server:cast(?SERVER, {monitor_connection, ConnectionData}).

-spec detect_intrusion(map()) -> {ok, list()} | {error, term()}.
detect_intrusion(EventData) ->
    gen_server:call(?SERVER, {detect_intrusion, EventData}).

-spec get_attack_signatures() -> map().
get_attack_signatures() ->
    gen_server:call(?SERVER, get_attack_signatures).

-spec update_signatures(binary()) -> ok | {error, term()}.
update_signatures(SignatureData) when is_binary(SignatureData) ->
    gen_server:call(?SERVER, {update_signatures, SignatureData}).

%%====================================================================
%% Gen Server Callbacks
%%====================================================================

-spec init(term()) -> {ok, #state{}} | {stop, term()}.
init(_Args) ->
    process_flag(trap_exit, true),

    %% Initialize connection monitor
    ConnectionMonitor = ets:new(connection_monitor, [
        set, private, {keypos, 1},
        {write_concurrency, true}, {read_concurrency, true}
    ]),

    %% Initialize attack signatures
    AttackSignatures = load_attack_signatures(),

    %% Initialize traffic patterns
    TrafficPatterns = load_traffic_patterns(),

    %% Initialize state
    State = #state{
        attack_signatures = AttackSignatures,
        traffic_patterns = TrafficPatterns,
        connection_monitor = ConnectionMonitor,
        alert_queue = queue:new(),
        detection_threshold = application:get_env(erlmcp_security_monitoring, intrusion_threshold, 0.8),
        block_list = load_block_list(),
        whitelisted_sources = load_whitelisted_sources(),
        analysis_window = application:get_env(erlmcp_security_monitoring, analysis_window, 300000) % 5 minutes
    },

    %% Start pattern update timer
    erlang:send_after(300000, self(), update_patterns), % 5 minutes

    {ok, State}.

-spec handle_call(term(), {pid(), term()}, #state{}) -> {reply, term(), #state{}} | {noreply, #state{}}.
handle_call({detect_intrusion, EventData}, _From, State) ->
    %% Analyze event for intrusion patterns
    Analysis = analyze_event(EventData, State),

    case Analysis#intrusion_analysis.is_intrusion of
        true ->
            %% Generate intrusion alert
            Alert = generate_intrusion_alert(Analysis),
            queue_intrusion_alert(Alert, State),

            %% Take action if above threshold
            case Analysis#intrusion_analysis.confidence >= State#state.detection_threshold of
                true ->
                    take_intrusion_action(Analysis, State),
                    {reply, {ok, [Alert]}, State};
                false ->
                    {reply, {ok, []}, State}
            end;
        false ->
            {reply, {ok, []}, State}
    end;

handle_call(get_attack_signatures, _From, State) ->
    {reply, State#state.attack_signatures, State};

handle_call({update_signatures, SignatureData}, _From, State) ->
    %% Parse and update attack signatures
    case parse_signature_data(SignatureData) of
        {ok, NewSignatures} ->
            %% Merge with existing signatures
            MergedSignatures = merge_signatures(State#state.attack_signatures, NewSignatures),
            {reply, ok, State#state{attack_signatures = MergedSignatures}};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast({monitor_connection, ConnectionData}, State) ->
    %% Monitor connection for suspicious activity
    ConnectionId = generate_connection_id(ConnectionData),
    ets:insert(State#state.connection_monitor, {ConnectionId, ConnectionData, erlang:timestamp()}),

    %% Perform initial connection analysis
    case analyze_connection(ConnectionData, State) of
        suspicious ->
            %% Add to monitoring queue
            queue_connection_alert(ConnectionData, State);
        _ ->
            ok
    end,

    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info(update_patterns, State) ->
    %% Update attack signatures and patterns
    NewSignatures = load_attack_signatures(),
    NewPatterns = load_traffic_patterns(),

    %% Update state
    erlang:send_after(300000, self(), update_patterns),

    {noreply, State#state{attack_signatures = NewSignatures, traffic_patterns = NewPatterns}};

handle_info(process_intrusion_alerts, State) ->
    %% Process queued intrusion alerts
    process_alert_queue(State),

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
%% Internal Types and Functions
%%====================================================================

-record(intrusion_analysis, {
    connection_id :: binary(),
    attack_type :: binary(),
    confidence :: float(),
    is_intrusion :: boolean(),
    attack_vector :: list(),
    affected_resources :: list(),
    severity :: binary()
}).

-record(connection_metadata, {
    id :: binary(),
    source_ip :: binary(),
    destination_ip :: binary(),
    source_port :: integer(),
    destination_port :: integer(),
    protocol :: binary(),
    start_time :: integer(),
    duration :: integer(),
    bytes_sent :: integer(),
    bytes_received :: integer(),
    connection_count :: integer(),
    flags :: list()
}).

-record(attack_signature, {
    id :: binary(),
    name :: binary(),
    category :: binary(),
    description :: binary(),
    severity :: binary(),
    pattern :: binary(),
    enabled :: boolean(),
    updated :: integer()
}).

load_attack_signatures() ->
    %% Load attack signatures from file or default
    DefaultSignatures = load_default_signatures(),

    %% Try to load from file system
    case file:read_file(?SIGNATURES_DIR ++ "/attack_signatures.json") of
        {ok, Binary} ->
            try
                json_decode(Binary)
            catch
                _: _ -> DefaultSignatures
            end;
        _ -> DefaultSignatures
    end.

load_default_signatures() ->
    %% Default attack signatures
    #{
        sql_injection => #attack_signature{
            id => <<"sql_injection">>,
            name => "SQL Injection Attack",
            category => "web",
            description => "SQL command injection attempt",
            severity => <<"high">>,
            pattern => "union.*select.*from",
            enabled => true,
            updated => erlang:system_time(second)
        },
        xss_attack => #attack_signature{
            id => <<"xss_attack">>,
            name => "Cross-Site Scripting",
            category => "web",
            description => "Cross-site scripting attempt",
            severity => <<"high">>,
            pattern => "<script.*?>",
            enabled => true,
            updated => erlang:system_time(second)
        },
        ddos_attack => #attack_signature{
            id => <<"ddos_attack">>,
            name => "DDoS Attack",
            category => "network",
            description => "Distributed denial of service attack",
            severity => <<"critical">>,
            pattern => "flood|syn_flood|udp_flood",
            enabled => true,
            updated => erlang:system_time(second)
        },
        brute_force => #attack_signature{
            id => <<"brute_force">>,
            name => "Brute Force Attack",
            category => "auth",
            description => "Credential brute force attempt",
            severity => <<"high">>,
            pattern => "failed_login.*[0-9]{4,}",
            enabled => true,
            updated => erlang:system_time(second)
        }
    }.

load_traffic_patterns() ->
    %% Load normal traffic patterns
    DefaultPatterns = #{
        normal => #{
            connection_rate => {avg, 1000, max, 2000},
            packet_size => {avg, 1500, max, 4000},
            protocol_distribution => #{http => 0.4, https => 0.3, ssh => 0.2, other => 0.1},
            geographic_distribution => #{us => 0.4, eu => 0.3, asia => 0.2, other => 0.1}
        },
        peak => #{
            connection_rate => {avg, 5000, max, 8000},
            packet_size => {avg, 2000, max, 6000},
            protocol_distribution => #{http => 0.3, https => 0.4, ssh => 0.1, other => 0.2},
            geographic_distribution => #{us => 0.5, eu => 0.25, asia => 0.15, other => 0.1}
        }
    },

    %% Load from file if available
    case file:read_file(?ATTACK_PATTERNS_DIR ++ "/traffic_patterns.json") of
        {ok, Binary} ->
            try
                json_decode(Binary)
            catch
                _:_ -> DefaultPatterns
            end;
        _ -> DefaultPatterns
    end.

load_block_list() ->
    %% Load IP block list
    case file:read_file("/etc/erlmcp/security/blocklist.txt") of
        {ok, Binary} ->
            binary:split(Binary, [<<"\n">>], [global, trim]);
        _ ->
            %% Default block list for testing
            ["192.168.1.100", "10.0.0.50", "172.16.0.25"]
    end.

load_whitelisted_sources() ->
    %% Load whitelisted sources
    case file:read_file("/etc/erlmcp/security/whitelist.txt") of
        {ok, Binary} ->
            binary:split(Binary, [<<"\n">>], [global, trim]);
        _ ->
            ["127.0.0.1", "::1", "10.0.0.0/8"]
    end.

analyze_event(EventData, State) ->
    %% Analyze event for intrusion patterns
    ConnectionId = maps:get(connection_id, EventData),
    AttackSignatures = State#state.attack_signatures,

    %% Check for signature matches
    SignatureMatches = lists:foldl(fun({SigId, Sig}, Acc) ->
        case maps:get(enabled, Sig) of
            true ->
                case match_signature(EventData, Sig) of
                    true ->
                        [#intrusion_analysis{
                            connection_id = ConnectionId,
                            attack_type = SigId,
                            confidence = calculate_confidence(EventData, Sig),
                            is_intrusion = true,
                            attack_vector = extract_attack_vector(EventData),
                            affected_resources = extract_affected_resources(EventData),
                            severity = maps:get(severity, Sig)
                        } | Acc];
                    false -> Acc
                end;
            false -> Acc
        end
    end, [], maps:to_list(AttackSignatures)),

    case SignatureMatches of
        [] ->
            %% No signature match, check for anomaly
            #intrusion_analysis{
                connection_id = ConnectionId,
                attack_type = <<"anomaly">>,
                confidence = calculate_anomaly_score(EventData, State),
                is_intrusion = false,
                attack_vector = [],
                affected_resources = [],
                severity = <<"low">>
            };
        Matches ->
            %% Return highest confidence match
            lists:last(lists:sort(fun(A1, A2) -> A1#intrusion_analysis.confidence > A2#intrusion_analysis.confidence end, Matches))
    end.

match_signature(EventData, Signature) ->
    %% Check if event data matches attack signature
    Pattern = maps:get(pattern, Signature),
    EventContent = maps:get(content, EventData, <<>>),

    %% Simple pattern matching (in production, use regex)
    case binary:match(EventContent, Pattern) of
        nomatch -> false;
        _ -> true
    end.

calculate_confidence(EventData, Signature) ->
    %% Calculate confidence score for signature match
    BaseConfidence = case maps:get(severity, Signature) of
        <<"critical">> -> 0.95;
        <<"high">> -> 0.85;
        <<"medium">> -> 0.70;
        <<"low">> -> 0.55
    end,

    %% Adjust based on event characteristics
    EventFactors = analyze_event_factors(EventData),

    min(BaseConfidence + EventFactors, 1.0).

analyze_event_factors(EventData) ->
    %% Analyze factors affecting confidence
    Factors = 0.0,

    %% Check source reputation
    SourceIP = maps:get(source_ip, EventData),
    case is_source_suspicious(SourceIP, State) of
        true -> Factors + 0.1;
        false -> Factors
    end,

    %% Check time of day
    Hour = calendar:hour_of_day(erlang:timestamp()),
    if
        Hour < 6 or Hour > 22 -> Factors + 0.1;
        true -> Factors
    end,

    %% Check frequency
    Frequency = get_connection_frequency(SourceIP, State),
    if
        Frequency > 100 -> Factors + 0.2;
        Frequency > 50 -> Factors + 0.1;
        true -> Factors
    end,

    Factors.

is_source_suspicious(SourceIP, State) ->
    %% Check if source IP is suspicious
    lists:member(SourceIP, State#state.block_list).

get_connection_frequency(SourceIP, State) ->
    %% Get connection frequency for source IP
    case ets:lookup(State#state.connection_monitor, SourceIP) of
        [] -> 0;
        Connections -> length(Connections)
    end.

analyze_connection(ConnectionData, State) ->
    %% Analyze connection for suspicious activity
    SourceIP = maps:get(source_ip, ConnectionData),

    %% Check whitelist
    case is_whitelisted(SourceIP, State) of
        true -> normal;
        false ->
            %% Check for suspicious patterns
            case is_suspicious_connection(ConnectionData, State) of
                true -> suspicious;
                false -> normal
            end
    end.

is_whitelisted(SourceIP, State) ->
    %% Check if source is whitelisted
    lists:any(fun(Wildcard) ->
        case binary:match(Wildcard, <<"/">>) of
            nomatch -> SourceIP == Wildcard;
            _ -> matches_cidr(SourceIP, Wildcard)
        end
    end, State#state.whitelisted_sources).

matches_cidr(IP, CIDR) ->
    %% Check if IP matches CIDR notation
    case binary:split(CIDR, <<"/">>) of
        [Network, Bits] ->
            IPInt = ip_to_int(IP),
            NetworkInt = ip_to_int(Network),
            Mask = (bnot (1 bsl (32 - binary_to_integer(Bits)))) band 16#ffffffff,
            (IPInt band Mask) == (NetworkInt band Mask);
        [Network] ->
            IP == Network
    end.

ip_to_int(IP) when is_binary(IP) ->
    %% Convert IP string to integer
    Parts = binary:split(IP, <<".">>, [global]),
    lists:foldl(fun(Part, Acc) ->
        (Acc bsl 8) + binary_to_integer(Part)
    end, 0, Parts).

is_suspicious_connection(ConnectionData, State) ->
    %% Check if connection shows suspicious patterns
    Patterns = State#state.traffic_patterns,

    %% Check connection rate
    CurrentRate = maps:get(connection_rate, ConnectionData),
    NormalMax = maps:get(max, maps:get(normal, Patterns#connection_metadata.connection_rate, #{max => 1000})),
    PeakMax = maps:get(max, maps:get(peak, Patterns#connection_metadata.connection_rate, #{max => 5000})),

    case CurrentRate > NormalMax * 2 of
        true when CurrentRate < PeakMax -> false;
        true when CurrentRate > PeakMax -> true;
        false -> false
    end.

generate_intrusion_alert(Analysis) ->
    %% Generate intrusion alert
    #{
        timestamp => erlang:timestamp(),
        alert_id => generate_alert_id(),
        connection_id => Analysis#intrusion_analysis.connection_id,
        attack_type => Analysis#intrusion_analysis.attack_type,
        confidence => Analysis#intrusion_analysis.confidence,
        severity => Analysis#intrusion_analysis.severity,
        attack_vector => Analysis#intrusion_analysis.attack_vector,
        affected_resources => Analysis#intrusion_analysis.affected_resources,
        source => <<"erlmcp_intrusion_detection">>,
        action => block_intrusion(Analysis)
    }.

queue_intrusion_alert(Alert, State) ->
    %% Add alert to queue
    NewQueue = queue:in(Alert, State#state.alert_queue),

    %% Process alerts periodically
    case queue:len(NewQueue) >= 10 of
        true ->
            erlang:send_after(0, self(), process_intrusion_alerts);
        false ->
            erlang:send_after(5000, self(), process_intrusion_alerts)
    end,

    {noreply, State#state{alert_queue = NewQueue}}.

process_alert_queue(State) ->
    %% Process all queued alerts
    case queue:out(State#state.alert_queue) of
        {{value, Alert}, NewQueue} ->
            %% Send alert to SIEM
            erlmcp_siem_generic:send_event(Alert),

            %% Send to incident response
            erlmcp_incident_response_orchestrator:new_incident(Alert),

            %% Continue processing
            process_alert_queue(State#state{alert_queue = NewQueue});
        {empty, _} ->
            ok
    end.

take_intrusion_action(Analysis, State) ->
    %% Take action against detected intrusion
    ConnectionId = Analysis#intrusion_analysis.connection_id,

    case Analysis#intrusion_analysis.severity of
        <<"critical">> ->
            block_connection(ConnectionId, State),
            isolate_system(ConnectionId, State);
        <<"high">> ->
            block_connection(ConnectionId, State),
            notify_security_team(Analysis, State);
        <<"medium">> ->
            log_intrusion(Analysis, State);
        _ ->
            monitor_closely(ConnectionId, State)
    end.

block_connection(ConnectionId, State) ->
    %% Block the connection
    ets:delete(State#state.connection_monitor, ConnectionId),

    %% Add to block list
    BlockList = State#state.block_list ++ [ConnectionId],
    State#state{block_list = BlockList}.

isolate_system(ConnectionId, State) ->
    %% Isolate affected system
    Alert = #{
        timestamp => erlang:timestamp(),
        alert_id => generate_alert_id(),
        action => isolate,
        connection_id => ConnectionId,
        source => <<"erlmcp_intrusion_detection">>,
        reason => "critical intrusion detected"
    },

    erlmcp_siem_generic:send_event(Alert),
    erlmcp_incident_response_orchestrator:new_incident(Alert).

notify_security_team(Analysis, State) ->
    %% Notify security team
    Alert = #{
        timestamp => erlang:timestamp(),
        alert_id => generate_alert_id(),
        action => notify,
        connection_id => Analysis#intrusion_analysis.connection_id,
        attack_type => Analysis#intrusion_analysis.attack_type,
        severity => Analysis#intrusion_analysis.severity,
        source => <<"erlmcp_intrusion_detection">>,
        message => "Security team notification for intrusion attempt"
    },

    erlmcp_siem_generic:send_event(Alert).

log_intrusion(Analysis, State) ->
    %% Log intrusion for investigation
    Alert = #{
        timestamp => erlang:timestamp(),
        alert_id => generate_alert_id(),
        action => log,
        connection_id => Analysis#intrusion_analysis.connection_id,
        attack_type => Analysis#intrusion_analysis.attack_type,
        confidence => Analysis#intrusion_analysis.confidence,
        source => <<"erlmcp_intrusion_detection">>
    },

    erlmcp_siem_generic:send_event(Alert).

monitor_closely(ConnectionId, State) ->
    %% Put connection under close monitoring
    case ets:lookup(State#state.connection_monitor, ConnectionId) of
        [{_, Data, _}] ->
            ets:insert(State#state.connection_monitor, {ConnectionId, Data, erlang:timestamp(), monitored});
        _ ->
            ok
    end.

generate_connection_id(ConnectionData) ->
    %% Generate unique connection ID
    SourceIP = maps:get(source_ip, ConnectionData),
    DestIP = maps:get(destination_ip, ConnectionData),
    SourcePort = maps:get(source_port, ConnectionData),
    DestPort = maps:get(destination_port, ConnectionData),

    Hash = crypto:hash(sha256, <<SourceIP/binary, DestIP/binary, SourcePort:16, DestPort:16>>),
    binary:part(Hash, 0, 16).

generate_alert_id() ->
    %% Generate unique alert ID
    integer_to_binary(erlang:system_time(nanosecond)).

merge_signatures(Existing, New) ->
    %% Merge new signatures with existing ones
    maps:merge(Existing, New).

parse_signature_data(Binary) ->
    %% Parse signature data (placeholder)
    {ok, #{}}.

extract_attack_vector(EventData) ->
    %% Extract attack vector from event
    [].

extract_affected_resources(EventData) ->
    %% Extract affected resources from event
    [].

calculate_anomaly_score(EventData, State) ->
    %% Calculate anomaly score for unknown events
    0.5.

json_decode(Binary) ->
    %% JSON decode placeholder (use jsx in production)
    {}.