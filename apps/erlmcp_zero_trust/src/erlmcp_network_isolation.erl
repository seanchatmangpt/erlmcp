%% -*- erlang -*-
%%====================================================================
%% Zero-Trust Network Isolation System
%%====================================================================
-module(erlmcp_network_isolation).
-behaviour(gen_server).
-include("erlmcp_zero_trust_app.hrl").

%% API
-export([
    start_link/0,
    isolate_network/1,
    create_micro_segment/2,
    apply_network_policy/2,
    check_network_access/3,
    monitor_traffic/2,
    generate_network_report/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%% Records
-record(segment, {
    id :: binary(),
    name :: binary(),
    description :: binary(),
    network_zones :: [binary()],
    allowed_services :: [binary()],
    blocked_services :: [binary()],
    firewall_rules :: [map()],
    encryption_required :: boolean(),
    monitoring_enabled :: boolean(),
    created_at :: integer(),
    updated_at :: integer()
}).

-record.network_policy, {
    id :: binary(),
    name :: binary(),
    description :: binary(),
    source_segments :: [binary()],
    target_segments :: [binary()],
    allowed_traffic :: [map()],
    blocked_traffic :: [map()],
    encryption :: boolean(),
    monitoring :: boolean(),
    priority :: integer(),
    created_at :: integer(),
    updated_at :: integer()
}).

-record.traffic_flow, {
    id :: binary(),
    source :: binary(),
    target :: binary(),
    protocol :: binary(),
    ports :: [integer()],
    bytes :: integer(),
    packets :: integer(),
    start_time :: integer(),
    end_time :: integer() | undefined,
    allowed :: boolean(),
    encrypted :: boolean()
}.

-record.state, {
    segments :: map(),
    policies :: map(),
    traffic_flows :: map(),
    monitoring :: map(),
    config :: map(),
    firewalls :: map()
}.

%%====================================================================
%% API Functions
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

isolate_network(NetworkId) ->
    gen_server:call(?MODULE, {isolate_network, NetworkId}).

create_micro_segment(SegmentData) ->
    gen_server:call(?MODULE, {create_micro_segment, SegmentData}).

apply_network_policy(PolicyData) ->
    gen_server:call(?MODULE, {apply_network_policy, PolicyData}).

check_network_access(Source, Target, Protocol) ->
    gen_server:call(?MODULE, {check_network_access, Source, Target, Protocol}).

monitor_traffic(TrafficData, Context) ->
    gen_server:cast(?MODULE, {monitor_traffic, TrafficData, Context}).

generate_network_report(TimeRange) ->
    gen_server:call(?MODULE, {generate_report, TimeRange}).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

init([]) ->
    %% Initialize network isolation stores
    SegmentStore = ets:new(segment_store, [set, protected, {keypos, #segment.id}]),
    PolicyStore = ets:new(policy_store, [set, protected, {keypos, #network_policy.id}]),
    TrafficStore = ets:new(traffic_store, [set, protected, {keypos, #traffic_flow.id}]),
    FirewallStore = ets:new(firewall_store, [set, protected, {keypos, 2}]),

    %% Load configuration
    Config = load_network_config(),

    %% Initialize default micro-segments
    initialize_default_segments(),

    {ok, #{
        segments => SegmentStore,
        policies => PolicyStore,
        traffic_flows => TrafficStore,
        monitoring => #{},
        config => Config,
        firewalls => FirewallStore
    }}.

handle_call({isolate_network, NetworkId}, _From, State) ->
    Result = do_isolate_network(NetworkId, State),
    {reply, Result, State};

handle_call({create_micro_segment, SegmentData}, _From, State) ->
    Result = do_create_micro_segment(SegmentData, State),
    {reply, Result, State};

handle_call({apply_network_policy, PolicyData}, _From, State) ->
    Result = do_apply_network_policy(PolicyData, State),
    {reply, Result, State};

handle_call({check_network_access, Source, Target, Protocol}, _From, State) ->
    Result = do_check_network_access(Source, Target, Protocol, State),
    {reply, Result, State};

handle_call({generate_report, TimeRange}, _From, State) ->
    Result = do_generate_network_report(TimeRange, State),
    {reply, Result, State}.

handle_cast({monitor_traffic, TrafficData, Context}, State) ->
    do_monitor_traffic(TrafficData, Context, State),
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

do_isolate_network(NetworkId, State) ->
    #{segments := SegmentStore, config := Config} = State,

    %% Find network zones to isolate
    NetworkSegments = find_network_segments(NetworkId, SegmentStore),

    %% Apply isolation rules
    IsolationRules = generate_isolation_rules(NetworkSegments),

    %% Update firewall rules
    lists:foreach(fun(Segment) ->
        update_firewall_rules(Segment#segment.id, IsolationRules, State)
    end, NetworkSegments),

    %% Apply network policies
    apply_isolation_policies(NetworkSegments, State),

    {ok, isolated}.

do_create_micro_segment(SegmentData, State) ->
    #{segments := SegmentStore} = State,

    %% Validate segment data
    case validate_segment_data(SegmentData) of
        {ok, ValidatedData} ->
            SegmentId = generate_segment_id(),
            Segment = #segment{
                id = SegmentId,
                name = maps:get(name, ValidatedData),
                description = maps:get(description, ValidatedData),
                network_zones = maps:get(network_zones, ValidatedData, []),
                allowed_services = maps:get(allowed_services, ValidatedData, []),
                blocked_services = maps:get(blocked_services, ValidatedData, []),
                firewall_rules = maps:get(firewall_rules, ValidatedData, []),
                encryption_required = maps:get(encryption_required, ValidatedData, true),
                monitoring_enabled = maps:get(monitoring_enabled, ValidatedData, true),
                created_at = erlang:system_time(second),
                updated_at = erlang:system_time(second)
            },

            %% Store segment
            ets:insert(SegmentStore, Segment),

            %% Create initial firewall rules
            create_segment_firewall_rules(Segment, State),

            %% Apply default policies
            apply_default_segment_policies(Segment, State),

            {ok, SegmentId};
        {error, Reason} ->
            {error, Reason}
    end.

do_apply_network_policy(PolicyData, State) ->
    #{policies := PolicyStore} = State,

    case validate_policy_data(PolicyData) of
        {ok, ValidatedData} ->
            PolicyId = generate_policy_id(),
            Policy = #network_policy{
                id = PolicyId,
                name = maps:get(name, ValidatedData),
                description = maps:get(description, ValidatedData),
                source_segments = maps:get(source_segments, ValidatedData),
                target_segments = maps:get(target_segments, ValidatedData),
                allowed_traffic = maps:get(allowed_traffic, ValidatedData),
                blocked_traffic = maps:get(blocked_traffic, ValidatedData),
                encryption = maps:get(encryption, ValidatedData, true),
                monitoring = maps:get(monitoring, ValidatedData, true),
                priority = maps:get(priority, ValidatedData, 0),
                created_at = erlang:system_time(second),
                updated_at = erlang:system_time(second)
            },

            %% Store policy
            ets:insert(PolicyStore, Policy),

            %% Apply firewall rules
            apply_policy_firewall_rules(Policy, State),

            %% Start monitoring if enabled
            if
                Policy#network_policy.monitoring ->
                    start_policy_monitoring(Policy, State);
                true ->
                    ok
            end,

            {ok, PolicyId};
        {error, Reason} ->
            {error, Reason}
    end.

do_check_network_access(Source, Target, Protocol, State) ->
    #{policies := PolicyStore} = State,

    %% Find applicable policies
    ApplicablePolicies = find_applicable_policies(Source, Target, PolicyStore),

    %% Evaluate policies in priority order
    case evaluate_policies(ApplicablePolicies, Protocol, State) of
        {allow, Policy} ->
            {ok, allowed, Policy};
        {deny, Policy} ->
            {error, denied, Policy};
        {conditional, Policy, Conditions} ->
            {conditional, Policy, Conditions}
    end.

do_generate_network_report(TimeRange, State) ->
    #{traffic_flows := TrafficStore, monitoring := Monitoring} = State,

    %% Filter traffic by time range
    CurrentTime = erlang:system_time(second),
    StartTime = CurrentTime - TimeRange,

    RecentTraffic = ets:foldl(fun({_Key, Flow}, Acc) ->
        case Flow#traffic_flow.start_time >= StartTime of
            true ->
                [Flow | Acc];
            false ->
                Acc
        end
    end, [], TrafficStore),

    %% Generate report metrics
    Report = #{
        time_range => TimeRange,
        total_flows => length(RecentTraffic),
        allowed_flows => length(lists:filter(fun(F) -> F#traffic_flow.allowed end, RecentTraffic)),
        blocked_flows => length(lists:filter(fun(F) -> not F#traffic_flow.allowed end, RecentTraffic)),
        total_bytes => lists:foldl(fun(F, Acc) -> Acc + F#traffic_flow.bytes end, 0, RecentTraffic),
        total_packets => lists:foldl(fun(F, Acc) -> Acc + F#traffic_flow.packets end, 0, RecentTraffic),
        by_protocol => analyze_by_protocol(RecentTraffic),
        by_segment => analyze_by_segment(RecentTraffic),
        security_metrics => generate_security_metrics(Monitoring)
    },

    {ok, Report}.

%% Network isolation implementation
initialize_default_segments() ->
    %% Initialize default micro-segments
    DefaultSegments = [
        #{
            name => "application-tier",
            description => "Application server segment",
            network_zones => ["app-zone"],
            allowed_services => ["http", "https", "database"],
            blocked_services => ["ftp", "telnet", "rsh"],
            encryption_required => true,
            monitoring_enabled => true
        },
        #{
            name => "database-tier",
            description => "Database server segment",
            network_zones => ["db-zone"],
            allowed_services => ["database", "backup"],
            blocked_services => ["all"],
            encryption_required => true,
            monitoring_enabled => true
        },
        #{
            name => "api-gateway",
            description => "API gateway segment",
            network_zones => ["api-zone"],
            allowed_services => ["http", "https"],
            blocked_services => ["all"],
            encryption_required => true,
            monitoring_enabled => true
        }
    ],

    lists:foreach(fun(SegmentData) ->
        create_micro_segment(SegmentData)
    end, DefaultSegments).

generate_isolation_rules(Segments) ->
    %% Generate isolation rules for segments
    lists:foldl(fun(Segment, Acc) ->
        Rules = Segment#segment.firewall_rules ++ default_isolation_rules(),
        Acc ++ Rules
    end, [], Segments).

default_isolation_rules() ->
    %% Default isolation rules
    [
        #{action => deny, source => "any", destination => "any", protocol => "any"},
        #{action => allow, source => "trusted", destination => "trusted", protocol => "any"},
        #{action => allow, source => "app-zone", destination => "api-zone", protocol => "http"},
        #{action => allow, source => "app-zone", destination => "app-zone", protocol => "https"},
        #{action => allow, source => "api-zone", destination => "db-zone", protocol => "https"}
    ].

update_firewall_rules(SegmentId, Rules, State) ->
    #{firewalls := FirewallStore} = State,

    FirewallConfig = #{
        segment_id => SegmentId,
        rules => Rules,
        last_updated => erlang:system_time(second)
    },

    ets:insert(FirewallStore, {SegmentId, FirewallConfig}).

apply_isolation_policies(Segments, State) ->
    %% Apply isolation policies between segments
    lists:foreach(fun(SourceSegment) ->
        lists:foreach(fun(TargetSegment) ->
            IsolationPolicy = create_isolation_policy(SourceSegment, TargetSegment),
            apply_network_policy(IsolationPolicy)
        end, Segments)
    end, Segments).

%% Traffic monitoring implementation
do_monitor_traffic(TrafficData, Context, State) ->
    #{traffic_flows := TrafficStore} = State,

    %% Create traffic flow record
    Flow = #traffic_flow{
        id = generate_flow_id(),
        source = maps:get(source, TrafficData),
        target = maps:get(target, TrafficData),
        protocol = maps:get(protocol, TrafficData),
        ports = maps:get(ports, TrafficData, []),
        bytes = maps:get(bytes, TrafficData, 0),
        packets = maps:get(packets, TrafficData, 0),
        start_time = maps:get(start_time, TrafficData, erlang:system_time(second)),
        end_time = maps:get(end_time, TrafficData),
        allowed = maps:get(allowed, TrafficData, false),
        encrypted = maps:get(encrypted, TrafficData, false)
    },

    %% Store traffic flow
    ets:insert(TrawallStore, Flow),

    %% Check for anomalies
    case detect_traffic_anomaly(Flow, State) of
        true ->
            trigger_traffic_anomaly_alert(Flow, State);
        false ->
            ok
    end,

    %% Update monitoring metrics
    update_traffic_metrics(Flow, State).

detect_traffic_anomaly(Flow, State) ->
    %% Detect traffic anomalies based on patterns
    Baseline = get_traffic_baseline(Flow#traffic_flow.source, Flow#traffic_flow.target),
    CurrentFlow = Flow#traffic_flow.bytes,

    case CurrentFlow > Baseline * 2 of
        true ->
            true;
        false ->
            false
    end.

trigger_traffic_anomaly_alert(Flow, State) ->
    %% Trigger anomaly alert
    Alert = #{
        type => traffic_anomaly,
        severity => high,
        details => Flow,
        timestamp => erlang:system_time(second)
    },

    erlmcp_security_monitor:log_event(traffic_anomaly, Alert).

update_traffic_metrics(Flow, State) ->
    #{monitoring := Monitoring} = State,

    %% Update traffic metrics
    Metrics = Monitoring#{
        total_bytes => maps:get(total_bytes, Monitoring, 0) + Flow#traffic_flow.bytes,
        total_packets => maps:get(total_packets, Monitoring, 0) + Flow#traffic_flow.packets,
        flows_by_protocol => update_protocol_count(Flow, Monitoring)
    },

    State#state{monitoring = Metrics}.

%% Policy evaluation implementation
find_applicable_policies(Source, Target, PolicyStore) ->
    %% Find policies that apply to source and target
    ets:foldl(fun({_Key, Policy}, Acc) ->
        case lists:member(Source, Policy#network_policy.source_segments) andalso
             lists:member(Target, Policy#network_policy.target_segments) of
            true ->
                [Policy | Acc];
            false ->
                Acc
        end
    end, [], PolicyStore).

evaluate_policies([], _Protocol, _State) ->
    {deny, no_matching_policy};

evaluate_policies([Policy|Rest], Protocol, State) ->
    case evaluate_policy(Policy, Protocol, State) of
        {allow, _} -> {allow, Policy};
        {deny, _} -> {deny, Policy};
        {conditional, _, Conditions} -> {conditional, Policy, Conditions}
    end.

evaluate_policy(Policy, Protocol, State) ->
    %% Evaluate individual policy
    AllowedTraffic = Policy#network_policy.allowed_traffic,
    BlockedTraffic = Policy#network_policy.blocked_traffic,

    case check_traffic_rules(AllowedTraffic, Protocol, State) of
        {matched, Rule} -> {allow, Rule};
        not_matched ->
            case check_traffic_rules(BlockedTraffic, Protocol, State) of
                {matched, Rule} -> {deny, Rule};
                not_matched -> {conditional, Policy, generate_conditions(Policy, State)}
            end
    end.

check_traffic_rules([], _Protocol, _State) ->
    not_matched;

check_traffic_rules([Rule|Rest], Protocol, State) ->
    case match_traffic_rule(Rule, Protocol) of
        true -> {matched, Rule};
        false -> check_traffic_rules(Rest, Protocol, State)
    end.

match_traffic_rule(Rule, Protocol) ->
    %% Check if traffic matches rule
    RuleProtocol = maps:get(protocol, Rule, "any"),
    RuleProtocol == Protocol.

%% Helper functions
validate_segment_data(SegmentData) ->
    Required = [name],
    case check_required_fields(SegmentData, Required) of
        ok ->
            {ok, SegmentData};
        {error, missing_field} ->
            {error, {invalid_segment_data, missing_field}}
    end.

validate_policy_data(PolicyData) ->
    Required = [name, source_segments, target_segments],
    case check_required_fields(PolicyData, Required) of
        ok ->
            {ok, PolicyData};
        {error, missing_field} ->
            {error, {invalid_policy_data, missing_field}}
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

generate_segment_id() ->
    crypto:strong_rand_bytes(16).

generate_policy_id() ->
    crypto:strong_rand_bytes(16).

generate_flow_id() ->
    crypto:strong_rand_bytes(16).

find_network_segments(NetworkId, SegmentStore) ->
    %% Find segments in network
    ets:foldl(fun({_Key, Segment}, Acc) ->
        case lists:member(NetworkId, Segment#segment.network_zones) of
            true -> [Segment | Acc];
            false -> Acc
        end
    end, [], SegmentStore).

create_segment_firewall_rules(Segment, State) ->
    %% Create firewall rules for segment
    FirewallRules = Segment#segment.firewall_rules,
    update_firewall_rules(Segment#segment.id, FirewallRules, State).

apply_default_segment_policies(Segment, State) ->
    %% Apply default policies for segment
    DefaultPolicies = generate_default_policies(Segment),
    lists:foreach(fun(Policy) ->
        apply_network_policy(Policy)
    end, DefaultPolicies).

create_isolation_policy(SourceSegment, TargetSegment) ->
    %% Create isolation policy between segments
    #{
        name => <<"isolate_", SourceSegment/binary, "_from_", TargetSegment/binary>>,
        description => <<"Isolation policy for ", SourceSegment/binary>>,
        source_segments => [SourceSegment],
        target_segments => [TargetSegment],
        allowed_traffic => [
            #{protocol => "http", ports => [80]},
            #{protocol => "https", ports => [443]}
        ],
        blocked_traffic => [
            #{protocol => "all"}
        ],
        encryption => true,
        monitoring => true,
        priority => 1000
    }.

start_policy_monitoring(Policy, State) ->
    %% Start monitoring for policy
    MonitoringConfig = #{
        policy_id => Policy#network_policy.id,
        start_time => erlang:system_time(second),
        baseline => establish_traffic_baseline(Policy)
    },

    State#state{monitoring = maps:put(Policy#network_policy.id, MonitoringConfig, State#state.monitoring)}.

establish_traffic_baseline(Policy) ->
    %% Establish baseline traffic for policy
    #{
        avg_bytes => 1000000,
        avg_packets => 10000,
        peak_bytes => 5000000,
        peak_packets => 50000
    }.

analyze_by_protocol(TrafficFlows) ->
    %% Analyze traffic by protocol
    lists:foldl(fun(Flow, Acc) ->
        Protocol = Flow#traffic_flow.protocol,
        Acc#{Protocol => maps:get(Protocol, Acc, 0) + 1}
    end, #{}, TrafficFlows).

analyze_by_segment(TrafficFlows) ->
    %% Analyze traffic by segment
    lists:foldl(fun(Flow, Acc) ->
        Source = Flow#traffic_flow.source,
        Target = Flow#traffic_flow.target,
        Key = {Source, Target},
        Current = maps:get(Key, Acc, 0),
        Acc#{Key => Current + 1}
    end, #{}, TrafficFlows).

generate_security_metrics(Monitoring) ->
    %% Generate security metrics
    #{
        suspicious_flows => maps:get(suspicious_flows, Monitoring, 0),
        blocked_attempts => maps:get(blocked_attempts, Monitoring, 0),
        average_response_time => maps:get(average_response_time, Monitoring, 0),
        security_score => calculate_security_score(Monitoring)
    }.

calculate_security_score(Monitoring) ->
    %% Calculate overall security score
    SuspiciousFlows = maps:get(suspicious_flows, Monitoring, 0),
    BlockedAttempts = maps:get(blocked_attempts, Monitoring, 0),

    case SuspiciousFlows + BlockedAttempts of
        Total when Total > 100 ->
            0.3; %% Low security score
        Total when Total > 50 ->
            0.6; %% Medium security score
        _ ->
            0.9  %% High security score
    end.

get_traffic_baseline(Source, Target) ->
    %% Get baseline traffic for source-target pair
    %% This would typically come from historical data
    1000000.

load_network_config() ->
    %% Load network isolation configuration
    case file:read_file("config/network_isolation.json") of
        {ok, Data} ->
            jsone:decode(Data);
        _ ->
            %% Default configuration
            #{
                default_isolation => true,
                encryption_required => true,
                monitoring_enabled => true,
                retention_period => 2592000, %% 30 days
                anomaly_threshold => 2.0
            }
    end.