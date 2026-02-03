-module(erlmcp_micro_segmentation).
-behaviour(gen_server).

%% API
<think>
content</arg_key>
<arg_value>-module(erlmcp_micro_segmentation).
-behaviour(gen_server).

%% API
-export([start_link/0, create_segment/2, update_segment/3, delete_segment/2]).
-export([add_resource_to_segment/3, remove_resource_from_segment/3]).
-export([check_segment_access/3, get_segment_policy/2]).
-export([enforce_isolation/2, monitor_segment_boundaries/1]).
-export([apply_security_posture/3, validate_segment_compliance/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Records
-record(segment, {
    id :: binary(),
    name :: binary(),
    description :: binary(),
    resources :: list(),
    policies :: list(),
    isolation_mode :: strict | moderate | permissive,
    network_rules :: list(),
    application_rules :: list(),
    data_rules :: list(),
    monitoring_enabled :: boolean(),
    compliance_status :: compliant | non_compliant | unknown,
    created_at :: integer(),
    updated_at :: integer()
}).

-record(resource, {
    id :: binary(),
    type :: service | database | application | endpoint,
    name :: binary(),
    segment_id :: binary(),
    security_profile :: binary(),
    network_zones :: list(),
    trust_level :: integer(),
    monitoring_enabled :: boolean(),
    metadata :: map()
}).

-record.segment_policy, {
    id :: binary(),
    segment_id :: binary(),
    rule_type :: inbound | outbound | internal,
    source :: binary(),
    destination :: binary(),
    protocol :: binary(),
    ports :: list(),
    action :: allow | deny | inspect,
    conditions :: list(),
    priority :: integer()
}).

-record(state, {
    segments :: map(),
    resources :: map(),
    policies :: map(),
    compliance_log :: list(),
    config :: map()
}).

-define(TIMEOUT, 30000).
-define(DEFAULT_ISOLATION_MODE, moderate).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create_segment(SegmentData) ->
    gen_server:call(?MODULE, {create_segment, SegmentData}, ?TIMEOUT).

update_segment(SegmentId, UpdateData) ->
    gen_server:call(?MODULE, {update_segment, SegmentId, UpdateData}, ?TIMEOUT).

delete_segment(SegmentId, Reason) ->
    gen_server:call(?MODULE, {delete_segment, SegmentId, Reason}, ?TIMEOUT).

add_resource_to_segment(ResourceId, SegmentId, Policy) ->
    gen_server:call(?MODULE, {add_resource_to_segment, ResourceId, SegmentId, Policy}, ?TIMEOUT).

remove_resource_from_segment(ResourceId, SegmentId) ->
    gen_server:call(?MODULE, {remove_resource_from_segment, ResourceId, SegmentId}, ?TIMEOUT).

check_segment_access(SourceId, DestinationId, Action) ->
    gen_server:call(?MODULE, {check_segment_access, SourceId, DestinationId, Action}, ?TIMEOUT).

get_segment_policy(SegmentId) ->
    gen_server:call(?MODULE, {get_segment_policy, SegmentId}, ?TIMEOUT).

enforce_isolation(SegmentId, IsolationLevel) ->
    gen_server:call(?MODULE, {enforce_isolation, SegmentId, IsolationLevel}, ?TIMEOUT).

monitor_segment_boundaries(SegmentId) ->
    gen_server:call(?MODULE, {monitor_segment_boundaries, SegmentId}, ?TIMEOUT).

apply_security_posture(SegmentId, Posture, Config) ->
    gen_server:call(?MODULE, {apply_security_posture, SegmentId, Posture, Config}, ?TIMEOUT).

validate_segment_compliance(SegmentId) ->
    gen_server:call(?MODULE, {validate_segment_compliance, SegmentId}, ?TIMEOUT).

init([]) ->
    State = #state{
        segments = load_default_segments(),
        resources = #{},
        policies = #{},
        compliance_log = [],
        config = load_config()
    },
    erlmcp_micro_segmentation:initialize(),
    {ok, State}.

handle_call({create_segment, SegmentData}, _From, State) ->
    case validate_segment_data(SegmentData) of
        {ok, ValidatedData} ->
            SegmentId = generate_segment_id(),
            Segment = #segment{
                id = SegmentId,
                name = maps:get(name, ValidatedData),
                description = maps:get(description, ValidatedData, <<"">>),
                resources = [],
                policies = [],
                isolation_mode = maps:get(isolation_mode, ValidatedData, ?DEFAULT_ISOLATION_MODE),
                network_rules = maps:get(network_rules, ValidatedData, []),
                application_rules = maps:get(application_rules, ValidatedData, []),
                data_rules = maps:get(data_rules, ValidatedData, []),
                monitoring_enabled = maps:get(monitoring_enabled, ValidatedData, true),
                compliance_status = unknown,
                created_at = timestamp(),
                updated_at = timestamp()
            },
            NewState = State#state{
                segments = maps:put(SegmentId, Segment, State#state.segments)
            },
            {reply, {ok, SegmentId}, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({update_segment, SegmentId, UpdateData}, _From, State) ->
    case maps:find(SegmentId, State#state.segments) of
        {ok, Segment} ->
            UpdatedSegment = Segment#segment{
                name = maps:get(name, UpdateData, Segment#segment.name),
                description = maps:get(description, UpdateData, Segment#segment.description),
                isolation_mode = maps:get(isolation_mode, UpdateData, Segment#segment.isolation_mode),
                network_rules = maps:get(network_rules, UpdateData, Segment#segment.network_rules),
                application_rules = maps:get(application_rules, UpdateData, Segment#segment.application_rules),
                data_rules = maps:get(data_rules, UpdateData, Segment#segment.data_rules),
                monitoring_enabled = maps:get(monitoring_enabled, UpdateData, Segment#segment.monitoring_enabled),
                updated_at = timestamp()
            },
            NewState = State#state{
                segments = maps:put(SegmentId, UpdatedSegment, State#state.segments)
            },
            {reply, {ok, SegmentId}, NewState};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({delete_segment, SegmentId, Reason}, _From, State) ->
    case maps:find(SegmentId, State#state.segments) of
        {ok, Segment} ->
            %% Remove all resources from segment
            UpdatedResources = remove_segment_resources(SegmentId, State#state.resources),
            %% Remove segment policies
            UpdatedPolicies = maps:filter(fun(_, Policy) ->
                Policy#segment_policy.segment_id /= SegmentId
            end, State#state.policies),
            NewState = State#state{
                segments = maps:remove(SegmentId, State#state.segments),
                resources = UpdatedResources,
                policies = UpdatedPolicies
            },
            {reply, {ok, deleted}, NewState};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({add_resource_to_segment, ResourceId, SegmentId, Policy}, _From, State) ->
    case validate_resource_placement(ResourceId, SegmentId, State) of
        {ok, ResourceInfo} ->
            %% Add resource to segment
            UpdatedResources = add_resource_to_segment_1(ResourceId, SegmentId, State#state.resources),
            %% Create or update policies
            UpdatedPolicies = apply_resource_policies(ResourceId, SegmentId, Policy, State#state.policies),
            NewState = State#state{
                resources = UpdatedResources,
                policies = UpdatedPolicies
            },
            {reply, {ok, added}, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({remove_resource_from_segment, ResourceId, SegmentId}, _From, State) ->
    case maps:find(ResourceId, State#state.resources) of
        {ok, Resource} ->
            case Resource#resource.segment_id == SegmentId of
                true ->
                    UpdatedResources = maps:put(ResourceId, Resource#resource{segment_id = undefined}, State#state.resources),
                    %% Remove resource-specific policies
                    UpdatedPolicies = remove_resource_policies(ResourceId, SegmentId, State#state.policies),
                    NewState = State#state{
                        resources = UpdatedResources,
                        policies = UpdatedPolicies
                    },
                    {reply, {ok, removed}, NewState};
                false ->
                    {reply, {error, resource_not_in_segment}, State}
            end;
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({check_segment_access, SourceId, DestinationId, Action}, _From, State) ->
    case evaluate_segment_traffic(SourceId, DestinationId, Action, State) of
        {allow, Policies} ->
            {reply, {allow, Policies}, State};
        {deny, Reason} ->
            {reply, {deny, Reason}, State}
    end;

handle_call({get_segment_policy, SegmentId}, _From, State) ->
    case maps:find(SegmentId, State#state.segments) of
        {ok, Segment} ->
            Policies = maps:values(lists:filtermap(fun(Policy) ->
                case Policy#segment_policy.segment_id == SegmentId of
                    true ->
                        {true, Policy};
                    false ->
                        false
                end
            end, State#state.policies)),
            {reply, {ok, Policies}, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({enforce_isolation, SegmentId, IsolationLevel}, _From, State) ->
    case maps:find(SegmentId, State#state.segments) of
        {ok, Segment} ->
            %% Update isolation mode
            UpdatedSegment = Segment#segment{
                isolation_mode = IsolationLevel,
                updated_at = timestamp()
            },
            %% Apply isolation rules
            IsolationPolicies = generate_isolation_policies(SegmentId, IsolationLevel),
            NewState = State#state{
                segments = maps:put(SegmentId, UpdatedSegment, State#state.segments),
                policies = maps:merge(IsolationPolicies, State#state.policies)
            },
            {reply, {ok, isolation_enforced}, NewState};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({monitor_segment_boundaries, SegmentId}, _From, State) ->
    case maps:find(SegmentId, State#state.segments) of
        {ok, Segment} ->
            %% Start monitoring segment boundaries
            MonitoringConfig = #{
                segment_id => SegmentId,
                monitoring_interval => 30000, %% 30 seconds
                alert_threshold => 10, %% alerts per minute
                enabled => true
            },
            start_monitoring(SegmentId, MonitoringConfig),
            {reply, {ok, monitoring_started}, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({apply_security_posture, SegmentId, Posture, Config}, _From, State) ->
    case maps:find(SegmentId, State#state.segments) of
        {ok, Segment} ->
            case validate_security_posture(Posture, Config) of
                {ok, ValidatedConfig} ->
                    %% Apply security posture to segment
                    UpdatedSegment = apply_posture_to_segment(Segment, Posture, ValidatedConfig),
                    NewState = State#state{
                        segments = maps:put(SegmentId, UpdatedSegment, State#state.segments)
                    },
                    {reply, {ok, posture_applied}, NewState};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end;
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({validate_segment_compliance, SegmentId}, _From, State) ->
    case maps:find(SegmentId, State#state.segments) of
        {ok, Segment} ->
            ComplianceResult = validate_compliance(SegmentId, State),
            UpdatedSegment = Segment#segment{
                compliance_status = ComplianceResult#compliance.status,
                updated_at = timestamp()
            },
            NewState = State#state{
                segments = maps:put(SegmentId, UpdatedSegment, State#state.segments),
                compliance_log = [ComplianceResult|State#state.compliance_log]
            },
            {reply, {ok, ComplianceResult}, NewState};
        error ->
            {reply, {error, not_found}, State}
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

%% Internal functions
initialize() ->
    %% Initialize network isolation components
    %% Configure security posture templates
    %% Start boundary monitoring
    ok.

load_config() ->
    #{
        segment_timeout => 60000,
        max_resources_per_segment => 100,
        compliance_check_interval => 3600000,
        isolation_enforcement => true,
        network_monitoring_enabled => true,
        application_monitoring_enabled => true
    }.

load_default_segments() ->
    %% Load default segments for Fortune 500
    #{
        finance => #segment{
            id => <<"finance">>,
            name => <<"Finance Segment">>,
            description => <<"Financial systems and sensitive data">>,
            isolation_mode => strict,
            monitoring_enabled => true,
            created_at => timestamp(),
            updated_at => timestamp()
        },
        hr => #segment{
            id => <<"hr">>,
            name => <<"HR Segment">>,
            description => <<">Human resources and employee data">>,
            isolation_mode => strict,
            monitoring_enabled => true,
            created_at => timestamp(),
            updated_at => timestamp()
        },
        dev => #segment{
            id => <<"dev">>,
            name => <<"Development Segment">>,
            description => <<"Development and testing environments">>,
            isolation_mode => moderate,
            monitoring_enabled => true,
            created_at => timestamp(),
            updated_at => timestamp()
        },
        prod => #segment{
            id => <<"prod">>,
            name => <<"Production Segment">>,
            description => <<"Production environment">>,
            isolation_mode => strict,
            monitoring_enabled => true,
            created_at => timestamp(),
            updated_at => timestamp()
        }
    }.

validate_segment_data(Data) ->
    Required = [name],
    case check_required_fields(Data, Required) of
        ok ->
            {ok, Data};
        {error, missing_field} ->
            {error, {invalid_segment_data, missing_field}}
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

validate_resource_placement(ResourceId, SegmentId, State) ->
    case maps:find(ResourceId, State#state.resources) of
        {ok, Resource} ->
            case Resource#resource.trust_level >= get_minimum_trust_level(SegmentId, State) of
                true ->
                    {ok, Resource};
                false ->
                    {error, "insufficient_trust_level"}
            end;
        error ->
            {error, "resource_not_found"}
    end.

add_resource_to_segment_1(ResourceId, SegmentId, Resources) ->
    case maps:find(ResourceId, Resources) of
        {ok, Resource} ->
            UpdatedResource = Resource#resource{
                segment_id = SegmentId,
                network_zones = [SegmentId|Resource#resource.network_zones]
            },
            maps:put(ResourceId, UpdatedResource, Resources);
        error ->
            %% Create new resource record
            NewResource = #resource{
                id = ResourceId,
                type = undefined, %% Should be provided
                name = ResourceId,
                segment_id = SegmentId,
                security_profile = <<"default">>,
                network_zones = [SegmentId],
                trust_level = 1,
                monitoring_enabled = true,
                metadata = #{}
            },
            maps:put(ResourceId, NewResource, Resources)
    end.

apply_resource_policies(ResourceId, SegmentId, Policy, Policies) ->
    %% Create segment policies for the resource
    SegmentPolicy = #segment_policy{
        id = generate_policy_id(),
        segment_id = SegmentId,
        rule_type = inbound,
        source = ResourceId,
        destination = <<"*">>,
        protocol = <<"*">>,
        ports = [<<"*">>],
        action = allow,
        conditions = Policy,
        priority = 100
    },
    maps:put(SegmentPolicy#segment_policy.id, SegmentPolicy, Policies).

remove_resource_policies(ResourceId, SegmentId, Policies) ->
    %% Remove policies specific to this resource
    maps:filter(fun(_, Policy) ->
        not (Policy#segment_policy.segment_id == SegmentId andalso
             Policy#segment_policy.source == ResourceId)
    end, Policies).

remove_segment_resources(SegmentId, Resources) ->
    %% Remove segment_id from all resources
    maps:map(fun(_, Resource) ->
        case Resource#resource.segment_id == SegmentId of
            true ->
                Resource#resource{segment_id = undefined};
            false ->
                Resource
        end
    end, Resources).

evaluate_segment_traffic(SourceId, DestinationId, Action, State) ->
    %% Check if source and destination are in compatible segments
    SourceSegment = get_resource_segment(SourceId, State),
    DestSegment = get_resource_segment(DestinationId, State),

    case SourceSegment == DestSegment of
        true ->
            %% Same segment - allow internal traffic
            {allow, [internal_traffic]};
        false ->
            %% Cross-segment traffic - check policies
            case check_cross_segment_policies(SourceSegment, DestSegment, Action, State) of
                {allow, Policies} ->
                    {allow, Policies};
                {deny, Reason} ->
                    {deny, Reason}
            end
    end.

get_resource_segment(ResourceId, State) ->
    case maps:find(ResourceId, State#state.resources) of
        {ok, Resource} ->
            Resource#resource.segment_id;
        error ->
            undefined
    end.

check_cross_segment_policies(SourceSegment, DestSegment, Action, State) ->
    %% Check if there are policies allowing traffic between segments
    CrossSegmentPolicies = lists:filtermap(fun(Policy) ->
        case Policy#segment_policy.rule_type of
            inbound ->
                case Policy#segment_policy.segment_id == DestSegment andalso
                     Policy#segment_policy.source == SourceSegment of
                    true ->
                        {true, Policy};
                    false ->
                        false
                end;
            _ ->
                false
        end
    end, maps:values(State#state.policies)),

    case CrossSegmentPolicies of
        [Policy|_] ->
            {allow, Policy};
        [] ->
            {deny, "no_cross_segment_policy"}
    end.

generate_isolation_policies(SegmentId, IsolationLevel) ->
    %% Generate isolation policies based on level
    case IsolationLevel of
        strict ->
            %% Strict isolation - deny all external traffic
            #{
                isolation_policy_1 => #segment_policy{
                    id => generate_policy_id(),
                    segment_id = SegmentId,
                    rule_type = inbound,
                    source = <<"*">>,
                    destination = <<"*">>,
                    protocol = <<"*">>,
                    ports = [<<"*">>],
                    action = deny,
                    conditions = [],
                    priority = 1000
                },
                isolation_policy_2 => #segment_policy{
                    id => generate_policy_id(),
                    segment_id = SegmentId,
                    rule_type = outbound,
                    source = SegmentId,
                    destination = <<"*">>,
                    protocol = <<"*">>,
                    ports = [<<"*">>],
                    action = deny,
                    conditions = [],
                    priority = 1000
                }
            };
        moderate ->
            %% Moderate isolation - allow specific protocols
            #{
                isolation_policy_1 => #segment_policy{
                    id => generate_policy_id(),
                    segment_id = SegmentId,
                    rule_type = inbound,
                    source = <<"*">>,
                    destination = <<"*">>,
                    protocol = <<"tcp">>,
                    ports = [<<"80">>, <<"443">>],
                    action = allow,
                    conditions = [],
                    priority = 100
                }
            };
        permissive ->
            %% Permissive - minimal restrictions
            #{}
    end.

validate_compliance(SegmentId, State) ->
    %% Validate segment against compliance requirements
    Segment = maps:get(SegmentId, State#state.segments),
    ComplianceCheckpoints = [
        {segment_isolation, validate_isolation_compliance(Segment)},
        {resource_monitoring, validate_resource_monitoring(Segment, State)},
        {policy_coverage, validate_policy_coverage(SegmentId, State)},
        {network_controls, validate_network_controls(Segment)}
    ],

    Passed = lists:foldl(fun({_, Check}, Acc) ->
        case Check of
            {compliant, _} ->
                Acc + 1;
            {non_compliant, _} ->
                Acc
        end
    end, 0, ComplianceCheckpoints),

    Status = case Passed == length(ComplianceCheckpoints) of
        true -> compliant;
        false -> non_compliant
    end,

    #{
        segment_id => SegmentId,
        status => Status,
        checkpoints => ComplianceCheckpoints,
        score Passed / length(ComplianceCheckpoints),
        timestamp => timestamp()
    }.

validate_isolation_compliance(Segment) ->
    case Segment#segment.isolation_mode of
        strict ->
            case Segment#segment.network_rules of
                [] ->
                    {non_compliant, "missing_network_rules"};
                _ ->
                    {compliant, "strict_isolation_enforced"}
            end;
        moderate ->
            {compliant, "moderate_isolation_enforced"};
        permissive ->
            {compliant, "permissive_isolation"}
    end.

validate_resource_monitoring(Segment, State) ->
    SegmentResources = lists:filter(fun(R) ->
        R#resource.segment_id == Segment#segment.id
    end, maps:values(State#state.resources)),

    MonitoredResources = lists:filter(fun(R) ->
        R#resource.monitoring_enabled
    end, SegmentResources),

    case length(MonitoredResources) / length(SegmentResources) >= 0.9 of
        true ->
            {compliant, "monitoring_coverage"};
        false ->
            {non_compliant, "insufficient_monitoring"}
    end.

validate_policy_coverage(SegmentId, State) ->
    SegmentPolicies = lists:filter(fun(P) ->
        P#segment_policy.segment_id == SegmentId
    end, maps:values(State#state.policies)),

    case SegmentPolicies of
        [] ->
            {non_compliant, "no_segment_policies"};
        _ ->
            {compliant, "policy_coverage"}
    end.

validate_network_controls(Segment) ->
    case Segment#segment.network_rules of
        [] ->
            {non_compliant, "missing_network_controls"};
        _ ->
            {compliant, "network_controls_present"}
    end.

apply_posture_to_segment(Segment, Posture, Config) ->
    %% Apply security posture to segment
    UpdatedSegment = Segment#segment{
        isolation_mode = maps:get(isolation_mode, Config, Segment#segment.isolation_mode),
        monitoring_enabled = maps:get(monitoring_enabled, Config, Segment#segment.monitoring_enabled),
        updated_at = timestamp()
    },
    UpdatedSegment.

validate_security_posture(Posture, Config) ->
    case Posture of
        hardening ->
            validate_hardening_posture(Config);
        monitoring ->
            validate_monitoring_posture(Config);
        encryption ->
            validate_encryption_posture(Config);
        _ ->
            {error, "invalid_posture"}
    end.

validate_hardening_posture(Config) ->
    Required = [patch_level, firewall_config, intrusion_detection],
    case check_required_fields(Config, Required) of
        ok ->
            {ok, Config};
        {error, missing_field} ->
            {error, {invalid_posture, missing_field}}
    end.

validate_monitoring_posture(Config) ->
    Required = [monitoring_interval, alert_threshold, log_retention],
    case check_required_fields(Config, Required) of
        ok ->
            {ok, Config};
        {error, missing_field} ->
            {error, {invalid_posture, missing_field}}
    end.

validate_encryption_posture(Config) ->
    Required = [encryption_algorithms, key_rotation, certificate_management],
    case check_required_fields(Config, Required) of
        ok ->
            {ok, Config};
        {error, missing_field} ->
            {error, {invalid_posture, missing_field}}
    end.

start_monitoring(SegmentId, Config) ->
    %% Start boundary monitoring for segment
    erlmcp_security_monitor:start_segment_monitoring(SegmentId, Config).

get_minimum_trust_level(SegmentId, State) ->
    case maps:find(SegmentId, State#state.segments) of
        {ok, Segment} ->
            case Segment#segment.isolation_mode of
                strict -> 3;
                moderate -> 2;
                permissive -> 1
            end;
        error ->
            0
    end.

generate_segment_id() ->
    crypto:strong_rand_bytes(16).

generate_policy_id() ->
    crypto:strong_rand_bytes(16).

timestamp() ->
    erlang:system_time(millisecond).