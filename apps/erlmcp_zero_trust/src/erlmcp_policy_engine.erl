-module(erlmcp_policy_engine).
-behaviour(gen_server).

%% API
-export([start_link/0, evaluate/3, create_policy/2, update_policy/3]).
-export([delete_policy/2, get_policies/1, get_policy/1]).
-export([enforce_least_privilege/1, validate_principle_of_least_privilege/1]).
-export([simulate_policy_impact/2, audit_policy_compliance/1]).
-export([configure_policy_set/2, get_policy_set/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Records
-record.policy, {
    id :: binary(),
    name :: binary(),
    description :: binary(),
    type :: access | data | network | application,
    effect :: allow | deny,
    conditions :: list(),
    actions :: list(),
    resources :: list(),
    subjects :: list(),
    priority :: integer(),
    audit_logging :: boolean(),
    expiration :: integer() | undefined,
    created_at :: integer(),
    updated_at :: integer()
}.

-record.policy_set, {
    id :: binary(),
    name :: binary(),
    description :: binary(),
    policies :: list(),
    inheritance :: boolean(),
    override :: boolean(),
    compliance :: boolean(),
    created_at :: integer(),
    updated_at :: integer()
}.

-record.policy_evaluation, {
    policy_id :: binary(),
    result :: allow | deny,
    reason :: binary(),
    conditions_met :: list(),
    risk_score :: float(),
    confidence :: float(),
    timestamp :: integer()
}.

-record.state, {
    policies :: map(),
    policy_sets :: map(),
    evaluation_log :: list(),
    config :: map()
}).

-define(TIMEOUT, 30000).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

evaluate(PolicyId, Context, Resource) ->
    gen_server:call(?MODULE, {evaluate, PolicyId, Context, Resource}, ?TIMEOUT).

create_policy(PolicyData) ->
    gen_server:call(?MODULE, {create_policy, PolicyData}, ?TIMEOUT).

update_policy(PolicyId, UpdateData) ->
    gen_server:call(?MODULE, {update_policy, PolicyId, UpdateData}, ?TIMEOUT).

delete_policy(PolicyId, Reason) ->
    gen_server:call(?MODULE, {delete_policy, PolicyId, Reason}, ?TIMEOUT).

get_policies(Filter) ->
    gen_server:call(?MODULE, {get_policies, Filter}, ?TIMEOUT).

get_policy(PolicyId) ->
    gen_server:call(?MODULE, {get_policy, PolicyId}, ?TIMEOUT).

enforce_least_privilege(PolicyId) ->
    gen_server:call(?MODULE, {enforce_least_privilege, PolicyId}, ?TIMEOUT).

validate_principle_of_least_privilege(PolicyId) ->
    gen_server:call(?MODULE, {validate_principle_of_least_privilege, PolicyId}, ?TIMEOUT).

simulate_policy_impact(PolicyId, Scenario) ->
    gen_server:call(?MODULE, {simulate_policy_impact, PolicyId, Scenario}, ?TIMEOUT).

audit_policy_compliance(PolicyId) ->
    gen_server:call(?MODULE, {audit_policy_compliance, PolicyId}, ?TIMEOUT).

configure_policy_set(SetId, SetData) ->
    gen_server:call(?MODULE, {configure_policy_set, SetId, SetData}, ?TIMEOUT).

get_policy_set(SetId) ->
    gen_server:call(?MODULE, {get_policy_set, SetId}, ?TIMEOUT).

init([]) ->
    State = #state{
        policies = load_default_policies(),
        policy_sets = load_default_policy_sets(),
        evaluation_log = [],
        config = load_config()
    },
    erlmcp_policy_engine:initialize(),
    {ok, State}.

handle_call({evaluate, PolicyId, Context, Resource}, _From, State) ->
    case evaluate_policy(PolicyId, Context, Resource, State) of
        {Result, Evaluation} ->
            %% Log evaluation result
            NewLog = [Evaluation|State#state.evaluation_log],
            LimitedLog = lists:sublist(NewLog, 1000), %% Keep last 1000 evaluations
            {reply, {ok, Result, Evaluation}, State#state{evaluation_log = LimitedLog}};
        {error, not_found} ->
            {reply, {error, not_found}, State}
    end;

handle_call({create_policy, PolicyData}, _From, State) ->
    case validate_policy_data(PolicyData) of
        {ok, ValidatedData} ->
            PolicyId = generate_policy_id(),
            Policy = #policy{
                id = PolicyId,
                name = maps:get(name, ValidatedData),
                description = maps:get(description, ValidatedData, <<"">>),
                type = maps:get(type, ValidatedData),
                effect = maps:get(effect, ValidatedData, allow),
                conditions = maps:get(conditions, ValidatedData, []),
                actions = maps:get(actions, ValidatedData),
                resources = maps:get(resources, ValidatedData),
                subjects = maps:get(subjects, ValidatedData),
                priority = maps:get(priority, ValidatedData, 0),
                audit_logging = maps:get(audit_logging, ValidatedData, true),
                expiration = maps:get(expiration, ValidatedData, undefined),
                created_at = timestamp(),
                updated_at = timestamp()
            },
            NewState = State#state{
                policies = maps:put(PolicyId, Policy, State#state.policies)
            },
            {reply, {ok, PolicyId}, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({update_policy, PolicyId, UpdateData}, _From, State) ->
    case maps:find(PolicyId, State#state.policies) of
        {ok, Policy} ->
            UpdatedPolicy = Policy#policy{
                name = maps:get(name, UpdateData, Policy#policy.name),
                description = maps:get(description, UpdateData, Policy#policy.description),
                type = maps:get(type, UpdateData, Policy#policy.type),
                effect = maps:get(effect, UpdateData, Policy#policy.effect),
                conditions = maps:get(conditions, UpdateData, Policy#policy.conditions),
                actions = maps:get(actions, UpdateData, Policy#policy.actions),
                resources = maps:get(resources, UpdateData, Policy#policy.resources),
                subjects = maps:get(subjects, UpdateData, Policy#policy.subjects),
                priority = maps:get(priority, UpdateData, Policy#policy.priority),
                audit_logging = maps:get(audit_logging, UpdateData, Policy#policy.audit_logging),
                expiration = maps:get(expiration, UpdateData, Policy#policy.expiration),
                updated_at = timestamp()
            },
            NewState = State#state{
                policies = maps:put(PolicyId, UpdatedPolicy, State#state.policies)
            },
            {reply, {ok, PolicyId}, NewState};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({delete_policy, PolicyId, Reason}, _From, State) ->
    case maps:find(PolicyId, State#state.policies) of
        {ok, Policy} ->
            %% Log policy deletion
            erlmcp_security_monitor:log_event(policy_deleted, #{
                policy_id => PolicyId,
                reason => Reason,
                type => Policy#policy.type
            }),
            NewState = State#state{
                policies = maps:remove(PolicyId, State#state.policies)
            },
            {reply, {ok, deleted}, NewState};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({get_policies, Filter}, _From, State) ->
    FilteredPolicies = apply_filter(State#state.policies, Filter),
    {reply, {ok, maps:values(FilteredPolicies)}, State};

handle_call({get_policy, PolicyId}, _From, State) ->
    case maps:find(PolicyId, State#state.policies) of
        {ok, Policy} ->
            {reply, {ok, Policy}, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({enforce_least_privilege, PolicyId}, _From, State) ->
    case maps:find(PolicyId, State#state.policies) of
        {ok, Policy} ->
            case apply_least_privilege_principle(Policy) of
                {ok, OptimizedPolicy} ->
                    NewState = State#state{
                        policies = maps:put(PolicyId, OptimizedPolicy, State#state.policies)
                    },
                    {reply, {ok, optimized}, NewState};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end;
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({validate_principle_of_least_privilege, PolicyId}, _From, State) ->
    case maps:find(PolicyId, State#state.policies) of
        {ok, Policy} ->
            case validate_least_privilege(Policy) of
                {compliant, Analysis} ->
                    {reply, {compliant, Analysis}, State};
                {non_compliant, Violations} ->
                    {reply, {non_compliant, Violations}, State}
            end;
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({simulate_policy_impact, PolicyId, Scenario}, _From, State) ->
    case simulate_impact(PolicyId, Scenario, State) of
        {ok, SimulationResult} ->
            {reply, {ok, SimulationResult}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({audit_policy_compliance, PolicyId}, _From, State) ->
    case audit_compliance(PolicyId, State) of
        {compliant, Report} ->
            {reply, {compliant, Report}, State};
        {non_compliant, Report} ->
            {reply, {non_compliant, Report}, State}
    end;

handle_call({configure_policy_set, SetId, SetData}, _From, State) ->
    case validate_policy_set_data(SetData) of
        {ok, ValidatedData} ->
            PolicySet = #policy_set{
                id = SetId,
                name = maps:get(name, ValidatedData),
                description = maps:get(description, ValidatedData, <<"">>),
                policies = maps:get(policies, ValidatedData, []),
                inheritance = maps:get(inheritance, ValidatedData, true),
                override = maps:get(override, ValidatedData, false),
                compliance = maps:get(compliance, ValidatedData, true),
                created_at = timestamp(),
                updated_at = timestamp()
            },
            NewState = State#state{
                policy_sets = maps:put(SetId, PolicySet, State#state.policy_sets)
            },
            {reply, {ok, SetId}, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({get_policy_set, SetId}, _From, State) ->
    case maps:find(SetId, State#state.policy_sets) of
        {ok, PolicySet} ->
            {reply, {ok, PolicySet}, State};
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
    %% Initialize policy evaluation engine
    %% Load compliance frameworks
    %% Configure policy optimization
    ok.

load_config() ->
    #{
        evaluation_timeout => 10000,
        max_evaluations_per_second => 1000,
        policy_optimization_enabled => true,
        compliance_check_interval => 3600000,
        audit_retention_days => 365
    }.

load_default_policies() ->
    %% Load Fortune 500 security policies
    #{
        deny_all => #policy{
            id => <<"deny_all">>,
            name => <<"Deny All Access">>,
            description => <<"Default deny-all policy">>,
            type => access,
            effect => deny,
            conditions => [],
            actions => [<<"*">>],
            resources = [<<"*">>],
            subjects = [<<"*">>],
            priority => 9999,
            audit_logging => true,
            created_at => timestamp(),
            updated_at => timestamp()
        },
        allow_admin => #policy{
            id => <<"allow_admin">>,
            name => <<"Admin Access">>,
            description => <<"Allow administrator access">>,
            type => access,
            effect => allow,
            conditions = [{role, admin}],
            actions = [<<"*">>],
            resources = [<<"*">>],
            subjects = [{role, admin}],
            priority => 1000,
            audit_logging => true,
            created_at => timestamp(),
            updated_at => timestamp()
        }
    }.

load_default_policy_sets() ->
    %% Load default policy sets
    #{
        financial => #policy_set{
            id => <<"financial">>,
            name => <<"Financial Services Policies">>,
            description => <<"Policies for financial systems">>,
            policies = [<<"financial_data_access">>, <<"transaction_monitoring">>],
            inheritance = true,
            override = false,
            compliance = true,
            created_at => timestamp(),
            updated_at => timestamp()
        }
    }.

evaluate_policy(PolicyId, Context, Resource, State) ->
    case maps:find(PolicyId, State#state.policies) of
        {ok, Policy} ->
            CheckConditions = evaluate_conditions(Policy#policy.conditions, Context),
            CheckActions = lists:member(Resource#resource.action, Policy#policy.actions),
            CheckResources = match_resource(Resource#resource.id, Policy#policy.resources),
            CheckSubjects = match_subject(Context#subject.id, Policy#policy.subjects),

            case {CheckConditions, CheckActions, CheckResources, CheckSubjects} of
                {true, true, true, true} ->
                    Result = Policy#policy.effect,
                    Evaluation = #policy_evaluation{
                        policy_id = PolicyId,
                        result = Result,
                        reason => conditions_met,
                        conditions_met = Policy#policy.conditions,
                        risk_score = calculate_risk_score(Context),
                        confidence = 1.0,
                        timestamp = timestamp()
                    },
                    {Result, Evaluation};
                {_, _, _, _} ->
                    Evaluation = #policy_evaluation{
                        policy_id = PolicyId,
                        result = deny,
                        reason => conditions_not_met,
                        conditions_met = [],
                        risk_score = 0.0,
                        confidence = 0.0,
                        timestamp = timestamp()
                    },
                    {deny, Evaluation}
            end;
        error ->
            {error, not_found}
    end.

evaluate_conditions(Conditions, Context) ->
    %% Evaluate all conditions
    Results = lists:map(fun(Condition) ->
        evaluate_condition(Condition, Context)
    end, Conditions),

    %% Combine results with AND logic
    lists:foldl(fun(Result, Acc) ->
        Acc and Result
    end, true, Results).

evaluate_condition({time, Range}, _Context) ->
    %% Time-based condition
    CurrentTime = erlang:system_time(second),
    case Range of
        {start, End} ->
            CurrentTime >= Start andalso CurrentTime =< End;
        {before, End} ->
            CurrentTime =< End;
        {after, Start} ->
            CurrentTime >= Start;
        _ ->
            true
    end;

evaluate_condition({location, AllowedLocations}, _Context) ->
    %% Location-based condition
    CurrentLocation = maps:get(location, Context, undefined),
    case CurrentLocation of
        undefined ->
            false;
        _ ->
            lists:any(fun(L) -> location_match(CurrentLocation, L) end, AllowedLocations)
    end;

evaluate_condition({device, DeviceRequirements}, _Context) ->
    %% Device-based condition
    DeviceInfo = maps:get(device, Context, #{}),
    case check_device_compliance(DeviceInfo, DeviceRequirements) of
        {compliant, _} ->
            true;
        {non_compliant, _} ->
            false
    end;

evaluate_condition({risk, RiskThreshold}, _Context) ->
    %% Risk-based condition
    CurrentRisk = maps:get(risk_score, Context, 0.0),
    CurrentRisk =< RiskThreshold;

evaluate_condition({ip_reputation, AllowedReputations}, _Context) ->
    %% IP reputation condition
    IP = maps:get(ip, Context, undefined),
    case IP of
        undefined ->
            false;
        _ ->
            Reputation = erlmcp_threat_intel:get_ip_reputation(IP),
            lists:member(Reputation, AllowedReputations)
    end;

evaluate_condition({_, _}, _Context) ->
    %% Unknown condition - deny
    false.

match_resource(ResourceId, ResourcePatterns) ->
    %% Check if resource matches any pattern
    lists:any(fun(Pattern) ->
        match_resource_pattern(ResourceId, Pattern)
    end, ResourcePatterns).

match_resource_pattern(ResourceId, Pattern) ->
    %% Simple pattern matching - can be enhanced with regex
    case Pattern of
        <<$/>> ++ _ ->
            binary:match(ResourceId, Pattern) /= nomatch;
        _ ->
            ResourceId == Pattern
    end.

match_subject(SubjectId, SubjectPatterns) ->
    %% Check if subject matches any pattern
    lists:any(fun(Pattern) ->
        case Pattern of
            {role, Role} ->
                erlmcp_identity_provider:has_role(SubjectId, Role);
            {group, Group} ->
                erlmcp_identity_provider:is_member_of_group(SubjectId, Group);
            _ ->
                SubjectId == Pattern
        end
    end, SubjectPatterns).

location_match(Location1, Location2) ->
    %% Check if locations match
    %% Simplified implementation
    Location1 == Location2.

check_device_compliance(DeviceInfo, Requirements) ->
    %% Check if device meets requirements
    case maps:get(os, Requirements, undefined) of
        undefined ->
            {compliant, no_os_requirement};
        RequiredOS ->
            case maps:get(os, DeviceInfo, undefined) of
                RequiredOS ->
                    {compliant, os_match};
                _ ->
                    {non_compliant, os_mismatch}
            end
    end.

calculate_risk_score(Context) ->
    %% Calculate risk score based on context
    RiskFactors = [
        {ip_reputation, maps:get(ip_reputation, Context, 0.0)},
        {user_behavior, maps:get(user_behavior, Context, 0.0)},
        {time_anomaly, maps:get(time_anomaly, Context, 0.0)},
        {location_anomaly, maps:get(location_anomaly, Context, 0.0)}
    ],

    calculate_weighted_risk(RiskFactors).

calculate_weighted_risk(Factors) ->
    Weights = [
        {ip_reputation, 0.4},
        {user_behavior, 0.3},
        {time_anomaly, 0.1},
        {location_anomaly, 0.2}
    ],

    lists:foldl(fun({Factor, Value}, Acc) ->
        case proplists:get_value(Factor, Weights) of
            undefined ->
                Acc;
            Weight ->
                Acc + Value * Weight
        end
    end, 0.0, Factors).

validate_policy_data(Data) ->
    Required = [name, type, effect, actions, resources, subjects],
    case check_required_fields(Data, Required) of
        ok ->
            {ok, Data};
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

apply_least_privilege_principle(Policy) ->
    %% Apply least privilege principle to optimize policy
    case Policy#policy.effect of
        allow ->
            %% Minimize permissions in allow policies
            MinimizedActions = minimize_actions(Policy#policy.actions),
            MinimizedResources = minimize_resources(Policy#policy.resources),
            case {MinimizedActions, MinimizedResources} of
                {Policy#policy.actions, Policy#policy.resources} ->
                    {error, "already_minimized"};
                _ ->
                    {ok, Policy#policy{
                        actions = MinimizedActions,
                        resources = MinimizedResources,
                        updated_at = timestamp()
                    }}
            end;
        deny ->
            {ok, Policy}
    end.

minimize_actions(Actions) ->
    %% Remove redundant or overly broad actions
    case Actions of
        [<<"*">>] ->
            %% Replace catch-all with specific actions
            [<<"read">>, <<"write">>, <<"execute">>];
        _ ->
            Actions
    end.

minimize_resources(Resources) ->
    %% Remove redundant or overly broad resources
    case Resources of
        [<<"*">>] ->
            %% Replace catch-all with specific resources
            [<<"/data">>, <<"/config">>, <<"/logs">>];
        _ ->
            Resources
    end.

validate_least_privilege(Policy) ->
    %% Validate if policy follows least privilege principle
    case Policy#policy.effect of
        allow ->
            case lists:member(<<"*">>, Policy#policy.actions) of
                true ->
                    %% Check if broad actions are justified
                    case has_justification(Policy) of
                        true ->
                            {compliant, <<"broad_actions_justified">>};
                        false ->
                            {non_compliant, [<<"unnecessary_broad_actions">>]}
                    end;
                false ->
                    {compliant, <<"specific_actions">>}
            end;
        deny ->
            {compliant, <<"deny_policy">>}
    end.

has_justification(Policy) ->
    %% Check if policy has valid justification for broad permissions
    case lists:member(admin_role, Policy#policy.conditions) of
        true ->
            true;
        false ->
            case Policy#policy.type of
                emergency =>
                    true;
                _ ->
                    false
            end
    end.

simulate_impact(PolicyId, Scenario, State) ->
    %% Simulate policy impact on security posture
    case maps:find(PolicyId, State#state.policies) of
        {ok, Policy} ->
            ImpactAnalysis = #{
                security_impact => calculate_security_impact(Policy, Scenario),
                compliance_impact => calculate_compliance_impact(Policy, Scenario),
                business_impact => calculate_business_impact(Policy, Scenario),
                risk_assessment => assess_risk_impact(Policy, Scenario)
            },
            {ok, ImpactAnalysis};
        error ->
            {error, "policy_not_found"}
    end.

calculate_security_impact(Policy, Scenario) ->
    %% Calculate impact on security posture
    case Policy#policy.effect of
        allow ->
            Impact = case Policy#policy.actions of
                [<<"*">>] ->
                    high;
                _ ->
                    medium
            end;
        deny ->
            Impact = low
    end,
    Impact.

calculate_compliance_impact(Policy, Scenario) ->
    %% Calculate impact on compliance
    case Policy#policy.audit_logging of
        true ->
            compliant;
        false ->
            non_compliant
    end.

calculate_business_impact(Policy, Scenario) ->
    %% Calculate business impact
    case Scenario of
        {emergency, _} ->
            high;
        {maintenance, _} ->
            medium;
        _ ->
            low
    end.

assess_risk_impact(Policy, Scenario) ->
    %% Assess risk impact
    RiskScore = calculate_risk_score(Scenario),
    case RiskScore > 0.7 of
        true ->
            high_risk;
        false ->
            low_risk
    end.

audit_compliance(PolicyId, State) ->
    %% Audit policy compliance with regulations
    case maps:find(PolicyId, State#state.policies) of
        {ok, Policy} ->
            ComplianceCheckpoints = [
                {regulatory_compliance, check_regulatory_compliance(Policy)},
                {internal_standards, check_internal_standards(Policy)},
                {security_best_practices, check_security_best_practices(Policy)},
                {audit_trail, check_audit_trail(Policy)}
            ],

            Passed = lists:foldl(fun({_, Check}, Acc) ->
                case Check of
                    {compliant, _} ->
                        Acc + 1;
                    {non_compliant, _} ->
                        Acc
                end
            end, 0, ComplianceCheckpoints),

            Report = #{
                policy_id => PolicyId,
                compliance_score => Passed / length(ComplianceCheckpoints),
                checkpoints => ComplianceCheckpoints,
                audit_date => timestamp()
            },

            case Passed == length(ComplianceCheckpoints) of
                true ->
                    {compliant, Report};
                false ->
                    {non_compliant, Report}
            end;
        error ->
            {error, "policy_not_found"}
    end.

check_regulatory_compliance(Policy) ->
    %% Check compliance with regulations (GDPR, HIPAA, etc.)
    case Policy#policy.type of
        data ->
            case lists:member(<<"pii">>, Policy#policy.resources) of
                true ->
                    {compliant, pii_handling};
                false ->
                    {compliant, no_pii_handling}
            end;
        _ ->
            {compliant, non_data_policy}
    end.

check_internal_standards(Policy) ->
    %% Check against internal security standards
    case Policy#policy.audit_logging of
        true ->
            {compliant, audit_logging_enabled};
        false ->
            {non_compliant, audit_logging_disabled}
    end.

check_security_best_practices(Policy) ->
    %% Check against security best practices
    case Policy#policy.effect of
        allow ->
            case Policy#policy.priority < 5000 of
                true ->
                    {compliant, appropriate_priority};
                false ->
                    {non_compliant, high_priority_allow}
            end;
        deny ->
            {compliant, deny_policy}
    end.

check_audit_trail(Policy) ->
    %% Check audit trail requirements
    case Policy#policy.audit_logging of
        true ->
            {compliant, audit_enabled};
        false ->
            {non_compliant, audit_disabled}
    end.

apply_filter(Policies, Filter) ->
    %% Apply filter to policies
    maps:filter(fun(_, Policy) ->
        matches_filter(Policy, Filter)
    end, Policies).

matches_filter(Policy, Filter) ->
    case Filter of
        #{type := Type} ->
            Policy#policy.type == Type;
        #{effect := Effect} ->
            Policy#policy.effect == Effect;
        #{priority := MinPriority} ->
            Policy#policy.priority >= MinPriority;
        _ ->
            true
    end.

validate_policy_set_data(Data) ->
    Required = [name, policies],
    case check_required_fields(Data, Required) of
        ok ->
            {ok, Data};
        {error, missing_field} ->
            {error, {invalid_policy_set_data, missing_field}}
    end.

generate_policy_id() ->
    crypto:strong_rand_bytes(16).

timestamp() ->
    erlang:system_time(millisecond).