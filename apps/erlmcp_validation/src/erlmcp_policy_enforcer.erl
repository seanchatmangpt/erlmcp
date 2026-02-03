%% @doc Policy Enforcement Service
%% Enforces compliance policies and controls
%%
%% Responsibilities:
%% - Real-time policy enforcement
%% - Access control verification
%% - Data protection enforcement
%% - Audit policy compliance
%% - Policy violation detection
%% - Automated remediation
%% - Continuous monitoring
%%
%% @end
-module(erlmcp_policy_enforcer).

-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Policy Enforcement API
-export([
    add_policy/2,
    remove_policy/1,
    enforce_policy/3,
    check_policy_compliance/3,
    get_policy_status/1,
    get_violations/2,
    auto_remediate/3,
    create_policy_rule/2,
    update_policy_rule/3,
    get_audit_trail/2
]).

%% Types
-type policy_type() :: access_control | data_protection | encryption | audit_logging | incident_response | change_management.
-type policy_rule() :: binary().
-type enforcement_action() :: allow | deny | log | alert | remediate | quarantine.
-type violation_type() :: access_violation | data_violation | encryption_violation | audit_violation | policy_violation.
-type compliance_status() :: compliant | non_compliant | partial | requires_attention.

-record(policy_rule, {
    id :: binary(),
    policy_id :: binary(),
    name :: binary(),
    description :: binary(),
    condition :: binary(),
    action :: enforcement_action(),
    severity :: low | medium | high | critical,
    enabled :: boolean(),
    created_at :: erlang:timestamp(),
    modified_at :: erlang:timestamp(),
    last_triggered :: erlang:timestamp() | undefined,
    trigger_count :: integer()
}).

-record(policy, {
    id :: binary(),
    name :: binary(),
    type :: policy_type(),
    description :: binary(),
    rules :: [policy_rule()],
    scope :: binary(),
    compliance_standard :: binary(),
    enforcement_level :: mandatory | recommended,
    enabled :: boolean(),
    created_at :: erlang:timestamp(),
    modified_at :: erlang:timestamp(),
    last_enforced :: erlang:timestamp() | undefined
}).

-record(violation, {
    id :: binary(),
    policy_id :: binary(),
    rule_id :: binary(),
    type :: violation_type(),
    severity :: low | medium | high | critical,
    user_id :: binary(),
    resource :: binary(),
    action :: binary(),
    details :: map(),
    detected_at :: erlang:timestamp(),
    resolved_at :: erlang:timestamp() | undefined,
    status :: open | resolved | remediated | escalated,
    remediation_action :: binary() | undefined
}).

-record(state, {
    policies :: [policy()],
    active_rules :: [binary()],
    violations :: [violation()],
    audit_trail :: [map()],
    metrics :: map(),
    configuration :: map(),
    auto_remediate_config :: map()
}).

%% Constants
-define(SERVER, ?MODULE).
-define(RULE_EVALUATION_TIMEOUT, 5000).
-define(VIOLATION_CLEANUP_INTERVAL, 86400000).  % 24 hours
-define(AUDIT_MAX_SIZE, 100000).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the policy enforcer
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Stop the policy enforcer
-spec stop() -> ok.
stop() ->
    gen_server:call(?SERVER, stop).

%% @doc Add a new policy
-spec add_policy(binary(), map()) -> ok | {error, term()}.
add_policy(PolicyId, PolicyData) ->
    gen_server:call(?SERVER, {add_policy, PolicyId, PolicyData}).

%% @doc Remove a policy
-spec remove_policy(binary()) -> ok | {error, term()}.
remove_policy(PolicyId) ->
    gen_server:call(?SERVER, {remove_policy, PolicyId}).

%% @doc Enforce a policy
-spec enforce_policy(binary(), map(), binary()) -> ok | {error, term()}.
enforce_policy(PolicyId, Context, UserId) ->
    gen_server:call(?SERVER, {enforce_policy, PolicyId, Context, UserId}).

%% @doc Check policy compliance
-spec check_policy_compliance(binary(), map(), binary()) -> {compliance_status(), map()}.
check_policy_compliance(PolicyId, Context, UserId) ->
    gen_server:call(?SERVER, {check_policy_compliance, PolicyId, Context, UserId}).

%% @doc Get policy status
-spec get_policy_status(binary()) -> {ok, map()} | {error, term()}.
get_policy_status(PolicyId) ->
    gen_server:call(?SERVER, {get_policy_status, PolicyId}).

%% @doc Get violations for a policy
-spec get_violations(binary(), map()) -> {ok, [map()]} | {error, term()}.
get_violations(PolicyId, Filter) ->
    gen_server:call(?SERVER, {get_violations, PolicyId, Filter}).

%% @doc Auto-remediate violations
-spec auto_remediate(binary(), [binary()], binary()) -> ok | {error, term()}.
auto_remediate(PolicyId, ViolationIds, UserId) ->
    gen_server:call(?SERVER, {auto_remediate, PolicyId, ViolationIds, UserId}).

%% @doc Create a policy rule
-spec create_policy_rule(binary(), map()) -> ok | {error, term()}.
create_policy_rule(PolicyId, RuleData) ->
    gen_server:call(?SERVER, {create_policy_rule, PolicyId, RuleData}).

%% @doc Update a policy rule
-spec update_policy_rule(binary(), binary(), map()) -> ok | {error, term()}.
update_policy_rule(PolicyId, RuleId, RuleData) ->
    gen_server:call(?SERVER, {update_policy_rule, PolicyId, RuleId, RuleData}).

%% @doc Get audit trail
-spec get_audit_trail(binary(), map()) -> {ok, [map()]} | {error, term()}.
get_audit_trail(PolicyId, Options) ->
    gen_server:call(?SERVER, {get_audit_trail, Policy_id, Options}).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

init([]) ->
    %% Initialize with default policies
    DefaultPolicies = init_default_policies(),
    State = #state{
        policies = DefaultPolicies,
        active_rules = [],
        violations = [],
        audit_trail = [],
        metrics = init_metrics(),
        configuration = init_configuration(),
        auto_remediate_config = init_auto_remediate_config()
    },

    %% Start policy enforcement timer
    {ok, State, 0}.

handle_call({add_policy, PolicyId, PolicyData}, _From, State) ->
    Policy = validate_and_create_policy(PolicyId, PolicyData),
    UpdatedPolicies = [Policy | State#state.policies],

    %% Add policy rules to active rules
    UpdatedActiveRules = add_policy_rules(Policy, State#state.active_rules),

    {reply, ok, State#state{
        policies = UpdatedPolicies,
        active_rules = UpdatedActiveRules
    }};

handle_call({remove_policy, PolicyId}, _From, State) ->
    UpdatedPolicies = lists:filter(fun(P) -> P#policy.id =/= PolicyId end, State#state.policies),
    UpdatedActiveRules = lists:filter(fun(R) -> not is_rule_for_policy(R, PolicyId) end, State#state.active_rules),
    UpdatedViolations = lists:filter(fun(V) -> V#violation.policy_id =/= PolicyId end, State#state.violations),

    {reply, ok, State#state{
        policies = UpdatedPolicies,
        active_rules = UpdatedActiveRules,
        violations = UpdatedViolations
    }};

handle_call({enforce_policy, PolicyId, Context, UserId}, _From, State) ->
    case find_policy(PolicyId, State#state.policies) of
        {ok, Policy} ->
            if Policy#policy.enabled ->
                    Result = evaluate_policy_rules(Policy, Context, UserId),
                    audit_enforcement(PolicyId, Context, UserId, Result, State),

                    case Result of
                        {allowed, Details} ->
                            {reply, {ok, allowed}, State};
                        {denied, Reason} ->
                            {reply, {error, Reason}, State};
                        {violated, Violation} ->
                            {reply, {error, policy_violation}, State#state{violations = [Violation | State#state.violations]}}
                    end;
                true ->
                    {reply, {error, policy_disabled}, State}
            end;
        {error, not_found} ->
            {reply, {error, policy_not_found}, State}
    end};

handle_call({check_policy_compliance, PolicyId, Context, UserId}, _From, State) ->
    case find_policy(PolicyId, State#state.policies) of
        {ok, Policy} ->
            Compliance = evaluate_policy_compliance(Policy, Context, UserId, State),
            {reply, {ok, Compliance}, State};
        {error, not_found} ->
            {reply, {error, policy_not_found}, State}
    end;

handle_call({get_policy_status, PolicyId}, _From, State) ->
    case find_policy(PolicyId, State#state.policies) of
        {ok, Policy} ->
            Status = generate_policy_status(Policy, State),
            {reply, {ok, Status}, State};
        {error, not_found} ->
            {reply, {error, policy_not_found}, State}
    end;

handle_call({get_violations, PolicyId, Filter}, _From, State) ->
    FilteredViolations = filter_violations(State#state.violations, PolicyId, Filter),
    ViolationMaps = [violation_to_map(V) || V <- FilteredViolations],
    {reply, {ok, ViolationMaps}, State};

handle_call({auto_remediate, PolicyId, ViolationIds, UserId}, _From, State) ->
    case find_policy(PolicyId, State#state.policies) of
        {ok, Policy} ->
            Remediated = lists:foldl(fun(ViolationId, Acc) ->
                case auto_remediate_violation(ViolationId, Policy, UserId) of
                    {ok, RemediatedViolation} -> [RemediatedViolation | Acc];
                    _ -> Acc
                end
            end, [], ViolationIds),

            UpdatedViolations = lists:foldl(fun(RV, Acc) ->
                lists:keyreplace(RV#violation.id, #violation.id, Acc, RV)
            end, State#state.violations, Remediated),

            {reply, ok, State#state{violations = UpdatedViolations}};
        {error, not_found} ->
            {reply, {error, policy_not_found}, State}
    end;

handle_call({create_policy_rule, PolicyId, RuleData}, _From, State) ->
    case find_policy(PolicyId, State#state.policies) of
        {ok, Policy} ->
            RuleId = generate_rule_id(),
            Rule = create_policy_rule_record(RuleId, PolicyId, RuleData),
            UpdatedPolicy = Policy#policy{
                rules = [Rule | Policy#policy.rules],
                modified_at = erlang:timestamp()
            },
            UpdatedPolicies = lists:keyreplace(PolicyId, #policy.id, State#state.policies, UpdatedPolicy),
            UpdatedActiveRules = [RuleId | State#state.active_rules],

            {reply, ok, State#state{
                policies = UpdatedPolicies,
                active_rules = UpdatedActiveRules
            }};
        {error, not_found} ->
            {reply, {error, policy_not_found}, State}
    end;

handle_call({update_policy_rule, PolicyId, RuleId, RuleData}, _From, State) ->
    case find_policy(PolicyId, State#state.policies) of
        {ok, Policy} ->
            UpdatedRules = lists:map(fun(R) ->
                case R#policy_rule.id of
                    RuleId -> update_rule_record(R, RuleData);
                    _ -> R
                end
            end, Policy#policy.rules),

            UpdatedPolicy = Policy#policy{
                rules = UpdatedRules,
                modified_at = erlang:timestamp()
            },
            UpdatedPolicies = lists:keyreplace(PolicyId, #policy.id, State#state.policies, UpdatedPolicy),

            {reply, ok, State#state{policies = UpdatedPolicies}};
        {error, not_found} ->
            {reply, {error, policy_not_found}, State}
    end;

handle_call({get_audit_trail, PolicyId, Options}, _From, State) ->
    FilteredTrail = case PolicyId of
        all -> State#state.audit_trail;
        _ -> lists:filter(fun(T) -> maps:get(policy_id, T) =:= PolicyId end, State#state.audit_trail)
    end,

    case maps:get(limit, Options, undefined) of
        undefined -> {reply, {ok, FilteredTrail}, State};
        Limit -> {reply, {ok, lists:sublist(FilteredTrail, Limit)}, State}
    end;

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, State) ->
    %% Periodic policy enforcement and cleanup
    NewState = perform_periodic_checks(State),

    %% Clean up old audit trail entries
    CleanedState = cleanup_audit_trail(NewState),

    %% Update metrics
    UpdatedState = update_metrics(CleanedState),

    %% Schedule next check
    {noreply, UpdatedState, 60000};  % 1 minute

handle_info(policy_check, State) ->
    %% Perform policy compliance checks
    NewState = perform_policy_compliance_checks(State),
    {noreply, NewState};

handle_info(violation_cleanup, State) ->
    %% Clean up resolved violations
    CleanedState = cleanup_violations(State),
    {noreply, CleanedState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

init_default_policies() ->
    %% Initialize with default compliance policies
    [
        #{
            id => "access_control_policy",
            name => "Access Control Policy",
            type => access_control,
            description => "Enforces access control requirements for SOC2 and HIPAA",
            rules => [
                #{
                    id => "mfa_required",
                    policy_id => "access_control_policy",
                    name => "Multi-factor Authentication",
                    description => "Requires MFA for privileged access",
                    condition => "user.privileged = true",
                    action => allow,
                    severity => high,
                    enabled => true,
                    created_at => erlang:timestamp(),
                    modified_at => erlang:timestamp(),
                    last_triggered => undefined,
                    trigger_count => 0
                },
                #{
                    id => "least_privilege",
                    policy_id => "access_control_policy",
                    name => "Principle of Least Privilege",
                    description => "Users should have minimum required privileges",
                    condition => "user.privileges > required_privileges",
                    action => deny,
                    severity => critical,
                    enabled => true,
                    created_at => erlang:timestamp(),
                    modified_at => erlang:timestamp(),
                    last_triggered => undefined,
                    trigger_count => 0
                }
            ],
            scope => "system",
            compliance_standard => "SOC2",
            enforcement_level => mandatory,
            enabled => true,
            created_at => erlang:timestamp(),
            modified_at => erlang:timestamp(),
            last_enforced => undefined
        },
        #{
            id => "data_protection_policy",
            name => "Data Protection Policy",
            type => data_protection,
            description => "Enforces data protection requirements for GDPR and HIPAA",
            rules => [
                #{
                    id => "pii_detection",
                    policy_id => "data_protection_policy",
                    name => "PII Detection",
                    description => "Detects and protects personally identifiable information",
                    condition => "data.contains_pii = true",
                    action => encrypt,
                    severity => high,
                    enabled => true,
                    created_at => erlang:timestamp(),
                    modified_at => erlang:timestamp(),
                    last_triggered => undefined,
                    trigger_count => 0
                },
                #{
                    id => "data_retention",
                    policy_id => "data_protection_policy",
                    name => "Data Retention",
                    description => "Enforces data retention policies",
                    condition => "data.age > retention_period",
                    action => alert,
                    severity => medium,
                    enabled => true,
                    created_at => erlang:timestamp(),
                    modified_at => erlang:timestamp(),
                    last_triggered => undefined,
                    trigger_count => 0
                }
            ],
            scope => "data",
            compliance_standard => "GDPR",
            enforcement_level => mandatory,
            enabled => true,
            created_at => erlang:timestamp(),
            modified_at => erlang:timestamp(),
            last_enforced => undefined
        },
        #{
            id => "encryption_policy",
            name => "Encryption Policy",
            type => encryption,
            description => "Enforces encryption requirements for sensitive data",
            rules => [
                #{
                    id => "at_rest_encryption",
                    policy_id => "encryption_policy",
                    name => "At-Rest Encryption",
                    description => "Sensitive data must be encrypted at rest",
                    condition => "data.sensitive = true AND data.encrypted = false",
                    action => alert,
                    severity => critical,
                    enabled => true,
                    created_at => erlang:timestamp(),
                    modified_at => erlang:timestamp(),
                    last_triggered => undefined,
                    trigger_count => 0
                },
                #{
                    id => "in_transit_encryption",
                    policy_id => "encryption_policy",
                    name => "In-Transit Encryption",
                    description => "Data must be encrypted during transmission",
                    condition => "data.sensitive = true AND connection.encrypted = false",
                    action => deny,
                    severity => high,
                    enabled => true,
                    created_at => erlang:timestamp(),
                    modified_at => erlang:timestamp(),
                    last_triggered => undefined,
                    trigger_count => 0
                }
            ],
            scope => "data",
            compliance_standard => "HIPAA",
            enforcement_level => mandatory,
            enabled => true,
            created_at => erlang:timestamp(),
            modified_at => erlang:timestamp(),
            last_enforced => undefined
        },
        #{
            id => "audit_logging_policy",
            name => "Audit Logging Policy",
            type => audit_logging,
            description => "Enforces audit logging requirements",
            rules => [
                #{
                    id => "privileged_operations_log",
                    policy_id => "audit_logging_policy",
                    name => "Privileged Operations Log",
                    description => "All privileged operations must be logged",
                    condition => "operation.privileged = true AND logged = false",
                    action => alert,
                    severity => high,
                    enabled => true,
                    created_at => erlang:timestamp(),
                    modified_at => erlang:timestamp(),
                    last_triggered => undefined,
                    trigger_count => 0
                },
                #{
                    id => "audit_integrity",
                    policy_id => "audit_logging_policy",
                    name => "Audit Integrity",
                    description => "Audit logs must be protected from tampering",
                    condition => "audit_log.tampered = true",
                    action => alert,
                    severity => critical,
                    enabled => true,
                    created_at => erlang:timestamp(),
                    modified_at => erlang:timestamp(),
                    last_triggered => undefined,
                    trigger_count => 0
                }
            ],
            scope => "system",
            compliance_standard => "SOC2",
            enforcement_level => mandatory,
            enabled => true,
            created_at => erlang:timestamp(),
            modified_at => erlang:timestamp(),
            last_enforced => undefined
        },
        #{
            id => "change_management_policy",
            name => "Change Management Policy",
            type => change_management,
            description => "Enforces change management procedures",
            rules => [
                #{
                    id => "change_approval",
                    policy_id => "change_management_policy",
                    name => "Change Approval",
                    description => "All changes must be approved",
                    condition => "change.approved = false",
                    action => deny,
                    severity => high,
                    enabled => true,
                    created_at => erlang:timestamp(),
                    modified_at => erlang:timestamp(),
                    last_triggered => undefined,
                    trigger_count => 0
                },
                #{
                    id => "change_testing",
                    policy_id => "change_management_policy",
                    name => "Change Testing",
                    description => "Changes must be tested before deployment",
                    condition => "change.tested = false",
                    action => alert,
                    severity => medium,
                    enabled => true,
                    created_at => erlang:timestamp(),
                    modified_at => erlang:timestamp(),
                    last_triggered => undefined,
                    trigger_count => 0
                }
            ],
            scope => "system",
            compliance_standard => "SOC2",
            enforcement_level => mandatory,
            enabled => true,
            created_at => erlang:timestamp(),
            modified_at => erlang:timestamp(),
            last_enforced => undefined
        }
    ].

init_metrics() ->
    #{
        total_policies => 0,
        active_policies => 0,
        total_rules => 0,
        active_rules => 0,
        violations_detected => 0,
        violations_resolved => 0,
        average_response_time => 0,
        last_enforcement => undefined,
        compliance_rate => 0.0
    }.

init_configuration() ->
    #{
        evaluation_timeout => ?RULE_EVALUATION_TIMEOUT,
        batch_size => 100,
        concurrent_evaluations => 10,
        notification_channels => ["email", "slack", "sms"],
        escalation_thresholds => #{
            low => 5,
            medium => 10,
            high => 20,
            critical => 1
        }
    }.

init_auto_remediate_config() ->
    #{
        enabled => true,
        remediation_timeout => 30000,  % 30 seconds
        max_retries => 3,
        remediation_actions => #{
            "expired_credentials" => "force_password_reset",
            "unused_permissions" => "revoke_permissions",
            "unencrypted_data" => "encrypt_data",
            "missing_audit_log" => "enable_audit"
        }
    }.

validate_and_create_policy(PolicyId, PolicyData) ->
    %% Validate and create policy record
    RequiredFields = [name, type, description, compliance_standard, enforcement_level],
    lists:foreach(fun(Field) ->
        case maps:is_key(Field, PolicyData) of
            false -> error({missing_required_field, Field});
            true -> ok
        end
    end, RequiredFields),

    #policy{
        id = PolicyId,
        name = maps:get(name, PolicyData),
        type = maps:get(type, PolicyData),
        description = maps:get(description, PolicyData),
        rules = maps:get(rules, PolicyData, []),
        scope = maps:get(scope, PolicyData, "system"),
        compliance_standard = maps:get(compliance_standard, PolicyData),
        enforcement_level = maps:get(enforcement_level, PolicyData),
        enabled = maps:get(enabled, PolicyData, true),
        created_at = erlang:timestamp(),
        modified_at = erlang:timestamp(),
        last_enforced = undefined
    }.

create_policy_rule_record(RuleId, PolicyId, RuleData) ->
    %% Create policy rule record
    RequiredFields = [name, description, condition, action, severity],
    lists:foreach(fun(Field) ->
        case maps:is_key(Field, RuleData) of
            false -> error({missing_required_field, Field});
            true -> ok
        end
    end, RequiredFields),

    #policy_rule{
        id = RuleId,
        policy_id = PolicyId,
        name = maps:get(name, RuleData),
        description = maps:get(description, RuleData),
        condition = maps:get(condition, RuleData),
        action = maps:get(action, RuleData),
        severity = maps:get(severity, RuleData),
        enabled = maps:get(enabled, RuleData, true),
        created_at = erlang:timestamp(),
        modified_at = erlang:timestamp(),
        last_triggered = undefined,
        trigger_count => 0
    }.

update_rule_record(Rule, RuleData) ->
    %% Update policy rule record
    UpdatedRule = Rule#policy_rule{
        name = maps:get(name, RuleData, Rule#policy_rule.name),
        description = maps:get(description, RuleData, Rule#policy_rule.description),
        condition = maps:get(condition, RuleData, Rule#policy_rule.condition),
        action = maps:get(action, RuleData, Rule#policy_rule.action),
        severity = maps:get(severity, RuleData, Rule#policy_rule.severity),
        enabled = maps:get(enabled, RuleData, Rule#policy_rule.enabled),
        modified_at = erlang:timestamp()
    },
    UpdatedRule.

find_policy(PolicyId, Policies) ->
    case lists:keyfind(PolicyId, #policy.id, Policies) of
        false -> {error, not_found};
        Policy -> {ok, Policy}
    end.

evaluate_policy_rules(Policy, Context, UserId) ->
    %% Evaluate all rules in the policy
    case Policy#policy.enabled of
        true ->
            Results = lists:map(fun(Rule) ->
                evaluate_rule(Rule, Context, UserId)
            end, Policy#policy.rules),

            process_rule_results(Results, Policy);
        false ->
            {denied, policy_disabled}
    end.

evaluate_rule(Rule, Context, UserId) ->
    %% Evaluate a single rule
    case Rule#policy_rule.enabled of
        true ->
            case evaluate_condition(Rule#policy_rule.condition, Context) of
                true ->
                    Action = Rule#policy_rule.action,
                    case Action of
                        allow -> {allowed, #{}};
                        deny -> {denied, rule_denied};
                        log -> {logged, #{}};
                        alert -> {alert, #{}};
                        remediate -> {remediate, #{}};
                        quarantine -> {quarantine, #{}}
                    end;
                false -> {passed, #{}}
            end;
        false ->
            {passed, #{}}
    end.

evaluate_condition(Condition, Context) ->
    %% Evaluate rule condition against context
    %% This is a simplified version - actual implementation would use a proper rule engine
    case Condition of
        "user.privileged = true" ->
            maps:get(<<"privileged">>, Context, false);
        "user.privileges > required_privileges" ->
            UserPrivs = maps:get(<<"privileges">>, Context, []),
            Required = maps:get(<<"required_privileges">>, Context, []),
            length(UserPrivs) > length(Required);
        "data.contains_pii = true" ->
            maps:get(<<"contains_pii">>, Context, false);
        "data.age > retention_period" ->
            DataAge = maps:get(<<"age">>, Context, 0),
            RetentionPeriod = maps:get(<<"retention_period">>, Context, 0),
            DataAge > RetentionPeriod;
        "data.sensitive = true AND data.encrypted = false" ->
            maps:get(<<"sensitive">>, Context, false) andalso
            not maps:get(<<"encrypted">>, Context, false);
        "data.sensitive = true AND connection.encrypted = false" ->
            maps:get(<<"sensitive">>, Context, false) andalso
            not maps:get(<<"encrypted">>, Context, false);
        "operation.privileged = true AND logged = false" ->
            maps:get(<<"privileged">>, Context, false) andalso
            not maps:get(<<"logged">>, Context, false);
        "audit_log.tampered = true" ->
            maps:get(<<"tampered">>, Context, false);
        "change.approved = false" ->
            not maps:get(<<"approved">>, Context, false);
        "change.tested = false" ->
            not maps:get(<<"tested">>, Context, false);
        _ -> false
    end.

process_rule_results(Results, _Policy) ->
    %% Process rule evaluation results
    Allowed = lists:filter(fun({allowed, _}) -> true; (_) -> false end, Results),
    Denied = lists:filter(fun({denied, _}) -> true; (_) -> false end, Results),
    Alerts = lists:filter(fun({alert, _}) -> true; (_) -> false end, Results),
    Remediated = lists:filter(fun({remediate, _}) -> true; (_) -> false end, Results),

    case length(Denied) > 0 of
        true -> {denied, rule_denied};
        false when length(Alerts) > 0 or length(Remediated) > 0 ->
            {violated, create_violation(Alerts, Remediated)};
        true ->
            {allowed, #{}}
    end.

create_violation(Alerts, Remediated) ->
    %% Create violation record
    #violation{
        id = generate_violation_id(),
        policy_id = "temp_policy",
        rule_id = "temp_rule",
        type = policy_violation,
        severity = high,
        user_id = "temp_user",
        resource = "temp_resource",
        action = "policy_violation",
        details = #{alerts => Alerts, remediated => Remediated},
        detected_at = erlang:timestamp(),
        status = open,
        remediation_action = undefined
    }.

generate_violation_id() ->
    integer_to_binary(erlang:system_time(nanosecond)).

generate_rule_id() ->
    integer_to_binary(erlang:system_time(nanosecond)).

add_policy_rules(Policy, ActiveRules) ->
    %% Add policy rules to active rules list
    lists:foldl(fun(Rule, Acc) ->
        case Rule#policy_rule.enabled of
            true -> [Rule#policy_rule.id | Acc];
            false -> Acc
        end
    end, ActiveRules, Policy#policy.rules).

is_rule_for_policy(RuleId, PolicyId) ->
    %% Check if rule belongs to policy
    true.  % Simplified implementation

evaluate_policy_compliance(Policy, Context, UserId, State) ->
    %% Evaluate overall policy compliance
    RuleResults = lists:map(fun(Rule) ->
        evaluate_rule(Rule, Context, UserId)
    end, Policy#policy.rules),

    Passed = length([R || {passed, _} <- RuleResults]),
    Total = length(Policy#policy.rules),
    ComplianceRate = Passed / Total,

    case ComplianceRate of
        R when R >= 0.9 -> {compliant, #{rate => ComplianceRate}};
        R when R >= 0.7 -> {partial, #{rate => ComplianceRate}};
        _ -> {non_compliant, #{rate => ComplianceRate}}
    end.

audit_enforcement(PolicyId, Context, UserId, Result, State) ->
    %% Log enforcement action
    AuditEntry = #{
        id => generate_audit_id(),
        policy_id => PolicyId,
        user_id => UserId,
        action => "policy_enforcement",
        result => Result,
        context => Context,
        timestamp => erlang:timestamp()
    },

    UpdatedTrail = [AuditEntry | State#state.audit_trail],
    TruncatedTrail = lists:sublist(UpdatedTrail, ?AUDIT_MAX_SIZE),

    State#state{audit_trail = TruncatedTrail}.

generate_audit_id() ->
    integer_to_binary(erlang:system_time(nanosecond)).

generate_policy_status(Policy, State) ->
    %% Generate policy status report
    TotalRules = length(Policy#policy.rules),
    EnabledRules = length([R || R <- Policy#policy.rules, R#policy_rule.enabled]),
    ActiveViolations = length([V || V <- State#state.violations, V#violation.policy_id =:= Policy#policy.id]),

    #{
        id => Policy#policy.id,
        name => Policy#policy.name,
        type => Policy#policy.type,
        status => case Policy#policy.enabled of
            true -> active;
            false => disabled
        end,
        total_rules => TotalRules,
        enabled_rules => EnabledRules,
        disabled_rules => TotalRules - EnabledRules,
        active_violations => ActiveViolations,
        compliance_standard => Policy#policy.compliance_standard,
        enforcement_level => Policy#policy.enforcement_level,
        last_enforced => Policy#policy.last_enforced,
        created_at => Policy#policy.created_at,
        modified_at => Policy#policy.modified_at
    }.

filter_violations(Violations, PolicyId, Filter) ->
    %% Filter violations based on criteria
    Filtered = case PolicyId of
        all -> Violations;
        _ -> lists:filter(fun(V) -> V#violation.policy_id =:= PolicyId end, Violations)
    end,

    case maps:get(severity, Filter, undefined) of
        undefined -> Filtered;
        Severity -> lists:filter(fun(V) -> V#violation.severity =:= Severity end, Filtered)
    end.

violation_to_map(Violation) ->
    %% Convert violation record to map
    #{
        id => Violation#violation.id,
        policy_id => Violation#violation.policy_id,
        rule_id => Violation#violation.rule_id,
        type => Violation#violation.type,
        severity => Violation#violation.severity,
        user_id => Violation#violation.user_id,
        resource => Violation#violation.resource,
        action => Violation#violation.action,
        details => Violation#violation.details,
        detected_at => Violation#violation.detected_at,
        resolved_at => Violation#violation.resolved_at,
        status => Violation#violation.status,
        remediation_action => Violation#violation.remediation_action
    }.

auto_remediate_violation(ViolationId, Policy, UserId) ->
    %% Auto-remediate a violation
    case find_violation(ViolationId, Policy#policy.id, Policy#policy.rules) of
        {ok, Violation} ->
            case get_remediation_action(Violation) of
                {ok, Action} ->
                    RemediatedViolation = Violation#violation{
                        status => remediated,
                        remediation_action => Action,
                        resolved_at => erlang:timestamp()
                    },
                    {ok, RemediatedViolation};
                {error, no_action} ->
                    {error, no_remediation_action}
            end;
        {error, not_found} ->
            {error, not_found}
    end.

find_violation(ViolationId, PolicyId, Rules) ->
    %% Find violation in policy rules
    case lists:filter(fun(R) -> R#policy_rule.id =:= ViolationId end, Rules) of
        [Rule] -> {ok, #violation{
            id = Rule#policy_rule.id,
            policy_id = PolicyId,
            rule_id = Rule#policy_rule.id,
            type = policy_violation,
            severity = Rule#policy_rule.severity,
            user_id = "temp_user",
            resource = "temp_resource",
            action = "remediate",
            details = #{},
            detected_at = erlang:timestamp(),
            status = open,
            remediation_action = undefined
        }};
        [] -> {error, not_found}
    end.

get_remediation_action(Violation) ->
    %% Get remediation action for violation
    Config = State#state.auto_remediate_config,
    case maps:get(remediation_actions, Config, #{}) of
        Actions ->
            case maps:get(Violation#violation.action, Actions, undefined) of
                Action -> {ok, Action};
                undefined -> {error, no_action}
            end;
        _ -> {error, no_action}
    end.

perform_periodic_checks(State) ->
    %% Perform periodic policy checks
    lists:foreach(fun(Policy) ->
        if Policy#policy.enabled ->
                perform_policy_check(Policy);
            true -> ok
        end
    end, State#state.policies),
    State.

perform_policy_check(Policy) ->
    %% Perform check for a specific policy
    %% This would involve checking system state against policy requirements
    ok.

perform_policy_compliance_checks(State) ->
    %% Perform comprehensive policy compliance checks
    lists:foreach(fun(Policy) ->
        if Policy#policy.enabled ->
                check_policy_comprehensive(Policy);
            true -> ok
        end
    end, State#state.policies),
    State.

check_policy_comprehensive(Policy) ->
    %% Perform comprehensive policy check
    %% This would involve deep analysis of system compliance
    ok.

cleanup_audit_trail(State) ->
    %% Clean up old audit trail entries
    CurrentTime = erlang:timestamp(),
    RetentionPeriod = ?AUDIT_MAX_SIZE,

    CleanedTrail = lists:filter(fun(Entry) ->
        %% Keep recent entries
        true  % Simplified - keep all entries for now
    end, State#state.audit_trail),

    State#state{audit_trail = CleanedTrail}.

cleanup_violations(State) ->
    %% Clean up resolved violations
    CurrentTime = erlang:timestamp(),
    RetentionDays = 30,  % Keep resolved violations for 30 days

    CleanedViolations = lists:filter(fun(V) ->
        case V#violation.status of
            resolved -> time_diff(V#violation.resolved_at, CurrentTime) < RetentionDays * 24 * 60 * 60 * 1000;
            _ -> true  % Keep active and other violations
        end
    end, State#state.violations),

    State#state{violations = CleanedViolations}.

time_diff(Timestamp1, Timestamp2) ->
    %% Calculate time difference in milliseconds
    erlang:time_diff(Timestamp2, Timestamp1).

update_metrics(State) ->
    %% Update monitoring metrics
    TotalPolicies = length(State#state.policies),
    ActivePolicies = length([P || P <- State#state.policies, P#policy.enabled]),
    TotalRules = lists:foldl(fun(P, Acc) -> Acc + length(P#policy.rules) end, 0, State#state.policies),
    ActiveRules = length(State#state.active_rules),
    TotalViolations = length(State#state.violations),
    ResolvedViolations = length([V || V <- State#state.violations, V#violation.status =:= resolved]),

    State#state.metrics#{
        total_policies => TotalPolicies,
        active_policies => ActivePolicies,
        total_rules => TotalRules,
        active_rules => ActiveRules,
        violations_detected => TotalViolations,
        violations_resolved => ResolvedViolations,
        last_enforcement => erlang:timestamp(),
        compliance_rate => calculate_compliance_rate(State)
    }.

calculate_compliance_rate(State) ->
    %% Calculate overall compliance rate
    case State#state.metrics#total_rules of
        0 -> 0.0;
        Total ->
            Resolved = State#state.metrics#violations_resolved,
            1.0 - (Resolved / Total)
    end.

%%====================================================================
%% End of File
%%====================================================================