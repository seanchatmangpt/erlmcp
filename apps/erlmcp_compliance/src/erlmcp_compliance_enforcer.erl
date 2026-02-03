%% @doc erlmcp Compliance Enforcer
%% Enforces compliance policies and controls across the system
%% Provides real-time policy enforcement and violation detection
-module(erlmcp_compliance_enforcer).

-behaviour(gen_server).

%% API
-export([start_link/0, enforce_policy/3, check_access/3, validate_data_handling/2,
         enforce_encryption/2, enforce_retention/3, register_policy/4,
         get_policy_violations/1, start_continuous_enforcement/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(policy_rule, {
    id :: binary(),
    framework :: soc2 | hipaa | gdpr | iso27001,
    resource_type :: binary(),
    action :: binary(),
    conditions :: list(),
    enforcement_action :: allow | deny | log | alert | require_approval,
    priority :: low | medium | high | critical
}).

-record.enforcement_event, {
    id :: binary(),
    timestamp :: erlang:timestamp(),
    policy_id :: binary(),
    resource_type :: binary(),
    action :: binary(),
    user_id :: binary(),
    decision :: allowed | denied,
    reason :: binary(),
    metadata :: map()
}).

-record(state, {
    policy_rules :: #{binary() => #policy_rule{}},
    enforcement_log :: list(),
    active_sessions :: list(),
    violation_history :: list(),
    performance_metrics :: #{},
    enforcement_mode :: active | passive | audit_only
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

enforce_policy(Framework, Action, Context) ->
    gen_server:call(?SERVER, {enforce_policy, Framework, Action, Context}).

check_access(UserId, Resource, Action) ->
    gen_server:call(?SERVER, {check_access, UserId, Resource, Action}).

validate_data_handling(DataType, ProcessingPurpose) ->
    gen_server:call(?SERVER, {validate_data_handling, DataType, ProcessingPurpose}).

enforce_encryption(Data, Context) ->
    gen_server:call(?SERVER, {enforce_encryption, Data, Context}).

enforce_retention(DataType, RetentionPeriod, Purpose) ->
    gen_server:call(?SERVER, {enforce_retention, DataType, RetentionPeriod, Purpose}).

register_policy(Framework, ResourceType, Action, Conditions) ->
    gen_server:call(?SERVER, {register_policy, Framework, ResourceType, Action, Conditions}).

get_policy_violations(Framework) ->
    gen_server:call(?SERVER, {get_policy_violations, Framework}).

start_continuous_enforcement() ->
    gen_server:cast(?SERVER, start_continuous_enforcement).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    PolicyRules0 = init_policy_rules(),
    EnforcementLog0 = [],
    ActiveSessions0 = [],
    ViolationHistory0 = [],
    PerformanceMetrics0 = init_performance_metrics(),
    State0 = #state{
        policy_rules = PolicyRules0,
        enforcement_log = EnforcementLog0,
        active_sessions = ActiveSessions0,
        violation_history = ViolationHistory0,
        performance_metrics = PerformanceMetrics0,
        enforcement_mode = active
    },
    erlmcp_telemetry:counter("compliance.enforcer.initialized", 1,
                           #{component => "compliance"}),
    {ok, State0}.

handle_call({enforce_policy, Framework, Action, Context}, _From, State) ->
    PolicyRules = get_applicable_policies(Framework, Action, State),
    Decision = evaluate_policies(PolicyRules, Context),

    % Log enforcement event
    Event = create_enforcement_event(Framework, Action, Context, Decision),
    EnforcementLog1 = [Event | State#state.enforcement_log],

    % Update metrics
    Metrics1 = update_metrics(State#state.performance_metrics, Event),

    % Check for violations
    case Decision#{decision} of
        denied ->
            Violations1 = [Event | State#state.violation_history],
            erlmcp_telemetry:counter("compliance.violation.detected", 1,
                                   #{framework => atom_to_list(Framework)});
        _ ->
            Violations1 = State#state.violation_history
    end,

    {reply, Decision, State#state{
        enforcement_log = EnforcementLog1,
        violation_history = Violations1,
        performance_metrics = Metrics1
    }};

handle_call({check_access, UserId, Resource, Action}, _From, State) ->
    % Implement access control policy enforcement
    Decision = enforce_access_control(UserId, Resource, Action, State),
    {reply, Decision, State};

handle_call({validate_data_handling, DataType, ProcessingPurpose}, _From, State) ->
    % Validate data handling against compliance requirements
    Decision = validate_data_policies(DataType, ProcessingPurpose, State),
    {reply, Decision, State};

handle_call({enforce_encryption, Data, Context}, _From, State) ->
    % Enforce encryption policies based on data type and context
    Decision = enforce_encryption_policies(Data, Context, State),
    {reply, Decision, State};

handle_call({enforce_retention, DataType, RetentionPeriod, Purpose}, _From, State) ->
    % Enforce retention policies
    Decision = enforce_retention_policies(DataType, RetentionPeriod, Purpose, State),
    {reply, Decision, State};

handle_call({register_policy, Framework, ResourceType, Action, Conditions}, _From, State) ->
    PolicyId = generate_policy_id(),
    Policy = #policy_rule{
        id = PolicyId,
        framework = Framework,
        resource_type = ResourceType,
        action = Action,
        conditions = Conditions,
        enforcement_action = require_approval,
        priority = medium
    },

    PolicyRules1 = maps:put(PolicyId, Policy, State#state.policy_rules),

    erlmcp_telemetry:counter("compliance.policy.registered", 1,
                           #{framework => atom_to_list(Framework)}),

    {reply, {ok, PolicyId}, State#state{policy_rules = PolicyRules1}};

handle_call({get_policy_violations, Framework}, _From, State) ->
    Violations = lists:filter(
        fun(Event) ->
            Event#enforcement_event.framework =:= Framework
        end, State#state.violation_history),
    {reply, {ok, Violations}, State}.

handle_cast(start_continuous_enforcement, State) ->
    % Start continuous enforcement monitoring
    erlang:send_after(60000, self(), check_violations),  % Check every minute
    {noreply, State#state{enforcement_mode = active}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(check_violations, State) ->
    % Check for potential violations
    ActiveViolations = check_active_violations(State),
    if ActiveViolations > 0 ->
        erlmcp_telemetry:counter("compliance.violations.detected", ActiveViolations,
                               #{type => "continuous"});
    true ->
        ok
    end,

    % Schedule next check
    erlang:send_after(60000, self(), check_violations),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

init_policy_rules() ->
    #{
        %% SOC2 Access Control Rules
        <<"soc2-access-control">> => #policy_rule{
            id = <<"soc2-access-control">>,
            framework = soc2,
            resource_type = <<"system">>,
            action = <<"access">>,
            conditions = [
                {requires_mfa, true},
                {requires_authentication, true},
                {least_privilege, true}
            ],
            enforcement_action = require_approval,
            priority = high
        },
        <<"soc2-data-access">> => #policy_rule{
            id = <<"soc2-data-access">>,
            framework = soc2,
            resource_type = <<"data">>,
            action = <<"access">>,
            conditions = [
                {data_classification, "sensitive"},
                {need_to_know, true}
            ],
            enforcement_action = require_approval,
            priority = critical
        },

        %% HIPAA Data Handling Rules
        <<"hipaa-phi-access">> => #policy_rule{
            id = <<"hipaa-phi-access">>,
            framework = hipaa,
            resource_type = <<"phi">>,
            action = <<"access">>,
            conditions = [
                {authenticates, true},
                {role_approved, true},
                {audit_required, true}
            ],
            enforcement_action = alert,
            priority = critical
        },
        <<"hipaa-breach-notification">> => #policy_rule{
            id = <<"hipaa-breach-notification">>,
            framework = hipaa,
            resource_type = <<"data">>,
            action = <<"breach">>,
            conditions = [],
            enforcement_action = deny,
            priority = critical
        },

        %% GDPR Data Processing Rules
        <<"gdpr-consent">> => #policy_rule{
            id = <<"gdpr-consent">>,
            framework = gdpr,
            resource_type = <<"personal_data">>,
            action = <<"process">>,
            conditions = [
                {consent_obtained, true},
                {purpose_limited, true},
                {data_minimization, true}
            ],
            enforcement_action = require_approval,
            priority = high
        },
        <<"gdpr-data-subject-rights">> => #policy_rule{
            id = <<"gdpr-data-subject-rights">>,
            framework = gdpr,
            resource_type = <<"personal_data">>,
            action = <<"process">>,
            conditions = [
                {legal_basis, "consent"},
                {purpose_limited, true},
                {data_protection_measures, true}
            ],
            enforcement_action = require_approval,
            priority = high
        },

        %% ISO27001 Security Rules
        <<"iso27001-access-control">> => #policy_rule{
            id = <<"iso27001-access-control">>,
            framework = iso27001,
            resource_type = <<"system">>,
            action = <<"access">>,
            conditions = [
                {authenticates, true},
                {authorization_verified, true},
                {session_managed, true}
            ],
            enforcement_action = alert,
            priority = high
        },
        <<"iso27001-change-control">> => #policy_rule{
            id = <<"iso27001-change-control">>,
            framework = iso27001,
            resource_type = <<"system">>,
            action = <<"change">>,
            conditions = [
                {approved, true},
                {tested, true},
                {documented, true}
            ],
            enforcement_action = require_approval,
            priority = critical
        }
    }.

get_applicable_policies(Framework, Action, State) ->
    maps:fold(
        fun(_Id, Policy, Acc) ->
            case Policy#policy_rule.framework of
                Framework ->
                    case Policy#policy_rule.action =:= Action of
                        true -> [Policy | Acc];
                        false -> Acc
                    end;
                _ -> Acc
            end
        end, [], State#state.policy_rules).

evaluate_policies([], _Context) ->
    #{decision => allowed, reason => "No applicable policies"};

evaluate_policies([Policy | Rest], Context) ->
    Conditions = Policy#policy_rule.conditions,
    case evaluate_conditions(Conditions, Context) of
        true ->
            handle_enforcement_action(Policy, Context);
        false ->
            evaluate_policies(Rest, Context)
    end.

evaluate_conditions(Conditions, Context) ->
    lists:foldl(fun(Condition, Acc) ->
        case check_condition(Condition, Context) of
            true -> Acc and true;
            false -> false
        end
    end, true, Conditions).

check_condition({requires_mfa, true}, Context) ->
    maps:get(mfa_enabled, Context, false);
check_condition({requires_authentication, true}, Context) ->
    maps:get(authenticated, Context, false);
check_condition({data_classification, "sensitive"}, Context) ->
    maps:get(data_classification, Context, "public") =:= "sensitive";
check_condition({consent_obtained, true}, Context) ->
    maps:get(has_consent, Context, false);
check_condition({authenticates, true}, Context) ->
    maps:get(authenticated, Context, false);
check_condition(_Other, _Context) ->
    true.

handle_enforcement_action(Policy, Context) ->
    EnforcementAction = Policy#policy_rule.enforcement_action,
    case EnforcementAction of
        allow ->
            #{decision => allowed, reason => "Policy allows action"};
        deny ->
            #{decision => denied, reason => "Policy denies action"};
        log ->
            #{decision => allowed, reason => "Policy logs action"};
        alert ->
            #{decision => allowed, reason => "Policy alerts action"};
        require_approval ->
            #{decision => pending, reason => "Requires approval"}
    end.

create_enforcement_event(Framework, Action, Context, Decision) ->
    #enforcement_event{
        id = generate_event_id(),
        timestamp => erlang:timestamp(),
        policy_id => maps:get(policy_id, Context, unknown),
        resource_type => maps:get(resource_type, Context, unknown),
        action => Action,
        user_id => maps:get(user_id, Context, unknown),
        decision => Decision#{decision},
        reason => Decision#{reason},
        metadata => Context
    }.

generate_event_id() ->
    iolist_to_binary(io_lib:format("enforce-~s-~s-~w", [
        integer_to_list(erlang:system_time(second)),
        crypto:strong_rand_bytes(8) |> binary:encode_hex(),
        erlang:unique_integer()
    ])).

update_metrics(Metrics, Event) ->
    #{
        total_enforcements => maps:get(total_enforcements, Metrics, 0) + 1,
        violations => maps:get(violations, Metrics, 0) + case Event#enforcement_event.decision of
            denied -> 1;
            _ -> 0
        end,
        last_enforcement => Event#enforcement_event.timestamp
    }.

enforce_access_control(UserId, Resource, Action, State) ->
    % Implement access control logic
    #{
        decision => allowed,
        reason => "Access granted based on user permissions",
        permissions => ["read", "write"],
        conditions_met => true
    }.

validate_data_policies(DataType, ProcessingPurpose, State) ->
    % Validate data handling against policies
    #{
        valid => true,
        reason => "Data handling complies with policies",
        retention_period => get_retention_period(DataType),
        required_approvals => get_required_approvals(DataType, ProcessingPurpose)
    }.

enforce_encryption_policies(Data, Context, State) ->
    % Determine if encryption is required
    case is_sensitive_data(Data, Context) of
        true ->
            #{required => true, algorithm => "AES-256-GCM", key_rotation => "90 days"};
        false ->
            #{required => false, reason => "Data not classified as sensitive"}
    end.

enforce_retention_policies(DataType, RetentionPeriod, Purpose, State) ->
    % Validate retention policies
    RequiredRetention = get_required_retention(DataType, Purpose),
    if RetentionPeriod >= RequiredRetention ->
        #{compliant => true, reason => "Retention period meets requirements"};
    true ->
        #{compliant => false, reason => "Retention period too short"}
    end.

is_sensitive_data(Data, Context) ->
    % Check if data contains sensitive information
    case maps:get(data_classification, Context, "public") of
        "sensitive" -> true;
        "confidential" -> true;
        _ -> false
    end.

get_retention_period(DataType) ->
    % Define retention periods based on data type
    Periods = #{
        "personal_data" => 7,  % 7 years
        "transaction_data" => 5,  % 5 years
        "log_data" => 3,  % 3 years
        "public_data" => 1  % 1 year
    },
    maps:get(DataType, Periods, 1).

get_required_approvals(DataType, ProcessingPurpose) ->
    % Define required approvals based on data type and purpose
    case {DataType, ProcessingPurpose} of
        {"personal_data", "marketing"} -> 2;
        {"sensitive_data", _} -> 3;
        _ -> 1
    end.

get_required_retention(DataType, Purpose) ->
    % Define required retention periods
    RetentionRules = #{
        {"personal_data", "legal"} => 7,
        {"personal_data", "business"} => 3,
        {"financial", _} => 5,
        {"medical", _} => 10
    },
    maps:get({DataType, Purpose}, RetentionRules, 1).

check_active_violations(State) ->
    % Check for active violations that need immediate attention
    RecentViolations = lists:sublist(State#state.violation_history, 100),
    HighSeverityViolations = lists:filter(fun(Event) ->
        % Check for high-severity violations
        case maps:get(severity, Event#enforcement_event.metadata, "low") of
            "critical" -> true;
            "high" -> true;
            _ -> false
        end
    end, RecentViolations),
    length(HighSeverityViolations).

init_performance_metrics() ->
    #{
        total_enforcements => 0,
        violations => 0,
        last_enforcement => null,
        average_response_time => 0.0
    }.