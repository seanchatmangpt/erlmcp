-module(erlmcp_api_gateway_compliance).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([
    add_policy/1, validate_policy/1, enforce_policy/2,
    check_compliance/1, generate_compliance_report/1,
    audit_event/2, set_standard/2
]).

-record.policy, {
    id :: binary(),
    name :: binary(),
    description :: binary(),
    standard :: binary(),
    rules :: list(),
    severity :: low | medium | high | critical,
    active :: boolean(),
    created_at :: integer()
}.

-record.audit_log, {
    id :: binary(),
    timestamp :: integer(),
    api_id :: binary(),
    consumer_id :: binary(),
    action :: binary(),
    details :: map(),
    result :: passed | failed | error,
    policy_id :: binary()
}.

-record.compliance_standard, {
    id :: binary(),
    name :: binary(),
    description :: binary(),
    requirements :: list(),
    version :: binary()
}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #{policies => #{}, audit_log => [], standards => #{}}}.

add_policy(PolicySpec) ->
    gen_server:call(?MODULE, {add_policy, PolicySpec}).

validate_policy(PolicyId) ->
    gen_server:call(?MODULE, {validate_policy, PolicyId}).

enforce_policy(ApiId, PolicyId) ->
    gen_server:call(?MODULE, {enforce_policy, ApiId, PolicyId}).

check_compliance(ApiId) ->
    gen_server:call(?MODULE, {check_compliance, ApiId}).

generate_compliance_report(Standard) ->
    gen_server:call(?MODULE, {generate_compliance_report, Standard}).

audit_event(Event, PolicyId) ->
    gen_server:cast(?MODULE, {audit_event, Event, PolicyId}).

set_standard(StandardId, StandardSpec) ->
    gen_server:call(?MODULE, {set_standard, StandardId, StandardSpec}).

handle_call({add_policy, PolicySpec}, _From, State) ->
    PolicyId = generate_policy_id(),
    Policy = #policy{
        id = PolicyId,
        name = maps:get(name, PolicySpec),
        description = maps:get(description, PolicySpec),
        standard = maps:get(standard, PolicySpec),
        rules = maps:get(rules, PolicySpec),
        severity = maps:get(severity, PolicySpec, medium),
        active = maps:get(active, PolicySpec, true),
        created_at = erlang:system_time(millisecond)
    },

    Policies = State#{policies},
    NewPolicies = maps:put(PolicyId, Policy, Policies),

    {reply, {ok, Policy}, State#{policies => NewPolicies}};

handle_call({validate_policy, PolicyId}, _From, State) ->
    case maps:find(PolicyId, State#{policies}) of
        {ok, Policy} ->
            Validations = validate_policy_rules(Policy#policy.rules),
            {reply, {ok, Validations}, State};
        error ->
            {reply, {error, policy_not_found}, State}
    end;

handle_call({enforce_policy, ApiId, PolicyId}, _From, State) ->
    case maps:find(PolicyId, State#{policies}) of
        {ok, #policy{rules = Rules} = Policy} when Policy#policy.active =:= true ->
            Api = erlmcp_api_gateway_registry:get_api(ApiId),
            case Api of
                {ok, ApiData} ->
                    Validation = validate_rules_against_api(Rules, ApiData),
                    case Validation of
                        {ok, compliant} ->
                            enforce_policy_actions(Rules, ApiId);
                        {error, violations} ->
                            {error, violations}
                    end;
                {error, not_found} ->
                    {error, api_not_found}
            end;
        {ok, _} ->
            {error, policy_inactive};
        error ->
            {error, policy_not_found}
    end;

handle_call({check_compliance, ApiId}, _From, State) ->
    Policies = State#{policies},
    ActivePolicies = maps:filter(fun(_, Policy) ->
        Policy#policy.active =:= true
    end, Policies),

    ComplianceResults = lists:foldl(fun(PolicyId, Acc) ->
        case enforce_policy(ApiId, PolicyId) of
            {ok, _} -> maps:put(PolicyId, passed, Acc);
            {error, _} -> maps:put(PolicyId, failed, Acc)
        end
    end, #{}, maps:keys(ActivePolicies)),

    OverallStatus = case lists:any(fun(Result) -> Result =:= failed end, maps:values(ComplianceResults)) of
        true -> non_compliant;
        false -> compliant
    end,

    {reply, {ok, #{overall => OverallStatus, results => ComplianceResults}}, State};

handle_call({generate_compliance_report, Standard}, _From, State) ->
    Report = generate_compliance_report_for_standard(Standard, State),
    {reply, {ok, Report}, State};

handle_call({set_standard, StandardId, StandardSpec}, _From, State) ->
    Standard = #compliance_standard{
        id = StandardId,
        name = maps:get(name, StandardSpec),
        description = maps:get(description, StandardSpec),
        requirements = maps:get(requirements, StandardSpec),
        version = maps:get(version, StandardSpec, <<"1.0">>)
    },

    Standards = State#{standards},
    NewStandards = maps:put(StandardId, Standard, Standards),

    {reply, {ok, Standard}, State#{standards => NewStandards}}.

handle_cast({audit_event, Event, PolicyId}, State) ->
    AuditEntry = #audit_log{
        id = generate_audit_id(),
        timestamp = erlang:system_time(millisecond),
        api_id = maps:get(api_id, Event),
        consumer_id = maps:get(consumer_id, Event),
        action = maps:get(action, Event),
        details = maps:get(details, Event, #{}),
        result = maps:get(result, Event, passed),
        policy_id = PolicyId
    },

    AuditLog = State#{audit_log},
    NewAuditLog = [AuditEntry | AuditLog],

    CheckTriggered = check_if_policy_triggered(AuditEntry#audit_log.result, PolicyId),
    if
        CheckTriggered =:= true ->
            trigger_compliance_alert(AuditEntry);
        true ->
            ok
    end,

    {noreply, State#{audit_log => NewAuditLog}}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

validate_policy_rules(Rules) ->
    lists:map(fun(Rule) ->
        validate_rule(Rule)
    end, Rules).

validate_rule(Rule) ->
    case Rule of
        #{type := authentication} ->
            validate_auth_rule(Rule);
        #{type := rate_limiting} ->
            validate_rate_limit_rule(Rule);
        #{type := encryption} ->
            validate_encryption_rule(Rule);
        #{type := audit} ->
            validate_audit_rule(Rule);
        _ ->
            {error, invalid_rule_type}
    end.

validate_auth_rule(Rule) ->
    Required = [type, method, level],
    case check_required_fields(Required, Rule) of
        true -> {ok, valid};
        false -> {error, missing_required_fields}
    end.

validate_rate_limit_rule(Rule) ->
    Required = [type, limit, window],
    case check_required_fields(Required, Rule) of
        true ->
            Limit = maps:get(limit, Rule),
            Window = maps:get(window, Rule),
            if
                is_integer(Limit) andalso is_binary(Window) ->
                    {ok, valid};
                true ->
                    {error, invalid_limit_or_window}
            end;
        false ->
            {error, missing_required_fields}
    end.

validate_encryption_rule(Rule) ->
    Required = [type, algorithm, strength],
    case check_required_fields(Required, Rule) of
        true ->
            Algorithm = maps:get(algorithm, Rule),
            Strength = maps:get(strength, Rule),
            case validate_algorithm_strength(Algorithm, Strength) of
                true -> {ok, valid};
                false -> {error, invalid_algorithm_strength}
            end;
        false ->
            {error, missing_required_fields}
    end.

validate_audit_rule(Rule) ->
    Required = [type, level, retention],
    case check_required_fields(Required, Rule) of
        true -> {ok, valid};
        false -> {error, missing_required_fields}
    end.

check_required_fields(Required, Rule) ->
    lists:all(fun(Field) ->
        maps:is_key(Field, Rule)
    end, Required).

validate_algorithm_strength(algorithm, strength) ->
    AllowedStrengths = [128, 256, 512],
    lists:member(strength, AllowedStrengths).

validate_rules_against_api(Rules, ApiData) ->
    Violations = lists:foldl(fun(Rule, Acc) ->
        case validate_rule_against_api(Rule, ApiData) of
            {ok, _} -> Acc;
            {error, Violation} -> [Violation | Acc]
        end
    end, [], Rules),

    case Violations of
        [] -> {ok, compliant};
        _ -> {error, Violations}
    end.

validate_rule_against_api(Rule, ApiData) ->
    case maps:get(type, Rule) of
        authentication ->
            validate_api_auth(Rule, ApiData);
        rate_limiting ->
            validate_api_rate_limit(Rule, ApiData);
        encryption ->
            validate_api_encryption(Rule, ApiData);
        audit ->
            validate_api_audit(Rule, ApiData)
    end.

validate_api_auth(Rule, ApiData) ->
    case maps:get(security, ApiData, undefined) of
        undefined ->
            {error, #{
                rule_type => authentication,
                violation => missing_security_configuration
            }};
        SecurityConfig ->
            case maps:get(auth_type, SecurityConfig, undefined) of
                undefined ->
                    {error, #{
                        rule_type => authentication,
                        violation => missing_auth_type
                    }};
                _ ->
                    {ok, valid}
            end
    end.

validate_api_rate_limit(Rule, ApiData) ->
    case maps:get(rate_limit, ApiData, undefined) of
        undefined ->
            {error, #{
                rule_type => rate_limiting,
                violation => missing_rate_limit_configuration
            }};
        RateLimit ->
            RequiredLimit = maps:get(limit, Rule),
            RequiredWindow = maps:get(window, Rule),
            ActualLimit = maps:get(requests, RateLimit, 0),
            ActualWindow = maps:get(window, RateLimit, <<"1m">>),

            if
                ActualLimit < RequiredLimit ->
                    {error, #{
                        rule_type => rate_limiting,
                        violation => insufficient_rate_limit,
                        required => RequiredLimit,
                        actual => ActualLimit
                    }};
                ActualWindow =/= RequiredWindow ->
                    {error, #{
                        rule_type => rate_limiting,
                        violation => incorrect_rate_limit_window,
                        required => RequiredWindow,
                        actual => ActualWindow
                    }};
                true ->
                    {ok, valid}
            end
    end.

validate_api_encryption(Rule, ApiData) ->
    case maps:get(ssl, ApiData, undefined) of
        undefined ->
            {error, #{
                rule_type => encryption,
                violation => missing_ssl_configuration
            }};
        SSLConfig ->
            RequiredAlgorithm = maps:get(algorithm, Rule),
            RequiredStrength = maps:get(strength, Rule),
            ActualAlgorithm = maps:get(cipher, SSLConfig, undefined),
            ActualStrength = maps:get(strength, SSLConfig, 0),

            case is_encryption_compatible(ActualAlgorithm, RequiredAlgorithm, ActualStrength, RequiredStrength) of
                true -> {ok, valid};
                false ->
                    {error, #{
                        rule_type => encryption,
                        violation => encryption_not_compliant,
                        required => #{algorithm => RequiredAlgorithm, strength => RequiredStrength},
                        actual => #{algorithm => ActualAlgorithm, strength => ActualStrength}
                    }}
            end
    end.

validate_api_audit(Rule, ApiData) ->
    case maps:get(audit, ApiData, undefined) of
        undefined ->
            {error, #{
                rule_type => audit,
                violation => missing_audit_configuration
            }};
        _ ->
            {ok, valid}
    end.

is_encryption_compatible(ActualAlg, RequiredAlg, ActualStrength, RequiredStrength) ->
    (ActualAlg =:= RequiredAlg) orelse
    (is_cipher_compatible(ActualAlg, RequiredAlg)) andalso
    ActualStrength >= RequiredStrength.

is_cipher_compatible(Actual, Required) ->
    CipherFamilies = #{
        aes => [aes128, aes256],
        rsa => [rsa1024, rsa2048, rsa4096],
        ecc => [secp256r1, secp384r1]
    },
    CipherGroups = maps:values(CipherFamilies),
    lists:any(fun(Group) ->
        lists:member(Actual, Group) andalso lists:member(Required, Group)
    end, CipherGroups).

enforce_policy_actions(Rules, ApiId) ->
    lists:foreach(fun(Rule) ->
        enforce_rule_action(Rule, ApiId)
    end, Rules).

enforce_rule_action(Rule, ApiId) ->
    case maps:get(type, Rule) of
        authentication ->
            enforce_auth_policy(Rule, ApiId);
        rate_limiting ->
            enforce_rate_limit_policy(Rule, ApiId);
        encryption ->
            enforce_encryption_policy(Rule, ApiId);
        audit ->
            enforce_audit_policy(Rule, ApiId)
    end.

enforce_auth_policy(Rule, ApiId) ->
    erlmcp_api_gateway_registry:update_api(ApiId, #{security => #{auth_type => maps:get(method, Rule)}}).

enforce_rate_limit_policy(Rule, ApiId) ->
    erlmcp_api_gateway_registry:update_api(ApiId, #{
        rate_limit => #{requests => maps:get(limit, Rule), window => maps:get(window, Rule)}
    }).

enforce_encryption_policy(Rule, ApiId) ->
    erlmcp_api_gateway_registry:update_api(ApiId, #{
        ssl => #{cipher => maps:get(algorithm, Rule), strength => maps:get(strength, Rule)}
    }).

enforce_audit_policy(Rule, ApiId) ->
    erlmcp_api_gateway_registry:update_api(ApiId, #{audit => #{enabled => true}}).

generate_compliance_report_for_standard(Standard, State) ->
    case maps:find(Standard, State#{standards}) of
        {ok, ComplianceStandard} ->
            Report = #{
                standard => ComplianceStandard#compliance_standard.name,
                version => ComplianceStandard#compliance_standard.version,
                requirements => ComplianceStandard#compliance_standard.requirements,
                policies => list_policies_for_standard(Standard, State),
                compliance_score => calculate_compliance_score(Standard, State),
                last_updated => erlang:system_time(millisecond)
            },
            Report;
        error ->
            {error, standard_not_found}
    end.

list_policies_for_standard(Standard, State) ->
    Policies = State#{policies},
    lists:filtermap(fun({_, Policy}) ->
        case Policy#policy.standard =:= Standard andalso Policy#policy.active =:= true of
            true -> {true, Policy};
            false -> false
        end
    end, maps:to_list(Policies)).

calculate_compliance_score(Standard, State) ->
    Policies = list_policies_for_standard(Standard, State),
    TotalPolicies = length(Policies),

    case TotalPolicies of
        0 -> 0;
        _ ->
            PassedPolicies = lists:foldl(fun(Policy, Acc) ->
                case enforce_policy(Policy#policy.api_id, Policy#policy.id) of
                    {ok, _} -> Acc + 1;
                    {error, _} -> Acc
                end
            end, 0, Policies),
            (PassedPolicies / TotalPolicies) * 100
    end.

check_if_policy_triggered(Result, PolicyId) ->
    case Result of
        failed -> true;
        error -> true;
        _ -> false
    end.

trigger_compliance_alert(AuditEntry) ->
    io:format("COMPLIANCE ALERT: ~p ~p~n", [AuditEntry#audit_log.api_id, AuditEntry#audit_log.policy_id]).

generate_policy_id() ->
    uuid:uuid4().

generate_audit_id() ->
    uuid:uuid4().