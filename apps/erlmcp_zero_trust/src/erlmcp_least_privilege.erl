%% @doc Least Privilege Access Control Implementation
%% Implements attribute-based access control (ABAC) and just-enough access
-module(erlmcp_least_privilege).
-behaviour(gen_server).

%% API
-export([start_link/0, grant_access/4, revoke_access/3]).
-export([check_permission/4, simulate_permission/3]).
-export([create_policy/3, list_policies/1, update_policy/4]).
-export([audit_access/2, get_permission_history/3]).
-export([enforce_least_privilege/2, validate_principle_of_least_privilege/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

%% Records
-record(attribute {
    name :: binary(),
    value :: term(),
    source :: binary(),
    timestamp :: integer()
}).

-record(identity_attributes {
    identity_id :: binary(),
    attributes :: list(),
    last_updated :: integer(),
    risk_score :: float()
}).

-record.access_policy {
    id :: binary(),
    name :: binary(),
    description :: binary(),
    resource_type :: binary(),
    action :: binary(),
    effect :: allow | deny,
    conditions :: list(),
    attributes :: list(),
    priority :: integer(),
    active :: boolean()
}.

#permission_request {
    identity_id :: binary(),
    resource_id :: binary(),
    action :: binary(),
    context :: map(),
    timestamp :: integer()
}.

#permission_decision {
    request :: #permission_request{},
    decision :: allowed | denied,
    reason :: binary(),
    policies_applied :: list(),
    expires_at :: integer()
}.

#state {
    identities :: ets:tid(),
    policies :: ets:tid(),
    access_decisions :: ets:tid(),
    audit_log :: ets:tid(),
    config :: map()
}.

-define(TIMEOUT, 30000).
-define(POLICY_CACHE_TIMEOUT, 60000).
-define.ACCESS_TOKEN_EXPIRY, 3600). %% 1 hour

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Grant least privilege access
-spec grant_access(binary(), binary(), binary(), map()) -> {ok, binary()} | {error, term()}.
grant_access(IdentityId, ResourceId, Action, Context) ->
    gen_server:call(?MODULE, {grant_access, IdentityId, ResourceId, Action, Context}, 10000).

%% Revoke access
-spec revoke_access(binary(), binary(), binary()) -> ok.
revoke_access(IdentityId, ResourceId, Action) ->
    gen_server:cast(?MODULE, {revoke_access, IdentityId, ResourceId, Action}).

%% Check permission
-spec check_permission(binary(), binary(), binary(), map()) -> {ok, allowed} | {ok, denied, binary()} | {error, term()}.
check_permission(IdentityId, ResourceId, Action, Context) ->
    gen_server:call(?MODULE, {check_permission, IdentityId, ResourceId, Action, Context}, 5000).

%% Simulate permission
-spec simulate_permission(binary(), binary(), map()) -> {ok, list()} | {error, term()}.
simulate_permission(IdentityId, ResourceId, Context) ->
    gen_server:call(?MODULE, {simulate_permission, IdentityId, ResourceId, Context}, 10000).

%% Create policy
-spec create_policy(binary(), binary(), map()) -> {ok, binary()} | {error, term()}.
create_policy(PolicyName, ResourceType, PolicyData) ->
    gen_server:call(?MODULE, {create_policy, PolicyName, ResourceType, PolicyData}, 10000).

%% List policies
-spec list_policies(binary()) -> {ok, list()} | {error, term()}.
list_policies(ResourceType) ->
    gen_server:call(?MODULE, {list_policies, ResourceType}, 5000).

%% Update policy
-spec update_policy(binary(), binary(), binary(), map()) -> ok | {error, term()}.
update_policy(PolicyId, ResourceType, Action, Updates) ->
    gen_server:call(?MODULE, {update_policy, PolicyId, ResourceType, Action, Updates}, 10000).

%% Audit access
-spec audit_access(binary(), binary()) -> {ok, list()} | {error, term()}.
audit_access(IdentityId, ResourceId) ->
    gen_server:call(?MODULE, {audit_access, IdentityId, ResourceId}, 5000).

%% Get permission history
-spec get_permission_history(binary(), binary(), integer()) -> {ok, list()} | {error, term()}.
get_permission_history(IdentityId, ResourceId, Limit) ->
    gen_server:call(?MODULE, {get_permission_history, IdentityId, ResourceId, Limit}, 5000).

%% Enforce least privilege
-spec enforce_least_privilege(binary(), binary()) -> ok.
enforce_least_privilege(IdentityId, ResourceId) ->
    gen_server:cast(?MODULE, {enforce_least_privilege, IdentityId, ResourceId}).

%% Validate principle of least privilege
-spec validate_principle_of_least_privilege(binary()) -> {ok, map()} | {error, term()}.
validate_principle_of_least_privilege(ResourceId) ->
    gen_server:call(?MODULE, {validate_principle_of_least_privilege, ResourceId}, 5000).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    %% Initialize ETS tables
    Identities = ets:new(?MODULE, [set, protected, named_table, {keypos, element(2, record_info(state, 2))}]),
    Policies = ets:new(?MODULE ++ "_policies", [set, protected, named_table, {keypos, element(2, record_info(state, 2))}]),
    AccessDecisions = ets:new(?MODULE ++ "_decisions", [set, protected, named_table, {keypos, element(2, record_info(state, 2))}]),
    AuditLog = ets:new(?MODULE ++ "_audit", [set, protected, named_table, {keypos, element(2, record_info(state, 2))}]),

    %% Load initial configuration
    Config = load_config(),
    PoliciesData = load_default_policies(),

    %% Insert default policies
    lists:foreach(fun(Policy) ->
        ets:insert(Policies, Policy)
    end, PoliciesData),

    {ok, #state{
        identities = Identities,
        policies = Policies,
        access_decisions = AccessDecisions,
        audit_log = AuditLog,
        config = Config
    }}.

handle_call({grant_access, IdentityId, ResourceId, Action, Context}, _From, State) ->
    %% Get current permissions
    CurrentPermissions = get_current_permissions(IdentityId, ResourceId, State),

    %% Calculate required permissions
    RequiredPermissions = calculate_required_permissions(ResourceId, Action),

    %% Grant exactly what's needed
    NewPermissions = sets:union(CurrentPermissions, RequiredPermissions),

    %% Store permission
    PermissionId = generate_permission_id(),
    ExpiryTime = timestamp() + ?ACCESS_TOKEN_EXPIRY,

    Decision = #permission_decision{
        request = #permission_request{
            identity_id = IdentityId,
            resource_id = ResourceId,
            action = Action,
            context = Context,
            timestamp = timestamp()
        },
        decision = allowed,
        reason = "least_privilege_grant",
        policies_applied = [find_applied_policies(IdentityId, ResourceId, Action, State)],
        expires_at = ExpiryTime
    },

    %% Store decision
    ets:insert(State#state.access_decisions, {PermissionId, Decision}),

    %% Audit access
    audit_grant(IdentityId, ResourceId, Action, Context, State),

    {reply, {ok, PermissionId}, State};

handle_call({check_permission, IdentityId, ResourceId, Action, Context}, _From, State) ->
    %% Get identity attributes
    case ets:lookup(State#state.identities, IdentityId) of
        [IdentityAttributes] ->
            %% Find applicable policies
            ApplicablePolicies = find_applicable_policies(IdentityId, ResourceId, Action, State),

            %% Evaluate policies
            case evaluate_policies(ApplicablePolicies, IdentityAttributes, Context) of
                {allow, Reason} ->
                    %% Store decision
                    DecisionId = store_permission_decision(IdentityId, ResourceId, Action, allowed, Reason, State),
                    {reply, {ok, allowed}, State};
                {deny, Reason} ->
                    DecisionId = store_permission_decision(IdentityId, ResourceId, Action, denied, Reason, State),
                    {reply, {ok, denied, Reason}, State}
            end;
        [] ->
            {reply, {error, identity_not_found}, State}
    end;

handle_call({simulate_permission, IdentityId, ResourceId, Context}, _From, State) ->
    %% Simulate all possible actions on resource
    PossibleActions = get_possible_actions(ResourceId),

    Results = lists:map(fun(Action) ->
        case check_permission(IdentityId, ResourceId, Action, Context) of
            {ok, allowed} ->
                {Action, allowed, "granted"};
            {ok, denied, Reason} ->
                {Action, denied, Reason};
            {error, Reason} ->
                {Action, error, Reason}
        end
    end, PossibleActions),

    {reply, {ok, Results}, State};

handle_call({create_policy, PolicyName, ResourceType, PolicyData}, _From, State) ->
    PolicyId = generate_policy_id(),
    Policy = #access_policy{
        id = PolicyId,
        name = PolicyName,
        description = maps:get(description, PolicyData, <<"">>),
        resource_type = ResourceType,
        action = maps:get(action, PolicyData),
        effect = maps:get(effect, PolicyData, allow),
        conditions = maps:get(conditions, PolicyData, []),
        attributes = maps:get(attributes, PolicyData, []),
        priority = maps:get(priority, PolicyData, 100),
        active = true
    },

    ets:insert(State#state.policies, Policy),
    {reply, {ok, PolicyId}, State};

handle_call({list_policies, ResourceType}, _From, State) ->
    Policies = ets:foldl(fun(Policy, Acc) ->
        case Policy#access_policy.resource_type =:= ResourceType of
            true -> [Policy | Acc];
            false -> Acc
        end
    end, [], State#state.policies),

    {reply, {ok, Policies}, State};

handle_call({update_policy, PolicyId, ResourceType, Action, Updates}, _From, State) ->
    case ets:lookup(State#state.policies, PolicyId) of
        [Policy] ->
            UpdatedPolicy = Policy#access_policy{
                action = Action,
                conditions = maps:get(conditions, Updates, Policy#access_policy.conditions),
                attributes = maps:get(attributes, Updates, Policy#access_policy.attributes),
                priority = maps:get(priority, Updates, Policy#access_policy.priority),
                active = maps:get(active, Updates, Policy#access_policy.active)
            },

            ets:insert(State#state.policies, UpdatedPolicy),
            {reply, ok, State};
        [] ->
            {reply, {error, not_found}, State}
    end;

handle_call({audit_access, IdentityId, ResourceId}, _From, State) ->
    AuditEntries = ets:foldl(fun({_Key, Entry}, Acc) ->
        case Entry#permission_decision.request.identity_id =:= IdentityId andalso
             Entry#permission_decision.request.resource_id =:= ResourceId of
            true -> [Entry | Acc];
            false -> Acc
        end
    end, [], State#state.access_decisions),

    {reply, {ok, AuditEntries}, State};

handle_call({get_permission_history, IdentityId, ResourceId, Limit}, _From, State) ->
    History = ets:foldl(fun({_Key, Entry}, Acc) ->
        case Entry#permission_decision.request.identity_id =:= IdentityId andalso
             Entry#permission_decision.request.resource_id =:= ResourceId of
            true -> [Entry | Acc];
            false -> Acc
        end
    end, [], State#state.access_decisions),

    LimitedHistory = lists:sublist(lists:sort(fun(E1, E2) ->
        E1#permission_decision.request.timestamp > E2#permission_decision.request.timestamp
    end, History), Limit),

    {reply, {ok, LimitedHistory}, State};

handle_call({validate_principle_of_least_privilege, ResourceId}, _From, State) ->
    %% Analyze current permissions for the resource
    AllPermissions = get_all_permissions(ResourceId, State),

    %% Check for excess permissions
    ExcessPermissions = find_excess_permissions(AllPermissions, State),

    %% Generate report
    Report = #{
        resource_id => ResourceId,
        total_permissions => length(AllPermissions),
        excess_permissions => length(ExcessPermissions),
        compliance_score => calculate_compliance_score(AllPermissions, ExcessPermissions),
        recommendations => generate_recommendations(AllPermissions, ExcessPermissions)
    },

    {reply, {ok, Report}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({revoke_access, IdentityId, ResourceId, Action}, State) ->
    %% Remove specific permission
    ets:match_delete(State#state.access_decisions, '_', IdentityId, ResourceId, Action),

    %% Audit revocation
    audit_revoke(IdentityId, ResourceId, Action, State),

    {noreply, State};

handle_cast({enforce_least_privilege, IdentityId, ResourceId}, State) ->
    %% Analyze current permissions
    CurrentPermissions = get_current_permissions(IdentityId, ResourceId, State),

    %% Find minimum required permissions
    RequiredPermissions = find_minimum_required_permissions(IdentityId, ResourceId, State),

    %% Revoke excess permissions
    ExcessPermissions = sets:subtract(CurrentPermissions, RequiredPermissions),

    %% Revoke excess
    revoke_excess_permissions(IdentityId, ResourceId, ExcessPermissions, State),

    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

load_config() ->
    #{
        policy_cache_ttl => ?POLICY_CACHE_TIMEOUT,
        max_permissions_per_identity => 100,
        audit_retention_days => 90,
        enforcement_enabled => true,
        simulation_enabled => true
    }.

load_default_policies() ->
    %% Load default ABAC policies for Fortune 500
    [
        #access_policy{
            id = <<"standard_read">>,
            name = <<"Standard Read Access">>,
            description => <<"Standard read access for all users">>,
            resource_type = <<"*">>,
            action = <<"read">>,
            effect = allow,
            conditions = [],
            attributes = [],
            priority = 100,
            active = true
        },
        #access_policy{
            id = <<"admin_full">>,
            name = <<"Administrator Full Access">>,
            description => <<"Full access for administrators">>,
            resource_type = <<"*">>,
            action = <<"*">>,
            effect = allow,
            conditions = [{attribute, <<"role">>, <<"admin">>}],
            attributes = [],
            priority = 50,
            active = true
        },
        #access_policy{
            id = <<"deny_all">>,
            name = <<"Default Deny">>,
            description => <<"Default deny policy">>,
            resource_type = <<"*">>,
            action = <<"*">>,
            effect = deny,
            conditions = [],
            attributes = [],
            priority = 1000,
            active = true
        }
    ].

get_current_permissions(IdentityId, ResourceId, State) ->
    %% Get current permissions from decisions
    Permissions = ets:foldl(fun({_Key, Decision}, Acc) ->
        case Decision#permission_decision.request.identity_id =:= IdentityId andalso
             Decision#permission_decision.request.resource_id =:= ResourceId of
            true ->
                sets:add_element(Decision#permission_decision.request.action, Acc);
            false ->
                Acc
        end
    end, sets:new(), State#state.access_decisions),

    Permissions.

calculate_required_permissions(ResourceId, Action) ->
    %% Calculate minimum required permissions based on resource type and action
    case ResourceId of
        <<"database">> ->
            case Action of
                <<"read">> -> sets:from_list([select]);
                <<"write">> -> sets:from_list([insert, update]);
                <<"delete">> -> sets:from_list([delete]);
                _ -> sets:from_list([Action])
            end;
        <<"api">> ->
            case Action of
                <<"read">> -> sets:from_list([get]);
                <<"write">> -> sets:from_list([post, put]);
                <<"delete">> -> sets:from_list([delete]);
                _ -> sets:from_list([Action])
            end;
        _ ->
            sets:from_list([Action])
    end.

find_applicable_policies(IdentityId, ResourceId, Action, State) ->
    %% Find all policies that apply to this request
    ApplicablePolicies = ets:foldl(fun(Policy, Acc) ->
        case policy_applies(Policy, IdentityId, ResourceId, Action, State) of
            true -> [Policy | Acc];
            false -> Acc
        end
    end, [], State#state.policies),

    %% Sort by priority (lower number = higher priority)
    lists:sort(fun(P1, P2) ->
        P1#access_policy.priority =< P2#access_policy.priority
    end, ApplicablePolicies).

policy_applies(Policy, IdentityId, ResourceId, Action, State) ->
    %% Check policy conditions
    case Policy#access_policy.resource_type =:= <<"*">> orelse
         Policy#access_policy.resource_type =:= ResourceId of
        true ->
            case Policy#access_policy.action =:= <<"*">> orelse
                 Policy#access_policy.action =:= Action of
                true ->
                    %% Check attribute conditions
                    check_attribute_conditions(Policy#access_policy.conditions, IdentityId, State);
                false ->
                    false
            end;
        false ->
            false
    end.

check_attribute_conditions(Conditions, IdentityId, State) ->
    case ets:lookup(State#state.identities, IdentityId) of
        [IdentityAttributes] ->
            lists:all(fun(Condition) ->
                case Condition of
                    {attribute, Name, ExpectedValue} ->
                        case find_attribute_value(Name, IdentityAttributes) of
                            ExpectedValue -> true;
                            _ -> false
                        end;
                    {time_range, Start, End} ->
                        CurrentTime = timestamp(),
                        CurrentTime >= Start andalso CurrentTime =< End;
                    {ip_range, StartIP, EndIP} ->
                        check_ip_in_range(get_client_ip(), StartIP, EndIP)
                end
            end, Conditions);
        [] ->
            false
    end.

find_attribute_value(Name, IdentityAttributes) ->
    %% Find attribute value by name
    case lists:keyfind(Name, #attribute.name, IdentityAttributes#identity_attributes.attributes) of
        #attribute{value = Value} -> Value;
        false -> undefined
    end.

evaluate_policies([Policy | Rest], IdentityAttributes, Context) ->
    case Policy#access_policy.effect of
        allow ->
            case check_attribute_conditions(Policy#access_policy.conditions, IdentityAttributes#identity_attributes.identity_id, #state{identities = ets:fun2ms(fun(I) -> I end)}) of
                true ->
                    {allow, Policy#access_policy.name};
                false ->
                    evaluate_policies(Rest, IdentityAttributes, Context)
            end;
        deny ->
            {deny, Policy#access_policy.name}
    end;

evaluate_policies([], _IdentityAttributes, _Context) ->
    {deny, "no_matching_policy"}.

find_applied_policies(IdentityId, ResourceId, Action, State) ->
    %% Find policies that would apply to this request
    ApplicablePolicies = find_applicable_policies(IdentityId, ResourceId, Action, State),
    lists:map(fun(P) -> P#access_policy.name end, ApplicablePolicies).

store_permission_decision(IdentityId, ResourceId, Action, Decision, Reason, State) ->
    DecisionId = generate_decision_id(),

    DecisionRecord = #permission_decision{
        request = #permission_request{
            identity_id = IdentityId,
            resource_id = ResourceId,
            action = Action,
            context = #{},
            timestamp = timestamp()
        },
        decision = Decision,
        reason = Reason,
        policies_applied = [],
        expires_at = timestamp() + ?ACCESS_TOKEN_EXPIRY
    },

    ets:insert(State#state.access_decisions, {DecisionId, DecisionRecord}),
    DecisionId.

audit_grant(IdentityId, ResourceId, Action, Context, State) ->
    AuditEntry = #{
        timestamp => timestamp(),
        identity_id => IdentityId,
        resource_id => ResourceId,
        action => Action,
        type => grant,
        context => Context,
        decision => allowed
    },

    ets:insert(State#state.audit_log, {audit_key(AuditEntry), AuditEntry}).

audit_revoke(IdentityId, ResourceId, Action, State) ->
    AuditEntry = #{
        timestamp => timestamp(),
        identity_id => IdentityId,
        resource_id => ResourceId,
        action => Action,
        type => revoke,
        decision => denied
    },

    ets:insert(State#state.audit_log, {audit_key(AuditEntry), AuditEntry}).

get_possible_actions(ResourceId) ->
    %% Define possible actions for resource type
    case ResourceId of
        <<"database">> -> [select, insert, update, delete];
        <<"api">> -> [get, post, put, delete];
        <<"file">> -> [read, write, execute];
        _ -> [<<"*">>]
    end.

get_all_permissions(ResourceId, State) ->
    %% Get all permissions for a resource
    Permissions = ets:foldl(fun({_Key, Decision}, Acc) ->
        case Decision#permission_decision.request.resource_id =:= ResourceId of
            true -> [Decision | Acc];
            false -> Acc
        end
    end, [], State#state.access_decisions),

    Permissions.

find_excess_permissions(Permissions, State) ->
    %% Find permissions that exceed minimum required
    Excess = lists:filter(fun(Permission) ->
        is_excess_permission(Permission, State)
    end, Permissions),

    Excess.

is_excess_permission(Permission, State) ->
    %% Check if permission is excessive
    %% This is a simplified check - in production would be more sophisticated
    case Permission#permission_decision.request.action of
        <<"*">> -> true;
        _ -> false
    end.

calculate_compliance_score(AllPermissions, ExcessPermissions) ->
    Total = length(AllPermissions),
    Excess = length(ExcessPermissions),
    case Total of
        0 -> 1.0;
        _ -> (Total - Excess) / Total
    end.

generate_recommendations(AllPermissions, ExcessPermissions) ->
    %% Generate recommendations for improving compliance
    Recommendations = [
        <<"Review permissions exceeding minimum requirements">>,
        <<"Implement time-based access controls">>,
        <<"Consider role-based access patterns">>
    ],

    case length(ExcessPermissions) > 0 of
        true -> Recommendations;
        false -> [<<"No recommendations needed">>]
    end.

revoke_excess_permissions(IdentityId, ResourceId, ExcessPermissions, State) ->
    %% Remove excess permissions
    lists:foreach(fun(Action) ->
        ets:match_delete(State#state.access_decisions, '_', IdentityId, ResourceId, Action)
    end, sets:to_list(ExcessPermissions)).

find_minimum_required_permissions(IdentityId, ResourceId, State) ->
    %% Find minimum required permissions based on identity and resource
    %% This would involve analyzing the identity's role, resource sensitivity, etc.
    Required = [<<"read">>], %% Simplified
    sets:from_list(Required).

generate_permission_id() ->
    crypto:strong_rand_bytes(16).

generate_decision_id() ->
    crypto:strong_rand_bytes(16).

audit_key(AuditEntry) ->
    {AuditEntry#timestamp, AuditEntry#identity_id, AuditEntry#resource_id}.

timestamp() ->
    erlang:system_time(millisecond).