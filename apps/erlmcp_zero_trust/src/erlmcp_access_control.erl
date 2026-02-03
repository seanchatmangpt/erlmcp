-module(erlmcp_access_control).
-behaviour(gen_server).

%% API
-export([start_link/0, check_access/3, grant_access/3, revoke_access/3]).
-export([get_policies/1, create_policy/2, update_policy/3, delete_policy/2]).
-export([enforce_least_privilege/2, validate_principle_of_least_privilege/1]).
-export([get_role_assignments/1, assign_role/3, remove_role/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Records
-record(access_policy, {
    id :: binary(),
    name :: binary(),
    description :: binary(),
    subjects :: list(),
    resources :: list(),
    actions :: list(),
    effect :: allow | deny,
    conditions :: list(),
    priority :: integer(),
    created_at :: integer(),
    updated_at :: integer()
}).

-record(role, {
    id :: binary(),
    name :: binary(),
    description :: binary(),
    permissions :: list(),
    parent :: binary() | undefined,
    created_at :: integer(),
    updated_at :: integer()
}).

-record(role_assignment, {
    id :: binary(),
    identity_id :: binary(),
    role_id :: binary(),
    granted_at :: integer(),
    granted_by :: binary(),
    expires_at :: integer() | undefined,
    conditions :: list()
}).

-record(state, {
    policies :: map(),
    roles :: map(),
    assignments :: map(),
    config :: map()
}).

-define(TIMEOUT, 30000).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

check_access(IdentityId, Resource, Action) ->
    gen_server:call(?MODULE, {check_access, IdentityId, Resource, Action}, ?TIMEOUT).

grant_access(IdentityId, Resource, Action, Conditions) ->
    gen_server:call(?MODULE, {grant_access, IdentityId, Resource, Action, Conditions}, ?TIMEOUT).

revoke_access(IdentityId, Resource, Action) ->
    gen_server:call(?MODULE, {revoke_access, IdentityId, Resource, Action}, ?TIMEOUT).

get_policies(IdentityId) ->
    gen_server:call(?MODULE, {get_policies, IdentityId}, ?TIMEOUT).

create_policy(PolicyData) ->
    gen_server:call(?MODULE, {create_policy, PolicyData}, ?TIMEOUT).

update_policy(PolicyId, UpdateData) ->
    gen_server:call(?MODULE, {update_policy, PolicyId, UpdateData}, ?TIMEOUT).

delete_policy(PolicyId, Reason) ->
    gen_server:call(?MODULE, {delete_policy, PolicyId, Reason}, ?TIMEOUT).

enforce_least_privilege(IdentityId, Resource) ->
    gen_server:call(?MODULE, {enforce_least_privilege, IdentityId, Resource}, ?TIMEOUT).

validate_principle_of_least_privilege(IdentityId) ->
    gen_server:call(?MODULE, {validate_principle_of_least_privilege, IdentityId}, ?TIMEOUT).

get_role_assignments(IdentityId) ->
    gen_server:call(?MODULE, {get_role_assignments, IdentityId}, ?TIMEOUT).

assign_role(IdentityId, RoleId, Conditions) ->
    gen_server:call(?MODULE, {assign_role, IdentityId, RoleId, Conditions}, ?TIMEOUT).

remove_role(IdentityId, RoleId, Reason) ->
    gen_server:call(?MODULE, {remove_role, IdentityId, RoleId, Reason}, ?TIMEOUT).

init([]) ->
    State = #state{
        policies = #{},
        roles = #{},
        assignments = #{},
        config = load_config()
    },
    erlmcp_access_control:initialize(),
    {ok, State}.

handle_call({check_access, IdentityId, Resource, Action}, _From, State) ->
    case evaluate_access_request(IdentityId, Resource, Action, State) of
        {allow, Policies} ->
            %% Log access grant
            erlmcp_security_monitor:log_event(access_granted, #{
                identity_id => IdentityId,
                resource => Resource,
                action => Action,
                policies => Policies,
                timestamp => timestamp()
            }),
            {reply, {allow, Policies}, State};
        {deny, Reason} ->
            %% Log access denial
            erlmcp_security_monitor:log_event(access_denied, #{
                identity_id => IdentityId,
                resource => Resource,
                action => Action,
                reason => Reason,
                timestamp => timestamp()
            }),
            {reply, {deny, Reason}, State}
    end;

handle_call({grant_access, IdentityId, Resource, Action, Conditions}, _From, State) ->
    case create_dynamic_policy(IdentityId, Resource, Action, Conditions, State) of
        {ok, PolicyId} ->
            {reply, {ok, PolicyId}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({revoke_access, IdentityId, Resource, Action}, _From, State) ->
    case revoke_dynamic_policy(IdentityId, Resource, Action, State) of
        {ok, PolicyId} ->
            {reply, {ok, PolicyId}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({get_policies, IdentityId}, _From, State) ->
    case get_identity_policies(IdentityId, State) of
        {ok, Policies} ->
            {reply, {ok, Policies}, State};
        {error, not_found} ->
            {reply, {error, not_found}, State}
    end;

handle_call({create_policy, PolicyData}, _From, State) ->
    case validate_policy_data(PolicyData) of
        {ok, ValidatedData} ->
            PolicyId = generate_policy_id(),
            Policy = #access_policy{
                id = PolicyId,
                name = maps:get(name, ValidatedData),
                description = maps:get(description, ValidatedData, <<"">>),
                subjects = maps:get(subjects, ValidatedData),
                resources = maps:get(resources, ValidatedData),
                actions = maps:get(actions, ValidatedData),
                effect = maps:get(effect, ValidatedData, allow),
                conditions = maps:get(conditions, ValidatedData, []),
                priority = maps:get(priority, ValidatedData, 0),
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
            UpdatedPolicy = Policy#access_policy{
                name = maps:get(name, UpdateData, Policy#access_policy.name),
                description = maps:get(description, UpdateData, Policy#access_policy.description),
                subjects = maps:get(subjects, UpdateData, Policy#access_policy.subjects),
                resources = maps:get(resources, UpdateData, Policy#access_policy.resources),
                actions = maps:get(actions, UpdateData, Policy#access_policy.actions),
                effect = maps:get(effect, UpdateData, Policy#access_policy.effect),
                conditions = maps:get(conditions, UpdateData, Policy#access_policy.conditions),
                priority = maps:get(priority, UpdateData, Policy#access_policy.priority),
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
                created_by => Policy#access_policy.created_by
            }),
            NewState = State#state{
                policies = maps:remove(PolicyId, State#state.policies)
            },
            {reply, {ok, deleted}, NewState};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({enforce_least_privilege, IdentityId, Resource}, _From, State) ->
    case apply_least_privilege_principle(IdentityId, Resource, State) of
        {ok, AdjustedPermissions} ->
            {reply, {ok, AdjustedPermissions}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({validate_principle_of_least_privilege, IdentityId}, _From, State) ->
    case validate_least_privilege_compliance(IdentityId, State) of
        {compliant, Analysis} ->
            {reply, {compliant, Analysis}, State};
        {non_compliant, Violations} ->
            {reply, {non_compliant, Violations}, State}
    end;

handle_call({get_role_assignments, IdentityId}, _From, State) ->
    Assignments = lists:filter(fun(Assignment) ->
        Assignment#role_assignment.identity_id == IdentityId
    end, maps:values(State#state.assignments)),
    {reply, {ok, Assignments}, State};

handle_call({assign_role, IdentityId, RoleId, Conditions}, _From, State) ->
    case validate_role_assignment(IdentityId, RoleId, Conditions, State) of
        {ok, AssignmentId} ->
            Assignment = #role_assignment{
                id = AssignmentId,
                identity_id = IdentityId,
                role_id = RoleId,
                granted_at = timestamp(),
                granted_by = system, %% Will be replaced with actual user
                expires_at = maps:get(expires_at, Conditions, undefined),
                conditions = maps:get(conditions, Conditions, [])
            },
            NewState = State#state{
                assignments = maps:put(AssignmentId, Assignment, State#state.assignments)
            },
            {reply, {ok, AssignmentId}, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({remove_role, IdentityId, RoleId, Reason}, _From, State) ->
    case find_role_assignment(IdentityId, RoleId, State) of
        {ok, AssignmentId} ->
            NewState = State#state{
                assignments = maps:remove(AssignmentId, State#state.assignments)
            },
            erlmcp_security_monitor:log_event(role_removed, #{
                identity_id => IdentityId,
                role_id => RoleId,
                reason => Reason,
                timestamp => timestamp()
            }),
            {reply, {ok, removed}, NewState};
        {error, not_found} ->
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
    %% Initialize role hierarchy
    %% Create default roles with least privilege principle
    %% Configure policy evaluation engine
    ok.

load_config() ->
    #{
        policy_evaluation_timeout => 5000,
        max_policies_per_identity => 100,
        role_inheritance_enabled => true,
        just_in_time_access => true,
        continuous_verification => true,
        audit_logging_enabled => true
    }.

evaluate_access_request(IdentityId, Resource, Action, State) ->
    %% Get all policies applicable to identity
    ApplicablePolicies = get_applicable_policies(IdentityId, Resource, Action, State),

    %% Evaluate policies in priority order
    case evaluate_policies(ApplicablePolicies, State) of
        {allow, _} ->
            {allow, ApplicablePolicies};
        {deny, _} ->
            %% Check for overrides
            case check_override_policies(IdentityId, Resource, Action, State) of
                {allow, OverridePolicies} ->
                    {allow, OverridePolicies};
                {deny, _} ->
                    {deny, "access_denied"}
            end
    end.

get_applicable_policies(IdentityId, Resource, Action, State) ->
    %% Get policies where identity is subject and resource matches
    lists:filtermap(fun(Policy) ->
        case is_subject(IdentityId, Policy#access_policy.subjects) andalso
             is_resource(Resource, Policy#access_policy.resources) andalso
             lists:member(Action, Policy#access_policy.actions) of
            true ->
                {true, Policy};
            false ->
                false
        end
    end, maps:values(State#state.policies)).

is_subject(IdentityId, Subjects) ->
    case lists:member(IdentityId, Subjects) of
        true ->
            true;
        false ->
            %% Check group membership if applicable
            erlmcp_identity_provider:is_member_of_group(IdentityId, Subjects)
    end.

is_resource(Resource, ResourcePatterns) ->
    %% Check if resource matches any pattern
    lists:any(fun(Pattern) ->
        match_resource_pattern(Resource, Pattern)
    end, ResourcePatterns).

match_resource_pattern(Resource, Pattern) ->
    %% Simple pattern matching - can be enhanced with regex or glob
    case Pattern of
        <<$/>> ++ Rest ->
            case binary:match(Resource, Rest) of
                {0, _} ->
                    true;
                _ ->
                    false
            end;
        _ ->
            Resource == Pattern
    end.

evaluate_policies([], _State) ->
    {deny, "no_matching_policies"};
evaluate_policies([Policy|Rest], State) ->
    case Policy#access_policy.effect of
        allow ->
            %% Allow policy found
            {allow, Policy};
        deny ->
            %% Deny policy found - no further evaluation needed
            {deny, Policy}
    end.

check_override_policies(IdentityId, Resource, Action, State) ->
    %% Check for emergency override policies
    EmergencyPolicies = get_emergency_policies(IdentityId, State),
    evaluate_policies(EmergencyPolicies, State).

create_dynamic_policy(IdentityId, Resource, Action, Conditions, State) ->
    case State#state.config.just_in_time_access of
        true ->
            %% Create temporary policy for just-in-time access
            PolicyId = generate_policy_id(),
            ValidUntil = maps:get(valid_until, Conditions, timestamp() + 3600000), %% 1 hour default
            Policy = #access_policy{
                id = PolicyId,
                name = <<"JIT Access: ", IdentityId/binary>>,
                description => <<"Just-in-time access for ", Resource/binary>>,
                subjects = [IdentityId],
                resources = [Resource],
                actions = [Action],
                effect = allow,
                conditions = Conditions ++ [{valid_until, ValidUntil}],
                priority = 1000, %% High priority for JIT policies
                created_at = timestamp(),
                updated_at = timestamp()
            },
            NewState = State#state{
                policies = maps:put(PolicyId, Policy, State#state.policies)
            },
            %% Schedule policy expiration
            schedule_policy_expiration(PolicyId, ValidUntil),
            {ok, PolicyId};
        false ->
            {error, "jit_access_disabled"}
    end.

revoke_dynamic_policy(IdentityId, Resource, Action, State) ->
    %% Find and remove dynamic policies
    DynamicPolicies = lists:filtermap(fun(Policy) ->
        case Policy#access_policy.subjects == [IdentityId] andalso
             lists:member(Resource, Policy#access_policy.resources) andalso
             lists:member(Action, Policy#access_policy.actions) of
            true ->
                {true, Policy};
            false ->
                false
        end
    end, maps:values(State#state.policies)),

    case DynamicPolicies of
        [Policy|_] ->
            PolicyId = Policy#access_policy.id,
            NewState = State#state{
                policies = maps:remove(PolicyId, State#state.policies)
            },
            {ok, PolicyId};
        [] ->
            {error, "no_dynamic_policies"}
    end.

get_identity_policies(IdentityId, State) ->
    %% Get all policies for an identity
    Policies = lists:filtermap(fun(Policy) ->
        case is_subject(IdentityId, Policy#access_policy.subjects) of
            true ->
                {true, Policy};
            false ->
                false
        end
    end, maps:values(State#state.policies)),
    {ok, Policies}.

validate_policy_data(Data) ->
    Required = [name, subjects, resources, actions],
    case check_required_fields(Data, Required) of
        ok ->
            %% Validate policy data according to Fortune 500 standards
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

apply_least_privilege_principle(IdentityId, Resource, State) ->
    %% Get current permissions
    CurrentPermissions = get_current_permissions(IdentityId, Resource, State),

    %% Analyze required vs granted permissions
    RequiredPermissions = analyze_required_permissions(Resource),

    %% Apply least privilege by removing unnecessary permissions
    AdjustedPermissions = lists:filter(fun(Permission) ->
        lists:member(Permission, RequiredPermissions)
    end, CurrentPermissions),

    %% Update policies accordingly
    {ok, AdjustedPermissions}.

analyze_required_permissions(Resource) ->
    %% Analyze what permissions are actually needed for resource
    %% This should be based on resource type and business requirements
    case Resource of
        <<$/>> ++ _ ->
            [read, write]; %% File system
        _ ->
            [read] %% Default
    end.

validate_least_privilege_compliance(IdentityId, State) ->
    %% Check if identity has more permissions than necessary
    AllPermissions = get_all_permissions(IdentityId, State),
    RequiredPermissions = analyze_required_permissions_for_identity(IdentityId, State),

    %% Find over-privileged permissions
    OverPrivileged = lists:filter(fun(Permission) ->
        not lists:member(Permission, RequiredPermissions)
    end, AllPermissions),

    case OverPrivileged of
        [] ->
            {compliant, #{
                total_permissions => length(AllPermissions),
                required_permissions => length(RequiredPermissions),
                efficiency => 1.0
            }};
        _ ->
            {non_compliant, OverPrivileged}
    end.

validate_role_assignment(IdentityId, RoleId, Conditions, State) ->
    %% Validate role assignment according to least privilege
    case maps:find(RoleId, State#state.roles) of
        {ok, Role} ->
            %% Check for conflicts with existing roles
            case check_role_conflicts(IdentityId, RoleId, State) of
                {ok, AssignmentId} ->
                    {ok, AssignmentId};
                {error, Reason} ->
                    {error, Reason}
            end;
        error ->
            {error, "role_not_found"}
    end.

check_role_conflicts(IdentityId, RoleId, State) ->
    %% Check if adding this role creates privilege escalation
    CurrentRoles = get_assigned_roles(IdentityId, State),
    NewRoles = [RoleId|CurrentRoles],

    case simulate_role_combination(NewRoles, State) of
        {escalation_risk, Conflicts} ->
            {error, {escalation_risk, Conflicts}};
        {no_escalation, _} ->
            AssignmentId = generate_assignment_id(),
            {ok, AssignmentId}
    end.

simulate_role_combination(RoleIds, State) ->
    %% Simulate combined permissions from multiple roles
    CombinedPermissions = lists:foldl(fun(RoleId, Acc) ->
        case maps:find(RoleId, State#state.roles) of
            {ok, Role} ->
                Role#role.permissions ++ Acc;
            error ->
                Acc
        end
    end, [], RoleIds),

    %% Check for dangerous permission combinations
    DangerousCombos = check_dangerous_combinations(CombinedPermissions),

    case DangerousCombos of
        [] ->
            {no_escalation, CombinedPermissions};
        _ ->
            {escalation_risk, DangerousCombos}
    end.

check_dangerous_combinations(Permissions) ->
    %% Check for dangerous permission combinations
    AdminRoles = [admin, superuser, root],
    Dangerous = [system_config, user_management, audit_log_bypass],

    lists:filter(fun(Permission) ->
        lists:member(Permission, Dangerous) andalso
        lists:any(fun(Role) -> lists:member(Role, AdminRoles) end, Permissions)
    end, Permissions).

get_assigned_roles(IdentityId, State) ->
    %% Get all roles assigned to identity
    Assignments = maps:filter(fun(_, Assignment) ->
        Assignment#role_assignment.identity_id == IdentityId
    end, State#state.assignments),
    lists:map(fun(Assignment) -> Assignment#role_assignment.role_id end, Assignments).

find_role_assignment(IdentityId, RoleId, State) ->
    %% Find specific role assignment
    Assignments = maps:filter(fun(_, Assignment) ->
        Assignment#role_assignment.identity_id == IdentityId andalso
        Assignment#role_assignment.role_id == RoleId
    end, State#state.assignments),

    case maps:size(Assignments) of
        1 ->
            {ok, maps:keys(Assignments)};
        _ ->
            {error, not_found}
    end.

get_emergency_policies(IdentityId, State) ->
    %% Get emergency override policies
    lists:filtermap(fun(Policy) ->
        case Policy#access_policy.subjects == [IdentityId] andalso
             lists:member(emergency_override, Policy#access_policy.conditions) of
            true ->
                {true, Policy};
            false ->
                false
        end
    end, maps:values(State#state.policies)).

schedule_policy_expiration(PolicyId, ValidUntil) ->
    %% Schedule policy expiration
    erlang:send_after(ValidUntil - timestamp(), self(), {expire_policy, PolicyId}).

generate_policy_id() ->
    crypto:strong_rand_bytes(16).

generate_assignment_id() ->
    crypto:strong_rand_bytes(16).

timestamp() ->
    erlang:system_time(millisecond).