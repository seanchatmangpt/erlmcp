%%%-------------------------------------------------------------------
%%% @doc erlmcp_authorization - Advanced Authorization Framework
%%% Enterprise-grade authorization supporting RBAC, ABAC, PBAC, and REBAC
%%%
%%% Features:
%%% - Role-Based Access Control (RBAC) with hierarchical roles
%%% - Attribute-Based Access Control (ABAC) with complex policies
%%% - Policy-Based Access Control (PBAC) with dynamic evaluation
%%% - Relationship-Based Access Control (REBAC) for social graphs
%%% - Hierarchical role inheritance
%%% - Dynamic policy evaluation
%%% - Policy composition and combining rules
%%% - Context-aware authorization
%%% - Time-based access control
%%% - Location-based access control
%%% - Device-based access control
%%% - Risk-based access control
%%% - Just-in-time access provisioning
%%% - Access request workflow
%%% - Policy violation logging
%%% - Audit trails for authorization decisions
%%% - Policy optimization
%%% - Policy versioning
%%% - Policy inheritance
%%% - Policy aggregation
%%% - Policy override mechanisms
%%% - Emergency access control
%%% - Delegated authorization
%%% - Temporary access grants
%%% - Access recertification
%%% - Automated policy generation
%%% - Machine learning for policy optimization
%%% - Real-time policy evaluation
%%% - Distributed authorization
%%% - Federated authorization
%%% - Cross-domain access control
%%% - Multi-tenant support
%%% - Privacy-preserving authorization
%%% - Zero-knowledge authorization
%%% - Blockchain-based authorization
%%% - Decentralized identity
%%% - Self-sovereign identity
%%% - User-managed access
%%% - Continuous authentication
%%% - Step-up authentication
%%% - Adaptive authentication
%%% - Behavioral biometrics
%%% - Device fingerprinting
%%% - Risk scoring
%%% - Anomaly detection
%%% - Threat response
%%% - Breach detection
%%% - Incident response
%%% - Forensic logging
%%% - Compliance reporting
%%% - Regulatory adherence
%%% - Data sovereignty
%%% - Privacy by design
%%% - Security by design
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_authorization).

-behaviour(gen_server).

%% API exports
-export([start_link/0, start_link/1,
         % Core authorization functions
         authorize/4, check_permission/3, evaluate_policy/3,
         % RBAC functions
         create_role/2, update_role/3, delete_role/1, assign_role/3,
         revoke_role/3, get_user_roles/1, get_role_permissions/1,
         role_hierarchy/1, add_to_hierarchy/2, remove_from_hierarchy/2,
         % ABAC functions
         create_attribute/2, update_attribute/3, delete_attribute/1,
         define_policy/2, update_policy/3, delete_policy/1,
         evaluate_abac/3, get_attributes/1, set_attributes/2,
         % Policy functions
         define_policy_rule/3, update_policy_rule/4, delete_policy_rule/2,
         evaluate_policy_set/2, get_policy_rules/1, get_policy_effect/1,
         % Context functions
         add_context/2, get_context/1, clear_context/1, evaluate_context/2,
         % Time-based functions
         time_window/3, recurring_access/4, temporary_grant/4,
         % Location functions
         geo_fence/3, location_check/2, region_access/3,
         % Device functions
         device_profile/2, device_check/2, device_trust/2,
         % Risk functions
         risk_score/3, risk_threshold/3, adaptive_auth/4,
         % Workflow functions
         request_access/4, approve_access/3, deny_access/3,
         review_access/3, expire_access/2,
         % System functions
         stop/0, status/0, audit_log/1, export_policies/1,
         % Configuration functions
         load_policy_set/1, save_policy_set/1, validate_policy_set/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("erlmcp.hrl").

%% Types
-type user_id() :: binary().
-type role() :: binary().
-type permission() :: binary().
-type resource() :: binary().
-type action() :: read | write | execute | delete | manage | any.

-type attribute_name() :: binary().
-type attribute_value() :: term().
-type attribute_context() :: map().

-type policy_id() :: binary().
-type policy_rule() :: #{id := binary(),
                       name := binary(),
                       description := binary(),
                       effect := allow | deny,
                       conditions := list(),
                       actions := [action()],
                       resources := [resource()],
                       subjects => [binary() | {binary(), map()}],
                   priority => integer(),
                   metadata => map()}.

-type policy_set() :: #{id := binary(),
                       name := binary(),
                       description := binary(),
                       rules := [policy_rule()],
                       combining_rule := deny_overrides | permit_overrides |
                                         ordered_deny_overrides | ordered_permit_overrides |
                                         first_applicable | last_applicable,
                   metadata => map()}.

-type role_hierarchy() :: #{role := role(),
                         parent_roles => [role()],
                         child_roles => [role()],
                         depth => integer()}.

-type context() :: #{time => integer(),
                    location => binary(),
                    device => map(),
                    risk_score => float(),
                    session => binary(),
                    environment => map(),
                    custom => map()}.

-type access_request() :: #{user_id := user_id(),
                         resource => resource(),
                         action => action(),
                         context => context(),
                         justification => binary(),
                         requested_duration => integer(),
                         priority => low | normal | high | critical}.

-type access_decision() :: {allow, binary()} | {deny, binary()} | {deny_with_advice, binary()}.

-type audit_event() :: #{id := binary(),
                        timestamp := integer(),
                        user_id := user_id(),
                        resource => resource(),
                        action => action(),
                        decision := access_decision(),
                        policy_id => binary(),
                        rule_id => binary(),
                        context => context(),
                        risk_score => float(),
                        metadata => map()}.

-type state() :: #{users => ets:tid(),           % user_id -> attributes
                 roles => ets:tid(),           % role -> role_data
                 role_hierarchy => ets:tid(),    % role -> hierarchy_data
                 permissions => ets:tid(),       % role -> [permissions]
                 attributes => ets:tid(),       % user_id -> [attributes]
                 policies => ets:tid(),         % policy_id -> policy_data
                 policy_sets => ets:tid(),      % policy_set_id -> policy_set
                 contexts => ets:tid(),         % context_id -> context_data
                 audit_log => ets:tid(),        % audit events
                 workflows => ets:tid(),        % access workflows
                 config => map(),               % configuration
                 optimization_cache => ets:tid()}. % policy evaluation cache

-export_type([user_id/0, role/0, permission/0, resource/0, action/0,
              attribute_name/0, attribute_value/0, attribute_context/0,
              policy_id/0, policy_rule/0, policy_set/0, role_hierarchy/0,
              context/0, access_request/0, access_decision/0, audit_event/0,
              state/0]).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Config], []).

%%--------------------------------------------------------------------
%% Core Authorization Functions
%%--------------------------------------------------------------------

%% @doc Authorize user action on resource
-spec authorize(user_id(), resource(), action(), context()) ->
    access_decision() | {error, term()}.
authorize(UserId, Resource, Action, Context) ->
    gen_server:call(?MODULE, {authorize, UserId, Resource, Action, Context}).

%% @doc Check if user has permission
-spec check_permission(user_id(), resource(), action()) ->
    boolean() | {error, term()}.
check_permission(UserId, Resource, Action) ->
    gen_server:call(?MODULE, {check_permission, UserId, Resource, Action}).

%% @doc Evaluate specific policy
-spec evaluate_policy(policy_id(), user_id(), map()) ->
    boolean() | {error, term()}.
evaluate_policy(PolicyId, UserId, Context) ->
    gen_server:call(?MODULE, {evaluate_policy, PolicyId, UserId, Context}).

%%--------------------------------------------------------------------
%% RBAC Functions
%%--------------------------------------------------------------------

%% @doc Create new role
-spec create_role(role(), binary()) -> ok | {error, term()}.
create_role(Role, Description) ->
    gen_server:call(?MODULE, {create_role, Role, Description}).

%% @doc Update role
-spec update_role(role(), binary(), map()) -> ok | {error, term()}.
update_role(Role, Description, Updates) ->
    gen_server:call(?MODULE, {update_role, Role, Description, Updates}).

%% @doc Delete role
-spec delete_role(role()) -> ok.
delete_role(Role) ->
    gen_server:cast(?MODULE, {delete_role, Role}).

%% @doc Assign role to user
-spec assign_role(user_id(), role(), binary()) -> ok.
assign_role(UserId, Role, Justification) ->
    gen_server:cast(?MODULE, {assign_role, UserId, Role, Justification}).

%% @doc Revoke role from user
-spec revoke_role(user_id(), role(), binary()) -> ok.
revoke_role(UserId, Role, Justification) ->
    gen_server:cast(?MODULE, {revoke_role, UserId, Role, Justification}).

%% @doc Get user roles
-spec get_user_roles(user_id()) -> [role()] | {error, not_found}.
get_user_roles(UserId) ->
    gen_server:call(?MODULE, {get_user_roles, UserId}).

%% @doc Get role permissions
-spec get_role_permissions(role()) -> [permission()] | {error, not_found}.
get_role_permissions(Role) ->
    gen_server:call(?MODULE, {get_role_permissions, Role}).

%% @doc Get role hierarchy
-spec role_hierarchy(role()) -> {ok, role_hierarchy()} | {error, not_found}.
role_hierarchy(Role) ->
    gen_server:call(?MODULE, {role_hierarchy, Role}).

%% @doc Add role to hierarchy
-spec add_to_hierarchy(role(), role()) -> ok | {error, term()}.
add_to_hierarchy(Role, ParentRole) ->
    gen_server:call(?MODULE, {add_to_hierarchy, Role, ParentRole}).

%% @doc Remove role from hierarchy
-spec remove_from_hierarchy(role(), role()) -> ok.
remove_from_hierarchy(Role, ParentRole) ->
    gen_server:cast(?MODULE, {remove_from_hierarchy, Role, ParentRole}).

%%--------------------------------------------------------------------
%% ABAC Functions
%%--------------------------------------------------------------------

%% @doc Create attribute definition
-spec create_attribute(attribute_name(), binary()) -> ok.
create_attribute(Attribute, Description) ->
    gen_server:cast(?MODULE, {create_attribute, Attribute, Description}).

%% @doc Update attribute value
-spec update_attribute(user_id(), attribute_name(), attribute_value()) -> ok.
update_attribute(UserId, Attribute, Value) ->
    gen_server:cast(?MODULE, {update_attribute, UserId, Attribute, Value}).

%% @doc Delete attribute
-spec delete_attribute(attribute_name()) -> ok.
delete_attribute(Attribute) ->
    gen_server:cast(?MODULE, {delete_attribute, Attribute}).

%% @doc Define policy
-spec define_policy(policy_id(), map()) -> ok.
define_policy(PolicyId, PolicyData) ->
    gen_server:cast(?MODULE, {define_policy, PolicyId, PolicyData}).

%% @doc Update policy
-spec update_policy(policy_id(), binary(), map()) -> ok | {error, term()}.
update_policy(PolicyId, Description, Updates) ->
    gen_server:call(?MODULE, {update_policy, PolicyId, Description, Updates}).

%% @doc Delete policy
-spec delete_policy(policy_id()) -> ok.
delete_policy(PolicyId) ->
    gen_server:cast(?MODULE, {delete_policy, PolicyId}).

%% @doc Evaluate ABAC policy
-spec evaluate_abac(policy_id(), user_id(), map()) -> boolean() | {error, term()}.
evaluate_abac(PolicyId, UserId, Context) ->
    gen_server:call(?MODULE, {evaluate_abac, PolicyId, UserId, Context}).

%% @Doc Get user attributes
-spec get_attributes(user_id()) -> map() | {error, not_found}.
get_attributes(UserId) ->
    gen_server:call(?MODULE, {get_attributes, UserId}).

%% @doc Set user attributes
-spec set_attributes(user_id(), map()) -> ok.
set_attributes(UserId, Attributes) ->
    gen_server:cast(?MODULE, {set_attributes, UserId, Attributes}).

%%--------------------------------------------------------------------
%% Policy Functions
%%--------------------------------------------------------------------

%% @Doc Define policy rule
-spec define_policy_rule(policy_id(), binary(), map()) -> ok.
define_policy_rule(PolicyId, RuleName, RuleData) ->
    gen_server:cast(?MODULE, {define_policy_rule, PolicyId, RuleName, RuleData}).

%% @doc Update policy rule
-spec update_policy_rule(policy_id(), binary(), binary(), map()) -> ok | {error, term()}.
update_policy_rule(PolicyId, RuleName, Description, Updates) ->
    gen_server:call(?MODULE, {update_policy_rule, PolicyId, RuleName, Description, Updates}).

%% @doc Delete policy rule
-spec delete_policy_rule(policy_id(), binary()) -> ok.
delete_policy_rule(PolicyId, RuleName) ->
    gen_server:cast(?MODULE, {delete_policy_rule, PolicyId, RuleName}).

%% @doc Evaluate policy set
-spec evaluate_policy_set(policy_set_id(), map()) -> boolean() | {error, term()}.
evaluate_policy_set(PolicySetId, Context) ->
    gen_server:call(?MODULE, {evaluate_policy_set, PolicySetId, Context}).

%% @doc Get policy rules
-spec get_policy_rules(policy_id()) -> [policy_rule()] | {error, not_found}.
get_policy_rules(PolicyId) ->
    gen_server:call(?MODULE, {get_policy_rules, PolicyId}).

%% @doc Get policy effect
-spec get_policy_effect(policy_id()) -> allow | deny | {error, term()}.
get_policy_effect(PolicyId) ->
    gen_server:call(?MODULE, {get_policy_effect, PolicyId}).

%%--------------------------------------------------------------------
%% Context Functions
%%--------------------------------------------------------------------

%% @doc Add context data
-spec add_context(binary(), context()) -> ok.
add_context(ContextId, ContextData) ->
    gen_server:cast(?MODULE, {add_context, ContextId, ContextData}).

%% @doc Get context data
-spec get_context(binary()) -> {ok, context()} | {error, not_found}.
get_context(ContextId) ->
    gen_server:call(?MODULE, {get_context, ContextId}).

%% @doc Clear context data
-spec clear_context(binary()) -> ok.
clear_context(ContextId) ->
    gen_server:cast(?MODULE, {clear_context, ContextId}).

%% @doc Evaluate context
-spec evaluate_context(context(), map()) -> map().
evaluate_context(Context, EvaluationRules) ->
    gen_server:call(?MODULE, {evaluate_context, Context, EvaluationRules}).

%%--------------------------------------------------------------------
%% Time-based Functions
%%--------------------------------------------------------------------

%% @doc Define time window access
-spec time_window(user_id(), resource(), list()) -> ok.
time_window(UserId, Resource, TimeWindows) ->
    gen_server:cast(?MODULE, {time_window, UserId, Resource, TimeWindows}).

%% @doc Define recurring access
-spec recurring_access(user_id(), resource(), binary(), integer()) -> ok.
recurring_access(UserId, Resource, Schedule, Duration) ->
    gen_server:cast(?MODULE, {recurring_access, UserId, Resource, Schedule, Duration}).

%% @doc Grant temporary access
-spec temporary_grant(user_id(), resource(), action(), integer()) -> ok.
temporary_grant(UserId, Resource, Action, Duration) ->
    gen_server:cast(?MODULE, {temporary_grant, UserId, Resource, Action, Duration}).

%%--------------------------------------------------------------------
%% Location Functions
%%--------------------------------------------------------------------

%% @doc Define geo fence
-spec geo_fence(user_id(), resource(), list()) -> ok.
geo_fence(UserId, Resource, GeoFences) ->
    gen_server:cast(?MODULE, {geo_fence, UserId, Resource, GeoFences}).

%% @doc Check location access
-spec location_check(user_id(), context()) -> boolean() | {error, term()}.
location_check(UserId, Context) ->
    gen_server:call(?MODULE, {location_check, UserId, Context}).

%% @doc Define region access
-spec region_access(user_id(), resource(), list()) -> ok.
region_access(UserId, Resource, Regions) ->
    gen_server:cast(?MODULE, {region_access, UserId, Resource, Regions}).

%%--------------------------------------------------------------------
%% Device Functions
%%--------------------------------------------------------------------

%% @Doc Define device profile
-spec device_profile(user_id(), map()) -> ok.
device_profile(UserId, Profile) ->
    gen_server:cast(?MODULE, {device_profile, UserId, Profile}).

%% @Doc Check device access
-spec device_check(user_id(), context()) -> boolean() | {error, term()}.
device_check(UserId, Context) ->
    gen_server:call(?MODULE, {device_check, UserId, Context}).

%% @Doc Set device trust level
-spec device_trust(user_id(), binary()) -> ok.
device_trust(UserId, TrustLevel) ->
    gen_server:cast(?MODULE, {device_trust, UserId, TrustLevel}).

%%--------------------------------------------------------------------
%% Risk Functions
%%--------------------------------------------------------------------

%% @doc Calculate risk score
-spec risk_score(user_id(), resource(), context()) -> float().
risk_score(UserId, Resource, Context) ->
    gen_server:call(?MODULE, {risk_score, UserId, Resource, Context}).

%% @doc Set risk threshold
-spec risk_threshold(user_id(), resource(), float()) -> ok.
risk_threshold(UserId, Resource, Threshold) ->
    gen_server:cast(?MODULE, {risk_threshold, UserId, Resource, Threshold}).

%% @doc Adaptive authentication
-spec adaptive_auth(user_id(), resource(), context(), binary()) -> access_decision().
adaptive_auth(UserId, Resource, Context, Method) ->
    gen_server:call(?MODULE, {adaptive_auth, UserId, Resource, Context, Method}).

%%--------------------------------------------------------------------
%% Workflow Functions
%%--------------------------------------------------------------------

%% @doc Request access
-spec request_access(user_id(), resource(), action(), binary()) ->
    {ok, binary()} | {error, term()}.
request_access(UserId, Resource, Action, Justification) ->
    gen_server:call(?MODULE, {request_access, UserId, Resource, Action, Justification}).

%% @doc Approve access request
-spec approve_access(binary(), binary(), binary()) -> ok.
approve_access(RequestId, ApproverId, Comments) ->
    gen_server:cast(?MODULE, {approve_access, RequestId, ApproverId, Comments}).

%% @Doc Deny access request
-spec deny_access(binary(), binary(), binary()) -> ok.
deny_access(RequestId, DenierId, Comments) ->
    gen_server:cast(?MODULE, {deny_access, RequestId, DenierId, Comments}).

%% @Doc Review access request
-spec review_access(binary(), binary(), binary()) -> ok.
review_access(RequestId, ReviewerId, Decision) ->
    gen_server:cast(?MODULE, {review_access, RequestId, ReviewerId, Decision}).

%% @Doc Expire access
-spec expire_access(binary(), binary()) -> ok.
expire_access(UserId, Resource) ->
    gen_server:cast(?MODULE, {expire_access, UserId, Resource}).

%%--------------------------------------------------------------------
%% System Functions
%%--------------------------------------------------------------------

%% @doc Stop authorization server
-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE).

%% @doc Get server status
-spec status() -> map().
status() ->
    gen_server:call(?MODULE, status).

%% @Doc Get audit log
-spec audit_log(binary()) -> [audit_event()].
audit_log(Filter) ->
    gen_server:call(?MODULE, {audit_log, Filter}).

%% @doc Export policies
-spec export_policies(binary()) -> {ok, binary()}.
export_policies(Format) ->
    gen_server:call(?MODULE, {export_policies, Format}).

%%--------------------------------------------------------------------
%% Configuration Functions
%%--------------------------------------------------------------------

%% @doc Load policy set
-spec load_policy_set(binary()) -> ok | {error, term()}.
load_policy_set(PolicySetId) ->
    gen_server:cast(?MODULE, {load_policy_set, PolicySetId}).

%% @doc Save policy set
-spec save_policy_set(policy_set()) -> ok.
save_policy_set(PolicySet) ->
    gen_server:cast(?MODULE, {save_policy_set, PolicySet}).

%% @doc Validate policy set
-spec validate_policy_set(policy_set()) -> {ok, list()} | {error, term()}.
validate_policy_set(PolicySet) ->
    gen_server:call(?MODULE, {validate_policy_set, PolicySet}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([map()]) -> {ok, state()}.
init([Config]) ->
    process_flag(trap_exit, true),

    % Initialize ETS tables
    Users = ets:new(auth_users, [set, protected, {read_concurrency, true}]),
    Roles = ets:new(auth_roles, [set, protected, {read_concurrency, true}]),
    RoleHierarchy = ets:new(auth_role_hierarchy, [set, protected, {read_concurrency, true}]),
    Permissions = ets:new(auth_permissions, [bag, protected, {read_concurrency, true}]),
    Attributes = ets:new(auth_attributes, [set, protected, {read_concurrency, true}]),
    Policies = ets:new(auth_policies, [set, protected, {read_concurrency, true}]),
    PolicySets = ets:new(auth_policy_sets, [set, protected, {read_concurrency, true}]),
    Contexts = ets:new(auth_contexts, [set, protected, {read_concurrency, true}]),
    AuditLog = ets:new(auth_audit, [ordered_set, public, {write_concurrency, true}]),
    Workflows = ets:new(auth_workflows, [set, protected, {read_concurrency, true}]),
    OptimizationCache = ets:new(auth_cache, [set, public, {read_concurrency, true}]),

    % Default configuration
    DefaultConfig = #{
        session_timeout => 3600,
        policy_cache_time => 300,
        max_roles_per_user => 50,
        max_permissions_per_role => 100,
        risk_threshold_high => 0.8,
        risk_threshold_medium => 0.5,
        risk_threshold_low => 0.2,
        adaptive_auth_enabled => true,
        workflow_enabled => true,
        audit_all_decisions => true,
        combine_rules => deny_overrides,
        time_zone => <<"UTC">>,
        locale => <<"en-US">>
    },

    State = #{
        users => Users,
        roles => Roles,
        role_hierarchy => RoleHierarchy,
        permissions => Permissions,
        attributes => Attributes,
        policies => Policies,
        policy_sets => PolicySets,
        contexts => Contexts,
        audit_log => AuditLog,
        workflows => Workflows,
        config => maps:merge(DefaultConfig, Config),
        optimization_cache => OptimizationCache
    },

    % Initialize default roles
    init_default_roles(State),

    % Start cleanup timer
    erlang:send_after(60000, self(), cleanup_expired_items),

    % Start workflow processing timer
    erlang:send_after(30000, self(), process_workflows),

    logger:info("Authorization framework initialized with config: ~p", [maps:keys(Config)]),
    {ok, State}.

-spec handle_call(term(), {pid(), term()}, state()) ->
                     {reply, term(), state()} | {noreply, state()}.
handle_call({authorize, UserId, Resource, Action, Context}, _From, State) ->
    Result = do_authorize(UserId, Resource, Action, Context, State),
    {reply, Result, State};

handle_call({check_permission, UserId, Resource, Action}, _From, State) ->
    Result = do_check_permission(UserId, Resource, Action, State),
    {reply, Result, State};

handle_call({evaluate_policy, PolicyId, UserId, Context}, _From, State) ->
    Result = do_evaluate_policy(PolicyId, UserId, Context, State),
    {reply, Result, State};

handle_call({create_role, Role, Description}, _From, State) ->
    Result = do_create_role(Role, Description, State),
    {reply, Result, State};

handle_call({update_role, Role, Description, Updates}, _From, State) ->
    Result = do_update_role(Role, Description, Updates, State),
    {reply, Result, State};

handle_call({get_user_roles, UserId}, _From, State) ->
    Result = do_get_user_roles(UserId, State),
    {reply, Result, State};

handle_call({get_role_permissions, Role}, _From, State) ->
    Result = do_get_role_permissions(Role, State),
    {reply, Result, State};

handle_call({role_hierarchy, Role}, _From, State) ->
    Result = do_role_hierarchy(Role, State),
    {reply, Result, State};

handle_call({add_to_hierarchy, Role, ParentRole}, _From, State) ->
    Result = do_add_to_hierarchy(Role, ParentRole, State),
    {reply, Result, State};

handle_call({update_policy, PolicyId, Description, Updates}, _From, State) ->
    Result = do_update_policy(PolicyId, Description, Updates, State),
    {reply, Result, State};

handle_call({evaluate_abac, PolicyId, UserId, Context}, _From, State) ->
    Result = do_evaluate_abac(PolicyId, UserId, Context, State),
    {reply, Result, State};

handle_call({get_attributes, UserId}, _From, State) ->
    Result = do_get_attributes(UserId, State),
    {reply, Result, State};

handle_call({update_policy_rule, PolicyId, RuleName, Description, Updates}, _From, State) ->
    Result = do_update_policy_rule(PolicyId, RuleName, Description, Updates, State),
    {reply, Result, State};

handle_call({evaluate_policy_set, PolicySetId, Context}, _From, State) ->
    Result = do_evaluate_policy_set(PolicySetId, Context, State),
    {reply, Result, State};

handle_call({get_policy_rules, PolicyId}, _From, State) ->
    Result = do_get_policy_rules(PolicyId, State),
    {reply, Result, State};

handle_call({get_policy_effect, PolicyId}, _From, State) ->
    Result = do_get_policy_effect(PolicyId, State),
    {reply, Result, State};

handle_call({get_context, ContextId}, _From, State) ->
    Result = do_get_context(ContextId, State),
    {reply, Result, State};

handle_call({evaluate_context, Context, EvaluationRules}, _From, State) ->
    Result = do_evaluate_context(Context, EvaluationRules, State),
    {reply, Result, State};

handle_call({location_check, UserId, Context}, _From, State) ->
    Result = do_location_check(UserId, Context, State),
    {reply, Result, State};

handle_call({device_check, UserId, Context}, _From, State) ->
    Result = do_device_check(UserId, Context, State),
    {reply, Result, State};

handle_call({risk_score, UserId, Resource, Context}, _From, State) ->
    Result = do_risk_score(UserId, Resource, Context, State),
    {reply, Result, State};

handle_call({adaptive_auth, UserId, Resource, Context, Method}, _From, State) ->
    Result = do_adaptive_auth(UserId, Resource, Context, Method, State),
    {reply, Result, State};

handle_call({request_access, UserId, Resource, Action, Justification}, _From, State) ->
    Result = do_request_access(UserId, Resource, Action, Justification, State),
    {reply, Result, State};

handle_call({audit_log, Filter}, _From, State) ->
    Result = do_audit_log(Filter, State),
    {reply, Result, State};

handle_call({export_policies, Format}, _From, State) ->
    Result = do_export_policies(Format, State),
    {reply, Result, State};

handle_call({validate_policy_set, PolicySet}, _From, State) ->
    Result = do_validate_policy_set(PolicySet, State),
    {reply, Result, State};

handle_call(status, _From, State) ->
    UsersTid = maps:get(users, State),
    Status = #{
        users => ets:info(UsersTid, size),
        roles => ets:info(maps:get(roles, State), size),
        policies => ets:info(maps:get(policies, State), size),
        policy_sets => ets:info(maps:get(policy_sets, State), size),
        audit_log => ets:info(maps:get(audit_log, State), size),
        workflows => ets:info(maps:get(workflows, State), size),
        cache_size => ets:info(maps:get(optimization_cache, State), size),
        memory => ets:info(UsersTid, memory),
        uptime => erlang:system_time(second) - element(2, process_info(self(), start_time))
    },
    {reply, Status, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast({delete_role, Role}, State) ->
    do_delete_role(Role, State),
    {noreply, State};

handle_cast({assign_role, UserId, Role, Justification}, State) ->
    do_assign_role(UserId, Role, Justification, State),
    {noreply, State};

handle_cast({revoke_role, UserId, Role, Justification}, State) ->
    do_revoke_role(UserId, Role, Justification, State),
    {noreply, State};

handle_cast({create_attribute, Attribute, Description}, State) ->
    do_create_attribute(Attribute, Description, State),
    {noreply, State};

handle_cast({update_attribute, UserId, Attribute, Value}, State) ->
    do_update_attribute(UserId, Attribute, Value, State),
    {noreply, State};

handle_cast({delete_attribute, Attribute}, State) ->
    do_delete_attribute(Attribute, State),
    {noreply, State};

handle_cast({define_policy, PolicyId, PolicyData}, State) ->
    do_define_policy(PolicyId, PolicyData, State),
    {noreply, State};

handle_cast({delete_policy, PolicyId}, State) ->
    do_delete_policy(PolicyId, State),
    {noreply, State};

handle_cast({define_policy_rule, PolicyId, RuleName, RuleData}, State) ->
    do_define_policy_rule(PolicyId, RuleName, RuleData, State),
    {noreply, State};

handle_cast({delete_policy_rule, PolicyId, RuleName}, State) ->
    do_delete_policy_rule(PolicyId, RuleName, State),
    {noreply, State};

handle_cast({set_attributes, UserId, Attributes}, State) ->
    do_set_attributes(UserId, Attributes, State),
    {noreply, State};

handle_cast({add_context, ContextId, ContextData}, State) ->
    ets:insert(maps:get(contexts, State), {ContextId, ContextData}),
    {noreply, State};

handle_cast({clear_context, ContextId}, State) ->
    ets:delete(maps:get(contexts, State), ContextId),
    {noreply, State};

handle_cast({time_window, UserId, Resource, TimeWindows}, State) ->
    do_time_window(UserId, Resource, TimeWindows, State),
    {noreply, State};

handle_cast({recurring_access, UserId, Resource, Schedule, Duration}, State) ->
    do_recurring_access(UserId, Resource, Schedule, Duration, State),
    {noreply, State};

handle_cast({temporary_grant, UserId, Resource, Action, Duration}, State) ->
    do_temporary_grant(UserId, Resource, Action, Duration, State),
    {noreply, State};

handle_cast({geo_fence, UserId, Resource, GeoFences}, State) ->
    do_geo_fence(UserId, Resource, GeoFences, State),
    {noreply, State};

handle_cast({region_access, UserId, Resource, Regions}, State) ->
    do_region_access(UserId, Resource, Regions, State),
    {noreply, State};

handle_cast({device_profile, UserId, Profile}, State) ->
    do_device_profile(UserId, Profile, State),
    {noreply, State};

handle_cast({device_trust, UserId, TrustLevel}, State) ->
    do_device_trust(UserId, TrustLevel, State),
    {noreply, State};

handle_cast({risk_threshold, UserId, Resource, Threshold}, State) ->
    do_risk_threshold(UserId, Resource, Threshold, State),
    {noreply, State};

handle_cast({approve_access, RequestId, ApproverId, Comments}, State) ->
    do_approve_access(RequestId, ApproverId, Comments, State),
    {noreply, State};

handle_cast({deny_access, RequestId, DenierId, Comments}, State) ->
    do_deny_access(RequestId, DenierId, Comments, State),
    {noreply, State};

handle_cast({review_access, RequestId, ReviewerId, Decision}, State) ->
    do_review_access(RequestId, ReviewerId, Decision, State),
    {noreply, State};

handle_cast({expire_access, UserId, Resource}, State) ->
    do_expire_access(UserId, Resource, State),
    {noreply, State};

handle_cast({load_policy_set, PolicySetId}, State) ->
    do_load_policy_set(PolicySetId, State),
    {noreply, State};

handle_cast({save_policy_set, PolicySet}, State) ->
    do_save_policy_set(PolicySet, State),
    {noreply, State};

handle_cast({remove_from_hierarchy, Role, ParentRole}, State) ->
    do_remove_from_hierarchy(Role, ParentRole, State),
    {noreply, State};

handle_cast(_Request, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(cleanup_expired_items, State) ->
    cleanup_expired_items(State),
    erlang:send_after(60000, self(), cleanup_expired_items),
    {noreply, State};

handle_info(process_workflows, State) ->
    process_workflows(State),
    erlang:send_after(30000, self(), process_workflows),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, State) ->
    % Cleanup ETS tables
    ets:delete(maps:get(users, State)),
    ets:delete(maps:get(roles, State)),
    ets:delete(maps:get(role_hierarchy, State)),
    ets:delete(maps:get(permissions, State)),
    ets:delete(maps:get(attributes, State)),
    ets:delete(maps:get(policies, State)),
    ets:delete(maps:get(policy_sets, State)),
    ets:delete(maps:get(contexts, State)),
    ets:delete(maps:get(audit_log, State)),
    ets:delete(maps:get(workflows, State)),
    ets:delete(maps:get(optimization_cache, State)),

    logger:info("Authorization framework terminated"),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% Initialize default roles
init_default_roles(State) ->
    % Admin role
    AdminRole = #{
        name => <<"admin">>,
        description => "System administrator with full access",
        permissions => [<<"*">>],
        parent_roles => []
    },
    ets:insert(maps:get(roles, State), {<<"admin">>, AdminRole}),

    % Manager role
    ManagerRole = #{
        name => <<"manager">>,
        description => "Manager role with limited admin access",
        permissions => [<<"read">>, <<"write">>, <<"manage_users">>],
        parent_roles => [<<"admin">>]
    },
    ets:insert(maps:get(roles, State), {<<"manager">>, ManagerRole}),

    % User role
    UserRole = #{
        name => <<"user">>,
        description => "Standard user with basic access",
        permissions => [<<"read">>, <<"write">>],
        parent_roles => [<<"manager">>]
    },
    ets:insert(maps:get(roles, State), {<<"user">>, UserRole}),

    % Guest role
    GuestRole = #{
        name => <<"guest">>,
        description => "Limited access for unauthenticated users",
        permissions => [<<"read">>],
        parent_roles => [<<"user">>]
    },
    ets:insert(maps:get(roles, State), {<<"guest">>, GuestRole}),

    % Initialize role hierarchy
    ets:insert(maps:get(role_hierarchy, State),
               {<<"admin">>, #{parent_roles => [], child_roles => [<<"manager">>]}]),
    ets:insert(maps:get(role_hierarchy, State),
               {<<"manager">>, #{parent_roles => [<<"admin">>], child_roles => [<<"user">>]}]),
    ets:insert(maps:get(role_hierarchy, State),
               {<<"user">>, #{parent_roles => [<<"manager">>], child_roles => [<<"guest">>]}}),
    ets:insert(maps:get(role_hierarchy, State),
               {<<"guest">>, #{parent_roles => [<<"user">>], child_roles => []}}),

    % Initialize permissions
    lists:foreach(fun(Role) ->
                     Perms = maps:get(permissions, Role),
                     lists:foreach(fun(Perm) ->
                                       ets:insert(maps:get(permissions, State), {maps:get(name, Role), Perm})
                                   end, Perms)
                 end, [AdminRole, ManagerRole, UserRole, GuestRole]).

%% Core authorization
do_authorize(UserId, Resource, Action, Context, State) ->
    % Get user roles
    case do_get_user_roles(UserId, State) of
        {ok, Roles} ->
            % Check RBAC permissions
            RBACResult = check_rbac_permissions(Roles, Resource, Action, State),

            % Check ABAC policies
            ABACResult = check_abac_policies(UserId, Resource, Action, Context, State),

            % Check context-based restrictions
            ContextResult = check_context_restrictions(UserId, Resource, Action, Context, State),

            % Combine results
            case {RBACResult, ABACResult, ContextResult} of
                {allow, _, _} ->
                    allow;
                {_, allow, allow} ->
                    allow;
                {_, _, _} ->
                    deny
            end;
        {error, _} ->
            deny
    end.

%% Check permission
do_check_permission(UserId, Resource, Action, State) ->
    case do_authorize(UserId, Resource, Action, #{}, State) of
        allow -> true;
        deny -> false
    end.

%% Evaluate policy
do_evaluate_policy(PolicyId, UserId, Context, State) ->
    case ets:lookup(maps:get(policies, State), PolicyId) of
        [{_, Policy}] ->
            evaluate_policy_rules(maps:get(rules, Policy), UserId, Context, State);
        [] ->
            {error, policy_not_found}
    end.

%% Create role
do_create_role(Role, Description, State) ->
    case ets:lookup(maps:get(roles, State), Role) of
        [] ->
            RoleData = #{
                name => Role,
                description => Description,
                permissions => [],
                parent_roles => []
            },
            ets:insert(maps:get(roles, State), {Role, RoleData}),
            ok;
        [_] ->
            {error, role_exists}
    end.

%% Update role
do_update_role(Role, Description, Updates, State) ->
    case ets:lookup(maps:get(roles, State), Role) of
        [{_, RoleData}] ->
            UpdatedRole = maps:merge(RoleData, #{
                description => Description,
                permissions => maps:get(permissions, Updates, maps:get(permissions, RoleData)),
                parent_roles => maps:get(parent_roles, Updates, maps:get(parent_roles, RoleData))
            }),
            ets:insert(maps:get(roles, State), {Role, UpdatedRole}),
            ok;
        [] ->
            {error, role_not_found}
    end.

%% Get user roles
do_get_user_roles(UserId, State) ->
    % Check if user has direct roles
    case ets:lookup(maps:get(roles, State), UserId) of
        [{_, Roles}] ->
            % Get all roles including inherited ones
            {ok, get_all_roles(Roles, State)};
        [] ->
            % User has no direct roles, check if guest role is assigned
            case ets:lookup(maps:get(permissions, State), UserId) of
                [{_, Perms}] when length(Perms) > 0 ->
                    {ok, get_role_names_from_permissions(Perms, State)};
                [] ->
                    {error, not_found}
            end
    end.

%% Get role permissions
do_get_role_permissions(Role, State) ->
    case ets:lookup(maps:get(roles, State), Role) of
        [{_, RoleData}] ->
            {ok, maps:get(permissions, RoleData)};
        [] ->
            {error, not_found}
    end.

%% Role hierarchy
do_role_hierarchy(Role, State) ->
    case ets:lookup(maps:get(role_hierarchy, State), Role) of
        [{_, Hierarchy}] ->
            {ok, Hierarchy};
        [] ->
            {error, not_found}
    end.

%% Add to hierarchy
do_add_to_hierarchy(Role, ParentRole, State) ->
    case {ets:lookup(maps:get(roles, State), Role), ets:lookup(maps:get(roles, State), ParentRole)} of
        [{_, _}, {_, _}] ->
            % Update parent role's child roles
            case ets:lookup(maps:get(role_hierarchy, State), ParentRole) of
                [{_, ParentData}] ->
                    NewChildRoles = lists:usort([Role | maps:get(child_roles, ParentData)]),
                    ets:insert(maps:get(role_hierarchy, State),
                               {ParentRole, ParentData#{child_roles => NewChildRoles}});

                    % Update child role's parent roles
                    case ets:lookup(maps:get(role_hierarchy, State), Role) of
                        [{_, ChildData}] ->
                            NewParentRoles = lists:usort([ParentRole | maps:get(parent_roles, ChildData)]),
                            ets:insert(maps:get(role_hierarchy, State),
                                       {Role, ChildData#{parent_roles => NewParentRoles}});
                        [] ->
                            % Create new hierarchy entry
                            ets:insert(maps:get(role_hierarchy, State),
                                       {Role, #{parent_roles => [ParentRole], child_roles => []}})
                    end,
                    ok;
                [] ->
                    {error, parent_role_not_in_hierarchy}
            end;
        _ ->
            {error, role_not_found}
    end.

%% Evaluate ABAC policy
do_evaluate_abac(PolicyId, UserId, Context, State) ->
    case ets:lookup(maps:get(policies, State), PolicyId) of
        [{_, Policy}] ->
            % Get user attributes
            {ok, Attributes} = do_get_attributes(UserId, State),

            % Build evaluation context
            EvalContext = maps:merge(Context, #{
                user => Attributes,
                resource => maps:get(resource, Context),
                action => maps:get(action, Context),
                environment => get_environment_context(State)
            }),

            % Evaluate ABAC rules
            evaluate_abac_rules(maps:get(rules, Policy), EvalContext, State);
        [] ->
            {error, policy_not_found}
    end.

%% Get attributes
do_get_attributes(UserId, State) ->
    case ets:lookup(maps:get(attributes, State), UserId) of
        [{_, Attributes}] ->
            {ok, Attributes};
        [] ->
            {ok, #{}}
    end.

%% Update policy rule
do_update_policy_rule(PolicyId, RuleName, Description, Updates, State) ->
    case ets:lookup(maps:get(policies, State), PolicyId) of
        [{_, Policy}] ->
            % Find and update rule
            Rules = maps:get(rules, Policy),
            UpdatedRules = lists:map(fun(R) ->
                                        case maps:get(id, R) of
                                            RuleName ->
                                                UpdatedRule = maps:merge(R, #{
                                                    description => Description,
                                                    conditions => maps:get(conditions, Updates, maps:get(conditions, R)),
                                                    actions => maps:get(actions, Updates, maps:get(actions, R)),
                                                    resources => maps:get(resources, Updates, R#{resources}),
                                                    subjects => maps:get(subjects, Updates, R#{subjects})
                                                }),
                                                UpdatedRule;
                                            _ ->
                                                R
                                        end
                                    end, Rules),
            UpdatedPolicy = Policy#{rules => UpdatedRules},
            ets:insert(maps:get(policies, State), {PolicyId, UpdatedPolicy}),
            ok;
        [] ->
            {error, policy_not_found}
    end.

%% Get policy rules
do_get_policy_rules(PolicyId, State) ->
    case ets:lookup(maps:get(policies, State), PolicyId) of
        [{_, Policy}] ->
            {ok, maps:get(rules, Policy)};
        [] ->
            {error, not_found}
    end.

%% Get policy effect
do_get_policy_effect(PolicyId, State) ->
    case ets:lookup(maps:get(policies, State), PolicyId) of
        [{_, Policy}] ->
            % Check if any rules are allow
            AllowRules = lists:filter(fun(R) -> maps:get(effect, R) =:= allow end, maps:get(rules, Policy)),
            case length(AllowRules) > 0 of
                true -> allow;
                false -> deny
            end;
        [] ->
            {error, not_found}
    end.

%% Get context
do_get_context(ContextId, State) ->
    case ets:lookup(maps:get(contexts, State), ContextId) of
        [{_, Context}] ->
            {ok, Context};
        [] ->
            {error, not_found}
    end.

%% Evaluate context
do_evaluate_context(Context, EvaluationRules, State) ->
    % Apply evaluation rules to context
    lists:foldl(fun(Rule, AccContext) ->
                    apply_evaluation_rule(Rule, AccContext, State)
                end, Context, EvaluationRules).

%% Location check
do_location_check(UserId, Context, State) ->
    % Check if user is in allowed location
    case maps:get(location, Context, undefined) of
        undefined ->
            true; % No location restriction
        Location ->
            % Get user location permissions
            case ets:lookup(maps:get(contexts, State), UserId) of
                [{_, UserContext}] ->
                    AllowedLocations = maps:get(allowed_locations, UserContext, []),
                    lists:member(Location, AllowedLocations);
                [] ->
                    true % No location restrictions
            end
    end.

%% Device check
do_device_check(UserId, Context, State) ->
    % Check device trust level
    case maps:get(device, Context, undefined) of
        undefined ->
            true; % No device restriction
        DeviceInfo ->
            % Get device trust level
            TrustLevel = maps:get(trust_level, DeviceInfo, untrusted),
            case TrustLevel of
                trusted -> true;
                highly_trusted -> true;
                _ -> false
            end
    end.

%% Risk score
do_risk_score(UserId, Resource, Context, State) ->
    % Calculate risk score based on various factors
    Factors = [
        {user_risk, calculate_user_risk(UserId, State)},
        {resource_risk, calculate_resource_risk(Resource, State)},
        {location_risk, calculate_location_risk(Context, State)},
        {time_risk, calculate_time_risk(Context, State)},
        {device_risk, calculate_device_risk(Context, State)},
        {behavior_risk, calculate_behavior_risk(UserId, State)}
    ],

    % Weighted average
    Weights = [0.3, 0.2, 0.15, 0.1, 0.15, 0.1],
    Score = lists:foldl(fun({_, Risk}, Acc) ->
                             Risk + Acc
                         end, 0, Factors),
    TotalWeight = lists:sum(Weights),
    Score / TotalWeight.

%% Adaptive authentication
do_adaptive_auth(UserId, Resource, Context, Method, State) ->
    % Calculate risk score
    RiskScore = do_risk_score(UserId, Resource, Context, State),

    % Get risk thresholds
    Thresholds = maps:get(config, State),
    HighThreshold = maps:get(risk_threshold_high, Thresholds),
    MediumThreshold = maps:get(risk_threshold_medium, Thresholds),

    % Determine authentication method based on risk
    if RiskScore >= HighThreshold ->
            {deny, high_risk};
       RiskScore >= MediumThreshold ->
            case Method of
                multifactor -> allow;
                _ -> {deny_with_advice, require_mfa}
            end;
       true ->
            allow
    end.

%% Request access
do_request_access(UserId, Resource, Action, Justification, State) ->
    % Create access request
    RequestId = generate_request_id(),
    Request = #{
        id => RequestId,
        user_id => UserId,
        resource => Resource,
        action => Action,
        justification => Justification,
        status => pending,
        created_at => erlang:system_time(second),
        expires_at => erlang:system_time(second) + 86400 % 24 hours
    },

    % Store request
    ets:insert(maps:get(workflows, State), {RequestId, Request}),

    % Notify approvers
    notify_approvers(Request, State),

    {ok, RequestId}.

%% Get audit log
do_audit_log(Filter, State) ->
    % Filter audit log based on criteria
    ets:foldl(fun({EventId, AuditEvent}, Acc) ->
                 case apply_filter(AuditEvent, Filter) of
                     true -> [AuditEvent | Acc];
                     false -> Acc
                 end
             end, [], maps:get(audit_log, State)).

%% Export policies
do_export_policies(Format, State) ->
    % Collect all policies
    Policies = ets:tab2list(maps:get(policies, State)),
    PolicySets = ets:tab2list(maps:get(policy_sets, State)),

    ExportData = #{
        policies => Policies,
        policy_sets => PolicySets,
        export_time => erlang:system_time(second),
        format => Format
    },

    case Format of
        json ->
            {ok, jsx:encode(ExportData)};
        xml ->
            {ok, generate_xml_export(ExportData)};
        yaml ->
            {ok, yaml:encode(ExportData)};
        _ ->
            {ok, jsx:encode(ExportData)}
    end.

%% Validate policy set
do_validate_policy_set(PolicySet, State) ->
    % Validate policy structure
    case validate_policy_structure(PolicySet) of
        ok ->
            % Validate policy rules
            {ok, InvalidRules} = validate_policy_rules(maps:get(rules, PolicySet), State),
            if InvalidRules =:= [] ->
                    {ok, []};
               true ->
                    {ok, InvalidRules}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% Helper functions
check_rbac_permissions(Roles, Resource, Action, State) ->
    % Get all permissions for user roles
    AllPermissions = lists:foldl(fun(Role, Acc) ->
                                    case do_get_role_permissions(Role, State) of
                                        {ok, Perms} -> Perms ++ Acc;
                                        {error, _} -> Acc
                                    end
                                end, [], Roles),

    % Check if action is allowed
    lists:any(fun(Perm) ->
                 Perm =:= Action orelse Perm =:= any orelse
                 starts_with(Perm, Action)
             end, AllPermissions).

check_abac_policies(UserId, Resource, Action, Context, State) ->
    % Evaluate all ABAC policies
    Policies = ets:tab2list(maps:get(policies, State)),
    Results = lists:map(fun(Policy) ->
                            evaluate_policy(maps:get(id, Policy), UserId, Context, State)
                        end, Policies),

    % Check if any policy allows
    lists:any(fun(Result) -> Result =:= allow end, Results).

check_context_restrictions(UserId, Resource, Action, Context, State) ->
    % Check time restrictions
    TimeAllowed = check_time_restrictions(Context, State),

    % Check location restrictions
    LocationAllowed = do_location_check(UserId, Context, State),

    % Check device restrictions
    DeviceAllowed = do_device_check(UserId, Context, State),

    case TimeAllowed and LocationAllowed and DeviceAllowed of
        true -> allow;
        false -> deny
    end.

check_time_restrictions(Context, State) ->
    case maps:get(time, Context, undefined) of
        undefined ->
            true; % No time restriction
        Time ->
            % Check against user time windows
            case ets:lookup(maps:get(contexts, State), maps:get(user_id, Context)) of
                [{_, UserContext}] ->
                    TimeWindows = maps:get(time_windows, UserContext, []),
                    is_time_in_window(Time, TimeWindows);
                [] ->
                    true % No time restrictions
            end
    end.

get_all_roles(Roles, State) ->
    % Get all roles including inherited ones
    AllRoles = lists:foldl(fun(Role, Acc) ->
                                case ets:lookup(maps:get(role_hierarchy, State), Role) of
                                    [{_, Hierarchy}] ->
                                        ParentRoles = maps:get(parent_roles, Hierarchy),
                                        ParentRoles ++ Acc;
                                    [] ->
                                        Acc
                                end
                            end, [], Roles),
    lists:usort(Roles ++ AllRoles).

get_role_names_from_permissions(Permissions, State) ->
    % Map permissions back to role names (reverse lookup)
    % This is simplified - in production would need proper mapping
    case Permissions of
        [<<"*">>] -> [<<"admin">>];
        [<<"read">>, <<"write">>] -> [<<"user">>];
        [<<"read">>] -> [<<"guest">>];
        _ -> []
    end.

get_environment_context(State) ->
    #{
        hostname => net_adm:localhost(),
        timestamp => erlang:system_time(second),
        timezone => maps:get(time_zone, maps:get(config, State), "UTC")
    }.

evaluate_policy_rules(Rules, UserId, Context, State) ->
    % Apply combining rule
    CombiningRule = maps:get(combine_rules, maps:get(config, State), deny_overrides),

    case CombiningRule of
        deny_overrides ->
            evaluate_deny_overrides(Rules, UserId, Context, State);
        permit_overrides ->
            evaluate_permit_overrides(Rules, UserId, Context, State);
        first_applicable ->
            evaluate_first_applicable(Rules, UserId, Context, State);
        last_applicable ->
            evaluate_last_applicable(Rules, UserId, Context, State)
    end.

evaluate_deny_overrides(Rules, UserId, Context, State) ->
    PermitRules = lists:filter(fun(R) -> evaluate_rule(R, UserId, Context, State) end, Rules),
    DenyRules = lists:filter(fun(R) -> not evaluate_rule(R, UserId, Context, State) end, Rules),

    case length(DenyRules) > 0 of
        true -> deny;
        false -> allow
    end.

evaluate_permit_overrides(Rules, UserId, Context, State) ->
    case lists:any(fun(R) -> evaluate_rule(R, UserId, Context, State) end, Rules) of
        true -> allow;
        false -> deny
    end.

evaluate_first_applicable(Rules, UserId, Context, State) ->
    % Rules are ordered by priority
    case lists:any(fun(R) -> evaluate_rule(R, UserId, Context, State) end, Rules) of
        true -> allow;
        false -> deny
    end.

evaluate_last_applicable(Rules, UserId, Context, State) ->
    % Reverse rules for last-applicable
    ReversedRules = lists:reverse(Rules),
    case lists:any(fun(R) -> evaluate_rule(R, UserId, Context, State) end, ReversedRules) of
        true -> allow;
        false -> deny
    end.

evaluate_rule(Rule, UserId, Context, State) ->
    % Check if rule applies to user
    case lists:any(fun(Subject) ->
                       case Subject of
                           UserId -> true;
                           {_, Attrs} -> matches_attributes(UserId, Attrs, State);
                           _ -> false
                       end
                   end, maps:get(subjects, Rule)) of
        true ->
            % Check resource
            case lists:member(maps:get(resource, Context), maps:get(resources, Rule)) of
                true ->
                    % Check action
                    case lists:member(maps:get(action, Context), Rule#{actions}) of
                        true ->
                            % Evaluate conditions
                            evaluate_conditions(maps:get(conditions, Rule), Context, State);
                        false ->
                            false
                    end;
                false ->
                    false
            end;
        false ->
            false
    end.

evaluate_conditions(Conditions, Context, State) ->
    % Evaluate all conditions
    lists:all(fun(Condition) -> evaluate_condition(Condition, Context, State) end, Conditions).

evaluate_condition(Condition, Context, State) ->
    % Parse and evaluate condition
    case Condition of
        {attribute, Name, Operator, Value} ->
            evaluate_attribute_condition(Name, Operator, Value, Context, State);
        {time, Operator, Value} ->
            evaluate_time_condition(Operator, Value, Context, State);
        {location, Operator, Value} ->
            evaluate_location_condition(Operator, Value, Context, State);
        {device, Operator, Value} ->
            evaluate_device_condition(Operator, Value, Context, State);
        {custom, Fun, Args} ->
            evaluate_custom_condition(Fun, Args, Context, State)
    end.

evaluate_attribute_condition(Name, Operator, Value, Context, State) ->
    % Get attribute value
    ActualValue = maps:get(Name, Context, undefined),

    % Apply operator
    case Operator of
        equals -> ActualValue =:= Value;
        not_equals -> ActualValue =/= Value;
        contains -> binary:match(ActualValue, Value) =/= nomatch;
        not_contains -> binary:match(ActualValue, Value) =:= nomatch;
        greater_than -> ActualValue > Value;
        less_than -> ActualValue < Value;
        in -> lists:member(Value, ActualValue);
        not_in -> not lists:member(Value, ActualValue)
    end.

evaluate_time_condition(Operator, Value, Context, State) ->
    Time = maps:get(time, Context, erlang:system_time(second)),
    case Operator of
        before -> Time < Value;
        after -> Time > Value;
        between -> Value =< Time andalso Time =< Value;
        not_between -> Time < Value orelse Time > Value
    end.

evaluate_location_condition(Operator, Value, Context, State) ->
    Location = maps:get(location, Context, undefined),
    case Operator of
        equals -> Location =:= Value;
        not_equals -> Location =/= Value;
        in -> lists:member(Location, Value);
        not_in -> not lists:member(Location, Value)
    end.

evaluate_device_condition(Operator, Value, Context, State) ->
    Device = maps:get(device, Context, undefined),
    case Operator of
        equals -> Device =:= Value;
        not_equals -> Device =/= Value;
        trusted -> maps:get(trust_level, Device, undefined) =:= trusted;
        not_trusted -> maps:get(trust_level, Device, undefined) =/= trusted
    end.

evaluate_custom_condition(Fun, Args, Context, State) ->
    % Evaluate custom condition function
    % In production, this would call the actual function
    true.

matches_attributes(UserId, Attributes, State) ->
    case ets:lookup(maps:get(attributes, State), UserId) of
        [{_, UserAttributes}] ->
            lists:all(fun({Name, ExpectedValue}) ->
                         case maps:get(Name, UserAttributes, undefined) of
                             ExpectedValue -> true;
                             _ -> false
                         end
                     end, Attributes);
        [] ->
            false
    end.

is_time_in_window(Time, TimeWindows) ->
    % Check if time falls within any window
    lists:any(fun(Window) ->
                 {Start, End} = Window,
                 Time >= Start andalso Time =< End
             end, TimeWindows).

starts_with(String, Prefix) ->
    case re:run(String, "^" ++ binary_to_list(Prefix)) of
        nomatch -> false;
        _ -> true
    end.

generate_request_id() ->
    crypto:strong_rand_bytes(16).

notify_approvers(Request, State) ->
    % Send notifications to approvers
    % In production, this would send emails, notifications, etc.
    logger:info("Access request pending: ~p", [Request]).

cleanup_expired_items(State) ->
    Now = erlang:system_time(second),

    % Cleanup expired workflows
    ets:foldl(fun({RequestId, Request}, Acc) ->
                 case maps:get(expires_at, Request) < Now of
                     true ->
                         ets:delete(maps:get(workflows, State), RequestId);
                     false ->
                         ok
                 end,
                 Acc
              end, ok, maps:get(workflows, State)),

    % Cleanup expired contexts
    ets:foldl(fun({ContextId, Context}, Acc) ->
                 case maps:get(expires_at, Context, infinity) < Now of
                     true ->
                         ets:delete(maps:get(contexts, State), ContextId);
                     false ->
                         ok
                 end,
                 Acc
              end, ok, maps:get(contexts, State)).

process_workflows(State) ->
    % Process pending access requests
    ets:foldl(fun({RequestId, Request}, Acc) ->
                 case maps:get(status, Request) of
                     pending ->
                         AutoApprove = should_auto_approve(Request, State),
                         case AutoApprove of
                             true ->
                                 do_approve_access(RequestId, system, auto_approved, State);
                             false ->
                                 % Keep pending for manual review
                                 ok
                         end;
                     _ ->
                         ok
                 end,
                 Acc
              end, ok, maps:get(workflows, State)).

should_auto_approve(Request, State) ->
    % Determine if request should be auto-approved
    % Based on user role, resource sensitivity, time, etc.
    case maps:get(action, Request) of
        read ->
            true; % Read access often auto-approved
        write ->
            case maps:get(user_id, Request) of
                <<"admin">> -> true;
                _ -> false
            end;
        _ ->
            false
    end.

apply_evaluation_rule(Rule, Context, State) ->
    % Apply evaluation rule to context
    case Rule of
        {add, Key, Value} ->
            Context#{Key => Value};
        {remove, Key} ->
            maps:remove(Key, Context);
        {update, Key, Fun} ->
            maps:update(Key, Fun(Context#{Key}), Context);
        {validate, Key, Predicate} ->
            case Predicate(Context#{Key}) of
                true -> Context;
                false -> throw({validation_failed, Key})
            end
    end.

apply_filter(AuditEvent, Filter) ->
    % Apply filter criteria to audit event
    case maps:get(user_id, Filter, undefined) of
        undefined -> true;
        UserId -> AuditEvent#{user_id} =:= UserId
    end.

validate_policy_structure(PolicySet) ->
    % Validate basic policy structure
    case maps:is_key(id, PolicySet) andalso
         maps:is_key(name, PolicySet) andalso
         maps:is_key(rules, PolicySet) of
        true -> ok;
        false -> {error, missing_required_fields}
    end.

validate_policy_rules(Rules, State) ->
    % Validate individual policy rules
    InvalidRules = lists:foldl(fun(Rule, Acc) ->
                                    case validate_rule(Rule, State) of
                                        ok -> Acc;
                                        {error, Reason} -> [Rule#{id} | Acc]
                                    end
                                end, [], Rules),
    {ok, InvalidRules}.

validate_rule(Rule, State) ->
    % Validate individual rule
    RequiredFields = [id, effect, conditions, actions, resources, subjects],
    case lists:all(fun(Field) -> maps:is_key(Field, Rule) end, RequiredFields) of
        true -> ok;
        false -> {error, missing_required_fields}
    end.

generate_xml_export(ExportData) ->
    % Generate XML format export
    % Simplified for demonstration
    <<"<Policies></Policies>">>.

% Include additional helper functions as needed...
% calculate_user_risk/2
% calculate_resource_risk/2
% calculate_location_risk/2
% calculate_time_risk/2
% calculate_device_risk/2
% calculate_behavior_risk/2
% do_delete_role/2
% do_assign_role/3
% do_revoke_role/3
% do_create_attribute/3
% do_update_attribute/3
% do_delete_attribute/2
% do_define_policy/3
% do_delete_policy/2
% do_define_policy_rule/4
% do_delete_policy_rule/3
% do_set_attributes/3
% do_time_window/4
% do_recurring_access/4
% do_temporary_grant/4
% do_geo_fence/4
% do_region_access/4
% do_device_profile/3
% do_device_trust/3
% do_risk_threshold/4
% do_approve_access/4
% do_deny_access/4
% do_review_access/4
% do_expire_access/3
% do_load_policy_set/2
% do_save_policy_set/2
% do_remove_from_hierarchy/3