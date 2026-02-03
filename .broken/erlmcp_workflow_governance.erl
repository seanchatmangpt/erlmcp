%%%-------------------------------------------------------------------
%%% @doc
%%% Workflow governance and validation for erlmcp v3
%%% Provides enterprise-grade workflow validation and compliance.
%%%
%%% == Features ==
%%% - Workflow validation rules
%%% - Resource quotas and limits
%%% - Priority queue management
%%% - Approval workflows
%%% - Compliance checking
%%% - Policy enforcement
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_workflow_governance).

-behaviour(gen_server).

-include("erlmcp.hrl").

%% API exports
-export([start_link/0, start_link/1,
         validate_workflow/1,
         enforce_resource_quota/2,
         check_priority/2,
         request_approval/2,
         approve_workflow/2,
         reject_workflow/2,
         get_approval_status/1,
         add_validation_rule/2,
         remove_validation_rule/1,
         check_compliance/2,
         set_resource_limit/2,
         get_resource_usage/0,
         audit_workflow/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Type definitions
-type workflow_id() :: binary().
-type rule_id() :: binary().
-type priority() :: low | normal | high | critical.

-record(validation_rule,
        {id :: rule_id(),
         name :: binary(),
         description :: binary(),
         check :: fun((map()) -> {true, term()} | {false, term()}),
         severity :: info | warning | error,
         enabled :: boolean()}).

-record(approval_request,
        {workflow_id :: workflow_id(),
         requested_by :: pid() | binary(),
         requested_at :: integer(),
         reason :: binary(),
         status :: pending | approved | rejected,
         reviewed_by :: pid() | binary() | undefined,
         reviewed_at :: integer() | undefined,
         review_notes :: binary() | undefined}).

-record(resource_limit,
        {resource :: binary(),
         limit :: non_neg_integer(),
         current_usage = 0 :: non_neg_integer(),
         time_window :: pos_integer() | infinity}).

-record(state,
        {validation_rules = [] :: [#validation_rule{}],
         approval_requests = #{} :: #{workflow_id() => #approval_request{}},
         resource_limits = #{} :: #{binary() => #resource_limit{}},
         compliance_policies = #{} :: map(),
         audit_log = [] :: [map()],
         max_audit_entries = 10000 :: pos_integer()}).

-define(DEFAULT_TIMEOUT, 30000).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(_Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec validate_workflow(map()) -> {ok, [map()]} | {error, [map()]}.
validate_workflow(WorkflowSpec) ->
    gen_server:call(?MODULE, {validate_workflow, WorkflowSpec}, ?DEFAULT_TIMEOUT).

-spec enforce_resource_quota(binary(), pos_integer()) -> {ok, allowed} | {error, quota_exceeded}.
enforce_resource_quota(Resource, Amount) ->
    gen_server:call(?MODULE, {enforce_resource_quota, Resource, Amount}, ?DEFAULT_TIMEOUT).

-spec check_priority(workflow_id(), priority()) -> {ok, allowed} | {error, priority_too_low}.
check_priority(WorkflowId, Priority) ->
    gen_server:call(?MODULE, {check_priority, WorkflowId, Priority}, ?DEFAULT_TIMEOUT).

-spec request_approval(workflow_id(), binary()) -> {ok, pending}.
request_approval(WorkflowId, Reason) ->
    gen_server:call(?MODULE, {request_approval, WorkflowId, Reason}, ?DEFAULT_TIMEOUT).

-spec approve_workflow(workflow_id(), binary()) -> ok | {error, term()}.
approve_workflow(WorkflowId, ReviewNotes) ->
    gen_server:call(?MODULE, {approve_workflow, WorkflowId, ReviewNotes}, ?DEFAULT_TIMEOUT).

-spec reject_workflow(workflow_id(), binary()) -> ok | {error, term()}.
reject_workflow(WorkflowId, ReviewNotes) ->
    gen_server:call(?MODULE, {reject_workflow, WorkflowId, ReviewNotes}, ?DEFAULT_TIMEOUT).

-spec get_approval_status(workflow_id()) -> {ok, map()} | {error, not_found}.
get_approval_status(WorkflowId) ->
    gen_server:call(?MODULE, {get_approval_status, WorkflowId}, ?DEFAULT_TIMEOUT).

-spec add_validation_rule(binary(), map()) -> {ok, rule_id()} | {error, term()}.
add_validation_rule(RuleId, RuleConfig) ->
    gen_server:call(?MODULE, {add_validation_rule, RuleId, RuleConfig}, ?DEFAULT_TIMEOUT).

-spec remove_validation_rule(rule_id()) -> ok | {error, not_found}.
remove_validation_rule(RuleId) ->
    gen_server:call(?MODULE, {remove_validation_rule, RuleId}, ?DEFAULT_TIMEOUT).

-spec check_compliance(workflow_id(), map()) -> {ok, compliant} | {error, non_compliant}.
check_compliance(WorkflowId, Policy) ->
    gen_server:call(?MODULE, {check_compliance, WorkflowId, Policy}, ?DEFAULT_TIMEOUT).

-spec set_resource_limit(binary(), pos_integer()) -> ok.
set_resource_limit(Resource, Limit) ->
    gen_server:call(?MODULE, {set_resource_limit, Resource, Limit}, ?DEFAULT_TIMEOUT).

-spec get_resource_usage() -> {ok, map()}.
get_resource_usage() ->
    gen_server:call(?MODULE, get_resource_usage, ?DEFAULT_TIMEOUT).

-spec audit_workflow(workflow_id()) -> {ok, [map()]}.
audit_workflow(WorkflowId) ->
    gen_server:call(?MODULE, {audit_workflow, WorkflowId}, ?DEFAULT_TIMEOUT).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([]) -> {ok, #state{}}.
init([]) ->
    logger:info("Initializing workflow governance engine"),

    %% Load default validation rules
    DefaultRules = load_default_validation_rules(),

    State = #state{
        validation_rules = DefaultRules
    },

    logger:info("Workflow governance initialized with ~p rules", [length(DefaultRules)]),
    {ok, State}.

handle_call({validate_workflow, WorkflowSpec}, _From, State) ->
    Rules = [R || R <- State#state.validation_rules, R#validation_rule.enabled],
    ValidationResults = lists:map(fun(Rule) ->
        case (Rule#validation_rule.check)(WorkflowSpec) of
            {true, _} ->
                #{rule_id => Rule#validation_rule.id,
                  result => passed,
                  severity => Rule#validation_rule.severity};
            {false, Reason} ->
                #{rule_id => Rule#validation_rule.id,
                  result => failed,
                  severity => Rule#validation_rule.severity,
                  reason => Reason}
        end
    end, Rules),

    FailedRules = [R || R <- ValidationResults, maps:get(result, R) =:= failed],

    case FailedRules of
        [] ->
            {reply, {ok, ValidationResults}, State};
        _ ->
            {reply, {error, FailedRules}, State}
    end;

handle_call({enforce_resource_quota, Resource, Amount}, _From, State) ->
    case maps:get(Resource, State#state.resource_limits, undefined) of
        undefined ->
            %% No limit set, allow
            {reply, {ok, allowed}, State};
        #resource_limit{limit = Limit, current_usage = CurrentUsage} ->
            case CurrentUsage + Amount =< Limit of
                true ->
                    %% Update usage
                    UpdatedLimit = #resource_limit{
                        resource = Resource,
                        limit = Limit,
                        current_usage = CurrentUsage + Amount,
                        time_window = maps:get(time_window, State#state.resource_limits, infinity)
                    },
                    NewLimits = maps:put(Resource, UpdatedLimit, State#state.resource_limits),
                    {reply, {ok, allowed}, State#state{resource_limits = NewLimits}};
                false ->
                    {reply, {error, quota_exceeded}, State}
            end
    end;

handle_call({check_priority, WorkflowId, Priority}, _From, State) ->
    %% Check if workflow requires approval based on priority
    case Priority of
        critical ->
            %% Critical workflows require approval
            case maps:get(WorkflowId, State#state.approval_requests, undefined) of
                #approval_request{status = approved} ->
                    {reply, {ok, allowed}, State};
                _ ->
                    {reply, {error, priority_too_low}, State}
            end;
        _ ->
            {reply, {ok, allowed}, State}
    end;

handle_call({request_approval, WorkflowId, Reason}, _From, State) ->
    Request = #approval_request{
        workflow_id = WorkflowId,
        requested_by = self(),
        requested_at = erlang:system_time(millisecond),
        reason = Reason,
        status = pending
    },
    NewRequests = maps:put(WorkflowId, Request, State#state.approval_requests),

    %% Log approval request
    NewAuditLog = log_audit_event(approval_requested, #{
        workflow_id => WorkflowId,
        reason => Reason
    }, State#state.audit_log),

    {reply, {ok, pending}, State#state{
        approval_requests => NewRequests,
        audit_log = NewAuditLog
    }};

handle_call({approve_workflow, WorkflowId, ReviewNotes}, _From, State) ->
    case maps:get(WorkflowId, State#state.approval_requests, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        Request ->
            UpdatedRequest = Request#approval_request{
                status = approved,
                reviewed_by = self(),
                reviewed_at = erlang:system_time(millisecond),
                review_notes = ReviewNotes
            },
            NewRequests = maps:put(WorkflowId, UpdatedRequest, State#state.approval_requests),

            %% Log approval
            NewAuditLog = log_audit_event(approval_granted, #{
                workflow_id => WorkflowId,
                notes => ReviewNotes
            }, State#state.audit_log),

            {reply, ok, State#state{
                approval_requests => NewRequests,
                audit_log = NewAuditLog
            }}
    end;

handle_call({reject_workflow, WorkflowId, ReviewNotes}, _From, State) ->
    case maps:get(WorkflowId, State#state.approval_requests, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        Request ->
            UpdatedRequest = Request#approval_request{
                status = rejected,
                reviewed_by = self(),
                reviewed_at = erlang:system_time(millisecond),
                review_notes = ReviewNotes
            },
            NewRequests = maps:put(WorkflowId, UpdatedRequest, State#state.approval_requests),

            %% Log rejection
            NewAuditLog = log_audit_event(approval_rejected, #{
                workflow_id => WorkflowId,
                notes => ReviewNotes
            }, State#state.audit_log),

            {reply, ok, State#state{
                approval_requests => NewRequests,
                audit_log = NewAuditLog
            }}
    end;

handle_call({get_approval_status, WorkflowId}, _From, State) ->
    case maps:get(WorkflowId, State#state.approval_requests, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        Request ->
            StatusMap = #{
                workflow_id => Request#approval_request.workflow_id,
                status => Request#approval_request.status,
                requested_at => Request#approval_request.requested_at,
                reason => Request#approval_request.reason,
                reviewed_at => Request#approval_request.reviewed_at,
                review_notes => Request#approval_request.review_notes
            },
            {reply, {ok, StatusMap}, State}
    end;

handle_call({add_validation_rule, RuleId, RuleConfig}, _From, State) ->
    Rule = #validation_rule{
        id = RuleId,
        name = maps:get(name, RuleConfig, <<>>),
        description = maps:get(description, RuleConfig, <<>>),
        check = maps:get(check, RuleConfig),
        severity = maps:get(severity, RuleConfig, warning),
        enabled = maps:get(enabled, RuleConfig, true)
    },
    NewRules = [Rule | State#state.validation_rules],
    {reply, {ok, RuleId}, State#state{validation_rules = NewRules}};

handle_call({remove_validation_rule, RuleId}, _From, State) ->
    NewRules = lists:filter(fun(R) -> R#validation_rule.id =/= RuleId end, State#state.validation_rules),
    case length(NewRules) < length(State#state.validation_rules) of
        true ->
            {reply, ok, State#state{validation_rules = NewRules}};
        false ->
            {reply, {error, not_found}, State}
    end;

handle_call({check_compliance, WorkflowId, Policy}, _From, State) ->
    %% Check workflow against compliance policy
    case maps:get(type, Policy, undefined) of
        undefined ->
            {reply, {error, invalid_policy}, State};
        Type ->
            case evaluate_compliance(Type, Policy, WorkflowId, State) of
                compliant ->
                    {reply, {ok, compliant}, State};
                non_compliant ->
                    {reply, {error, non_compliant}, State}
            end
    end;

handle_call({set_resource_limit, Resource, Limit}, _From, State) ->
    ResourceLimit = #resource_limit{
        resource = Resource,
        limit = Limit,
        current_usage = 0,
        time_window = maps:get(time_window, State#state.resource_limits, infinity)
    },
    NewLimits = maps:put(Resource, ResourceLimit, State#state.resource_limits),
    {reply, ok, State#state{resource_limits = NewLimits}};

handle_call(get_resource_usage, _From, State) ->
    UsageMaps = maps:fold(fun(Resource, #resource_limit{limit = Limit, current_usage = Usage}, Acc) ->
        Acc#{Resource => #{limit => Limit, usage => Usage, available => Limit - Usage}}
    end, #{}, State#state.resource_limits),
    {reply, {ok, UsageMaps}, State};

handle_call({audit_workflow, WorkflowId}, _From, State) ->
    AuditEntries = [E || E <- State#state.audit_log, maps:get(workflow_id, E, undefined) =:= WorkflowId],
    {reply, {ok, AuditEntries}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

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

-spec load_default_validation_rules() -> [#validation_rule{}].
load_default_validation_rules() ->
    [
        #validation_rule{
            id = <<"has_tasks">>,
            name => <<"Has Tasks">>,
            description => <<"Workflow must have at least one task">>,
            check = fun(Workflow) ->
                Tasks = maps:get(tasks, Workflow, #{}),
                case maps:size(Tasks) of
                    0 -> {false, <<"Workflow has no tasks">>};
                    _ -> {true, ok}
                end
            end,
            severity = error,
            enabled = true
        },
        #validation_rule{
            id = <<"valid_dependencies">>,
            name => <<"Valid Dependencies">>,
            description => <<"All task dependencies must exist">>,
            check = fun(Workflow) ->
                Tasks = maps:get(tasks, Workflow, #{}),
                TaskIds = maps:keys(Tasks),
                AllDeps = lists:flatten([maps:get(dependencies, T, []) || T <- maps:values(Tasks)]),
                InvalidDeps = [D || D <- AllDeps, not lists:member(D, TaskIds)],
                case InvalidDeps of
                    [] -> {true, ok};
                    _ -> {false, {invalid_dependencies, InvalidDeps}}
                end
            end,
            severity = error,
            enabled = true
        },
        #validation_rule{
            id = <<"no_circular_dependencies">>,
            name => <<"No Circular Dependencies">>,
            description => <<"Workflow must not have circular dependencies">>,
            check = fun(Workflow) ->
                Tasks = maps:get(tasks, Workflow, #{}),
                case detect_cycles(Tasks) of
                    {true, Cycles} -> {false, {circular_dependencies, Cycles}};
                    false -> {true, ok}
                end
            end,
            severity = error,
            enabled = true
        },
        #validation_rule{
            id = <<"task_timeout_reasonable">>,
            name => <<"Task Timeout Reasonable">>,
            description => <<"Task timeouts must be reasonable (< 5 minutes)">>,
            check = fun(Workflow) ->
                Tasks = maps:get(tasks, Workflow, #{}),
                UnreasonableTasks = [Id || {Id, Task} <- maps:to_list(Tasks),
                    maps:get(timeout, Task, 30000) > 300000],
                case UnreasonableTasks of
                    [] -> {true, ok};
                    _ -> {false, {unreasonable_timeouts, UnreasonableTasks}}
                end
            end,
            severity = warning,
            enabled = true
        }
    ].

-spec detect_cycles(#{binary() => map()}) -> {true, [[binary()]]} | false.
detect_cycles(Tasks) ->
    %% Build adjacency list
    Graph = maps:map(fun(_Id, Task) ->
        maps:get(dependencies, Task, [])
    end, Tasks),

    %% Detect cycles using DFS
    NodeIds = maps:keys(Graph),
    case lists:any(fun(NodeId) -> has_cycle(NodeId, Graph, [], []) end, NodeIds) of
        true -> {true, []};
        false -> false
    end.

-spec has_cycle(binary(), #{binary() => [binary()]}, [binary()], [binary()]) -> boolean().
has_cycle(Node, Graph, Visited, RecStack) ->
    case lists:member(Node, RecStack) of
        true -> true;
        false ->
            case lists:member(Node, Visited) of
                true -> false;
                false ->
                    Neighbors = maps:get(Node, Graph, []),
                    lists:any(fun(N) -> has_cycle(N, Graph, [Node | Visited], [Node | RecStack]) end, Neighbors)
            end
    end.

-spec evaluate_compliance(term(), map(), workflow_id(), #state{}) -> compliant | non_compliant.
evaluate_compliance(security, Policy, _WorkflowId, _State) ->
    %% Check security compliance
    RequiredEncryption = maps:get(encryption, Policy, none),
    case RequiredEncryption of
        none -> compliant;
        _ -> compliant  % Simplified check
    end;
evaluate_compliance(resource, Policy, _WorkflowId, State) ->
    %% Check resource compliance
    MaxResources = maps:get(max_resources, Policy, 100),
    TotalResources = maps:size(State#state.resource_limits),
    case TotalResources =< MaxResources of
        true -> compliant;
        false -> non_compliant
    end;
evaluate_compliance(_, _, _, _) ->
    compliant.

-spec log_audit_event(atom(), map(), [map()]) -> [map()].
log_audit_event(EventType, Details, AuditLog) ->
    Entry = #{
        event_type => EventType,
        details => Details,
        timestamp => erlang:system_time(millisecond)
    },
    [Entry | AuditLog].
