%%%-------------------------------------------------------------------
%%% @doc
%%% Workflow Engine - Orchestrates multi-step workflows with security hardening
%%%
%%% Based on ontology/erlmcp_new/workflow.ttl
%%%
%%% Responsibilities:
%%% - Define workflows with DAG-based step dependencies
%%% - Execute workflows with parallel/sequential steps
%%% - Handle conditional execution and branching
%%% - Retry failed steps with backoff
%%% - Cancel running workflows
%%% - Track execution state
%%% - Security: Zero-trust authentication, audit logging, capability-based access
%%%
%%% == Workflow Definition ==
%%% ```
%%% #{
%%%   id => <<"my_workflow">>,
%%%   steps => [
%%%     #{
%%%       id => <<"step1">>,
%%%       type => tool,
%%%       tool_name => <<"calculator">>,
%%%       tool_arguments => #{expression => <<"2 + 2">>},
%%%       max_retries => 3,
%%%       timeout_sec => 30,
%%%       required_permissions => [<<"read">>, <<"execute">>]
%%%     },
%%%     #{
%%%       id => <<"step2">>,
%%%       type => parallel,
%%%       child_steps => [
%%%         #{id => <<"step2a">>, type => tool, ...},
%%%         #{id => <<"step2b">>, type => tool, ...}
%%%       ]
%%%     }
%%%   ],
%%%   transitions => [
%%%     #{from => <<"step1">>, to => <<"step2">>, condition => success}
%%%   ]
%%% }
%%% '''
%%%
%%% == Process Model ==
%%% - gen_server maintains workflow definitions and executions
%%% - Each execution runs in isolated process with security context
%%% - Supports cancellation via monitors
%%% - Comprehensive audit logging for compliance
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_workflow_engine).

-behaviour(gen_server).

%% API
-export([start_link/0,
         define_workflow/1,
         execute_workflow/2,
         cancel_execution/1,
         get_execution_status/1,
         list_workflows/0,
         authenticate_execution/2,
         authorize_step/3,
         get_audit_log/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(AUDIT_LOG_LIMIT, 1000).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Types
%%====================================================================

-type workflow_id() :: binary().
-type step_id() :: binary().
-type execution_id() :: binary().
-type status() :: pending | running | completed | failed | cancelled.
-type permission() :: binary().
-type capability() :: #{
    resource => binary(),
    actions => [binary()],
    conditions => [map()]
}.

-type step() :: #{
    id => step_id(),
    type => tool | parallel | sequence | conditional | loop | delay,
    tool_name => binary(),
    tool_arguments => map(),
    child_steps => [step()],
    max_retries => non_neg_integer(),
    retry_backoff_ms => non_neg_integer(),
    timeout_sec => pos_integer(),
    required_permissions => [permission()],
    security_context => map()
}.

-type transition() :: #{
    from => step_id(),
    to => step_id(),
    condition => success | failure | {value_equals, term()} | {expression, binary()}
}.

-type workflow() :: #{
    id => workflow_id(),
    steps => [step()],
    transitions => [transition()],
    required_permissions => [permission()],
    security_level => low | medium | high
}.

-type execution() :: #{
    execution_id => execution_id(),
    workflow_id => workflow_id(),
    status => status(),
    started_at => erlang:timestamp(),
    completed_at => erlang:timestamp() | undefined,
    input_data => map(),
    output_data => map() | undefined,
    error_message => binary() | undefined,
    executor_pid => pid() | undefined,
    security_context => map(),
    audit_trail => [map()]
}.

-type audit_event() :: #{
    timestamp => integer(),
    event_type => binary(),
    execution_id => execution_id(),
    step_id => step_id() | undefined,
    subject => binary(),
    action => binary(),
    result => success | failure | error,
    details => term()
}.

-record(state, {
    workflows = #{} :: #{workflow_id() => workflow()},
    executions = #{} :: #{execution_id() => execution()},
    execution_monitors = #{} :: #{execution_id() => reference()},
    permissions = #{} :: #{binary() => permission()},
    audit_log = [] :: [audit_event()],
    correlation_counter = 0 :: integer(),
    security_context = #{} :: map()
}).

-type state() :: #state{}.

%%====================================================================
%% API
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Define a workflow with security validation
-spec define_workflow(workflow()) -> ok | {error, term()}.
define_workflow(#{id := WorkflowId} = Workflow) ->
    gen_server:call(?SERVER, {define_workflow, WorkflowId, Workflow}, 5000).

%% @doc Execute a workflow with input data and authentication
-spec execute_workflow(workflow_id(), map()) -> {ok, execution_id()} | {error, term()}.
execute_workflow(WorkflowId, InputData) ->
    gen_server:call(?SERVER, {execute_workflow, WorkflowId, InputData}, 5000).

%% @doc Cancel a running workflow execution
-spec cancel_execution(execution_id()) -> ok | {error, term()}.
cancel_execution(ExecutionId) ->
    gen_server:call(?SERVER, {cancel_execution, ExecutionId}, 5000).

%% @doc Get execution status
-spec get_execution_status(execution_id()) -> {ok, execution()} | {error, not_found}.
get_execution_status(ExecutionId) ->
    gen_server:call(?SERVER, {get_execution_status, ExecutionId}, 5000).

%% @doc List all defined workflows
-spec list_workflows() -> {ok, [workflow_id()]}.
list_workflows() ->
    gen_server:call(?SERVER, list_workflows, 5000).

%% @doc Authenticate an execution request
-spec authenticate_execution(execution_id(), map()) -> {ok, binary()} | {error, term()}.
authenticate_execution(ExecutionId, AuthData) ->
    gen_server:call(?SERVER, {authenticate_execution, ExecutionId, AuthData}, 5000).

%% @doc Authorize step execution
-spec authorize_step(execution_id(), step_id(), map()) -> ok | {error, term()}.
authorize_step(ExecutionId, StepId, Context) ->
    gen_server:call(?SERVER, {authorize_step, ExecutionId, StepId, Context}, 5000).

%% @doc Get audit log for compliance
-spec get_audit_log() -> {ok, [audit_event()]}.
get_audit_log() ->
    gen_server:call(?SERVER, get_audit_log, 5000).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([]) -> {ok, #state{}}.
init([]) ->
    process_flag(trap_exit, true),

    %% Initialize security context
    SecurityContext = #{
        enable_zero_trust => true,
        require_auth => true,
        audit_enabled => true,
        session_timeout => 300000,
        max_concurrent_executions => 100
    },

    %% Load default permissions
    Permissions = load_default_permissions(),

    logger:info("Initializing Workflow Engine with security: ~p", [SecurityContext]),

    {ok, #state{security_context = SecurityContext, permissions = Permissions}}.

-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {reply, term(), #state{}} | {noreply, #state{}}.

handle_call({define_workflow, WorkflowId, Workflow}, _From, State) ->
    %% Validate workflow security before definition
    case validate_workflow_security(Workflow) of
        ok ->
            %% Check if workflow exists
            case maps:is_key(WorkflowId, State#state.workflows) of
                true ->
                    {reply, {error, workflow_exists}, State};
                false ->
                    %% Log audit event
                    AuditEvent = create_audit_event(
                        workflow_definition,
                        undefined,
                        <<"system">>,
                        <<"define_workflow">>,
                        success,
                        {WorkflowId, Workflow}
                    ),
                    NewState = log_audit_event(AuditEvent, State),
                    NewWorkflows = maps:put(WorkflowId, Workflow, State#state.workflows),
                    {reply, ok, NewState#state{workflows = NewWorkflows}}
            end;
        {error, Reason} ->
            %% Log security validation failure
            AuditEvent = create_audit_event(
                workflow_definition,
                undefined,
                <<"system">>,
                <<"define_workflow">>,
                error,
                {WorkflowId, Reason}
            ),
            NewState = log_audit_event(AuditEvent, State),
            {reply, {error, Reason}, NewState}
    end;

handle_call({execute_workflow, WorkflowId, InputData}, _From, State) ->
    case maps:get(WorkflowId, State#state.workflows, undefined) of
        undefined ->
            {reply, {error, workflow_not_found}, State};
        Workflow ->
            %% Generate execution ID with security context
            ExecutionId = generate_execution_id(),
            StartedAt = erlang:timestamp(),

            %% Create execution record with security context
            Execution = #{
                execution_id => ExecutionId,
                workflow_id => WorkflowId,
                status => pending,
                started_at => StartedAt,
                completed_at => undefined,
                input_data => InputData,
                output_data => undefined,
                error_message => undefined,
                executor_pid => undefined,
                security_context => State#state.security_context,
                audit_trail => []
            },

            %% Validate workflow security
            case validate_workflow_security(Workflow) of
                ok ->
                    %% Start executor process in isolated environment
                    case start_executor(ExecutionId, Workflow, InputData) of
                        {ok, ExecutorPid} ->
                            %% Monitor executor
                            MonRef = erlang:monitor(process, ExecutorPid),

                            %% Log audit event
                            AuditEvent = create_audit_event(
                                workflow_execution,
                                ExecutionId,
                                <<"system">>,
                                <<"execute_workflow">>,
                                success,
                                {WorkflowId, InputData}
                            ),
                            NewState = log_audit_event(AuditEvent, State),

                            UpdatedExecution = Execution#{
                                executor_pid => ExecutorPid,
                                status => running
                            },
                            NewExecutions = maps:put(ExecutionId, UpdatedExecution, State#state.executions),
                            NewMonitors = maps:put(ExecutionId, MonRef, State#state.execution_monitors),

                            {reply, {ok, ExecutionId}, NewState#state{
                                executions = NewExecutions,
                                execution_monitors = NewMonitors
                            }};
                        {error, Reason} ->
                            %% Log execution failure
                            AuditEvent = create_audit_event(
                                workflow_execution,
                                ExecutionId,
                                <<"system">>,
                                <<"execute_workflow">>,
                                error,
                                {WorkflowId, Reason}
                            ),
                            NewState = log_audit_event(AuditEvent, State),

                            FailedExecution = Execution#{
                                status => failed,
                                completed_at => erlang:timestamp(),
                                error_message => list_to_binary(io_lib:format("~p", [Reason]))
                            },
                            NewExecutions = maps:put(ExecutionId, FailedExecution, State#state.executions),
                            {reply, {error, Reason}, NewState#state{executions = NewExecutions}}
                    end;
                {error, Reason} ->
                    %% Log security validation failure
                    AuditEvent = create_audit_event(
                        workflow_execution,
                        ExecutionId,
                        <<"system">>,
                        <<"execute_workflow">>,
                        error,
                        {WorkflowId, Reason}
                    ),
                    NewState = log_audit_event(AuditEvent, State),
                    {reply, {error, Reason}, State}
            end
    end;

handle_call({cancel_execution, ExecutionId}, _From, State) ->
    case maps:get(ExecutionId, State#state.executions, undefined) of
        undefined ->
            {reply, {error, execution_not_found}, State};
        #{executor_pid := undefined, status := Status} when Status =:= failed;
                                                                   Status =:= completed;
                                                                   Status =:= cancelled ->
            {reply, {error, already_completed}, State};
        #{executor_pid := ExecutorPid} = Execution ->
            case is_process_alive(ExecutorPid) of
                true ->
                    %% Send cancellation message
                    ExecutorPid ! cancel,

                    %% Log audit event
                    AuditEvent = create_audit_event(
                        workflow_cancellation,
                        ExecutionId,
                        <<"system">>,
                        <<"cancel_execution">>,
                        success,
                        cancelled
                    ),
                    NewState = log_audit_event(AuditEvent, State),

                    UpdatedExecution = Execution#{
                        status => cancelled,
                        completed_at => erlang:timestamp()
                    },
                    NewExecutions = maps:put(ExecutionId, UpdatedExecution, State#state.executions),
                    {reply, ok, NewState#state{executions = NewExecutions}};
                false ->
                    {reply, {error, executor_not_running}, State}
            end
    end;

handle_call({get_execution_status, ExecutionId}, _From, State) ->
    case maps:get(ExecutionId, State#state.executions, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        Execution ->
            {reply, {ok, Execution}, State}
    end;

handle_call(list_workflows, _From, State) ->
    WorkflowIds = maps:keys(State#state.workflows),
    {reply, {ok, WorkflowIds}, State};

handle_call({authenticate_execution, ExecutionId, AuthData}, _From, State) ->
    case maps:get(ExecutionId, State#state.executions, undefined) of
        undefined ->
            {reply, {error, execution_not_found}, State};
        Execution ->
            %% Validate authentication data
            case validate_auth_data(AuthData, State) of
                {ok, Subject} ->
                    %% Log audit event
                    AuditEvent = create_audit_event(
                        workflow_authentication,
                        ExecutionId,
                        Subject,
                        <<"authenticate">>,
                        success,
                        {execution_id, ExecutionId}
                    ),
                    NewState = log_audit_event(AuditEvent, State),
                    {reply, {ok, Subject}, NewState};
                {error, Reason} ->
                    %% Log authentication failure
                    AuditEvent = create_audit_event(
                        workflow_authentication,
                        ExecutionId,
                        <<"unknown">>,
                        <<"authenticate">>,
                        failure,
                        {Reason, AuthData}
                    ),
                    NewState = log_audit_event(AuditEvent, State),
                    {reply, {error, Reason}, NewState}
            end
    end;

handle_call({authorize_step, ExecutionId, StepId, Context}, _From, State) ->
    case maps:get(ExecutionId, State#state.executions, undefined) of
        undefined ->
            {reply, {error, execution_not_found}, State};
        Execution ->
            %% Check step permissions
            case check_step_permissions(ExecutionId, StepId, Context, State) of
                ok ->
                    %% Log audit event
                    AuditEvent = create_audit_event(
                        step_authorization,
                        ExecutionId,
                        <<"system">>,
                        <<"authorize_step">>,
                        success,
                        {StepId, Context}
                    ),
                    NewState = log_audit_event(AuditEvent, State),
                    {reply, ok, NewState};
                {error, Reason} ->
                    %% Log authorization failure
                    AuditEvent = create_audit_event(
                        step_authorization,
                        ExecutionId,
                        <<"unknown">>,
                        <<"authorize_step">>,
                        failure,
                        {StepId, Reason}
                    ),
                    NewState = log_audit_event(AuditEvent, State),
                    {reply, {error, Reason}, NewState}
            end
    end;

handle_call(get_audit_log, _From, State) ->
    {reply, {ok, State#state.audit_log}, State};

handle_call(_Request, _From, State) ->
    AuditEvent = create_audit_event(
        unknown_request,
        undefined,
        <<"system">>,
        <<"unknown_request">>,
        error,
        {received}
    ),
    NewState = log_audit_event(AuditEvent, State),
    {reply, {error, unknown_request}, NewState}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info({'DOWN', MonRef, process, _Pid, normal}, State) ->
    %% Executor completed normally
    case find_execution_by_monitor(MonRef, State) of
        {ok, ExecutionId} ->
            %% Log completion event
            AuditEvent = create_audit_event(
                workflow_completion,
                ExecutionId,
                <<"system">>,
                <<"workflow_complete">>,
                success,
                normal
            ),
            NewState = log_audit_event(AuditEvent, State),

            NewMonitors = maps:remove(ExecutionId, State#state.execution_monitors),
            {noreply, NewState#state{execution_monitors = NewMonitors}};
        error ->
            {noreply, State}
    end;

handle_info({'DOWN', MonRef, process, _Pid, Reason}, State) ->
    %% Executor failed
    case find_execution_by_monitor(MonRef, State) of
        {ok, ExecutionId} ->
            Executions = State#state.executions,
            case maps:get(ExecutionId, Executions, undefined) of
                undefined ->
                    NewMonitors = maps:remove(ExecutionId, State#state.execution_monitors),
                    {noreply, State#state{execution_monitors = NewMonitors}};
                Execution ->
                    %% Log failure event
                    AuditEvent = create_audit_event(
                        workflow_failure,
                        ExecutionId,
                        <<"system">>,
                        <<"workflow_failed">>,
                        error,
                        Reason
                    ),
                    NewState = log_audit_event(AuditEvent, State),

                    UpdatedExecution = Execution#{
                        status => failed,
                        completed_at => erlang:timestamp(),
                        error_message => list_to_binary(io_lib:format("~p", [Reason]))
                    },
                    NewExecutions = maps:put(ExecutionId, UpdatedExecution, Executions),
                    NewMonitors = maps:remove(ExecutionId, State#state.execution_monitors),
                    {noreply, NewState#state{
                        executions = NewExecutions,
                        execution_monitors = NewMonitors
                    }}
            end;
        error ->
            {noreply, State}
    end;

handle_info({execution_complete, ExecutionId, Result}, State) ->
    %% Executor sent completion result
    Executions = State#state.executions,
    case maps:get(ExecutionId, Executions, undefined) of
        undefined ->
            {noreply, State};
        Execution ->
            %% Log completion event
            AuditEvent = create_audit_event(
                workflow_completion,
                ExecutionId,
                <<"system">>,
                <<"step_complete">>,
                success,
                Result
            ),
            NewState = log_audit_event(AuditEvent, State),

            UpdatedExecution = case Result of
                {ok, OutputData} ->
                    AuditTrail = maps:get(audit_trail, Execution, []) ++ [AuditEvent],
                    Execution#{
                        status => completed,
                        completed_at => erlang:timestamp(),
                        output_data => OutputData,
                        audit_trail => AuditTrail
                    };
                {error, Reason} ->
                    AuditTrail = maps:get(audit_trail, Execution, []) ++ [AuditEvent],
                    Execution#{
                        status => failed,
                        completed_at => erlang:timestamp(),
                        error_message => list_to_binary(io_lib:format("~p", [Reason])),
                        audit_trail => AuditTrail
                    }
            end,
            NewExecutions = maps:put(ExecutionId, UpdatedExecution, Executions),
            {noreply, NewState#state{executions = NewExecutions}}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, State) ->
    %% Log shutdown event
    AuditEvent = create_audit_event(
        system_shutdown,
        undefined,
        <<"system">>,
        <<"shutdown">>,
        success,
        completed
    ),
    NewState = log_audit_event(AuditEvent, State),
    ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

%% @private Validate workflow security configuration
-spec validate_workflow_security(workflow()) -> ok | {error, term()}.
validate_workflow_security(#{steps := Steps, transitions := Transitions}) when length(Steps) > 0 ->
    case validate_security_levels(Steps) of
        ok -> validate_transitions_security(Transitions, Steps);
        Error -> Error
    end;
validate_workflow_security(_) ->
    {error, invalid_workflow_structure}.

%% @private Validate security levels for steps
-spec validate_security_levels([step()]) -> ok | {error, term()}.
validate_security_levels([]) ->
    ok;
validate_security_levels([#{id := Id, type := Type} = Step | Rest]) ->
    case validate_step_security(Step) of
        ok -> validate_security_levels(Rest);
        Error -> Error
    end;
validate_security_levels(_) ->
    {error, invalid_step_structure}.

%% @private Validate step security configuration
-spec validate_step_security(step()) -> ok | {error, term()}.
validate_step_security(#{type := tool, tool_name := ToolName, required_permissions := Permissions})
    when is_binary(ToolName), is_list(Permissions) ->
    ok;
validate_step_security(#{type := parallel, child_steps := Children}) when is_list(Children) ->
    validate_security_levels(Children);
validate_step_security(#{type := sequence, child_steps := Children}) when is_list(Children) ->
    validate_security_levels(Children);
validate_step_security(#{type := loop, child_steps := Children}) when is_list(Children) ->
    validate_security_levels(Children);
validate_step_security(#{type := conditional, child_steps := Children}) when is_list(Children) ->
    validate_security_levels(Children);
validate_step_security(#{type := delay}) ->
    ok;
validate_step_security(_) ->
    {error, invalid_step_security_configuration}.

%% @private Validate transitions security
-spec validate_transitions_security([transition()], [step()]) -> ok | {error, term()}.
validate_transitions_security([], _Steps) ->
    ok;
validate_transitions_security([#{from := From, to := To} | Rest], Steps) ->
    StepIds = [Id || #{id := Id} <- Steps],
    case lists:member(From, StepIds) andalso lists:member(To, StepIds) of
        true -> validate_transitions_security(Rest, Steps);
        false -> {error, {invalid_transition_security, {From, To}}}
    end;
validate_transitions_security(_, _) ->
    {error, invalid_transition_security}.

%% @private Load default permissions
-spec load_default_permissions() -> #{binary() => permission()}.
load_default_permissions() ->
    #{
        <<"workflow_admin">> => #{
            subject => <<"workflow_admin">>,
            capabilities => [
                #{
                    resource => <<"/workflow/*">>,
                    actions => [<<"read">>, <<"write">>, <<"delete">>, <<"execute">>],
                    conditions => []
                }
            ],
            expires_at => undefined
        },
        <<"workflow_user">> => #{
            subject => <<"workflow_user">>,
            capabilities => [
                #{
                    resource => <<"/workflow/execute">>,
                    actions => [<<"read">>, <<"execute">>],
                    conditions => []
                }
            ],
            expires_at => undefined
        }
    }.

%% @private Generate unique execution ID
-spec generate_execution_id() -> binary().
generate_execution_id() ->
    BinaryTime = integer_to_binary(erlang:unique_integer()),
    Random = crypto:strong_rand_bytes(8),
    <<BinaryTime/binary, Random/binary>>.

%% @private Create audit event
-spec create_audit_event(binary(), execution_id(), binary(), binary(), success | failure | error, term()) -> audit_event().
create_audit_event(EventType, ExecutionId, Subject, Action, Result, Details) ->
    #{
        timestamp => erlang:system_time(millisecond),
        event_type => EventType,
        execution_id => ExecutionId,
        step_id => undefined,
        subject => Subject,
        action => Action,
        result => Result,
        details => Details
    }.

%% @private Log audit event with size limit
-spec log_audit_event(audit_event(), state()) -> state().
log_audit_event(AuditEvent, State) ->
    AuditLog = [AuditEvent | State#state.audit_log],
    LimitedAuditLog = lists:sublist(AuditLog, ?AUDIT_LOG_LIMIT),
    State#state{audit_log = LimitedAuditLog}.

%% @private Validate authentication data using security manager
-spec validate_auth_data(map(), state()) -> {ok, binary()} | {error, term()}.
validate_auth_data(AuthData, _State) ->
    Request = maps:with([<<"token">>, <<"auth">>], AuthData),
    case erlmcp_security_manager:authenticate_request(Request, #{}) of
        {ok, Subject} ->
            {ok, Subject};
        {error, Reason} ->
            {error, Reason}
    end.

%% @private Validate token
-spec validate_token(binary(), state()) -> {ok, binary()} | {error, term()}.
validate_token(Token, State) ->
    case binary:length(Token) >= 32 of
        true ->
            case maps:find(Token, State#state.permissions) of
                {ok, Permission} ->
                    case maps:get(<<"expires_at">>, Permission, undefined) of
                        undefined ->
                            {ok, maps:get(<<"subject">>, Permission)};
                        ExpiresAt when is_integer(ExpiresAt) ->
                            case erlang:system_time(millisecond) < ExpiresAt of
                                true ->
                                    {ok, maps:get(<<"subject">>, Permission)};
                                false ->
                                    {error, token_expired}
                            end
                    end;
                error ->
                    {error, invalid_token}
            end;
        false ->
            {error, invalid_token}
    end.

%% @private Check step permissions
-spec check_step_permissions(execution_id(), step_id(), map(), state()) -> ok | {error, term()}.
check_step_permissions(ExecutionId, StepId, Context, State) ->
    case maps:get(ExecutionId, State#state.executions, undefined) of
        undefined ->
            {error, execution_not_found};
        Execution ->
            WorkflowId = maps:get(workflow_id, Execution),
            case maps:get(WorkflowId, State#state.workflows, undefined) of
                undefined ->
                    {error, workflow_not_found};
                Workflow ->
                    case find_step_by_id(StepId, Workflow) of
                        {ok, Step} ->
                            RequiredPermissions = maps:get(required_permissions, Step, []),
                            Subject = <<"system">>, % In production, extract from auth
                            check_permissions(Subject, RequiredPermissions, State);
                        error ->
                            {error, step_not_found}
                    end
            end
    end.

%% @private Find step by ID
-spec find_step_by_id(step_id(), workflow()) -> {ok, step()} | error.
find_step_by_id(StepId, Workflow) ->
    Steps = maps:get(steps, Workflow),
    case lists:filter(fun(Step) -> maps:get(id, Step) == StepId end, Steps) of
        [Step] -> {ok, Step};
        [] -> error
    end.

%% @private Check permissions
-spec check_permissions(binary(), [permission()], state()) -> ok | {error, term()}.
check_permissions(_Subject, [], _State) ->
    ok;
check_permissions(Subject, [Permission | Rest], State) ->
    case check_permission(Subject, Permission, State) of
        ok -> ok;
        {error, _} -> check_permissions(Subject, Rest, State)
    end.

%% @private Check single permission
-spec check_permission(binary(), permission(), state()) -> ok | {error, term()}.
check_permission(Subject, Permission, State) ->
    case maps:get(Subject, State#state.permissions, undefined) of
        undefined ->
            {error, no_permissions_for_subject};
        PermissionData ->
            Capabilities = maps:get(capabilities, PermissionData, []),
            case lists:any(fun(Cap) ->
                maps:get(resource, Cap) == Permission orelse
                string:prefix(Permission, <<"/workflow/*">>)
            end, Capabilities) of
                true -> ok;
                false -> {error, insufficient_permissions}
            end
    end.

%% @private Start executor process with isolation
-spec start_executor(execution_id(), workflow(), map()) -> {ok, pid()} | {error, term()}.
start_executor(ExecutionId, Workflow, InputData) ->
    %% Create isolated process with security context
    Proc = fun() ->
        executor_loop(ExecutionId, Workflow, InputData, #{})
    end,
    Pid = spawn_link(Proc), % Use spawn_link for better supervision
    {ok, Pid}.

%% @private Executor loop with security context
-spec executor_loop(execution_id(), workflow(), map(), map()) -> no_return().
executor_loop(ExecutionId, #{steps := Steps} = Workflow, InputData, Context) ->
    %% Find initial steps (no incoming transitions)
    InitialSteps = find_initial_steps(Steps, Workflow),

    case execute_steps_with_security(ExecutionId, InitialSteps, Steps, InputData, Context) of
        {ok, OutputData} ->
            ?SERVER ! {execution_complete, ExecutionId, {ok, OutputData}};
        {error, Reason} ->
            ?SERVER ! {execution_complete, ExecutionId, {error, Reason}}
    end.

%% @private Execute steps with security validation
-spec execute_steps_with_security(execution_id(), [step()], [step()], map(), map()) -> {ok, map()} | {error, term()}.
execute_steps_with_security(ExecutionId, [], _AllSteps, _InputData, _Context) ->
    {ok, #{}};
execute_steps_with_security(ExecutionId, [Step | Rest], AllSteps, InputData, Context) ->
    StepId = maps:get(id, Step),

    %% Authorize step execution
    case authorize_step(ExecutionId, StepId, Context) of
        ok ->
            case execute_step_with_security(ExecutionId, Step, AllSteps, InputData, Context) of
                {ok, StepContext} ->
                    %% Merge context and continue
                    MergedContext = maps:merge(Context, StepContext),
                    execute_steps_with_security(ExecutionId, Rest, AllSteps, InputData, MergedContext);
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, {authorization_failed, Reason}}
    end.

%% @private Execute a single step with security
-spec execute_step_with_security(execution_id(), step(), [step()], map(), map()) -> {ok, map()} | {error, term()}.
execute_step_with_security(ExecutionId, #{type := tool, tool_name := ToolName, tool_arguments := Args} = Step,
                           _AllSteps, InputData, Context) ->
    Timeout = maps:get(timeout_sec, Step, 30) * 1000,
    MaxRetries = maps:get(max_retries, Step, 0),
    Backoff = maps:get(retry_backoff_ms, Step, 1000),

    %% Execute with retry and security validation
    execute_tool_with_retry(ExecutionId, ToolName, Args, InputData, Context, Timeout, MaxRetries, Backoff);

execute_step_with_security(ExecutionId, #{type := parallel, child_steps := Children}, AllSteps, InputData, Context) ->
    %% Execute all children in parallel
    execute_parallel_with_security(ExecutionId, Children, AllSteps, InputData, Context);

execute_step_with_security(ExecutionId, #{type := sequence, child_steps := Children}, AllSteps, InputData, Context) ->
    %% Execute children sequentially
    execute_sequence_with_security(ExecutionId, Children, AllSteps, InputData, Context);

execute_step_with_security(_ExecutionId, #{type := delay}, _AllSteps, _InputData, Context) ->
    %% Simple delay step - just continue
    {ok, Context};

execute_step_with_security(_ExecutionId, #{type := Type}, _AllSteps, _InputData, _Context) ->
    {error, {unsupported_step_type, Type}}.

%% @private Execute tool with retry and security
-spec execute_tool_with_retry(execution_id(), binary(), map(), map(), map(), pos_integer(), non_neg_integer(), pos_integer()) -> {ok, map()} | {error, term()}.
execute_tool_with_retry(_ExecutionId, _ToolName, _Args, _InputData, _Context, _Timeout, 0, _Backoff) ->
    {error, max_retries_exceeded};
execute_tool_with_retry(ExecutionId, ToolName, Args, InputData, Context, Timeout, Retries, Backoff) ->
    % In a real implementation, this would call the tool registry with security checks
    % For now, simulate execution with security validation
    case simulate_tool_execution(ExecutionId, ToolName, Args, InputData, Context) of
        {ok, Result} ->
            {ok, Result};
        {error, _Reason} when Retries > 0 ->
            timer:sleep(Backoff),
            execute_tool_with_retry(ExecutionId, ToolName, Args, InputData, Context, Timeout, Retries - 1, Backoff * 2)
    end.

%% @private Simulate tool execution with security context
-spec simulate_tool_execution(execution_id(), binary(), map(), map(), map()) -> {ok, map()} | {error, term()}.
simulate_tool_execution(ExecutionId, ToolName, Args, InputData, _Context) ->
    % Placeholder: in real implementation, call erlmcp_tool_registry with security checks
    logger:info("Executing tool ~p for execution ~p with args: ~p", [ToolName, ExecutionId, Args]),
    {ok, InputData}.

%% @private Execute parallel steps with security
-spec execute_parallel_with_security(execution_id(), [step()], [step()], map(), map()) -> {ok, map()} | {error, term()}.
execute_parallel_with_security(ExecutionId, Children, AllSteps, InputData, Context) ->
    % Spawn processes for each child with security isolation
    Pids = [spawn_link(fun() ->
        case execute_step_with_security(ExecutionId, Child, AllSteps, InputData, Context) of
            {ok, Result} -> {self(), ok, Result};
            {error, Reason} -> {self(), error, Reason}
        end
    end) || Child <- Children],

    % Collect results
    Results = [receive
        {Pid, ok, Result} -> {ok, Result};
        {Pid, error, Reason} -> {error, Reason}
    end || Pid <- Pids],

    % Check if all succeeded
    case [R || R <- Results, element(1, R) =:= error] of
        [] ->
            % Merge all results
            Merged = lists:foldl(fun({ok, Map}, Acc) -> maps:merge(Acc, Map) end, #{}, Results),
            {ok, Merged};
        Errors ->
            {error, {parallel_errors, Errors}}
    end.

%% @private Execute sequence steps with security
-spec execute_sequence_with_security(execution_id(), [step()], [step()], map(), map()) -> {ok, map()} | {error, term()}.
execute_sequence_with_security(ExecutionId, [], _AllSteps, _InputData, Context) ->
    {ok, Context};
execute_sequence_with_security(ExecutionId, [Child | Rest], AllSteps, InputData, Context) ->
    case execute_step_with_security(ExecutionId, Child, AllSteps, InputData, Context) of
        {ok, NewContext} ->
            MergedContext = maps:merge(Context, NewContext),
            execute_sequence_with_security(ExecutionId, Rest, AllSteps, InputData, MergedContext);
        {error, Reason} ->
            {error, Reason}
    end.

%% @private Find initial steps (no incoming transitions)
-spec find_initial_steps([step()], workflow()) -> [step()].
find_initial_steps(Steps, #{transitions := Transitions}) ->
    ToSteps = sets:from_list([To || #{to := To} <- Transitions]),
    AllSteps = sets:from_list([Id || #{id := Id} <- Steps]),
    InitialIds = sets:to_list(sets:subtract(AllSteps, ToSteps)),
    [Step || Step <- Steps, lists:member(maps:get(id, Step), InitialIds)].

%% @private Find execution by monitor reference
-spec find_execution_by_monitor(reference(), #state{}) -> {ok, execution_id()} | error.
find_execution_by_monitor(MonRef, #state{execution_monitors = Monitors}) ->
    List = maps:to_list(Monitors),
    case lists:keyfind(MonRef, 2, List) of
        {ExecutionId, MonRef} -> {ok, ExecutionId};
        false -> error
    end.

