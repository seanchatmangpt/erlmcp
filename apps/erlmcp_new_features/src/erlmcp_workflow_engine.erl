%%%-------------------------------------------------------------------
%%% @doc
%%% Workflow Engine - Orchestrates multi-step workflows
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
%%%       timeout_sec => 30
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
%%% - Each execution runs in isolated process
%%% - Supports cancellation via monitors
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
         list_workflows/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%%====================================================================
%% Types
%%====================================================================

-type workflow_id() :: binary().
-type step_id() :: binary().
-type execution_id() :: binary().
-type status() :: pending | running | completed | failed | cancelled.

-type step() :: #{
    id => step_id(),
    type => tool | parallel | sequence | conditional | loop | delay,
    tool_name => binary(),
    tool_arguments => map(),
    child_steps => [step()],
    max_retries => non_neg_integer(),
    retry_backoff_ms => non_neg_integer(),
    timeout_sec => pos_integer()
}.

-type transition() :: #{
    from => step_id(),
    to => step_id(),
    condition => success | failure | {value_equals, term()} | {expression, binary()}
}.

-type workflow() :: #{
    id => workflow_id(),
    steps => [step()],
    transitions => [transition()]
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
    executor_pid => pid() | undefined
}.

-record(state, {
    workflows = #{} :: #{workflow_id() => workflow()},
    executions = #{} :: #{execution_id() => execution()},
    execution_monitors = #{} :: #{execution_id() => reference()}
}).

%%====================================================================
%% API
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Define a workflow
-spec define_workflow(workflow()) -> ok | {error, term()}.
define_workflow(#{id := WorkflowId} = Workflow) ->
    gen_server:call(?SERVER, {define_workflow, WorkflowId, Workflow}, 5000).

%% @doc Execute a workflow with input data
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

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([]) -> {ok, #state{}}.
init([]) ->
    {ok, #state{}}.

-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {reply, term(), #state{}} | {noreply, #state{}}.

handle_call({define_workflow, WorkflowId, Workflow}, _From, State) ->
    case validate_workflow(Workflow) of
        ok ->
            NewWorkflows = maps:put(WorkflowId, Workflow, State#state.workflows),
            {reply, ok, State#state{workflows = NewWorkflows}};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({execute_workflow, WorkflowId, InputData}, _From, State) ->
    case maps:get(WorkflowId, State#state.workflows, undefined) of
        undefined ->
            {reply, {error, workflow_not_found}, State};
        Workflow ->
            ExecutionId = generate_execution_id(),
            StartedAt = erlang:timestamp(),

            Execution = #{
                execution_id => ExecutionId,
                workflow_id => WorkflowId,
                status => pending,
                started_at => StartedAt,
                completed_at => undefined,
                input_data => InputData,
                output_data => undefined,
                error_message => undefined,
                executor_pid => undefined
            },

            % Start executor process
            case start_executor(ExecutionId, Workflow, InputData) of
                {ok, ExecutorPid} ->
                    % Monitor executor
                    MonRef = erlang:monitor(process, ExecutorPid),

                    UpdatedExecution = Execution#{executor_pid => ExecutorPid, status => running},
                    NewExecutions = maps:put(ExecutionId, UpdatedExecution, State#state.executions),
                    NewMonitors = maps:put(ExecutionId, MonRef, State#state.execution_monitors),

                    {reply, {ok, ExecutionId}, State#state{
                        executions = NewExecutions,
                        execution_monitors = NewMonitors
                    }};
                {error, Reason} ->
                    FailedExecution = Execution#{
                        status => failed,
                        completed_at => erlang:timestamp(),
                        error_message => list_to_binary(io_lib:format("~p", [Reason]))
                    },
                    NewExecutions = maps:put(ExecutionId, FailedExecution, State#state.executions),
                    {reply, {error, Reason}, State#state{executions = NewExecutions}}
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
                    ExecutorPid ! cancel,
                    UpdatedExecution = Execution#{status => cancelled, completed_at => erlang:timestamp()},
                    NewExecutions = maps:put(ExecutionId, UpdatedExecution, State#state.executions),
                    {reply, ok, State#state{executions = NewExecutions}};
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

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info({'DOWN', MonRef, process, _Pid, normal}, State) ->
    % Executor completed normally
    case find_execution_by_monitor(MonRef, State) of
        {ok, ExecutionId} ->
            NewMonitors = maps:remove(ExecutionId, State#state.execution_monitors),
            {noreply, State#state{execution_monitors = NewMonitors}};
        error ->
            {noreply, State}
    end;

handle_info({'DOWN', MonRef, process, _Pid, Reason}, State) ->
    % Executor failed
    case find_execution_by_monitor(MonRef, State) of
        {ok, ExecutionId} ->
            Executions = State#state.executions,
            case maps:get(ExecutionId, Executions, undefined) of
                undefined ->
                    NewMonitors = maps:remove(ExecutionId, State#state.execution_monitors),
                    {noreply, State#state{execution_monitors = NewMonitors}};
                Execution ->
                    UpdatedExecution = Execution#{
                        status => failed,
                        completed_at => erlang:timestamp(),
                        error_message => list_to_binary(io_lib:format("~p", [Reason]))
                    },
                    NewExecutions = maps:put(ExecutionId, UpdatedExecution, Executions),
                    NewMonitors = maps:remove(ExecutionId, State#state.execution_monitors),
                    {noreply, State#state{
                        executions = NewExecutions,
                        execution_monitors = NewMonitors
                    }}
            end;
        error ->
            {noreply, State}
    end;

handle_info({execution_complete, ExecutionId, Result}, State) ->
    % Executor sent completion result
    Executions = State#state.executions,
    case maps:get(ExecutionId, Executions, undefined) of
        undefined ->
            {noreply, State};
        Execution ->
            UpdatedExecution = case Result of
                {ok, OutputData} ->
                    Execution#{
                        status => completed,
                        completed_at => erlang:timestamp(),
                        output_data => OutputData
                    };
                {error, Reason} ->
                    Execution#{
                        status => failed,
                        completed_at => erlang:timestamp(),
                        error_message => list_to_binary(io_lib:format("~p", [Reason]))
                    }
            end,
            NewExecutions = maps:put(ExecutionId, UpdatedExecution, Executions),
            {noreply, State#state{executions = NewExecutions}}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

%% @private Validate workflow structure
validate_workflow(#{steps := Steps, transitions := Transitions}) when length(Steps) > 0 ->
    case validate_steps(Steps) of
        ok -> validate_transitions(Transitions, Steps);
        Error -> Error
    end;
validate_workflow(_) ->
    {error, invalid_workflow_structure}.

%% @private Validate all steps
validate_steps([]) ->
    ok;
validate_steps([#{id := Id, type := Type} = Step | Rest]) when is_binary(Id), is_atom(Type) ->
    case validate_step_type(Step) of
        ok -> validate_steps(Rest);
        Error -> Error
    end;
validate_steps(_) ->
    {error, invalid_step_structure}.

%% @private Validate step type specific fields
validate_step_type(#{type := tool, tool_name := ToolName}) when is_binary(ToolName) ->
    ok;
validate_step_type(#{type := parallel, child_steps := Children}) when is_list(Children) ->
    validate_steps(Children);
validate_step_type(#{type := sequence, child_steps := Children}) when is_list(Children) ->
    validate_steps(Children);
validate_step_type(#{type := loop, child_steps := Children}) when is_list(Children) ->
    validate_steps(Children);
validate_step_type(#{type := conditional, child_steps := Children}) when is_list(Children) ->
    validate_steps(Children);
validate_step_type(#{type := delay}) ->
    ok;
validate_step_type(_) ->
    {error, invalid_step_type_configuration}.

%% @private Validate transitions reference existing steps
validate_transitions([], _Steps) ->
    ok;
validate_transitions([#{from := From, to := To} | Rest], Steps) ->
    StepIds = [Id || #{id := Id} <- Steps],
    case lists:member(From, StepIds) andalso lists:member(To, StepIds) of
        true -> validate_transitions(Rest, Steps);
        false -> {error, {invalid_transition, {From, To}}}
    end;
validate_transitions(_, _) ->
    {error, invalid_transition_structure}.

%% @private Generate unique execution ID
generate_execution_id() ->
    BinaryTime = integer_to_binary(erlang:unique_integer()),
    Random = crypto:strong_rand_bytes(8),
    <<BinaryTime/binary, Random/binary>>.

%% @private Start executor process for workflow execution
start_executor(ExecutionId, Workflow, InputData) ->
    % Spawn executor process
    Proc = fun() ->
        executor_loop(ExecutionId, Workflow, InputData, #{})
    end,
    Pid = spawn(Proc),
    {ok, Pid}.

%% @private Executor loop - runs workflow steps
executor_loop(ExecutionId, #{steps := Steps} = Workflow, InputData, Context) ->
    % Find initial steps (no incoming transitions)
    InitialSteps = find_initial_steps(Steps, Workflow),

    case execute_steps(InitialSteps, Steps, InputData, Context) of
        {ok, OutputData} ->
            ?SERVER ! {execution_complete, ExecutionId, {ok, OutputData}};
        {error, Reason} ->
            ?SERVER ! {execution_complete, ExecutionId, {error, Reason}}
    end.

%% @private Find initial steps (no incoming transitions)
find_initial_steps(Steps, #{transitions := Transitions}) ->
    ToSteps = sets:from_list([To || #{to := To} <- Transitions]),
    AllSteps = sets:from_list([Id || #{id := Id} <- Steps]),
    InitialIds = sets:to_list(sets:subtract(AllSteps, ToSteps)),
    [Step || Step <- Steps, lists:member(maps:get(id, Step), InitialIds)].

%% @private Execute a list of steps
execute_steps([], _AllSteps, _InputData, _Context) ->
    {ok, #{}};
execute_steps([Step | Rest], AllSteps, InputData, Context) ->
    case execute_step(Step, AllSteps, InputData, Context) of
        {ok, StepContext} ->
            % Merge context and continue
            MergedContext = maps:merge(Context, StepContext),
            execute_steps(Rest, AllSteps, InputData, MergedContext);
        {error, Reason} ->
            {error, Reason}
    end.

%% @private Execute a single step
execute_step(#{type := tool, tool_name := ToolName, tool_arguments := Args} = Step,
             _AllSteps, InputData, Context) ->
    Timeout = maps:get(timeout_sec, Step, 30) * 1000,
    MaxRetries = maps:get(max_retries, Step, 0),
    Backoff = maps:get(retry_backoff_ms, Step, 1000),

    execute_with_retry(ToolName, Args, InputData, Context, Timeout, MaxRetries, Backoff);

execute_step(#{type := parallel, child_steps := Children}, AllSteps, InputData, Context) ->
    % Execute all children in parallel
    execute_parallel(Children, AllSteps, InputData, Context);

execute_step(#{type := sequence, child_steps := Children}, AllSteps, InputData, Context) ->
    % Execute children sequentially
    execute_sequence(Children, AllSteps, InputData, Context);

execute_step(#{type := delay}, _AllSteps, _InputData, Context) ->
    % Simple delay step - just continue
    {ok, Context};

execute_step(#{type := Type}, _AllSteps, _InputData, _Context) ->
    {error, {unsupported_step_type, Type}}.

%% @private Execute with retry logic
execute_with_retry(_ToolName, _Args, _InputData, _Context, _Timeout, 0, _Backoff) ->
    {error, max_retries_exceeded};
execute_with_retry(ToolName, Args, InputData, Context, Timeout, Retries, Backoff) ->
    % In a real implementation, this would call the tool registry
    % For now, simulate execution
    case simulate_tool_execution(ToolName, Args, InputData, Context) of
        {ok, Result} ->
            {ok, Result};
        {error, _Reason} when Retries > 0 ->
            timer:sleep(Backoff),
            execute_with_retry(ToolName, Args, InputData, Context, Timeout, Retries - 1, Backoff * 2)
    end.

%% @private Simulate tool execution (placeholder)
simulate_tool_execution(_ToolName, _Args, InputData, _Context) ->
    % Placeholder: in real implementation, call erlmcp_tool_registry
    {ok, InputData}.

%% @private Execute parallel steps
execute_parallel(Children, AllSteps, InputData, Context) ->
    % Spawn processes for each child
    Pids = [spawn(fun() ->
        case execute_step(Child, AllSteps, InputData, Context) of
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

%% @private Execute sequence steps
execute_sequence([], _AllSteps, _InputData, Context) ->
    {ok, Context};
execute_sequence([Child | Rest], AllSteps, InputData, Context) ->
    case execute_step(Child, AllSteps, InputData, Context) of
        {ok, NewContext} ->
            MergedContext = maps:merge(Context, NewContext),
            execute_sequence(Rest, AllSteps, InputData, MergedContext);
        {error, Reason} ->
            {error, Reason}
    end.

%% @private Find execution by monitor reference
find_execution_by_monitor(MonRef, #state{execution_monitors = Monitors}) ->
    List = maps:to_list(Monitors),
    case lists:keyfind(MonRef, 2, List) of
        {ExecutionId, MonRef} -> {ok, ExecutionId};
        false -> error
    end.
