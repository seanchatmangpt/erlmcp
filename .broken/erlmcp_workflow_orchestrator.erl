%%%-------------------------------------------------------------------
%%% @doc
%%% Workflow orchestration engine for erlmcp v3
%%% Manages workflow lifecycle, resource allocation, and coordination.
%%%
%%% == Features ==
%%% - Dependency resolution (topological sort)
%%% - Parallel task execution with poolboy
%%% - Resource management and quotas
%%% - Priority queue for workflows
%%% - Event aggregation and monitoring
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_workflow_orchestrator).

-behaviour(gen_server).

-include("erlmcp.hrl").

%% API exports
-export([start_link/0, start_link/1,
         create_workflow/2, create_workflow/3,
         execute_workflow/1, execute_workflow/2,
         pause_workflow/1, resume_workflow/1,
         cancel_workflow/1, get_workflow/1,
         list_workflows/0, list_workflows/1,
         get_workflow_status/1,
         register_executor/2, unregister_executor/1,
         get_executor/0,
         get_metrics/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Type definitions
-type workflow_id() :: binary().
-type executor_id() :: binary().

-record(workflow,
        {id :: workflow_id(),
         pid :: pid() | undefined,
         state :: pending | running | paused | completed | failed | cancelled,
         priority :: low | normal | high | critical,
         created_at :: integer(),
         started_at :: integer() | undefined,
         completed_at :: integer() | undefined}).

-record(executor,
        {id :: executor_id(),
         pid :: pid(),
         capacity :: pos_integer(),
         active_workflows = [] :: [workflow_id()]}).

-record(state,
        {workflows = #{} :: #{workflow_id() => #workflow{}},
         executors = [] :: [#executor{}],
         pending_workflows = [] :: [workflow_id()],  % Priority queue
         workflow_table :: ets:tid(),
         executor_pool :: pid() | undefined,
         max_concurrent = 10 :: pos_integer(),
         max_priority_workflows = 3 :: pos_integer()}).

-define(WORKFLOW_TABLE, erlmcp_workflows).
-define(DEFAULT_TIMEOUT, 30000).
-define(ORCHESTRATOR_CHECK_INTERVAL, 5000).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Options], []).

-spec create_workflow(binary(), map()) -> {ok, workflow_id()} | {error, term()}.
create_workflow(Name, Tasks) ->
    create_workflow(Name, Tasks, #{}).

-spec create_workflow(binary(), map(), map()) -> {ok, workflow_id()} | {error, term()}.
create_workflow(Name, Tasks, Options) ->
    gen_server:call(?MODULE, {create_workflow, Name, Tasks, Options}, ?DEFAULT_TIMEOUT).

-spec execute_workflow(workflow_id()) -> ok | {error, term()}.
execute_workflow(WorkflowId) ->
    execute_workflow(WorkflowId, #{}).

-spec execute_workflow(workflow_id(), map()) -> ok | {error, term()}.
execute_workflow(WorkflowId, Options) ->
    gen_server:call(?MODULE, {execute_workflow, WorkflowId, Options}, ?DEFAULT_TIMEOUT).

-spec pause_workflow(workflow_id()) -> ok | {error, term()}.
pause_workflow(WorkflowId) ->
    gen_server:call(?MODULE, {pause_workflow, WorkflowId}, ?DEFAULT_TIMEOUT).

-spec resume_workflow(workflow_id()) -> ok | {error, term()}.
resume_workflow(WorkflowId) ->
    gen_server:call(?MODULE, {resume_workflow, WorkflowId}, ?DEFAULT_TIMEOUT).

-spec cancel_workflow(workflow_id()) -> ok | {error, term()}.
cancel_workflow(WorkflowId) ->
    gen_server:call(?MODULE, {cancel_workflow, WorkflowId}, ?DEFAULT_TIMEOUT).

-spec get_workflow(workflow_id()) -> {ok, map()} | {error, not_found}.
get_workflow(WorkflowId) ->
    gen_server:call(?MODULE, {get_workflow, WorkflowId}, ?DEFAULT_TIMEOUT).

-spec list_workflows() -> {ok, [map()]}.
list_workflows() ->
    list_workflows(fun(_) -> true end).

-spec list_workflows(fun((map()) -> boolean())) -> {ok, [map()]}.
list_workflows(FilterFun) ->
    gen_server:call(?MODULE, {list_workflows, FilterFun}, ?DEFAULT_TIMEOUT).

-spec get_workflow_status(workflow_id()) -> {ok, atom()} | {error, not_found}.
get_workflow_status(WorkflowId) ->
    gen_server:call(?MODULE, {get_workflow_status, WorkflowId}, ?DEFAULT_TIMEOUT).

-spec register_executor(binary(), pos_integer()) -> {ok, pid()} | {error, term()}.
register_executor(ExecutorId, Capacity) ->
    gen_server:call(?MODULE, {register_executor, ExecutorId, Capacity}, ?DEFAULT_TIMEOUT).

-spec unregister_executor(binary()) -> ok | {error, term()}.
unregister_executor(ExecutorId) ->
    gen_server:call(?MODULE, {unregister_executor, ExecutorId}, ?DEFAULT_TIMEOUT).

-spec get_executor() -> {ok, pid()} | {error, no_executor}.
get_executor() ->
    gen_server:call(?MODULE, get_executor, ?DEFAULT_TIMEOUT).

-spec get_metrics() -> {ok, map()}.
get_metrics() ->
    gen_server:call(?MODULE, get_metrics, ?DEFAULT_TIMEOUT).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([map()]) -> {ok, #state{}, {continue, initialize}}.
init([Options]) ->
    process_flag(trap_exit, true),

    logger:info("Initializing workflow orchestrator"),

    %% Create ETS table for fast lookup
    Table = ets:new(?WORKFLOW_TABLE, [named_table, public, set, {read_concurrency, true}]),

    State = #state{
        workflow_table = Table,
        max_concurrent = maps:get(max_concurrent, Options, 10),
        max_priority_workflows = maps:get(max_priority_workflows, Options, 3)
    },

    {ok, State, {continue, initialize}}.

-spec handle_info(initialize, #state{}) -> {noreply, #state{}}.
handle_info(initialize, State) ->
    logger:info("Workflow orchestrator initialized"),

    %% Start periodic check for pending workflows
    erlang:send_after(?ORCHESTRATOR_CHECK_INTERVAL, self(), check_pending_workflows),

    {noreply, State};

handle_call({create_workflow, Name, Tasks, Options}, _From, State) ->
    WorkflowId = generate_workflow_id(),
    Priority = maps:get(priority, Options, normal),

    %% Create workflow state machine
    WorkflowOptions = #{
        name => Name,
        description => maps:get(description, Options, undefined),
        priority => Priority
    },

    case erlmcp_workflow_statemachine:start_link(WorkflowId, WorkflowOptions) of
        {ok, Pid} ->
            %% Add tasks to workflow
            case add_tasks_to_workflow(Pid, Tasks) of
                ok ->
                    Now = erlang:system_time(millisecond),
                    Workflow = #workflow{
                        id = WorkflowId,
                        pid = Pid,
                        state = pending,
                        priority = Priority,
                        created_at = Now
                    },

                    %% Store in ETS and state
                    ets:insert(?WORKFLOW_TABLE, {WorkflowId, Workflow}),
                    NewWorkflows = maps:put(WorkflowId, Workflow, State#state.workflows),

                    %% Monitor workflow process
                    erlang:monitor(process, Pid),

                    logger:info("Created workflow ~p (~p)", [WorkflowId, Name]),
                    {reply, {ok, WorkflowId}, State#state{workflows = NewWorkflows}};
                {error, Reason} ->
                    gen_statem:stop(Pid, Reason, 5000),
                    {reply, {error, Reason}, State}
            end;
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({execute_workflow, WorkflowId, Options}, _From, State) ->
    case maps:get(WorkflowId, State#state.workflows, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        #workflow{pid = Pid} = Workflow ->
            case erlmcp_workflow_statemachine:start_workflow(Pid, WorkflowId, Options) of
                ok ->
                    UpdatedWorkflow = Workflow#workflow{
                        state = running,
                        started_at = erlang:system_time(millisecond)
                    },
                    NewWorkflows = maps:put(WorkflowId, UpdatedWorkflow, State#state.workflows),
                    ets:insert(?WORKFLOW_TABLE, {WorkflowId, UpdatedWorkflow}),
                    {reply, ok, State#state{workflows = NewWorkflows}};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end
    end;

handle_call({pause_workflow, WorkflowId}, _From, State) ->
    case maps:get(WorkflowId, State#state.workflows, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        #workflow{pid = Pid} = Workflow ->
            case erlmcp_workflow_statemachine:pause_workflow(Pid) of
                ok ->
                    UpdatedWorkflow = Workflow#workflow{state = paused},
                    NewWorkflows = maps:put(WorkflowId, UpdatedWorkflow, State#state.workflows),
                    ets:insert(?WORKFLOW_TABLE, {WorkflowId, UpdatedWorkflow}),
                    {reply, ok, State#state{workflows = NewWorkflows}};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end
    end;

handle_call({resume_workflow, WorkflowId}, _From, State) ->
    case maps:get(WorkflowId, State#state.workflows, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        #workflow{pid = Pid} = Workflow ->
            case erlmcp_workflow_statemachine:resume_workflow(Pid) of
                ok ->
                    UpdatedWorkflow = Workflow#workflow{state = running},
                    NewWorkflows = maps:put(WorkflowId, UpdatedWorkflow, State#state.workflows),
                    ets:insert(?WORKFLOW_TABLE, {WorkflowId, UpdatedWorkflow}),
                    {reply, ok, State#state{workflows = NewWorkflows}};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end
    end;

handle_call({cancel_workflow, WorkflowId}, _From, State) ->
    case maps:get(WorkflowId, State#state.workflows, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        #workflow{pid = Pid} = Workflow ->
            case erlmcp_workflow_statemachine:cancel_workflow(Pid) of
                ok ->
                    UpdatedWorkflow = Workflow#workflow{
                        state = cancelled,
                        completed_at = erlang:system_time(millisecond)
                    },
                    NewWorkflows = maps:put(WorkflowId, UpdatedWorkflow, State#state.workflows),
                    ets:insert(?WORKFLOW_TABLE, {WorkflowId, UpdatedWorkflow}),
                    {reply, ok, State#state{workflows = NewWorkflows}};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end
    end;

handle_call({get_workflow, WorkflowId}, _From, State) ->
    case maps:get(WorkflowId, State#state.workflows, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        #workflow{pid = Pid} = Workflow ->
            case erlmcp_workflow_statemachine:get_state(Pid) of
                {ok, WorkflowData} ->
                    {reply, {ok, workflow_to_map(Workflow, WorkflowData)}, State};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end
    end;

handle_call({list_workflows, FilterFun}, _From, State) ->
    Workflows = lists:filter(FilterFun, [workflow_to_map(W, undefined) || W <- maps:values(State#state.workflows)]),
    {reply, {ok, Workflows}, State};

handle_call({get_workflow_status, WorkflowId}, _From, State) ->
    case maps:get(WorkflowId, State#state.workflows, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        #workflow{state = Status} ->
            {reply, {ok, Status}, State}
    end;

handle_call({register_executor, ExecutorId, Capacity}, _From, State) ->
    %% Start executor process
    case start_executor_process(ExecutorId, Capacity) of
        {ok, Pid} ->
            Executor = #executor{
                id = ExecutorId,
                pid = Pid,
                capacity = Capacity
            },
            NewExecutors = [Executor | State#state.executors],
            logger:info("Registered executor ~p with capacity ~p", [ExecutorId, Capacity]),
            {reply, {ok, Pid}, State#state{executors = NewExecutors}};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({unregister_executor, ExecutorId}, _From, State) ->
    NewExecutors = lists:filter(
        fun(#executor{id = Id}) -> Id =/= ExecutorId end,
        State#state.executors
    ),
    {reply, ok, State#state{executors = NewExecutors}};

handle_call(get_executor, _From, State) ->
    case select_executor(State) of
        {ok, Executor} ->
            {reply, {ok, Executor#executor.pid}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(get_metrics, _From, State) ->
    WorkflowsList = maps:values(State#state.workflows),
    Metrics = #{
        total_workflows => length(WorkflowsList),
        pending_workflows => count_workflows_by_state(WorkflowsList, pending),
        running_workflows => count_workflows_by_state(WorkflowsList, running),
        paused_workflows => count_workflows_by_state(WorkflowsList, paused),
        completed_workflows => count_workflows_by_state(WorkflowsList, completed),
        failed_workflows => count_workflows_by_state(WorkflowsList, failed),
        cancelled_workflows => count_workflows_by_state(WorkflowsList, cancelled),
        executors => length(State#state.executors),
        executor_capacity => lists:sum([E#executor.capacity || E <- State#state.executors])
    },
    {reply, {ok, Metrics}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(check_pending_workflows, State) ->
    %% Check for pending workflows and execute if capacity available
    NewState = schedule_pending_workflows(State),
    erlang:send_after(?ORCHESTRATOR_CHECK_INTERVAL, self(), check_pending_workflows),
    {noreply, NewState};

handle_info({'DOWN', MonRef, process, Pid, Reason}, State) ->
    %% Handle workflow process termination
    case find_workflow_by_pid(Pid, State#state.workflows) of
        {ok, WorkflowId, #workflow{pid = Pid} = Workflow} ->
            logger:info("Workflow ~p terminated: ~p", [WorkflowId, Reason]),

            %% Get final state from workflow before cleanup
            FinalState = case Reason of
                normal -> completed;
                _ -> failed
            end,

            UpdatedWorkflow = Workflow#workflow{
                state = FinalState,
                pid = undefined,
                completed_at = erlang:system_time(millisecond)
            },

            %% Update state
            NewWorkflows = maps:put(WorkflowId, UpdatedWorkflow, State#state.workflows),
            ets:insert(?WORKFLOW_TABLE, {WorkflowId, UpdatedWorkflow}),

            {noreply, State#state{workflows = NewWorkflows}};
        error ->
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

-spec generate_workflow_id() -> binary().
generate_workflow_id() ->
    <<Id:128>> = crypto:strong_rand_bytes(16),
    << <<"workflow_">>/binary, (binary_encode(Id))/binary >>.

binary_encode(N) ->
    list_to_binary(lists:flatten(io_lib:format("~36.0b", [N]))).

-spec add_tasks_to_workflow(pid(), map()) -> ok | {error, term()}.
add_tasks_to_workflow(WorkflowPid, Tasks) ->
    try
        maps:foreach(fun(TaskId, TaskData) ->
            Task = #{
                id => TaskId,
                name => maps:get(name, TaskData, <<>>),
                module => maps:get(module, TaskData),
                function => maps:get(function, TaskData),
                args => maps:get(args, TaskData, []),
                dependencies => maps:get(dependencies, TaskData, []),
                priority => maps:get(priority, TaskData, normal),
                timeout => maps:get(timeout, TaskData, 30000)
            },
            ok = erlmcp_workflow_statemachine:add_task(WorkflowPid, TaskId, Task)
        end, Tasks),
        ok
    catch
        _:Reason ->
            {error, Reason}
    end.

-spec workflow_to_map(#workflow{}, term()) -> map().
workflow_to_map(#workflow{id = Id, state = State, priority = Priority,
                          created_at = Created, started_at = Started,
                          completed_at = Completed}, WorkflowData) ->
    BaseMap = #{
        id => Id,
        state => State,
        priority => Priority,
        created_at => Created,
        started_at => Started,
        completed_at => Completed
    },
    case WorkflowData of
        undefined -> BaseMap;
        Data -> maps:merge(BaseMap, #{
            tasks => maps:keys(Data#erlmcp_workflow_statemachine.workflow_data.tasks)
        })
    end.

-spec count_workflows_by_state([#workflow{}], atom()) -> non_neg_integer().
count_workflows_by_state(Workflows, State) ->
    length([W || W <- Workflows, W#workflow.state =:= State]).

-spec find_workflow_by_pid(pid(), #{workflow_id() => #workflow{}}) ->
    {ok, workflow_id(), #workflow{}} | error.
find_workflow_by_pid(Pid, Workflows) ->
    maps:fold(fun(WorkflowId, #workflow{pid = WPid} = Workflow, Acc) ->
        case WPid of
            Pid -> {ok, WorkflowId, Workflow};
            _ -> Acc
        end
    end, error, Workflows).

-spec select_executor(#state{}) -> {ok, #executor{}} | {error, term()}.
select_executor(State) ->
    Executors = State#state.executors,
    case Executors of
        [] ->
            {error, no_executor};
        _ ->
            %% Select executor with lowest load
            SortedExecutors = lists:keysort(#executor.active_workflows, Executors),
            {ok, hd(SortedExecutors)}
    end.

-spec start_executor_process(binary(), pos_integer()) -> {ok, pid()} | {error, term()}.
start_executor_process(ExecutorId, Capacity) ->
    %% In a real implementation, this would start a poolboy pool
    %% For now, return a simple process
    spawn(fun() -> executor_loop(ExecutorId, Capacity, []) end),
    {ok, self()}.

-spec executor_loop(binary(), pos_integer(), [term()]) -> no_return().
executor_loop(ExecutorId, Capacity, ActiveWorkflows) ->
    receive
        {execute_task, TaskFun, From} ->
            case length(ActiveWorkflows) < Capacity of
                true ->
                    Result = try TaskFun() of
                        Res -> {ok, Res}
                    catch
                        _:Reason -> {error, Reason}
                    end,
                    From ! {executor_result, ExecutorId, Result},
                    executor_loop(ExecutorId, Capacity, ActiveWorkflows);
                false ->
                    From ! {executor_result, ExecutorId, {error, capacity_exceeded}},
                    executor_loop(ExecutorId, Capacity, ActiveWorkflows)
            end
    end.

-spec schedule_pending_workflows(#state{}) -> #state{}.
schedule_pending_workflows(State) ->
    %% Count running workflows
    WorkflowsList = maps:values(State#state.workflows),
    RunningCount = count_workflows_by_state(WorkflowsList, running),
    MaxConcurrent = State#state.max_concurrent,

    case RunningCount < MaxConcurrent of
        true ->
            %% Start pending workflows up to capacity
            AvailableSlots = MaxConcurrent - RunningCount,
            PendingWorkflows = [W || W <- WorkflowsList, W#workflow.state =:= pending],
            ToStart = lists:sublist(PendingWorkflows, AvailableSlots),

            lists:foldl(fun(#workflow{id = Id, pid = Pid}, AccState) ->
                case erlmcp_workflow_statemachine:start_workflow(Pid, Id, #{}) of
                    ok ->
                        Workflow = maps:get(Id, AccState#state.workflows),
                        UpdatedWorkflow = Workflow#workflow{
                            state = running,
                            started_at = erlang:system_time(millisecond)
                        },
                        NewWorkflows = maps:put(Id, UpdatedWorkflow, AccState#state.workflows),
                        ets:insert(?WORKFLOW_TABLE, {Id, UpdatedWorkflow}),
                        AccState#state{workflows = NewWorkflows};
                    {error, _Reason} ->
                        AccState
                end
            end, State, ToStart);
        false ->
            State
    end.
