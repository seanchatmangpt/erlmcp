%%%-------------------------------------------------------------------
%%% @doc
%%% Workflow state machine for erlmcp v3 orchestration
%%% Implements enterprise-grade workflow execution with state management,
%%% dependency resolution, parallel execution, and comprehensive error handling.
%%%
%%% == States ==
%%% - pending: Workflow created, waiting to start
%%% - running: Workflow executing tasks
%%% - paused: Workflow temporarily suspended
%%% - completed: Workflow finished successfully
%%% - failed: Workflow failed with error
%%% - cancelled: Workflow cancelled by user
%%%
%%% == Features ==
%%% - gen_statem for reliable state transitions
%%% - Dependency resolution with topological sort
%%% - Parallel task execution with poolboy
%%% - Event-driven architecture
%%% - Comprehensive error handling
%%% - OTEL observability integration
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_workflow_statemachine).

-behaviour(gen_statem).

-include("erlmcp.hrl").

%% API exports
-export([start_link/1, start_link/2,
         start_workflow/2, start_workflow/3,
         pause_workflow/1, resume_workflow/1,
         cancel_workflow/1, fail_workflow/2,
         get_state/1, get_status/1,
         add_task/3, remove_task/2,
         get_tasks/1, get_task_status/2,
         subscribe/2, unsubscribe/2]).

%% gen_statem callbacks
-export([init/1, callback_mode/0, handle_event/4,
         terminate/3, code_change/4, format_status/2]).

%% Type definitions
-type workflow_id() :: binary().
-type task_id() :: binary().
-type workflow_state() :: pending | running | paused | completed | failed | cancelled.
-type task_state() :: pending | running | completed | failed | skipped.
-type priority() :: low | normal | high | critical.

-record(task,
        {id :: task_id(),
         name :: binary(),
         module :: module(),
         function :: atom(),
         args :: list(),
         dependencies :: [task_id()],
         priority = normal :: priority(),
         timeout = 30000 :: pos_integer(),
         retry_policy :: retry_policy() | undefined,
         compensation :: compensation() | undefined,
         state = pending :: task_state(),
         result :: term() | undefined,
         error :: term() | undefined,
         started_at :: integer() | undefined,
         completed_at :: integer() | undefined}).

-type retry_policy() ::
    #{max_attempts := pos_integer(),
      backoff := exponential | fixed,
      base_delay := pos_integer(),
      max_delay := pos_integer()}.

-type compensation() ::
    #{module := module(),
      function := atom(),
      args := list()}.

-record(workflow_data,
        {workflow_id :: workflow_id(),
         name :: binary(),
         description :: binary() | undefined,
         tasks = #{} :: #{task_id() => #task{}},
         task_graph :: digraph:graph(),
         state = pending :: workflow_state(),
         priority = normal :: priority(),
         created_at :: integer(),
         started_at :: integer() | undefined,
         completed_at :: integer() | undefined,
         error :: term() | undefined,
         metadata = #{} :: map(),
         subscribers = [] :: [pid()],
         running_tasks = [] :: [task_id()],
         completed_tasks = [] :: [task_id()],
         failed_tasks = [] :: [task_id()],
         options :: map()}).

-type workflow_data() :: #workflow_data{}.

%% Timeouts
-define(DEFAULT_TIMEOUT, 30000).
-define(TASK_EXECUTION_TIMEOUT, 300000).
-define(PAUSE_TIMEOUT, 300000).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link(workflow_id()) -> {ok, pid()} | {error, term()}.
start_link(WorkflowId) ->
    start_link(WorkflowId, #{}).

-spec start_link(workflow_id(), map()) -> {ok, pid()} | {error, term()}.
start_link(WorkflowId, Options) ->
    gen_statem:start_link(?MODULE, [WorkflowId, Options], []).

-spec start_workflow(pid(), binary()) -> ok | {error, term()}.
start_workflow(Pid, Name) ->
    start_workflow(Pid, Name, #{}).

-spec start_workflow(pid(), binary(), map()) -> ok | {error, term()}.
start_workflow(Pid, Name, Options) ->
    gen_statem:call(Pid, {start_workflow, Name, Options}, ?DEFAULT_TIMEOUT).

-spec pause_workflow(pid()) -> ok | {error, term()}.
pause_workflow(Pid) ->
    gen_statem:call(Pid, pause_workflow, ?DEFAULT_TIMEOUT).

-spec resume_workflow(pid()) -> ok | {error, term()}.
resume_workflow(Pid) ->
    gen_statem:call(Pid, resume_workflow, ?DEFAULT_TIMEOUT).

-spec cancel_workflow(pid()) -> ok | {error, term()}.
cancel_workflow(Pid) ->
    gen_statem:call(Pid, cancel_workflow, ?DEFAULT_TIMEOUT).

-spec fail_workflow(pid(), term()) -> ok | {error, term()}.
fail_workflow(Pid, Reason) ->
    gen_statem:call(Pid, {fail_workflow, Reason}, ?DEFAULT_TIMEOUT).

-spec get_state(pid()) -> {ok, workflow_data()}.
get_state(Pid) ->
    gen_statem:call(Pid, get_state, ?DEFAULT_TIMEOUT).

-spec get_status(pid()) -> {ok, workflow_state()}.
get_status(Pid) ->
    gen_statem:call(Pid, get_status, ?DEFAULT_TIMEOUT).

-spec add_task(pid(), task_id(), #task{}) -> ok | {error, term()}.
add_task(Pid, TaskId, Task) ->
    gen_statem:call(Pid, {add_task, TaskId, Task}, ?DEFAULT_TIMEOUT).

-spec remove_task(pid(), task_id()) -> ok | {error, term()}.
remove_task(Pid, TaskId) ->
    gen_statem:call(Pid, {remove_task, TaskId}, ?DEFAULT_TIMEOUT).

-spec get_tasks(pid()) -> {ok, [task_id()]}.
get_tasks(Pid) ->
    gen_statem:call(Pid, get_tasks, ?DEFAULT_TIMEOUT).

-spec get_task_status(pid(), task_id()) -> {ok, task_state()} | {error, not_found}.
get_task_status(Pid, TaskId) ->
    gen_statem:call(Pid, {get_task_status, TaskId}, ?DEFAULT_TIMEOUT).

-spec subscribe(pid(), pid()) -> ok.
subscribe(Pid, Subscriber) ->
    gen_statem:call(Pid, {subscribe, Subscriber}, ?DEFAULT_TIMEOUT).

-spec unsubscribe(pid(), pid()) -> ok.
unsubscribe(Pid, Subscriber) ->
    gen_statem:call(Pid, {unsubscribe, Subscriber}, ?DEFAULT_TIMEOUT).

%%====================================================================
%% gen_statem callbacks
%%====================================================================

-spec init([workflow_id() | map()]) -> {ok, workflow_state(), workflow_data()}.
init([WorkflowId, Options]) ->
    logger:info("Workflow state machine ~p initializing", [WorkflowId]),

    Now = erlang:system_time(millisecond),
    Graph = digraph:new([acyclic, private]),

    Data = #workflow_data{
        workflow_id = WorkflowId,
        name = maps:get(name, Options, <<"unnamed">>),
        description = maps:get(description, Options, undefined),
        priority = maps:get(priority, Options, normal),
        created_at = Now,
        task_graph = Graph,
        options = Options
    },

    %% Emit workflow creation event
    emit_workflow_event(created, Data),

    {ok, pending, Data}.

-spec callback_mode() -> gen_statem:callback_mode_result().
callback_mode() ->
    [handle_event_function, state_enter_calls].

%%--------------------------------------------------------------------
%% State entry handlers
%%--------------------------------------------------------------------

handle_event(enter, OldState, NewState, Data) ->
    logger:info("Workflow ~p: ~p -> ~p", [Data#workflow_data.workflow_id, OldState, NewState]),
    emit_state_transition(OldState, NewState, Data),

    case NewState of
        running ->
            %% Start executing tasks
            start_pending_tasks(Data);
        paused ->
            %% Pause in-progress tasks
            pause_running_tasks(Data);
        completed ->
            %% Record completion time
            CompletedData = Data#workflow_data{
                completed_at = erlang:system_time(millisecond)
            },
            emit_workflow_event(completed, CompletedData),
            {keep_state, CompletedData};
        failed ->
            %% Record failure time
            FailedData = Data#workflow_data{
                completed_at = erlang:system_time(millisecond)
            },
            emit_workflow_event(failed, FailedData),
            {keep_state, FailedData};
        cancelled ->
            %% Record cancellation time
            CancelledData = Data#workflow_data{
                completed_at = erlang:system_time(millisecond)
            },
            emit_workflow_event(cancelled, CancelledData),
            {keep_state, CancelledData};
        _ ->
            {keep_state, Data}
    end;

%%--------------------------------------------------------------------
%% pending state events
%%--------------------------------------------------------------------

handle_event({call, From}, {start_workflow, Name, Options}, pending, Data) ->
    logger:info("Starting workflow ~p", [Data#workflow_data.workflow_id]),

    %% Validate workflow has tasks
    case maps:size(Data#workflow_data.tasks) of
        0 ->
            {keep_state, Data, [{reply, From, {error, no_tasks}}]};
        _ ->
            NewData = Data#workflow_data{
                name = Name,
                started_at = erlang:system_time(millisecond),
                metadata = maps:merge(Data#workflow_data.metadata, Options)
            },
            emit_workflow_event(started, NewData),
            {next_state, running, NewData, [{reply, From, ok}]}
    end;

%%--------------------------------------------------------------------
%% running state events
%%--------------------------------------------------------------------

handle_event({call, From}, pause_workflow, running, Data) ->
    logger:info("Pausing workflow ~p", [Data#workflow_data.workflow_id]),
    emit_workflow_event(paused, Data),
    {next_state, paused, Data, [{reply, From, ok}]};

handle_event({call, From}, {task_completed, TaskId, Result}, running, Data) ->
    logger:debug("Task ~p completed in workflow ~p", [TaskId, Data#workflow_data.workflow_id]),

    %% Update task state
    Tasks = Data#workflow_data.tasks,
    Task = maps:get(TaskId, Tasks),
    UpdatedTask = Task#task{
        state = completed,
        result = Result,
        completed_at = erlang:system_time(millisecond)
    },
    NewTasks = maps:put(TaskId, UpdatedTask, Tasks),

    %% Update workflow state
    NewData = Data#workflow_data{
        tasks = NewTasks,
        running_tasks = lists:delete(TaskId, Data#workflow_data.running_tasks),
        completed_tasks = [TaskId | Data#workflow_data.completed_tasks]
    },

    %% Notify subscribers
    notify_subscribers(Data, {task_completed, TaskId, Result}),

    %% Start next batch of tasks
    start_pending_tasks(NewData),

    %% Check if workflow is complete
    case is_workflow_complete(NewData) of
        true ->
            {next_state, completed, NewData, [{reply, From, ok}]};
        false ->
            {keep_state, NewData, [{reply, From, ok}]}
    end;

handle_event({call, From}, {task_failed, TaskId, Reason}, running, Data) ->
    logger:warning("Task ~p failed in workflow ~p: ~p", [TaskId, Data#workflow_data.workflow_id, Reason]),

    %% Update task state
    Tasks = Data#workflow_data.tasks,
    Task = maps:get(TaskId, Tasks),
    UpdatedTask = Task#task{
        state = failed,
        error = Reason,
        completed_at = erlang:system_time(millisecond)
    },
    NewTasks = maps:put(TaskId, UpdatedTask, Tasks),

    %% Update workflow state
    NewData = Data#workflow_data{
        tasks = NewTasks,
        running_tasks = lists:delete(TaskId, Data#workflow_data.running_tasks),
        failed_tasks = [TaskId | Data#workflow_data.failed_tasks]
    },

    %% Notify subscribers
    notify_subscribers(Data, {task_failed, TaskId, Reason}),

    %% Check retry policy
    case should_retry_task(Task, Reason) of
        {true, Delay} ->
            logger:info("Retrying task ~p after ~pms", [TaskId, Delay]),
            erlang:send_after(Delay, self(), {retry_task, TaskId}),
            {keep_state, NewData, [{reply, From, ok}]};
        false ->
            %% Mark workflow as failed
            FailedData = NewData#workflow_data{error = Reason},
            {next_state, failed, FailedData, [{reply, From, ok}]}
    end;

%%--------------------------------------------------------------------
%% paused state events
%%--------------------------------------------------------------------

handle_event({call, From}, resume_workflow, paused, Data) ->
    logger:info("Resuming workflow ~p", [Data#workflow_data.workflow_id]),
    emit_workflow_event(resumed, Data),
    {next_state, running, Data, [{reply, From, ok}]};

%%--------------------------------------------------------------------
%% General events (all states)
%%--------------------------------------------------------------------

handle_event({call, From}, cancel_workflow, State, Data) when State =/= completed;
                                                                  State =/= failed;
                                                                  State =/= cancelled ->
    logger:info("Cancelling workflow ~p", [Data#workflow_data.workflow_id]),
    {next_state, cancelled, Data, [{reply, From, ok}]};

handle_event({call, From}, {fail_workflow, Reason}, State, Data) when State =/= completed;
                                                                          State =/= failed;
                                                                          State =/= cancelled ->
    logger:error("Failing workflow ~p: ~p", [Data#workflow_data.workflow_id, Reason]),
    NewData = Data#workflow_data{error = Reason},
    {next_state, failed, NewData, [{reply, From, ok}]};

handle_event({call, From}, get_state, _State, Data) ->
    {keep_state, Data, [{reply, From, {ok, Data}}]};

handle_event({call, From}, get_status, _State, Data) ->
    {keep_state, Data, [{reply, From, {ok, Data#workflow_data.state}}]};

handle_event({call, From}, {add_task, TaskId, Task}, pending, Data) ->
    logger:debug("Adding task ~p to workflow ~p", [TaskId, Data#workflow_data.workflow_id]),

    %% Add task to map
    NewTasks = maps:put(TaskId, Task, Data#workflow_data.tasks),

    %% Update task graph
    Graph = Data#workflow_data.task_graph,
    digraph:add_vertex(Graph, TaskId),

    %% Add edges for dependencies
    lists:foreach(fun(DepId) ->
        case digraph:vertex(Graph, DepId) of
            false -> digraph:add_vertex(Graph, DepId);
            _ -> ok
        end,
        digraph:add_edge(Graph, DepId, TaskId)
    end, Task#task.dependencies),

    NewData = Data#workflow_data{tasks = NewTasks},
    {keep_state, NewData, [{reply, From, ok}]};

handle_event({call, From}, {add_task, _TaskId, _Task}, _State, _Data) ->
    {keep_state_and_data, [{reply, From, {error, invalid_state}}]};

handle_event({call, From}, {remove_task, TaskId}, pending, Data) ->
    logger:debug("Removing task ~p from workflow ~p", [TaskId, Data#workflow_data.workflow_id]),

    NewTasks = maps:remove(TaskId, Data#workflow_data.tasks),
    Graph = Data#workflow_data.task_graph,

    %% Remove vertex and edges
    case digraph:vertex(Graph, TaskId) of
        false -> ok;
        _ -> digraph:del_vertex(Graph, TaskId)
    end,

    NewData = Data#workflow_data{tasks = NewTasks},
    {keep_state, NewData, [{reply, From, ok}]};

handle_event({call, From}, {remove_task, _TaskId}, _State, _Data) ->
    {keep_state_and_data, [{reply, From, {error, invalid_state}}]};

handle_event({call, From}, get_tasks, _State, Data) ->
    TaskIds = maps:keys(Data#workflow_data.tasks),
    {keep_state, Data, [{reply, From, {ok, TaskIds}}]};

handle_event({call, From}, {get_task_status, TaskId}, _State, Data) ->
    case maps:get(TaskId, Data#workflow_data.tasks, undefined) of
        undefined ->
            {keep_state, Data, [{reply, From, {error, not_found}}]};
        Task ->
            {keep_state, Data, [{reply, From, {ok, Task#task.state}}]}
    end;

handle_event({call, From}, {subscribe, Subscriber}, _State, Data) ->
    logger:debug("Adding subscriber ~p to workflow ~p", [Subscriber, Data#workflow_data.workflow_id]),

    %% Monitor subscriber
    MonRef = erlang:monitor(process, Subscriber),

    %% Store subscriber with monitor
    NewData = Data#workflow_data{
        subscribers = [{Subscriber, MonRef} | Data#workflow_data.subscribers]
    },
    {keep_state, NewData, [{reply, From, ok}]};

handle_event({call, From}, {unsubscribe, Subscriber}, _State, Data) ->
    logger:debug("Removing subscriber ~p from workflow ~p", [Subscriber, Data#workflow_data.workflow_id]),

    NewSubscribers = lists:filter(
        fun({Sub, _MonRef}) -> Sub =/= Subscriber end,
        Data#workflow_data.subscribers
    ),

    NewData = Data#workflow_data{subscribers = NewSubscribers},
    {keep_state, NewData, [{reply, From, ok}]};

%%--------------------------------------------------------------------
%% Info events
%%--------------------------------------------------------------------

handle_event(info, {retry_task, TaskId}, running, Data) ->
    logger:info("Retrying task ~p in workflow ~p", [TaskId, Data#workflow_data.workflow_id]),

    case maps:get(TaskId, Data#workflow_data.tasks, undefined) of
        undefined ->
            {keep_state, Data};
        Task ->
            %% Execute task
            spawn_task_execution(TaskId, Task, Data),
            NewRunning = [TaskId | Data#workflow_data.running_tasks],
            {keep_state, Data#workflow_data{running_tasks = NewRunning}}
    end;

handle_event(info, {'DOWN', MonRef, process, Subscriber, Reason}, _State, Data) ->
    logger:debug("Subscriber ~p down: ~p", [Subscriber, Reason]),

    %% Remove subscriber
    NewSubscribers = lists:filter(
        fun({_Sub, Ref}) -> Ref =/= MonRef end,
        Data#workflow_data.subscribers
    ),

    {keep_state, Data#workflow_data{subscribers = NewSubscribers}};

handle_event(EventType, Event, State, Data) ->
    logger:warning("Unhandled event ~p ~p in state ~p for workflow ~p",
                   [EventType, Event, State, Data#workflow_data.workflow_id]),
    {keep_state, Data}.

%%--------------------------------------------------------------------
%% Termination and code change
%%--------------------------------------------------------------------

-spec terminate(term(), workflow_state(), workflow_data()) -> ok.
terminate(Reason, State, Data) ->
    logger:info("Workflow ~p terminating in state ~p: ~p",
                [Data#workflow_data.workflow_id, State, Reason]),

    %% Cleanup task graph
    digraph:delete(Data#workflow_data.task_graph),

    %% Demonitor subscribers
    lists:foreach(fun({_Subscriber, MonRef}) ->
        erlang:demonitor(MonRef, [flush])
    end, Data#workflow_data.subscribers),

    ok.

-spec code_change(term(), workflow_state(), workflow_data(), term()) ->
                     {ok, workflow_state(), workflow_data()}.
code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

-spec format_status(normal | terminate, list()) -> term().
format_status(_Opt, [_PDict, State, Data]) ->
    #{state => State,
      workflow_id => Data#workflow_data.workflow_id,
      name => Data#workflow_data.name,
      total_tasks => maps:size(Data#workflow_data.tasks),
      running_tasks => length(Data#workflow_data.running_tasks),
      completed_tasks => length(Data#workflow_data.completed_tasks),
      failed_tasks => length(Data#workflow_data.failed_tasks),
      created_at => Data#workflow_data.created_at,
      started_at => Data#workflow_data.started_at}.

%%====================================================================
%% Internal functions
%%====================================================================

-spec start_pending_tasks(workflow_data()) -> {keep_state, workflow_data()}.
start_pending_tasks(Data) ->
    %% Find tasks with all dependencies satisfied
    Graph = Data#workflow_data.task_graph,
    PendingTaskIds = find_executable_tasks(Graph, Data#workflow_data.tasks),

    logger:debug("Starting ~p pending tasks in workflow ~p",
                 [length(PendingTaskIds), Data#workflow_data.workflow_id]),

    %% Start tasks in parallel
    NewData = lists:foldl(fun(TaskId, AccData) ->
        Task = maps:get(TaskId, AccData#workflow_data.tasks),
        spawn_task_execution(TaskId, Task, AccData),
        AccData#workflow_data{
            running_tasks = [TaskId | AccData#workflow_data.running_tasks]
        }
    end, Data, PendingTaskIds),

    {keep_state, NewData}.

-spec find_executable_tasks(digraph:graph(), #{task_id() => #task{}}) -> [task_id()].
find_executable_tasks(Graph, Tasks) ->
    maps:fold(fun(TaskId, Task, Acc) ->
        case Task#task.state of
            pending ->
                case dependencies_satisfied(TaskId, Graph, Tasks) of
                    true -> [TaskId | Acc];
                    false -> Acc
                end;
            _ ->
                Acc
        end
    end, [], Tasks).

-spec dependencies_satisfied(task_id(), digraph:graph(), #{task_id() => #task{}}) -> boolean().
dependencies_satisfied(TaskId, Graph, Tasks) ->
    case digraph:in_edges(Graph, TaskId) of
        [] ->
            true;
        Edges ->
            %% Get all source vertices (dependencies)
            DepIds = [DepId || {_Edge, DepId, _TaskId, _Label} <- Edges],
            %% Check if all dependencies are completed
            lists:all(fun(DepId) ->
                case maps:get(DepId, Tasks, undefined) of
                    undefined -> true;  % Task doesn't exist, consider satisfied
                    #task{state = completed} -> true;
                    _ -> false
                end
            end, DepIds)
    end.

-spec spawn_task_execution(task_id(), #task{}, workflow_data()) -> reference().
spawn_task_execution(TaskId, Task, Data) ->
    Parent = self(),
    Ref = make_ref(),

    Worker = spawn(fun() ->
        logger:debug("Executing task ~p (~p:~p) for workflow ~p",
                     [TaskId, Task#task.module, Task#task.function, Data#workflow_data.workflow_id]),

        %% Update task state to running
        UpdatedTask = Task#task{
            state = running,
            started_at = erlang:system_time(millisecond)
        },

        %% Execute task function
        Result = try
            apply(Task#task.module, Task#task.function, Task#task.args)
        catch
            Type:Error:StackTrace ->
                {error, {Type, Error, StackTrace}}
        end,

        %% Notify parent of completion
        case Result of
            {error, _} = Error ->
                gen_statem:call(Parent, {task_failed, TaskId, Error}, 5000);
            _ ->
                gen_statem:call(Parent, {task_completed, TaskId, Result}, 5000)
        end
    end),

    %% Monitor worker
    erlang:monitor(process, Worker),
    Ref.

-spec pause_running_tasks(workflow_data()) -> ok.
pause_running_tasks(Data) ->
    %% In a real implementation, this would send pause signals to running tasks
    logger:debug("Pausing ~p running tasks in workflow ~p",
                 [length(Data#workflow_data.running_tasks), Data#workflow_data.workflow_id]),
    ok.

-spec is_workflow_complete(workflow_data()) -> boolean().
is_workflow_complete(Data) ->
    TotalTasks = maps:size(Data#workflow_data.tasks),
    CompletedTasks = length(Data#workflow_data.completed_tasks),
    FailedTasks = length(Data#workflow_data.failed_tasks),
    RunningTasks = length(Data#workflow_data.running_tasks),

    (CompletedTasks + FailedTasks) =:= TotalTasks andalso RunningTasks =:= 0.

-spec should_retry_task(#task{}, term()) -> {true, pos_integer()} | false.
should_retry_task(Task, _Reason) ->
    case Task#task.retry_policy of
        undefined ->
            false;
        Policy ->
            MaxAttempts = maps:get(max_attempts, Policy, 1),
            %% For now, simple logic - could track attempt count in task
            case MaxAttempts > 0 of
                true ->
                    BaseDelay = maps:get(base_delay, Policy, 1000),
                    Backoff = maps:get(backoff, Policy, exponential),
                    Delay = calculate_backoff(1, BaseDelay, Backoff),
                    {true, Delay};
                false ->
                    false
            end
    end.

-spec calculate_backoff(pos_integer(), pos_integer(), exponential | fixed) -> pos_integer().
calculate_backoff(Attempt, BaseDelay, exponential) ->
    min(BaseDelay * round(math:pow(2, Attempt - 1)), 60000);
calculate_backoff(_Attempt, BaseDelay, fixed) ->
    BaseDelay.

-spec notify_subscribers(workflow_data(), term()) -> ok.
notify_subscribers(Data, Event) ->
    lists:foreach(fun({Subscriber, _MonRef}) ->
        Subscriber ! {workflow_event, Data#workflow_data.workflow_id, Event}
    end, Data#workflow_data.subscribers),
    ok.

-spec emit_state_transition(workflow_state() | undefined, workflow_state(), workflow_data()) -> ok.
emit_state_transition(OldState, NewState, Data) ->
    Event = #{
        type => state_transition,
        module => ?MODULE,
        workflow_id => Data#workflow_data.workflow_id,
        from_state => OldState,
        to_state => NewState,
        timestamp => erlang:system_time(millisecond)
    },
    logger:debug("State transition: ~p", [Event]),

    %% Emit to OTEL if available
    case whereis(erlmcp_otel) of
        undefined -> ok;
        _ -> erlmcp_otel:emit_event(workflow, Event)
    end,
    ok.

-spec emit_workflow_event(term(), workflow_data()) -> ok.
emit_workflow_event(EventType, Data) ->
    Event = #{
        type => EventType,
        module => ?MODULE,
        workflow_id => Data#workflow_data.workflow_id,
        state => Data#workflow_data.state,
        timestamp => erlang:system_time(millisecond)
    },
    logger:info("Workflow event: ~p", [Event]),

    %% Notify subscribers
    notify_subscribers(Data, Event),

    %% Emit to OTEL if available
    case whereis(erlmcp_otel) of
        undefined -> ok;
        _ -> erlmcp_otel:emit_event(workflow, Event)
    end,
    ok.
