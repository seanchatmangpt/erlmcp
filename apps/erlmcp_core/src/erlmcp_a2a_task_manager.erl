%%%-------------------------------------------------------------------
%%% @doc A2A Task Manager
%%%
%%% This module manages the lifecycle of A2A tasks, providing:
%%% - Task CRUD operations (create, get, list, cancel)
%%% - Task state machine transitions (submitted -> working -> completed/failed/canceled)
%%% - Context management (grouping tasks by context_id)
%%% - Task history management (message history per task)
%%% - Artifact management (add/append artifacts to tasks)
%%% - Subscription management for task updates
%%% - Push notification trigger hooks
%%% - Pagination support for list operations
%%% - ETS-based storage for high-performance task lookup
%%%
%%% Valid state transitions:
%%%   submitted -> working | input_required | auth_required | rejected | canceled | failed
%%%   working -> completed | failed | canceled | input_required | auth_required
%%%   input_required -> working | canceled | failed
%%%   auth_required -> working | canceled | failed
%%%
%%% Terminal states: completed, failed, canceled, rejected
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_a2a_task_manager).

-behaviour(gen_server).

-include("erlmcp.hrl").
-include("erlmcp_a2a.hrl").

%% API exports
-export([
    start_link/0,
    start_link/1,
    %% Task CRUD
    create_task/2,
    create_task/3,
    get_task/1,
    get_task/2,
    list_tasks/0,
    list_tasks/1,
    cancel_task/1,
    cancel_task/2,
    %% State transitions
    update_task_status/2,
    update_task_status/3,
    transition_to_working/1,
    transition_to_completed/1,
    transition_to_completed/2,
    transition_to_failed/1,
    transition_to_failed/2,
    transition_to_input_required/1,
    transition_to_input_required/2,
    transition_to_auth_required/1,
    %% Context management
    get_tasks_by_context/1,
    get_tasks_by_context/2,
    delete_context/1,
    %% History management
    add_message_to_history/2,
    get_task_history/1,
    get_task_history/2,
    clear_task_history/1,
    %% Artifact management
    add_artifact/2,
    append_artifact/3,
    get_artifacts/1,
    get_artifact/2,
    %% Subscription management
    subscribe/1,
    subscribe/2,
    unsubscribe/1,
    unsubscribe/2,
    list_subscriptions/1,
    %% Push notification hooks
    register_push_config/2,
    unregister_push_config/2,
    get_push_configs/1,
    trigger_push_notification/2,
    %% Statistics
    get_stats/0
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    handle_continue/2,
    terminate/2,
    code_change/3
]).

%% Types
-type task_id() :: binary().
-type context_id() :: binary().
-type artifact_id() :: binary().
-type pagination_opts() :: #{
    page_size => pos_integer(),
    page_token => binary() | undefined,
    status_filter => a2a_task_state() | undefined,
    timestamp_after => binary() | undefined,
    include_artifacts => boolean(),
    history_length => non_neg_integer() | undefined
}.

-type list_result() :: #{
    tasks := [#a2a_task{}],
    next_page_token := binary(),
    page_size := pos_integer(),
    total_size := non_neg_integer()
}.

-export_type([task_id/0, context_id/0, artifact_id/0, pagination_opts/0, list_result/0]).

%% ETS table names
-define(TASK_TABLE, erlmcp_a2a_tasks).
-define(CONTEXT_TABLE, erlmcp_a2a_contexts).
-define(SUBSCRIPTION_TABLE, erlmcp_a2a_subscriptions).
-define(PUSH_CONFIG_TABLE, erlmcp_a2a_push_configs).

%% Default values
-define(DEFAULT_PAGE_SIZE, 50).
-define(MAX_PAGE_SIZE, 100).
-define(DEFAULT_HISTORY_LENGTH, 10).

%% State record
-record(state, {
    task_table :: ets:tid(),
    context_table :: ets:tid(),
    subscription_table :: ets:tid(),
    push_config_table :: ets:tid(),
    task_count = 0 :: non_neg_integer(),
    push_notification_handler :: fun((task_id(), term()) -> ok) | undefined
}).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the task manager with default options
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

%% @doc Start the task manager with options
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

%% @doc Create a new task with context_id and initial message
-spec create_task(context_id(), #a2a_message{}) -> {ok, #a2a_task{}} | {error, term()}.
create_task(ContextId, Message) ->
    create_task(ContextId, Message, #{}).

%% @doc Create a new task with options
-spec create_task(context_id(), #a2a_message{}, map()) -> {ok, #a2a_task{}} | {error, term()}.
create_task(ContextId, Message, Opts) ->
    gen_server:call(?MODULE, {create_task, ContextId, Message, Opts}).

%% @doc Get a task by ID
-spec get_task(task_id()) -> {ok, #a2a_task{}} | {error, not_found}.
get_task(TaskId) ->
    get_task(TaskId, #{}).

%% @doc Get a task with options (e.g., history_length)
-spec get_task(task_id(), map()) -> {ok, #a2a_task{}} | {error, not_found}.
get_task(TaskId, Opts) ->
    gen_server:call(?MODULE, {get_task, TaskId, Opts}).

%% @doc List all tasks
-spec list_tasks() -> list_result().
list_tasks() ->
    list_tasks(#{}).

%% @doc List tasks with pagination options
-spec list_tasks(pagination_opts()) -> list_result().
list_tasks(Opts) ->
    gen_server:call(?MODULE, {list_tasks, Opts}).

%% @doc Cancel a task
-spec cancel_task(task_id()) -> {ok, #a2a_task{}} | {error, term()}.
cancel_task(TaskId) ->
    cancel_task(TaskId, undefined).

%% @doc Cancel a task with optional message
-spec cancel_task(task_id(), #a2a_message{} | undefined) -> {ok, #a2a_task{}} | {error, term()}.
cancel_task(TaskId, Message) ->
    gen_server:call(?MODULE, {cancel_task, TaskId, Message}).

%% @doc Update task status directly
-spec update_task_status(task_id(), #a2a_task_status{}) -> {ok, #a2a_task{}} | {error, term()}.
update_task_status(TaskId, Status) ->
    update_task_status(TaskId, Status, #{}).

%% @doc Update task status with options
-spec update_task_status(task_id(), #a2a_task_status{}, map()) -> {ok, #a2a_task{}} | {error, term()}.
update_task_status(TaskId, Status, Opts) ->
    gen_server:call(?MODULE, {update_task_status, TaskId, Status, Opts}).

%% @doc Transition task to working state
-spec transition_to_working(task_id()) -> {ok, #a2a_task{}} | {error, term()}.
transition_to_working(TaskId) ->
    Status = #a2a_task_status{
        state = working,
        timestamp = iso8601_timestamp()
    },
    update_task_status(TaskId, Status).

%% @doc Transition task to completed state
-spec transition_to_completed(task_id()) -> {ok, #a2a_task{}} | {error, term()}.
transition_to_completed(TaskId) ->
    transition_to_completed(TaskId, undefined).

%% @doc Transition task to completed state with message
-spec transition_to_completed(task_id(), #a2a_message{} | undefined) ->
    {ok, #a2a_task{}} | {error, term()}.
transition_to_completed(TaskId, Message) ->
    Status = #a2a_task_status{
        state = completed,
        message = Message,
        timestamp = iso8601_timestamp()
    },
    update_task_status(TaskId, Status).

%% @doc Transition task to failed state
-spec transition_to_failed(task_id()) -> {ok, #a2a_task{}} | {error, term()}.
transition_to_failed(TaskId) ->
    transition_to_failed(TaskId, undefined).

%% @doc Transition task to failed state with message
-spec transition_to_failed(task_id(), #a2a_message{} | undefined) ->
    {ok, #a2a_task{}} | {error, term()}.
transition_to_failed(TaskId, Message) ->
    Status = #a2a_task_status{
        state = failed,
        message = Message,
        timestamp = iso8601_timestamp()
    },
    update_task_status(TaskId, Status).

%% @doc Transition task to input_required state
-spec transition_to_input_required(task_id()) -> {ok, #a2a_task{}} | {error, term()}.
transition_to_input_required(TaskId) ->
    transition_to_input_required(TaskId, undefined).

%% @doc Transition task to input_required state with message
-spec transition_to_input_required(task_id(), #a2a_message{} | undefined) ->
    {ok, #a2a_task{}} | {error, term()}.
transition_to_input_required(TaskId, Message) ->
    Status = #a2a_task_status{
        state = input_required,
        message = Message,
        timestamp = iso8601_timestamp()
    },
    update_task_status(TaskId, Status).

%% @doc Transition task to auth_required state
-spec transition_to_auth_required(task_id()) -> {ok, #a2a_task{}} | {error, term()}.
transition_to_auth_required(TaskId) ->
    Status = #a2a_task_status{
        state = auth_required,
        timestamp = iso8601_timestamp()
    },
    update_task_status(TaskId, Status).

%% @doc Get all tasks for a context
-spec get_tasks_by_context(context_id()) -> [#a2a_task{}].
get_tasks_by_context(ContextId) ->
    get_tasks_by_context(ContextId, #{}).

%% @doc Get all tasks for a context with pagination
-spec get_tasks_by_context(context_id(), pagination_opts()) -> list_result().
get_tasks_by_context(ContextId, Opts) ->
    gen_server:call(?MODULE, {get_tasks_by_context, ContextId, Opts}).

%% @doc Delete all tasks for a context
-spec delete_context(context_id()) -> {ok, non_neg_integer()}.
delete_context(ContextId) ->
    gen_server:call(?MODULE, {delete_context, ContextId}).

%% @doc Add a message to task history
-spec add_message_to_history(task_id(), #a2a_message{}) -> ok | {error, term()}.
add_message_to_history(TaskId, Message) ->
    gen_server:call(?MODULE, {add_message_to_history, TaskId, Message}).

%% @doc Get task history
-spec get_task_history(task_id()) -> {ok, [#a2a_message{}]} | {error, not_found}.
get_task_history(TaskId) ->
    get_task_history(TaskId, ?DEFAULT_HISTORY_LENGTH).

%% @doc Get task history with limit
-spec get_task_history(task_id(), non_neg_integer()) -> {ok, [#a2a_message{}]} | {error, not_found}.
get_task_history(TaskId, Limit) ->
    gen_server:call(?MODULE, {get_task_history, TaskId, Limit}).

%% @doc Clear task history
-spec clear_task_history(task_id()) -> ok | {error, not_found}.
clear_task_history(TaskId) ->
    gen_server:call(?MODULE, {clear_task_history, TaskId}).

%% @doc Add an artifact to a task
-spec add_artifact(task_id(), #a2a_artifact{}) -> ok | {error, term()}.
add_artifact(TaskId, Artifact) ->
    gen_server:call(?MODULE, {add_artifact, TaskId, Artifact}).

%% @doc Append content to an existing artifact (streaming)
-spec append_artifact(task_id(), artifact_id(), [#a2a_part{}]) -> ok | {error, term()}.
append_artifact(TaskId, ArtifactId, Parts) ->
    gen_server:call(?MODULE, {append_artifact, TaskId, ArtifactId, Parts}).

%% @doc Get all artifacts for a task
-spec get_artifacts(task_id()) -> {ok, [#a2a_artifact{}]} | {error, not_found}.
get_artifacts(TaskId) ->
    gen_server:call(?MODULE, {get_artifacts, TaskId}).

%% @doc Get a specific artifact from a task
-spec get_artifact(task_id(), artifact_id()) -> {ok, #a2a_artifact{}} | {error, not_found}.
get_artifact(TaskId, ArtifactId) ->
    gen_server:call(?MODULE, {get_artifact, TaskId, ArtifactId}).

%% @doc Subscribe to task updates (current process)
-spec subscribe(task_id()) -> ok | {error, not_found}.
subscribe(TaskId) ->
    subscribe(TaskId, self()).

%% @doc Subscribe a specific pid to task updates
-spec subscribe(task_id(), pid()) -> ok | {error, not_found}.
subscribe(TaskId, Pid) ->
    gen_server:call(?MODULE, {subscribe, TaskId, Pid}).

%% @doc Unsubscribe from task updates (current process)
-spec unsubscribe(task_id()) -> ok.
unsubscribe(TaskId) ->
    unsubscribe(TaskId, self()).

%% @doc Unsubscribe a specific pid from task updates
-spec unsubscribe(task_id(), pid()) -> ok.
unsubscribe(TaskId, Pid) ->
    gen_server:call(?MODULE, {unsubscribe, TaskId, Pid}).

%% @doc List all subscriptions for a task
-spec list_subscriptions(task_id()) -> [pid()].
list_subscriptions(TaskId) ->
    gen_server:call(?MODULE, {list_subscriptions, TaskId}).

%% @doc Register push notification config for a task
-spec register_push_config(task_id(), #a2a_task_push_notification_config{}) -> ok | {error, term()}.
register_push_config(TaskId, Config) ->
    gen_server:call(?MODULE, {register_push_config, TaskId, Config}).

%% @doc Unregister push notification config
-spec unregister_push_config(task_id(), binary()) -> ok.
unregister_push_config(TaskId, ConfigId) ->
    gen_server:call(?MODULE, {unregister_push_config, TaskId, ConfigId}).

%% @doc Get push configs for a task
-spec get_push_configs(task_id()) -> [#a2a_task_push_notification_config{}].
get_push_configs(TaskId) ->
    gen_server:call(?MODULE, {get_push_configs, TaskId}).

%% @doc Trigger push notification for a task event
-spec trigger_push_notification(task_id(), term()) -> ok.
trigger_push_notification(TaskId, Event) ->
    gen_server:cast(?MODULE, {trigger_push_notification, TaskId, Event}).

%% @doc Get task manager statistics
-spec get_stats() -> map().
get_stats() ->
    gen_server:call(?MODULE, get_stats).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init(map()) -> {ok, #state{}} | {ok, #state{}, {continue, initialize_tables}}.
init(Opts) ->
    process_flag(trap_exit, true),

    PushHandler = maps:get(push_notification_handler, Opts, undefined),

    State = #state{
        push_notification_handler = PushHandler
    },

    logger:info("Starting A2A task manager (async initialization)"),
    {ok, State, {continue, initialize_tables}}.

-spec handle_continue(term(), #state{}) -> {noreply, #state{}}.
handle_continue(initialize_tables, State) ->
    %% Create ETS tables for task storage
    %% OTP 28 optimizations: read/write concurrency, decentralized counters
    TaskTable = ets:new(?TASK_TABLE, [
        set,
        public,
        named_table,
        {read_concurrency, true},
        {write_concurrency, true},
        {keypos, 1}
    ]),

    ContextTable = ets:new(?CONTEXT_TABLE, [
        bag,
        public,
        named_table,
        {read_concurrency, true},
        {write_concurrency, true},
        {keypos, 1}
    ]),

    SubscriptionTable = ets:new(?SUBSCRIPTION_TABLE, [
        bag,
        public,
        named_table,
        {read_concurrency, true},
        {write_concurrency, true},
        {keypos, 1}
    ]),

    PushConfigTable = ets:new(?PUSH_CONFIG_TABLE, [
        bag,
        public,
        named_table,
        {read_concurrency, true},
        {write_concurrency, true},
        {keypos, 1}
    ]),

    NewState = State#state{
        task_table = TaskTable,
        context_table = ContextTable,
        subscription_table = SubscriptionTable,
        push_config_table = PushConfigTable
    },

    logger:info("A2A task manager initialized"),
    {noreply, NewState};
handle_continue(_Continue, State) ->
    {noreply, State}.

-spec handle_call(term(), {pid(), term()}, #state{}) -> {reply, term(), #state{}}.
%% Create task
handle_call({create_task, ContextId, Message, Opts}, _From, State) ->
    TaskId = generate_task_id(),
    Timestamp = iso8601_timestamp(),

    %% Validate message
    case erlmcp_a2a_protocol:validate_message(Message) of
        ok ->
            Status = #a2a_task_status{
                state = submitted,
                timestamp = Timestamp
            },

            Metadata = maps:get(metadata, Opts, undefined),

            Task = #a2a_task{
                id = TaskId,
                context_id = ContextId,
                status = Status,
                artifacts = [],
                history = [Message],
                metadata = Metadata
            },

            %% Store task
            true = ets:insert(State#state.task_table, {TaskId, Task}),

            %% Update context index
            true = ets:insert(State#state.context_table, {ContextId, TaskId}),

            NewState = State#state{task_count = State#state.task_count + 1},

            logger:debug("Created task ~s in context ~s", [TaskId, ContextId]),

            {reply, {ok, Task}, NewState};
        {error, Reason} ->
            {reply, {error, {invalid_message, Reason}}, State}
    end;

%% Get task
handle_call({get_task, TaskId, Opts}, _From, State) ->
    case ets:lookup(State#state.task_table, TaskId) of
        [{TaskId, Task}] ->
            HistoryLength = maps:get(history_length, Opts, undefined),
            FilteredTask = maybe_limit_history(Task, HistoryLength),
            {reply, {ok, FilteredTask}, State};
        [] ->
            {reply, {error, not_found}, State}
    end;

%% List tasks
handle_call({list_tasks, Opts}, _From, State) ->
    Result = do_list_tasks(State, Opts),
    {reply, Result, State};

%% Cancel task
handle_call({cancel_task, TaskId, Message}, _From, State) ->
    case ets:lookup(State#state.task_table, TaskId) of
        [{TaskId, Task}] ->
            CurrentState = (Task#a2a_task.status)#a2a_task_status.state,
            case is_cancelable(CurrentState) of
                true ->
                    NewStatus = #a2a_task_status{
                        state = canceled,
                        message = Message,
                        timestamp = iso8601_timestamp()
                    },
                    UpdatedTask = Task#a2a_task{status = NewStatus},
                    ets:insert(State#state.task_table, {TaskId, UpdatedTask}),
                    notify_subscribers(State, TaskId, {status_update, UpdatedTask}),
                    {reply, {ok, UpdatedTask}, State};
                false ->
                    {reply, {error, {task_not_cancelable, CurrentState}}, State}
            end;
        [] ->
            {reply, {error, not_found}, State}
    end;

%% Update task status
handle_call({update_task_status, TaskId, NewStatus, _Opts}, _From, State) ->
    case ets:lookup(State#state.task_table, TaskId) of
        [{TaskId, Task}] ->
            CurrentState = (Task#a2a_task.status)#a2a_task_status.state,
            NewState = NewStatus#a2a_task_status.state,
            case validate_state_transition(CurrentState, NewState) of
                ok ->
                    UpdatedTask = Task#a2a_task{status = NewStatus},
                    ets:insert(State#state.task_table, {TaskId, UpdatedTask}),
                    notify_subscribers(State, TaskId, {status_update, UpdatedTask}),
                    maybe_trigger_push(State, TaskId, {status_update, NewStatus}),
                    {reply, {ok, UpdatedTask}, State};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end;
        [] ->
            {reply, {error, not_found}, State}
    end;

%% Get tasks by context
handle_call({get_tasks_by_context, ContextId, Opts}, _From, State) ->
    TaskIds = [TId || {_CId, TId} <- ets:lookup(State#state.context_table, ContextId)],
    Tasks = lists:filtermap(
        fun(TaskId) ->
            case ets:lookup(State#state.task_table, TaskId) of
                [{TaskId, Task}] -> {true, Task};
                [] -> false
            end
        end,
        TaskIds
    ),
    Result = paginate_tasks(Tasks, Opts),
    {reply, Result, State};

%% Delete context
handle_call({delete_context, ContextId}, _From, State) ->
    TaskIds = [TId || {_CId, TId} <- ets:lookup(State#state.context_table, ContextId)],

    %% Delete all tasks in context
    lists:foreach(
        fun(TaskId) ->
            ets:delete(State#state.task_table, TaskId),
            ets:delete(State#state.subscription_table, TaskId),
            ets:delete(State#state.push_config_table, TaskId)
        end,
        TaskIds
    ),

    %% Delete context index
    ets:delete(State#state.context_table, ContextId),

    DeletedCount = length(TaskIds),
    NewState = State#state{task_count = max(0, State#state.task_count - DeletedCount)},

    {reply, {ok, DeletedCount}, NewState};

%% Add message to history
handle_call({add_message_to_history, TaskId, Message}, _From, State) ->
    case ets:lookup(State#state.task_table, TaskId) of
        [{TaskId, Task}] ->
            case erlmcp_a2a_protocol:validate_message(Message) of
                ok ->
                    CurrentHistory = case Task#a2a_task.history of
                        undefined -> [];
                        H -> H
                    end,
                    UpdatedTask = Task#a2a_task{history = CurrentHistory ++ [Message]},
                    ets:insert(State#state.task_table, {TaskId, UpdatedTask}),
                    {reply, ok, State};
                {error, Reason} ->
                    {reply, {error, {invalid_message, Reason}}, State}
            end;
        [] ->
            {reply, {error, not_found}, State}
    end;

%% Get task history
handle_call({get_task_history, TaskId, Limit}, _From, State) ->
    case ets:lookup(State#state.task_table, TaskId) of
        [{TaskId, Task}] ->
            History = case Task#a2a_task.history of
                undefined -> [];
                H -> H
            end,
            LimitedHistory = case Limit of
                0 -> History;
                N when is_integer(N), N > 0 -> lists:sublist(History, max(1, length(History) - N + 1), N);
                _ -> History
            end,
            {reply, {ok, LimitedHistory}, State};
        [] ->
            {reply, {error, not_found}, State}
    end;

%% Clear task history
handle_call({clear_task_history, TaskId}, _From, State) ->
    case ets:lookup(State#state.task_table, TaskId) of
        [{TaskId, Task}] ->
            UpdatedTask = Task#a2a_task{history = []},
            ets:insert(State#state.task_table, {TaskId, UpdatedTask}),
            {reply, ok, State};
        [] ->
            {reply, {error, not_found}, State}
    end;

%% Add artifact
handle_call({add_artifact, TaskId, Artifact}, _From, State) ->
    case ets:lookup(State#state.task_table, TaskId) of
        [{TaskId, Task}] ->
            CurrentArtifacts = case Task#a2a_task.artifacts of
                undefined -> [];
                A -> A
            end,
            UpdatedTask = Task#a2a_task{artifacts = CurrentArtifacts ++ [Artifact]},
            ets:insert(State#state.task_table, {TaskId, UpdatedTask}),
            notify_subscribers(State, TaskId, {artifact_update, Artifact, false}),
            maybe_trigger_push(State, TaskId, {artifact_update, Artifact}),
            {reply, ok, State};
        [] ->
            {reply, {error, not_found}, State}
    end;

%% Append to artifact
handle_call({append_artifact, TaskId, ArtifactId, Parts}, _From, State) ->
    case ets:lookup(State#state.task_table, TaskId) of
        [{TaskId, Task}] ->
            case find_and_update_artifact(Task, ArtifactId, Parts) of
                {ok, UpdatedTask, UpdatedArtifact} ->
                    ets:insert(State#state.task_table, {TaskId, UpdatedTask}),
                    notify_subscribers(State, TaskId, {artifact_update, UpdatedArtifact, true}),
                    {reply, ok, State};
                {error, artifact_not_found} ->
                    {reply, {error, artifact_not_found}, State}
            end;
        [] ->
            {reply, {error, not_found}, State}
    end;

%% Get artifacts
handle_call({get_artifacts, TaskId}, _From, State) ->
    case ets:lookup(State#state.task_table, TaskId) of
        [{TaskId, Task}] ->
            Artifacts = case Task#a2a_task.artifacts of
                undefined -> [];
                A -> A
            end,
            {reply, {ok, Artifacts}, State};
        [] ->
            {reply, {error, not_found}, State}
    end;

%% Get artifact
handle_call({get_artifact, TaskId, ArtifactId}, _From, State) ->
    case ets:lookup(State#state.task_table, TaskId) of
        [{TaskId, Task}] ->
            Artifacts = case Task#a2a_task.artifacts of
                undefined -> [];
                A -> A
            end,
            case lists:keyfind(ArtifactId, #a2a_artifact.artifact_id, Artifacts) of
                false -> {reply, {error, not_found}, State};
                Artifact -> {reply, {ok, Artifact}, State}
            end;
        [] ->
            {reply, {error, not_found}, State}
    end;

%% Subscribe
handle_call({subscribe, TaskId, Pid}, _From, State) ->
    case ets:lookup(State#state.task_table, TaskId) of
        [{TaskId, _Task}] ->
            %% Monitor subscriber for cleanup
            erlang:monitor(process, Pid),
            ets:insert(State#state.subscription_table, {TaskId, Pid}),
            {reply, ok, State};
        [] ->
            {reply, {error, not_found}, State}
    end;

%% Unsubscribe
handle_call({unsubscribe, TaskId, Pid}, _From, State) ->
    ets:delete_object(State#state.subscription_table, {TaskId, Pid}),
    {reply, ok, State};

%% List subscriptions
handle_call({list_subscriptions, TaskId}, _From, State) ->
    Subs = [Pid || {_TId, Pid} <- ets:lookup(State#state.subscription_table, TaskId)],
    {reply, Subs, State};

%% Register push config
handle_call({register_push_config, TaskId, Config}, _From, State) ->
    case ets:lookup(State#state.task_table, TaskId) of
        [{TaskId, _Task}] ->
            ets:insert(State#state.push_config_table, {TaskId, Config}),
            {reply, ok, State};
        [] ->
            {reply, {error, not_found}, State}
    end;

%% Unregister push config
handle_call({unregister_push_config, TaskId, ConfigId}, _From, State) ->
    Configs = ets:lookup(State#state.push_config_table, TaskId),
    lists:foreach(
        fun({TId, Config}) ->
            case Config#a2a_task_push_notification_config.id of
                ConfigId -> ets:delete_object(State#state.push_config_table, {TId, Config});
                _ -> ok
            end
        end,
        Configs
    ),
    {reply, ok, State};

%% Get push configs
handle_call({get_push_configs, TaskId}, _From, State) ->
    Configs = [C || {_TId, C} <- ets:lookup(State#state.push_config_table, TaskId)],
    {reply, Configs, State};

%% Get stats
handle_call(get_stats, _From, State) ->
    Stats = #{
        task_count => State#state.task_count,
        context_count => ets:info(State#state.context_table, size),
        subscription_count => ets:info(State#state.subscription_table, size),
        push_config_count => ets:info(State#state.push_config_table, size),
        task_table_memory => ets:info(State#state.task_table, memory),
        context_table_memory => ets:info(State#state.context_table, memory)
    },
    {reply, Stats, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast({trigger_push_notification, TaskId, Event}, State) ->
    maybe_trigger_push(State, TaskId, Event),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    %% Clean up subscriptions for dead processes
    %% This is O(n) but subscriptions should be manageable
    ets:foldl(
        fun({TaskId, SubPid}, _Acc) when SubPid =:= Pid ->
            ets:delete_object(State#state.subscription_table, {TaskId, Pid});
           (_, Acc) -> Acc
        end,
        ok,
        State#state.subscription_table
    ),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, State) ->
    %% Clean up ETS tables
    catch ets:delete(State#state.task_table),
    catch ets:delete(State#state.context_table),
    catch ets:delete(State#state.subscription_table),
    catch ets:delete(State#state.push_config_table),
    ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Generate a unique task ID
-spec generate_task_id() -> binary().
generate_task_id() ->
    Rand = crypto:strong_rand_bytes(16),
    binary:encode_hex(Rand).

%% @private Generate ISO 8601 timestamp
-spec iso8601_timestamp() -> binary().
iso8601_timestamp() ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:universal_time(),
    iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
        [Year, Month, Day, Hour, Min, Sec])).

%% @private Check if task is in a cancelable state
-spec is_cancelable(a2a_task_state()) -> boolean().
is_cancelable(State) ->
    not lists:member(State, ?A2A_TERMINAL_STATES).

%% @private Validate state transition
-spec validate_state_transition(a2a_task_state(), a2a_task_state()) -> ok | {error, term()}.
validate_state_transition(CurrentState, NewState) ->
    %% Check if current state is terminal
    case lists:member(CurrentState, ?A2A_TERMINAL_STATES) of
        true ->
            {error, {invalid_transition, {from_terminal, CurrentState, NewState}}};
        false ->
            %% Validate allowed transitions
            AllowedTransitions = get_allowed_transitions(CurrentState),
            case lists:member(NewState, AllowedTransitions) of
                true -> ok;
                false -> {error, {invalid_transition, {CurrentState, NewState}}}
            end
    end.

%% @private Get allowed state transitions from a given state
-spec get_allowed_transitions(a2a_task_state()) -> [a2a_task_state()].
get_allowed_transitions(submitted) ->
    [working, input_required, auth_required, rejected, canceled, failed];
get_allowed_transitions(working) ->
    [completed, failed, canceled, input_required, auth_required];
get_allowed_transitions(input_required) ->
    [working, canceled, failed];
get_allowed_transitions(auth_required) ->
    [working, canceled, failed];
get_allowed_transitions(_Terminal) ->
    [].

%% @private Limit history in task response
-spec maybe_limit_history(#a2a_task{}, non_neg_integer() | undefined) -> #a2a_task{}.
maybe_limit_history(Task, undefined) ->
    Task;
maybe_limit_history(Task, 0) ->
    Task#a2a_task{history = undefined};
maybe_limit_history(Task, Limit) when is_integer(Limit), Limit > 0 ->
    case Task#a2a_task.history of
        undefined -> Task;
        [] -> Task;
        History ->
            Len = length(History),
            Limited = if
                Len =< Limit -> History;
                true -> lists:sublist(History, max(1, Len - Limit + 1), Limit)
            end,
            Task#a2a_task{history = Limited}
    end.

%% @private List tasks with filtering and pagination
-spec do_list_tasks(#state{}, pagination_opts()) -> list_result().
do_list_tasks(State, Opts) ->
    %% Collect all tasks
    AllTasks = ets:foldl(
        fun({_TaskId, Task}, Acc) -> [Task | Acc] end,
        [],
        State#state.task_table
    ),

    %% Apply filters
    FilteredTasks = filter_tasks(AllTasks, Opts),

    %% Paginate
    paginate_tasks(FilteredTasks, Opts).

%% @private Filter tasks based on options
-spec filter_tasks([#a2a_task{}], pagination_opts()) -> [#a2a_task{}].
filter_tasks(Tasks, Opts) ->
    StatusFilter = maps:get(status_filter, Opts, undefined),
    TimestampAfter = maps:get(timestamp_after, Opts, undefined),
    ContextId = maps:get(context_id, Opts, undefined),

    lists:filter(
        fun(Task) ->
            filter_by_status(Task, StatusFilter) andalso
            filter_by_timestamp(Task, TimestampAfter) andalso
            filter_by_context(Task, ContextId)
        end,
        Tasks
    ).

-spec filter_by_status(#a2a_task{}, a2a_task_state() | undefined) -> boolean().
filter_by_status(_Task, undefined) -> true;
filter_by_status(Task, Status) ->
    (Task#a2a_task.status)#a2a_task_status.state =:= Status.

-spec filter_by_timestamp(#a2a_task{}, binary() | undefined) -> boolean().
filter_by_timestamp(_Task, undefined) -> true;
filter_by_timestamp(Task, TimestampAfter) ->
    case (Task#a2a_task.status)#a2a_task_status.timestamp of
        undefined -> true;
        Ts -> Ts > TimestampAfter
    end.

-spec filter_by_context(#a2a_task{}, binary() | undefined) -> boolean().
filter_by_context(_Task, undefined) -> true;
filter_by_context(Task, ContextId) ->
    Task#a2a_task.context_id =:= ContextId.

%% @private Paginate task list
-spec paginate_tasks([#a2a_task{}], pagination_opts()) -> list_result().
paginate_tasks(Tasks, Opts) ->
    PageSize = min(maps:get(page_size, Opts, ?DEFAULT_PAGE_SIZE), ?MAX_PAGE_SIZE),
    PageToken = maps:get(page_token, Opts, undefined),
    HistoryLength = maps:get(history_length, Opts, undefined),
    IncludeArtifacts = maps:get(include_artifacts, Opts, true),

    %% Sort by task ID for consistent pagination
    SortedTasks = lists:sort(
        fun(A, B) -> A#a2a_task.id =< B#a2a_task.id end,
        Tasks
    ),

    %% Find starting point based on page token
    {StartTasks, TotalSize} = case PageToken of
        undefined ->
            {SortedTasks, length(SortedTasks)};
        Token ->
            AfterToken = lists:dropwhile(
                fun(T) -> T#a2a_task.id =< Token end,
                SortedTasks
            ),
            {AfterToken, length(SortedTasks)}
    end,

    %% Take page
    PageTasks = lists:sublist(StartTasks, PageSize),

    %% Process tasks (limit history, optionally exclude artifacts)
    ProcessedTasks = lists:map(
        fun(Task) ->
            T1 = maybe_limit_history(Task, HistoryLength),
            case IncludeArtifacts of
                true -> T1;
                false -> T1#a2a_task{artifacts = undefined}
            end
        end,
        PageTasks
    ),

    %% Calculate next page token
    NextPageToken = case length(StartTasks) > PageSize of
        true ->
            LastTask = lists:last(PageTasks),
            LastTask#a2a_task.id;
        false ->
            <<>>
    end,

    #{
        tasks => ProcessedTasks,
        next_page_token => NextPageToken,
        page_size => length(ProcessedTasks),
        total_size => TotalSize
    }.

%% @private Find and update artifact by ID
-spec find_and_update_artifact(#a2a_task{}, artifact_id(), [#a2a_part{}]) ->
    {ok, #a2a_task{}, #a2a_artifact{}} | {error, artifact_not_found}.
find_and_update_artifact(Task, ArtifactId, NewParts) ->
    Artifacts = case Task#a2a_task.artifacts of
        undefined -> [];
        A -> A
    end,

    case find_artifact_index(Artifacts, ArtifactId, 1) of
        {ok, Index, Artifact} ->
            CurrentParts = Artifact#a2a_artifact.parts,
            UpdatedArtifact = Artifact#a2a_artifact{parts = CurrentParts ++ NewParts},
            UpdatedArtifacts = replace_at(Artifacts, Index, UpdatedArtifact),
            {ok, Task#a2a_task{artifacts = UpdatedArtifacts}, UpdatedArtifact};
        not_found ->
            {error, artifact_not_found}
    end.

-spec find_artifact_index([#a2a_artifact{}], artifact_id(), pos_integer()) ->
    {ok, pos_integer(), #a2a_artifact{}} | not_found.
find_artifact_index([], _ArtifactId, _Index) ->
    not_found;
find_artifact_index([#a2a_artifact{artifact_id = ArtifactId} = A | _Rest], ArtifactId, Index) ->
    {ok, Index, A};
find_artifact_index([_H | Rest], ArtifactId, Index) ->
    find_artifact_index(Rest, ArtifactId, Index + 1).

-spec replace_at([T], pos_integer(), T) -> [T].
replace_at(List, Index, Element) ->
    {Before, [_ | After]} = lists:split(Index - 1, List),
    Before ++ [Element] ++ After.

%% @private Notify all subscribers of a task event
-spec notify_subscribers(#state{}, task_id(), term()) -> ok.
notify_subscribers(State, TaskId, Event) ->
    Subscribers = [Pid || {_TId, Pid} <- ets:lookup(State#state.subscription_table, TaskId)],

    Notification = case Event of
        {status_update, Task} ->
            #a2a_task_status_update_event{
                task_id = TaskId,
                context_id = Task#a2a_task.context_id,
                status = Task#a2a_task.status,
                metadata = Task#a2a_task.metadata
            };
        {artifact_update, Artifact, Append} ->
            case ets:lookup(State#state.task_table, TaskId) of
                [{TaskId, Task}] ->
                    #a2a_task_artifact_update_event{
                        task_id = TaskId,
                        context_id = Task#a2a_task.context_id,
                        artifact = Artifact,
                        append = Append,
                        last_chunk = false,
                        metadata = undefined
                    };
                [] ->
                    undefined
            end
    end,

    case Notification of
        undefined -> ok;
        _ ->
            lists:foreach(
                fun(Pid) ->
                    Pid ! {a2a_task_event, Notification}
                end,
                Subscribers
            )
    end,
    ok.

%% @private Maybe trigger push notifications
-spec maybe_trigger_push(#state{}, task_id(), term()) -> ok.
maybe_trigger_push(State, TaskId, Event) ->
    case State#state.push_notification_handler of
        undefined ->
            %% No handler configured, just log
            Configs = ets:lookup(State#state.push_config_table, TaskId),
            case Configs of
                [] -> ok;
                _ ->
                    logger:debug("Would push to ~p configs for task ~s: ~p",
                        [length(Configs), TaskId, Event])
            end;
        Handler when is_function(Handler, 2) ->
            %% Call handler for each config
            Configs = [C || {_TId, C} <- ets:lookup(State#state.push_config_table, TaskId)],
            lists:foreach(
                fun(Config) ->
                    try
                        Handler(Config, Event)
                    catch
                        Class:Reason:Stack ->
                            logger:error("Push notification handler error: ~p:~p~n~p",
                                [Class, Reason, Stack])
                    end
                end,
                Configs
            )
    end,
    ok.
