%%%-------------------------------------------------------------------
%%% @doc MCP Tasks API for async long-running operations
%%% Implements MCP Tasks API per MCP 2025-11-25 specification.
%%%
%%% == Task Lifecycle ==
%%% Tasks progress through states:
%%%   pending -> processing -> completed | failed | cancelled
%%%
%%% == Features ==
%%% - ETS-based state persistence (concurrent access)
%%% - Progress token integration via erlmcp_progress
%%% - Concurrent task limiting (max 1000)
%%% - Task timeout handling
%%% - Worker process monitoring
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_tasks).
-behaviour(gen_server).

%% Client API
-export([start_link/0]).
-export([create_task/3, list_tasks/3, get_task/2, cancel_task/3, get_task_result/2]).

%% Server API (internal)
-export([start_task_execution/2, complete_task/2, fail_task/2]).
-export([set_task_progress/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("erlmcp.hrl").

%%%===================================================================
%%% Constants & Macros
%%%===================================================================

-define(SERVER, ?MODULE).
-define(TASKS_TABLE, erlmcp_tasks).
-define(MAX_CONCURRENT_TASKS, 1000).
-define(DEFAULT_TASK_TIMEOUT_MS, 300000).  % 5 minutes
-define(DEFAULT_EXPIRY_MS, 3600000).  % 1 hour

%%%===================================================================
%%% Types
%%%===================================================================

-type task_id() :: binary().
-type task_status() :: pending | processing | completed | failed | cancelled.
-type task() :: #mcp_task{}.
-type task_list_result() :: #{
    tasks := [map()],
    cursor => binary()
}.

-record(state, {
    task_count = 0 :: non_neg_integer(),
    pending_cleanup = [] :: [task_id()]
}).

%%%===================================================================
%%% Client API
%%%===================================================================

%% @doc Start the tasks manager.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Create a new task.
%% Action is a map describing the operation to perform.
%% Metadata can include progressToken, timeout, etc.
-spec create_task(pid() | undefined, map(), map()) ->
    {ok, task_id()} | {error, term()}.
create_task(ClientPid, Action, Metadata) when is_map(Action), is_map(Metadata) ->
    gen_server:call(?SERVER, {create_task, ClientPid, Action, Metadata}).

%% @doc List tasks with optional cursor-based pagination.
-spec list_tasks(pid() | undefined, binary() | undefined, pos_integer()) ->
    {ok, task_list_result()} | {error, term()}.
list_tasks(ClientPid, Cursor, Limit) when is_integer(Limit), Limit > 0 ->
    gen_server:call(?SERVER, {list_tasks, ClientPid, Cursor, Limit}).

%% @doc Get task details by ID.
-spec get_task(pid() | undefined, task_id()) ->
    {ok, task()} | {error, term()}.
get_task(ClientPid, TaskId) when is_binary(TaskId) ->
    gen_server:call(?SERVER, {get_task, ClientPid, TaskId}).

%% @doc Cancel a running or pending task.
-spec cancel_task(pid() | undefined, task_id(), binary()) ->
    {ok, cancelled} | {error, term()}.
cancel_task(ClientPid, TaskId, Reason) when is_binary(TaskId), is_binary(Reason) ->
    gen_server:call(?SERVER, {cancel_task, ClientPid, TaskId, Reason}).

%% @doc Get the result of a completed task.
-spec get_task_result(pid() | undefined, task_id()) ->
    {ok, term()} | {error, term()}.
get_task_result(ClientPid, TaskId) when is_binary(TaskId) ->
    gen_server:call(?SERVER, {get_task_result, ClientPid, TaskId}).

%%%===================================================================
%%% Server API (Internal)
%%%===================================================================

%% @doc Start task execution by assigning worker process.
-spec start_task_execution(task_id(), pid()) -> ok | {error, term()}.
start_task_execution(TaskId, WorkerPid) when is_binary(TaskId), is_pid(WorkerPid) ->
    gen_server:call(?SERVER, {start_task_execution, TaskId, WorkerPid}).

%% @doc Mark task as completed with result.
-spec complete_task(task_id(), term()) -> ok | {error, term()}.
complete_task(TaskId, Result) when is_binary(TaskId) ->
    gen_server:call(?SERVER, {complete_task, TaskId, Result}).

%% @doc Mark task as failed with error details.
-spec fail_task(task_id(), term()) -> ok | {error, term()}.
fail_task(TaskId, Error) when is_binary(TaskId) ->
    gen_server:call(?SERVER, {fail_task, TaskId, Error}).

%% @doc Update task progress (if progress token available).
-spec set_task_progress(task_id(), number() | {number(), number()}) ->
    ok | {error, term()}.
set_task_progress(TaskId, Progress) when is_binary(TaskId) ->
    gen_server:cast(?SERVER, {set_task_progress, TaskId, Progress}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
-spec init([]) -> {ok, #state{}}.
init([]) ->
    logger:info("Starting MCP Tasks manager"),

    % Create ETS table for tasks (only if it doesn't exist)
    case ets:info(?TASKS_TABLE) of
        undefined ->
            ets:new(?TASKS_TABLE, [
                named_table,
                public,
                set,
                {keypos, #mcp_task.id},
                {read_concurrency, true},
                {write_concurrency, true}
            ]);
        _ ->
            ok  % Table already exists
    end,

    % Schedule periodic cleanup of expired tasks
    schedule_cleanup(),

    {ok, #state{
        task_count = 0,
        pending_cleanup = []
    }}.

%% @private
-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {reply, term(), #state{}}.
handle_call({create_task, ClientPid, Action, Metadata}, _From, State) ->
    case State#state.task_count >= ?MAX_CONCURRENT_TASKS of
        true ->
            {reply, {error, ?MCP_ERROR_MAX_CONCURRENT_TASKS}, State};
        false ->
            {Reply, NewState} = do_create_task(ClientPid, Action, Metadata, State),
            {reply, Reply, NewState}
    end;

handle_call({list_tasks, ClientPid, Cursor, Limit}, _From, State) ->
    Reply = do_list_tasks(ClientPid, Cursor, Limit),
    {reply, Reply, State};

handle_call({get_task, ClientPid, TaskId}, _From, State) ->
    Reply = do_get_task(ClientPid, TaskId),
    {reply, Reply, State};

handle_call({cancel_task, ClientPid, TaskId, Reason}, _From, State) ->
    {Reply, NewState} = do_cancel_task(ClientPid, TaskId, Reason, State),
    {reply, Reply, NewState};

handle_call({get_task_result, ClientPid, TaskId}, _From, State) ->
    Reply = do_get_task_result(ClientPid, TaskId),
    {reply, Reply, State};

handle_call({start_task_execution, TaskId, WorkerPid}, _From, State) ->
    {Reply, NewState} = do_start_task_execution(TaskId, WorkerPid, State),
    {reply, Reply, NewState};

handle_call({complete_task, TaskId, Result}, _From, State) ->
    {Reply, NewState} = do_complete_task(TaskId, Result, State),
    {reply, Reply, NewState};

handle_call({fail_task, TaskId, Error}, _From, State) ->
    {Reply, NewState} = do_fail_task(TaskId, Error, State),
    {reply, Reply, NewState};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast({set_task_progress, TaskId, Progress}, State) ->
    NewState = do_set_task_progress(TaskId, Progress, State),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info({task_timeout, TaskId}, State) ->
    NewState = do_handle_task_timeout(TaskId, State),
    {noreply, NewState};

handle_info({'DOWN', MonitorRef, process, _Pid, _Reason}, State) ->
    NewState = do_handle_worker_down(MonitorRef, State),
    {noreply, NewState};

handle_info(cleanup_expired_tasks, State) ->
    NewState = do_cleanup_expired_tasks(State),
    schedule_cleanup(),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

%% @private
-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, #state{task_count = Count}) ->
    logger:info("Tasks manager terminating with ~p active tasks", [Count]),
    ok.

%% @private
-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @private
do_create_task(ClientPid, Action, Metadata, State) ->
    TaskId = generate_task_id(),
    TimeoutMs = maps:get(<<"timeout">>, Metadata, ?DEFAULT_TASK_TIMEOUT_MS),
    ExpiresAt = erlang:system_time(millisecond) +
                 maps:get(<<"expiresAfter">>, Metadata, ?DEFAULT_EXPIRY_MS),

    ProgressToken = case maps:get(<<"progressToken">>, Metadata, undefined) of
        undefined -> undefined;
        _ -> erlmcp_progress:generate_token()
    end,

    Now = erlang:system_time(millisecond),

    Task = #mcp_task{
        id = TaskId,
        status = pending,
        action = Action,
        metadata = Metadata,
        result = undefined,
        error = undefined,
        created_at = Now,
        updated_at = Now,
        expires_at = ExpiresAt,
        client_pid = ClientPid,
        worker_pid = undefined,
        worker_monitor = undefined,
        progress_token = ProgressToken,
        progress = undefined,
        total = undefined,
        timeout_ms = TimeoutMs,
        timer_ref = undefined
    },

    ets:insert(?TASKS_TABLE, Task),

    TimerRef = erlang:send_after(TimeoutMs, self(), {task_timeout, TaskId}),
    Task1 = Task#mcp_task{timer_ref = TimerRef},
    ets:insert(?TASKS_TABLE, Task1),

    case ProgressToken of
        undefined -> ok;
        Token ->
            erlmcp_progress:create(ClientPid, <<"Task created: ", TaskId/binary>>)
    end,

    logger:debug("Created task ~p for client ~p", [TaskId, ClientPid]),
    send_task_notification(ClientPid, TaskId),

    {{ok, TaskId}, State#state{task_count = State#state.task_count + 1}}.

%% @private
do_list_tasks(ClientPid, Cursor, Limit) ->
    AllTasks = ets:foldl(
        fun(#mcp_task{id = Id, client_pid = Pid} = Task, Acc) ->
            case Pid =:= ClientPid of
                true -> [Task | Acc];
                false -> Acc
            end
        end,
        [],
        ?TASKS_TABLE
    ),

    SortedTasks = lists:sort(
        fun(A, B) ->
            A#mcp_task.created_at >= B#mcp_task.created_at
        end,
        AllTasks
    ),

    FilteredTasks = case Cursor of
        undefined -> SortedTasks;
        _ ->
            case lists:keyfind(Cursor, #mcp_task.id, SortedTasks) of
                false -> [];
                _CursorTask ->
                    {Before, After} = lists:splitwith(
                        fun(T) -> T#mcp_task.id =/= Cursor end,
                        SortedTasks
                    ),
                    case After of
                        [] -> [];
                        [_ | Rest] -> Rest
                    end
            end
    end,

    FilteredLength = length(FilteredTasks),
    {PageTasks, Remaining} = case FilteredLength > Limit of
        true ->
            {lists:sublist(FilteredTasks, Limit), lists:nthtail(Limit, FilteredTasks)};
        false ->
            {FilteredTasks, []}
    end,

    NextCursor = case Remaining of
        [] -> undefined;
        [NextTask | _] -> NextTask#mcp_task.id
    end,

    TaskList = [format_task_for_api(T) || T <- PageTasks],

    {ok, #{
        tasks => TaskList,
        cursor => NextCursor
    }}.

%% @private
do_get_task(ClientPid, TaskId) ->
    case ets:lookup(?TASKS_TABLE, TaskId) of
        [#mcp_task{client_pid = Pid} = Task] when Pid =:= ClientPid ->
            {ok, format_task_for_api(Task)};
        [#mcp_task{}] ->
            {error, ?MCP_MSG_TASK_NOT_FOUND};
        [] ->
            {error, ?MCP_ERROR_TASK_NOT_FOUND}
    end.

%% @private
do_cancel_task(ClientPid, TaskId, Reason, State) ->
    case ets:lookup(?TASKS_TABLE, TaskId) of
        [#mcp_task{client_pid = Pid, status = Status} = Task] when Pid =:= ClientPid ->
            case Status of
                completed ->
                    {{error, ?MCP_ERROR_TASK_ALREADY_COMPLETED}, State};
                cancelled ->
                    {{error, ?MCP_ERROR_TASK_STATE_INVALID}, State};
                _ ->
                    cancel_task_timer(Task),

                    UpdatedTask = Task#mcp_task{
                        status = cancelled,
                        updated_at = erlang:system_time(millisecond),
                        error = #mcp_error{
                            code = ?MCP_ERROR_TASK_CANCELLED,
                            message = ?MCP_MSG_TASK_CANCELLED,
                            data = #{reason => Reason}
                        }
                    },
                    ets:insert(?TASKS_TABLE, UpdatedTask),

                    case Task#mcp_task.worker_pid of
                        undefined -> ok;
                        WorkerPid when is_pid(WorkerPid) ->
                            exit(WorkerPid, {task_cancelled, Reason})
                    end,

                    case Task#mcp_task.progress_token of
                        undefined -> ok;
                        Token -> erlmcp_progress:complete(Token)
                    end,

                    logger:info("Cancelled task ~p: ~s", [TaskId, Reason]),
                    send_task_notification(ClientPid, TaskId),

                    {{ok, cancelled}, State}
            end;
        [#mcp_task{}] ->
            {{error, ?MCP_MSG_TASK_NOT_FOUND}, State};
        [] ->
            {{error, ?MCP_ERROR_TASK_NOT_FOUND}, State}
    end.

%% @private
do_get_task_result(ClientPid, TaskId) ->
    case ets:lookup(?TASKS_TABLE, TaskId) of
        [#mcp_task{client_pid = Pid, status = Status, result = Result}] when Pid =:= ClientPid ->
            case Status of
                completed -> {ok, Result};
                failed -> {error, ?MCP_ERROR_TASK_FAILED};
                pending -> {error, ?MCP_ERROR_TASK_RESULT_NOT_READY};
                processing -> {error, ?MCP_ERROR_TASK_RESULT_NOT_READY};
                cancelled -> {error, ?MCP_ERROR_TASK_CANCELLED}
            end;
        [#mcp_task{}] ->
            {error, ?MCP_MSG_TASK_NOT_FOUND};
        [] ->
            {error, ?MCP_ERROR_TASK_NOT_FOUND}
    end.

%% @private
do_start_task_execution(TaskId, WorkerPid, State) ->
    case ets:lookup(?TASKS_TABLE, TaskId) of
        [#mcp_task{status = pending} = Task] ->
            MonitorRef = erlang:monitor(process, WorkerPid),

            UpdatedTask = Task#mcp_task{
                status = processing,
                worker_pid = WorkerPid,
                worker_monitor = MonitorRef,
                updated_at = erlang:system_time(millisecond)
            },
            ets:insert(?TASKS_TABLE, UpdatedTask),

            logger:debug("Task ~p started execution by worker ~p", [TaskId, WorkerPid]),

            {ok, State};
        [#mcp_task{status = Status}] ->
            logger:warning("Cannot start task ~p in status ~p", [TaskId, Status]),
            {{error, ?MCP_ERROR_TASK_STATE_INVALID}, State};
        [] ->
            {{error, ?MCP_ERROR_TASK_NOT_FOUND}, State}
    end.

%% @private
do_complete_task(TaskId, Result, State) ->
    case ets:lookup(?TASKS_TABLE, TaskId) of
        [#mcp_task{status = processing, timer_ref = TimerRef, progress_token = ProgressToken} = Task] ->
            cancel_timer(TimerRef),

            UpdatedTask = Task#mcp_task{
                status = completed,
                result = Result,
                updated_at = erlang:system_time(millisecond),
                timer_ref = undefined
            },
            ets:insert(?TASKS_TABLE, UpdatedTask),

            case ProgressToken of
                undefined -> ok;
                Token -> erlmcp_progress:complete(Token)
            end,

            logger:info("Task ~p completed successfully", [TaskId]),
            send_task_notification(Task#mcp_task.client_pid, TaskId),

            {ok, State#state{task_count = max(0, State#state.task_count - 1)}};
        [#mcp_task{status = Status}] ->
            logger:warning("Cannot complete task ~p in status ~p", [TaskId, Status]),
            {{error, ?MCP_ERROR_TASK_STATE_INVALID}, State};
        [] ->
            {{error, ?MCP_ERROR_TASK_NOT_FOUND}, State}
    end.

%% @private
do_fail_task(TaskId, Error, State) ->
    case ets:lookup(?TASKS_TABLE, TaskId) of
        [#mcp_task{status = Status, timer_ref = TimerRef, progress_token = ProgressToken} = Task]
          when Status =:= processing; Status =:= pending ->
            cancel_timer(TimerRef),

            ErrorRec = case Error of
                #mcp_error{} -> Error;
                _ when is_binary(Error) ->
                    #mcp_error{
                        code = ?MCP_ERROR_TASK_FAILED,
                        message = Error,
                        data = undefined
                    };
                _ ->
                    #mcp_error{
                        code = ?MCP_ERROR_TASK_FAILED,
                        message = ?MCP_MSG_TASK_FAILED,
                        data = Error
                    }
            end,

            UpdatedTask = Task#mcp_task{
                status = failed,
                error = ErrorRec,
                updated_at = erlang:system_time(millisecond),
                timer_ref = undefined
            },
            ets:insert(?TASKS_TABLE, UpdatedTask),

            case ProgressToken of
                undefined -> ok;
                Token -> erlmcp_progress:complete(Token)
            end,

            logger:error("Task ~p failed: ~p", [TaskId, Error]),
            send_task_notification(Task#mcp_task.client_pid, TaskId),

            {ok, State#state{task_count = max(0, State#state.task_count - 1)}};
        [#mcp_task{status = Status}] ->
            logger:warning("Cannot fail task ~p in status ~p", [TaskId, Status]),
            {{error, ?MCP_ERROR_TASK_STATE_INVALID}, State};
        [] ->
            {{error, ?MCP_ERROR_TASK_NOT_FOUND}, State}
    end.

%% @private
do_set_task_progress(TaskId, Progress, State) ->
    case ets:lookup(?TASKS_TABLE, TaskId) of
        [#mcp_task{status = processing, progress_token = ProgressToken} = Task] ->
            {Current, Total} = case Progress of
                {Cur, Tot} -> {Cur, Tot};
                Cur when is_number(Cur) -> {Cur, Task#mcp_task.total}
            end,

            UpdatedTask = Task#mcp_task{
                progress = Current,
                total = Total,
                updated_at = erlang:system_time(millisecond)
            },
            ets:insert(?TASKS_TABLE, UpdatedTask),

            case ProgressToken of
                undefined -> ok;
                Token ->
                    Update = #{
                        current => Current,
                        total => Total
                    },
                    erlmcp_progress:update(Token, Update)
            end,

            State;
        [#mcp_task{}] ->
            logger:warning("Cannot set progress on non-processing task ~p", [TaskId]),
            State;
        [] ->
            logger:warning("Task ~p not found for progress update", [TaskId]),
            State
    end.

%% @private
do_handle_task_timeout(TaskId, State) ->
    case ets:lookup(?TASKS_TABLE, TaskId) of
        [#mcp_task{status = Status} = Task] when Status =:= pending; Status =:= processing ->
            UpdatedTask = Task#mcp_task{
                status = failed,
                error = #mcp_error{
                    code = ?MCP_ERROR_TASK_TIMEOUT,
                    message = ?MCP_MSG_TASK_TIMEOUT,
                    data = #{timeout => Task#mcp_task.timeout_ms}
                },
                updated_at = erlang:system_time(millisecond),
                timer_ref = undefined
            },
            ets:insert(?TASKS_TABLE, UpdatedTask),

            logger:warning("Task ~p timed out after ~p ms", [TaskId, Task#mcp_task.timeout_ms]),

            case Task#mcp_task.worker_pid of
                undefined -> ok;
                WorkerPid -> exit(WorkerPid, task_timeout)
            end,

            State#state{task_count = max(0, State#state.task_count - 1)};
        [#mcp_task{}] ->
            State;
        [] ->
            logger:warning("Timeout for unknown task ~p", [TaskId]),
            State
    end.

%% @private
do_handle_worker_down(MonitorRef, State) ->
    case ets:foldl(
        fun(#mcp_task{worker_monitor = Mon, status = processing} = Task, Acc) when Mon =:= MonitorRef ->
                [Task | Acc];
           (_, Acc) ->
                Acc
        end,
        [],
        ?TASKS_TABLE
    ) of
        [#mcp_task{id = TaskId} = Task] ->
            UpdatedTask = Task#mcp_task{
                status = failed,
                error = #mcp_error{
                    code = ?MCP_ERROR_TASK_FAILED,
                    message = <<"Worker process died unexpectedly">>,
                    data = #{}
                },
                updated_at = erlang:system_time(millisecond)
            },
            ets:insert(?TASKS_TABLE, UpdatedTask),

            logger:error("Task ~p failed due to worker death", [TaskId]),

            State#state{task_count = max(0, State#state.task_count - 1)};
        [] ->
            State
    end.

%% @private
do_cleanup_expired_tasks(State) ->
    Now = erlang:system_time(millisecond),

    ExpiredTasks = ets:foldl(
        fun(#mcp_task{id = Id, expires_at = Expires, status = Status}, Acc) ->
            case Status of
                completed ->
                    case Expires =< Now of
                        true -> [Id | Acc];
                        false -> Acc
                    end;
                _ ->
                    Acc
            end
        end,
        [],
        ?TASKS_TABLE
    ),

    lists:foreach(
        fun(TaskId) ->
            ets:delete(?TASKS_TABLE, TaskId),
            logger:debug("Cleaned up expired task ~p", [TaskId])
        end,
        ExpiredTasks
    ),

    State#state{task_count = max(0, State#state.task_count - length(ExpiredTasks))}.

%% @private
generate_task_id() ->
    Unique = erlang:unique_integer([positive, monotonic]),
    Time = erlang:system_time(microsecond),
    <<Unique:64, Time:64>>.

%% @private
format_task_for_api(#mcp_task{
    id = Id,
    status = Status,
    action = Action,
    metadata = Metadata,
    error = Error,
    created_at = CreatedAt,
    updated_at = UpdatedAt,
    expires_at = ExpiresAt,
    progress = Progress,
    total = Total
}) ->
    Base = #{
        ?MCP_PARAM_TASK_ID => Id,
        ?MCP_PARAM_STATUS => status_to_binary(Status),
        <<"action">> => Action,
        <<"createdAt">> => CreatedAt,
        <<"updatedAt">> => UpdatedAt
    },

    Base1 = case Progress of
        undefined -> Base;
        _ -> Base#{?MCP_PARAM_PROGRESS => Progress}
    end,

    Base2 = case Total of
        undefined -> Base1;
        _ -> Base1#{?MCP_PARAM_TOTAL => Total}
    end,

    Base3 = case ExpiresAt of
        undefined -> Base2;
        _ -> Base2#{<<"expiresAt">> => ExpiresAt}
    end,

    Base4 = case maps:size(Metadata) of
        0 -> Base3;
        _ -> Base3#{?MCP_PARAM_METADATA => Metadata}
    end,

    Base5 = case Error of
        undefined -> Base4;
        #mcp_error{code = Code, message = Msg, data = Data} ->
            Base4#{?MCP_PARAM_ERROR => #{
                <<"code">> => Code,
                <<"message">> => Msg,
                <<"data">> => Data
            }}
    end,

    Base5.

%% @private
status_to_binary(pending) -> <<"pending">>;
status_to_binary(processing) -> <<"processing">>;
status_to_binary(completed) -> <<"completed">>;
status_to_binary(failed) -> <<"failed">>;
status_to_binary(cancelled) -> <<"cancelled">>.

%% @private
cancel_task_timer(#mcp_task{timer_ref = undefined}) -> ok;
cancel_task_timer(#mcp_task{timer_ref = TimerRef}) ->
    erlang:cancel_timer(TimerRef),
    receive
        {task_timeout, _} -> ok
    after 0 ->
        ok
    end.

%% @private
cancel_timer(undefined) -> ok;
cancel_timer(TimerRef) when is_reference(TimerRef) ->
    erlang:cancel_timer(TimerRef),
    receive
        {task_timeout, _} -> ok
    after 0 ->
        ok
    end.

%% @private
send_task_notification(undefined, _TaskId) -> ok;
send_task_notification(ClientPid, TaskId) when is_pid(ClientPid) ->
    case ets:lookup(?TASKS_TABLE, TaskId) of
        [#mcp_task{} = Task] ->
            Notification = #{
                ?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,
                ?JSONRPC_FIELD_METHOD => ?MCP_METHOD_NOTIFICATIONS_TASKS_STATUS,
                ?JSONRPC_FIELD_PARAMS => #{
                    ?MCP_PARAM_TASK => format_task_for_api(Task)
                }
            },
            ClientPid ! {mcp_notification, Notification},
            ok;
        [] ->
            ok
    end.

%% @private
schedule_cleanup() ->
    erlang:send_after(300000, self(), cleanup_expired_tasks).
