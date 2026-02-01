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
-export([create_task/3, create_task/4, create/3, create/4]).
-export([list_tasks/2, list_tasks/3]).
-export([get_task/2, get/2]).
-export([cancel_task/3, cancel/3, cancel_task/2]).
-export([get_task_result/2, get_result/2]).
-export([update_task/3, update_status/3, update_progress/3]).
-export([cleanup_expired/0, cleanup_expired/1]).
-export([get_max_concurrent/0, stop/1, complete/3]).

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
-type update_function() :: fun((map()) -> map()).

-record(state, {
    pending_cleanup = [] :: [task_id()]
    % Note: task_count removed - now stored as atomic ETS counter for idempotency
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
-spec create_task(pid() | undefined, map(), map() | undefined) ->
    {ok, task_id()} | {error, term()}.
create_task(ClientPid, Action, Metadata) when is_map(Action) ->
    MetadataMap = case Metadata of
        undefined -> #{};
        _ when is_map(Metadata) -> Metadata
    end,
    gen_server:call(?SERVER, {create_task, ClientPid, Action, MetadataMap}).

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

%% @doc Alternative API: Create task (alias for create_task/3).
-spec create(pid() | undefined, map(), map()) ->
    {ok, task_id()} | {error, term()}.
create(ClientPid, Action, Metadata) ->
    create_task(ClientPid, Action, Metadata).

%% @doc Create task with options map (supports ttl_ms for expiry).
-spec create(pid() | undefined, map(), map(), map()) ->
    {ok, task_id()} | {error, term()}.
create(ClientPid, Action, Metadata, Options) when is_map(Action), is_map(Metadata), is_map(Options) ->
    gen_server:call(?SERVER, {create_task, ClientPid, Action, Metadata, Options}).

%% @doc Alternative API: Create task with options (alias for create_task/4).
-spec create_task(pid() | undefined, map(), map(), map()) ->
    {ok, task_id()} | {error, term()}.
create_task(ClientPid, Action, Metadata, Options) ->
    create(ClientPid, Action, Metadata, Options).

%% @doc List tasks without cursor (defaults to limit 100).
-spec list_tasks(pid() | undefined) ->
    {ok, task_list_result()} | {error, term()}.
list_tasks(ClientPid) ->
    list_tasks(ClientPid, undefined, 100).

%% @doc List tasks with limit only (no cursor).
-spec list_tasks(pid() | undefined, pos_integer()) ->
    {ok, task_list_result()} | {error, term()}.
list_tasks(ClientPid, Limit) when is_integer(Limit), Limit > 0 ->
    list_tasks(ClientPid, undefined, Limit).

%% @doc Alternative API: Get task (alias for get_task/2).
-spec get(pid() | undefined, task_id()) ->
    {ok, task()} | {error, term()}.
get(ClientPid, TaskId) ->
    get_task(ClientPid, TaskId).

%% @doc Alternative API: Get task result (alias for get_task_result/2).
-spec get_result(pid() | undefined, task_id()) ->
    {ok, term()} | {error, term()}.
get_result(ClientPid, TaskId) ->
    get_task_result(ClientPid, TaskId).

%% @doc Cancel task with default reason.
-spec cancel_task(pid() | undefined, task_id()) ->
    {ok, cancelled} | {error, term()}.
cancel_task(ClientPid, TaskId) ->
    cancel_task(ClientPid, TaskId, <<"Cancelled by request">>).

%% @doc Alternative API: Cancel task (alias for cancel_task/3).
-spec cancel(pid() | undefined, task_id(), binary()) ->
    {ok, cancelled} | {error, term()}.
cancel(ClientPid, TaskId, Reason) ->
    cancel_task(ClientPid, TaskId, Reason).

%% @doc Alternative API: Complete task (alias for complete_task/2).
-spec complete(pid() | undefined, task_id(), term()) -> ok | {error, term()}.
complete(_ClientPid, TaskId, Result) ->
    complete_task(TaskId, Result).

%% @doc Update task with custom function.
%% Fun receives task map and returns updated task map.
-spec update_task(pid() | undefined, task_id(), fun((map()) -> map())) ->
    ok | {error, term()}.
update_task(ClientPid, TaskId, UpdateFun) when is_function(UpdateFun, 1) ->
    gen_server:call(?SERVER, {update_task, ClientPid, TaskId, UpdateFun}).

%% @doc Directly update task status.
-spec update_status(pid() | undefined, task_id(), binary()) ->
    ok | {error, term()}.
update_status(ClientPid, TaskId, Status) when is_binary(Status) ->
    gen_server:call(?SERVER, {update_status, ClientPid, TaskId, Status}).

%% @doc Update progress with token.
-spec update_progress(pid() | undefined, task_id(), map()) -> ok | {error, term()}.
update_progress(_ClientPid, TaskId, Update) when is_map(Update) ->
    case maps:get(<<"progressToken">>, Update, undefined) of
        undefined ->
            {error, ?MCP_MSG_INVALID_PROGRESS_TOKEN};
        Token ->
            _Progress = maps:get(<<"progress">>, Update, 0.0),
            Total = maps:get(<<"total">>, Update, undefined),
            set_task_progress(TaskId, {_Progress, Total})
    end.

%% @doc Manually trigger cleanup of expired tasks.
-spec cleanup_expired() -> {ok, non_neg_integer()}.
cleanup_expired() ->
    gen_server:call(?SERVER, cleanup_expired).

%% @doc Alternative API: Cleanup expired (for compatibility).
-spec cleanup_expired(pid()) -> {ok, non_neg_integer()}.
cleanup_expired(_Pid) ->
    cleanup_expired().

%% @doc Get maximum concurrent tasks limit.
-spec get_max_concurrent() -> pos_integer().
get_max_concurrent() ->
    ?MAX_CONCURRENT_TASKS.

%% @doc Stop the tasks manager.
-spec stop(pid()) -> ok.
stop(Pid) when is_pid(Pid) ->
    gen_server:stop(Pid).

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

    % Create ETS table for tasks (always recreate to ensure clean state)
    % If table exists from previous run, delete it first
    case ets:info(?TASKS_TABLE) of
        undefined -> ok;
        _ -> ets:delete(?TASKS_TABLE)
    end,

    ets:new(?TASKS_TABLE, [
        named_table,
        public,
        set,
        {keypos, #mcp_task.id},
        {read_concurrency, true},
        {write_concurrency, true}
    ]),

    % Initialize atomic task counter for idempotent operations
    % Using ETS counter instead of state ensures:
    % - Atomic increments (no race conditions)
    % - Survives process restarts
    % - No counter drift on supervisor restart
    ets:insert(?TASKS_TABLE, {task_count, 0}),

    % Schedule periodic cleanup of expired tasks
    schedule_cleanup(),

    {ok, #state{
        pending_cleanup = []
    }}.

%% @private
-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {reply, term(), #state{}}.
handle_call({create_task, ClientPid, Action, Metadata}, _From, State) ->
    case get_task_count() >= ?MAX_CONCURRENT_TASKS of
        true ->
            {reply, {error, {max_concurrent_tasks, ?MCP_ERROR_MAX_CONCURRENT_TASKS}}, State};
        false ->
            {Reply, NewState} = do_create_task(ClientPid, Action, Metadata, State),
            {reply, Reply, NewState}
    end;

handle_call({create_task, ClientPid, Action, Metadata, Options}, _From, State) ->
    case get_task_count() >= ?MAX_CONCURRENT_TASKS of
        true ->
            {reply, {error, {max_concurrent_tasks, ?MCP_ERROR_MAX_CONCURRENT_TASKS}}, State};
        false ->
            {Reply, NewState} = do_create_task_with_options(ClientPid, Action, Metadata, Options, State),
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

handle_call({update_task, ClientPid, TaskId, UpdateFun}, _From, State) ->
    Reply = do_update_task(ClientPid, TaskId, UpdateFun),
    {reply, Reply, State};

handle_call({update_status, ClientPid, TaskId, Status}, _From, State) ->
    Reply = do_update_status(ClientPid, TaskId, Status),
    {reply, Reply, State};

handle_call(cleanup_expired, _From, State) ->
    {ok, Count, NewState} = do_cleanup_expired_tasks_with_count(State),
    {reply, {ok, Count}, NewState};

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
terminate(_Reason, _State) ->
    Count = get_task_count(),
    logger:info("Tasks manager terminating with ~p active tasks", [Count]),
    % Clean up ETS table to prevent state leakage
    catch ets:delete(?TASKS_TABLE),
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

    % Always generate a progress token (per MCP spec requirements)
    ProgressToken = erlmcp_progress:generate_token(),

    Now = erlang:system_time(millisecond),

    % CONCURRENCY SAFETY: Create timer BEFORE creating task to avoid split-brain state.
    % Previously, the task was inserted twice: once without timer_ref, then with timer_ref.
    % This created a window where readers could see incomplete state (timer_ref = undefined).
    % Now we create the complete task state before any ETS insertion.
    TimerRef = erlang:send_after(TimeoutMs, self(), {task_timeout, TaskId}),

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
        timer_ref = TimerRef  % Timer created BEFORE task insertion
    },

    % Single atomic insert of complete task state
    ets:insert(?TASKS_TABLE, Task),

    % Create progress tracking
    ProgressPid = resolve_client_pid(ClientPid),
    erlmcp_progress:create(ProgressPid, <<"Task created: ", TaskId/binary>>),

    % Atomically increment task counter (idempotent operation)
    % Using update_counter ensures no race conditions or counter drift
    NewCount = ets:update_counter(?TASKS_TABLE, task_count, {2, 1}, {task_count, 0}),

    logger:debug("Created task ~p for client ~p (total: ~p)", [TaskId, ClientPid, NewCount]),
    send_task_notification(ClientPid, TaskId),

    {{ok, TaskId}, State}.

%% @private
do_list_tasks(ClientPid, Cursor, Limit) ->
    % Normalize ClientPid for comparison - resolve atoms to their registered pids
    NormalizedPid = resolve_client_pid(ClientPid),
    AllTasks = ets:foldl(
        fun(#mcp_task{client_pid = TaskClientPid} = Task, Acc) ->
            % Match if:
            % 1. TaskClientPid is undefined (all tasks)
            % 2. NormalizedPid is undefined (return all)
            % 3. Both match exactly (both pids or both undefined)
            % 4. TaskClientPid is an atom that resolves to NormalizedPid
            TaskNormalized = resolve_client_pid(TaskClientPid),
            case match_client_pids(NormalizedPid, TaskNormalized) of
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
                    {_Before, After} = lists:splitwith(
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
        <<"tasks">> => TaskList,
        <<"cursor">> => NextCursor
    }}.

%% @private
do_get_task(ClientPid, TaskId) ->
    NormalizedPid = resolve_client_pid(ClientPid),
    case ets:lookup(?TASKS_TABLE, TaskId) of
        [#mcp_task{client_pid = TaskPid} = Task] ->
            TaskNormalized = resolve_client_pid(TaskPid),
            case match_client_pids(NormalizedPid, TaskNormalized) of
                true -> {ok, format_task_for_api(Task)};
                false -> {error, ?MCP_MSG_TASK_NOT_FOUND}
            end;
        [] ->
            {error, ?MCP_ERROR_TASK_NOT_FOUND}
    end.

%% @private
do_cancel_task(ClientPid, TaskId, Reason, State) ->
    NormalizedPid = resolve_client_pid(ClientPid),
    case ets:lookup(?TASKS_TABLE, TaskId) of
        [#mcp_task{client_pid = TaskPid, status = Status} = Task] ->
            TaskNormalized = resolve_client_pid(TaskPid),
            case match_client_pids(NormalizedPid, TaskNormalized) of
                true ->
                    case Status of
                        completed ->
                            {{error, {task_already_completed, ?MCP_ERROR_TASK_ALREADY_COMPLETED}}, State};
                        cancelled ->
                            {{error, {task_state_invalid, ?MCP_ERROR_TASK_STATE_INVALID}}, State};
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

                            erlmcp_progress:complete(Task#mcp_task.progress_token),

                            logger:info("Cancelled task ~p: ~s", [TaskId, Reason]),
                            send_task_notification(ClientPid, TaskId),

                            {{ok, cancelled}, State}
                    end;
                false ->
                    {{error, {task_not_found, ?MCP_ERROR_TASK_NOT_FOUND}}, State}
            end;
        [] ->
            {{error, {task_not_found, ?MCP_ERROR_TASK_NOT_FOUND}}, State}
    end.

%% @private
do_get_task_result(ClientPid, TaskId) ->
    NormalizedPid = resolve_client_pid(ClientPid),
    case ets:lookup(?TASKS_TABLE, TaskId) of
        [#mcp_task{client_pid = TaskPid, status = Status, result = Result}] ->
            TaskNormalized = resolve_client_pid(TaskPid),
            case match_client_pids(NormalizedPid, TaskNormalized) of
                true ->
                    case Status of
                        completed -> {ok, Result};
                        failed -> {error, ?MCP_ERROR_TASK_FAILED};
                        pending -> {error, ?MCP_ERROR_TASK_RESULT_NOT_READY};
                        processing -> {error, ?MCP_ERROR_TASK_RESULT_NOT_READY};
                        cancelled -> {error, ?MCP_ERROR_TASK_CANCELLED}
                    end;
                false ->
                    {error, ?MCP_MSG_TASK_NOT_FOUND}
            end;
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
            {{error, {task_state_invalid, ?MCP_ERROR_TASK_STATE_INVALID}}, State};
        [] ->
            {{error, {task_not_found, ?MCP_ERROR_TASK_NOT_FOUND}}, State}
    end.

%% @private
do_complete_task(TaskId, Result, State) ->
    case ets:lookup(?TASKS_TABLE, TaskId) of
        [#mcp_task{status = Status, timer_ref = TimerRef, progress_token = ProgressToken} = Task]
          when Status =:= processing; Status =:= pending ->
            cancel_timer(TimerRef),

            UpdatedTask = Task#mcp_task{
                status = completed,
                result = Result,
                updated_at = erlang:system_time(millisecond),
                timer_ref = undefined
            },
            ets:insert(?TASKS_TABLE, UpdatedTask),

            erlmcp_progress:complete(ProgressToken),

            logger:info("Task ~p completed successfully", [TaskId]),
            send_task_notification(Task#mcp_task.client_pid, TaskId),

            % Atomically decrement task count
            decrement_task_count(),
            {ok, State};
        [#mcp_task{status = Status}] ->
            logger:warning("Cannot complete task ~p in status ~p", [TaskId, Status]),
            {{error, {task_state_invalid, ?MCP_ERROR_TASK_STATE_INVALID}}, State};
        [] ->
            {{error, {task_not_found, ?MCP_ERROR_TASK_NOT_FOUND}}, State}
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

            erlmcp_progress:complete(ProgressToken),

            logger:error("Task ~p failed: ~p", [TaskId, Error]),
            send_task_notification(Task#mcp_task.client_pid, TaskId),

            % Atomically decrement task count
            decrement_task_count(),
            {ok, State};
        [#mcp_task{status = Status}] ->
            logger:warning("Cannot fail task ~p in status ~p", [TaskId, Status]),
            {{error, {task_state_invalid, ?MCP_ERROR_TASK_STATE_INVALID}}, State};
        [] ->
            {{error, {task_not_found, ?MCP_ERROR_TASK_NOT_FOUND}}, State}
    end.

%% @private
do_set_task_progress(TaskId, Progress, State) ->
    case ets:lookup(?TASKS_TABLE, TaskId) of
        [#mcp_task{status = Status, progress_token = ProgressToken} = Task]
          when Status =:= processing; Status =:= pending ->
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

            Update = #{
                current => Current,
                total => Total
            },
            erlmcp_progress:update(ProgressToken, Update),

            State;
        [#mcp_task{}] ->
            logger:warning("Cannot set progress on task ~p with completed/failed status", [TaskId]),
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

            % Atomically decrement task count
            decrement_task_count(),
            State;
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

            % Atomically decrement task count
            decrement_task_count(),
            State;
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

    % Atomically decrement task count by number of cleaned tasks
    decrement_task_count(length(ExpiredTasks)),
    State.

%% @private
generate_task_id() ->
    Unique = erlang:unique_integer([positive, monotonic]),
    Time = erlang:system_time(microsecond),
    Random = crypto:strong_rand_bytes(16),
    <<Unique:64, Time:64, Random/binary>>.

%% @private
%% @doc Get current task count atomically from ETS
%% This function reads the atomic counter from ETS, ensuring:
%% - No race conditions
%% - Consistent reads across process restarts
%% - Idempotent operation (safe to retry)
get_task_count() ->
    case ets:lookup(?TASKS_TABLE, task_count) of
        [{task_count, Count}] -> Count;
        [] -> 0
    end.

%% @private
%% @doc Decrement task count atomically
%% Used when tasks are cancelled or completed
decrement_task_count() ->
    ets:update_counter(?TASKS_TABLE, task_count, {2, -1, 0, 0}, {task_count, 0}).

%% @private
%% @doc Decrement task count by N atomically
%% Used when multiple tasks are cleaned up
decrement_task_count(N) when N > 0 ->
    ets:update_counter(?TASKS_TABLE, task_count, {2, -N, 0, 0}, {task_count, 0});
decrement_task_count(_) ->
    get_task_count().

%% @private
format_task_for_api(#mcp_task{
    id = Id,
    status = Status,
    action = Action,
    metadata = Metadata,
    result = Result,
    error = Error,
    created_at = CreatedAt,
    updated_at = UpdatedAt,
    expires_at = ExpiresAt,
    progress_token = ProgressToken,
    progress = Progress,
    total = Total
}) ->
    Base = #{
        ?MCP_PARAM_TASK_ID => Id,
        ?MCP_PARAM_STATUS => status_to_binary(Status),
        <<"action">> => Action,
        <<"metadata">> => Metadata,
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

    Base4 = case ProgressToken of
        undefined -> Base3#{<<"progressToken">> => null};
        _ -> Base3#{<<"progressToken">> => ProgressToken}
    end,

    Base5 = case Result of
        undefined -> Base4;
        _ -> Base4#{<<"result">> => Result}
    end,

    Base6 = case Error of
        undefined -> Base5;
        #mcp_error{code = Code, message = Msg, data = Data} ->
            %% Convert data map keys to binary for JSON compatibility
            DataBin = maps:map(
                fun(_K, V) -> V end,
                maps:fold(fun(K, V, Acc) -> Acc#{atom_to_binary(K) => V} end, #{}, Data)
            ),
            Base5#{?MCP_PARAM_ERROR => #{
                <<"code">> => Code,
                <<"message">> => Msg,
                <<"data">> => DataBin
            }}
    end,

    Base6.

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
    send_notification_to_pid(ClientPid, TaskId);
send_task_notification(ClientPidName, TaskId) when is_atom(ClientPidName) ->
    % Handle registered process name
    case whereis(ClientPidName) of
        undefined -> ok;
        Pid when is_pid(Pid) -> send_notification_to_pid(Pid, TaskId)
    end.

%% @private
send_notification_to_pid(TargetPid, TaskId) ->
    case ets:lookup(?TASKS_TABLE, TaskId) of
        [#mcp_task{} = Task] ->
            Notification = #{
                ?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,
                ?JSONRPC_FIELD_METHOD => ?MCP_METHOD_NOTIFICATIONS_TASKS_STATUS,
                ?JSONRPC_FIELD_PARAMS => #{
                    ?MCP_PARAM_TASK => format_task_for_api(Task)
                }
            },
            TargetPid ! {mcp_notification, Notification},
            ok;
        [] ->
            ok
    end.

%% @private
schedule_cleanup() ->
    erlang:send_after(300000, self(), cleanup_expired_tasks).

%% @private
%% Resolve client identifier to pid for operations requiring actual pids
%% Handles: undefined -> undefined, pid() -> pid(), atom() -> look up registered name
resolve_client_pid(undefined) -> undefined;
resolve_client_pid(Pid) when is_pid(Pid) -> Pid;
resolve_client_pid(Name) when is_atom(Name) ->
    case whereis(Name) of
        undefined -> undefined;
        Pid when is_pid(Pid) -> Pid
    end.

%% @private
%% Match client pids for task filtering
%% undefined matches everything (get all tasks)
match_client_pids(undefined, _TaskPid) -> true;
match_client_pids(_QueryPid, undefined) -> true;
match_client_pids(QueryPid, TaskPid) when QueryPid =:= TaskPid -> true;
match_client_pids(_, _) -> false.

%% @private
do_create_task_with_options(ClientPid, Action, Metadata, Options, State) ->
    TaskId = generate_task_id(),
    %% Check both Metadata (binary key) and Options (atom key) for timeout
    TimeoutMs = case maps:get(<<"timeout">>, Metadata, undefined) of
        undefined ->
            maps:get(timeout_ms, Options, ?DEFAULT_TASK_TIMEOUT_MS);
        Val ->
            Val
    end,

    % Calculate expiry time based on ttl_ms option or default
    TTL = maps:get(ttl_ms, Options, maps:get(<<"expiresAfter">>, Metadata, ?DEFAULT_EXPIRY_MS)),
    ExpiresAt = erlang:system_time(millisecond) + TTL,

    % Always generate a progress token (per MCP spec requirements)
    ProgressToken = erlmcp_progress:generate_token(),

    Now = erlang:system_time(millisecond),

    % CONCURRENCY SAFETY: Create timer BEFORE creating task to avoid split-brain state.
    % Previously, the task was inserted twice: once without timer_ref, then with timer_ref.
    % This created a window where readers could see incomplete state (timer_ref = undefined).
    % Now we create the complete task state before any ETS insertion.
    TimerRef = erlang:send_after(TimeoutMs, self(), {task_timeout, TaskId}),

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
        timer_ref = TimerRef  % Timer created BEFORE task insertion
    },

    % Single atomic insert of complete task state
    ets:insert(?TASKS_TABLE, Task),

    % Create progress tracking
    ProgressPid = resolve_client_pid(ClientPid),
    erlmcp_progress:create(ProgressPid, <<"Task created: ", TaskId/binary>>),

    % Atomically increment task counter (idempotent operation)
    NewCount = ets:update_counter(?TASKS_TABLE, task_count, {2, 1}, {task_count, 0}),

    logger:debug("Created task ~p for client ~p with TTL ~p ms (total: ~p)", [TaskId, ClientPid, TTL, NewCount]),
    send_task_notification(ClientPid, TaskId),

    {{ok, TaskId}, State}.

%% @private
do_update_task(ClientPid, TaskId, UpdateFun) ->
    NormalizedPid = resolve_client_pid(ClientPid),
    case ets:lookup(?TASKS_TABLE, TaskId) of
        [#mcp_task{client_pid = TaskPid} = Task] ->
            TaskNormalized = resolve_client_pid(TaskPid),
            case match_client_pids(NormalizedPid, TaskNormalized) of
                true ->
                    try
                        TaskMap = format_task_for_api(Task),
                        UpdatedTaskMap = UpdateFun(TaskMap),

                        % Extract updated fields from map
                        NewMetadata = maps:get(<<"metadata">>, UpdatedTaskMap, Task#mcp_task.metadata),

                        UpdatedTask = Task#mcp_task{
                            metadata = NewMetadata,
                            updated_at = erlang:system_time(millisecond)
                        },
                        ets:insert(?TASKS_TABLE, UpdatedTask),
                        ok
                    catch
                        _:Error ->
                            logger:error("Update function failed for task ~p: ~p", [TaskId, Error]),
                            {error, invalid_update_function}
                    end;
                false ->
                    {error, ?MCP_MSG_TASK_NOT_FOUND}
            end;
        [] ->
            {error, ?MCP_ERROR_TASK_NOT_FOUND}
    end.

%% @private
do_update_status(ClientPid, TaskId, StatusBin) ->
    Status = case StatusBin of
        <<"pending">> -> pending;
        <<"processing">> -> processing;
        <<"completed">> -> completed;
        <<"failed">> -> failed;
        <<"cancelled">> -> cancelled;
        _ -> undefined
    end,

    case Status of
        undefined ->
            {error, {task_state_invalid, ?MCP_ERROR_TASK_STATE_INVALID}};
        _ ->
            NormalizedPid = resolve_client_pid(ClientPid),
            case ets:lookup(?TASKS_TABLE, TaskId) of
                [#mcp_task{client_pid = TaskPid} = Task] ->
                    TaskNormalized = resolve_client_pid(TaskPid),
                    case match_client_pids(NormalizedPid, TaskNormalized) of
                        true ->
                            UpdatedTask = Task#mcp_task{
                                status = Status,
                                updated_at = erlang:system_time(millisecond)
                            },
                            ets:insert(?TASKS_TABLE, UpdatedTask),
                            send_task_notification(ClientPid, TaskId),
                            ok;
                        false ->
                            {error, ?MCP_MSG_TASK_NOT_FOUND}
                    end;
                [] ->
                    {error, ?MCP_ERROR_TASK_NOT_FOUND}
            end
    end.

%% @private
do_cleanup_expired_tasks_with_count(State) ->
    Now = erlang:system_time(millisecond),

    ExpiredTasks = ets:foldl(
        fun(#mcp_task{id = Id, expires_at = Expires, status = Status}, Acc) ->
            case Status of
                completed ->
                    case Expires =< Now of
                        true -> [Id | Acc];
                        false -> Acc
                    end;
                failed ->
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

    Count = length(ExpiredTasks),
    % Atomically decrement task count
    decrement_task_count(Count),
    {ok, Count, State}.
