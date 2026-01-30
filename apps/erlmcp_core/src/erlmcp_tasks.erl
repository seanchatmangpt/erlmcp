-module(erlmcp_tasks).
-behaviour(gen_server).

-include("erlmcp.hrl").

%% API exports
-export([
    start_link/0,
    create_task/2,
    create_task/3,
    list_tasks/0,
    list_tasks/1,
    get_task/1,
    get_task_result/1,
    cancel_task/1,
    update_task_progress/3,
    update_task_status/2,
    complete_task/2,
    fail_task/2,
    cleanup_expired_tasks/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Default task expiry: 1 hour after completion
-define(DEFAULT_TASK_EXPIRY_MS, 3600000).
-define(CLEANUP_INTERVAL_MS, 60000).  % Cleanup every minute

%% State record
-record(state, {
    tasks = #{} :: #{task_id() => #mcp_task{}},
    max_tasks = 1000 :: pos_integer(),
    default_expiry_ms = ?DEFAULT_TASK_EXPIRY_MS :: pos_integer(),
    cleanup_timer :: reference() | undefined
}).

-type state() :: #state{}.

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec create_task(binary(), map()) -> {ok, task_id()} | {error, term()}.
create_task(Name, Options) when is_binary(Name), is_map(Options) ->
    gen_server:call(?MODULE, {create_task, Name, Options}).

-spec create_task(binary(), map(), pos_integer()) -> {ok, task_id()} | {error, term()}.
create_task(Name, Options, ExpiryMs) when is_binary(Name), is_map(Options), is_integer(ExpiryMs) ->
    gen_server:call(?MODULE, {create_task, Name, Options, ExpiryMs}).

-spec list_tasks() -> {ok, [#mcp_task{}]}.
list_tasks() ->
    gen_server:call(?MODULE, list_tasks).

-spec list_tasks(map()) -> {ok, [#mcp_task{}]}.
list_tasks(Filter) when is_map(Filter) ->
    gen_server:call(?MODULE, {list_tasks, Filter}).

-spec get_task(task_id()) -> {ok, #mcp_task{}} | {error, not_found}.
get_task(TaskId) when is_binary(TaskId) ->
    gen_server:call(?MODULE, {get_task, TaskId}).

-spec get_task_result(task_id()) -> {ok, term()} | {error, not_found | not_ready}.
get_task_result(TaskId) when is_binary(TaskId) ->
    gen_server:call(?MODULE, {get_task_result, TaskId}).

-spec cancel_task(task_id()) -> ok | {error, term()}.
cancel_task(TaskId) when is_binary(TaskId) ->
    gen_server:call(?MODULE, {cancel_task, TaskId}).

-spec update_task_progress(task_id(), float(), float()) -> ok | {error, term()}.
update_task_progress(TaskId, Progress, Total) when is_binary(TaskId), is_number(Progress), is_number(Total) ->
    gen_server:call(?MODULE, {update_task_progress, TaskId, Progress, Total}).

-spec update_task_status(task_id(), task_status()) -> ok | {error, term()}.
update_task_status(TaskId, Status) when is_binary(TaskId), is_atom(Status) ->
    gen_server:call(?MODULE, {update_task_status, TaskId, Status}).

-spec complete_task(task_id(), term()) -> ok | {error, term()}.
complete_task(TaskId, Result) when is_binary(TaskId) ->
    gen_server:call(?MODULE, {complete_task, TaskId, Result}).

-spec fail_task(task_id(), #mcp_error{} | binary()) -> ok | {error, term()}.
fail_task(TaskId, Error) when is_binary(TaskId) ->
    gen_server:call(?MODULE, {fail_task, TaskId, Error}).

-spec cleanup_expired_tasks() -> {ok, integer()}.
cleanup_expired_tasks() ->
    gen_server:call(?MODULE, cleanup_expired_tasks).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([]) -> {ok, state()}.
init([]) ->
    process_flag(trap_exit, true),

    % Start periodic cleanup timer
    TimerRef = erlang:send_after(?CLEANUP_INTERVAL_MS, self(), cleanup_expired_tasks),

    State = #state{
        cleanup_timer = TimerRef
    },

    logger:info("Task manager started", []),
    {ok, State}.

-spec handle_call(term(), {pid(), term()}, state()) ->
    {reply, term(), state()}.

handle_call({create_task, Name, Options}, _From, State) ->
    handle_call({create_task, Name, Options, State#state.default_expiry_ms}, _From, State);

handle_call({create_task, Name, Options, ExpiryMs}, _From, State) ->
    case maps:size(State#state.tasks) >= State#state.max_tasks of
        true ->
            {reply, {error, {?MCP_ERROR_MAX_CONCURRENT_TASKS, ?MCP_MSG_MAX_CONCURRENT_TASKS, #{}}}, State};
        false ->
            TaskId = generate_task_id(),
            Now = erlang:system_time(millisecond),

            Task = #mcp_task{
                id = TaskId,
                name = Name,
                status = working,
                created_at = Now,
                updated_at = Now,
                expires_at = undefined,  % Set when completed
                metadata = Options
            },

            NewTasks = maps:put(TaskId, Task, State#state.tasks),
            {reply, {ok, TaskId}, State#state{tasks = NewTasks}}
    end;

handle_call(list_tasks, _From, State) ->
    Tasks = maps:values(State#state.tasks),
    {reply, {ok, Tasks}, State};

handle_call({list_tasks, Filter}, _From, State) ->
    Tasks = maps:values(State#state.tasks),
    FilteredTasks = filter_tasks(Tasks, Filter),
    {reply, {ok, FilteredTasks}, State};

handle_call({get_task, TaskId}, _From, State) ->
    case maps:find(TaskId, State#state.tasks) of
        {ok, Task} ->
            {reply, {ok, Task}, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({get_task_result, TaskId}, _From, State) ->
    case maps:find(TaskId, State#state.tasks) of
        {ok, #mcp_task{status = completed, result = Result}} ->
            {reply, {ok, Result}, State};
        {ok, #mcp_task{status = failed, error = Error}} ->
            {reply, {error, {task_failed, Error}}, State};
        {ok, #mcp_task{status = Status}} when Status =:= working; Status =:= input_required ->
            {reply, {error, not_ready}, State};
        {ok, #mcp_task{status = cancelled}} ->
            {reply, {error, cancelled}, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({cancel_task, TaskId}, _From, State) ->
    case maps:find(TaskId, State#state.tasks) of
        {ok, Task = #mcp_task{status = Status}} when Status =:= working; Status =:= input_required ->
            Now = erlang:system_time(millisecond),
            UpdatedTask = Task#mcp_task{
                status = cancelled,
                updated_at = Now,
                completed_at = Now,
                expires_at = Now + State#state.default_expiry_ms
            },
            NewTasks = maps:put(TaskId, UpdatedTask, State#state.tasks),
            {reply, ok, State#state{tasks = NewTasks}};
        {ok, #mcp_task{status = completed}} ->
            {reply, {error, {?MCP_ERROR_TASK_ALREADY_COMPLETED, ?MCP_MSG_TASK_ALREADY_COMPLETED, #{}}}, State};
        {ok, #mcp_task{status = cancelled}} ->
            {reply, {error, {?MCP_ERROR_TASK_CANCELLED, ?MCP_MSG_TASK_CANCELLED, #{}}}, State};
        {ok, #mcp_task{status = failed}} ->
            {reply, {error, {?MCP_ERROR_TASK_FAILED, ?MCP_MSG_TASK_FAILED, #{}}}, State};
        error ->
            {reply, {error, {?MCP_ERROR_TASK_NOT_FOUND, ?MCP_MSG_TASK_NOT_FOUND, #{}}}, State}
    end;

handle_call({update_task_progress, TaskId, Progress, Total}, _From, State) ->
    case maps:find(TaskId, State#state.tasks) of
        {ok, Task = #mcp_task{status = Status}} when Status =:= working; Status =:= input_required ->
            Now = erlang:system_time(millisecond),
            UpdatedTask = Task#mcp_task{
                progress = Progress,
                total = Total,
                updated_at = Now
            },
            NewTasks = maps:put(TaskId, UpdatedTask, State#state.tasks),
            {reply, ok, State#state{tasks = NewTasks}};
        {ok, #mcp_task{status = _}} ->
            {reply, {error, {?MCP_ERROR_TASK_STATE_INVALID, ?MCP_MSG_TASK_STATE_INVALID, #{}}}, State};
        error ->
            {reply, {error, {?MCP_ERROR_TASK_NOT_FOUND, ?MCP_MSG_TASK_NOT_FOUND, #{}}}, State}
    end;

handle_call({update_task_status, TaskId, NewStatus}, _From, State) ->
    case maps:find(TaskId, State#state.tasks) of
        {ok, Task} when NewStatus =:= working; NewStatus =:= input_required ->
            case is_valid_status_transition(Task#mcp_task.status, NewStatus) of
                true ->
                    Now = erlang:system_time(millisecond),
                    UpdatedTask = Task#mcp_task{
                        status = NewStatus,
                        updated_at = Now
                    },
                    NewTasks = maps:put(TaskId, UpdatedTask, State#state.tasks),
                    {reply, ok, State#state{tasks = NewTasks}};
                false ->
                    {reply, {error, {?MCP_ERROR_TASK_STATE_INVALID, ?MCP_MSG_TASK_STATE_INVALID, #{}}}, State}
            end;
        {ok, _Task} ->
            {reply, {error, {?MCP_ERROR_TASK_STATE_INVALID, ?MCP_MSG_TASK_STATE_INVALID, #{}}}, State};
        error ->
            {reply, {error, {?MCP_ERROR_TASK_NOT_FOUND, ?MCP_MSG_TASK_NOT_FOUND, #{}}}, State}
    end;

handle_call({complete_task, TaskId, Result}, _From, State) ->
    case maps:find(TaskId, State#state.tasks) of
        {ok, Task = #mcp_task{status = Status}} when Status =:= working; Status =:= input_required ->
            Now = erlang:system_time(millisecond),
            UpdatedTask = Task#mcp_task{
                status = completed,
                result = Result,
                updated_at = Now,
                completed_at = Now,
                expires_at = Now + State#state.default_expiry_ms
            },
            NewTasks = maps:put(TaskId, UpdatedTask, State#state.tasks),
            {reply, ok, State#state{tasks = NewTasks}};
        {ok, #mcp_task{status = completed}} ->
            {reply, {error, {?MCP_ERROR_TASK_ALREADY_COMPLETED, ?MCP_MSG_TASK_ALREADY_COMPLETED, #{}}}, State};
        {ok, #mcp_task{status = _}} ->
            {reply, {error, {?MCP_ERROR_TASK_STATE_INVALID, ?MCP_MSG_TASK_STATE_INVALID, #{}}}, State};
        error ->
            {reply, {error, {?MCP_ERROR_TASK_NOT_FOUND, ?MCP_MSG_TASK_NOT_FOUND, #{}}}, State}
    end;

handle_call({fail_task, TaskId, Error}, _From, State) ->
    case maps:find(TaskId, State#state.tasks) of
        {ok, Task = #mcp_task{status = Status}} when Status =:= working; Status =:= input_required ->
            Now = erlang:system_time(millisecond),
            ErrorRecord = case Error of
                #mcp_error{} -> Error;
                Msg when is_binary(Msg) ->
                    #mcp_error{
                        code = ?MCP_ERROR_TASK_FAILED,
                        message = Msg,
                        data = undefined
                    }
            end,
            UpdatedTask = Task#mcp_task{
                status = failed,
                error = ErrorRecord,
                updated_at = Now,
                completed_at = Now,
                expires_at = Now + State#state.default_expiry_ms
            },
            NewTasks = maps:put(TaskId, UpdatedTask, State#state.tasks),
            {reply, ok, State#state{tasks = NewTasks}};
        {ok, #mcp_task{status = _}} ->
            {reply, {error, {?MCP_ERROR_TASK_STATE_INVALID, ?MCP_MSG_TASK_STATE_INVALID, #{}}}, State};
        error ->
            {reply, {error, {?MCP_ERROR_TASK_NOT_FOUND, ?MCP_MSG_TASK_NOT_FOUND, #{}}}, State}
    end;

handle_call(cleanup_expired_tasks, _From, State) ->
    {Count, NewTasks} = do_cleanup_expired_tasks(State#state.tasks),
    {reply, {ok, Count}, State#state{tasks = NewTasks}};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(cleanup_expired_tasks, State) ->
    {_Count, NewTasks} = do_cleanup_expired_tasks(State#state.tasks),
    % Schedule next cleanup
    TimerRef = erlang:send_after(?CLEANUP_INTERVAL_MS, self(), cleanup_expired_tasks),
    {noreply, State#state{tasks = NewTasks, cleanup_timer = TimerRef}};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, State) ->
    % Cancel cleanup timer
    case State#state.cleanup_timer of
        undefined -> ok;
        Ref -> erlang:cancel_timer(Ref)
    end,
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

-spec generate_task_id() -> task_id().
generate_task_id() ->
    Timestamp = integer_to_binary(erlang:system_time(nanosecond)),
    Random = integer_to_binary(rand:uniform(999999)),
    <<"task_", Timestamp/binary, "_", Random/binary>>.

-spec filter_tasks([#mcp_task{}], map()) -> [#mcp_task{}].
filter_tasks(Tasks, Filter) ->
    lists:filter(fun(Task) ->
        matches_filter(Task, Filter)
    end, Tasks).

-spec matches_filter(#mcp_task{}, map()) -> boolean().
matches_filter(Task, Filter) ->
    StatusMatch = case maps:get(status, Filter, undefined) of
        undefined -> true;
        Status -> Task#mcp_task.status =:= Status
    end,

    NameMatch = case maps:get(name, Filter, undefined) of
        undefined -> true;
        Name -> Task#mcp_task.name =:= Name
    end,

    StatusMatch andalso NameMatch.

-spec is_valid_status_transition(task_status(), task_status()) -> boolean().
is_valid_status_transition(working, input_required) -> true;
is_valid_status_transition(input_required, working) -> true;
is_valid_status_transition(_, _) -> false.

-spec do_cleanup_expired_tasks(#{task_id() => #mcp_task{}}) -> {integer(), #{task_id() => #mcp_task{}}}.
do_cleanup_expired_tasks(Tasks) ->
    Now = erlang:system_time(millisecond),

    {Expired, Remaining} = maps:fold(fun(TaskId, Task, {ExpAcc, RemAcc}) ->
        case Task#mcp_task.expires_at of
            undefined ->
                {ExpAcc, maps:put(TaskId, Task, RemAcc)};
            ExpiryTime when ExpiryTime =< Now ->
                {ExpAcc + 1, RemAcc};
            _ ->
                {ExpAcc, maps:put(TaskId, Task, RemAcc)}
        end
    end, {0, #{}}, Tasks),

    case Expired of
        0 -> ok;
        N -> logger:info("Cleaned up ~p expired tasks", [N])
    end,

    {Expired, Remaining}.
