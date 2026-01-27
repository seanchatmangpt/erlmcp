%%--------------------------------------------------------------------
%% MCP Task Manager - asynchronous task/job queue for MCP tasks API
%%--------------------------------------------------------------------
-module(erlmcp_task_manager).
-behaviour(gen_server).

-compile({no_auto_import, [max/2]}).

-include("erlmcp.hrl").

-export([
    start_link/0,
    stop/0,
    register_server/2,
    unregister_server/1,
    create_tool_task/5,
    list_tasks/1,
    get_task/1,
    cancel_task/1,
    complete_task/2,
    fail_task/2
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(TASK_TABLE, erlmcp_tasks).
-define(DEFAULT_MAX_WORKERS, 5).

-record(state, {
    queue = queue:new() :: queue:queue(task_id()),
    running = 0 :: non_neg_integer(),
    max_workers = ?DEFAULT_MAX_WORKERS :: pos_integer(),
    subscribers = #{} :: #{server_id() => pid()},
    monitors = #{} :: #{pid() => server_id()}
}).

-type task_id() :: binary().

%%%===================================================================
%%% Public API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

-spec register_server(server_id(), pid()) -> ok.
register_server(ServerId, Pid) when is_pid(Pid) ->
    gen_server:call(?MODULE, {register_server, ServerId, Pid}).

-spec unregister_server(server_id()) -> ok.
unregister_server(ServerId) ->
    gen_server:call(?MODULE, {unregister_server, ServerId}).

-spec create_tool_task(server_id(), atom(), json_rpc_id(), binary(), map()) ->
    {ok, map()} | {error, term()}.
create_tool_task(ServerId, TransportId, RequestId, ToolName, Arguments) ->
    Payload = #{
        type => tool_call,
        tool => ToolName,
        arguments => Arguments,
        request_id => RequestId,
        transport_id => TransportId
    },
    gen_server:call(?MODULE, {create_task, ServerId, Payload}).

-spec list_tasks(server_id()) -> [map()].
list_tasks(ServerId) ->
    gen_server:call(?MODULE, {list_tasks, ServerId}).

-spec get_task(task_id()) -> {ok, map()} | {error, term()}.
get_task(TaskId) ->
    gen_server:call(?MODULE, {get_task, TaskId}).

-spec cancel_task(task_id()) -> ok | {error, term()}.
cancel_task(TaskId) ->
    gen_server:call(?MODULE, {cancel_task, TaskId}).

-spec complete_task(task_id(), map()) -> ok.
complete_task(TaskId, Result) ->
    gen_server:cast(?MODULE, {complete_task, TaskId, Result}).

-spec fail_task(task_id(), term()) -> ok.
fail_task(TaskId, Reason) ->
    gen_server:cast(?MODULE, {fail_task, TaskId, Reason}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    case ets:info(?TASK_TABLE) of
        undefined ->
            ets:new(?TASK_TABLE, [named_table, set, public, {read_concurrency, true}]);
        _ -> ok
    end,
    {ok, #state{}}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call({register_server, ServerId, Pid}, _From, #state{subscribers = Subs, monitors = Mons} = State) ->
    {Subs1, Mons1} = case maps:get(ServerId, Subs, undefined) of
        undefined -> {Subs, Mons};
        OldPid ->
            demonitor(OldPid, [flush]),
            {maps:remove(ServerId, Subs), maps:remove(OldPid, Mons)}
    end,
    monitor(process, Pid),
    Mons2 = Mons1#{Pid => ServerId},
    Subs2 = Subs1#{ServerId => Pid},
    {reply, ok, State#state{subscribers = Subs2, monitors = Mons2}};

handle_call({unregister_server, ServerId}, _From, #state{subscribers = Subs, monitors = Mons} = State) ->
    Subs1 = maps:remove(ServerId, Subs),
    Mons1 = remove_monitor(ServerId, Mons),
    {reply, ok, State#state{subscribers = Subs1, monitors = Mons1}};

handle_call({create_task, ServerId, Payload}, _From, State) ->
    case validate_payload(Payload) of
        {error, Reason} -> {reply, {error, Reason}, State};
        ok ->
            Task = new_task(ServerId, Payload),
            store_task(Task),
            State1 = enqueue_task(Task#{status := queued}, State),
            notify(Task, State1),
            {reply, {ok, Task}, maybe_dispatch(State1)}
    end;

handle_call({list_tasks, ServerId}, _From, State) ->
    Tasks = ets:foldl(fun({_, Task}, Acc) ->
        case maps:get(server_id, Task) of
            ServerId -> [Task | Acc];
            _ -> Acc
        end
    end, [], ?TASK_TABLE),
    {reply, lists:reverse(Tasks), State};

handle_call({get_task, TaskId}, _From, State) ->
    Reply = case lookup_task(TaskId) of
        {ok, Task} -> {ok, Task};
        error -> {error, not_found}
    end,
    {reply, Reply, State};

handle_call({cancel_task, TaskId}, _From, State) ->
    Reply = case lookup_task(TaskId) of
        {ok, Task} ->
            case maps:get(status, Task) of
                queued ->
                    Task1 = Task#{status => cancelled, updated_at => now_ms()},
                    store_task(Task1),
                    notify(Task1, State),
                    ok;
                running ->
                    Task1 = Task#{status => cancel_requested, updated_at => now_ms()},
                    store_task(Task1),
                    notify(Task1, State),
                    send_cancel_request(Task1, State),
                    ok;
                _ -> ok
            end;
        error -> {error, not_found}
    end,
    {reply, Reply, State}.

handle_cast({complete_task, TaskId, Result}, State) ->
    State1 = finalize_task(TaskId, completed, Result, State),
    {noreply, State1};

handle_cast({fail_task, TaskId, Reason}, State) ->
    State1 = finalize_task(TaskId, failed, Reason, State),
    {noreply, State1};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, _Reason}, #state{subscribers = Subs, monitors = Mons} = State) ->
    case maps:find(Pid, Mons) of
        {ok, ServerId} ->
            {noreply, State#state{subscribers = maps:remove(ServerId, Subs), monitors = maps:remove(Pid, Mons)}};
        error -> {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal helpers
%%%===================================================================

validate_payload(#{type := tool_call, tool := Tool, arguments := Args}) when is_binary(Tool), is_map(Args) -> ok;
validate_payload(_) -> {error, invalid_task_payload}.

new_task(ServerId, Payload) ->
    TaskId = list_to_binary(integer_to_list(erlang:unique_integer([positive, monotonic]))),
    Timestamp = now_ms(),
    Payload#{
        id => TaskId,
        server_id => ServerId,
        status => queued,
        created_at => Timestamp,
        updated_at => Timestamp,
        result => undefined
    }.

store_task(Task) ->
    ets:insert(?TASK_TABLE, {maps:get(id, Task), Task}).

lookup_task(TaskId) ->
    case ets:lookup(?TASK_TABLE, TaskId) of
        [{_, Task}] -> {ok, Task};
        [] -> error
    end.

enqueue_task(Task, #state{queue = Queue} = State) ->
    State#state{queue = queue:in(maps:get(id, Task), Queue)}.

maybe_dispatch(State) ->
    dispatch(State).

dispatch(State = #state{running = Running, max_workers = Max}) when Running >= Max ->
    State;
dispatch(State = #state{queue = Queue}) ->
    case queue:out(Queue) of
        {empty, _} -> State;
        {{value, TaskId}, Queue1} ->
            case lookup_task(TaskId) of
                {ok, Task} ->
                    case maps:get(status, Task) of
                        queued ->
                            send_task_to_server(Task, State),
                            Task1 = Task#{status => running, started_at => now_ms(), updated_at => now_ms()},
                            store_task(Task1),
                            notify(Task1, State),
                            dispatch(State#state{queue = Queue1, running = State#state.running + 1});
                        cancelled ->
                            dispatch(State#state{queue = Queue1});
                        _ ->
                            dispatch(State#state{queue = Queue1})
                    end;
                error ->
                    dispatch(State#state{queue = Queue1})
            end
    end.

finalize_task(TaskId, Status, Value, State) ->
    case lookup_task(TaskId) of
        {ok, Task} ->
            Task1 = Task#{
                status => Status,
                updated_at => now_ms(),
                completed_at => now_ms(),
                result => Value
            },
            store_task(Task1),
            notify(Task1, State),
            dispatch(State#state{running = max(State#state.running - 1, 0)});
        error ->
            State
    end.

send_task_to_server(Task, #state{subscribers = Subs}) ->
    ServerId = maps:get(server_id, Task),
    case maps:get(ServerId, Subs, undefined) of
        undefined -> ok;
        Pid -> Pid ! {task_execute, Task}
    end.

notify(Task, State) ->
    ServerId = maps:get(server_id, Task),
    case maps:get(ServerId, State#state.subscribers, undefined) of
        undefined -> ok;
        Pid -> Pid ! {task_status_update, Task}
    end.

send_cancel_request(Task, State) ->
    ServerId = maps:get(server_id, Task),
    case maps:get(ServerId, State#state.subscribers, undefined) of
        undefined -> ok;
        Pid -> Pid ! {task_cancel, Task}
    end.

remove_monitor(ServerId, Mons) ->
    Filtered = maps:filter(fun(Pid, SId) ->
        case SId =:= ServerId of
            true -> demonitor(Pid, [flush]), false;
            false -> true
        end
    end, Mons),
    Filtered.

now_ms() -> erlang:system_time(millisecond).

max(A, B) when A >= B -> A;
max(_, B) -> B.
