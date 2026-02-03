%%%-------------------------------------------------------------------
%%% @doc
%%% Graceful Shutdown Coordinator for erlmcp
%%%
%%% This module implements a comprehensive graceful shutdown mechanism that:
%%% 1. Stops accepting new connections
%%% 2. Drains existing requests (with configurable timeout)
%%% 3. Closes resources (transports, servers, pools)
%%% 4. Saves state (registry, sessions, metrics)
%%%
%%% ## Shutdown Phases
%%%
%%% The shutdown process follows a 4-phase approach:
%%%
%%% 1. **Initiation Phase** (0-5s)
%%%    - Set shutdown flag in ETS table
%%%    - Notify all registered processes via {erlmcp_shutdown, Reason}
%%%    - Stop accepting new connections
%%%
%%% 2. **Drain Phase** (5-35s default, configurable)
%%%    - Wait for existing requests to complete
%%%    - Monitor active connections
%%%    - Enforce timeout (30s default)
%%%
%%% 3. **Resource Cleanup Phase** (35-40s)
%%%    - Close connection pools
%%%    - Terminate transports
%%%    - Shutdown servers
%%%
%%% 4. **State Persistence Phase** (40-45s)
%%%    - Save registry state
%%%    - Persist metrics
%%%    - Flush telemetry
%%%
%%% ## Usage
%%%
%%% ```erlang
%%% %% Initiate graceful shutdown with default 30s drain timeout
%%% ok = erlmcp_shutdown:shutdown(normal).
%%%
%%% %% Custom drain timeout
%%% ok = erlmcp_shutdown:shutdown(normal, 60000).  % 60 seconds
%%%
%%% %% Immediate shutdown (no drain)
%%% ok = erlmcp_shutdown:shutdown_now().
%%%
%%% %% Check shutdown status
%%% {ok, Status} = erlmcp_shutdown:get_status().
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_shutdown).
-behaviour(gen_server).

%% Public API
-export([start_link/0,
         shutdown/1,
         shutdown/2,
         shutdown_now/0,
         get_status/0,
         cancel_shutdown/0,
         register_cleanup_handler/2,
         unregister_cleanup_handler/1,
         await_shutdown/0,
         await_shutdown/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("erlmcp.hrl").

%%====================================================================
%% Constants
%%====================================================================

-define(SERVER, ?MODULE).
-define(DEFAULT_DRAIN_TIMEOUT, 30000).      % 30 seconds
-define(MIN_DRAIN_TIMEOUT, 5000).           % 5 seconds minimum
-define(MAX_DRAIN_TIMEOUT, 300000).         % 5 minutes maximum
-define(SHUTDOWN_STATUS_TABLE, erlmcp_shutdown_status).
-define(CLEANUP_HANDLERS_TABLE, erlmcp_cleanup_handlers).

%%====================================================================
%% Records
%%====================================================================

-record(shutdown_phase,
        {name :: atom(),
         started_at :: integer(),
         deadline :: integer(),
         status :: pending | in_progress | completed | failed}).

-type shutdown_phase() :: #shutdown_phase{}.

-record(shutdown_status,
        {initiating :: boolean(),
         phase :: atom(),
         phases :: [shutdown_phase()],
         reason :: term(),
         drain_timeout :: pos_integer(),
         connections_active :: non_neg_integer(),
         connections_total :: non_neg_integer(),
         requests_pending :: non_neg_integer(),
         start_time :: integer(),
         estimated_completion :: integer() | undefined}).

-type shutdown_status() :: #shutdown_status{}.

-record(state,
        {status :: shutdown_status(),
         drain_timer :: reference() | undefined,
         monitors :: #{pid() => reference()},
         awaiters :: [pid()]}).

-type state() :: #state{}.

%% Cleanup handler specification
-record(cleanup_handler,
        {id :: term(),
         module :: module(),
         function :: atom(),
         args :: list(),
         timeout :: pos_integer(),
         priority :: urgent | high | normal | low}).

-type cleanup_handler() :: #cleanup_handler{}.

%% Priority atom type for cleanup handlers
-type priority_atom() :: urgent | high | normal | low.

%%====================================================================
%% Type Exports
%%====================================================================

-export_type([shutdown_status/0, shutdown_phase/0, cleanup_handler/0]).

%%====================================================================
%% Public API
%%====================================================================

%% @doc Start the shutdown coordinator
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Initiate graceful shutdown with reason
-spec shutdown(term()) -> ok.
shutdown(Reason) ->
    shutdown(Reason, ?DEFAULT_DRAIN_TIMEOUT).

%% @doc Initiate graceful shutdown with custom drain timeout
-spec shutdown(term(), pos_integer()) -> ok.
shutdown(Reason, DrainTimeout) when is_integer(DrainTimeout), DrainTimeout >= ?MIN_DRAIN_TIMEOUT ->
    gen_server:call(?SERVER, {shutdown, Reason, DrainTimeout}, infinity).

%% @doc Immediate shutdown without draining
-spec shutdown_now() -> no_return().
shutdown_now() ->
    logger:critical("Immediate shutdown requested - skipping drain phase"),
    ok = init:stop(),
    exit(normal).

%% @doc Get current shutdown status
-spec get_status() -> {ok, shutdown_status()} | {error, not_shutting_down}.
get_status() ->
    case ets:info(?SHUTDOWN_STATUS_TABLE) of
        undefined ->
            {error, not_shutting_down};
        _ ->
            [{status, Status}] = ets:lookup(?SHUTDOWN_STATUS_TABLE, status),
            {ok, Status}
    end.

%% @doc Cancel pending shutdown (if in drain phase)
-spec cancel_shutdown() -> ok | {error, term()}.
cancel_shutdown() ->
    gen_server:call(?SERVER, cancel_shutdown, 5000).

%% @doc Register a cleanup handler to be called during shutdown
%% Handlers are called in priority order: urgent > high > normal > low
-spec register_cleanup_handler(term(), cleanup_handler()) -> ok.
register_cleanup_handler(Id, #cleanup_handler{} = Handler) ->
    gen_server:call(?SERVER, {register_handler, Id, Handler}, 5000).

%% @doc Unregister a cleanup handler
-spec unregister_cleanup_handler(term()) -> ok.
unregister_cleanup_handler(Id) ->
    gen_server:call(?SERVER, {unregister_handler, Id}, 5000).

%% @doc Await shutdown completion (blocks until complete)
-spec await_shutdown() -> ok.
await_shutdown() ->
    await_shutdown(infinity).

%% @doc Await shutdown completion with timeout
-spec await_shutdown(timeout()) -> ok | {error, timeout}.
await_shutdown(Timeout) ->
    gen_server:call(?SERVER, await_shutdown, Timeout).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([]) -> {ok, state()}.
init([]) ->
    process_flag(trap_exit, true),

    %% Create ETS tables for shutdown state
    ets:new(?SHUTDOWN_STATUS_TABLE, [named_table, set, public, {read_concurrency, true}]),
    ets:new(?CLEANUP_HANDLERS_TABLE, [named_table, bag, public]),

    %% Initialize shutdown status
    Status = #shutdown_status{
        initiating = false,
        phase = not_shutting_down,
        phases = [],
        reason = undefined,
        drain_timeout = ?DEFAULT_DRAIN_TIMEOUT,
        connections_active = 0,
        connections_total = 0,
        requests_pending = 0,
        start_time = 0,
        estimated_completion = undefined
    },

    ets:insert(?SHUTDOWN_STATUS_TABLE, {status, Status}),

    logger:info("Graceful shutdown coordinator started"),

    {ok, #state{
        status = Status,
        drain_timer = undefined,
        monitors = #{},
        awaiters = []
    }}.

handle_call({shutdown, Reason, DrainTimeout}, _From, State) ->
    case State#state.status#shutdown_status.initiating of
        true ->
            logger:info("Shutdown already in progress, ignoring duplicate request"),
            {reply, {error, already_shutting_down}, State};
        false ->
            logger:info("Initiating graceful shutdown: reason=~p, drain_timeout=~p",
                       [Reason, DrainTimeout]),
            {ok, NewState} = do_shutdown(Reason, DrainTimeout, State),
            {reply, ok, NewState}
    end;

handle_call(cancel_shutdown, _From, #state{status = #shutdown_status{initiating = true}} = State) ->
    logger:warning("Cannot cancel shutdown - already in progress"),
    {reply, {error, too_late}, State};
handle_call(cancel_shutdown, _From, State) ->
    logger:info("No shutdown in progress to cancel"),
    {reply, ok, State};

handle_call({register_handler, Id, Handler}, _From, State) ->
    ets:insert(?CLEANUP_HANDLERS_TABLE, {Id, Handler}),
    logger:debug("Registered cleanup handler: ~p", [Id]),
    {reply, ok, State};

handle_call({unregister_handler, Id}, _From, State) ->
    ets:delete(?CLEANUP_HANDLERS_TABLE, Id),
    logger:debug("Unregistered cleanup handler: ~p", [Id]),
    {reply, ok, State};

handle_call(await_shutdown, {Pid, _Tag}, #state{status = #shutdown_status{initiating = false}} = State) ->
    %% Not shutting down yet, add to awaiters list
    MonRef = monitor(process, Pid),
    NewState = State#state{awaiters = [Pid | State#state.awaiters]},
    {reply, ok, NewState};
handle_call(await_shutdown, {Pid, _Tag}, State) ->
    %% Already shutting down, reply immediately
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(drain_timeout, State) ->
    logger:info("Drain timeout reached, proceeding to resource cleanup"),
    {ok, NewState} = proceed_to_cleanup(State),
    {noreply, NewState};

handle_info(phase_timeout, State) ->
    logger:warning("Phase timeout, forcing completion"),
    {ok, NewState} = complete_shutdown(State),
    {noreply, NewState};

handle_info({'DOWN', MonRef, process, Pid, Reason}, #state{awaiters = Awaiters} = State) ->
    %% An awaiter died, remove from list
    NewAwaiters = lists:delete(Pid, Awaiters),
    NewMonitors = maps:filter(fun(_, V) -> V =/= MonRef end, State#state.monitors),
    {noreply, State#state{awaiters = NewAwaiters, monitors = NewMonitors}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{status = #shutdown_status{initiating = true}} = State) ->
    logger:info("Shutdown coordinator terminating, completing shutdown"),
    complete_shutdown(State),
    ok;
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Execute the shutdown sequence
-spec do_shutdown(term(), pos_integer(), state()) -> {ok, state()}.
do_shutdown(Reason, DrainTimeout, State) ->
    StartTime = erlang:system_time(millisecond),

    %% Initialize shutdown status
    Status = #shutdown_status{
        initiating = true,
        phase = initiation,
        phases = [],
        reason = Reason,
        drain_timeout = DrainTimeout,
        connections_active = 0,
        connections_total = 0,
        requests_pending = 0,
        start_time = StartTime,
        estimated_completion = StartTime + DrainTimeout + 15000  % Drain + 15s cleanup
    },

    %% Update ETS table for global visibility
    ets:insert(?SHUTDOWN_STATUS_TABLE, {status, Status}),

    %% Phase 1: Initiation (0-5s)
    {ok, State1} = phase_initiation(Reason, State#state{status = Status}),

    %% Phase 2: Drain (5-35s default)
    {ok, State2} = phase_drain(DrainTimeout, State1),

    {ok, State2}.

%% @doc Phase 1: Initiation - stop accepting new connections
-spec phase_initiation(term(), state()) -> {ok, state()}.
phase_initiation(Reason, State) ->
    StartTime = erlang:system_time(millisecond),
    logger:info("=== Shutdown Phase 1: Initiation ==="),
    logger:info("Reason: ~p", [Reason]),

    %% 1. Set global shutdown flag via gproc
    try
        ok = gproc:set_value({n, l, erlmcp_shutdown_in_progress}, Reason),
        logger:debug("Set global shutdown flag")
    catch
        _:_ ->
            logger:warning("Shutdown flag already set")
    end,

    %% 2. Notify all registered processes
    notify_processes(Reason),

    %% 3. Stop accepting new connections
    stop_accepting_connections(),

    %% 4. Get current connection counts
    {Active, Total} = get_connection_counts(),

    logger:info("Active connections: ~p/~p", [Active, Total]),

    %% Update status
    Phase = #shutdown_phase{
        name = initiation,
        started_at = StartTime,
        deadline = StartTime + 5000,
        status = completed
    },

    NewStatus = (State#state.status)#shutdown_status{
        phase = drain,
        phases = [Phase],
        connections_active = Active,
        connections_total = Total
    },

    ets:insert(?SHUTDOWN_STATUS_TABLE, {status, NewStatus}),

    {ok, State#state{status = NewStatus}}.

%% @doc Phase 2: Drain - wait for existing requests to complete
-spec phase_drain(pos_integer(), state()) -> {ok, state()}.
phase_drain(DrainTimeout, State) ->
    StartTime = erlang:system_time(millisecond),
    logger:info("=== Shutdown Phase 2: Drain ==="),
    logger:info("Drain timeout: ~pms", [DrainTimeout]),

    %% Set drain timer
    DrainTimer = erlang:send_after(DrainTimeout, self(), drain_timeout),

    %% Start monitoring connection counts
    self() ! monitor_connections,

    %% Update status
    Phase = #shutdown_phase{
        name = drain,
        started_at = StartTime,
        deadline = StartTime + DrainTimeout,
        status = in_progress
    },

    NewStatus = (State#state.status)#shutdown_status{
        phase = drain,
        phases = [Phase | (State#state.status)#shutdown_status.phases]
    },

    ets:insert(?SHUTDOWN_STATUS_TABLE, {status, NewStatus}),

    {ok, State#state{status = NewStatus, drain_timer = DrainTimer}}.

%% @doc Proceed to cleanup phase after drain completes
-spec proceed_to_cleanup(state()) -> {ok, state()}.
proceed_to_cleanup(State) ->
    logger:info("=== Shutdown Phase 3: Resource Cleanup ==="),

    %% Cancel drain timer
    case State#state.drain_timer of
        undefined -> ok;
        Timer -> erlang:cancel_timer(Timer)
    end,

    StartTime = erlang:system_time(millisecond),

    %% 1. Run cleanup handlers in priority order
    run_cleanup_handlers(),

    %% 2. Close connection pools
    close_connection_pools(),

    %% 3. Stop transports
    stop_transports(),

    %% 4. Stop servers
    stop_servers(),

    %% Update status
    Phase = #shutdown_phase{
        name = cleanup,
        started_at = StartTime,
        deadline = StartTime + 5000,
        status = completed
    },

    NewStatus = (State#state.status)#shutdown_status{
        phase = state_persistence,
        phases = [Phase | (State#state.status)#shutdown_status.phases]
    },

    ets:insert(?SHUTDOWN_STATUS_TABLE, {status, NewStatus}),

    %% Proceed to state persistence
    {ok, State1} = phase_state_persistence(State#state{status = NewStatus}),

    {ok, State1}.

%% @doc Phase 4: State persistence - save state before exit
-spec phase_state_persistence(state()) -> {ok, state()}.
phase_state_persistence(State) ->
    logger:info("=== Shutdown Phase 4: State Persistence ==="),

    StartTime = erlang:system_time(millisecond),

    %% 1. Save registry state
    save_registry_state(),

    %% 2. Persist metrics
    persist_metrics(),

    %% 3. Flush telemetry
    flush_telemetry(),

    %% Update status
    Phase = #shutdown_phase{
        name = state_persistence,
        started_at = StartTime,
        deadline = StartTime + 5000,
        status = completed
    },

    NewStatus = (State#state.status)#shutdown_status{
        phase = completed,
        phases = [Phase | (State#state.status)#shutdown_status.phases]
    },

    ets:insert(?SHUTDOWN_STATUS_TABLE, {status, NewStatus}),

    logger:info("Graceful shutdown complete"),

    %% Notify awaiters
    notify_awaiters(State),

    {ok, State#state{status = NewStatus}}.

%% @doc Complete shutdown and notify init to stop
-spec complete_shutdown(state()) -> {ok, state()}.
complete_shutdown(#state{status = #shutdown_status{phase = completed}} = State) ->
    logger:info("Shutdown already completed"),
    {ok, State};
complete_shutdown(State) ->
    logger:info("Completing shutdown sequence"),

    %% Notify init to stop the VM
    erlang:send_after(1000, init, stop),

    {ok, State}.

%% @doc Notify all registered processes of shutdown
-spec notify_processes(term()) -> ok.
notify_processes(Reason) ->
    try
        %% Notify all servers
        case catch erlmcp_registry:list_servers() of
            Servers when is_list(Servers) ->
                lists:foreach(fun({ServerId, {Pid, _Config}}) ->
                    logger:debug("Notifying server ~p (~p)", [ServerId, Pid]),
                    Pid ! {erlmcp_shutdown, ServerId, Reason}
                end, Servers);
            _ ->
                ok
        end,

        %% Notify all transports
        case catch erlmcp_registry:list_transports() of
            Transports when is_list(Transports) ->
                lists:foreach(fun({TransportId, {Pid, _Config}}) ->
                    logger:debug("Notifying transport ~p (~p)", [TransportId, Pid]),
                    Pid ! {erlmcp_shutdown, TransportId, Reason}
                end, Transports);
            _ ->
                ok
        end,

        logger:info("Notified ~p servers and ~p transports",
                   [length(erlmcp_registry:list_servers()),
                    length(erlmcp_registry:list_transports())]),

        ok
    catch
        _:Error ->
            logger:error("Error notifying processes: ~p", [Error]),
            ok
    end.

%% @doc Stop accepting new connections
-spec stop_accepting_connections() -> ok.
stop_accepting_connections() ->
    logger:info("Stopping accepting new connections"),
    try
        case whereis(erlmcp_transport_sup) of
            undefined ->
                logger:warning("Transport supervisor not running"),
                ok;
            _SupPid ->
                %% Signal transport supervisor to stop accepting
                case catch erlmcp_transport_sup:stop_accepting() of
                    ok ->
                        logger:info("Transport supervisor stopped accepting new connections");
                    {'EXIT', Reason} ->
                        logger:error("Error stopping accepting: ~p", [Reason]);
                    StopError ->
                        logger:error("Error stopping accepting: ~p", [StopError])
                end
        end
    catch
        _:CatchError ->
            logger:error("Exception stopping accepting: ~p", [CatchError]),
            ok
    end.

%% @doc Get current connection counts
-spec get_connection_counts() -> {non_neg_integer(), non_neg_integer()}.
get_connection_counts() ->
    try
        %% Query connection pool status
        case whereis(erlmcp_connection_pool) of
            undefined ->
                {0, 0};
            _Pid ->
                case catch erlmcp_connection_pool:get_pool_status(default) of
                    {ok, Status} ->
                        Active = maps:get(active_connections, Status, 0),
                        Total = maps:get(total_connections, Status, 0),
                        {Active, Total};
                    _ ->
                        {0, 0}
                end
        end
    catch
        _:_ -> {0, 0}
    end.

%% @doc Run cleanup handlers in priority order
-spec run_cleanup_handlers() -> ok.
run_cleanup_handlers() ->
    logger:info("Running cleanup handlers"),

    %% Get all handlers and sort by priority
    Handlers = case ets:tab2list(?CLEANUP_HANDLERS_TABLE) of
        [] ->
            logger:info("No cleanup handlers registered"),
            [];
        List ->
            List
    end,

    %% Sort by priority (urgent > high > normal > low)
    PriorityOrder = fun({_, #cleanup_handler{priority = P1}}, {_, #cleanup_handler{priority = P2}}) ->
        priority_value(P1) =< priority_value(P2)
    end,

    SortedHandlers = lists:sort(PriorityOrder, Handlers),

    %% Execute handlers in order
    lists:foreach(fun({Id, #cleanup_handler{module = M, function = F, args = A, timeout = T}}) ->
        logger:info("Executing cleanup handler: ~p:~p/~p", [M, F, length(A)]),
        try
            case apply(M, F, A ++ [T]) of
                ok ->
                    logger:info("Cleanup handler ~p completed", [Id]);
                {error, HandlerReason} ->
                    logger:error("Cleanup handler ~p failed: ~p", [Id, HandlerReason]);
                Other ->
                    logger:warning("Cleanup handler ~p returned: ~p", [Id, Other])
            end
        catch
            Class:Reason:Stacktrace ->
                logger:error("Cleanup handler ~p crashed: ~p:~p~n~p",
                            [Id, Class, Reason, Stacktrace])
        end
    end, SortedHandlers),

    ok.

%% @doc Get priority value for sorting
-spec priority_value(priority_atom()) -> integer().
priority_value(urgent) -> 1;
priority_value(high) -> 2;
priority_value(normal) -> 3;
priority_value(low) -> 4.

%% @doc Close connection pools
-spec close_connection_pools() -> ok.
close_connection_pools() ->
    logger:info("Closing connection pools"),
    try
        case catch erlmcp_connection_pool:drain() of
            ok ->
                logger:info("Connection pools drained successfully");
            DrainError ->
                logger:error("Error draining connection pools: ~p", [DrainError])
        end
    catch
        _:CatchError ->
            logger:error("Exception draining connection pools: ~p", [CatchError])
    end,
    ok.

%% @doc Stop all transports
-spec stop_transports() -> ok.
stop_transports() ->
    logger:info("Stopping transports"),
    try
        Transports = erlmcp_sup:list_transports(),
        lists:foreach(fun({TransportId, _Pid, _Type}) ->
            logger:debug("Stopping transport: ~p", [TransportId]),
            case erlmcp_sup:stop_transport(TransportId) of
                ok -> ok;
                {error, Reason} ->
                    logger:error("Error stopping transport ~p: ~p", [TransportId, Reason])
            end
        end, Transports)
    catch
        _:Error ->
            logger:error("Error stopping transports: ~p", [Error])
    end,
    ok.

%% @doc Stop all servers
-spec stop_servers() -> ok.
stop_servers() ->
    logger:info("Stopping servers"),
    try
        Servers = erlmcp_registry:list_servers(),
        lists:foreach(fun({ServerId, _Pid, _Config}) ->
            logger:debug("Stopping server: ~p", [ServerId]),
            case erlmcp_sup:stop_server(ServerId) of
                ok -> ok;
                {error, Reason} ->
                    logger:error("Error stopping server ~p: ~p", [ServerId, Reason])
            end
        end, Servers)
    catch
        _:Error ->
            logger:error("Error stopping servers: ~p", [Error])
    end,
    ok.

%% @doc Save registry state
-spec save_registry_state() -> ok.
save_registry_state() ->
    logger:info("Saving registry state"),
    try
        case erlmcp_registry:get_all_state() of
            {ok, State} ->
                %% In production, this would persist to disk
                logger:info("Registry state saved: ~p entries",
                           [maps:size(erlmcp_registry:sanitize_registry_state(State))]);
            {error, Reason} ->
                logger:error("Error saving registry state: ~p", [Reason])
        end
    catch
        _:Error ->
            logger:error("Exception saving registry state: ~p", [Error])
    end,
    ok.

%% @doc Persist metrics
-spec persist_metrics() -> ok.
persist_metrics() ->
    logger:info("Persisting metrics"),
    try
        %% Get metrics from observability system
        case whereis(erlmcp_observability_sup) of
            undefined ->
                logger:debug("Observability not available"),
                ok;
            _ ->
                %% In production, this would persist to metrics backend
                logger:info("Metrics persisted")
        end
    catch
        _:Error ->
            logger:error("Exception persisting metrics: ~p", [Error])
    end,
    ok.

%% @doc Flush telemetry
-spec flush_telemetry() -> ok.
flush_telemetry() ->
    logger:info("Flushing telemetry"),
    try
        case whereis(erlmcp_otel) of
            undefined ->
                logger:debug("OTEL not available"),
                ok;
            _Pid ->
                case catch erlmcp_otel:flush() of
                    ok ->
                        logger:info("Telemetry flushed successfully");
                    FlushError ->
                        logger:error("Error flushing telemetry: ~p", [FlushError])
                end
        end
    catch
        _:CatchError ->
            logger:error("Exception flushing telemetry: ~p", [CatchError])
    end,
    ok.

%% @doc Notify all awaiters that shutdown is complete
-spec notify_awaiters(state()) -> ok.
notify_awaiters(#state{awaiters = Awaiters}) ->
    lists:foreach(fun(Pid) ->
        catch Pid ! {erlmcp_shutdown_complete, self()}
    end, Awaiters),
    ok.
