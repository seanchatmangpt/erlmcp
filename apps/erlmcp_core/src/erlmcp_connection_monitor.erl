%%%-------------------------------------------------------------------
%%% @doc Connection Monitor - State Machine for Connection Tracking
%%%
%%% This module implements comprehensive connection tracking using gen_statem
%%% to model the monitor's operational states. States represent the monitor
%%% service health:
%%%
%%% - disconnected: Initial state, monitor not active
%%% - connecting: Setting up ETS tables, timers, configuration
%%% - connected: Normal operation, actively monitoring connections
%%% - reconnecting: Degraded mode, recovering from issues (leak detection)
%%%
%%% Features:
%%% - ETS-based tracking of all active connections
%%% - Process monitoring with automatic cleanup on termination
%%% - Leak detection (>100 connections/min growth)
%%% - Per-server connection tracking
%%% - Connection lifecycle metrics
%%% - Automatic cleanup of orphaned connections
%%% - Exponential backoff for recovery
%%%
%%% Configuration via sys.config:
%%% ```erlang
%%% {erlmcp, [
%%%     {connection_monitoring, #{
%%%         enabled => true,
%%%         leak_threshold => 100,  % connections per minute
%%%         cleanup_interval => 60000,  % 1 minute
%%%         max_reconnect_attempts => 5
%%%     }}
%%% ]}
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_connection_monitor).

-behaviour(gen_statem).

%% API
-export([start_link/0, stop/0, monitor_connection/2, unmonitor_connection/1, get_connection_count/0,
         get_connection_stats/0, force_cleanup/0, is_leak_detected/0, get_state/0]).
%% gen_statem callbacks
-export([init/1, callback_mode/0, terminate/3, code_change/4]).
%% State functions
-export([disconnected/3, connecting/3, connected/3, reconnecting/3]).

-include_lib("kernel/include/logger.hrl").

%% Constants
-define(ETS_TABLE, erlmcp_connections).
-define(DEFAULT_LEAK_THRESHOLD, 100).  % connections per minute
-define(DEFAULT_CLEANUP_INTERVAL, 60000).  % 1 minute
-define(LEAK_WINDOW_MS, 60000).  % 1 minute window
-define(MAX_RECONNECT_ATTEMPTS, 5).
-define(INITIAL_BACKOFF_MS, 1000).
-define(MAX_BACKOFF_MS, 30000).
%% Hibernation configuration for idle monitor
%% Reduces memory per idle monitor from ~50KB to ~5KB
-define(HIBERNATE_AFTER_MS, 30000). % 30 seconds of inactivity triggers hibernation

%% Types
-type connection_id() :: pid() | {pid(), reference()}.
-type connection_info() ::
    #{pid => pid(),
      socket => port() | undefined,
      monitor_ref => reference(),
      server_id => atom() | binary(),
      transport_id => atom() | binary(),
      created_at => integer(),
      last_activity => integer(),
      bytes_sent => non_neg_integer(),
      bytes_received => non_neg_integer()}.
-type connection_stats() ::
    #{total_connections => non_neg_integer(),
      active_connections => non_neg_integer(),
      orphaned_connections => non_neg_integer(),
      cleanup_attempts => non_neg_integer(),
      last_cleanup_time => integer() | undefined}.

%% State data
-record(data,
        {leak_threshold :: pos_integer(),
         cleanup_interval :: pos_integer(),
         cleanup_timer :: reference() | undefined,
         leak_check_timer :: reference() | undefined,
         previous_count = 0 :: non_neg_integer(),
         last_check_time :: integer() | undefined,
         leak_detected = false :: boolean(),
         cleanup_attempts = 0 :: non_neg_integer(),
         reconnect_attempts = 0 :: non_neg_integer(),
         stats = #{} :: map()}).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the connection monitor
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    %% Enable hibernation after 30 seconds of inactivity to reduce memory usage
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], [{hibernate_after, ?HIBERNATE_AFTER_MS}]).

%% @doc Stop the connection monitor
-spec stop() -> ok.
stop() ->
    gen_statem:stop(?MODULE).

%% @doc Monitor a connection
%% Registers the connection and sets up process monitoring
-spec monitor_connection(pid(), connection_info()) -> ok.
monitor_connection(ConnectionPid, Info) when is_pid(ConnectionPid), is_map(Info) ->
    gen_statem:call(?MODULE, {monitor_connection, ConnectionPid, Info}, 1000).

%% @doc Unmonitor a connection
%% Removes the connection from tracking
-spec unmonitor_connection(connection_id()) -> ok.
unmonitor_connection(ConnectionId) ->
    gen_statem:call(?MODULE, {unmonitor_connection, ConnectionId}, 1000).

%% @doc Get current connection count
-spec get_connection_count() -> non_neg_integer().
get_connection_count() ->
    case ets:info(?ETS_TABLE, size) of
        undefined ->
            0;
        Size ->
            Size
    end.

%% @doc Get connection statistics
-spec get_connection_stats() -> connection_stats().
get_connection_stats() ->
    gen_statem:call(?MODULE, get_connection_stats, 1000).

%% @doc Force cleanup of orphaned connections
-spec force_cleanup() -> {ok, non_neg_integer()}.
force_cleanup() ->
    gen_statem:call(?MODULE, force_cleanup, 1000).

%% @doc Check if a leak has been detected
-spec is_leak_detected() -> boolean().
is_leak_detected() ->
    gen_statem:call(?MODULE, is_leak_detected, 1000).

%% @doc Get current state of the monitor
-spec get_state() -> atom().
get_state() ->
    gen_statem:call(?MODULE, get_state, 1000).

%%====================================================================
%% gen_statem Callbacks
%%====================================================================

-spec init([]) -> {ok, disconnected, #data{}}.
init([]) ->
    logger:info("Connection monitor initializing"),

    %% Load configuration
    LeakThreshold = load_config(leak_threshold, ?DEFAULT_LEAK_THRESHOLD),
    CleanupInterval = load_config(cleanup_interval, ?DEFAULT_CLEANUP_INTERVAL),

    Data =
        #data{leak_threshold = LeakThreshold,
              cleanup_interval = CleanupInterval,
              last_check_time = erlang:monotonic_time(millisecond)},

    %% Trigger transition to connecting state
    {ok, disconnected, Data, [{next_event, internal, setup}]}.

callback_mode() ->
    [state_functions, state_enter].

%%====================================================================
%% State Functions
%%====================================================================

%% @doc Disconnected state - Monitor not active
disconnected(enter, _OldState, Data) ->
    logger:info("Connection monitor entering disconnected state"),
    {keep_state,
     Data#data{cleanup_timer = undefined,
               leak_check_timer = undefined,
               reconnect_attempts = 0}};
disconnected(internal, setup, Data) ->
    %% Trigger setup and transition to connecting
    {next_state, connecting, Data};
disconnected({call, From}, get_state, _Data) ->
    {keep_state_and_data, [{reply, From, disconnected}]};
disconnected({call, From}, _Request, _Data) ->
    {keep_state_and_data, [{reply, From, {error, not_connected}}]};
disconnected(_EventType, _EventContent, _Data) ->
    keep_state_and_data.

%% @doc Connecting state - Setting up monitor infrastructure
connecting(enter, _OldState, _Data) ->
    logger:info("Connection monitor entering connecting state"),
    %% Trigger async setup
    gen_statem:cast(?MODULE, do_setup),
    keep_state_and_data;
connecting(cast, do_setup, Data) ->
    %% Create ETS table for connection tracking
    SetupResult =
        try
            ets:new(?ETS_TABLE,
                    [named_table,
                     set,
                     public,
                     {keypos, 1},
                     {read_concurrency, true},
                     {write_concurrency, true}]),
            logger:info("Connection monitor ETS table created: ~p", [?ETS_TABLE]),
            ok
        catch
            error:badarg ->
                %% Table already exists
                logger:info("Connection monitor ETS table already exists: ~p", [?ETS_TABLE]),
                ok
        end,

    case SetupResult of
        ok ->
            %% Setup successful, transition to connected
            {next_state, connected, Data};
        {error, Reason} ->
            logger:error("Connection monitor setup failed: ~p", [Reason]),
            %% Setup failed, transition to reconnecting with backoff
            {next_state, reconnecting, Data#data{reconnect_attempts = 1}}
    end;
connecting({call, From}, get_state, _Data) ->
    {keep_state_and_data, [{reply, From, connecting}]};
connecting({call, From}, _Request, _Data) ->
    {keep_state_and_data, [{reply, From, {error, connecting}}]};
connecting(_EventType, _EventContent, _Data) ->
    keep_state_and_data.

%% @doc Connected state - Normal monitoring operation
connected(enter, _OldState, Data) ->
    logger:info("Connection monitor entering connected state (leak_threshold=~p conn/min, cleanup_interval=~pms)",
                [Data#data.leak_threshold, Data#data.cleanup_interval]),

    %% Start timers
    CleanupTimer = erlang:send_after(Data#data.cleanup_interval, self(), cleanup_connections),
    LeakCheckTimer = erlang:send_after(?LEAK_WINDOW_MS, self(), check_leak),

    {keep_state,
     Data#data{cleanup_timer = CleanupTimer,
               leak_check_timer = LeakCheckTimer,
               reconnect_attempts = 0,
               leak_detected = false}};
connected({call, From}, {monitor_connection, ConnectionPid, Info}, Data) ->
    Result = do_monitor_connection(ConnectionPid, Info),
    {keep_state, Data, [{reply, From, Result}]};
connected({call, From}, {unmonitor_connection, ConnectionId}, Data) ->
    Result = do_unmonitor_connection(ConnectionId),
    {keep_state, Data, [{reply, From, Result}]};
connected({call, From}, get_connection_stats, Data) ->
    Stats = get_connection_stats_internal(Data),
    {keep_state, Data, [{reply, From, Stats}]};
connected({call, From}, force_cleanup, Data) ->
    Cleaned = do_cleanup_connections(),
    NewData =
        Data#data{cleanup_attempts = Data#data.cleanup_attempts + 1,
                  stats = maps:put(last_cleanup_count, Cleaned, Data#data.stats)},
    {keep_state, NewData, [{reply, From, {ok, Cleaned}}]};
connected({call, From}, is_leak_detected, Data) ->
    {keep_state, Data, [{reply, From, Data#data.leak_detected}]};
connected({call, From}, get_state, _Data) ->
    {keep_state_and_data, [{reply, From, connected}]};
connected(info, cleanup_connections, Data) ->
    %% Perform periodic cleanup
    Cleaned = do_cleanup_connections(),

    %% Update stats
    NewStats =
        maps:merge(Data#data.stats,
                   #{last_cleanup_count => Cleaned,
                     last_cleanup_time => erlang:monotonic_time(millisecond)}),

    %% Reset cleanup attempts if successful
    NewAttempts =
        case Cleaned of
            0 ->
                Data#data.cleanup_attempts;
            _ ->
                0
        end,

    %% Reschedule cleanup timer
    NewTimer = erlang:send_after(Data#data.cleanup_interval, self(), cleanup_connections),

    {keep_state,
     Data#data{cleanup_timer = NewTimer,
               cleanup_attempts = NewAttempts,
               stats = NewStats}};
connected(info, check_leak, Data) ->
    %% Check for connection leaks
    case do_check_leak(Data) of
        {ok, NewData} ->
            %% No leak detected, reschedule check
            NewTimer = erlang:send_after(?LEAK_WINDOW_MS, self(), check_leak),
            {keep_state, NewData#data{leak_check_timer = NewTimer}};
        {leak_detected, NewData} ->
            %% Leak detected, transition to reconnecting state
            logger:warning("Connection leak detected, entering reconnecting state"),
            {next_state, reconnecting, NewData#data{reconnect_attempts = 1}}
    end;
connected(info, {'DOWN', MonitorRef, process, Pid, Reason}, Data) ->
    %% Handle monitored process termination
    logger:debug("Connection process ~p died: ~p", [Pid, Reason]),
    cleanup_connection(Pid, MonitorRef),
    {keep_state, Data};
connected(_EventType, _EventContent, _Data) ->
    keep_state_and_data.

%% @doc Reconnecting state - Recovery mode with exponential backoff
reconnecting(enter, _OldState, #data{reconnect_attempts = Attempts} = Data)
    when Attempts >= ?MAX_RECONNECT_ATTEMPTS ->
    logger:error("Connection monitor max reconnect attempts (~p) exceeded, entering disconnected state",
                 [?MAX_RECONNECT_ATTEMPTS]),
    %% Max attempts reached, give up and transition to disconnected
    {next_state, disconnected, Data};
reconnecting(enter, _OldState, #data{reconnect_attempts = Attempts} = Data) ->
    %% Calculate exponential backoff
    BackoffMs = min(?MAX_BACKOFF_MS, ?INITIAL_BACKOFF_MS * round(math:pow(2, Attempts - 1))),
    logger:info("Connection monitor entering reconnecting state (attempt ~p/~p, backoff ~pms)",
                [Attempts, ?MAX_RECONNECT_ATTEMPTS, BackoffMs]),

    %% Cancel existing timers
    cancel_timer(Data#data.cleanup_timer),
    cancel_timer(Data#data.leak_check_timer),

    %% Schedule retry
    {keep_state, Data, [{state_timeout, BackoffMs, retry}]};
reconnecting(state_timeout, retry, Data) ->
    logger:info("Connection monitor attempting recovery"),

    %% Attempt cleanup to resolve issues
    Cleaned = do_cleanup_connections(),
    logger:info("Cleanup during recovery removed ~p connections", [Cleaned]),

    %% Check if leak is still present
    CurrentCount = get_connection_count(),
    LeakResolved =
        case Data#data.previous_count of
            0 ->
                true;
            PrevCount ->
                GrowthRate = (CurrentCount - PrevCount) * 60000 / ?LEAK_WINDOW_MS,
                GrowthRate =< Data#data.leak_threshold
        end,

    case LeakResolved of
        true ->
            %% Recovery successful, transition back to connected
            logger:info("Connection monitor recovery successful"),
            {next_state,
             connected,
             Data#data{leak_detected = false, previous_count = CurrentCount}};
        false ->
            %% Still having issues, increment attempts and retry
            logger:warning("Connection monitor recovery incomplete, retrying"),
            {next_state,
             reconnecting,
             Data#data{reconnect_attempts = Data#data.reconnect_attempts + 1,
                       previous_count = CurrentCount}}
    end;
reconnecting({call, From}, get_state, _Data) ->
    {keep_state_and_data, [{reply, From, reconnecting}]};
reconnecting({call, From}, is_leak_detected, _Data) ->
    {keep_state_and_data, [{reply, From, true}]};
reconnecting({call, From}, _Request, _Data) ->
    {keep_state_and_data, [{reply, From, {error, reconnecting}}]};
reconnecting(info, {'DOWN', MonitorRef, process, Pid, _Reason}, Data) ->
    %% Still clean up connections even during recovery
    cleanup_connection(Pid, MonitorRef),
    {keep_state, Data};
reconnecting(_EventType, _EventContent, _Data) ->
    keep_state_and_data.

terminate(_Reason, _State, _Data) ->
    %% Cleanup all monitored connections
    try
        ets:foldl(fun({Pid, _Info}, _Acc) ->
                     cleanup_connection(Pid, undefined),
                     ok
                  end,
                  ok,
                  ?ETS_TABLE)
    catch
        error:badarg ->
            ok
    end,

    %% Delete ETS table
    try
        ets:delete(?ETS_TABLE)
    catch
        error:badarg ->
            ok
    end,

    logger:info("Connection monitor terminated"),
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Monitor a connection
-spec do_monitor_connection(pid(), connection_info()) -> ok.
do_monitor_connection(ConnectionPid, Info) ->
    %% Set up process monitor
    MonitorRef = erlang:monitor(process, ConnectionPid),

    %% Build connection record
    Now = erlang:monotonic_time(millisecond),
    ConnectionInfo =
        #{pid => ConnectionPid,
          socket => maps:get(socket, Info, undefined),
          monitor_ref => MonitorRef,
          server_id => maps:get(server_id, Info, undefined),
          transport_id => maps:get(transport_id, Info, undefined),
          created_at => Now,
          last_activity => maps:get(last_activity, Info, Now),
          bytes_sent => maps:get(bytes_sent, Info, 0),
          bytes_received => maps:get(bytes_received, Info, 0)},

    %% Store in ETS
    ets:insert(?ETS_TABLE, {ConnectionPid, ConnectionInfo}),

    logger:debug("Monitoring connection ~p (ref=~p)", [ConnectionPid, MonitorRef]),
    ok.

%% @private Unmonitor a connection
-spec do_unmonitor_connection(connection_id()) -> ok.
do_unmonitor_connection(ConnectionId) ->
    case ets:lookup(?ETS_TABLE, ConnectionId) of
        [{_, Info}] ->
            %% Demonitor the process
            MonitorRef = maps:get(monitor_ref, Info, undefined),
            case MonitorRef of
                undefined ->
                    ok;
                Ref ->
                    erlang:demonitor(Ref, [flush])
            end,

            %% Remove from ETS
            ets:delete(?ETS_TABLE, ConnectionId),

            logger:debug("Unmonitoring connection ~p", [ConnectionId]),
            ok;
        [] ->
            ok
    end.

%% @private Cleanup a specific connection
-spec cleanup_connection(pid(), reference() | undefined) -> ok.
cleanup_connection(Pid, MonitorRef) ->
    case ets:lookup(?ETS_TABLE, Pid) of
        [{_, Info}] ->
            %% Close socket if exists
            Socket = maps:get(socket, Info, undefined),
            case Socket of
                undefined ->
                    ok;
                S when is_port(S) ->
                    try
                        gen_tcp:close(S)
                    catch
                        _:_ ->
                            ok
                    end;
                _ ->
                    ok
            end,

            %% Demonitor if ref provided
            case MonitorRef of
                undefined ->
                    ok;
                Ref ->
                    erlang:demonitor(Ref, [flush])
            end,

            %% Remove from tracking
            ets:delete(?ETS_TABLE, Pid),

            logger:debug("Cleaned up connection ~p", [Pid]),
            ok;
        [] ->
            ok
    end.

%%--------------------------------------------------------------------
%% @private Cleanup all orphaned connections
%%
%% Builds a set of alive PIDs using process iterator (O(1) memory)
%% then checks each connection against that set. Avoids N×is_process_alive/1 calls.
%%
%% Returns: Count of cleaned up connections
%% @end
%%--------------------------------------------------------------------
-spec do_cleanup_connections() -> non_neg_integer().
do_cleanup_connections() ->
    Now = erlang:monotonic_time(millisecond),
    OrphanedThreshold = 5 * 60 * 1000,  % 5 minutes

    %% Build set of alive PIDs using iterator (O(1) memory allocation)
    AlivePids = build_alive_pids_set(),

    %% Find orphaned connections by checking against alive set
    Orphaned =
        ets:foldl(fun({Pid, Info}, Acc) ->
                     case sets:is_element(Pid, AlivePids) of
                         false ->
                             %% Process not alive
                             [Pid | Acc];
                         true ->
                             %% Process alive, check activity
                             LastActivity = maps:get(last_activity, Info, Now),
                             case Now - LastActivity > OrphanedThreshold of
                                 true ->
                                     [Pid | Acc];
                                 false ->
                                     Acc
                             end
                     end
                  end,
                  [],
                  ?ETS_TABLE),

    %% Cleanup orphaned connections
    lists:foreach(fun(Pid) -> cleanup_connection(Pid, undefined) end, Orphaned),

    case length(Orphaned) of
        0 ->
            ok;
        Count ->
            logger:warning("Cleaned up ~p orphaned connections", [Count])
    end,

    length(Orphaned).

%% @private Build set of all alive process IDs using iterator
%% Memory: O(N) for set, but O(1) for iteration (no intermediate list)
-spec build_alive_pids_set() -> sets:set(pid()).
build_alive_pids_set() ->
    Iterator = erlang:processes_iterator(),
    build_alive_pids_set_iterator(Iterator, sets:new()).

-spec build_alive_pids_set_iterator(term(), sets:set(pid())) -> sets:set(pid()).
build_alive_pids_set_iterator(Iterator, Set) ->
    case erlang:process_next(Iterator) of
        {Pid, NewIterator} ->
            %% Add PID to set and continue
            build_alive_pids_set_iterator(NewIterator, sets:add_element(Pid, Set));
        none ->
            %% Iterator exhausted, return set
            Set
    end.

%% @private Check for connection leaks
-spec do_check_leak(#data{}) -> {ok, #data{}} | {leak_detected, #data{}}.
do_check_leak(Data) ->
    CurrentCount = get_connection_count(),
    Now = erlang:monotonic_time(millisecond),

    case Data#data.last_check_time of
        undefined ->
            %% First check, just record baseline
            {ok,
             Data#data{previous_count = CurrentCount,
                       last_check_time = Now,
                       leak_detected = false}};
        LastCheckTime ->
            TimeDelta = Now - LastCheckTime,
            CountDelta = CurrentCount - Data#data.previous_count,

            %% Calculate connections per minute
            ConnPerMin =
                case TimeDelta > 0 of
                    true ->
                        CountDelta * 60000 / TimeDelta;
                    false ->
                        0
                end,

            %% Check if leak threshold exceeded
            LeakDetected = ConnPerMin > Data#data.leak_threshold,

            case LeakDetected of
                true ->
                    logger:error("Connection leak detected: ~.2f connections/min growth (threshold: ~p conn/min)",
                                 [ConnPerMin, Data#data.leak_threshold]),
                    logger:error("Connection count: ~p -> ~p (+~p in ~pms)",
                                 [Data#data.previous_count, CurrentCount, CountDelta, TimeDelta]),

                    %% Force cleanup on leak detection
                    Cleaned = do_cleanup_connections(),
                    logger:info("Forced cleanup removed ~p orphaned connections", [Cleaned]),

                    {leak_detected,
                     Data#data{previous_count = CurrentCount,
                               last_check_time = Now,
                               leak_detected = true,
                               stats =
                                   maps:merge(Data#data.stats,
                                              #{leak_rate => ConnPerMin,
                                                leak_detected_at => Now})}};
                false ->
                    %% Log growth rate if significant
                    case ConnPerMin > 10 of
                        true ->
                            logger:info("Connection growth rate: ~.2f conn/min (~p -> ~p)",
                                        [ConnPerMin, Data#data.previous_count, CurrentCount]);
                        false ->
                            ok
                    end,

                    {ok,
                     Data#data{previous_count = CurrentCount,
                               last_check_time = Now,
                               leak_detected = false,
                               stats = maps:merge(Data#data.stats, #{leak_rate => ConnPerMin})}}
            end
    end.

%% @private Get connection statistics
-spec get_connection_stats_internal(#data{}) -> connection_stats().
get_connection_stats_internal(Data) ->
    Total = get_connection_count(),

    %% Count orphaned connections
    Orphaned =
        ets:foldl(fun({Pid, _Info}, Acc) ->
                     case erlang:is_process_alive(Pid) of
                         false ->
                             Acc + 1;
                         true ->
                             Acc
                     end
                  end,
                  0,
                  ?ETS_TABLE),

    #{total_connections => Total,
      active_connections => Total - Orphaned,
      orphaned_connections => Orphaned,
      cleanup_attempts => Data#data.cleanup_attempts,
      last_cleanup_time => maps:get(last_cleanup_time, Data#data.stats, undefined)}.

%% @private Load configuration value
-spec load_config(atom(), term()) -> term().
load_config(Key, Default) ->
    case application:get_env(erlmcp, connection_monitoring) of
        {ok, Config} when is_map(Config) ->
            maps:get(Key, Config, Default);
        {ok, Config} when is_list(Config) ->
            proplists:get_value(Key, Config, Default);
        _ ->
            Default
    end.

%% @private Cancel a timer if it exists
-spec cancel_timer(reference() | undefined) -> ok.
cancel_timer(undefined) ->
    ok;
cancel_timer(TimerRef) ->
    erlang:cancel_timer(TimerRef),
    ok.
