%%%-------------------------------------------------------------------
%%% @doc Connection Monitor - Detects and Prevents FD Leaks
%%%
%%% This module implements comprehensive connection tracking to prevent
%%% file descriptor leaks. It provides:
%%%
%%% - ETS-based tracking of all active connections
%%% - Process monitoring with automatic cleanup on termination
%%% - Leak detection (>100 connections/min growth)
%%% - Per-server connection tracking
%%% - Connection lifecycle metrics
%%% - Automatic cleanup of orphaned connections
%%%
%%% Configuration via sys.config:
%%% ```erlang
%%% {erlmcp, [
%%%     {connection_monitoring, #{
%%%         enabled => true,
%%%         leak_threshold => 100,  % connections per minute
%%%         cleanup_interval => 60000  % 1 minute
%%%     }}
%%% ]}
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_connection_monitor).

-behaviour(gen_server).

%% API
-export([
    start_link/0,
    stop/0,
    monitor_connection/2,
    unmonitor_connection/1,
    get_connection_count/0,
    get_connection_stats/0,
    force_cleanup/0,
    is_leak_detected/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

%% Constants
-define(ETS_TABLE, erlmcp_connections).
-define(DEFAULT_LEAK_THRESHOLD, 100).  % connections per minute
-define(DEFAULT_CLEANUP_INTERVAL, 60000).  % 1 minute
-define(LEAK_WINDOW_MS, 60000).  % 1 minute window
-define(MAX_CLEANUP_ATTEMPTS, 3).

%% Types
-type connection_id() :: pid() | {pid(), reference()}.
-type connection_info() :: #{
    pid => pid(),
    socket => port(),
    monitor_ref => reference(),
    server_id => atom() | binary(),
    transport_id => atom() | binary(),
    created_at => integer(),
    last_activity => integer(),
    bytes_sent => non_neg_integer(),
    bytes_received => non_neg_integer()
}.
-type leak_stats() :: #{
    current_connections => non_neg_integer(),
    connections_per_min => float(),
    leak_detected => boolean(),
    leak_rate => float()
}.
-type connection_stats() :: #{
    total_connections => non_neg_integer(),
    active_connections => non_neg_integer(),
    orphaned_connections => non_neg_integer(),
    cleanup_attempts => non_neg_integer(),
    last_cleanup_time => integer() | undefined
}.

%% Server state
-record(state, {
    leak_threshold :: pos_integer(),
    cleanup_interval :: pos_integer(),
    cleanup_timer :: reference() | undefined,
    leak_check_timer :: reference() | undefined,
    previous_count = 0 :: non_neg_integer(),
    last_check_time :: integer() | undefined,
    leak_detected = false :: boolean(),
    cleanup_attempts = 0 :: non_neg_integer(),
    stats = #{} :: map()
}).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the connection monitor
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Stop the connection monitor
-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE).

%% @doc Monitor a connection
%% Registers the connection and sets up process monitoring
-spec monitor_connection(pid(), connection_info()) -> ok.
monitor_connection(ConnectionPid, Info) when is_pid(ConnectionPid), is_map(Info) ->
    gen_server:call(?MODULE, {monitor_connection, ConnectionPid, Info}).

%% @doc Unmonitor a connection
%% Removes the connection from tracking
-spec unmonitor_connection(connection_id()) -> ok.
unmonitor_connection(ConnectionId) ->
    gen_server:call(?MODULE, {unmonitor_connection, ConnectionId}).

%% @doc Get current connection count
-spec get_connection_count() -> non_neg_integer().
get_connection_count() ->
    case ets:info(?ETS_TABLE, size) of
        undefined -> 0;
        Size -> Size
    end.

%% @doc Get connection statistics
-spec get_connection_stats() -> connection_stats().
get_connection_stats() ->
    gen_server:call(?MODULE, get_connection_stats).

%% @doc Force cleanup of orphaned connections
-spec force_cleanup() -> {ok, non_neg_integer()}.
force_cleanup() ->
    gen_server:call(?MODULE, force_cleanup).

%% @doc Check if a leak has been detected
-spec is_leak_detected() -> boolean().
is_leak_detected() ->
    gen_server:call(?MODULE, is_leak_detected).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

-spec init([]) -> {ok, #state{}}.
init([]) ->
    %% Create ETS table for connection tracking
    try
        ets:new(?ETS_TABLE, [
            named_table,
            set,
            public,
            {keypos, 1},
            {read_concurrency, true},
            {write_concurrency, true}
        ]),
        logger:info("Connection monitor ETS table created: ~p", [?ETS_TABLE])
    catch
        error:badarg ->
            %% Table already exists
            ok
    end,

    %% Load configuration
    LeakThreshold = load_config(leak_threshold, ?DEFAULT_LEAK_THRESHOLD),
    CleanupInterval = load_config(cleanup_interval, ?DEFAULT_CLEANUP_INTERVAL),

    %% Start cleanup timer
    CleanupTimer = erlang:send_after(CleanupInterval, self(), cleanup_connections),

    %% Start leak check timer
    LeakCheckTimer = erlang:send_after(?LEAK_WINDOW_MS, self(), check_leak),

    logger:info("Connection monitor started: leak_threshold=~p conn/min, cleanup_interval=~pms",
               [LeakThreshold, CleanupInterval]),

    {ok, #state{
        leak_threshold = LeakThreshold,
        cleanup_interval = CleanupInterval,
        cleanup_timer = CleanupTimer,
        leak_check_timer = LeakCheckTimer,
        last_check_time = erlang:monotonic_time(millisecond)
    }}.

handle_call({monitor_connection, ConnectionPid, Info}, _From, State) ->
    Result = do_monitor_connection(ConnectionPid, Info),
    {reply, Result, State};

handle_call({unmonitor_connection, ConnectionId}, _From, State) ->
    Result = do_unmonitor_connection(ConnectionId),
    {reply, Result, State};

handle_call(get_connection_stats, _From, State) ->
    Stats = get_connection_stats_internal(),
    {reply, Stats, State};

handle_call(force_cleanup, _From, State) ->
    Cleaned = do_cleanup_connections(),
    NewState = State#state{
        cleanup_attempts = State#state.cleanup_attempts + 1,
        stats = maps:put(last_cleanup_count, Cleaned, State#state.stats)
    },
    {reply, {ok, Cleaned}, NewState};

handle_call(is_leak_detected, _From, State) ->
    {reply, State#state.leak_detected, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(cleanup_connections, #state{cleanup_timer = _Timer} = State) ->
    %% Perform periodic cleanup
    Cleaned = do_cleanup_connections(),

    %% Update stats
    NewStats = maps:merge(State#state.stats, #{
        last_cleanup_count => Cleaned,
        last_cleanup_time => erlang:monotonic_time(millisecond)
    }),

    %% Reset cleanup attempts if successful
    NewAttempts = case Cleaned of
        0 -> State#state.cleanup_attempts;
        _ -> 0
    end,

    %% Reschedule cleanup timer
    NewTimer = erlang:send_after(State#state.cleanup_interval, self(), cleanup_connections),

    {noreply, State#state{
        cleanup_timer = NewTimer,
        cleanup_attempts = NewAttempts,
        stats = NewStats
    }};

handle_info(check_leak, #state{leak_check_timer = _Timer} = State) ->
    %% Check for connection leaks
    NewState = do_check_leak(State),

    %% Reschedule leak check timer
    NewTimer = erlang:send_after(?LEAK_WINDOW_MS, self(), check_leak),

    {noreply, NewState#state{leak_check_timer = NewTimer}};

handle_info({'DOWN', MonitorRef, process, Pid, Reason}, State) ->
    %% Handle monitored process termination
    logger:info("Connection process ~p died: ~p", [Pid, Reason]),
    cleanup_connection(Pid, MonitorRef),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    %% Cleanup all monitored connections
    try
        ets:foldl(fun({Pid, _Info}, _Acc) ->
            cleanup_connection(Pid, undefined),
            ok
        end, ok, ?ETS_TABLE)
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

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

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
    ConnectionInfo = #{
        pid => ConnectionPid,
        socket => maps:get(socket, Info, undefined),
        monitor_ref => MonitorRef,
        server_id => maps:get(server_id, Info, undefined),
        transport_id => maps:get(transport_id, Info, undefined),
        created_at => Now,
        last_activity => Now,
        bytes_sent => maps:get(bytes_sent, Info, 0),
        bytes_received => maps:get(bytes_received, Info, 0)
    },

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
                undefined -> ok;
                Ref -> erlang:demonitor(Ref, [flush])
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
                undefined -> ok;
                S ->
                    try
                        gen_tcp:close(S)
                    catch
                        _:_ -> ok
                    end
            end,

            %% Demonitor if ref provided
            case MonitorRef of
                undefined -> ok;
                Ref -> erlang:demonitor(Ref, [flush])
            end,

            %% Remove from tracking
            ets:delete(?ETS_TABLE, Pid),

            logger:debug("Cleaned up connection ~p", [Pid]),
            ok;
        [] ->
            ok
    end.

%% @private Cleanup all orphaned connections
-spec do_cleanup_connections() -> non_neg_integer().
do_cleanup_connections() ->
    Now = erlang:monotonic_time(millisecond),
    OrphanedThreshold = 5 * 60 * 1000,  % 5 minutes

    %% Find orphaned connections (no process alive or inactive)
    Orphaned = ets:foldl(fun({Pid, Info}, Acc) ->
        case erlang:is_process_alive(Pid) of
            false ->
                [Pid | Acc];
            true ->
                LastActivity = maps:get(last_activity, Info, Now),
                case (Now - LastActivity) > OrphanedThreshold of
                    true -> [Pid | Acc];
                    false -> Acc
                end
        end
    end, [], ?ETS_TABLE),

    %% Cleanup orphaned connections
    lists:foreach(fun(Pid) ->
        cleanup_connection(Pid, undefined)
    end, Orphaned),

    case length(Orphaned) of
        0 -> ok;
        Count -> logger:warning("Cleaned up ~p orphaned connections", [Count])
    end,

    length(Orphaned).

%% @private Check for connection leaks
-spec do_check_leak(#state{}) -> #state{}.
do_check_leak(State) ->
    CurrentCount = get_connection_count(),
    Now = erlang:monotonic_time(millisecond),

    case State#state.last_check_time of
        undefined ->
            %% First check, just record baseline
            State#state{
                previous_count = CurrentCount,
                last_check_time = Now,
                leak_detected = false
            };
        LastCheckTime ->
            TimeDelta = Now - LastCheckTime,
            CountDelta = CurrentCount - State#state.previous_count,

            %% Calculate connections per minute
            ConnPerMin = case TimeDelta > 0 of
                true -> (CountDelta * 60000) / TimeDelta;
                false -> 0
            end,

            %% Check if leak threshold exceeded
            LeakDetected = ConnPerMin > State#state.leak_threshold,

            case LeakDetected of
                true ->
                    logger:error("Connection leak detected: ~.2f connections/min growth (threshold: ~p conn/min)",
                               [ConnPerMin, State#state.leak_threshold]),
                    logger:error("Connection count: ~p -> ~p (+~p in ~pms)",
                               [State#state.previous_count, CurrentCount, CountDelta, TimeDelta]),

                    %% Force cleanup on leak detection
                    Cleaned = do_cleanup_connections(),
                    logger:info("Forced cleanup removed ~p orphaned connections", [Cleaned]),

                    State#state{
                        previous_count = CurrentCount,
                        last_check_time = Now,
                        leak_detected = true,
                        stats = maps:merge(State#state.stats, #{
                            leak_rate => ConnPerMin,
                            leak_detected_at => Now
                        })
                    };
                false ->
                    %% Log growth rate if significant
                    case ConnPerMin > 10 of
                        true ->
                            logger:info("Connection growth rate: ~.2f conn/min (~p -> ~p)",
                                       [ConnPerMin, State#state.previous_count, CurrentCount]);
                        false ->
                            ok
                    end,

                    State#state{
                        previous_count = CurrentCount,
                        last_check_time = Now,
                        leak_detected = false,
                        stats = maps:merge(State#state.stats, #{
                            leak_rate => ConnPerMin
                        })
                    }
            end
    end.

%% @private Get connection statistics
-spec get_connection_stats_internal() -> connection_stats().
get_connection_stats_internal() ->
    Total = get_connection_count(),

    %% Count orphaned connections
    Orphaned = ets:foldl(fun({Pid, _Info}, Acc) ->
        case erlang:is_process_alive(Pid) of
            false -> Acc + 1;
            true -> Acc
        end
    end, 0, ?ETS_TABLE),

    #{
        total_connections => Total,
        active_connections => Total - Orphaned,
        orphaned_connections => Orphaned,
        cleanup_attempts => 0,
        last_cleanup_time => undefined
    }.

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
