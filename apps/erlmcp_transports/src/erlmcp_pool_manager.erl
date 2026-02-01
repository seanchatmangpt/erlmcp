%%%====================================================================
%%% erlmcp_pool_manager.erl - Connection Pool Manager
%%%====================================================================
%%%
%%% Dynamic connection pooling for high-throughput scenarios.
%%%
%%% Features:
%%% - Dynamic pool sizing (10→1000 connections)
%%% - Health-aware routing (skip unhealthy connections)
%%% - Load balancing strategies: round_robin, least_loaded, random
%%% - Metrics: pool utilization, checkout time, failures
%%%
%%% Usage:
%%%   {ok, Pid} = erlmcp_pool_manager:start_link(PoolOpts),
%%%   {ok, ConnPid} = erlmcp_pool_manager:checkout(Pid),
%%%   erlmcp_pool_manager:checkin(Pid, ConnPid).
%%%
%%%====================================================================

-module(erlmcp_pool_manager).

-behaviour(gen_server).

-include("erlmcp_pool.hrl").

%% Public API
-export([start_link/1, start_link/2, checkout/1, checkout/2, checkin/2, get_metrics/1, get_status/1,
         resize/2, stop/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Types
-type pool_opts() ::
    #{name => atom(),
      min_size => pos_integer(),
      max_size => pos_integer(),
      strategy => round_robin | least_loaded | random,
      health_check_interval => pos_integer(),
      checkout_timeout => timeout(),
      worker_module => module(),
      worker_opts => map()}.
-type pool_metrics() ::
    #{total_checkouts => non_neg_integer(),
      active_connections => non_neg_integer(),
      idle_connections => non_neg_integer(),
      failed_checkouts => non_neg_integer(),
      avg_checkout_time_us => float(),
      pool_utilization_percent => float(),
      health_check_failures => non_neg_integer(),
      current_size => pos_integer(),
      max_size => pos_integer(),
      strategy => atom()}.

-record(state,
        {name :: atom() | undefined,
         min_size :: pos_integer(),
         max_size :: pos_integer(),
         strategy :: erlmcp_pool_strategy:strategy(),
         health_check_interval :: pos_integer(),
         checkout_timeout :: timeout(),
         worker_module :: module(),
         worker_opts :: map(),
         %% Pool state
         connections = [] :: [#connection{}],
         idle_connections = [] :: [pid()],
         active_connections = #{} :: #{pid() => connection_info()},
         %% Metrics
         total_checkouts = 0 :: non_neg_integer(),
         failed_checkouts = 0 :: non_neg_integer(),
         checkout_times = [] :: [non_neg_integer()],
         health_check_failures = 0 :: non_neg_integer(),
         %% Health check timer
         health_timer :: reference() | undefined}).
-record(connection_info, {conn :: #connection{}, checkout_time :: integer()}).

-type connection_info() :: #connection_info{}.

-export_type([pool_opts/0, pool_metrics/0]).

%% Default values
-define(DEFAULT_MIN_SIZE, 10).
-define(DEFAULT_MAX_SIZE, 1000).
-define(DEFAULT_STRATEGY, round_robin).
-define(DEFAULT_HEALTH_CHECK_INTERVAL, 5000).
-define(DEFAULT_CHECKOUT_TIMEOUT, 5000).
-define(MAX_CHECKOUT_TIMES, 1000).

%%====================================================================
%% Public API
%%====================================================================

%% @doc Start pool manager with options
-spec start_link(pool_opts()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

%% @doc Start named pool manager
-spec start_link(atom(), pool_opts()) -> {ok, pid()} | {error, term()}.
start_link(Name, Opts) ->
    gen_server:start_link({local, Name}, ?MODULE, Opts#{name => Name}, []).

%% @doc Check out a connection from the pool
-spec checkout(pid()) -> {ok, pid()} | {error, term()}.
checkout(Pool) ->
    checkout(Pool, ?DEFAULT_CHECKOUT_TIMEOUT).

%% @doc Check out a connection with timeout
-spec checkout(pid(), timeout()) -> {ok, pid()} | {error, term()}.
checkout(Pool, Timeout) ->
    gen_server:call(Pool, {checkout, Timeout}, Timeout + 1000).

%% @doc Return a connection to the pool
-spec checkin(pid(), pid()) -> ok.
checkin(Pool, ConnPid) ->
    gen_server:cast(Pool, {checkin, ConnPid}).

%% @doc Get pool metrics
-spec get_metrics(pid()) -> pool_metrics().
get_metrics(Pool) ->
    gen_server:call(Pool, get_metrics).

%% @doc Get pool status
-spec get_status(pid()) -> map().
get_status(Pool) ->
    gen_server:call(Pool, get_status).

%% @doc Resize pool
-spec resize(pid(), pos_integer()) -> ok | {error, term()}.
resize(Pool, NewSize) ->
    gen_server:call(Pool, {resize, NewSize}).

%% @doc Stop pool manager
-spec stop(pid()) -> ok.
stop(Pool) ->
    gen_server:stop(Pool).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

init(Opts) ->
    process_flag(trap_exit, true),

    MinSize = maps:get(min_size, Opts, ?DEFAULT_MIN_SIZE),
    MaxSize = maps:get(max_size, Opts, ?DEFAULT_MAX_SIZE),
    Strategy = maps:get(strategy, Opts, ?DEFAULT_STRATEGY),
    HealthCheckInterval = maps:get(health_check_interval, Opts, ?DEFAULT_HEALTH_CHECK_INTERVAL),
    CheckoutTimeout = maps:get(checkout_timeout, Opts, ?DEFAULT_CHECKOUT_TIMEOUT),
    WorkerModule = maps:get(worker_module, Opts),
    WorkerOpts = maps:get(worker_opts, Opts, #{}),
    Name = maps:get(name, Opts, undefined),

    %% Validate strategy
    StrategyMod = erlmcp_pool_strategy:get_strategy_module(Strategy),

    State =
        #state{name = Name,
               min_size = MinSize,
               max_size = MaxSize,
               strategy = StrategyMod,
               health_check_interval = HealthCheckInterval,
               checkout_timeout = CheckoutTimeout,
               worker_module = WorkerModule,
               worker_opts = WorkerOpts},

    %% Pre-warm pool with minimum connections
    State2 = prewarm_pool(State, MinSize),

    %% Start health check timer
    Timer = erlang:send_after(HealthCheckInterval, self(), health_check),
    State3 = State2#state{health_timer = Timer},

    {ok, State3}.

handle_call({checkout, Timeout}, From, State) ->
    StartTime = erlang:monotonic_time(microsecond),

    case do_checkout(State, Timeout) of
        {ok, ConnPid, NewState} ->
            CheckoutTime = erlang:monotonic_time(microsecond) - StartTime,
            FinalState = record_checkout(NewState, CheckoutTime),
            {reply, {ok, ConnPid}, FinalState};
        {error, Reason, NewState} ->
            FinalState = NewState#state{failed_checkouts = NewState#state.failed_checkouts + 1},
            {reply, {error, Reason}, FinalState}
    end;
handle_call(get_metrics, _From, State) ->
    Metrics = calculate_metrics(State),
    {reply, Metrics, State};
handle_call(get_status, _From, State) ->
    Status =
        #{name => State#state.name,
          min_size => State#state.min_size,
          max_size => State#state.max_size,
          current_size => length(State#state.connections),
          idle_count => length(State#state.idle_connections),
          active_count => maps:size(State#state.active_connections),
          strategy => erlmcp_pool_strategy:strategy_name(State#state.strategy),
          health_status => pool_health_status(State)},
    {reply, Status, State};
handle_call({resize, NewSize}, _From, State) ->
    case resize_pool(State, NewSize) of
        {ok, NewState} ->
            {reply, ok, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({checkin, ConnPid}, State) ->
    NewState = do_checkin(State, ConnPid),
    {noreply, NewState};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(health_check, State) ->
    NewState = perform_health_checks(State),

    %% Schedule next health check
    Timer = erlang:send_after(State#state.health_check_interval, self(), health_check),
    FinalState = NewState#state{health_timer = Timer},

    {noreply, FinalState};
handle_info({'DOWN', MonitorRef, process, ConnPid, Reason}, State) ->
    logger:warning("Connection ~p died: ~p", [ConnPid, Reason]),
    NewState = handle_connection_down(State, ConnPid, MonitorRef),
    {noreply, NewState};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    %% Cancel health check timer
    case State#state.health_timer of
        undefined ->
            ok;
        Timer ->
            erlang:cancel_timer(Timer)
    end,

    %% Close all connections
    lists:foreach(fun(#connection{pid = Pid}) -> catch exit(Pid, shutdown) end,
                  State#state.connections),

    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions - Connection Management
%%====================================================================

%% @doc Pre-warm pool with initial connections
prewarm_pool(State, Count) ->
    Connections = [create_connection(State) || _ <- lists:seq(1, Count)],
    IdleConnections = [Conn#connection.pid || Conn <- Connections],

    State#state{connections = Connections, idle_connections = IdleConnections}.

%% @doc Create a new connection
create_connection(#state{worker_module = Module, worker_opts = Opts}) ->
    case Module:start_link(Opts) of
        {ok, Pid} ->
            MonitorRef = monitor(process, Pid),
            #connection{pid = Pid,
                        monitor_ref = MonitorRef,
                        created_at = erlang:monotonic_time(millisecond),
                        last_used = erlang:monotonic_time(millisecond)};
        {error, Reason} ->
            logger:error("Failed to create connection: ~p", [Reason]),
            error({connection_creation_failed, Reason})
    end.

%% @doc Check out a connection using the configured strategy
do_checkout(State, Timeout) ->
    HealthyConns =
        [Conn || Conn <- State#state.connections, Conn#connection.health_status =:= healthy],

    case HealthyConns of
        [] ->
            %% Try to create new connection if under max_size
            case length(State#state.connections) < State#state.max_size of
                true ->
                    try
                        NewConn = create_connection(State),
                        NewState = State#state{connections = [NewConn | State#state.connections]},
                        checkout_connection(NewState, NewConn)
                    catch
                        _:Error ->
                            {error, {connection_creation_failed, Error}, State}
                    end;
                false ->
                    {error, no_healthy_connections, State}
            end;
        _ ->
            %% Select connection using strategy
            case erlmcp_pool_strategy:select_connection(State#state.strategy,
                                                        HealthyConns,
                                                        State#state.idle_connections)
            of
                {ok, ConnPid} ->
                    case lists:keyfind(ConnPid, #connection.pid, State#state.connections) of
                        #connection{} = Conn ->
                            checkout_connection(State, Conn);
                        false ->
                            {error, connection_not_found, State}
                    end;
                {error, no_idle_connections} ->
                    %% No idle connections, try to grow pool if under max_size
                    case length(State#state.connections) < State#state.max_size of
                        true ->
                            try
                                NewConn = create_connection(State),
                                NewState =
                                    State#state{connections = [NewConn | State#state.connections]},
                                checkout_connection(NewState, NewConn)
                            catch
                                _:Error ->
                                    {error, {connection_creation_failed, Error}, State}
                            end;
                        false ->
                            {error, no_idle_connections, State}
                    end;
                {error, Reason} ->
                    {error, Reason, State}
            end
    end.

%% @doc Mark connection as checked out
checkout_connection(State, #connection{pid = ConnPid} = Conn) ->
    Now = erlang:monotonic_time(millisecond),
    UpdatedConn =
        Conn#connection{last_used = Now, request_count = Conn#connection.request_count + 1},

    ConnInfo =
        #connection_info{conn = UpdatedConn, checkout_time = erlang:monotonic_time(microsecond)},

    NewState =
        State#state{connections =
                        lists:keyreplace(ConnPid,
                                         #connection.pid,
                                         State#state.connections,
                                         UpdatedConn),
                    idle_connections = lists:delete(ConnPid, State#state.idle_connections),
                    active_connections =
                        maps:put(ConnPid, ConnInfo, State#state.active_connections)},

    {ok, ConnPid, NewState}.

%% @doc Check in a connection
do_checkin(State, ConnPid) ->
    case maps:get(ConnPid, State#state.active_connections, undefined) of
        undefined ->
            logger:warning("Checkin of unknown connection: ~p", [ConnPid]),
            State;
        #connection_info{conn = Conn} ->
            State#state{idle_connections = [ConnPid | State#state.idle_connections],
                        active_connections = maps:remove(ConnPid, State#state.active_connections)}
    end.

%% @doc Handle connection process termination
handle_connection_down(State, ConnPid, MonitorRef) ->
    %% Remove from all tracking structures
    NewConnections = lists:keydelete(ConnPid, #connection.pid, State#state.connections),
    NewIdle = lists:delete(ConnPid, State#state.idle_connections),
    NewActive = maps:remove(ConnPid, State#state.active_connections),

    NewState =
        State#state{connections = NewConnections,
                    idle_connections = NewIdle,
                    active_connections = NewActive},

    %% Replace connection if under min_size
    case length(NewConnections) < State#state.min_size of
        true ->
            try
                NewConn = create_connection(State),
                NewState#state{connections = [NewConn | NewConnections],
                               idle_connections = [NewConn#connection.pid | NewIdle]}
            catch
                _:Error ->
                    logger:error("Failed to replace dead connection: ~p", [Error]),
                    NewState
            end;
        false ->
            NewState
    end.

%%====================================================================
%% Internal Functions - Health Checks
%%====================================================================

%% @doc Perform health checks on all connections
perform_health_checks(State) ->
    UpdatedConnections =
        lists:map(fun(Conn) -> check_connection_health(Conn) end, State#state.connections),

    %% Count failures
    Failures = length([C || C <- UpdatedConnections, C#connection.health_status =:= unhealthy]),

    %% Update idle list to exclude unhealthy connections
    HealthyIdle =
        [Pid
         || Pid <- State#state.idle_connections,
            lists:keyfind(Pid, #connection.pid, UpdatedConnections) =/= false,
            (lists:keyfind(Pid, #connection.pid, UpdatedConnections))#connection.health_status
            =:= healthy],

    State#state{connections = UpdatedConnections,
                idle_connections = HealthyIdle,
                health_check_failures = State#state.health_check_failures + Failures}.

%% @doc Check health of a single connection
check_connection_health(#connection{pid = Pid} = Conn) ->
    case erlang:is_process_alive(Pid) of
        true ->
            %% Simple health check - process is alive
            %% Can be extended with ping/pong protocol
            Conn#connection{health_status = healthy, failure_count = 0};
        false ->
            Conn#connection{health_status = unhealthy,
                            failure_count = Conn#connection.failure_count + 1}
    end.

%% @doc Calculate overall pool health status
pool_health_status(State) ->
    Total = length(State#state.connections),
    Healthy = length([C || C <- State#state.connections, C#connection.health_status =:= healthy]),

    case Total of
        0 ->
            empty;
        _ ->
            HealthPercent = Healthy / Total * 100,
            if HealthPercent >= 80 ->
                   healthy;
               HealthPercent >= 50 ->
                   degraded;
               true ->
                   unhealthy
            end
    end.

%%====================================================================
%% Internal Functions - Metrics
%%====================================================================

%% @doc Record checkout time
record_checkout(State, CheckoutTime) ->
    CheckoutTimes = State#state.checkout_times,
    NewCheckoutTimes =
        case length(CheckoutTimes) >= ?MAX_CHECKOUT_TIMES of
            true ->
                lists:sublist([CheckoutTime | CheckoutTimes], ?MAX_CHECKOUT_TIMES);
            false ->
                [CheckoutTime | CheckoutTimes]
        end,

    State#state{total_checkouts = State#state.total_checkouts + 1,
                checkout_times = NewCheckoutTimes}.

%% @doc Calculate pool metrics
calculate_metrics(State) ->
    TotalConns = length(State#state.connections),
    IdleConns = length(State#state.idle_connections),
    ActiveConns = maps:size(State#state.active_connections),

    AvgCheckoutTime =
        case State#state.checkout_times of
            [] ->
                0.0;
            Times ->
                lists:sum(Times) / length(Times)
        end,

    Utilization =
        case TotalConns of
            0 ->
                0.0;
            _ ->
                ActiveConns / TotalConns * 100
        end,

    #{total_checkouts => State#state.total_checkouts,
      active_connections => ActiveConns,
      idle_connections => IdleConns,
      failed_checkouts => State#state.failed_checkouts,
      avg_checkout_time_us => AvgCheckoutTime,
      pool_utilization_percent => Utilization,
      health_check_failures => State#state.health_check_failures,
      current_size => TotalConns,
      max_size => State#state.max_size,
      strategy => erlmcp_pool_strategy:strategy_name(State#state.strategy)}.

%%====================================================================
%% Internal Functions - Pool Sizing
%%====================================================================

%% @doc Resize pool to target size
resize_pool(State, NewSize) when NewSize < State#state.min_size ->
    {error, {below_minimum, State#state.min_size}};
resize_pool(State, NewSize) when NewSize > State#state.max_size ->
    {error, {above_maximum, State#state.max_size}};
resize_pool(State, NewSize) ->
    CurrentSize = length(State#state.connections),

    if NewSize > CurrentSize ->
           %% Grow pool
           Additional = NewSize - CurrentSize,
           NewConns = [create_connection(State) || _ <- lists:seq(1, Additional)],
           NewIdle = [C#connection.pid || C <- NewConns] ++ State#state.idle_connections,
           {ok,
            State#state{connections = NewConns ++ State#state.connections,
                        idle_connections = NewIdle}};
       NewSize < CurrentSize ->
           %% Shrink pool (remove only idle connections)
           ToRemove = CurrentSize - NewSize,
           {RemovedConns, RemainingIdle} =
               remove_idle_connections(State#state.idle_connections,
                                       State#state.connections,
                                       ToRemove),

           %% Close removed connections
           lists:foreach(fun(#connection{pid = Pid}) -> catch exit(Pid, shutdown) end,
                         RemovedConns),

           RemainingConns =
               lists:filter(fun(Conn) -> not lists:member(Conn, RemovedConns) end,
                            State#state.connections),

           {ok, State#state{connections = RemainingConns, idle_connections = RemainingIdle}};
       true ->
           {ok, State}
    end.

%% @doc Remove idle connections for pool shrinking
remove_idle_connections(IdleList, AllConns, Count) ->
    ToRemovePids = lists:sublist(IdleList, Count),
    RemovedConns = [Conn || Conn <- AllConns, lists:member(Conn#connection.pid, ToRemovePids)],
    RemainingIdle = IdleList -- ToRemovePids,
    {RemovedConns, RemainingIdle}.
