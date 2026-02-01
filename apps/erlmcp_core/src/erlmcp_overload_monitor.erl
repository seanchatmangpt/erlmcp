-module(erlmcp_overload_monitor).
-behaviour(gen_server).

%%% ====================================================================
%%% Overload Monitor - Queue Depth Tracking and Alerting
%%% ====================================================================
%%%
%%% Monitors queue depths across all process roles and triggers alerts
%%% when capacity thresholds are exceeded.
%%%
%%% Features:
%%%   - Real-time queue depth tracking per role
%%%   - Alert triggers at 80% capacity (warning) and 90% (critical)
%%%   - Live interrogation via queues/0 and queues/1
%%%   - Integration with erlmcp_health_monitor
%%%   - Periodic health checks (configurable interval)
%%%
%%% Alerts:
%%%   - warning: 80% capacity - log warning
%%%   - critical: 90% capacity - log error + health monitor notification
%%%   - overload: 100% capacity - circuit breaker trigger
%%%
%%% Usage:
%%%   erlmcp_overload_monitor:queues()         %% All roles
%%%   erlmcp_overload_monitor:queues(session)  %% Specific role
%%%   erlmcp_overload_monitor:get_overloaded() %% Processes at capacity
%%% ====================================================================

-include("erlmcp.hrl").

%% API exports
-export([
    start_link/0,
    start_link/1,
    queues/0,
    queues/1,
    get_overloaded/0,
    get_overloaded/1,
    get_alert_history/0,
    get_alert_history/1,
    reset_alerts/0,
    set_check_interval/1,
    force_check/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Alert types
-type alert_level() :: warning | critical | overload.
-type alert() :: #{
    level := alert_level(),
    role := erlmcp_queue_limits:role(),
    process := pid() | atom(),
    depth := non_neg_integer(),
    limit := non_neg_integer(),
    utilization := float(),
    timestamp := integer()
}.

%% Queue information
-type queue_info() :: #{
    role := erlmcp_queue_limits:role(),
    process := pid() | atom(),
    depth := non_neg_integer(),
    limit := non_neg_integer(),
    utilization := float(),
    status := ok | warning | critical | overload
}.

-export_type([alert_level/0, alert/0, queue_info/0]).

%% State record
-record(state, {
    check_interval :: pos_integer(),
    timer_ref :: reference() | undefined,
    alert_history = [] :: [alert()],
    max_alert_history :: pos_integer(),
    warning_threshold :: float(),
    critical_threshold :: float(),
    overload_threshold :: float(),
    last_check :: integer() | undefined
}).

-type state() :: #state{}.

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link([]).

-spec start_link(list()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

%% @doc Get queue information for all roles
-spec queues() -> [queue_info()].
queues() ->
    gen_server:call(?MODULE, queues).

%% @doc Get queue information for a specific role
-spec queues(erlmcp_queue_limits:role()) -> [queue_info()].
queues(Role) ->
    gen_server:call(?MODULE, {queues, Role}).

%% @doc Get all overloaded processes (at or above capacity)
-spec get_overloaded() -> [queue_info()].
get_overloaded() ->
    gen_server:call(?MODULE, get_overloaded).

%% @doc Get overloaded processes for a specific role
-spec get_overloaded(erlmcp_queue_limits:role()) -> [queue_info()].
get_overloaded(Role) ->
    gen_server:call(?MODULE, {get_overloaded, Role}).

%% @doc Get alert history (last N alerts)
-spec get_alert_history() -> [alert()].
get_alert_history() ->
    gen_server:call(?MODULE, get_alert_history).

%% @doc Get alert history for a specific role
-spec get_alert_history(erlmcp_queue_limits:role()) -> [alert()].
get_alert_history(Role) ->
    gen_server:call(?MODULE, {get_alert_history, Role}).

%% @doc Reset alert history
-spec reset_alerts() -> ok.
reset_alerts() ->
    gen_server:call(?MODULE, reset_alerts).

%% @doc Set health check interval (milliseconds)
-spec set_check_interval(pos_integer()) -> ok.
set_check_interval(IntervalMs) when is_integer(IntervalMs), IntervalMs > 0 ->
    gen_server:call(?MODULE, {set_check_interval, IntervalMs}).

%% @doc Force immediate health check
-spec force_check() -> ok.
force_check() ->
    gen_server:cast(?MODULE, force_check).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init(list()) -> {ok, state()}.
init(Opts) ->
    %% Load configuration values with fallbacks
    CheckInterval = application:get_env(erlmcp_core, overload_check_interval_ms, 5000),
    MaxAlertHistory = application:get_env(erlmcp_core, overload_max_alert_history, 100),
    WarningThreshold = application:get_env(erlmcp_core, overload_warning_threshold, 0.80),
    CriticalThreshold = application:get_env(erlmcp_core, overload_critical_threshold, 0.90),
    OverloadThreshold = application:get_env(erlmcp_core, overload_threshold, 1.00),

    %% Allow runtime override via options (for testing)
    FinalCheckInterval = proplists:get_value(check_interval, Opts, CheckInterval),
    FinalMaxAlertHistory = proplists:get_value(max_alert_history, Opts, MaxAlertHistory),
    FinalWarningThreshold = proplists:get_value(warning_threshold, Opts, WarningThreshold),
    FinalCriticalThreshold = proplists:get_value(critical_threshold, Opts, CriticalThreshold),
    FinalOverloadThreshold = proplists:get_value(overload_threshold, Opts, OverloadThreshold),

    %% Start periodic health check timer
    TimerRef = erlang:send_after(FinalCheckInterval, self(), health_check),

    {ok, #state{
        check_interval = FinalCheckInterval,
        timer_ref = TimerRef,
        max_alert_history = FinalMaxAlertHistory,
        warning_threshold = FinalWarningThreshold,
        critical_threshold = FinalCriticalThreshold,
        overload_threshold = FinalOverloadThreshold
    }}.

-spec handle_call(term(), {pid(), term()}, state()) ->
    {reply, term(), state()}.
handle_call(queues, _From, State) ->
    QueueInfos = collect_queue_info(State),
    {reply, QueueInfos, State};

handle_call({queues, Role}, _From, State) ->
    AllQueues = collect_queue_info(State),
    RoleQueues = lists:filter(
        fun(#{role := R}) -> R =:= Role end,
        AllQueues
    ),
    {reply, RoleQueues, State};

handle_call(get_overloaded, _From, State) ->
    AllQueues = collect_queue_info(State),
    Overloaded = lists:filter(
        fun(#{status := Status}) ->
            Status =:= critical orelse Status =:= overload
        end,
        AllQueues
    ),
    {reply, Overloaded, State};

handle_call({get_overloaded, Role}, _From, State) ->
    AllQueues = collect_queue_info(State),
    Overloaded = lists:filter(
        fun(#{role := R, status := Status}) ->
            R =:= Role andalso (Status =:= critical orelse Status =:= overload)
        end,
        AllQueues
    ),
    {reply, Overloaded, State};

handle_call(get_alert_history, _From, State) ->
    {reply, State#state.alert_history, State};

handle_call({get_alert_history, Role}, _From, State) ->
    RoleAlerts = lists:filter(
        fun(#{role := R}) -> R =:= Role end,
        State#state.alert_history
    ),
    {reply, RoleAlerts, State};

handle_call(reset_alerts, _From, State) ->
    {reply, ok, State#state{alert_history = []}};

handle_call({set_check_interval, IntervalMs}, _From, State) ->
    %% Cancel old timer
    case State#state.timer_ref of
        undefined -> ok;
        OldRef -> erlang:cancel_timer(OldRef)
    end,

    %% Start new timer
    NewRef = erlang:send_after(IntervalMs, self(), health_check),

    {reply, ok, State#state{check_interval = IntervalMs, timer_ref = NewRef}};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(force_check, State) ->
    NewState = perform_health_check(State),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(health_check, State) ->
    %% Perform health check
    NewState = perform_health_check(State),

    %% Schedule next check
    TimerRef = erlang:send_after(State#state.check_interval, self(), health_check),

    {noreply, NewState#state{timer_ref = TimerRef}};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, #state{timer_ref = TimerRef}) ->
    case TimerRef of
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

%% @doc Perform periodic health check
perform_health_check(State) ->
    QueueInfos = collect_queue_info(),
    Timestamp = erlang:system_time(millisecond),

    %% Check for alerts
    {Alerts, NewAlertHistory} = lists:foldl(
        fun(QueueInfo, {AccAlerts, AccHistory}) ->
            case check_thresholds(QueueInfo, Timestamp) of
                {ok, _} ->
                    {AccAlerts, AccHistory};
                {alert, Alert} ->
                    NewHistory = lists:sublist([Alert | AccHistory], State#state.max_alert_history),
                    {[Alert | AccAlerts], NewHistory}
            end
        end,
        {[], State#state.alert_history},
        QueueInfos
    ),

    %% Process alerts
    lists:foreach(fun process_alert/1, Alerts),

    State#state{
        alert_history = NewAlertHistory,
        last_check = Timestamp
    }.

%% @doc Collect queue information for all roles
collect_queue_info(State) ->
    case whereis(erlmcp_queue_limits) of
        undefined ->
            [];
        _Pid ->
            AllStats = erlmcp_queue_limits:get_all_stats(),
            AllLimits = erlmcp_queue_limits:get_all_limits(),

            %% Build queue info from stats
            maps:fold(
                fun(Role, Stats, Acc) ->
                    Limit = maps:get(Role, AllLimits, 1000),
                    Depth = maps:get(current_depth, Stats, 0),
                    Utilization = case Limit of
                        0 -> 0.0;
                        _ -> Depth / Limit
                    end,

                    Status = determine_status(Utilization, State),

                    Info = #{
                        role => Role,
                        process => Role,  % Using role as process identifier
                        depth => Depth,
                        limit => Limit,
                        utilization => Utilization,
                        status => Status
                    },

                    [Info | Acc]
                end,
                [],
                AllStats
            )
    end.

%% @doc Determine status based on utilization and thresholds
determine_status(Utilization, State) ->
    OverloadThreshold = State#state.overload_threshold,
    CriticalThreshold = State#state.critical_threshold,
    WarningThreshold = State#state.warning_threshold,

    if
        Utilization >= OverloadThreshold -> overload;
        Utilization >= CriticalThreshold -> critical;
        Utilization >= WarningThreshold -> warning;
        true -> ok
    end.

%% @doc Check if queue info exceeds thresholds
check_thresholds(#{status := ok}, _Timestamp) ->
    {ok, no_alert};
check_thresholds(#{status := Status, role := Role, process := Proc,
                   depth := Depth, limit := Limit, utilization := Util} = QueueInfo,
                 Timestamp) ->
    Level = case Status of
        warning -> warning;
        critical -> critical;
        overload -> overload
    end,

    Alert = #{
        level => Level,
        role => Role,
        process => Proc,
        depth => Depth,
        limit => Limit,
        utilization => Util,
        timestamp => Timestamp
    },

    {alert, Alert}.

%% @doc Process an alert (log and notify health monitor)
process_alert(#{level := warning, role := Role, process := Proc,
                depth := Depth, limit := Limit, utilization := Util}) ->
    logger:warning("Queue utilization warning: ~p:~p at ~.1f% (~p/~p)",
                  [Role, Proc, Util * 100, Depth, Limit]);

process_alert(#{level := critical, role := Role, process := Proc,
                depth := Depth, limit := Limit, utilization := Util}) ->
    logger:error("Queue utilization critical: ~p:~p at ~.1f% (~p/~p)",
                [Role, Proc, Util * 100, Depth, Limit]),

    %% Notify health monitor
    case whereis(erlmcp_health_monitor) of
        undefined -> ok;
        _Pid ->
            erlmcp_health_monitor:report_degradation(#{
                component => Role,
                reason => queue_overload,
                depth => Depth,
                limit => Limit,
                utilization => Util
            })
    end;

process_alert(#{level := overload, role := Role, process := Proc,
                depth := Depth, limit := Limit, utilization := Util}) ->
    logger:critical("Queue overload: ~p:~p at ~.1f% (~p/~p) - SHEDDING LOAD",
                   [Role, Proc, Util * 100, Depth, Limit]),

    %% Notify health monitor as unhealthy
    case whereis(erlmcp_health_monitor) of
        undefined -> ok;
        _Pid ->
            erlmcp_health_monitor:report_degradation(#{
                component => Role,
                reason => queue_overload,
                depth => Depth,
                limit => Limit,
                utilization => Util
            })
    end,

    %% Trigger circuit breaker if available
    case whereis(erlmcp_circuit_breaker) of
        undefined -> ok;
        _Pid ->
            erlmcp_circuit_breaker:open(Role, queue_overload)
    end.
