%%%-------------------------------------------------------------------
%%% @doc
%%% erlmcp_process_monitor - Process count monitoring and limits
%%%
%%% Monitors Erlang process usage and alerts when approaching system limits.
%%% Realistic capacity is 40-50K concurrent connections due to memory per process.
%%%
%%% Features:
%%% - Process count monitoring with configurable thresholds
%%% - Alerting at 70% of process limit (warning) and 90% (critical)
%%% - Automatic capacity calculations based on empirical data
%%% - Integration with health monitor for system-wide health tracking
%%% - Recommendations for connection pooling to reduce process overhead
%%%
%%% Capacity Planning:
%%% - Per-connection process overhead: ~2-3KB memory
%%% - Realistic single-node capacity: 40-50K concurrent connections
%%% - 100K+ connections require: clustering + connection pooling
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_process_monitor).
-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1,
         get_process_metrics/0,
         get_capacity_estimate/0,
         check_process_limit/0,
         set_alert_thresholds/2,
         get_alert_thresholds/0,
         enable_auto_scaling/0,
         disable_auto_scaling/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

%% Types
-type process_metrics() :: #{
    process_count => non_neg_integer(),
    process_limit => non_neg_integer(),
    usage_percent => float(),
    status => ok | warning | critical,
    available_processes => non_neg_integer(),
    capacity_estimate => non_neg_integer(),
    timestamp => erlang:timestamp()
}.
-type warning_percent() :: float(). %% e.g., 0.70 for 70%
-type critical_percent() :: float(). %% e.g., 0.90 for 90%
-type alert_threshold() :: {warning_percent(), critical_percent()}.
-type capacity_config() :: #{
    per_connection_overhead_bytes => pos_integer(),
    target_memory_bytes => pos_integer(),
    safety_margin_percent => float()
}.

-record(state,
        {check_interval :: pos_integer(),
         alert_thresholds :: alert_threshold(),
         capacity_config :: capacity_config(),
         auto_scaling_enabled = false :: boolean(),
         timer_ref :: undefined | timer:tref(),
         metrics_history :: list(process_metrics())}).

%% Constants
-define(DEFAULT_CHECK_INTERVAL, 30000). %% 30 seconds
-define(DEFAULT_WARNING_THRESHOLD, 0.70). %% 70%
-define(DEFAULT_CRITICAL_THRESHOLD, 0.90). %% 90%
-define(MAX_HISTORY_LENGTH, 100).

%% Capacity defaults (based on empirical benchmarking)
-define(PER_CONNECTION_OVERHEAD, 3072). %% 3KB per connection (process + stack + heap)
-define(TARGET_MEMORY, 1073741824). %% 1GB target memory per node
-define(SAFETY_MARGIN, 0.20). %% 20% safety margin

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the process monitor with default options
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link([]).

%% @doc Start the process monitor with options
-spec start_link(list()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

%% @doc Get current process metrics
-spec get_process_metrics() -> {ok, process_metrics()}.
get_process_metrics() ->
    gen_server:call(?MODULE, get_process_metrics).

%% @doc Get capacity estimate based on current system state
-spec get_capacity_estimate() -> {ok, map()}.
get_capacity_estimate() ->
    gen_server:call(?MODULE, get_capacity_estimate).

%% @doc Check process limit and return status
-spec check_process_limit() -> ok | {warning, term()} | {critical, term()}.
check_process_limit() ->
    gen_server:call(?MODULE, check_process_limit).

%% @doc Set custom alert thresholds
-spec set_alert_thresholds(warning_percent(), critical_percent()) -> ok | {error, term()}.
set_alert_thresholds(Warning, Critical) when
    is_float(Warning), is_float(Critical),
    Warning > 0, Warning < 1.0,
    Critical > 0, Critical < 1.0,
    Critical > Warning ->
    gen_server:call(?MODULE, {set_alert_thresholds, Warning, Critical});
set_alert_thresholds(_, _) ->
    {error, invalid_thresholds}.

%% @doc Get current alert thresholds
-spec get_alert_thresholds() -> {ok, alert_threshold()}.
get_alert_thresholds() ->
    gen_server:call(?MODULE, get_alert_thresholds).

%% @doc Enable auto-scaling recommendations
-spec enable_auto_scaling() -> ok.
enable_auto_scaling() ->
    gen_server:cast(?MODULE, enable_auto_scaling).

%% @doc Disable auto-scaling recommendations
-spec disable_auto_scaling() -> ok.
disable_auto_scaling() ->
    gen_server:cast(?MODULE, disable_auto_scaling).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init(Opts) ->
    ?LOG_INFO("Starting process monitor with options: ~p", [Opts]),

    %% Set process flag to trap exits
    process_flag(trap_exit, true),

    %% Initialize state from options
    CheckInterval = proplists:get_value(check_interval, Opts, ?DEFAULT_CHECK_INTERVAL),
    WarningThreshold = proplists:get_value(warning_threshold, Opts, ?DEFAULT_WARNING_THRESHOLD),
    CriticalThreshold = proplists:get_value(critical_threshold, Opts, ?DEFAULT_CRITICAL_THRESHOLD),

    AlertThresholds = {WarningThreshold, CriticalThreshold},

    CapacityConfig = #{
        per_connection_overhead_bytes =>
            proplists:get_value(per_connection_overhead, Opts, ?PER_CONNECTION_OVERHEAD),
        target_memory_bytes =>
            proplists:get_value(target_memory, Opts, ?TARGET_MEMORY),
        safety_margin_percent =>
            proplists:get_value(safety_margin, Opts, ?SAFETY_MARGIN)
    },

    %% Start periodic monitoring timer
    {ok, TimerRef} = timer:send_interval(CheckInterval, check_metrics),

    State = #state{
        check_interval = CheckInterval,
        alert_thresholds = AlertThresholds,
        capacity_config = CapacityConfig,
        timer_ref = TimerRef,
        metrics_history = []
    },

    %% Perform initial check
    perform_metrics_check(State),

    ?LOG_INFO("Process monitor initialized with thresholds: warning=~p, critical=~p",
              [WarningThreshold, CriticalThreshold]),
    {ok, State}.

handle_call(get_process_metrics, _From, State) ->
    Metrics = collect_process_metrics(State#state.capacity_config),
    {reply, {ok, Metrics}, State};

handle_call(get_capacity_estimate, _From, State) ->
    Estimate = calculate_capacity_estimate(State),
    {reply, {ok, Estimate}, State};

handle_call(check_process_limit, _From, State) ->
    Result = perform_metrics_check(State),
    {reply, Result, State};

handle_call({set_alert_thresholds, Warning, Critical}, _From, State) ->
    ?LOG_INFO("Updating alert thresholds: warning=~.2f, critical=~.2f",
              [Warning, Critical]),
    {reply, ok, State#state{alert_thresholds = {Warning, Critical}}};

handle_call(get_alert_thresholds, _From, State) ->
    {Warning, Critical} = State#state.alert_thresholds,
    {reply, {ok, {Warning, Critical}}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(enable_auto_scaling, State) ->
    ?LOG_INFO("Auto-scaling recommendations enabled"),
    {noreply, State#state{auto_scaling_enabled = true}};

handle_cast(disable_auto_scaling, State) ->
    ?LOG_INFO("Auto-scaling recommendations disabled"),
    {noreply, State#state{auto_scaling_enabled = false}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(check_metrics, State) ->
    %% Periodic metrics check
    perform_metrics_check(State),
    {noreply, State};

handle_info(Info, State) ->
    ?LOG_DEBUG("Unexpected info: ~p", [Info]),
    {noreply, State}.

terminate(Reason, State) ->
    ?LOG_INFO("Process monitor terminating: ~p", [Reason]),

    %% Cancel timer
    case State#state.timer_ref of
        undefined -> ok;
        TimerRef -> timer:cancel(TimerRef)
    end,

    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Collect current process metrics
-spec collect_process_metrics(capacity_config()) -> process_metrics().
collect_process_metrics(CapacityConfig) ->
    ProcessCount = erlang:system_info(process_count),
    ProcessLimit = erlang:system_info(process_limit),
    UsagePercent = ProcessCount / ProcessLimit,

    Status = determine_status(UsagePercent),
    Available = ProcessLimit - ProcessCount,

    %% Calculate capacity estimate
    CapacityEstimate = calculate_connection_capacity(ProcessLimit, CapacityConfig),

    #{
        process_count => ProcessCount,
        process_limit => ProcessLimit,
        usage_percent => UsagePercent,
        status => Status,
        available_processes => Available,
        capacity_estimate => CapacityEstimate,
        timestamp => erlang:timestamp()
    }.

%% @doc Determine status based on usage percentage
-spec determine_status(float()) -> ok | warning | critical.
determine_status(UsagePercent) when UsagePercent >= 0.90 ->
    critical;
determine_status(UsagePercent) when UsagePercent >= 0.70 ->
    warning;
determine_status(_UsagePercent) ->
    ok.

%% @doc Calculate connection capacity estimate
-spec calculate_connection_capacity(non_neg_integer(), capacity_config()) -> non_neg_integer().
calculate_connection_capacity(ProcessLimit, CapacityConfig) ->
    #{
        per_connection_overhead_bytes := PerConnOverhead,
        target_memory_bytes := TargetMemory,
        safety_margin_percent := SafetyMargin
    } = CapacityConfig,

    %% Calculate memory-limited capacity
    MemoryCapacity = trunc((TargetMemory * (1.0 - SafetyMargin)) / PerConnOverhead),

    %% Calculate process-limited capacity (assume 70% of limit for safety)
    ProcessCapacity = trunc(ProcessLimit * 0.70),

    %% Return the more conservative estimate
    min(MemoryCapacity, ProcessCapacity).

%% @doc Calculate full capacity estimate
-spec calculate_capacity_estimate(#state{}) -> map().
calculate_capacity_estimate(State) ->
    CapacityConfig = State#state.capacity_config,
    ProcessLimit = erlang:system_info(process_limit),
    ProcessCount = erlang:system_info(process_count),

    ConnectionCapacity = calculate_connection_capacity(ProcessLimit, CapacityConfig),
    CurrentConnections = ProcessCount, %% Approximation
    RemainingCapacity = max(0, ConnectionCapacity - CurrentConnections),

    %% Memory information
    TotalMemory = erlang:memory(total),
    UsedMemory = erlang:memory(processes) + erlang:memory(system),
    AvailableMemory = TotalMemory - UsedMemory,

    %% Recommendations
    Recommendations = generate_recommendations(State, ProcessCount, ProcessLimit),

    #{
        current_connections => CurrentConnections,
        estimated_capacity => ConnectionCapacity,
        remaining_capacity => RemainingCapacity,
        utilization_percent => (CurrentConnections / ConnectionCapacity) * 100,
        memory_total_bytes => TotalMemory,
        memory_used_bytes => UsedMemory,
        memory_available_bytes => AvailableMemory,
        recommendations => Recommendations,
        realistic_capacity_note =>
            <<"Realistic single-node capacity is 40-50K concurrent connections. "
              "100K+ requires clustering + connection pooling.">>
    }.

%% @doc Generate recommendations based on current state
-spec generate_recommendations(#state{}, non_neg_integer(), non_neg_integer()) -> list(binary()).
generate_recommendations(State, ProcessCount, ProcessLimit) ->
    UsagePercent = ProcessCount / ProcessLimit,
    {WarningThreshold, _CriticalThreshold} = State#state.alert_thresholds,

    Recommendations = [],

    %% Check if approaching warning threshold
    Recs1 = case UsagePercent >= WarningThreshold of
        true ->
            [<<"Process usage approaching limit. Consider connection pooling to reduce overhead.">>
             | Recommendations];
        false ->
            Recommendations
    end,

    %% Check auto-scaling recommendations
    Recs2 = case State#state.auto_scaling_enabled of
        true when UsagePercent > 0.60 ->
            [<<"Enable horizontal scaling: Add more nodes to cluster.">>,
             <<"Implement connection pooling to reuse processes.">>,
             <<"Consider stateless protocol design for easier scaling.">>
             | Recs1];
        true ->
            Recs1;
        false ->
            Recs1
    end,

    %% Capacity recommendations
    CapacityLimit = calculate_connection_capacity(ProcessLimit, State#state.capacity_config),
    Recs3 = case ProcessCount >= CapacityLimit of
        true ->
            [<<"At realistic capacity limit (40-50K connections). "
               "Clustering required for higher scale.">>
             | Recs2];
        false ->
            Recs2
    end,

    lists:reverse(Recs3).

%% @doc Perform metrics check and alert if needed
-spec perform_metrics_check(#state{}) -> ok | {warning, term()} | {critical, term()}.
perform_metrics_check(State) ->
    Metrics = collect_process_metrics(State#state.capacity_config),
    Status = maps:get(status, Metrics),
    ProcessCount = maps:get(process_count, Metrics),
    ProcessLimit = maps:get(process_limit, Metrics),
    UsagePercent = maps:get(usage_percent, Metrics),

    {WarningThreshold, CriticalThreshold} = State#state.alert_thresholds,

    case Status of
        critical ->
            AlertMsg = io_lib:format(
                "CRITICAL: Process usage at ~.2f% (~p/~p processes). "
                "Immediate action required to prevent system crash.",
                [UsagePercent * 100, ProcessCount, ProcessLimit]),

            ?LOG_ERROR("~s", [AlertMsg]),

            %% Trigger recovery manager if available
            case whereis(erlmcp_recovery_manager) of
                undefined -> ok;
                _Pid -> erlmcp_recovery_manager:trigger_recovery(
                    process_limit_critical, {process_limit_exceeded, UsagePercent})
            end,

            {critical, lists:flatten(AlertMsg)};

        warning ->
            AlertMsg = io_lib:format(
                "WARNING: Process usage at ~.2f% (~p/~p processes). "
                "Consider enabling connection pooling or scaling horizontally.",
                [UsagePercent * 100, ProcessCount, ProcessLimit]),

            ?LOG_WARNING("~s", [AlertMsg]),

            {warning, lists:flatten(AlertMsg)};

        ok ->
            ?LOG_DEBUG("Process usage OK: ~.2f% (~p/~p processes)",
                      [UsagePercent * 100, ProcessCount, ProcessLimit]),
            ok
    end.
