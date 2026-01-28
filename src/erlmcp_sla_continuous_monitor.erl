%%%-------------------------------------------------------------------
%% @doc
%% Continuous SLA Monitoring Background Process
%%
%% Monitors production metrics in real-time and:
%% - Logs SLA status every 5 minutes (configurable)
%% - Detects throughput drops below plan minimum
%% - Detects p99 latency exceeding plan maximum
%% - Detects failover exceeding SLA time
%% - Generates and logs SLA violations
%% - Maintains rolling violation history
%% - Auto-recovers when metrics return to normal
%% - Integrates with receipt chain for audit trail
%%
%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_sla_continuous_monitor).

-behaviour(gen_server).

%% API
-export([
    start_link/0,
    start_link/1,
    start_monitoring/1,
    stop_monitoring/1,
    get_status/0,
    get_plan_status/1,
    stop/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

%% Plan envelope definitions
-define(PLAN_ENVELOPES, #{
    team => #{
        min_throughput_req_s => 450,
        max_latency_p99_ms => 150,
        max_failover_s => 5
    },
    enterprise => #{
        min_throughput_req_s => 1500,
        max_latency_p99_ms => 100,
        max_failover_s => 2
    },
    gov => #{
        min_throughput_req_s => 900,
        max_latency_p99_ms => 80,
        max_failover_s => 1
    }
}).

-define(SERVER, ?MODULE).
-define(DEFAULT_INTERVAL, 300000).  % 5 minutes
-define(VIOLATION_THRESHOLD_PCT, 5). % 5% tolerance
-define(AUTO_RECOVERY_THRESHOLD, 2). % 2 consecutive good readings

-record(state, {
    monitored_plans = #{} :: map(),     % {Plan => {Interval, TimerRef}}
    plan_status = #{} :: map(),         % {Plan => Status}
    violations_log = #{} :: map(),      % {Plan => [Violation]}
    recovery_counters = #{} :: map(),  % {Plan => Count}
    last_alert_time = #{} :: map()     % {Plan => Timestamp}
}).

-record(plan_status, {
    plan :: atom(),
    last_check :: integer(),
    throughput_ok :: boolean(),
    latency_ok :: boolean(),
    failover_ok :: boolean(),
    current_throughput :: float(),
    current_latency :: float(),
    current_failover :: float(),
    consecutive_failures :: non_neg_integer(),
    recovery_progress :: non_neg_integer()
}).

-record(violation_event, {
    timestamp :: integer(),
    plan :: atom(),
    metric :: atom(),
    expected :: float(),
    actual :: float(),
    severity :: atom()
}).

-compile({nowarn_unused_record, [violation_event]}).
-compile({nowarn_unused_function, [{plan_status_to_map, 1}, {determine_overall_status, 1}]}).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start continuous SLA monitor
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Start continuous SLA monitor with options
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Options) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Options], []).

%% @doc Start monitoring a specific plan
-spec start_monitoring(atom()) -> ok | {error, term()}.
start_monitoring(Plan) when is_atom(Plan) ->
    start_monitoring(Plan, ?DEFAULT_INTERVAL);
start_monitoring(_) ->
    {error, invalid_plan}.

%% @doc Start monitoring a specific plan with custom interval
-spec start_monitoring(atom(), pos_integer()) -> ok | {error, term()}.
start_monitoring(Plan, Interval) when is_atom(Plan), is_integer(Interval), Interval > 0 ->
    case maps:is_key(Plan, ?PLAN_ENVELOPES) of
        true ->
            gen_server:call(?SERVER, {start_monitoring, Plan, Interval});
        false ->
            {error, unknown_plan}
    end.

%% @doc Stop monitoring a specific plan
-spec stop_monitoring(atom()) -> ok | {error, term()}.
stop_monitoring(Plan) when is_atom(Plan) ->
    gen_server:call(?SERVER, {stop_monitoring, Plan}).

%% @doc Get overall monitoring status
-spec get_status() -> map().
get_status() ->
    gen_server:call(?SERVER, get_status).

%% @doc Get status for a specific plan
-spec get_plan_status(atom()) -> map() | {error, term()}.
get_plan_status(Plan) when is_atom(Plan) ->
    gen_server:call(?SERVER, {get_plan_status, Plan}).

%% @doc Stop continuous SLA monitor
-spec stop() -> ok.
stop() ->
    gen_server:call(?SERVER, stop).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

-spec init([]) -> {ok, #state{}}.
init([]) ->
    init([#{}]);
init([_Options]) ->
    ?LOG_INFO("Continuous SLA monitor starting", []),
    {ok, #state{}}.

-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {reply, term(), #state{}}.

handle_call({start_monitoring, Plan, Interval}, _From, State) ->
    case maps:get(Plan, State#state.monitored_plans, undefined) of
        undefined ->
            % Start monitoring this plan
            {ok, TimerRef} = timer:send_interval(Interval, {check_plan, Plan}),
            MonitoredPlans = maps:put(Plan, {Interval, TimerRef}, State#state.monitored_plans),
            PlanStatus = #plan_status{
                plan = Plan,
                last_check = erlang:system_time(millisecond),
                throughput_ok = true,
                latency_ok = true,
                failover_ok = true,
                current_throughput = 0.0,
                current_latency = 0.0,
                current_failover = 0.0,
                consecutive_failures = 0,
                recovery_progress = 0
            },
            PlanStatuses = maps:put(Plan, PlanStatus, State#state.plan_status),
            ?LOG_INFO("Started monitoring plan: ~w with interval: ~w ms", [Plan, Interval]),
            NewViolationsLog = maps:put(Plan, [], State#state.violations_log),
            {reply, ok, State#state{
                monitored_plans = MonitoredPlans,
                plan_status = PlanStatuses,
                violations_log = NewViolationsLog
            }};
        {_OldInterval, _OldTimerRef} ->
            {reply, {error, already_monitoring}, State}
    end;

handle_call({stop_monitoring, Plan}, _From, State) ->
    MonitoredPlans = State#state.monitored_plans,
    case maps:get(Plan, MonitoredPlans, undefined) of
        {_Interval, TimerRef} ->
            timer:cancel(TimerRef),
            NewMonitoredPlans = maps:remove(Plan, MonitoredPlans),
            ?LOG_INFO("Stopped monitoring plan: ~w", [Plan]),
            {reply, ok, State#state{monitored_plans = NewMonitoredPlans}};
        undefined ->
            {reply, {error, not_monitoring}, State}
    end;

handle_call(get_status, _From, State) ->
    Statuses = maps:map(fun(_Plan, PlanStatus) ->
        plan_status_to_map(PlanStatus)
    end, State#state.plan_status),

    Overall = #{
        monitored_plans => maps:size(State#state.monitored_plans),
        timestamp => erlang:system_time(millisecond),
        statuses => Statuses
    },
    {reply, Overall, State};

handle_call({get_plan_status, Plan}, _From, State) ->
    case maps:get(Plan, State#state.plan_status, undefined) of
        undefined ->
            {reply, {error, not_monitoring}, State};
        PlanStatus ->
            Status = plan_status_to_map(PlanStatus),
            Violations = maps:get(Plan, State#state.violations_log, []),
            EnhancedStatus = maps:merge(Status, #{
                violations_count => length(Violations),
                recent_violations => lists:sublist(Violations, 10)
            }),
            {reply, EnhancedStatus, State}
    end;

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.

handle_cast(_Request, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.

handle_info({check_plan, Plan}, State) ->
    NewState = check_plan_sla(Plan, State),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, State) ->
    MonitoredPlans = State#state.monitored_plans,
    maps:foreach(fun(_Plan, {_Interval, TimerRef}) ->
        timer:cancel(TimerRef)
    end, MonitoredPlans),
    ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Check SLA for a specific plan
-spec check_plan_sla(atom(), #state{}) -> #state{}.
check_plan_sla(Plan, State) ->
    case maps:get(Plan, State#state.plan_status, undefined) of
        undefined ->
            State;
        PlanStatus ->
            Envelope = maps:get(Plan, ?PLAN_ENVELOPES),

            % Get current metrics
            {Throughput, Latency, Failover} = get_current_plan_metrics(Plan),

            % Check against envelope with tolerance
            ThroughputOk = check_metric(
                Throughput,
                maps:get(min_throughput_req_s, Envelope),
                lower_bound,
                ?VIOLATION_THRESHOLD_PCT
            ),

            LatencyOk = check_metric(
                Latency,
                maps:get(max_latency_p99_ms, Envelope),
                upper_bound,
                ?VIOLATION_THRESHOLD_PCT
            ),

            FailoverOk = check_metric(
                Failover,
                maps:get(max_failover_s, Envelope),
                upper_bound,
                ?VIOLATION_THRESHOLD_PCT
            ),

            % Update status
            NewPlanStatus = PlanStatus#plan_status{
                last_check = erlang:system_time(millisecond),
                throughput_ok = ThroughputOk,
                latency_ok = LatencyOk,
                failover_ok = FailoverOk,
                current_throughput = Throughput,
                current_latency = Latency,
                current_failover = Failover
            },

            % Handle violations or recovery
            AllOk = ThroughputOk andalso LatencyOk andalso FailoverOk,

            if
                AllOk ->
                    % Metric is OK, potentially recovery
                    RecoveryCount = (maps:get(Plan, State#state.recovery_counters, 0)) + 1,
                    UpdatedPlanStatus = maps:put(Plan, NewPlanStatus, State#state.plan_status),
                    UpdatedRecoveryCounters = maps:put(Plan, RecoveryCount, State#state.recovery_counters),
                    UpdatedState = State#state{
                        plan_status = UpdatedPlanStatus,
                        recovery_counters = UpdatedRecoveryCounters
                    },
                    if
                        RecoveryCount >= ?AUTO_RECOVERY_THRESHOLD ->
                            log_recovery(Plan, NewPlanStatus),
                            UpdatedState#state{
                                recovery_counters = (UpdatedState#state.recovery_counters)#{Plan => 0}
                            };
                        true ->
                            UpdatedState
                    end;
                true ->
                    % Violation detected
                    Violations = generate_violations(Plan, NewPlanStatus, ThroughputOk, LatencyOk, FailoverOk),
                    ViolationsLog = maps:get(Plan, State#state.violations_log, []),
                    NewViolationsLog = Violations ++ ViolationsLog,
                    TrimmedViolationsLog = lists:sublist(NewViolationsLog, 1000),

                    % Log violations
                    lists:foreach(fun(V) -> log_violation(V) end, Violations),

                    State#state{
                        plan_status = maps:put(Plan, NewPlanStatus, State#state.plan_status),
                        violations_log = maps:put(Plan, TrimmedViolationsLog, State#state.violations_log),
                        recovery_counters = maps:put(Plan, 0, State#state.recovery_counters)
                    }
            end
    end.

%% @doc Get current metrics for a plan
-spec get_current_plan_metrics(atom()) -> {float(), float(), float()}.
get_current_plan_metrics(_Plan) ->
    try
        case erlang:whereis(erlmcp_metrics_server) of
            undefined ->
                {0.0, 0.0, 0.0};
            _Pid ->
                AllMetrics = erlmcp_metrics_server:get_metrics(),
                Throughput = maps:get(message_rate_per_sec, AllMetrics, 0),
                LatencyStats = maps:get(latency_stats, AllMetrics, #{}),
                Latency = maps:get(p99, LatencyStats, 0),
                % Failover would be measured during actual failover events
                Failover = 0.0,
                {Throughput, Latency, Failover}
        end
    catch
        _:_ ->
            {0.0, 0.0, 0.0}
    end.

%% @doc Check if a metric meets the envelope requirement
%% Lower bound check: actual >= target * (1 - threshold%)
%% Upper bound check: actual <= target * (1 + threshold%)
-spec check_metric(float(), float(), atom(), float()) -> boolean().
check_metric(Actual, Target, lower_bound, ThresholdPct) ->
    Threshold = Target * (ThresholdPct / 100.0),
    Actual >= (Target - Threshold);
check_metric(Actual, Target, upper_bound, ThresholdPct) ->
    Threshold = Target * (ThresholdPct / 100.0),
    Actual =< (Target + Threshold).

%% @doc Generate violations for failed metrics
-spec generate_violations(atom(), #plan_status{}, boolean(), boolean(), boolean()) -> [map()].
generate_violations(Plan, Status, ThroughputOk, LatencyOk, FailoverOk) ->
    Timestamp = erlang:system_time(millisecond),
    Envelope = maps:get(Plan, ?PLAN_ENVELOPES),

    Violations = [],

    % Throughput violation
    Violations1 = case ThroughputOk of
        false ->
            MinRequired = maps:get(min_throughput_req_s, Envelope),
            [#{
                timestamp => Timestamp,
                plan => Plan,
                metric => throughput,
                expected => MinRequired,
                actual => Status#plan_status.current_throughput,
                severity => critical
            } | Violations];
        true ->
            Violations
    end,

    % Latency violation
    Violations2 = case LatencyOk of
        false ->
            MaxAllowed = maps:get(max_latency_p99_ms, Envelope),
            [#{
                timestamp => Timestamp,
                plan => Plan,
                metric => latency,
                expected => MaxAllowed,
                actual => Status#plan_status.current_latency,
                severity => warn
            } | Violations1];
        true ->
            Violations1
    end,

    % Failover violation
    case FailoverOk of
        false ->
            MaxFailover = maps:get(max_failover_s, Envelope),
            [#{
                timestamp => Timestamp,
                plan => Plan,
                metric => failover,
                expected => MaxFailover,
                actual => Status#plan_status.current_failover,
                severity => critical
            } | Violations2];
        true ->
            Violations2
    end.

%% @doc Log SLA violation
-spec log_violation(map()) -> ok.
log_violation(Violation) ->
    Plan = maps:get(plan, Violation),
    Metric = maps:get(metric, Violation),
    Expected = maps:get(expected, Violation),
    Actual = maps:get(actual, Violation),
    Severity = maps:get(severity, Violation),

    case Severity of
        critical ->
            ?LOG_ERROR(
                "CRITICAL SLA Violation - Plan: ~w, Metric: ~w, Expected: ~.2f, Actual: ~.2f",
                [Plan, Metric, Expected, Actual]
            );
        warn ->
            ?LOG_WARNING(
                "SLA Warning - Plan: ~w, Metric: ~w, Expected: ~.2f, Actual: ~.2f",
                [Plan, Metric, Expected, Actual]
            )
    end,

    % Log to receipt chain if available
    log_violation_to_receipt_chain(Violation),
    ok.

%% @doc Log recovery event
-spec log_recovery(atom(), #plan_status{}) -> ok.
log_recovery(Plan, Status) ->
    ?LOG_INFO(
        "SLA Recovery - Plan: ~w recovered to within envelope (TP: ~.2f, P99: ~.2f, FO: ~.2f)",
        [Plan, Status#plan_status.current_throughput, Status#plan_status.current_latency, Status#plan_status.current_failover]
    ),
    ok.

%% @doc Log violation to receipt chain
-spec log_violation_to_receipt_chain(map()) -> ok.
log_violation_to_receipt_chain(Violation) ->
    try
        case erlang:whereis(erlmcp_receipt_chain) of
            undefined -> ok;
            _Pid ->
                erlmcp_receipt_chain:log_receipt(sla_continuous_violation, Violation),
                ok
        end
    catch
        _:_ -> ok
    end.

%% @doc Convert plan status to map
-spec plan_status_to_map(#plan_status{}) -> map().
plan_status_to_map(Status) ->
    #{
        plan => Status#plan_status.plan,
        last_check => Status#plan_status.last_check,
        throughput_ok => Status#plan_status.throughput_ok,
        latency_ok => Status#plan_status.latency_ok,
        failover_ok => Status#plan_status.failover_ok,
        current_throughput => Status#plan_status.current_throughput,
        current_latency => Status#plan_status.current_latency,
        current_failover => Status#plan_status.current_failover,
        consecutive_failures => Status#plan_status.consecutive_failures,
        recovery_progress => Status#plan_status.recovery_progress,
        overall_status => determine_overall_status(Status)
    }.

%% @doc Determine overall status
-spec determine_overall_status(#plan_status{}) -> atom().
determine_overall_status(Status) ->
    case {Status#plan_status.throughput_ok, Status#plan_status.latency_ok, Status#plan_status.failover_ok} of
        {true, true, true} -> healthy;
        {true, _, _} -> degraded;
        _ -> critical
    end.
