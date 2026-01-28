-module(erlmcp_health_monitor).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1, register_component/2, register_component/3,
         unregister_component/1, get_system_health/0, get_component_health/1,
         get_all_component_health/0, set_health_check_config/2, get_health_check_config/1,
         report_circuit_breaker/2, report_degradation/1, reset_health_status/0,
         trigger_health_check/1, trigger_system_health_check/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-include_lib("kernel/include/logger.hrl").

%% Types
-type component_id() :: atom().
-type health_status() :: healthy | unhealthy | degraded | unknown.
-type health_check_fun() :: fun(() -> health_status() | {health_status(), term()}).
-type health_config() ::
    #{check_interval => pos_integer(),
      timeout => pos_integer(),
      max_consecutive_failures => pos_integer(),
      recovery_check_interval => pos_integer()}.

-record(component_health,
        {id :: component_id(),
         pid :: pid() | undefined,
         status = unknown :: health_status(),
         last_check :: undefined | erlang:timestamp(),
         consecutive_failures = 0 :: non_neg_integer(),
         total_checks = 0 :: non_neg_integer(),
         successful_checks = 0 :: non_neg_integer(),
         config :: health_config(),
         check_fun :: health_check_fun() | undefined,
         last_error :: term() | undefined,
         circuit_breaker_active = false :: boolean(),
         degraded = false :: boolean()}).
-record(state,
        {components = #{} :: #{component_id() => #component_health{}},
         default_config :: health_config(),
         system_health = unknown :: health_status(),
         system_metrics = #{} :: map(),
         health_timer :: undefined | timer:tref(),
         alerts = [] :: list()}).

-define(DEFAULT_CONFIG,
        #{check_interval => 30000,          % 30 seconds
          timeout => 5000,                  % 5 seconds
          max_consecutive_failures => 3,    % 3 failures trigger unhealthy
          recovery_check_interval => 10000}).  % 10 seconds for unhealthy components
-define(SYSTEM_HEALTH_CHECK_INTERVAL, 15000). % 15 seconds
-define(MEMORY_WARNING_THRESHOLD, 0.85).      % 85% memory usage warning
-define(MEMORY_CRITICAL_THRESHOLD, 0.95).     % 95% memory usage critical

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link([]).

-spec start_link(list()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

-spec register_component(component_id(), pid()) -> ok | {error, term()}.
register_component(ComponentId, Pid) ->
    register_component(ComponentId, Pid, undefined).

-spec register_component(component_id(), pid(), health_check_fun() | undefined) ->
                            ok | {error, term()}.
register_component(ComponentId, Pid, CheckFun) ->
    gen_server:call(?MODULE, {register_component, ComponentId, Pid, CheckFun}).

-spec unregister_component(component_id()) -> ok.
unregister_component(ComponentId) ->
    gen_server:cast(?MODULE, {unregister_component, ComponentId}).

-spec get_system_health() -> map().
get_system_health() ->
    gen_server:call(?MODULE, get_system_health).

-spec get_component_health(component_id()) -> health_status() | not_found.
get_component_health(ComponentId) ->
    gen_server:call(?MODULE, {get_component_health, ComponentId}).

-spec get_all_component_health() -> map().
get_all_component_health() ->
    gen_server:call(?MODULE, get_all_component_health).

-spec set_health_check_config(component_id(), health_config()) -> ok | {error, term()}.
set_health_check_config(ComponentId, Config) ->
    gen_server:call(?MODULE, {set_health_check_config, ComponentId, Config}).

-spec get_health_check_config(component_id()) ->
                                 {ok, health_config()} | {error, not_found}.
get_health_check_config(ComponentId) ->
    gen_server:call(?MODULE, {get_health_check_config, ComponentId}).

-spec report_circuit_breaker(component_id(), open | closed) -> ok.
report_circuit_breaker(ComponentId, State) ->
    gen_server:cast(?MODULE, {report_circuit_breaker, ComponentId, State}).

-spec report_degradation(component_id()) -> ok.
report_degradation(ComponentId) ->
    gen_server:cast(?MODULE, {report_degradation, ComponentId}).

-spec reset_health_status() -> ok.
reset_health_status() ->
    gen_server:cast(?MODULE, reset_health_status).

-spec trigger_health_check(component_id()) -> ok | {error, term()}.
trigger_health_check(ComponentId) ->
    gen_server:cast(?MODULE, {trigger_health_check, ComponentId}),
    ok.

-spec trigger_system_health_check() -> ok.
trigger_system_health_check() ->
    gen_server:cast(?MODULE, trigger_system_health_check),
    ok.

%%====================================================================
%% gen_server callbacks
%%====================================================================

init(Opts) ->
    ?LOG_INFO("Starting health monitor with options: ~p", [Opts]),

    % Set up process monitoring
    process_flag(trap_exit, true),

    DefaultConfig =
        maps:merge(?DEFAULT_CONFIG, proplists:get_value(default_config, Opts, #{})),

    % Start periodic system health checks
    {ok, Timer} = timer:send_interval(?SYSTEM_HEALTH_CHECK_INTERVAL, system_health_check),

    State =
        #state{default_config = DefaultConfig,
               health_timer = Timer,
               system_metrics =
                   #{memory_status => ok,
                     process_count => 0,
                     system_load => 0.0,
                     last_check => erlang:timestamp()}},

    ?LOG_INFO("Health monitor initialized"),
    {ok, State}.

handle_call({register_component, ComponentId, Pid, CheckFun}, _From, State) ->
    ?LOG_INFO("Registering component ~p with PID ~p for health monitoring",
              [ComponentId, Pid]),

    % Monitor the component process
    case Pid of
        undefined ->
            ok;
        _ ->
            erlang:monitor(process, Pid)
    end,

    Component =
        #component_health{id = ComponentId,
                          pid = Pid,
                          config = State#state.default_config,
                          check_fun = CheckFun},

    NewComponents = maps:put(ComponentId, Component, State#state.components),
    NewState = State#state{components = NewComponents},

    % Schedule initial health check
    schedule_health_check(ComponentId, maps:get(check_interval, State#state.default_config)),

    {reply, ok, NewState};
handle_call(get_system_health, _From, State) ->
    SystemHealth = generate_system_health_report(State),
    {reply, SystemHealth, State};
handle_call({get_component_health, ComponentId}, _From, State) ->
    case maps:find(ComponentId, State#state.components) of
        {ok, Component} ->
            {reply, Component#component_health.status, State};
        error ->
            {reply, not_found, State}
    end;
handle_call(get_all_component_health, _From, State) ->
    AllHealth =
        maps:map(fun(_Id, Component) ->
                    #{status => Component#component_health.status,
                      last_check => Component#component_health.last_check,
                      consecutive_failures => Component#component_health.consecutive_failures,
                      total_checks => Component#component_health.total_checks,
                      successful_checks => Component#component_health.successful_checks,
                      circuit_breaker_active => Component#component_health.circuit_breaker_active,
                      degraded => Component#component_health.degraded}
                 end,
                 State#state.components),
    {reply, AllHealth, State};
handle_call({set_health_check_config, ComponentId, Config}, _From, State) ->
    case maps:find(ComponentId, State#state.components) of
        {ok, Component} ->
            MergedConfig = maps:merge(State#state.default_config, Config),
            UpdatedComponent = Component#component_health{config = MergedConfig},
            NewComponents = maps:put(ComponentId, UpdatedComponent, State#state.components),
            NewState = State#state{components = NewComponents},
            {reply, ok, NewState};
        error ->
            {reply, {error, not_found}, State}
    end;
handle_call({get_health_check_config, ComponentId}, _From, State) ->
    case maps:find(ComponentId, State#state.components) of
        {ok, Component} ->
            {reply, {ok, Component#component_health.config}, State};
        error ->
            {reply, {error, not_found}, State}
    end;
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({unregister_component, ComponentId}, State) ->
    ?LOG_INFO("Unregistering component ~p from health monitoring", [ComponentId]),
    NewComponents = maps:remove(ComponentId, State#state.components),
    NewState = State#state{components = NewComponents},
    {noreply, NewState};
handle_cast({report_circuit_breaker, ComponentId, CircuitState}, State) ->
    case maps:find(ComponentId, State#state.components) of
        {ok, Component} ->
            IsActive = CircuitState =:= open,
            UpdatedComponent = Component#component_health{circuit_breaker_active = IsActive},
            NewComponents = maps:put(ComponentId, UpdatedComponent, State#state.components),
            NewState = State#state{components = NewComponents},

            ?LOG_INFO("Circuit breaker ~p for component ~p", [CircuitState, ComponentId]),
            {noreply, NewState};
        error ->
            ?LOG_WARNING("Circuit breaker report for unknown component ~p", [ComponentId]),
            {noreply, State}
    end;
handle_cast({report_degradation, ComponentId}, State) ->
    case maps:find(ComponentId, State#state.components) of
        {ok, Component} ->
            UpdatedComponent = Component#component_health{degraded = true, status = degraded},
            NewComponents = maps:put(ComponentId, UpdatedComponent, State#state.components),
            NewState = State#state{components = NewComponents},

            ?LOG_INFO("Component ~p reported degraded", [ComponentId]),
            {noreply, NewState};
        error ->
            ?LOG_WARNING("Degradation report for unknown component ~p", [ComponentId]),
            {noreply, State}
    end;
handle_cast(reset_health_status, State) ->
    ?LOG_INFO("Resetting all health statuses"),
    ResetComponents =
        maps:map(fun(_Id, Component) ->
                    Component#component_health{status = unknown,
                                               consecutive_failures = 0,
                                               last_error = undefined,
                                               circuit_breaker_active = false,
                                               degraded = false}
                 end,
                 State#state.components),

    NewState =
        State#state{components = ResetComponents,
                    system_health = unknown,
                    alerts = []},
    {noreply, NewState};
handle_cast({trigger_health_check, ComponentId}, State) ->
    NewState = perform_component_health_check(ComponentId, State),
    {noreply, NewState};
handle_cast(trigger_system_health_check, State) ->
    NewState = perform_system_health_check(State),
    {noreply, NewState};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, Reason}, State) ->
    ?LOG_WARNING("Monitored process ~p died with reason: ~p", [Pid, Reason]),
    case find_component_by_pid(Pid, State#state.components) of
        {ok, ComponentId} ->
            NewState = handle_component_process_death(ComponentId, Reason, State),
            {noreply, NewState};
        error ->
            ?LOG_WARNING("Unknown monitored process ~p died", [Pid]),
            {noreply, State}
    end;
handle_info({health_check, ComponentId}, State) ->
    NewState = perform_component_health_check(ComponentId, State),

    % Schedule next health check
    case maps:find(ComponentId, NewState#state.components) of
        {ok, Component} ->
            CheckInterval = determine_check_interval(Component),
            schedule_health_check(ComponentId, CheckInterval);
        error ->
            ok % Component was removed
    end,

    {noreply, NewState};
handle_info(system_health_check, State) ->
    NewState = perform_system_health_check(State),
    {noreply, NewState};
handle_info(Info, State) ->
    ?LOG_DEBUG("Unexpected info: ~p", [Info]),
    {noreply, State}.

terminate(Reason, State) ->
    ?LOG_INFO("Health monitor terminating: ~p", [Reason]),

    % Cancel health timer
    case State#state.health_timer of
        undefined ->
            ok;
        Timer ->
            timer:cancel(Timer)
    end,

    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

-spec perform_component_health_check(component_id(), #state{}) -> #state{}.
perform_component_health_check(ComponentId, State) ->
    case maps:find(ComponentId, State#state.components) of
        {ok, Component} ->
            ?LOG_DEBUG("Performing health check for component ~p", [ComponentId]),

            CheckStart = erlang:timestamp(),
            CheckResult = execute_health_check(Component),
            CheckDuration =
                timer:now_diff(
                    erlang:timestamp(), CheckStart)
                div 1000,

            UpdatedComponent =
                update_component_health_status(Component, CheckResult, CheckDuration),

            NewComponents = maps:put(ComponentId, UpdatedComponent, State#state.components),

            % Trigger recovery if needed
            maybe_trigger_recovery(UpdatedComponent),

            State#state{components = NewComponents};
        error ->
            ?LOG_WARNING("Health check requested for unknown component ~p", [ComponentId]),
            State
    end.

-spec execute_health_check(#component_health{}) ->
                              {health_status(), term()} | health_status().
execute_health_check(Component) ->
    Config = Component#component_health.config,
    Timeout = maps:get(timeout, Config, 5000),

    try
        case Component#component_health.check_fun of
            undefined ->
                % Default health check - check if process is alive
                basic_process_health_check(Component);
            CheckFun when is_function(CheckFun, 0) ->
                % Execute custom health check with timeout
                execute_with_timeout(CheckFun, Timeout);
            _ ->
                {unhealthy, invalid_check_function}
        end
    catch
        Class:Exception:Stacktrace ->
            ?LOG_ERROR("Health check failed for ~p: ~p:~p~n~p",
                       [Component#component_health.id, Class, Exception, Stacktrace]),
            {unhealthy, {exception, {Class, Exception}}}
    end.

-spec basic_process_health_check(#component_health{}) -> health_status().
basic_process_health_check(Component) ->
    case Component#component_health.pid of
        undefined ->
            unknown;
        Pid ->
            case is_process_alive(Pid) of
                true ->
                    healthy;
                false ->
                    unhealthy
            end
    end.

-spec execute_with_timeout(fun(() -> term()), pos_integer()) ->
                              {health_status(), term()} | health_status().
execute_with_timeout(Fun, Timeout) ->
    Parent = self(),
    Ref = make_ref(),

    Pid = spawn(fun() ->
                   Result = Fun(),
                   Parent ! {Ref, Result}
                end),

    receive
        {Ref, Result} ->
            Result
    after Timeout ->
        exit(Pid, kill),
        {unhealthy, timeout}
    end.

-spec update_component_health_status(#component_health{},
                                     {health_status(), term()} | health_status(),
                                     pos_integer()) ->
                                        #component_health{}.
update_component_health_status(Component, CheckResult, _CheckDuration) ->
    {NewStatus, LastError} =
        case CheckResult of
            {Status, Error} ->
                {Status, Error};
            Status when is_atom(Status) ->
                {Status, undefined}
        end,

    TotalChecks = Component#component_health.total_checks + 1,

    {NewConsecutiveFailures, NewSuccessfulChecks} =
        case NewStatus of
            healthy ->
                {0, Component#component_health.successful_checks + 1};
            _ ->
                {Component#component_health.consecutive_failures + 1,
                 Component#component_health.successful_checks}
        end,

    % Determine final status based on consecutive failures
    FinalStatus = determine_final_health_status(NewStatus, NewConsecutiveFailures, Component),

    Component#component_health{status = FinalStatus,
                               last_check = erlang:timestamp(),
                               consecutive_failures = NewConsecutiveFailures,
                               total_checks = TotalChecks,
                               successful_checks = NewSuccessfulChecks,
                               last_error = LastError}.

-spec determine_final_health_status(health_status(),
                                    non_neg_integer(),
                                    #component_health{}) ->
                                       health_status().
determine_final_health_status(CheckStatus, ConsecutiveFailures, Component) ->
    MaxFailures = maps:get(max_consecutive_failures, Component#component_health.config, 3),

    case {CheckStatus, ConsecutiveFailures >= MaxFailures} of
        {healthy, _} ->
            healthy;
        {degraded, _} ->
            degraded;
        {_, true} ->
            unhealthy;
        {unhealthy, false} ->
            unhealthy;
        {unknown, false} ->
            unknown
    end.

-spec determine_check_interval(#component_health{}) -> pos_integer().
determine_check_interval(Component) ->
    Config = Component#component_health.config,
    case Component#component_health.status of
        unhealthy ->
            maps:get(recovery_check_interval, Config, 10000);
        _ ->
            maps:get(check_interval, Config, 30000)
    end.

-spec maybe_trigger_recovery(#component_health{}) -> ok.
maybe_trigger_recovery(Component) ->
    case Component#component_health.status of
        unhealthy ->
            % Trigger recovery through recovery manager
            case Component#component_health.consecutive_failures >= 3 of
                true ->
                    ?LOG_WARNING("Component ~p is unhealthy, triggering recovery",
                                 [Component#component_health.id]),
                    erlmcp_recovery_manager:trigger_recovery(Component#component_health.id,
                                                             health_check_failure);
                false ->
                    ok
            end;
        _ ->
            ok
    end.

-spec handle_component_process_death(component_id(), term(), #state{}) -> #state{}.
handle_component_process_death(ComponentId, Reason, State) ->
    case maps:find(ComponentId, State#state.components) of
        {ok, Component} ->
            UpdatedComponent =
                Component#component_health{status = unhealthy,
                                           pid = undefined,
                                           last_error = {process_death, Reason},
                                           consecutive_failures =
                                               Component#component_health.consecutive_failures + 1},

            NewComponents = maps:put(ComponentId, UpdatedComponent, State#state.components),

            % Trigger recovery
            erlmcp_recovery_manager:trigger_recovery(ComponentId, {process_death, Reason}),

            State#state{components = NewComponents};
        error ->
            State
    end.

-spec perform_system_health_check(#state{}) -> #state{}.
perform_system_health_check(State) ->
    ?LOG_DEBUG("Performing system health check"),

    % Collect system metrics
    SystemMetrics = collect_system_metrics(),

    % Check component health summary
    ComponentHealthSummary = analyze_component_health(State#state.components),

    % Determine overall system health
    SystemHealth = determine_system_health(SystemMetrics, ComponentHealthSummary),

    % Generate alerts if needed
    Alerts =
        generate_health_alerts(SystemMetrics, ComponentHealthSummary, State#state.alerts),

    ?LOG_INFO("System health check completed: ~p", [SystemHealth]),

    State#state{system_health = SystemHealth,
                system_metrics = SystemMetrics,
                alerts = Alerts}.

-spec collect_system_metrics() -> map().
collect_system_metrics() ->
    % Memory usage
    MemoryTotal = erlang:memory(total),
    MemoryProcesses = erlang:memory(processes),
    MemoryUsage = MemoryProcesses / MemoryTotal,

    MemoryStatus =
        if MemoryUsage > ?MEMORY_CRITICAL_THRESHOLD ->
               critical;
           MemoryUsage > ?MEMORY_WARNING_THRESHOLD ->
               warning;
           true ->
               ok
        end,

    % Process count
    ProcessCount = erlang:system_info(process_count),
    ProcessLimit = erlang:system_info(process_limit),
    ProcessUsage = ProcessCount / ProcessLimit,

    ProcessStatus =
        if ProcessUsage > 0.9 ->
               critical;
           ProcessUsage > 0.8 ->
               warning;
           true ->
               ok
        end,

    % System load (simplified)
    {ReductionCount, _} = statistics(reductions),

    #{memory_total => MemoryTotal,
      memory_processes => MemoryProcesses,
      memory_usage => MemoryUsage,
      memory_status => MemoryStatus,
      process_count => ProcessCount,
      process_limit => ProcessLimit,
      process_usage => ProcessUsage,
      process_status => ProcessStatus,
      reduction_count => ReductionCount,
      last_check => erlang:timestamp()}.

-spec analyze_component_health(#{component_id() => #component_health{}}) -> map().
analyze_component_health(Components) ->
    ComponentList = maps:values(Components),
    Total = length(ComponentList),

    StatusCounts =
        lists:foldl(fun(Component, Acc) ->
                       Status = Component#component_health.status,
                       maps:update_with(Status, fun(Count) -> Count + 1 end, 1, Acc)
                    end,
                    #{},
                    ComponentList),

    Healthy = maps:get(healthy, StatusCounts, 0),
    Unhealthy = maps:get(unhealthy, StatusCounts, 0),
    Degraded = maps:get(degraded, StatusCounts, 0),
    Unknown = maps:get(unknown, StatusCounts, 0),

    HealthyPercentage =
        case Total of
            0 ->
                1.0;
            _ ->
                Healthy / Total
        end,

    #{total_components => Total,
      healthy_count => Healthy,
      unhealthy_count => Unhealthy,
      degraded_count => Degraded,
      unknown_count => Unknown,
      healthy_percentage => HealthyPercentage,
      status_counts => StatusCounts}.

-spec determine_system_health(map(), map()) -> health_status().
determine_system_health(SystemMetrics, ComponentHealth) ->
    % Check memory status
    MemoryStatus = maps:get(memory_status, SystemMetrics),
    ProcessStatus = maps:get(process_status, SystemMetrics),

    % Check component health
    HealthyPercentage = maps:get(healthy_percentage, ComponentHealth),
    UnhealthyCount = maps:get(unhealthy_count, ComponentHealth),

    case {MemoryStatus, ProcessStatus, HealthyPercentage, UnhealthyCount} of
        {critical, _, _, _} ->
            unhealthy;
        {_, critical, _, _} ->
            unhealthy;
        {_, _, Percentage, _} when Percentage < 0.5 ->
            unhealthy;
        {_, _, _, Count} when Count > 3 ->
            unhealthy;
        {warning, _, _, _} ->
            degraded;
        {_, warning, _, _} ->
            degraded;
        {_, _, Percentage, _} when Percentage < 0.8 ->
            degraded;
        {_, _, _, Count} when Count > 0 ->
            degraded;
        _ ->
            healthy
    end.

-spec generate_health_alerts(map(), map(), list()) -> list().
generate_health_alerts(SystemMetrics, ComponentHealth, _ExistingAlerts) ->
    Alerts = [],

    % Memory alerts
    MemoryAlerts =
        case maps:get(memory_status, SystemMetrics) of
            critical ->
                [{memory_critical, erlang:timestamp()}];
            warning ->
                [{memory_warning, erlang:timestamp()}];
            ok ->
                []
        end,

    % Process alerts
    ProcessAlerts =
        case maps:get(process_status, SystemMetrics) of
            critical ->
                [{process_limit_critical, erlang:timestamp()}];
            warning ->
                [{process_limit_warning, erlang:timestamp()}];
            ok ->
                []
        end,

    % Component health alerts
    UnhealthyCount = maps:get(unhealthy_count, ComponentHealth),
    ComponentAlerts =
        case UnhealthyCount of
            Count when Count > 3 ->
                [{multiple_components_unhealthy, Count, erlang:timestamp()}];
            Count when Count > 0 ->
                [{components_unhealthy, Count, erlang:timestamp()}];
            0 ->
                []
        end,

    lists:flatten([Alerts, MemoryAlerts, ProcessAlerts, ComponentAlerts]).

-spec generate_system_health_report(#state{}) -> map().
generate_system_health_report(State) ->
    #{overall_status => State#state.system_health,
      system_metrics => State#state.system_metrics,
      component_health => analyze_component_health(State#state.components),
      active_alerts => State#state.alerts,
      last_check => erlang:timestamp()}.

-spec find_component_by_pid(pid(), #{component_id() => #component_health{}}) ->
                               {ok, component_id()} | error.
find_component_by_pid(Pid, Components) ->
    case maps:to_list(
             maps:filter(fun(_Id, Component) -> Component#component_health.pid =:= Pid end,
                         Components))
    of
        [{ComponentId, _Component}] ->
            {ok, ComponentId};
        [] ->
            error;
        Multiple ->
            ?LOG_WARNING("Multiple components with same PID: ~p", [Multiple]),
            {ok, element(1, hd(Multiple))}
    end.

-spec schedule_health_check(component_id(), pos_integer()) -> ok.
schedule_health_check(ComponentId, Interval) ->
    erlang:send_after(Interval, self(), {health_check, ComponentId}),
    ok.
