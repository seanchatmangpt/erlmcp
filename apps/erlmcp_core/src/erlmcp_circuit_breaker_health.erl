%%%-------------------------------------------------------------------
%%% @doc
%%% Circuit Breaker Health Integration
%%%
%%% Integrates circuit breaker state with the health monitoring system.
%%% Automatically registers circuit breakers with erlmcp_health_monitor.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_circuit_breaker_health).

-export([
    register_with_health_monitor/1,
    get_health_status/1,
    get_all_breaker_health/0
]).

-include_lib("kernel/include/logger.hrl").

-type health_status() :: healthy | degraded | unhealthy | unknown.

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Register a circuit breaker with the health monitor
-spec register_with_health_monitor(atom()) -> ok.
register_with_health_monitor(BreakerName) ->
    try
        % Create health check function for this breaker
        HealthCheckFun = fun() ->
            get_health_status(BreakerName)
        end,
        
        % Register with health monitor
        case whereis(erlmcp_health_monitor) of
            undefined ->
                ?LOG_WARNING("Health monitor not running, cannot register breaker ~p", [BreakerName]),
                {error, health_monitor_not_running};
            Pid ->
                % Register the circuit breaker
                erlmcp_health_monitor:register_component(
                    BreakerName,
                    Pid,
                    HealthCheckFun
                ),
                ok
        end
    catch
        Class:Reason:Stacktrace ->
            ?LOG_ERROR("Failed to register circuit breaker ~p with health monitor: ~p:~p~n~p",
                      [BreakerName, Class, Reason, Stacktrace]),
            {error, {registration_failed, Reason}}
    end.

%% @doc Get health status for a circuit breaker
-spec get_health_status(atom()) -> {health_status(), map()}.
get_health_status(BreakerName) ->
    try
        % Get circuit breaker state and stats
        case erlmcp_circuit_breaker:get_state(BreakerName) of
            {ok, State} ->
                Stats = erlmcp_circuit_breaker:get_stats(BreakerName),
                
                % Determine health based on circuit breaker state
                {HealthStatus, Metrics} = analyze_breaker_health(State, Stats),
                {HealthStatus, Metrics};
            {error, Reason} ->
                {unhealthy, #{
                    reason => Reason,
                    breaker => BreakerName
                }}
        end
    catch
        Class:Reason:Stacktrace ->
            ?LOG_ERROR("Failed to get health status for breaker ~p: ~p:~p~n~p",
                      [BreakerName, Class, Reason, Stacktrace]),
            {unhealthy, #{
                reason => {exception, Class, Reason},
                breaker => BreakerName
            }}
    end.

%% @doc Get health status for all registered circuit breakers
-spec get_all_breaker_health() -> map().
get_all_breaker_health() ->
    try
        AllStates = erlmcp_circuit_breaker:get_all_states(),
        AllStats = erlmcp_circuit_breaker:get_all_stats(),
        
        % Combine states and stats
        maps:fold(
            fun(BreakerName, State, Acc) ->
                Stats = maps:get(BreakerName, AllStats, #{}),
                {HealthStatus, Metrics} = analyze_breaker_health(State, Stats),
                maps:put(BreakerName, #{
                    status => HealthStatus,
                    metrics => Metrics,
                    state => State
                }, Acc)
            end,
            #{},
            AllStates
        )
    catch
        Class:Reason:Stacktrace ->
            ?LOG_ERROR("Failed to get all breaker health: ~p:~p~n~p",
                      [Class, Reason, Stacktrace]),
            #{}
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Analyze circuit breaker health based on state and stats
-spec analyze_breaker_health(atom(), map()) -> {health_status(), map()}.
analyze_breaker_health(State, Stats) ->
    % Extract key metrics
    FailureRate = maps:get(failure_rate, Stats, 0.0),
    TotalCalls = maps:get(total_calls, Stats, 0),
    TotalFailures = maps:get(total_failures, Stats, 0),
    TotalRejected = maps:get(total_rejected, Stats, 0),
    ConsecutiveFailures = maps:get(consecutive_failures, Stats, 0),
    
    % Determine health status based on circuit breaker state
    HealthStatus = case State of
        open ->
            % Circuit breaker is open - unhealthy
            unhealthy;
        half_open ->
            % Circuit breaker is half open - degraded
            degraded;
        closed ->
            % Circuit breaker is closed - check failure rate
            if
                FailureRate > 0.5 ->
                    % High failure rate - degraded
                    degraded;
                FailureRate > 0.0 andalso FailureRate =< 0.5 ->
                    % Some failures but acceptable - healthy
                    healthy;
                true ->
                    % No failures - healthy
                    healthy
            end;
        _ ->
            unknown
    end,
    
    % Calculate uptime percentage
    UptimePercentage = case TotalCalls of
        0 -> 100.0;
        _ -> 
            SuccessfulCalls = TotalCalls - TotalFailures - TotalRejected,
            (SuccessfulCalls / TotalCalls) * 100
    end,
    
    Metrics = #{
        state => State,
        failure_rate => FailureRate,
        total_calls => TotalCalls,
        total_failures => TotalFailures,
        total_rejected => TotalRejected,
        consecutive_failures => ConsecutiveFailures,
        uptime_percentage => UptimePercentage,
        health_status => HealthStatus
    },
    
    {HealthStatus, Metrics}.
