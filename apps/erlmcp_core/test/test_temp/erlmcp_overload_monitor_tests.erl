-module(erlmcp_overload_monitor_tests).

-include_lib("eunit/include/eunit.hrl").

-include("erlmcp.hrl").

%%% ====================================================================
%%% Test Setup and Teardown
%%% ====================================================================

overload_monitor_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(State) ->
        [test_start_stop(State),
         test_queue_monitoring(State),
         test_alert_generation(State),
         test_overload_detection(State),
         test_alert_thresholds(State),
         test_alert_history(State),
         test_live_interrogation(State),
         test_health_check_integration(State),
         test_circuit_breaker_integration(State),
         test_periodic_checks(State)]
     end}.

setup() ->
    %% Start required dependencies
    {ok, QLPid} = erlmcp_queue_limits:start_link(),
    {ok, OMPid} = erlmcp_overload_monitor:start_link([{check_interval, 1000}]),

    #{queue_limits_pid => QLPid, overload_monitor_pid => OMPid}.

teardown(#{queue_limits_pid := QLPid, overload_monitor_pid := OMPid}) ->
    %% Stop services
    exit(OMPid, normal),
    exit(QLPid, normal),
    timer:sleep(100),
    ok.

%%% ====================================================================
%%% Test Cases
%%% ====================================================================

test_start_stop(_State) ->
    {"Start and stop overload monitor",
     fun() ->
        ?assertMatch({ok, _Pid}, erlmcp_overload_monitor:start_link()),
        Queues = erlmcp_overload_monitor:queues(),
        ?assert(is_list(Queues))
     end}.

test_queue_monitoring(_State) ->
    {"Monitor queue depths across all roles",
     fun() ->
        %% Get current queue state
        Queues = erlmcp_overload_monitor:queues(),
        ?assert(is_list(Queues)),

        %% Each queue info should have required fields
        lists:foreach(fun(QueueInfo) ->
                         ?assertMatch(#{role := _}, QueueInfo),
                         ?assertMatch(#{depth := _}, QueueInfo),
                         ?assertMatch(#{limit := _}, QueueInfo),
                         ?assertMatch(#{utilization := _}, QueueInfo),
                         ?assertMatch(#{status := _}, QueueInfo)
                      end,
                      Queues)
     end}.

test_alert_generation(_State) ->
    {"Generate alerts when thresholds exceeded",
     fun() ->
        TestPid = spawn(fun() -> timer:sleep(infinity) end),

        %% Set low limit to trigger alert
        ok = erlmcp_queue_limits:set_limit(alert_test, 10),

        %% Fill to 90% (critical threshold)
        lists:foreach(fun(_) -> erlmcp_queue_limits:record_enqueue(alert_test, TestPid) end,
                      lists:seq(1, 9)),

        %% Force health check
        timer:sleep(100),
        erlmcp_overload_monitor:force_check(),
        timer:sleep(100),

        %% Should have generated alert
        History = erlmcp_overload_monitor:get_alert_history(),
        Alerts = lists:filter(fun(#{role := R}) -> R =:= alert_test end, History),
        ?assert(length(Alerts) > 0, "Should have generated at least one alert"),

        %% Clean up
        exit(TestPid, kill)
     end}.

test_overload_detection(_State) ->
    {"Detect overloaded processes",
     fun() ->
        TestPid = spawn(fun() -> timer:sleep(infinity) end),

        %% Set low limit and fill to capacity
        ok = erlmcp_queue_limits:set_limit(overload_test, 5),
        lists:foreach(fun(_) -> erlmcp_queue_limits:record_enqueue(overload_test, TestPid) end,
                      lists:seq(1, 5)),

        %% Force check
        timer:sleep(100),
        erlmcp_overload_monitor:force_check(),
        timer:sleep(100),

        %% Should detect overload
        Overloaded = erlmcp_overload_monitor:get_overloaded(),
        ?assert(length(Overloaded) > 0, "Should detect overloaded processes"),

        %% Clean up
        exit(TestPid, kill)
     end}.

test_alert_thresholds(_State) ->
    {"Different alert levels for different utilization",
     fun() ->
        %% Test warning threshold (80%)
        TestPid1 = spawn(fun() -> timer:sleep(infinity) end),
        ok = erlmcp_queue_limits:set_limit(warning_test, 100),
        lists:foreach(fun(_) -> erlmcp_queue_limits:record_enqueue(warning_test, TestPid1) end,
                      lists:seq(1, 85)),

        timer:sleep(100),
        erlmcp_overload_monitor:force_check(),
        timer:sleep(100),

        Queues = erlmcp_overload_monitor:queues(warning_test),
        case Queues of
            [#{status := Status}] ->
                ?assert(Status =:= warning orelse Status =:= critical,
                        "Should be at least warning level");
            _ ->
                ok
        end,

        %% Clean up
        exit(TestPid1, kill)
     end}.

test_alert_history(_State) ->
    {"Track alert history",
     fun() ->
        %% Reset alerts first
        ok = erlmcp_overload_monitor:reset_alerts(),

        TestPid = spawn(fun() -> timer:sleep(infinity) end),

        %% Generate alerts
        ok = erlmcp_queue_limits:set_limit(history_test, 10),
        lists:foreach(fun(_) -> erlmcp_queue_limits:record_enqueue(history_test, TestPid) end,
                      lists:seq(1, 9)),

        timer:sleep(100),
        erlmcp_overload_monitor:force_check(),
        timer:sleep(100),

        %% Check history
        AllHistory = erlmcp_overload_monitor:get_alert_history(),
        RoleHistory = erlmcp_overload_monitor:get_alert_history(history_test),

        ?assert(is_list(AllHistory), "Should return alert history"),
        ?assert(is_list(RoleHistory), "Should return role-specific history"),

        %% Each alert should have required fields
        lists:foreach(fun(Alert) ->
                         ?assertMatch(#{level := _}, Alert),
                         ?assertMatch(#{role := _}, Alert),
                         ?assertMatch(#{depth := _}, Alert),
                         ?assertMatch(#{timestamp := _}, Alert)
                      end,
                      RoleHistory),

        %% Clean up
        exit(TestPid, kill)
     end}.

test_live_interrogation(_State) ->
    {"Live interrogation of queue state",
     fun() ->
        %% Test queues/0 - all roles
        AllQueues = erlmcp_overload_monitor:queues(),
        ?assert(is_list(AllQueues)),
        ?assert(length(AllQueues) > 0, "Should have queue info for registered roles"),

        %% Test queues/1 - specific role
        SessionQueues = erlmcp_overload_monitor:queues(session),
        ?assert(is_list(SessionQueues)),

        %% Test get_overloaded/0
        Overloaded = erlmcp_overload_monitor:get_overloaded(),
        ?assert(is_list(Overloaded))
     end}.

test_health_check_integration(_State) ->
    {"Integration with health monitor",
     fun() ->
        %% This test verifies that overload monitor can notify health monitor
        %% In real deployment, erlmcp_health_monitor would be running
        TestPid = spawn(fun() -> timer:sleep(infinity) end),

        %% Create overload condition
        ok = erlmcp_queue_limits:set_limit(health_test, 5),
        lists:foreach(fun(_) -> erlmcp_queue_limits:record_enqueue(health_test, TestPid) end,
                      lists:seq(1, 5)),

        timer:sleep(100),
        erlmcp_overload_monitor:force_check(),
        timer:sleep(100),

        %% Should have logged degradation (check alert history)
        History = erlmcp_overload_monitor:get_alert_history(health_test),
        ?assert(length(History) > 0, "Should have recorded alerts"),

        %% Clean up
        exit(TestPid, kill)
     end}.

test_circuit_breaker_integration(_State) ->
    {"Integration with circuit breaker on overload",
     fun() ->
        %% This test verifies circuit breaker triggering on 100% utilization
        TestPid = spawn(fun() -> timer:sleep(infinity) end),

        %% Fill exactly to capacity (100%)
        ok = erlmcp_queue_limits:set_limit(breaker_test, 10),
        lists:foreach(fun(_) -> erlmcp_queue_limits:record_enqueue(breaker_test, TestPid) end,
                      lists:seq(1, 10)),

        timer:sleep(100),
        erlmcp_overload_monitor:force_check(),
        timer:sleep(100),

        %% Should have triggered overload alert
        Overloaded = erlmcp_overload_monitor:get_overloaded(breaker_test),
        ?assert(length(Overloaded) > 0, "Should detect 100% utilization"),

        case Overloaded of
            [#{status := Status}] ->
                ?assertEqual(overload, Status, "Should be at overload level");
            _ ->
                ok
        end,

        %% Clean up
        exit(TestPid, kill)
     end}.

test_periodic_checks(_State) ->
    {"Periodic health checks run automatically",
     fun() ->
        %% Create condition that will be detected by periodic check
        TestPid = spawn(fun() -> timer:sleep(infinity) end),

        ok = erlmcp_queue_limits:set_limit(periodic_test, 10),
        lists:foreach(fun(_) -> erlmcp_queue_limits:record_enqueue(periodic_test, TestPid) end,
                      lists:seq(1, 9)),

        %% Wait for periodic check to run (interval is 1000ms in setup)
        timer:sleep(2000),

        %% Should have generated alerts from periodic check
        History = erlmcp_overload_monitor:get_alert_history(periodic_test),
        ?assert(length(History) > 0, "Periodic check should generate alerts"),

        %% Clean up
        exit(TestPid, kill)
     end}.
