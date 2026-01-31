-module(erlmcp_health_monitor_priority_tests).

-include_lib("eunit/include/eunit.hrl").
-include("otp_compat.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

health_monitor_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [fun test_priority_health_check_latency/1,
      fun test_priority_system_health_latency/1,
      fun test_priority_component_health_latency/1,
      fun test_priority_metrics_tracking/1,
      fun test_k8s_liveness_probe_priority/1,
      fun test_concurrent_priority_requests/1,
      fun test_priority_under_load/1,
      fun test_fallback_otp_27/1]}.

setup() ->
    % Start health monitor
    {ok, Pid} = erlmcp_health_monitor:start_link([]),
    Pid.

cleanup(Pid) ->
    % Stop health monitor
    case is_process_alive(Pid) of
        true ->
            exit(Pid, shutdown),
            timer:sleep(10);
        false ->
            ok
    end.

%%====================================================================
%% Priority Health Check Latency Tests
%%====================================================================

test_priority_health_check_latency(Pid) ->
    {"Priority health check should have <1ms latency on OTP 28, <5ms on OTP 27",
     fun() ->
         % Register a test component
         ok = erlmcp_health_monitor:register_component(test_component, Pid, fun() -> healthy end),

         % Perform health check and measure latency
         StartTime = erlang:monotonic_time(microsecond),
         Result = erlmcp_health_monitor:get_component_health(test_component),
         EndTime = erlang:monotonic_time(microsecond),
         LatencyUs = EndTime - StartTime,

         ?assertEqual(healthy, Result),

         -ifdef(OTP_28).
         % OTP 28: Priority messages should be <1ms (1000 microseconds)
         ?assert(LatencyUs < 1000),
         ?debugFmt("OTP 28 priority health check latency: ~p us", [LatencyUs])
         -else.
         % OTP 25-27: Should be <5ms (5000 microseconds)
         ?assert(LatencyUs < 5000),
         ?debugFmt("OTP 25-27 health check latency: ~p us", [LatencyUs])
         -endif.
     end}.

test_priority_system_health_latency(Pid) ->
    {"Priority system health check should have low latency",
     fun() ->
         % Perform system health check and measure latency
         StartTime = erlang:monotonic_time(microsecond),
         Result = erlmcp_health_monitor:get_system_health(),
         EndTime = erlang:monotonic_time(microsecond),
         LatencyUs = EndTime - StartTime,

         ?assertMatch(#{overall_status := _}, Result),

         -ifdef(OTP_28).
         ?assert(LatencyUs < 1000),
         ?debugFmt("OTP 28 priority system health latency: ~p us", [LatencyUs])
         -else.
         ?assert(LatencyUs < 5000),
         ?debugFmt("OTP 25-27 system health latency: ~p us", [LatencyUs])
         -endif.
     end}.

test_priority_component_health_latency(_Pid) ->
    {"Component health check via priority path",
     fun() ->
         % Register multiple components
         {ok, Worker1} = start_test_worker(),
         {ok, Worker2} = start_test_worker(),
         {ok, Worker3} = start_test_worker(),

         ok = erlmcp_health_monitor:register_component(worker1, Worker1, fun() -> healthy end),
         ok = erlmcp_health_monitor:register_component(worker2, Worker2, fun() -> healthy end),
         ok = erlmcp_health_monitor:register_component(worker3, Worker3, fun() -> healthy end),

         % Measure latency for each component
         Latencies = lists:map(fun(Component) ->
             StartTime = erlang:monotonic_time(microsecond),
             _Result = erlmcp_health_monitor:get_component_health(Component),
             EndTime = erlang:monotonic_time(microsecond),
             EndTime - StartTime
         end, [worker1, worker2, worker3]),

         -ifdef(OTP_28).
         % All should be <1ms on OTP 28
         ?assert(lists:all(fun(L) -> L < 1000 end, Latencies)),
         ?debugFmt("OTP 28 component health latencies: ~p us", [Latencies])
         -else.
         % All should be <5ms on OTP 25-27
         ?assert(lists:all(fun(L) -> L < 5000 end, Latencies)),
         ?debugFmt("OTP 25-27 component health latencies: ~p us", [Latencies])
         -endif.,

         % Cleanup
         exit(Worker1, shutdown),
         exit(Worker2, shutdown),
         exit(Worker3, shutdown)
     end}.

%%====================================================================
%% Priority Metrics Tests
%%====================================================================

test_priority_metrics_tracking(_Pid) ->
    {"Priority messages should be tracked in metrics",
     fun() ->
         -ifdef(OTP_28).
         % Get initial system health
         Health1 = erlmcp_health_monitor:get_system_health(),
         Metrics1 = maps:get(system_metrics, Health1),

         InitialCount = maps:get(priority_messages_delivered, Metrics1, 0),

         % Perform several health checks
         _ = erlmcp_health_monitor:get_system_health(),
         _ = erlmcp_health_monitor:get_system_health(),
         _ = erlmcp_health_monitor:get_system_health(),

         % Check metrics increased
         Health2 = erlmcp_health_monitor:get_system_health(),
         Metrics2 = maps:get(system_metrics, Health2),

         FinalCount = maps:get(priority_messages_delivered, Metrics2, 0),

         % Should have delivered at least 4 priority messages (including the final get_system_health)
         ?assert(FinalCount >= InitialCount + 4),
         ?debugFmt("Priority messages delivered: ~p", [FinalCount]),

         % Check latency metrics exist
         ?assert(maps:is_key(priority_latency_sum_us, Metrics2)),
         ?assert(maps:is_key(last_priority_latency_us, Metrics2))
         -else.
         % On OTP 25-27, priority metrics may not exist
         ?debugMsg("Skipping priority metrics test on OTP 25-27")
         -endif.
     end}.

%%====================================================================
%% K8s Liveness Probe Tests
%%====================================================================

test_k8s_liveness_probe_priority(_Pid) ->
    {"K8s liveness probes should use priority messages",
     fun() ->
         % Simulate K8s liveness probe pattern: rapid consecutive health checks
         Results = [erlmcp_health_monitor:get_system_health() || _ <- lists:seq(1, 10)],

         % All should succeed
         ?assertEqual(10, length(Results)),
         ?assert(lists:all(fun(R) -> maps:is_key(overall_status, R) end, Results)),

         -ifdef(OTP_28).
         % On OTP 28, check that priority metrics show rapid delivery
         LastResult = lists:last(Results),
         Metrics = maps:get(system_metrics, LastResult),
         LastLatency = maps:get(last_priority_latency_us, Metrics, 0),

         % Last latency should be very low (<500us typical)
         ?assert(LastLatency < 1000),
         ?debugFmt("K8s liveness probe priority latency: ~p us", [LastLatency])
         -else.
         ?debugMsg("K8s liveness probe test on OTP 25-27")
         -endif.
     end}.

%%====================================================================
%% Concurrency Tests
%%====================================================================

test_concurrent_priority_requests(_Pid) ->
    {"Concurrent priority health checks should all complete quickly",
     fun() ->
         NumRequests = 100,

         % Spawn concurrent health check requests
         Parent = self(),
         Pids = [spawn_link(fun() ->
             StartTime = erlang:monotonic_time(microsecond),
             _Result = erlmcp_health_monitor:get_system_health(),
             EndTime = erlang:monotonic_time(microsecond),
             LatencyUs = EndTime - StartTime,
             Parent ! {latency, LatencyUs}
         end) || _ <- lists:seq(1, NumRequests)],

         % Collect all latencies
         Latencies = [receive {latency, L} -> L after 5000 -> error(timeout) end
                      || _ <- Pids],

         AvgLatency = lists:sum(Latencies) / length(Latencies),
         MaxLatency = lists:max(Latencies),

         -ifdef(OTP_28).
         % Average should be well under 1ms even with contention
         ?assert(AvgLatency < 1000),
         ?assert(MaxLatency < 2000), % Allow some outliers
         ?debugFmt("OTP 28 concurrent priority - Avg: ~p us, Max: ~p us", [AvgLatency, MaxLatency])
         -else.
         % On OTP 25-27, allow higher latency
         ?assert(AvgLatency < 5000),
         ?assert(MaxLatency < 10000),
         ?debugFmt("OTP 25-27 concurrent - Avg: ~p us, Max: ~p us", [AvgLatency, MaxLatency])
         -endif.
     end}.

%%====================================================================
%% Load Tests
%%====================================================================

test_priority_under_load(_Pid) ->
    {"Priority health checks should maintain low latency under load",
     fun() ->
         % Create background load
         LoadPids = start_background_load(50),

         timer:sleep(100), % Let load stabilize

         % Measure priority health check latency under load
         Latencies = lists:map(fun(_) ->
             StartTime = erlang:monotonic_time(microsecond),
             _Result = erlmcp_health_monitor:get_system_health(),
             EndTime = erlang:monotonic_time(microsecond),
             EndTime - StartTime
         end, lists:seq(1, 20)),

         AvgLatency = lists:sum(Latencies) / length(Latencies),

         % Stop background load
         stop_background_load(LoadPids),

         -ifdef(OTP_28).
         % Even under load, priority messages should be fast
         ?assert(AvgLatency < 2000), % Allow 2ms under load
         ?debugFmt("OTP 28 priority under load: ~p us average", [AvgLatency])
         -else.
         ?assert(AvgLatency < 8000), % Allow 8ms under load on older OTP
         ?debugFmt("OTP 25-27 under load: ~p us average", [AvgLatency])
         -endif.
     end}.

%%====================================================================
%% Fallback Tests
%%====================================================================

test_fallback_otp_27(_Pid) ->
    {"Health monitor should work correctly on OTP 25-27 without priority messages",
     fun() ->
         % This test verifies fallback behavior
         Result = erlmcp_health_monitor:get_system_health(),

         ?assertMatch(#{overall_status := _,
                       system_metrics := _,
                       component_health := _}, Result),

         -ifndef(OTP_28).
         % On OTP 25-27, priority metrics may not be present
         ?debugMsg("Verified fallback behavior on OTP 25-27")
         -endif.
     end}.

%%====================================================================
%% Helper Functions
%%====================================================================

start_test_worker() ->
    Pid = spawn_link(fun() ->
        receive
            shutdown -> ok
        end
    end),
    {ok, Pid}.

start_background_load(NumWorkers) ->
    [spawn_link(fun background_worker/0) || _ <- lists:seq(1, NumWorkers)].

background_worker() ->
    % Generate CPU and message queue load
    receive
        stop -> ok
    after 0 ->
        _ = [erlang:phash2(N) || N <- lists:seq(1, 1000)],
        background_worker()
    end.

stop_background_load(Pids) ->
    [Pid ! stop || Pid <- Pids],
    timer:sleep(50).
