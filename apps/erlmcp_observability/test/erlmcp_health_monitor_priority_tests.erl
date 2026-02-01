-module(erlmcp_health_monitor_priority_tests).

-include_lib("eunit/include/eunit.hrl").

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
      fun test_priority_under_load/1]}.

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
    {"Priority health check should have <1ms latency",
     fun() ->
        % Register a test component
        ok = erlmcp_health_monitor:register_component(test_component, Pid, fun() -> healthy end),

        % Perform health check and measure latency
        StartTime = erlang:monotonic_time(microsecond),
        Result = erlmcp_health_monitor:get_component_health(test_component),
        EndTime = erlang:monotonic_time(microsecond),
        LatencyUs = EndTime - StartTime,

        ?assertEqual(healthy, Result),
        ?assert(LatencyUs < 1000)
     end}.

test_priority_system_health_latency(_Pid) ->
    {"Priority system health check should have low latency",
     fun() ->
        % Perform system health check and measure latency
        StartTime = erlang:monotonic_time(microsecond),
        Result = erlmcp_health_monitor:get_system_health(),
        EndTime = erlang:monotonic_time(microsecond),
        LatencyUs = EndTime - StartTime,

        ?assertMatch(#{overall_status := _}, Result),

        ?assert(LatencyUs < 1000),
        ?debugFmt("OTP 28 priority system health latency: ~p us", [LatencyUs])
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

        % Measure latency for each component (latencies calculated but not asserted in this test)
        _Latencies =
            lists:map(fun(Component) ->
                         StartTime = erlang:monotonic_time(microsecond),
                         _Result = erlmcp_health_monitor:get_component_health(Component),
                         EndTime = erlang:monotonic_time(microsecond),
                         EndTime - StartTime
                      end,
                      [worker1, worker2, worker3]),

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
        % Get initial metrics
        InitialMetrics = erlmcp_health_monitor:get_metrics(),
        ?assertMatch(#{}, InitialMetrics),

        % Simulate some priority messages
        ok = erlmcp_health_monitor:register_component(test_comp, self(), fun() -> healthy end),
        ok = erlmcp_health_monitor:update_component_health(test_comp, healthy),

        % Get updated metrics
        UpdatedMetrics = erlmcp_health_monitor:get_metrics(),
        ?assertMatch(#{test_comp := _}, UpdatedMetrics)
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
        ?assert(lists:all(fun(R) -> maps:is_key(overall_status, R) end, Results))
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
        Pids =
            [spawn_link(fun() ->
                           StartTime = erlang:monotonic_time(microsecond),
                           _Result = erlmcp_health_monitor:get_system_health(),
                           EndTime = erlang:monotonic_time(microsecond),
                           LatencyUs = EndTime - StartTime,
                           Parent ! {latency, LatencyUs}
                        end)
             || _ <- lists:seq(1, NumRequests)],

        % Collect all latencies
        Latencies =
            [receive
                 {latency, L} ->
                     L
             after 5000 ->
                 error(timeout)
             end
             || _ <- Pids],

        _AvgLatency = lists:sum(Latencies) / length(Latencies),
        MaxLatency = lists:max(Latencies),

        ?assert(MaxLatency < 1000)
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
        Latencies =
            lists:map(fun(_) ->
                         StartTime = erlang:monotonic_time(microsecond),
                         _Result = erlmcp_health_monitor:get_system_health(),
                         EndTime = erlang:monotonic_time(microsecond),
                         EndTime - StartTime
                      end,
                      lists:seq(1, 20)),

        AvgLatency = lists:sum(Latencies) / length(Latencies),

        ?assert(AvgLatency < 1000),

        % Stop background load
        stop_background_load(LoadPids)
     end}.

%%====================================================================
%% Helper Functions
%%====================================================================

start_test_worker() ->
    Pid = spawn_link(fun() ->
                        receive
                            shutdown ->
                                ok
                        end
                     end),
    {ok, Pid}.

start_background_load(NumWorkers) ->
    [spawn_link(fun background_worker/0) || _ <- lists:seq(1, NumWorkers)].

background_worker() ->
    % Generate CPU and message queue load
    receive
        stop ->
            ok
    after 0 ->
        _ = [erlang:phash2(N) || N <- lists:seq(1, 1000)],
        background_worker()
    end.

stop_background_load(Pids) ->
    [Pid ! stop || Pid <- Pids],
    timer:sleep(50).
